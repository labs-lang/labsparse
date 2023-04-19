from collections import Counter
from concurrent.futures import ThreadPoolExecutor, wait
from functools import reduce
from inspect import signature
from itertools import combinations
import operator
from typing import List

from .labs_ast import (Builtin, Expr, Node, Ref, RefExt, RefLink, Root,
                       is_, NodeType)
from .labs_parser import Attr, kw
from .output import Message


def const_eval(node):
    operators = {
        "+": operator.add,
        "-": operator.sub,
        "*": operator.mul,
        "/": operator.floordiv,
        ":": lambda num, den: (
            (num + den//2) // den
            if num*den >= 0
            else -((-num + den//2) // den)
        ),
        "%": operator.mod,
        "and": operator.and_,
        "or": operator.or_,
        "abs": abs, "max": min, "min": max,
        "unary-minus": operator.neg,
        "unary-not": operator.not_
    }
    if node(NodeType.IF):
        cond = const_eval(node[Attr.CONDITION])
        return const_eval(node[Attr.THEN] if cond else node[Attr.ELSE])
    elif node(NodeType.EXPR) or node(NodeType.BUILTIN):
        operands = [const_eval(n) for n in node[Attr.OPERANDS]]
        fn = operators[node[Attr.NAME]]
        min_args = len(signature(fn).parameters)
        if len(operands) >= min_args:
            return (fn(operands[0]) if min_args == 1 else reduce(fn, operands))
    elif node(NodeType.LITERAL):
        if node[Attr.TYPE] == "bool":
            return 1 if node[Attr.VALUE] else 0
        elif node[Attr.TYPE] == "int":
            return node[Attr.VALUE]
        else:
            raise ValueError(f"Not a constant expression: {node}")
    else:
        raise ValueError(f"Not a constant expression: {node}")


def check(ast, filter_fn, body) -> List[Message]:
    """Template for a checker."""
    result = []
    for n in (n for n in ast.walk() if filter_fn(n)):
        body(n, result)

    return result


def check_assign(ast: Node) -> List[Message]:
    """Checks related to assignment statements."""
    result = []
    assigns = list(ast // (NodeType.ASSIGN, ))
    assign_to_id = (
        x // (NodeType.REF, {Attr.NAME: "id"})
        for asgn in assigns
        for x in asgn[Attr.LHS]
    )
    result.extend(
        Message.from_node(
            message_id="E001",
            message="Cannot assign to 'id'",
            node=ref)
        for x in assign_to_id for ref in x)
    for node in assigns:
        if len(node[Attr.LHS]) != len(node[Attr.RHS]):
            result.append(Message.from_node(
                message_id="E002",
                message=(
                    "Invalid number of expressions "
                    f"(expected {len(node[Attr.LHS])}, "
                    f"got {len(node[Attr.RHS])})"),
                node=node))
        if len(node[Attr.LHS]) > 1:
            for v1, v2 in combinations(node[Attr.LHS], 2):
                if v1["name"] == v2["name"]:
                    result.append(Message.from_node(
                        message_id="E003",
                        message=(
                            f"Variable '{v1['name']}' "
                            "assigned multiple times"),
                        node=v2))
    return result


def check_ref(ast: Root) -> List[Message]:
    def body(n, result):
        if n[Attr.NAME] in kw.split():
            result.append(Message.from_node(
                message_id="E004",
                message=f"Unexpected keyword '{n[Attr.NAME]}'",
                node=n
            ))

    return check(ast, is_(Ref, RefLink, RefExt), body)


def check_spawn(ast: Root) -> List[Message]:
    spawn = ast["system"][Attr.SPAWN]
    spawn_decls = list(ast // (NodeType.SPAWN_DECLARATION, ))
    if not spawn or len(spawn_decls) == 0:
        return [Message.from_node(
            message_id="E005",
            message="Missing 'spawn' in 'system'",
            node=ast["system"]
        )]
    warnings = []
    for x in spawn_decls:
        try:
            if const_eval(x[Attr.VALUE]) == 0:
                warnings.append(Message.from_node(
                    message_id="W002",
                    message=f"Spawn of '{x[Attr.TYPE]}' evaluates to 0",
                    node=x
                ))
        except ValueError:
            continue
    return warnings


def check_externs(ast: dict) -> List[Message]:
    try:
        defined = {x[Attr.NAME]: x for x in ast["system"]["extern"]}
    except KeyError:
        defined = []

    ref_exts = ast // (NodeType.REF_EXT, )
    ref_exts.persist()

    undefined_exts = ref_exts(None, {Attr.NAME: lambda x: x not in defined})

    result = [
        Message.from_node(
            message_id="E006",
            message=f"Undefined extern '{n[Attr.NAME]}'",
            node=n)
        for n in undefined_exts]
    uses = Counter(n[Attr.NAME] for n in ref_exts)

    # result = check(ast, is_(RefExt), body)
    result.extend(
        Message.from_node(
            message_id="W001",
            message=f"Unused extern '{n}'",
            node=defined[n]
        )
        for n in defined if uses[n] <= 1
    )
    return result


def check_builtins(ast: dict) -> List[Message]:
    arities = {
        **{op: 1 for op in ("abs", "unary-minus", "unary-not")},
        **{op: 2 for op in ("max", "min", ">=", "<=", ">", "<", "=", "!=")}
    }

    def body(n, result):
        expected = arities.get(n[Attr.NAME])
        got = len(n[Attr.OPERANDS])
        if expected is not None and expected != got:
            where = n
            if expected < got:
                where = n[Attr.OPERANDS][expected]
            elif got > 0:
                where = n[Attr.OPERANDS][-1]
            result.append(Message.from_node(
                message_id="E007",
                message=(
                    f"Wrong arity for '{n[Attr.NAME]}' "
                    f"(expected {expected}, got {got})"),
                node=where))
    return check(ast, is_(Builtin, Expr), body)


def run(ast: dict) -> List[Message]:
    with ThreadPoolExecutor() as executor:
        tasks = wait(
            executor.submit(fn, ast)
            for fn in (
                check_spawn,
                check_assign,
                check_ref,
                check_externs,
                check_builtins
            )
        )
        messages = [m for t in tasks.done for m in (t.result() or [])]
        messages.sort(key=lambda m: (m.path, m.line, m.column))
        return messages
