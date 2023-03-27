from collections import Counter
from concurrent.futures import ThreadPoolExecutor, wait
from functools import reduce
from inspect import signature
from itertools import combinations
import operator
from typing import List

from .labs_ast import (Assign, Builtin, Expr, Node, Ref, RefExt, RefLink, Root,
                       is_, NodeType)
from .labs_parser import Attr, kw
from .output import Message


def check(ast, filter_fn, body) -> List[Message]:
    """Template for a checker."""
    result = []
    for n in (n for n in ast.walk() if filter_fn(n)):
        body(n, result)

    return result


def check_assign(ast: Node) -> List[Message]:
    """Checks related to assignment statements."""
    def body(node, result):
        for var in node[Attr.LHS]:
            if var[Attr.NAME] == "id":
                result.append(Message.from_node(
                    message_id="E001",
                    message="Cannot assign to 'id'",
                    node=var))
        if len(node[Attr.LHS]) != len(node[Attr.RHS]):
            result.append(Message.from_node(
                message_id="E002",
                message=(
                    "Invalid number of expressions "
                    f'(expected {len(node[Attr.LHS])}, got {len(node[Attr.RHS])})'),
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

    return check(ast, is_(Assign), body)


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
    if not spawn:
        return [Message.from_node(
            message_id="E005",
            message="Missing 'spawn' in 'system'",
            node=ast["system"]
        )]
    # TODO try and check spawns that evaluate to 0


def check_externs(ast: dict) -> List[Message]:
    try:
        defined_exts = {x[Attr.NAME]: x for x in ast["system"]["extern"]}
    except KeyError:
        defined_exts = []

    uses = Counter()

    def body(n, result):
        uses[n[Attr.NAME]] += 1
        if n[Attr.NAME] not in defined_exts:
            result.append(Message.from_node(
                message_id="E006",
                message=f"Undefined extern '{n[Attr.NAME]}'",
                node=n))

    result = check(ast, is_(RefExt), body)
    result.extend(
        Message.from_node(
            message_id="W001",
            message=f"Unused extern '{n}'",
            node=defined_exts[n]
        )
        for n in uses if n in defined_exts and uses[n] <= 1
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
