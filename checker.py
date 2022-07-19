from concurrent.futures import ThreadPoolExecutor, wait
from itertools import combinations
from parser import Attr, NodeType, kw
from typing import List

from output import Message


def walk(d):
    """Return all AST nodes in d as an iterable.
    """
    if isinstance(d, dict):
        if Attr.NODE_TYPE in d:
            yield d
        for val in d.values():
            if isinstance(val, list):
                for x in val:
                    yield from walk(x)
            elif isinstance(val, dict):
                yield from walk(val)


def check(ast, filter_fn, body) -> List[Message]:
    """Template for a checker."""
    result = []
    for n in (n for n in walk(ast) if filter_fn(n)):
        body(n, ast, result)

    return result


def check_assign(ast: dict) -> List[Message]:
    """Checks related to assignment statements."""
    def body(node, ast, result):
        for var in node["lhs"]:
            if var["name"] == "id":
                result.append(Message.from_node(
                    message_id="E001",
                    message="Cannot assign to 'id'",
                    node=var))
        if len(node["lhs"]) != len(node["rhs"]):
            result.append(Message.from_node(
                message_id="E002",
                message=(
                    "Invalid number of expressions "
                    f'(expected {len(node["lhs"])}, got {len(node["rhs"])})'),
                node=node))
        if len(node["lhs"]) > 1:
            for v1, v2 in combinations(node["lhs"], 2):
                if v1["name"] == v2["name"]:
                    result.append(Message.from_node(
                        message_id="E003",
                        message=(
                            f"Variable '{v1['name']}' "
                            "assigned multiple times"),
                        node=v2))

    return check(ast, lambda n: n in NodeType.ASSIGN, body)


def check_ref(ast: dict) -> List[Message]:
    def body(n, ast, result):
        if n["name"] in kw.split():
            result.append(Message.from_node(
                message_id="E004",
                message=f"Unexpected keyword '{n['name']}'",
                node=n, ast=ast
            ))

    return check(ast, lambda n: n[Attr.NODE_TYPE].startswith("ref"), body)


def check_spawn(ast: dict) -> List[Message]:
    if "spawn" not in ast["system"]:
        return [Message.from_node(
            message_id="E005",
            message="Missing 'spawn' in 'system'",
            node=ast["system"], ast=ast
        )]
    # TODO try and check spawns that evaluate to 0


def check_externs(ast: dict) -> List[Message]:
    try:
        defined_exts = ast["system"]["extern"]["extern"]
    except KeyError:
        defined_exts = []

    def body(n, ast, result):
        if n["name"] not in defined_exts:
            result.append(Message.from_node(
                message_id="E006",
                message=f"Undefined extern '{n['name']}'",
                node=n))
    return check(ast, lambda n: n[Attr.NODE_TYPE] == "ext", body)


def check_builtins(ast: dict) -> List[Message]:
    arities = {
        "abs": 1,
        "max": 2,
        "min": 2,
        "unary-minus": 1
    }

    def body(n, _, result):
        expected = arities[n[Attr.NAME]]
        got = len(n.get(Attr.OPERANDS, []))
        if expected != got:
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
    return check(ast, lambda n: n in NodeType.BUILTIN, body)


def run(ast: dict) -> List[Message]:
    with ThreadPoolExecutor() as executor:
        tasks = wait(
            executor.submit(fn, ast)
            for fn in (
                check_spawn, check_assign, check_ref, check_externs,
                check_builtins
            )
        )
        messages = [m for t in tasks.done for m in (t.result() or [])]
        messages.sort(key=lambda m: (m.path, m.line, m.column))
        return messages
