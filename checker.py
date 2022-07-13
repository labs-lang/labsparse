from concurrent.futures import ThreadPoolExecutor, wait
import os
from parser import kw
from typing import List
from itertools import combinations

from message import Message


def walk(d):
    """Return all AST nodes in d as an iterable.
    """
    if isinstance(d, dict):
        if "ast-type" in d:
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
                    node=var, ast=ast))
        if len(node["lhs"]) != len(node["rhs"]):
            result.append(Message.from_node(
                message_id="E002",
                message=(
                    "Invalid number of expressions "
                    f'(expected {len(node["lhs"])}, got {len(node["rhs"])})'),
                node=node, ast=ast))
        if len(node["lhs"]) > 1:
            for v1, v2 in combinations(node["lhs"], 2):
                if v1["name"] == v2["name"]:
                    result.append(Message.from_node(
                        message_id="E003",
                        message=(
                            f"Variable '{v1['name']}' "
                            "assigned multiple times"),
                        node=v2, ast=ast))

    return check(ast, lambda n: n["ast-type"].startswith("assign"), body)


def check_ref(ast: dict) -> List[Message]:
    def body(n, ast, result):
        if n["name"] in kw.split():
            result.append(Message.from_node(
                message_id="E004",
                message=f"Unexpected keyword '{n['name']}'",
                node=n, ast=ast
            ))

    return check(ast, lambda n: n["ast-type"].startswith("ref"), body)


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
                node=n, ast=ast))
    return check(ast, lambda n: n["ast-type"] == "ext", body)


def check_builtins(ast: dict) -> List[Message]:
    arities = {
        "abs": 1,
        "max": 2,
        "min": 2
    }

    def body(n, ast, result):
        try:
            expected, got = arities[n["fn"]], len(n["args"])
            if expected != got:
                where = n
                if expected < got:
                    where = n["args"][expected]
                elif got > 0:
                    where = n["args"][-1]
                result.append(Message.from_node(
                    message_id="E007",
                    message=(
                        f"Wrong arity for '{n['fn']}' "
                        f"(expected {expected}, got {got})"),
                    node=where, ast=ast))
        except KeyError:
            pass
    return check(ast, lambda n: n["ast-type"] == "builtin", body)


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
