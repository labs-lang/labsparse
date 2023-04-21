import operator
from copy import deepcopy
from functools import reduce
from inspect import signature

from .labs_ast import (Attr, Builtin, Composition, Expr, Guarded, If, Literal,
                       Node, ProcDef)


def evaluate(node):
    """Evaluate an expression node. DESTRUCTIVE"""
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
    if isinstance(node, If):
        cond = evaluate(node[Attr.CONDITION])
        if isinstance(cond, Literal) and cond[Attr.TYPE] == "bool":
            return evaluate(
                node[Attr.THEN]
                if cond[Attr.VALUE]
                else node[Attr.ELSE])

    if isinstance(node, Expr) or isinstance(node, Builtin):
        operands = [evaluate(n) for n in node[Attr.OPERANDS]]
        literals, others = (
            [n[Attr.VALUE] for n in operands if isinstance(n, Literal)],
            [n for n in operands if not isinstance(n, Literal)])
        fn = operators[node[Attr.NAME]]
        min_args = len(signature(fn).parameters)
        if len(literals) >= min_args:
            value = (
                fn(literals[0])
                if min_args == 1
                else reduce(fn, literals))

            new = Literal(node[Attr.PATH], node[Attr.LN], node[Attr.COL], {})
            new[Attr.VALUE] = value
            new[Attr.TYPE] = "bool" if isinstance(value, bool) else "int"
            new[Attr.SYNTHETIC] = True

            if others:
                others.append(new)
                node[Attr.OPERANDS] = others
                return node
            else:
                return new
    return node


def simplify(proc):
    """Simplifies a LAbS process tree."""
    if isinstance(proc, Composition):
        if len(proc[Attr.OPERANDS]) == 1:
            return simplify(proc[Attr.OPERANDS][0])
        else:
            proc[Attr.OPERANDS] = [
                simplify(p)
                for p in proc[Attr.OPERANDS]
            ]
            return proc
    elif Attr.BODY in proc and isinstance(proc[Attr.BODY], Node):
        proc[Attr.BODY] = simplify(proc[Attr.BODY])
        return proc
    else:
        return proc


def simplify_expr(node):
    def _simplify(node):
        if not isinstance(node, Node):
            return node
        if Attr.OPERANDS in node:
            node[Attr.OPERANDS] = [_simplify(n) for n in node[Attr.OPERANDS]]
            if node[Attr.NAME] == "=" and all(node[Attr.OPERANDS][0].eq(x) for x in node[Attr.OPERANDS][1:]):  # noqa: E501
                return Literal.new(node, "bool", True)
            if (
                node[Attr.NAME] == "=" and
                all(isinstance(x, Literal) for x in node[Attr.OPERANDS]) and
                any(not node[Attr.OPERANDS][0].eq(x) for x in node[Attr.OPERANDS][1:])  # noqa: E501
            ):
                return Literal.new(node, "bool", False)

            if node[Attr.NAME] == "and":
                if any(isinstance(x, Literal) and x[Attr.VALUE] is False for x in node[Attr.OPERANDS]):  # noqa: E501
                    return Literal.new(node, "bool", False)
                else:
                    node[Attr.OPERANDS] = [x for x in node[Attr.OPERANDS] if Attr.VALUE not in x or x[Attr.VALUE] is not True]  # noqa: E501
            if node[Attr.NAME] == "or":
                if any(isinstance(x, Literal) and x[Attr.VALUE] is True for x in node[Attr.OPERANDS]):  # noqa: E501
                    return Literal.new(node, "bool", True)
                else:
                    node[Attr.OPERANDS] = [x for x in node[Attr.OPERANDS] if Attr.VALUE not in x or x[Attr.VALUE] is not False]  # noqa: E501
        return node

    while True:
        new_node = deepcopy(node)
        _simplify(new_node)
        if new_node.eq(node):
            return new_node
        node = new_node


def fold_constants(ast: Node):
    """Performs constant folding on the given AST."""
    for n in ast.walk():
        if not isinstance(n, Composition):
            for a in (Attr.OPERANDS, Attr.RHS):
                if hasattr(n, a):
                    n[a] = [evaluate(x) for x in n[a]]
            for a in (Attr.CONDITION, Attr.THEN, Attr.ELSE):
                if hasattr(n, a):
                    n[a] = evaluate(n[a])


def optimize(ast, level=1):
    if level >= 1:
        for n in ast.walk():
            if isinstance(n, ProcDef):
                n[Attr.BODY] = simplify(n[Attr.BODY])
    if level >= 2:
        fold_constants(ast)
