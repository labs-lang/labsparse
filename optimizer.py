import operator
from functools import reduce
from inspect import signature
from labs_ast import Composition, Guarded, ProcDef



def evaluate(node):
    """Evaluate an expression node."""
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
    if node in NodeType.IF:
        cond = evaluate(node[Attr.CONDITION])
        if cond in NodeType.LITERAL and cond[Attr.TYPE] == "bool":
            return evaluate(
                node[Attr.THEN]
                if cond[Attr.VALUE]
                else node[Attr.ELSE])

    if node in NodeType.EXPR or node in NodeType.BUILTIN:
        operands = [evaluate(n) for n in node[Attr.OPERANDS]]
        literals, others = (
            [n[Attr.VALUE] for n in operands if n in NodeType.LITERAL],
            [n for n in operands if n not in NodeType.LITERAL])
        fn = operators[node[Attr.NAME]]
        min_args = len(signature(fn).parameters)
        if len(literals) >= min_args:
            value = (
                fn(literals[0])
                if min_args == 1
                else reduce(fn, literals))

            new_node = {
                Attr.NODE_TYPE: NodeType.LITERAL,
                Attr.TYPE: "bool" if isinstance(value, bool) else "int",
                Attr.VALUE: value,
                Attr.PATH: node[Attr.PATH],
                Attr.LN: node[Attr.LN],
                Attr.COL: node[Attr.COL],
                Attr.SYNTHETIC: True
            }
            if others:
                others.append(new_node)
                node[Attr.OPERANDS] = others
                return node
            else:
                return new_node
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
    elif isinstance(proc, Guarded):
        proc[Attr.BODY] = simplify(proc[Attr.BODY])
        return proc
    else:
        return proc


def fold_constants(ast):
    """Performs constant folding on the given AST."""
    for n in walk(ast):
        for a in (Attr.OPERANDS, Attr.RHS):
            if a in n and n not in NodeType.COMPOSITION:
                n[a] = [evaluate(x) for x in n[a]]
        for a in (Attr.CONDITION, Attr.THEN, Attr.ELSE):
            if a in n:
                n[a] = evaluate(n[a])


def optimize(ast, level=1):
    if level >= 1:
        for a in ast["agents"]:
            for n in a.walk():
                if isinstance(n, ProcDef):
                    n[Attr.BODY] = simplify(n[Attr.BODY])
    if level >= 2:
        fold_constants(ast)
