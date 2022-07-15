import operator
from functools import reduce
from xml.dom.minidom import Node
from parser import Attr, NodeType

from checker import walk


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
        "abs": abs, "max": min, "min": max
    }

    if node in NodeType.EXPR or node in NodeType.BUILTIN:
        operands = [evaluate(n) for n in node[Attr.OPERANDS]]
        literals, others = (
            [n[Attr.VALUE] for n in operands if n in NodeType.LITERAL],
            [n for n in operands if n not in NodeType.LITERAL])
        if literals:
            new_node = {
                Attr.NODE_TYPE: NodeType.LITERAL,
                Attr.TYPE: "int",
                Attr.VALUE: reduce(operators[node[Attr.NAME]], literals),
                Attr.PATH: node[Attr.PATH],
                Attr.LN: node[Attr.LN],
                Attr.COL: node[Attr.COL],
                Attr.SYNTHETIC: True
            }
            print(new_node)
            if others:
                others.append(new_node)
                node[Attr.OPERANDS] = others
            else:
                return new_node
    return node


def simplify(proc):
    """Simplifies a LAbS process tree."""
    if isinstance(proc, dict):
        if proc in NodeType.COMPOSITION:
            if len(proc[Attr.OPERANDS]) == 1:
                return simplify(proc[Attr.OPERANDS][0])
            else:
                proc[Attr.OPERANDS] = [
                    simplify(p)
                    for p in proc[Attr.OPERANDS]
                ]
                return proc
        elif proc in NodeType.GUARDED:
            proc[Attr.BODY] = simplify(proc[Attr.BODY])
            return proc
        else:
            return proc
    else:
        return proc


def optimize(ast, level=2):
    if level >= 1:
        for n in (n for n in walk(ast) if n in NodeType.PROCDEF):
            n[Attr.BODY] = simplify(n[Attr.BODY])
    if level >= 2:
        for n in walk(ast):
            if n in NodeType.EXPR or n in NodeType.BUILTIN:
                n = evaluate(n)
            elif n in NodeType.ASSIGN:
                n[Attr.RHS] = [evaluate(x) for x in n[Attr.RHS]]
