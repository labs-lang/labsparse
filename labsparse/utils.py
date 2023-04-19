from .optimizer import simplify_expr
from .labs_ast import (Attr, Expr, Literal, NodeType)
from copy import deepcopy


def eliminate_quantifiers(qformula, info):
    if not qformula(NodeType.QFORMULA):
        # Nothing to do
        return qformula

    current_formula = qformula[Attr.CONDITION]
    for qvar in qformula[Attr.QVARS]:
        operands = []
        for tid in info.spawn.tids(qvar[Attr.TYPE]):
            f = deepcopy(current_formula)
            for node, parent, attr, idx in f.walk_with_handle():
                if node(NodeType.REF) and (
                    node[Attr.NAME] == qvar[Attr.NAME] or (
                        node[Attr.NAME] == "id" and
                        node[Attr.OF] is not None and
                        node[Attr.OF](NodeType.REF) and
                        node[Attr.OF][Attr.NAME] == qvar[Attr.NAME])
                ):
                    lit = Literal.new(node, "int", tid)
                    if idx is None:
                        parent[attr] = lit
                    else:
                        parent[attr][idx] = lit
            operands.append(f)
        current_formula = Expr(qformula[Attr.PATH], qformula[Attr.LN], qformula[Attr.COL], {})  # noqa: E501
        current_formula[Attr.NAME] = "and" if qvar[Attr.QUANTIFIER] == "forall" else "or"  # noqa: E501
        current_formula[Attr.OPERANDS] = operands
        current_formula = simplify_expr(current_formula)
    return current_formula


def _map_refs(node, ref_fn):
    def recurse(n):
        return _map_refs(n, ref_fn)
    if node(NodeType.REF) or node(NodeType.REF_EXT):
        return ref_fn(node)
    elif node(NodeType.EXPR) or node(NodeType.BUILTIN) or node(NodeType.COMPARISON):  # noqa: E501
        ops = [recurse(x) for x in node[Attr.OPERANDS]]
        node[Attr.OPERANDS] = ops
        return node
    elif node(NodeType.QFORMULA):
        cond = recurse(node[Attr.CONDITION])
        node[Attr.CONDITION] = cond
        return node
    else:
        return node


def replace_externs(f, externs, with_underscores=False):
    idx = 0 if with_underscores else 1
    # if the externs dictionary keys are in the form "_name", 
    # then with_underscores must be set to True

    def ref_fn(node):
        if node(NodeType.REF_EXT) and (node[Attr.NAME][idx:]) in externs:
            new = Literal.new(node, "int", externs[node[Attr.NAME][idx:]])
            return new
        else:
            return node
    return _map_refs(f, ref_fn)
