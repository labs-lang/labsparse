#!/usr/bin/env python3

import pyparsing as pp
from pyparsing import (Combine, FollowedBy, Forward, Group, Keyword, Literal,
                       OneOrMore, Optional, ParserElement, ParseResults,
                       Suppress, Word, ZeroOrMore, alphanums, alphas,
                       delimitedList, infixNotation, oneOf, opAssoc)
from pyparsing import pyparsing_common as ppc
from pyparsing import pythonStyleComment, ungroup

ParserElement.enablePackrat()
# sys.tracebacklimit = 0


def oneOfKw(lst):
    return oneOf(lst, asKeyword=True)


LBRACE, \
    RBRACE, LBRACK, RBRACK, EQ, COLON,\
    SEMICOLON, COMMA, LPAR, RPAR, RAWPREFIX = map(Suppress, "{}[]=:;,()$")

kw = "and or not true false forall exists pick where if then else"
kws_lower = oneOfKw(kw)
BUILTIN = oneOfKw("abs mix max")
QUANTIFIER = oneOfKw("forall exists")
MODALITY = oneOfKw("always eventually fairly fairly_inf finally")
kws_upper = oneOfKw("Skip Nil")


def validate_assign(node):
    result = []
    for var in node["lhs"]:
        if var["name"] == "id":
            result.append("Cannot assign to 'id'")
    if len(node["lhs"]) != len(node["rhs"]):
        result.append(
            "Invalid number of expressions "
            f'(expected {len(node["lhs"])}, got {len(node["rhs"])})'
        )
    if len(node["lhs"]) > 1:
        for i, l1 in enumerate(node["lhs"][:-1]):
            name = l1["name"]
            for _, l2 in enumerate(node["lhs"][i+1:]):
                if name == l2["name"]:
                    result.append(
                        f"Variable '{name}' assigned multiple times"
                    )
    if result:
        node["messages"] = result


def validate_ref(node):
    if node["name"] in kw.split(" "):
        node["messages"] = [f"Unexpected keyword '{node['name']}'"]


def make_node(s, loc, toks):
    try:
        name = toks.getName() or toks[0][1]
    except KeyError:
        return toks[0]
    node = {"ast-type": name, "ln": pp.lineno(loc, s), "col": pp.col(loc, s)}

    def add(*args):
        node.update({x: getattr(toks, x) for x in args if x in toks})

    def remove(*args):
        args = (x for x in args if x in node)
        for x in args:
            del node[x]

    to_add = {
        "agent": ("interface", "name", "processes"),
        "assume": ("assumptions",),
        "assumption": ("name", "quant"),
        "block": ("block",),
        "builtin": ("fn", "args"),
        "check": ("properties",),
        "choice": ("choice",),
        "guarded": ("guard", "process"),
        "if": ("condition", "then", "else"),
        "par": ("par",),
        "procdef": ("name", "body"),
        "property": ("name", "modality", "quant"),
        "quant": ("qvars", "formula"),
        "qvar": ("name", "type", "quantifier"),
        "raw": ("fn", "args"),
        "seq": ("seq",),
        "spawn-expression": ("num",),
        "spawn": ("spawn",),
        "stigmergy": ("link", "tuples"),
        "system": ("extern", "spawn", "processes"),
    }
    to_remove = {
        "extern": ("name",),
        "spawn": ("name",)
    }

    add("name", *to_add.get(name, []))
    remove(*to_remove.get(name, []))
    if name.startswith("assign-"):
        add("lhs", "rhs")

    to_listify = (
        "processes", "tuples", "spawn", "extern", "block", "lhs", "rhs"
    )

    for field in to_listify:
        if field in node and hasattr(node[field], "asList"):
            node[field] = node[field].asList()

    populate_from_toks = {
        "extern": "extern",
        "spawn": "spawn",
        "interface": "interface",
        "init-list": "values",
    }

    if name in populate_from_toks:
        node[populate_from_toks[name]] = toks.asList()

    # -------------------------------------------------------------------------
    if name in "+-*/%:" or name in ("and", "or"):
        node["operands"] = toks[0][0::2]
    elif name in ("scalar", "array"):
        node["name"] = toks[name]
    elif name == "comparison":
        node["ast-type"] = toks.op
    elif name.startswith("literal-"):
        node["ast-type"], node["type"] = name.split("-")
        fn = {"int": int, "bool": bool}
        node["value"] = fn.get(node["type"], lambda x: x)(toks[name])
    elif name == "ext":
        node["name"] = toks[0]
    elif name == "procdef":
        node["body"] = simplify(node["body"][0])
    elif name == "init-range":
        node["start"] = toks[0]
        node["bound"] = toks[1]
    elif name == "init-value":
        node["value"] = toks["init-value"]
    elif name.startswith("ref"):
        if "offset" in node:
            node["offset"] = node["offset"][0]
        validate_ref(node)
        if "of" in node:
            node["of"] = node["of"][0]
    # -------------------------------------------------------------------------

    # Validation
    validation_fn = (
        (name.startswith("assign-"), validate_assign),
        (name.startswith("ref"), validate_ref)
    )
    for guard, fn in validation_fn:
        if guard:
            fn(node)

    return node


VARNAME = Word(alphas.lower(), alphanums + "_")  # .setParseAction(fail_on_kw)
IDENTIFIER = Word(alphas.upper(), alphanums + "_")
EXTERN = Combine(Literal("_") + VARNAME)("name").setParseAction(lambda t: t[0])


def named_list(name, thing, sep=SEMICOLON):
    return (
        Keyword(name).suppress() + EQ + delimitedList(thing, sep)
    )(name).setParseAction(make_node)


def offset(pexpr):
    return LBRACK + pexpr + RBRACK


def baseVarRefParser(pexpr):
    return (
        VARNAME("name") +
        Optional(
            offset(pexpr)("offset") ^
            (LBRACK + RBRACK).setParseAction(lambda _: True)("array")
        ) +
        Optional(Keyword("of").suppress() + pexpr)("of")
    )("ref")


def linkVarRefParser(pexpr):
    return (
        VARNAME("name") +
        Optional(offset(pexpr))("offset") +
        (Keyword("of").suppress() + oneOfKw("1 2 c1 c2"))("of")
    )("ref-link")


def makeExprParsers(pvarrefMaker):
    EXPR = Forward()
    BEXPR = Forward()
    QUANT = Forward()
    VARREF = pvarrefMaker(EXPR)

    FUNCTIONNAME = Word(alphas + "_", alphanums + "_")
    RAWCALL = (
        Combine(RAWPREFIX + FUNCTIONNAME + LPAR)("fn")
        + Optional(delimitedList(EXPR))("args")
        + RPAR
    )

    EXPRATOM = (
        (pp.FollowedBy(QUANTIFIER) + QUANT)("quant") |
        (
            FollowedBy(Keyword("if")) +
            Keyword("if").suppress() + BEXPR("condition") +
            Keyword("then").suppress() + EXPR("then") +
            Keyword("else").suppress() + EXPR("else")
        )("if") |
        ppc.signed_integer("literal-int") |
        RAWCALL("raw") |
        (BUILTIN("fn") + LPAR + delimitedList(EXPR)("args") + RPAR)("builtin") |  # noqa: E501
        (VARREF ^ VARNAME) |
        EXTERN("ext")
    ).setParseAction(make_node)

    EXPR <<= infixNotation(EXPRATOM, [
        ("%", 2, opAssoc.LEFT, make_node),
        ("*", 2, opAssoc.LEFT, make_node),
        ("/", 2, opAssoc.LEFT, make_node),
        (":", 2, opAssoc.LEFT, make_node),
        ("+", 2, opAssoc.LEFT, make_node),
        ("-", 2, opAssoc.LEFT, make_node)])

    BEXPRATOM = (
        oneOfKw("true false")("literal-bool") |
        (EXPR("e1") + oneOf("> < = >= <= !=")("op") + EXPR("e2"))("comparison") ^  # noqa: E501
        EXPR
    ).setParseAction(make_node)

    BEXPR <<= infixNotation(BEXPRATOM, [
        (Keyword("and"), 2, opAssoc.LEFT, make_node),
        (Keyword("or"), 2, opAssoc.LEFT, make_node)
        ])

    QVAR = (
        QUANTIFIER("quantifier") +
        IDENTIFIER("type") +
        VARNAME("name")
    )("qvar").setParseAction(make_node)

    QUANT <<= (
        (Optional(delimitedList(QVAR)("qvars") + COMMA) + BEXPR("formula"))
    )("quant")

    return EXPR, BEXPR, QUANT


EXPR, BEXPR, QUANT = makeExprParsers(baseVarRefParser)
VARREF = baseVarRefParser(EXPR)
_, LINKBEXPR, _ = makeExprParsers(linkVarRefParser)

# QVAR = (
#     QUANTIFIER("quantifier") +
#     IDENTIFIER("type") +
#     VARNAME("name")
# )("qvar").setParseAction(make_node)

# QUANT = (
#     delimitedList(QVAR)("qvars") + COMMA + BEXPR("formula") ^
#     BEXPR("formula")
# )("quant").setParseAction(make_node)


INITIALIZER = (
    (LBRACK + delimitedList(ungroup(EXPR)) + RBRACK)("init-list") |
    (ungroup(EXPR) + Suppress("..") + ungroup(EXPR))("init-range") |
    ungroup(EXPR)("init-value")
).setParseAction(make_node)


LHS = (VARREF)("ref").setParseAction(make_node)

ASSIGN = (
    (
        delimitedList(LHS)("lhs") +
        Suppress("<--") +
        delimitedList(ungroup(EXPR))("rhs")
    )("assign-env") |
    (
        delimitedList(VARREF)("lhs") +
        Suppress("<~") +
        delimitedList(ungroup(EXPR))("rhs")
    )("assign-lstig") |
    (
        delimitedList(LHS)("lhs") +
        Suppress("<-") +
        delimitedList(ungroup(EXPR))("rhs")
    )("assign-attr")
).setParseAction(make_node)

PICK = (
    Keyword("pick").suppress() +
    ppc.integer()("num") +
    pp.Optional(IDENTIFIER)("agent-type") +
    pp.Optional(Keyword("where").suppress() + LINKBEXPR)
)("pick").setParseAction(make_node)

ASSIGN_BLOCK = (
    ASSIGN |
    (
        delimitedList(LHS)("lhs") +
        Suppress(":=") +
        delimitedList(PICK ^ ungroup(EXPR))("rhs")
    )("assign-local").setParseAction(make_node)
)

PROC = Forward()
PROCBASE = (
    (
        FollowedBy(BEXPR) + BEXPR("guard") + Suppress("->") +
        PROC("process")
    )("guarded") |
    Keyword("Skip")("Skip") |
    IDENTIFIER("call") |
    ASSIGN |
    (
        LBRACE +
        delimitedList(ASSIGN_BLOCK, SEMICOLON) +
        RBRACE
    )("block")
).setParseAction(make_node)

PAREN = (LPAR + PROC + RPAR)("paren")
SEQ = (delimitedList(PAREN | PROCBASE, SEMICOLON))("seq").setParseAction(make_node)  # noqa: E501
CHOICE = delimitedList(SEQ, Literal("++"))("choice").setParseAction(make_node)
PROC <<= delimitedList(CHOICE, Literal("||"))("par").setParseAction(make_node)

PROCDEF = (
    IDENTIFIER("name") + EQ + PROC("body")
)("procdef").setParseAction(make_node)

SYSTEM = (
    Keyword("system").suppress() + LBRACE + (
        Optional(named_list("extern", EXTERN, COMMA)) &
        Optional(named_list("environment", VARREF + COLON + INITIALIZER))  # noqa: E501
    ) +
    named_list(
        "spawn",
        (IDENTIFIER("name") + COLON + EXPR("num"))("spawn-expression"),
        COMMA
    ) +
    ZeroOrMore(PROCDEF)("processes") +
    RBRACE
).setParseAction(make_node)

TUPLEDEF = (
    Group(delimitedList(VARNAME)) + COLON +
    Group(delimitedList(INITIALIZER))
).setParseAction(lambda toks: list(zip(toks[0], toks[1])))

STIGMERGY = (
    Keyword("stigmergy").suppress() + IDENTIFIER("name") +
    LBRACE +
    Keyword("link").suppress() + EQ + LINKBEXPR("link") +
    ZeroOrMore(Group(TUPLEDEF))("tuples") +
    # SkipTo(RBRACE).suppress() +
    RBRACE
)("stigmergy").setParseAction(make_node)

AGENT = (
    Keyword("agent").suppress() + IDENTIFIER("name") +
    LBRACE + (
        Optional(named_list("interface", Group(LHS + COLON + INITIALIZER)))  # noqa: E501
        & Optional(named_list("stigmergies", IDENTIFIER))) +
    OneOrMore(PROCDEF)("processes") +
    RBRACE
)("agent").setParseAction(make_node)

ASSUME = (
    Keyword("assume").suppress() + LBRACE +
    # SkipTo(RBRACE) +
    ZeroOrMore((
        IDENTIFIER("name") + EQ + QUANT("quant").setParseAction(make_node)
    )("assumption").setParseAction(make_node))("assumptions") +
    RBRACE
).setParseAction(make_node)

CHECK = (
    Keyword("check").suppress() + LBRACE +
    ZeroOrMore((
        IDENTIFIER("name") + EQ +
        MODALITY("modality") + QUANT("quant").setParseAction(make_node)
        )("property").setParseAction(make_node)
    )("properties") +
    RBRACE
).setParseAction(make_node)

FILE = (
    SYSTEM("system") +
    ZeroOrMore(STIGMERGY)("stigmergies") +
    OneOrMore(AGENT)("agents") +
    Optional(ASSUME("assume")) +
    CHECK("check")
).ignore(pythonStyleComment)


def simplify(proc):
    """Simplifies a LAbS process tree"""
    if isinstance(proc, dict):
        if proc["ast-type"] in ("seq", "choice", "par"):
            if len(proc[proc["ast-type"]]) == 1:
                return simplify(proc[proc["ast-type"]][0])
            else:
                # x = {**proc}
                x = proc
                x[x["ast-type"]] = [
                    simplify(p)
                    for p in x[x["ast-type"]]
                ]
                return x
        elif proc["ast-type"] == "guarded":
            proc["process"] = simplify(proc["process"])
            return proc
        else:
            return proc
    else:
        return proc


if __name__ == "__main__":
    with open("../../../labs-examples/isola2022/s4.labs") as fp:
    # with open("../../../labs-examples/boids-aw.labs") as fp:
        p = FILE.parseFile(fp, parseAll=True)
