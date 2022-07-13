#!/usr/bin/env python3

from curses.ascii import DEL
import pyparsing as pp
from pyparsing import (Combine, FollowedBy, Forward, Group, Keyword, Literal,
                       OneOrMore, Optional, ParserElement, Suppress, Word,
                       ZeroOrMore, alphanums, alphas, delimitedList,
                       infixNotation, oneOf, opAssoc)
from pyparsing import pyparsing_common as ppc
from pyparsing import pythonStyleComment, ungroup

from message import FatalException, Message

ParserElement.enablePackrat()
# sys.tracebacklimit = 0


def oneOfKw(lst):
    return oneOf(lst, asKeyword=True)


LBRACE, \
    RBRACE, LBRACK, RBRACK, EQ, COLON,\
    SEMICOLON, COMMA, LPAR, RPAR, RAWPREFIX = map(Suppress, "{}[]=:;,()$")

kw = "and or not true false forall exists pick where if then else"
# kws_lower = oneOfKw(kw)
BUILTIN = oneOfKw("abs mix max")
QUANTIFIER = oneOfKw("forall exists")
MODALITY = oneOfKw("always eventually fairly fairly_inf finally")
kws_upper = oneOfKw("Skip Nil")


def make_node(s, loc, toks):
    try:
        ast_type = toks.getName() or toks[0][1]
    except KeyError:
        return toks[0]
    if isinstance(ast_type, dict) and toks[0][0] == "-":
        ast_type = "unary-minus"

    node = {
        "ast-type": ast_type,
        "ln": pp.lineno(loc, s),
        "col": pp.col(loc, s)
    }

    def add(*args):
        node.update({x: getattr(toks, x) for x in args if x in toks})

    def remove(*args):
        for x in (a for a in args if a in node):
            del node[x]

    to_add = {
        "agent": ("interface", "name", "processes"),
        "assume": ("assumptions",),
        "assumption": ("name", "quant"),
        "block": ("block",),
        "builtin": ("fn", "args"),
        "check": ("properties",),
        "choice": ("choice",),
        "comparison": ("e1", "e2"),
        "decl": ("var", "init-value", "init-range", "init-list"),
        "guarded": ("guard", "process"),
        "if": ("condition", "then", "else"),
        "par": ("par",),
        "procdef": ("name", "body"),
        "property": ("name", "modality", "quant"),
        "quant": ("qvars", "formula"),
        "qvar": ("name", "type", "quantifier"),
        "raw": ("fn", "args"),
        "ref": ("of", "offset"),
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

    add("name", *to_add.get(ast_type, []))
    remove(*to_remove.get(ast_type, []))
    if ast_type.startswith("assign-"):
        add("lhs", "rhs")

    for field in (
        "processes", "tuples", "spawn", "extern", "block",
        "lhs", "rhs", "args", "qvars", "operands",
        "assumptions", "properties"
    ):
        if field in node:
            if hasattr(node[field], "asList"):
                node[field] = node[field].asList()
            elif node[field] == "":
                node[field] = []
    if "name" in node and hasattr(node["name"], "asList"):
        node["name"] = node["name"][0]

    projections = {
        "extern": lambda t: {"extern": t.asList()},
        "spawn": lambda t: {"spawn": t.asList()},
        "interface": lambda t: {"interface": t.asList()},
        "values": lambda t: {"values": t.asList()},
        "decl": lambda t: {"init": t[1]},
        "ext": lambda t: {"name": t[0]},
        "unary-minus": lambda t: {"operand": t[0][1]},
        "init-range": lambda t: {"start": t[0], "bound": t[1]},
        "scalar": lambda t: {"name": t[ast_type]},
        "array": lambda t: {"name": t[ast_type]},
        "comparison": lambda t: {"ast-type": t.op},
        "procdef": lambda _: {"body": simplify(node["body"][0])},
        "and": lambda _: {"operands": toks[0][0::2]},
        "or": lambda _: {"operands": toks[0][0::2]},
        "init-value": lambda _: {"value": toks["init-value"]},
    }

    if ast_type in projections:
        node.update(projections[ast_type](toks))

    # -------------------------------------------------------------------------
    elif ast_type in "+-*/%:":
        node["operands"] = toks[0][0::2]
    elif ast_type.startswith("literal-"):
        node["ast-type"], node["type"] = ast_type.split("-")
        fn = {"int": int, "bool": bool}
        node["value"] = fn.get(node["type"], lambda x: x)(toks[ast_type])
    elif ast_type.startswith("ref"):
        if "offset" in node:
            node["offset"] = node["offset"][0]
        # validate_ref(node)
        if "of" in node:
            node["of"] = node["of"][0]
    # -------------------------------------------------------------------------

    return node


VARNAME = Word(alphas.lower(), alphanums + "_")
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
        (
            Keyword("if").suppress() + BEXPR("condition") +
            Keyword("then").suppress() + EXPR("then") +
            Keyword("else").suppress() + EXPR("else")
        )("if") |
        ppc.signed_integer("literal-int") |
        RAWCALL("raw") |
        (BUILTIN("fn") + LPAR + delimitedList(EXPR)("args") + RPAR)("builtin") |  # noqa: E501
        VARREF |
        EXTERN("ext")
    ).setParseAction(make_node)

    EXPR <<= infixNotation(EXPRATOM, [
        ("-", 1, opAssoc.RIGHT, make_node),
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
        FollowedBy(QUANTIFIER) +
        QUANTIFIER("quantifier") -
        IDENTIFIER("type") -
        VARNAME("name")
    )("qvar").setParseAction(make_node)

    QUANT <<= (
        delimitedList(QVAR)("qvars") - COMMA - BEXPR("formula")
    )("quant").setParseAction(make_node)

    return EXPR, BEXPR, QUANT


EXPR, BEXPR, QUANT = makeExprParsers(baseVarRefParser)
VARREF = baseVarRefParser(EXPR)
_, LINKBEXPR, _ = makeExprParsers(linkVarRefParser)


INITIALIZER = (
    (LBRACK + delimitedList(ungroup(EXPR)) + RBRACK)("init-list") |
    (ungroup(EXPR) + Suppress("..") + ungroup(EXPR))("init-range") |
    ungroup(EXPR)("init-value")
).setParseAction(make_node)

PICK = (
    Keyword("pick").suppress() +
    ppc.integer()("num") +
    pp.Optional(IDENTIFIER)("agent-type") +
    pp.Optional(Keyword("where").suppress() + LINKBEXPR)
)("pick").setParseAction(make_node)

LHS = (VARREF)("ref").setParseAction(make_node)
RHS = (  
    (pp.FollowedBy(QUANTIFIER) + QUANT)("quant") |
    PICK |
    ungroup(EXPR)
).setName("rhs expression")


ASSIGN = (
    (
        delimitedList(LHS)("lhs") +
        Suppress("<--") +
        delimitedList(RHS)("rhs")
    )("assign-env") |
    (
        delimitedList(VARREF)("lhs") +
        Suppress("<~") +
        delimitedList(RHS)("rhs")
    )("assign-lstig") |
    (
        delimitedList(LHS)("lhs") +
        Suppress("<-") +
        delimitedList(RHS)("rhs")
    )("assign-attr")
).setParseAction(make_node)


ASSIGN_BLOCK = (
    ASSIGN |
    (
        delimitedList(LHS)("lhs") +
        Suppress(":=") +
        delimitedList(RHS)("rhs")
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
        LBRACE -
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

DECLARATION = (
    LHS("var") + COLON + INITIALIZER
)("decl").setParseAction(make_node)

SYSTEM = (
    Keyword("system").suppress() + LBRACE + (
        Optional(named_list("extern", EXTERN, COMMA)) &
        Optional(named_list("environment", DECLARATION)) &
        Optional(named_list(
            "spawn",
            (IDENTIFIER("name") + COLON + EXPR("num"))("spawn-expression"),
            COMMA
        ))) +
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
        Optional(named_list("interface", DECLARATION))  # noqa: E501
        & Optional(named_list("stigmergies", IDENTIFIER))) +
    OneOrMore(PROCDEF)("processes") +
    RBRACE
)("agent").setParseAction(make_node)

ASSUME = (
    Keyword("assume").suppress() + LBRACE +
    # SkipTo(RBRACE) +
    ZeroOrMore((
        IDENTIFIER("name") + EQ + QUANT("quant")
    )("assumption"))("assumptions") +
    RBRACE
).setParseAction(make_node)

CHECK = (
    Keyword("check").suppress() + LBRACE +
    ZeroOrMore((
        IDENTIFIER("name") + EQ +
        MODALITY("modality") + QUANT("quant")
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


def parse_to_dict(path) -> dict:
    try:
        with open(path) as fp:
            p = FILE.parseFile(fp, parseAll=True)
        return {
            "<meta>": {
                "path": path,
            },
            "system": p.system,
            "agents": p.agents.asList(),
            "stigmergies": p.stigmergies or [],
            "assume": p.assume or [],
            "check": p.check or []
        }
    except pp.ParseBaseException as e:
        raise FatalException(Message.wrap_exception(e, path))
