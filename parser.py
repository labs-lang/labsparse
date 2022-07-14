#!/usr/bin/env python3

from enum import Enum, auto, unique

import pyparsing as pp
from pyparsing import (Combine, FollowedBy, Forward, Group, Keyword, Literal,
                       OneOrMore, Optional, ParserElement, Suppress, Word,
                       ZeroOrMore, alphanums, alphas, delimitedList,
                       infixNotation, oneOf, opAssoc)
from pyparsing import pyparsing_common as ppc
from pyparsing import pythonStyleComment, ungroup


ParserElement.enablePackrat()

_path = ""


class StringEnum(str, Enum):
    def _generate_next_value_(name, *_):
        return name.lower().replace("_", "-")

    def __repr__(self) -> str:
        return f"'{self.value}'"

    # def __eq__(self, __o: object) -> bool:
    #     return isinstance(__o, StringEnum) and super().__eq__(__o)

    def __hash__(self) -> int:
        return super().__hash__()


@unique
class Attr(StringEnum):
    """Attribute names of AST nodes.

    The str inheritance is needed for JSON serialization
    """
    NODE_TYPE = auto()
    LN = auto()
    COL = auto()
    PATH = auto()
    NAME = auto()
    BODY = auto()           # block, procdef
    PROCDEFS = auto()       # system, agent
    OPERANDS = auto()       # arithmetic, composition
    CONDITION = auto()      # if, guarded
    LOCATION = auto()       # assignment
    LHS = auto()            # assignment, comparison
    RHS = auto()            # assignment, comparison
    THEN = auto()           # if
    ELSE = auto()           # if
    OF = auto()             # ref
    OFFSET = auto()         # ref
    QUANTIFIER = auto()     # qvar
    SYNTHETIC = auto()      # (Used to mark synthetised nodes)
    TYPE = auto()           # literal
    VALUE = auto()          # literal


@unique
class NodeType(StringEnum):
    AGENT = auto()
    ASSIGN = auto()
    BLOCK = auto()
    BUILTIN = auto()
    COMPARISON = auto()
    COMPOSITION = auto()
    EXPR = auto()
    GUARDED = auto()
    IF = auto()
    LITERAL = auto()
    PROCDEF = auto()
    RAW_CALL = auto()
    REF = auto()
    REF_LINK = auto()

    def __contains__(self, __o: str) -> bool:
        return all((
            isinstance(__o, dict),
            Attr.NODE_TYPE in __o,
            __o[Attr.NODE_TYPE] == self))


def oneOfKw(lst):
    return oneOf(lst, asKeyword=True)


LBRACE, \
    RBRACE, LBRACK, RBRACK, EQ, COLON,\
    SEMICOLON, COMMA, LPAR, RPAR, RAWPREFIX = map(Suppress, "{}[]=:;,()$")

kw = "and or not true false forall exists pick where if then else"
BUILTIN = oneOfKw("abs mix max")
QUANTIFIER = oneOfKw("forall exists")
MODALITY = oneOfKw("always eventually fairly fairly_inf finally")
kws_upper = oneOfKw("Skip Nil")


def make_node(s, loc, toks):
    _t = toks.asDict()
    try:
        ast_type = toks.getName() or toks[0][1]
    except KeyError:
        return toks[0]

    if isinstance(ast_type, dict) and toks[0][0] == "-":
        _t[Attr.NAME] = "unary-minus"
        _t[Attr.OPERANDS] = [toks[0][1]]
        ast_type = NodeType.BUILTIN
    elif ast_type in ("seq", "choice", "par"):
        _t[Attr.NAME] = ast_type
        ast_type = NodeType.COMPOSITION
    elif ast_type in "+-*/%:" or ast_type in ("and", "or"):
        _t[Attr.NAME] = ast_type
        ast_type = NodeType.EXPR
    elif ast_type.startswith("literal-"):
        _t[Attr.TYPE] = ast_type.split("-")[-1]
        fn = {"int": int, "bool": bool}.get(_t[Attr.TYPE], lambda x: x)
        _t[Attr.VALUE] = fn(toks[ast_type])
        ast_type = NodeType.LITERAL

    node = {
        Attr.NODE_TYPE: ast_type,
        Attr.LN: pp.lineno(loc, s),
        Attr.COL: pp.col(loc, s),
        Attr.PATH: _path
    }

    def add(*args):
        node.update({x: _t[x] for x in args if x in _t})

    def remove(*args):
        for x in (a for a in args if a in node):
            del node[x]

    to_add = {
        "agent": ("interface", Attr.PROCDEFS),
        NodeType.ASSIGN: (Attr.LOCATION, Attr.LHS, Attr.RHS),
        "assume": ("assumptions",),
        "assumption": ("quant",),
        NodeType.BLOCK: (Attr.BODY,),
        NodeType.BUILTIN: (Attr.OPERANDS,),
        "check": ("properties",),
        NodeType.COMPARISON: (Attr.LHS, Attr.RHS),
        "decl": ("var", "init-value", "init-range", "init-list"),
        NodeType.GUARDED: (Attr.CONDITION, Attr.BODY),
        NodeType.IF: (Attr.CONDITION, Attr.THEN, Attr.ELSE),
        NodeType.LITERAL: (Attr.TYPE, Attr.VALUE),
        "property": ("modality", "quant"),
        "quant": ("qvars", "formula"),
        "qvar": (Attr.TYPE, Attr.QUANTIFIER),
        NodeType.RAW_CALL: (Attr.OPERANDS),
        NodeType.REF: (Attr.OF, Attr.OFFSET),
        NodeType.REF_LINK: (Attr.OF, Attr.OFFSET),
        "spawn-expression": ("num",),
        "spawn": ("spawn",),
        "stigmergy": ("link", "tuples"),
        "system": ("extern", "spawn", Attr.PROCDEFS),
    }
    to_remove = {
        "extern": (Attr.NAME,),
        "spawn": (Attr.NAME,)
    }

    add(Attr.NAME, *to_add.get(ast_type, []))
    remove(*to_remove.get(ast_type, []))

    for field in (
        Attr.OPERANDS, "tuples", "spawn", "extern", Attr.BODY,
        Attr.LHS, Attr.RHS, Attr.OPERANDS, "qvars", Attr.PROCDEFS,
        "assumptions", "properties"
    ):
        if field in node:
            if hasattr(node[field], "asList"):
                node[field] = node[field].asList()
            elif node[field] == "":
                node[field] = []
    if Attr.NAME in node and hasattr(node[Attr.NAME], "asList"):
        node[Attr.NAME] = node[Attr.NAME][0]

    projections = {
        "array": lambda t: {Attr.NAME: t[ast_type]},
        "call": lambda t: {Attr.NAME: t[0]},
        "composition": lambda _: {Attr.OPERANDS: _t[_t[Attr.NAME]]},
        "decl": lambda t: {"init": t[1]},
        "expr": lambda _: {Attr.OPERANDS: toks[0][0::2]},
        "ext": lambda t: {Attr.NAME: t[0]},
        "extern": lambda t: {"extern": t.asList()},
        "init-range": lambda t: {"start": t[0], "bound": t[1]},
        "init-value": lambda _: {"value": toks["init-value"]},
        "interface": lambda t: {"interface": t.asList()},
        "procdef": lambda t: {Attr.BODY: t[Attr.BODY].asList()[0]},
        "scalar": lambda t: {Attr.NAME: t[ast_type]},
        "spawn": lambda t: {"spawn": t.asList()},
        "values": lambda t: {"values": t.asList()},
    }

    if ast_type in projections:
        node.update(projections[ast_type](toks))
    elif ast_type in (NodeType.REF, NodeType.REF_LINK):
        if Attr.OFFSET in node:
            node[Attr.OFFSET] = node[Attr.OFFSET][0]
        if Attr.OF in node:
            node[Attr.OF] = node[Attr.OF][0]
    return node


VARNAME = Word(alphas.lower(), alphanums + "_")
IDENTIFIER = Word(alphas.upper(), alphanums + "_")
EXTERN = Combine(Literal("_") + VARNAME)(Attr.NAME).setParseAction(lambda t: t[0])  # noqa: E501


def named_list(name, thing, sep=SEMICOLON):
    return (
        Keyword(name).suppress() + EQ + delimitedList(thing, sep)
    )(name).setParseAction(make_node)


def offset(pexpr):
    return LBRACK + pexpr + RBRACK


def baseVarRefParser(pexpr):
    return (
        VARNAME(Attr.NAME) +
        Optional(
            offset(pexpr)(Attr.OFFSET) ^
            (LBRACK + RBRACK).setParseAction(lambda _: True)("array")
        ) +
        Optional(Keyword("of").suppress() + pexpr)(Attr.OF)
    )(NodeType.REF)


def linkVarRefParser(pexpr):
    return (
        VARNAME(Attr.NAME) +
        Optional(offset(pexpr))(Attr.OFFSET) +
        (Keyword(Attr.OF).suppress() + oneOfKw("1 2 c1 c2"))(Attr.OF)
    )("ref-link")


def makeExprParsers(pvarrefMaker):
    expr = Forward()
    bexpr = Forward()
    quant = Forward()
    var_ref = pvarrefMaker(expr)

    raw_fn_name = Word(alphas + "_", alphanums + "_")
    raw_fn_call = (
        Combine(RAWPREFIX + raw_fn_name + LPAR)(Attr.NAME)
        + Optional(delimitedList(expr))(Attr.OPERANDS)
        + RPAR
    )

    expr_atom = (
        (
            Keyword("if").suppress() + bexpr(Attr.CONDITION) +
            Keyword("then").suppress() + expr(Attr.THEN) +
            Keyword("else").suppress() + expr(Attr.ELSE)
        )(NodeType.IF) |
        ppc.signed_integer("literal-int") |
        raw_fn_call(NodeType.RAW_CALL) |
        (
            BUILTIN(Attr.NAME) + LPAR +
            Optional(delimitedList(expr)(Attr.OPERANDS)) + RPAR
        )(NodeType.BUILTIN) |  # noqa: E501
        var_ref |
        EXTERN("ext")
    ).setParseAction(make_node)

    expr <<= infixNotation(expr_atom, [
        ("-", 1, opAssoc.RIGHT, make_node),
        ("%", 2, opAssoc.LEFT, make_node),
        ("*", 2, opAssoc.LEFT, make_node),
        ("/", 2, opAssoc.LEFT, make_node),
        (":", 2, opAssoc.LEFT, make_node),
        ("+", 2, opAssoc.LEFT, make_node),
        ("-", 2, opAssoc.LEFT, make_node)])

    bexpr_atom = (
        oneOfKw("true false")("literal-bool") |
        (expr(Attr.LHS) + oneOf("> < = >= <= !=")(Attr.NAME) + expr(Attr.RHS))(NodeType.COMPARISON) ^  # noqa: E501
        expr
    ).setParseAction(make_node)

    bexpr <<= infixNotation(bexpr_atom, [
        (Keyword("and"), 2, opAssoc.LEFT, make_node),
        (Keyword("or"), 2, opAssoc.LEFT, make_node)
        ])

    quant_var = (
        FollowedBy(QUANTIFIER) +
        QUANTIFIER(Attr.QUANTIFIER) -
        IDENTIFIER(Attr.TYPE) -
        VARNAME(Attr.NAME)
    )("qvar").setParseAction(make_node)

    quant <<= (
        delimitedList(quant_var)("qvars") - COMMA - bexpr("formula")
    )("quant").setParseAction(make_node)

    return expr, bexpr, quant


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

LHS = (VARREF)(NodeType.REF).setParseAction(make_node)
RHS = (
    (pp.FollowedBy(QUANTIFIER) + QUANT)("quant") |
    PICK |
    ungroup(EXPR)
).setName("rhs expression")

ASSIGN_OP = (
    Literal("<--").setParseAction(lambda _: "environment") |
    Literal("<~").setParseAction(lambda _: "stigmergy") |
    Literal("<-").setParseAction(lambda _: "interface") |
    Literal(":=").setParseAction(lambda _: "local")
).setName("assignment operator")


ASSIGN = ungroup(
    (
        delimitedList(LHS)(Attr.LHS) +
        ASSIGN_OP(Attr.LOCATION) +
        # Literal("<--").setParseAction(lambda _: "environment")(Attr.LOCATION) +
        delimitedList(RHS)(Attr.RHS)
    )(NodeType.ASSIGN)
    # |
    # (
    #     delimitedList(VARREF)(Attr.LHS) +
    #     Suppress("<~")(Attr.LOCATION).setParseAction(lambda _: "stigmergy") +
    #     delimitedList(RHS)(Attr.RHS)
    # )("assign") |
    # (
    #     delimitedList(LHS)(Attr.LHS) +
    #     Literal("<-")(Attr.LOCATION).setParseAction(lambda _: "interface") +
    #     delimitedList(RHS)(Attr.RHS)
    # )("assign")
).setParseAction(make_node)


# ASSIGN_BLOCK = (
#     ASSIGN |
#     (
#         delimitedList(LHS)(Attr.LHS) +
#         Suppress(":=") +
#         delimitedList(RHS)(Attr.RHS)
#     )("assign").setParseAction(make_node)
# )

PROC = Forward()
PROCBASE = (
    (
        FollowedBy(BEXPR) + BEXPR(Attr.CONDITION) + Suppress("->") +
        PROC(Attr.BODY)
    )(NodeType.GUARDED) |
    Keyword("Skip")("Skip") |
    IDENTIFIER("call") |
    ASSIGN |
    (
        LBRACE -
        delimitedList(ASSIGN, SEMICOLON)(Attr.BODY) +
        RBRACE
    )(NodeType.BLOCK)
).setParseAction(make_node)

PAREN = (LPAR + PROC + RPAR)("paren")
SEQ = (delimitedList(PAREN | PROCBASE, SEMICOLON))("seq").setParseAction(make_node)  # noqa: E501
CHOICE = delimitedList(SEQ, Literal("++"))("choice").setParseAction(make_node)
PROC <<= delimitedList(CHOICE, Literal("||"))("par").setParseAction(make_node)

PROCDEF = (
    IDENTIFIER(Attr.NAME) + EQ + PROC(Attr.BODY)
)(NodeType.PROCDEF).setParseAction(make_node)

DECLARATION = (
    LHS("var") + COLON + INITIALIZER
)("decl").setParseAction(make_node)

SYSTEM = (
    Keyword("system").suppress() + LBRACE + (
        Optional(named_list("extern", EXTERN, COMMA)) &
        Optional(named_list("environment", DECLARATION)) &
        Optional(named_list(
            "spawn",
            (IDENTIFIER(Attr.NAME) + COLON + EXPR("num"))("spawn-expression"),
            COMMA
        ))) +
    ZeroOrMore(PROCDEF)(Attr.OPERANDS) +
    RBRACE
).setParseAction(make_node)

TUPLEDEF = (
    Group(delimitedList(VARNAME)) + COLON +
    Group(delimitedList(INITIALIZER))
).setParseAction(lambda toks: list(zip(toks[0], toks[1])))

STIGMERGY = (
    Keyword("stigmergy").suppress() + IDENTIFIER(Attr.NAME) +
    LBRACE +
    Keyword("link").suppress() + EQ + LINKBEXPR("link") +
    ZeroOrMore(Group(TUPLEDEF))("tuples") +
    # SkipTo(RBRACE).suppress() +
    RBRACE
)("stigmergy").setParseAction(make_node)

AGENT = (
    Keyword("agent").suppress() + IDENTIFIER(Attr.NAME) +
    LBRACE + (
        Optional(named_list("interface", DECLARATION))  # noqa: E501
        & Optional(named_list("stigmergies", IDENTIFIER))) +
    ZeroOrMore(PROCDEF)(Attr.PROCDEFS) +
    RBRACE
)("agent").setParseAction(make_node)

ASSUME = (
    Keyword("assume").suppress() + LBRACE +
    # SkipTo(RBRACE) +
    ZeroOrMore((
        IDENTIFIER(Attr.NAME) + EQ + QUANT("quant")
    )("assumption"))("assumptions") +
    RBRACE
).setParseAction(make_node)

CHECK = (
    Keyword("check").suppress() + LBRACE +
    ZeroOrMore((
        IDENTIFIER(Attr.NAME) + EQ +
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


def parse_to_dict(path) -> dict:
    global _path
    _path = str(path)
    with open(path) as fp:
        p = FILE.parseFile(fp, parseAll=True)
    return {
        "system": p.system,
        "agents": p.agents.asList(),
        "stigmergies": p.stigmergies or [],
        "assume": p.assume or [],
        "check": p.check or []
    }
