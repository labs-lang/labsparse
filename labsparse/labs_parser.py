#!/usr/bin/env python3

import pyparsing as pp
from pyparsing import (Combine, FollowedBy, Forward, Keyword, Literal,
                       OneOrMore, Optional, ParserElement, ParseResults,
                       Suppress, Word, ZeroOrMore, alphanums, alphas,
                       delimitedList, infixNotation, oneOf, opAssoc)
from pyparsing import pyparsing_common as ppc
from pyparsing import pythonStyleComment, ungroup

from .labs_ast import Attr, Node, NodeType

ParserElement.enablePackrat()

_path = ""


def oneOfKw(lst):
    return oneOf(lst, asKeyword=True)


LBRACE, RBRACE, LBRACK, RBRACK, LPAR, RPAR = map(Suppress, "{}[]()")
COLON, SEMICOLON, COMMA, RAWPREFIX = map(Suppress, ":;,$")
EQ = ~Literal("=>") + Suppress("=")
EQ_literal = ~Literal("=>") + Literal("=")

kw = "and or not true false forall exists pick where if then else"
BUILTIN = oneOfKw("abs min max")
QUANTIFIER = oneOfKw("forall exists")
MODALITY = oneOfKw("always eventually fairly fairly_inf finally")
kws_upper = oneOfKw("Skip Nil")


def make_node(s: str, loc: int, toks: pp.ParseResults):
    """Turn a ParseResults object into an AST node.

    Args:
        s (str): Original string being parsed
        loc (int): Location of the matched results
        toks (pp.ParseResults): An object holding the parsed elements

    Returns:
        dict: An AST node
    """
    _t = toks.asDict()
    unary_ops = {"-": "unary-minus", "!": "unary-not"}

    try:
        ast_type = toks.getName() or toks[0][1]
        if isinstance(ast_type, Node) and toks[0][0] not in unary_ops:
            return ast_type
    except (KeyError, TypeError):
        return toks[0]

    try:
        if len(toks) == 0:
            pass
        elif isinstance(toks[0], ParseResults) and toks[0][0] in unary_ops:
            toks[Attr.NAME] = unary_ops[toks[0][0]]
            toks[Attr.OPERANDS] = [toks[0][1]]
            ast_type = NodeType.BUILTIN
        elif ast_type in ("seq", "choice", "par"):
            toks[Attr.NAME] = ast_type
            toks[Attr.OPERANDS] = _t[ast_type]
            ast_type = NodeType.COMPOSITION
        elif ast_type == "Skip":
            toks[Attr.NAME] = ast_type
            ast_type = NodeType.CALL
        elif ast_type in "+-*/%:" or ast_type in ("and", "or"):
            toks[Attr.NAME] = ast_type
            toks[Attr.OPERANDS] = toks[0][0::2]
            ast_type = NodeType.EXPR
        elif ast_type.startswith("literal-"):
            toks[Attr.TYPE] = ast_type.split("-")[-1]
            fn = {"int": int, "bool": bool}.get(toks[Attr.TYPE], lambda x: x)
            toks[Attr.VALUE] = fn(toks[ast_type])
            ast_type = NodeType.LITERAL
        elif ast_type == NodeType.GUARDED:
            ast_type = NodeType.GUARDED if "GUARD" in toks else NodeType.CONDITIONAL  # noqa: E501
        elif ast_type.startswith("init-") or ast_type.startswith("nondet-from"):  # noqa: E501
            toks[Attr.NAME] = ast_type
            toks[Attr.OPERANDS] = {
                "init-list": lambda t: t.asList(),
                "init-range": lambda t: [t[0], t[1]],
                "init-value": lambda t: t["init-value"],
                "nondet-from-range": lambda t: [t["e1"], t["e2"]]
            }[ast_type](toks)

            ast_type = NodeType.BUILTIN

        return Node.factory(
            _path, pp.lineno(loc, s), pp.col(loc, s), ast_type, toks)
    except KeyError:
        return toks
    # # For debugging:
    # except Exception as e:
    #     print(type(e), e, pp.lineno(loc, s), pp.col(loc, s))
    #     return toks


VARNAME = Word(alphas.lower(), alphanums + "_")
IDENTIFIER = Word(alphas.upper(), alphanums + "_")
EXTERN = Combine(
    Literal("_") + VARNAME(Attr.NAME)
)(NodeType.REF_EXT).setParseAction(make_node)


def named_list(name, thing, sep=SEMICOLON):
    return (
        Keyword(name).suppress() + EQ + delimitedList(thing, sep)
    )(name).setParseAction(make_node)


def offset(pexpr):
    return LBRACK + delimitedList(pexpr) + RBRACK


def baseVarRefParser(pexpr):
    return (
        ~(oneOfKw(kw)) +
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
        ungroup(Keyword(Attr.OF).suppress() + oneOfKw("1 2 c1 c2"))(Attr.OF)
    )(NodeType.REF_LINK)


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

    builtin_call = (
        BUILTIN(Attr.NAME) + LPAR +
        Optional(delimitedList(expr))(Attr.OPERANDS) + RPAR
    )

    nondet_value = (
        LBRACK + expr("e1") + Literal("..") + expr("e2") + RBRACK
    ).setResultsName("nondet-from-range")

    expr_atom = (
        (
            Keyword("if").suppress() + bexpr(Attr.CONDITION) +
            Keyword("then").suppress() + expr(Attr.THEN) +
            Keyword("else").suppress() + expr(Attr.ELSE)
        )(NodeType.IF).setParseAction(make_node) ^
        ppc.integer.setResultsName("literal-int").setParseAction(make_node) ^
        nondet_value("nondet-from-range").setParseAction(make_node) ^
        raw_fn_call(NodeType.RAW_CALL).setParseAction(make_node) ^
        builtin_call(NodeType.BUILTIN).setParseAction(make_node) ^
        var_ref.setParseAction(make_node) ^
        EXTERN.setParseAction(make_node)
    )

    expr <<= infixNotation(expr_atom, [
        ("-", 1, opAssoc.RIGHT, make_node),
        ("%", 2, opAssoc.LEFT, make_node),
        ("*", 2, opAssoc.LEFT, make_node),
        ("/", 2, opAssoc.LEFT, make_node),
        (":", 2, opAssoc.LEFT, make_node),
        ("+", 2, opAssoc.LEFT, make_node),
        ("-", 2, opAssoc.LEFT, make_node)])

    bop = EQ_literal | (~Literal("<-") + oneOf("<= <")) | oneOf('!= <= >= >')

    bexpr_atom = (
        oneOfKw("true false")("literal-bool") |
        (expr("cmp-lhs") + ungroup(bop)(Attr.NAME) + expr("cmp-rhs"))(NodeType.COMPARISON) ^  # noqa: E501
        expr("cmp-lhs")
    ).setParseAction(make_node)

    bexpr <<= infixNotation(bexpr_atom, [
        (Literal("!"), 1, opAssoc.RIGHT, make_node),
        (Keyword("and"), 2, opAssoc.LEFT, make_node),
        (Keyword("or"), 2, opAssoc.LEFT, make_node)
        ])

    quant_var = (
        FollowedBy(QUANTIFIER) +
        QUANTIFIER(Attr.QUANTIFIER) -
        IDENTIFIER(Attr.TYPE) -
        VARNAME(Attr.NAME)
    )(NodeType.QVAR).setParseAction(make_node)

    quant <<= (
        delimitedList(quant_var)(Attr.QVARS) + COMMA + bexpr(Attr.CONDITION)
    )(NodeType.QFORMULA).setParseAction(make_node)

    return expr, bexpr, quant


EXPR, BEXPR, QUANT = makeExprParsers(baseVarRefParser)
VARREF = baseVarRefParser(EXPR)
_, LINKBEXPR, _ = makeExprParsers(linkVarRefParser)


INITIALIZER = (
    (LBRACK + delimitedList(ungroup(EXPR)) + RBRACK)("init-list") |
    (ungroup(EXPR) + Suppress("..") + ungroup(EXPR))("init-range") |
    ungroup(EXPR)
).setParseAction(make_node)

PICK = (
    Keyword("pick").suppress() +
    ppc.integer()(Attr.VALUE) +
    pp.Optional(IDENTIFIER)(Attr.TYPE) +
    pp.Optional(
        ungroup(Keyword("where").suppress() + LINKBEXPR)
    )(Attr.CONDITION)
)(NodeType.PICK).setParseAction(make_node)

LHS = (VARREF)(NodeType.REF).setParseAction(make_node)
RHS = (
    (pp.FollowedBy(QUANTIFIER) + QUANT)(NodeType.QFORMULA) |
    PICK |
    ungroup(EXPR)
).setName("rhs expression")

ASSIGN_OP = (
    Literal("<--").setParseAction(lambda _: "environment") |
    Literal("<~").setParseAction(lambda _: "stigmergy") |
    Literal("<-").setParseAction(lambda _: "interface") |
    Literal(":=").setParseAction(lambda _: "local")
).setName("assignment operator")

ASSIGN = ungroup((
    delimitedList(LHS)(Attr.LHS) +
    ASSIGN_OP(Attr.LOCATION) +
    delimitedList(RHS)(Attr.RHS))(NodeType.ASSIGN)
).setParseAction(make_node)

PROC = Forward()
PROCBASE = ((
    FollowedBy(BEXPR) + BEXPR(Attr.CONDITION) +
    (Literal("->")("GUARD") | Literal("=>")("CONDITIONAL")) +
    PROC(Attr.BODY))(NodeType.GUARDED) ^
    Keyword("Skip")("Skip") |
    IDENTIFIER(NodeType.CALL) |
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
    LHS(Attr.VARIABLE) + COLON + INITIALIZER
)(NodeType.DECLARATION).setParseAction(make_node)

SYSTEM = (
    Keyword("system").suppress() + LBRACE + (
        Optional(named_list(Attr.EXTERN, EXTERN, COMMA)) &
        Optional(named_list("environment", DECLARATION)) &
        Optional(named_list(Attr.SPAWN, (
                IDENTIFIER(Attr.TYPE) + COLON + EXPR(Attr.VALUE)
            )(NodeType.SPAWN_DECLARATION).setParseAction(make_node),
            COMMA))
    ) +
    ZeroOrMore(PROCDEF)(Attr.PROCDEFS) +
    RBRACE
).setParseAction(make_node)

TUPLEDEF = (
    delimitedList(LHS)(Attr.VARIABLE) + COLON +
    delimitedList(INITIALIZER)(Attr.VALUE)
)(NodeType.TUPLE_DECL).setParseAction(make_node)

STIGMERGY = (
    Keyword("stigmergy").suppress() + IDENTIFIER(Attr.NAME) +
    LBRACE +
    Keyword("link").suppress() + EQ + LINKBEXPR(Attr.CONDITION) +
    ZeroOrMore(TUPLEDEF)(Attr.TUPLES) +
    # SkipTo(RBRACE).suppress() +
    RBRACE
)(NodeType.STIGMERGY).setParseAction(make_node)

AGENT = (
    Keyword("agent").suppress() + IDENTIFIER(Attr.NAME) +
    LBRACE + (
        Optional(named_list("interface", DECLARATION))
        & Optional(named_list("stigmergies", IDENTIFIER))) +
    ZeroOrMore(PROCDEF)(Attr.PROCDEFS) +
    RBRACE
)(NodeType.AGENT).setParseAction(make_node)

ASSUME = (
    Keyword("assume").suppress() + LBRACE +
    # SkipTo(RBRACE) +
    ZeroOrMore((
        IDENTIFIER(Attr.NAME) + EQ + ungroup(BEXPR | QUANT)(Attr.CONDITION)
    )(NodeType.PROPERTY_DEF).setParseAction(make_node))(Attr.PROPERTIES) +
    RBRACE
).setParseAction(make_node)

PROPERTY = (
    IDENTIFIER(Attr.NAME) + EQ +
    MODALITY(Attr.MODALITY) + ungroup(BEXPR | QUANT)(Attr.CONDITION)
)(NodeType.PROPERTY_DEF).setParseAction(make_node)

CHECK = (
    Keyword("check").suppress() + LBRACE +
    ZeroOrMore(PROPERTY)(Attr.PROPERTIES) +
    RBRACE
).setParseAction(make_node)

FILE = (
    SYSTEM(NodeType.SYSTEM) +
    ZeroOrMore(STIGMERGY)("stigmergies") +
    OneOrMore(AGENT)("agents") +
    Optional(ASSUME(NodeType.ASSUME)) +
    CHECK(NodeType.CHECK)
).ignore(pythonStyleComment)


def parse_to_dict(path) -> dict:
    global _path
    _path = str(path)
    with open(path) as fp:
        # p = FILE.setDebug().parseFile(fp, parseAll=True)
        p = FILE.parseFile(fp, parseAll=True)
    return Node.make_root(
        p.system,
        [] if p.agents == "" else p.agents.asList(),
        [] if p.stigmergies == "" else p.stigmergies.asList(),
        p.assume or [],
        p.check or [])
