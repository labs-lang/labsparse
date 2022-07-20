from enum import Enum, unique, auto
from typing import Any


class StringEnum(str, Enum):
    def _generate_next_value_(name, *_):
        return name.lower().replace("_", "-")

    def __repr__(self) -> str:
        return f"'{self}'"

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
    SYNTHETIC = auto()

    BODY = auto()           # block, procdef
    PROCDEFS = auto()       # system, agent
    OPERANDS = auto()       # arithmetic, composition, comparison
    CONDITION = auto()      # if, guarded, pick, property, qformula
    LOCATION = auto()       # assignment
    LHS = auto()            # assignment
    RHS = auto()            # assignment
    THEN = auto()           # if
    ELSE = auto()           # if
    MODALITY = auto()       # property-def
    OF = auto()             # ref
    OFFSET = auto()         # ref
    QUANTIFIER = auto()     # qvar
    QVARS = auto()          # qformula
    TYPE = auto()           # literal
    VALUE = auto()          # literal, declaration
    INTERFACE = auto()      # agent
    STIGMERGIES = auto()    # agent, system
    VARIABLE = auto()       # declaration
    EXTERN = auto()         # system
    SPAWN = auto()          # system
    PROPERTIES = auto()     # assume, check
    TUPLES = auto()         # stigmergy


@unique
class NodeType(StringEnum):
    AGENT = auto()
    ASSIGN = auto()
    ASSUME = auto()
    BLOCK = auto()
    BUILTIN = auto()
    CALL = auto()
    CHECK = auto()
    COMPARISON = auto()
    COMPOSITION = auto()
    DECLARATION = auto()
    EXPR = auto()
    REF_EXT = auto()
    GUARDED = auto()
    IF = auto()
    LITERAL = auto()
    PICK = auto()
    PROCDEF = auto()
    PROPERTY_DEF = auto()
    QFORMULA = auto()
    QVAR = auto()
    RAW_CALL = auto()
    REF = auto()
    REF_LINK = auto()
    SPAWN_DECLARATION = auto()
    STIGMERGY = auto()
    SYSTEM = auto()
    TUPLE_DECL = auto()

    def __contains__(self, __o: Any) -> bool:
        try:
            return __o[Attr.NODE_TYPE] == self
        except (TypeError, KeyError):
            return False


class Node:
    __slots__ = Attr.PATH, Attr.LN, Attr.COL
    AS_NODETYPE = None

    @classmethod
    def factory(cls, path, ln, col, node_type, toks):
        lookup = {
            x.AS_NODETYPE: x
            for x in cls.__subclasses__()
        }
        lookup[Attr.VARIABLE] = Ref

        return lookup[node_type](path, ln, col, toks)

    def walk(self):
        yield self
        for attr in self.__slots__:
            if isinstance(self[attr], Node):
                yield from self[attr].walk()
            elif isinstance(self[attr], list):
                for x in self[attr]:
                    if isinstance(x, Node):
                        yield from x.walk()

    def __getitem__(self, key):
        return self._get(key)

    def __setitem__(self, key, value):
        return self._set(key, value)

    def __init__(self, path, ln, col, toks) -> None:
        self._set(Attr.PATH, path)
        self._set(Attr.LN, ln)
        self._set(Attr.COL, col)
        for x in self.__slots__:
            try:
                attr = Attr(x)
                self._set(x, toks.get(attr))
            except (KeyError, ValueError):
                continue
        self._listify(
            Attr.EXTERN,
            Attr.INTERFACE,
            Attr.LHS,
            Attr.OPERANDS,
            Attr.PROCDEFS,
            Attr.PROPERTIES,
            Attr.RHS,
            Attr.SPAWN,
            Attr.STIGMERGIES,
            Attr.TUPLES,
            Attr.QVARS, default_empty=True)

    def _set(self, __attr: Attr, __value: Any) -> None:
        setattr(self, __attr, __value)

    def _get(self, __attr: Attr) -> Any:
        return getattr(self, __attr)

    def _listify(self, *attrs, default_empty=False):
        for attr in attrs:
            if hasattr(self, attr):
                if hasattr(self._get(attr), "asList"):
                    self._set(attr, self._get(attr).asList())
                elif self._get(attr) is None and default_empty:
                    self._set(attr, [])

    def serialize(self) -> dict:
        def handle(value):
            return value.serialize() if isinstance(value, Node) else value

        return {
            Attr.NODE_TYPE: self.AS_NODETYPE.value,
            **{k: handle(self._get(k)) for k in Node.__slots__},
            **{k: handle(self._get(k)) for k in self.__slots__}
        }

    def __repr__(self) -> str:
        return str(self.serialize())


class Agent(Node):
    __slots__ = Attr.INTERFACE, Attr.STIGMERGIES, Attr.PROCDEFS  # noqa: E501
    AS_NODETYPE = NodeType.AGENT


class Assign(Node):
    __slots__ = Attr.TYPE, Attr.LHS, Attr.RHS
    AS_NODETYPE = NodeType.ASSIGN

    def __init__(self, path, ln, col, toks) -> None:
        super().__init__(path, ln, col, toks)
        self._set(Attr.TYPE, toks[Attr.LOCATION])


class Assume(Node):
    __slots__ = (Attr.PROPERTIES,)
    AS_NODETYPE = NodeType.ASSUME


class Block(Node):
    __slots__ = (Attr.BODY,)
    AS_NODETYPE = NodeType.BLOCK

    def __init__(self, path, ln, col, toks) -> None:
        super().__init__(path, ln, col, toks)
        self._listify(Attr.BODY)


class Builtin(Node):
    __slots__ = Attr.NAME, Attr.OPERANDS
    AS_NODETYPE = NodeType.BUILTIN


class Call(Node):
    __slots__ = (Attr.NAME,)
    AS_NODETYPE = NodeType.CALL

    def __init__(self, path, ln, col, toks) -> None:
        super().__init__(path, ln, col, toks)
        self._set(Attr.NAME, toks[0])


class Check(Node):
    __slots__ = (Attr.PROPERTIES,)
    AS_NODETYPE = NodeType.CHECK


class Comparison(Node):
    __slots__ = Attr.NAME, Attr.OPERANDS
    AS_NODETYPE = NodeType.COMPARISON

    def __init__(self, path, ln, col, toks) -> None:
        super().__init__(path, ln, col, toks)
        self._set(Attr.OPERANDS, [toks["cmp-lhs"], toks["cmp-rhs"]])


class Composition(Node):
    __slots__ = (Attr.NAME, Attr.OPERANDS)
    AS_NODETYPE = NodeType.COMPOSITION


class Declaration(Node):
    __slots__ = Attr.VARIABLE, Attr.VALUE
    AS_NODETYPE = NodeType.DECLARATION

    def __init__(self, path, ln, col, toks) -> None:
        super().__init__(path, ln, col, toks)
        setattr(self, Attr.VALUE, toks[1])


class TupleDeclaration(Node):
    __slots__ = Attr.VARIABLE, Attr.VALUE
    AS_NODETYPE = NodeType.TUPLE_DECL

    def __init__(self, path, ln, col, toks) -> None:
        super().__init__(path, ln, col, toks)
        self._listify(*self.__slots__)


class Expr(Node):
    __slots__ = Attr.NAME, Attr.OPERANDS
    AS_NODETYPE = NodeType.EXPR


class Guarded(Node):
    __slots__ = (Attr.CONDITION, Attr.BODY)
    AS_NODETYPE = NodeType.GUARDED


class If(Node):
    __slots__ = Attr.CONDITION, Attr.THEN, Attr.ELSE
    AS_NODETYPE = NodeType.IF


class Literal(Node):
    __slots__ = Attr.TYPE, Attr.VALUE
    AS_NODETYPE = NodeType.LITERAL


class Pick(Node):
    __slots__ = Attr.TYPE, Attr.CONDITION, Attr.VALUE
    AS_NODETYPE = NodeType.PICK


class ProcDef(Node):
    __slots__ = Attr.NAME, Attr.BODY
    AS_NODETYPE = NodeType.PROCDEF

    def __init__(self, path, ln, col, toks) -> None:
        super().__init__(path, ln, col, toks)
        self._listify(Attr.BODY)
        self._set(Attr.BODY, self._get(Attr.BODY)[0])


class PropertyDef(Node):
    __slots__ = Attr.MODALITY, Attr.CONDITION
    AS_NODETYPE = NodeType.PROPERTY_DEF


class QFormula(Node):
    __slots__ = Attr.QVARS, Attr.CONDITION
    AS_NODETYPE = NodeType.QFORMULA


class QVar(Node):
    __slots__ = Attr.NAME, Attr.TYPE, Attr.QUANTIFIER
    AS_NODETYPE = NodeType.QVAR


class RawCall(Node):
    __slots__ = Attr.NAME, Attr.OPERANDS
    AS_NODETYPE = NodeType.RAW_CALL


class Ref(Node):
    __slots__ = Attr.NAME, Attr.OF, Attr.OFFSET
    AS_NODETYPE = NodeType.REF

    def __init__(self, path, ln, col, toks) -> None:
        super().__init__(path, ln, col, toks)
        for attr in (Attr.OF, Attr.OFFSET):
            val = self._get(attr)
            if val is not None:
                self._set(attr, val[0])


class RefExt(Node):
    __slots__ = (Attr.NAME,)
    AS_NODETYPE = NodeType.REF_EXT


class RefLink(Node):
    __slots__ = Attr.NAME, Attr.OF, Attr.OFFSET
    AS_NODETYPE = NodeType.REF_LINK


class SpawnDeclaration(Node):
    __slots__ = Attr.TYPE, Attr.VALUE
    AS_NODETYPE = NodeType.SPAWN_DECLARATION


class Stigmergy(Node):
    __slots__ = Attr.CONDITION, Attr.NAME, Attr.TUPLES
    AS_NODETYPE = NodeType.STIGMERGY


class System(Node):
    __slots__ = (Attr.PROCDEFS, Attr.EXTERN, Attr.SPAWN)
    AS_NODETYPE = NodeType.SYSTEM

    def __init__(self, path, ln, col, toks) -> None:
        super().__init__(path, ln, col, toks)
        for attr in self.__slots__:
            if self._get(attr) is None:
                self._set(attr, [])
