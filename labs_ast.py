from enum import Enum, unique, auto
from typing import Any, Type

from zmq import TYPE

_NEWLINE = "\n"

_SYNTAX = {
    "choice": " ++\n",
    "environment": "<--",
    "interface": "<-",
    "local": ":=",
    "par": " ||\n",
    "stigmergy": "<~",
    "seq": ";\n",
    "unary-minus": "-",
    "unary-not": "!"
}


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
    ROOT = auto()
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
    __slots__ = Attr.PATH, Attr.LN, Attr.COL, Attr.SYNTHETIC
    AS_NODETYPE = None

    @classmethod
    def factory(cls, path, ln, col, node_type, toks):
        lookup = {
            x.AS_NODETYPE: x
            for x in cls.__subclasses__()
        }
        lookup[Attr.VARIABLE] = Ref

        return lookup[node_type](path, ln, col, toks)

    @staticmethod
    def make_root(system, agents, stigmergies, assume, check):
        return Root(
            system[Attr.PATH], system[Attr.LN], system[Attr.COL],
            system, agents, stigmergies, assume, check)

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

    def as_labs(self, indent=0) -> str:
        return str(self)

    def serialize(self) -> dict:
        def handle(value):
            return value.serialize() if isinstance(value, Node) else value

        return {
            Attr.NODE_TYPE: self.AS_NODETYPE.value,
            **{
                k: handle(self[k]) for k in Node.__slots__
                if k != Attr.SYNTHETIC or getattr(self, Attr.SYNTHETIC, False)
            },
            **{k: handle(self[k]) for k in self.__slots__}
        }

    def __repr__(self) -> str:
        return str(self.serialize())


class Agent(Node):
    __slots__ = Attr.NAME, Attr.INTERFACE, Attr.STIGMERGIES, Attr.PROCDEFS  # noqa: E501
    AS_NODETYPE = NodeType.AGENT

    def as_labs(self, indent=0) -> str:
        iface = ",".join(x.as_labs() for x in self[Attr.INTERFACE])
        iface = f"{' '*(indent+2)}interface = {iface}\n" if iface else ""
        stigs = ", ".join(self[Attr.STIGMERGIES])
        stigs = f"{' '*(indent+2)}stigmergies = {stigs}\n" if stigs else ""
        procdefs = "\n".join(
            x.as_labs(indent=indent+2) for x in self[Attr.PROCDEFS])

        return (
            f"{' '*indent}agent {{\n"
            f"{iface}{stigs}"
            f"{procdefs}"
            f"\n{' '*indent}}}"
        )


class Assign(Node):
    __slots__ = Attr.TYPE, Attr.LHS, Attr.RHS
    AS_NODETYPE = NodeType.ASSIGN

    def __init__(self, path, ln, col, toks) -> None:
        super().__init__(path, ln, col, toks)
        self._set(Attr.TYPE, toks[Attr.LOCATION])

    def as_labs(self, indent=0) -> str:
        lhs = ", ".join(x.as_labs() for x in self[Attr.LHS])
        rhs = ", ".join(x.as_labs() for x in self[Attr.RHS])
        return f"{' '*indent}{lhs} {_SYNTAX[self[Attr.TYPE]]} {rhs}"


class Assume(Node):
    __slots__ = (Attr.PROPERTIES,)
    AS_NODETYPE = NodeType.ASSUME


class Block(Node):
    __slots__ = (Attr.BODY,)
    AS_NODETYPE = NodeType.BLOCK

    def __init__(self, path, ln, col, toks) -> None:
        super().__init__(path, ln, col, toks)
        self._listify(Attr.BODY)

    def as_labs(self, indent=0) -> str:
        recurse = ";\n".join(
            x.as_labs(indent=indent+2) for x in self[Attr.BODY])
        return (
            f"{' '*(indent)}{{\n"
            f"{recurse}"
            f"\n{' '*(indent)}}}"
        )


class Builtin(Node):
    __slots__ = Attr.NAME, Attr.OPERANDS
    AS_NODETYPE = NodeType.BUILTIN

    def as_labs(self, indent=0) -> str:
        fn = _SYNTAX.get(self[Attr.NAME], self[Attr.NAME])
        args = ", ".join(x.as_labs() for x in self[Attr.OPERANDS])
        return f"{' '*indent}{fn}({args})"


class Call(Node):
    __slots__ = (Attr.NAME,)
    AS_NODETYPE = NodeType.CALL

    def __init__(self, path, ln, col, toks) -> None:
        super().__init__(path, ln, col, toks)
        self._set(Attr.NAME, toks[0])

    def as_labs(self, indent=0) -> str:
        return f"{' '*indent}{self[Attr.NAME]}"


class Check(Node):
    __slots__ = (Attr.PROPERTIES,)
    AS_NODETYPE = NodeType.CHECK


class Comparison(Node):
    __slots__ = Attr.NAME, Attr.OPERANDS
    AS_NODETYPE = NodeType.COMPARISON

    def __init__(self, path, ln, col, toks) -> None:
        super().__init__(path, ln, col, toks)
        self._set(Attr.OPERANDS, [toks["cmp-lhs"], toks["cmp-rhs"]])

    def as_labs(self, indent=0) -> str:
        op = f" {self[Attr.NAME]} "
        return op.join(x.as_labs() for x in self[Attr.OPERANDS])


class Composition(Node):
    __slots__ = (Attr.NAME, Attr.OPERANDS)
    AS_NODETYPE = NodeType.COMPOSITION

    def as_labs(self, indent=0) -> str:
        return _SYNTAX[self[Attr.NAME]].join(
            x.as_labs(indent=indent+2) for x in self[Attr.OPERANDS])


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

    def as_labs(self, indent=0) -> str:
        op = f" {self[Attr.NAME]} "
        return op.join(x.as_labs() for x in self[Attr.OPERANDS])


class Guarded(Node):
    __slots__ = (Attr.CONDITION, Attr.BODY)
    AS_NODETYPE = NodeType.GUARDED


class If(Node):
    __slots__ = Attr.CONDITION, Attr.THEN, Attr.ELSE
    AS_NODETYPE = NodeType.IF

    def as_labs(self, indent=0) -> str:
        cond = self[Attr.CONDITION].as_labs()
        then = self[Attr.THEN].as_labs()
        else_ = self[Attr.ELSE].as_labs()
        return f"if {cond} then {then} else {else_}"


class Literal(Node):
    __slots__ = Attr.TYPE, Attr.VALUE
    AS_NODETYPE = NodeType.LITERAL

    def as_labs(self, indent=0) -> str:
        if self[Attr.TYPE] == "bool":
            return "true" if self[Attr.VALUE] else "false"
        else:
            return str(self[Attr.VALUE])


class Pick(Node):
    __slots__ = Attr.TYPE, Attr.CONDITION, Attr.VALUE
    AS_NODETYPE = NodeType.PICK

    def as_labs(self, indent=0) -> str:
        where = f" where {self[Attr.CONDITION].as_labs()}" if self[Attr.CONDITION] else ""  # noqa: E501
        type_ = f" {self[Attr.TYPE]}" if self[Attr.TYPE] else ""
        return f"pick {self[Attr.VALUE]}{type_}{where}"


class ProcDef(Node):
    __slots__ = Attr.NAME, Attr.BODY
    AS_NODETYPE = NodeType.PROCDEF

    def __init__(self, path, ln, col, toks) -> None:
        super().__init__(path, ln, col, toks)
        self._listify(Attr.BODY)
        self._set(Attr.BODY, self._get(Attr.BODY)[0])

    def as_labs(self, indent=0) -> str:
        return (
            f"{' '*indent}{self[Attr.NAME]} "
            "=\n"
            f"{self[Attr.BODY].as_labs(indent)}")


class PropertyDef(Node):
    __slots__ = Attr.MODALITY, Attr.CONDITION
    AS_NODETYPE = NodeType.PROPERTY_DEF


class QFormula(Node):
    __slots__ = Attr.QVARS, Attr.CONDITION
    AS_NODETYPE = NodeType.QFORMULA

    def as_labs(self, indent=0) -> str:
        qvars = ", ".join(x.as_labs() for x in self[Attr.QVARS])
        qvars = f"{qvars}, " if qvars else ""
        return f"{qvars}{self[Attr.CONDITION].as_labs()}"


class QVar(Node):
    __slots__ = Attr.NAME, Attr.TYPE, Attr.QUANTIFIER
    AS_NODETYPE = NodeType.QVAR

    def as_labs(self, indent=0) -> str:
        return f"{self[Attr.QUANTIFIER]} {self[Attr.TYPE]} {self[Attr.NAME]}"


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

    def as_labs(self, indent=0) -> str:
        result = self[Attr.NAME]
        if self[Attr.OFFSET]:
            result += f"[{self[Attr.OFFSET].as_labs()}]"
        if self[Attr.OF]:
            result = f"({result} of {self[Attr.OF].as_labs()})"
        return result


class RefExt(Node):
    __slots__ = (Attr.NAME,)
    AS_NODETYPE = NodeType.REF_EXT

    def __init__(self, path, ln, col, toks) -> None:
        super().__init__(path, ln, col, toks)
        self[Attr.NAME] = toks[0][Attr.NAME]
        self[Attr.NAME] = f"_{self[Attr.NAME]}"

    def as_labs(self, indent=0) -> str:
        return self[Attr.NAME]


class RefLink(Node):
    __slots__ = Attr.NAME, Attr.OF, Attr.OFFSET
    AS_NODETYPE = NodeType.REF_LINK


class Root(Node):
    __slots__ = "system", "agents", "stigmergies", "assume", "check"
    AS_NODETYPE = NodeType.ROOT

    def __init__(
        self, path, ln, col, system, agents, stigmergies, assume, check
    ):
        self[Attr.PATH] = path
        self[Attr.LN] = ln
        self[Attr.COL] = col
        self.system = system
        self.agents = agents
        self.stigmergies = stigmergies
        self.assume = assume
        self.check = check

    def as_labs(self, _=0) -> str:
        return (
f"""{self["system"].as_labs()}

{_NEWLINE.join(a.as_labs() for a in self["agents"])}
""")


class SpawnDeclaration(Node):
    __slots__ = Attr.TYPE, Attr.VALUE
    AS_NODETYPE = NodeType.SPAWN_DECLARATION

    def as_labs(self) -> str:
        return f"{self[Attr.TYPE]}: {self[Attr.VALUE].as_labs()}"


class Stigmergy(Node):
    __slots__ = Attr.CONDITION, Attr.NAME, Attr.TUPLES
    AS_NODETYPE = NodeType.STIGMERGY


class System(Node):
    # TODO: add environment
    __slots__ = (Attr.PROCDEFS, Attr.EXTERN, Attr.SPAWN)
    AS_NODETYPE = NodeType.SYSTEM

    def __init__(self, path, ln, col, toks) -> None:
        super().__init__(path, ln, col, toks)
        for attr in self.__slots__:
            if self._get(attr) is None:
                self._set(attr, [])

    def as_labs(self, indent=0) -> str:
        ext = ", ".join(x.as_labs() for x in self[Attr.EXTERN])
        ext = f"{' '*(indent+2)}extern = {ext}\n"if ext else ""
        spawn = ", ".join(x.as_labs() for x in self[Attr.SPAWN])
        spawn = f"{' '*(indent+2)}spawn = {spawn}\n"if spawn else ""
        procdefs = "\n".join(
            x.as_labs(indent=indent+2) for x in self[Attr.PROCDEFS])

        return (
            f"{' '*indent}system {{\n"
            f"{ext}{spawn}"
            f"{procdefs}{_NEWLINE if procdefs else ''}"
            f"{' '*indent}}}")
