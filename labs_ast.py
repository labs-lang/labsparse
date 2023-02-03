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

_SYNTAX_MSUR = {
    "environment": "assign-env",
    "exists": "#exists",
    "forall": "#for-all",
    "interface": "assign-attr",
    "stigmergy": "assign-lstig",
    "local": "assign-local",
    "%": "mod",
    "!=": "/=",
    ":": "/"  # TODO (must be added to Masseur)
}

# Needed for Masseur translation
STIGMERGY_VARS = []


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


def is_(*node_classes):
    def fn(node):
        return any((isinstance(node, c) for c in node_classes))
    return fn


def maybe_list(iterable):
    if len(iterable) == 0:
        return "()"
    elif len(iterable) == 1:
        return iterable[0]
    else:
        return f"(list {' '.join(str(x) for x in iterable)})"


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
            except (KeyError, ValueError, IndexError):
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

    def as_msur(self) -> str:
        return str(self)

    def _as_labs_defs(self, attr, indent=0, wrapper=None):
        things = "\n".join(x.as_labs(indent=indent+2) for x in self[attr])
        if wrapper:
            return f"\n{' '*(indent)}{wrapper}{{\n{things}\n{' '*(indent)}}}"
        else:
            return things

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
        iface = ", ".join(x.as_labs() for x in self[Attr.INTERFACE])
        iface = f"{' '*(indent+2)}interface = {iface}\n" if iface else ""
        stigs = ", ".join(self[Attr.STIGMERGIES])
        stigs = f"{' '*(indent+2)}stigmergies = {stigs}\n" if stigs else ""
        procdefs = "\n".join(
            x.as_labs(indent=indent+2) for x in self[Attr.PROCDEFS])

        return (
            f"\n{' '*indent}agent {self[Attr.NAME]} {{\n"
            f"{iface}{stigs}"
            f"{procdefs}"
            f"\n{' '*indent}}}"
        )

    def as_msur(self) -> str:
        # rename process and #calls
        defined_procs = [x[Attr.NAME] for x in self[Attr.PROCDEFS]]

        def renamer(name):
            return f"{self[Attr.NAME]}-{name}"

        for proc in self[Attr.PROCDEFS]:
            proc[Attr.NAME] = renamer(proc[Attr.NAME])
            for n in proc.walk():
                if isinstance(n, Call) and n[Attr.NAME] in defined_procs:
                    n[Attr.NAME] = renamer(n[Attr.NAME])

        procdefs = "\n".join(x.as_msur() for x in self[Attr.PROCDEFS])
        iface = " ".join(x.as_msur() for x in self[Attr.INTERFACE])
        return (
            f"{procdefs}\n"
            f"(#agent {self[Attr.NAME]} {self[Attr.NAME]}-Behavior "
            f"{iface}"
            ")"
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

    def as_msur(self):
        # find all stigmergy variables in RHS
        stigmergy_refs = maybe_list([
            n[Attr.NAME]
            for expr in self[Attr.RHS]
            for n in expr.walk()
            if isinstance(n, Ref) and n[Attr.NAME] in STIGMERGY_VARS])

        result = ""
        lhs, rhs = self[Attr.LHS], self[Attr.RHS]

        if self[Attr.TYPE] == "local":
            zipped = list(zip(lhs, rhs))
            picks = [(l, r) for l, r in zipped if isinstance(r, Pick)]
            lhs = [l for l, r in zipped if not isinstance(r, Pick)]
            rhs = [r for _, r in zipped if not isinstance(r, Pick)]

            result = "\n".join([
                f"( pick {l.as_msur()} {r.as_msur()} )" for (l, r) in picks
            ])
            if not lhs:
                return result

        lhs = maybe_list([x.as_msur() for x in lhs])
        rhs = maybe_list([x.as_msur() for x in rhs])
        return result + f"( {_SYNTAX_MSUR[self[Attr.TYPE]]} {lhs} {rhs} {stigmergy_refs} )"


class Assume(Node):
    __slots__ = (Attr.PROPERTIES,)
    AS_NODETYPE = NodeType.ASSUME

    def as_labs(self, indent=0) -> str:
        return self._as_labs_defs(Attr.PROPERTIES, indent, "assume")


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

    def as_msur(self) -> str:
        # Collect locals
        local_vars = []
        for n in self.walk():
            if isinstance(n, Assign) and n[Attr.TYPE] == "local":
                local_vars += [
                    (x, isinstance(rhs, Pick))
                    for elem, rhs in zip(n[Attr.LHS], n[Attr.RHS])
                    for x in elem.walk()
                    if isinstance(x, Ref)]
        decl_locals = [
            f"( decl \"{x.as_msur()}\" ())"
            for (x, is_pick) in local_vars
            if not is_pick]
        decl_locals = "\n  ".join(decl_locals)
        local_names = [x[Attr.NAME] for (x, _) in local_vars]
        for n in self.walk():
            if isinstance(n, Ref) and n[Attr.NAME] in local_names:
                n[Attr.NAME] = f"\"{n[Attr.NAME]}\""

        # print(local_vars)
        recurse = "\n  ".join(x.as_msur() for x in self[Attr.BODY])
        return (f"(list\n  {decl_locals}\n  {recurse}\n)")


class Builtin(Node):
    __slots__ = Attr.NAME, Attr.OPERANDS
    AS_NODETYPE = NodeType.BUILTIN

    def as_labs(self, indent=0) -> str:
        if self[Attr.NAME] == "nondet-from-range":
            return f"{self[Attr.OPERANDS][0].as_labs()}..{self[Attr.OPERANDS][1].as_labs()}"  # noqa: E501
        elif self[Attr.NAME] == "nondet-from-list":
            return f'[{", ".join(x.as_labs() for x in self[Attr.OPERANDS])}]'
        else:
            fn = _SYNTAX.get(self[Attr.NAME], self[Attr.NAME])
            args = ", ".join(x.as_labs() for x in self[Attr.OPERANDS])
            return f"{' '*indent}{fn}({args})"

    def as_msur(self):
        if self[Attr.NAME] == "nondet-from-range":
            return f"( #range {self[Attr.OPERANDS][0].as_msur()} {self[Attr.OPERANDS][1].as_msur()})"  # noqa: E501
        elif self[Attr.NAME] == "nondet-from-list":
            return " ".join(x.as_msur() for x in self[Attr.OPERANDS])
        elif self[Attr.NAME] == "unary-minus":
            try:
                return -self[Attr.OPERANDS][0][Attr.VALUE]
            except AttributeError:
                return f"(- {self[Attr.OPERANDS][0].as_msur()})"
        else:
            # TODO
            return f"({self[Attr.NAME]} {' '.join(x.as_msur() for x in self[Attr.OPERANDS])})"  # noqa: E501


class Call(Node):
    __slots__ = (Attr.NAME,)
    AS_NODETYPE = NodeType.CALL

    def __init__(self, path, ln, col, toks) -> None:
        super().__init__(path, ln, col, toks)
        self._set(Attr.NAME, toks[0])

    def as_labs(self, indent=0) -> str:
        return f"{' '*indent}{self[Attr.NAME]}"

    def as_msur(self, indent=0) -> str:
        return f"( #call {self[Attr.NAME]} )"


class Check(Node):
    __slots__ = (Attr.PROPERTIES,)
    AS_NODETYPE = NodeType.CHECK        

    def as_labs(self, indent=0) -> str:
        return self._as_labs_defs(Attr.PROPERTIES, indent, "check")


class Comparison(Node):
    __slots__ = Attr.NAME, Attr.OPERANDS
    AS_NODETYPE = NodeType.COMPARISON

    def __init__(self, path, ln, col, toks) -> None:
        super().__init__(path, ln, col, toks)
        self._set(Attr.OPERANDS, [toks["cmp-lhs"], toks["cmp-rhs"]])

    def as_labs(self, indent=0) -> str:
        op = f" {self[Attr.NAME]} "
        return op.join(x.as_labs() for x in self[Attr.OPERANDS])

    def as_msur(self) -> str:
        op = _SYNTAX_MSUR.get(self[Attr.NAME], self[Attr.NAME])
        return f"( {op} {' '.join(x.as_msur() for x in self[Attr.OPERANDS])} )"


class Composition(Node):
    __slots__ = (Attr.NAME, Attr.OPERANDS)
    AS_NODETYPE = NodeType.COMPOSITION

    def as_labs(self, indent=0) -> str:
        return _SYNTAX[self[Attr.NAME]].join(
            x.as_labs(indent=indent+2) for x in self[Attr.OPERANDS])

    def as_msur(self):
        items = " ".join(x.as_msur() for x in self[Attr.OPERANDS])
        return f"( #{self[Attr.NAME]} {items} )"


class Declaration(Node):
    __slots__ = Attr.VARIABLE, Attr.VALUE
    AS_NODETYPE = NodeType.DECLARATION

    def __init__(self, path, ln, col, toks) -> None:
        super().__init__(path, ln, col, toks)
        setattr(self, Attr.VALUE, toks[1])

    def as_labs(self, indent=0) -> str:
        return f"{self[Attr.VARIABLE].as_labs()}: {self[Attr.VALUE].as_labs()}"

    def as_msur(self, indent=0) -> str:
        return f"( {self[Attr.VARIABLE].as_msur()} {self[Attr.VALUE].as_msur()} )"  # noqa: E501


class TupleDeclaration(Node):
    __slots__ = Attr.VARIABLE, Attr.VALUE
    AS_NODETYPE = NodeType.TUPLE_DECL

    def __init__(self, path, ln, col, toks) -> None:
        super().__init__(path, ln, col, toks)
        self._listify(*self.__slots__)

    def as_labs(self, indent=0) -> str:
        varz = ", ".join(x.as_labs() for x in self[Attr.VARIABLE])
        inits = ", ".join(x.as_labs() for x in self[Attr.VALUE])
        return f"{varz}: {inits}"


class Expr(Node):
    __slots__ = Attr.NAME, Attr.OPERANDS
    AS_NODETYPE = NodeType.EXPR

    def as_labs(self, indent=0) -> str:
        op = f" {self[Attr.NAME]} "
        return op.join(x.as_labs() for x in self[Attr.OPERANDS])

    def as_msur(self, indent=0) -> str:
        operands = " ".join(x.as_msur() for x in self[Attr.OPERANDS])
        op = _SYNTAX_MSUR.get(self[Attr.NAME], self[Attr.NAME])
        return f"( {op} {operands} )"


class Guarded(Node):
    __slots__ = (Attr.CONDITION, Attr.BODY)
    AS_NODETYPE = NodeType.GUARDED

    def as_labs(self, indent=0) -> str:
        return f"{self[Attr.CONDITION].as_labs()} ->\n  {self[Attr.BODY].as_labs(indent+2)}"  # noqa: E501

    def as_msur(self) -> str:
        return f"( #guard {self[Attr.CONDITION].as_msur()} {self[Attr.BODY].as_msur()})"


class If(Node):
    __slots__ = Attr.CONDITION, Attr.THEN, Attr.ELSE
    AS_NODETYPE = NodeType.IF

    def as_labs(self, indent=0) -> str:
        cond = self[Attr.CONDITION].as_labs()
        then = self[Attr.THEN].as_labs()
        else_ = self[Attr.ELSE].as_labs()
        return f"if {cond} then {then} else {else_}"

    def as_msur(self) -> str:
        cond = self[Attr.CONDITION].as_msur()
        then = self[Attr.THEN].as_msur()
        else_ = self[Attr.ELSE].as_msur()
        return f"( #if-else {cond} {then} {else_})"


class Literal(Node):
    __slots__ = Attr.TYPE, Attr.VALUE
    AS_NODETYPE = NodeType.LITERAL

    def as_labs(self, indent=0) -> str:
        if self[Attr.TYPE] == "bool":
            return "true" if self[Attr.VALUE] else "false"
        else:
            return str(self[Attr.VALUE])

    def as_msur(self) -> str:
        if self[Attr.TYPE] == "bool":
            return "#t" if self[Attr.VALUE] else "#f"
        else:
            return str(self[Attr.VALUE])


class Pick(Node):
    __slots__ = Attr.TYPE, Attr.CONDITION, Attr.VALUE
    AS_NODETYPE = NodeType.PICK

    def as_labs(self, indent=0) -> str:
        where = f" where {self[Attr.CONDITION].as_labs()}" if self[Attr.CONDITION] else ""  # noqa: E501
        type_ = f" {self[Attr.TYPE]}" if self[Attr.TYPE] else ""
        return f"pick {self[Attr.VALUE]}{type_}{where}"

    def as_msur(self) -> str:
        where = f" {self[Attr.CONDITION].as_msur()}" if self[Attr.CONDITION] else ""  # noqa: E501
        type_ = f" \"{self[Attr.TYPE]}\"" if self[Attr.TYPE] else ""
        return f"{self[Attr.VALUE]}{type_}{where}"


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

    def as_msur(self) -> str:
        return f"( #def {self[Attr.NAME]} {self[Attr.BODY].as_msur()} )"


class PropertyDef(Node):

    __slots__ = Attr.NAME, Attr.MODALITY, Attr.CONDITION
    AS_NODETYPE = NodeType.PROPERTY_DEF

    def as_labs(self, indent=0) -> str:
        mod = f" {self[Attr.MODALITY]}" if self[Attr.MODALITY] else ""
        return (
            f"{' '*indent}{self[Attr.NAME]} "
            f"={mod} "
            f"{self[Attr.CONDITION].as_labs()}")


class QFormula(Node):
    __slots__ = Attr.QVARS, Attr.CONDITION
    AS_NODETYPE = NodeType.QFORMULA

    def as_labs(self, indent=0) -> str:
        qvars = ", ".join(x.as_labs() for x in self[Attr.QVARS])
        qvars = f"{qvars}, " if qvars else ""
        return f"{qvars}{self[Attr.CONDITION].as_labs()}"

    def as_msur(self) -> str:
        qvars = " ".join(x.as_msur() for x in self[Attr.QVARS])
        close = " )" * len(self[Attr.QVARS])
        qvars = f"{qvars} " if qvars else ""
        return f"{qvars}{self[Attr.CONDITION].as_msur()}{close}"


class QVar(Node):
    __slots__ = Attr.NAME, Attr.TYPE, Attr.QUANTIFIER
    AS_NODETYPE = NodeType.QVAR

    def as_labs(self, indent=0) -> str:
        return f"{self[Attr.QUANTIFIER]} {self[Attr.TYPE]} {self[Attr.NAME]}"

    def as_msur(self) -> str:
        return f"( {_SYNTAX_MSUR[self[Attr.QUANTIFIER]]} {self[Attr.TYPE]} {self[Attr.NAME]}"  # noqa: E501


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

    def as_msur(self, indent=0) -> str:
        result = "#self" if self[Attr.NAME] == "id" else self[Attr.NAME]
        if self[Attr.OFFSET]:
            result = f"( #array {result} {self[Attr.OFFSET].as_msur()})"
        if self[Attr.OF]:
            result = f"( #var-of {result} {self[Attr.OF].as_msur()})"
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

    def as_msur(self) -> str:
        return self[Attr.NAME]


class RefLink(Node):
    __slots__ = Attr.NAME, Attr.OF, Attr.OFFSET
    AS_NODETYPE = NodeType.REF_LINK

    def as_labs(self, indent=0) -> str:
        result = self[Attr.NAME]
        if self[Attr.OFFSET]:
            result += f"[{self[Attr.OFFSET].as_labs()}]"
        if self[Attr.OF]:
            of = self[Attr.OF].replace("c", "")
            result = f"({result} of {of})"
        return result


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
{_NEWLINE.join(a.as_labs() for a in self["stigmergies"])}
{_NEWLINE.join(a.as_labs() for a in self["agents"])}
{self["assume"].as_labs() if self["assume"] else ""}{self["check"].as_labs()}
""")

    def as_msur(self) -> str:
        replicated = {}

        for agent in self["agents"]:
            for n in agent.walk():
                if isinstance(n, Declaration) and n[Attr.VARIABLE][Attr.NAME] not in replicated:  # noqa: E501
                    replicated[n[Attr.VARIABLE][Attr.NAME]] = (n[Attr.VARIABLE])
        for stigmergy in self["stigmergies"]:
            for n in stigmergy.walk():
                if isinstance(n, TupleDeclaration):
                    for var in n[Attr.VARIABLE]:
                        if var[Attr.NAME] not in replicated:
                            STIGMERGY_VARS.append(var[Attr.NAME])
                            replicated[var[Attr.NAME]] = var
        fmt_replicated = " ".join(x.as_msur() for x in replicated.values())

        return "\n".join((
            self["system"].as_msur(),
            f"(#replicated-vars {fmt_replicated})",
            "\n".join(x.as_msur() for x in self["agents"]),))


class SpawnDeclaration(Node):
    __slots__ = Attr.TYPE, Attr.VALUE
    AS_NODETYPE = NodeType.SPAWN_DECLARATION

    def as_labs(self) -> str:
        return f"{self[Attr.TYPE]}: {self[Attr.VALUE].as_labs()}"

    def as_msur(self) -> str:
        return f"({self[Attr.TYPE]} {self[Attr.VALUE].as_msur()})"


class Stigmergy(Node):
    __slots__ = Attr.CONDITION, Attr.NAME, Attr.TUPLES
    AS_NODETYPE = NodeType.STIGMERGY

    def as_labs(self, indent=0) -> str:
        tuples = [x.as_labs() for x in self[Attr.TUPLES]]
        tuples = ('\n'+(' '*(indent+2))).join(tuples)
        return (
            f"\n{' '*indent}stigmergy {self[Attr.NAME]} {{\n"
            f"{' '*(indent+2)}link = {self[Attr.CONDITION].as_labs()}"
            f"{tuples}"
            f"\n{' '*indent}}}"
        )

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

    def as_msur(self) -> str:
        spawn = " ".join(x.as_msur() for x in self[Attr.SPAWN])
        params = " ".join(x.as_msur() for x in self[Attr.EXTERN])
        procdefs = "\n".join(x.as_msur() for x in self[Attr.PROCDEFS])
        params = f"( #params {params} )\n" if params else ""

        return (
f"""{params}( #system {spawn} )
{procdefs}""")
