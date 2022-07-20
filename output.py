import json
from dataclasses import asdict, dataclass
from enum import Enum
from parser import Attr, NodeType
from pathlib import Path
from typing import Any

import pyparsing as pp

from labs_ast import Node


class OutputFormat(Enum):
    TEXT = "text"
    LABS = "labs"
    MASSEUR = "masseur"
    JSON = "json"


@dataclass
class Message:
    message_id: str
    message: str
    path: Path
    line: int
    column: int

    def print(self, fmt=OutputFormat.TEXT):
        def text_fn():
            pos = ":".join((str(self.path), str(self.line), str(self.column)))
            print(f"{pos}: {self.message_id}: {self.message}")

        def json_fn():
            print(j_dump(asdict(self)))

        {
            OutputFormat.TEXT: text_fn,
            OutputFormat.JSON: json_fn
        }.get(fmt, text_fn)()

    @classmethod
    def from_node(_, message_id, message, node) -> 'Message':
        """Create a `Message`, pulling location info from `node` and `ast`

        Args:
            message_id (str): Message identifier
            message (str): Message description
            node (dict): LAbS AST node
            ast (dict): The full AST to which `node` belongs

        Returns:
            Message: A message
        """
        return Message(
            message_id=message_id,
            message=message,
            path=node[Attr.PATH],
            line=node[Attr.LN],
            column=node[Attr.COL]
        )

    @classmethod
    def wrap_exception(_, ex: pp.ParseBaseException, path: Path) -> 'Message':
        """Wrap a `pyparsing` exception into a `Message`

        Args:
            ex (pp.ParseBaseException): Exception object
            path (Path): Path of the source file

        Returns:
            Message: A Message wrapping the parser message
        """
        msg = str(ex)
        # Strip away location from msg
        delete_from = msg.find("  (at char")
        return Message(
            'F001',
            f"Parsing failed: {msg[:delete_from]}",
            str(path),
            ex.lineno,
            ex.col
        )


def j_dump(x):
    """Invokes `json.dumps` with reasonable arguments."""
    def default(o: Any) -> Any:
        if isinstance(o, Node):
            return o.serialize()
        else:
            raise ValueError(o, type(o))
    # TODO configure indent from the CLI
    return json.dumps(x, default=default, indent=4)


def print_many(messages, fmt=OutputFormat.TEXT):

    def json_fn():
        print(j_dump([asdict(m) for m in messages]))

    def text_fn():
        for m in messages:
            m.print()
    {
        OutputFormat.TEXT: text_fn,
        OutputFormat.JSON: json_fn
    }[fmt]()


def sprint_labs(n, indent=""):
    def up_indent():
        return indent + "  "

    def sprint_list(lst, sep=", "):
        return sep.join(sprint_labs(x) for x in lst)

    def recurse_on(attr: Attr, inline=True, push_indent=False):
        if push_indent:
            inline = False
        i = "" if inline else up_indent() if push_indent else indent
        if isinstance(n[attr], list):
            result = "\n".join([sprint_labs(x, i) for x in n[attr]])
        else:
            result = sprint_labs(n[attr], i)
        return result if inline else "\n" + result

    if n in NodeType.ASSIGN:
        op = _SYNTAX[n[Attr.LOCATION]]
        lhs, rhs = sprint_list(n[Attr.LHS]), sprint_list(n[Attr.RHS])
        return f"{indent}{lhs} {op} {rhs}"

    elif n in NodeType.BLOCK:
        body = recurse_on(Attr.BODY, push_indent=True)
        return f"{indent}{{{body}\n{indent}}}"

    elif n in NodeType.BUILTIN:
        return f"{n[Attr.NAME]}({sprint_list(n[Attr.OPERANDS])})"

    elif n in NodeType.REF:
        result = n[Attr.NAME]
        if Attr.OFFSET in n:
            result += f"[{recurse_on(Attr.OFFSET)}]"
        if Attr.OF in n:
            result = f"({result} of {recurse_on(Attr.OF)})"
        return result

    elif n in NodeType.LITERAL:
        if n[Attr.TYPE] == "bool":
            return "true" if n[Attr.VALUE] else "false"
        else:
            return str(n[Attr.VALUE])

    elif n in NodeType.PROCDEF:
        return f"{n[Attr.NAME]} = {recurse_on(Attr.BODY, push_indent=True)}"
    elif n in NodeType.COMPOSITION:
        procs = [sprint_labs(x, up_indent()) for x in n[Attr.OPERANDS]]
        result = _SYNTAX[n[Attr.NAME]].join(procs)
        return result if len(procs) > 1 else f"{indent}({result}\n{indent})"
    elif n in NodeType.GUARDED:
        return f"{indent}{recurse_on(Attr.CONDITION)} -> ({recurse_on(Attr.BODY, inline=False, push_indent=True)}\n{indent})"   # noqa: E501
    elif n in NodeType.EXPR or n in NodeType.COMPARISON:
        operands = [sprint_labs(n) for n in n[Attr.OPERANDS]]
        op = _SYNTAX.get(n[Attr.NAME], f" {n[Attr.NAME]} ")
        if len(operands) > 1:
            return f"({op.join(operands)})"
        else:
            return f"{op}({operands[0]})"
    elif n in NodeType.REF_EXT or n in NodeType.CALL:
        return f"{indent}{n[Attr.NAME]}"
    return str(n)


def sprint_labs_ast(ast):
    pass
    # for x in walk(ast):
    #     if x in NodeType.PROCDEF:
    #         print(sprint_labs(x))
