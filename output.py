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
