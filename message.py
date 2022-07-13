from enum import Enum
import json
from pathlib import Path
from dataclasses import dataclass, asdict
import pyparsing as pp


class OutputFormat(Enum):
    TEXT = "TEXT"
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
        }[fmt]()

    @classmethod
    def from_node(_, message_id, message, node, ast) -> 'Message':
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
            path=ast["<meta>"]["path"],
            line=node["ln"],
            column=node["col"]
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


class FatalException(Exception):
    """Exception wrapping a `Message`."""


def j_dump(x):
    """Invokes `json.dumps` with reasonable arguments."""
    return json.dumps(x, default=str, indent=4)


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
