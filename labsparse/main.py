#!/usr/bin/env python3

import pprint
from pathlib import Path
from sys import stderr
import sys
from typing import Optional

import typer
from pyparsing import ParseBaseException

from .checker import run
from .labs_parser import parse_to_dict
from .optimizer import optimize
from .output import Message, OutputFormat, j_dump, print_many


def print_version(flag):
    __name__ = "LAbSParse"
    __version__ = "0.1"
    if flag is not None:
        typer.echo(f"{__name__} {__version__}")
        raise typer.Exit()


def main(
    path: Path = typer.Argument(
        ..., exists=True,
        help="Path to a LAbS specification."
    ),
    dump_ast: bool = typer.Option(
        False, "--dump-ast",
        help="Dump the syntax tree and exit."
    ),
    output_format: OutputFormat = typer.Option(
        OutputFormat.TEXT.value, "--output-format", "-f",
        help="Set the output format."
        ),
    version: Optional[bool] = typer.Option(
        None, "--version", callback=print_version, is_eager=True,
        help="Print version number and exit."
    )
):
    """LAbSParse"""
    messages = []
    try:
        ast = parse_to_dict(path)
        optimize(ast)
    except ParseBaseException as e:
        # print("[FATAL] parsing failed", file=stderr)
        messages = [Message.wrap_exception(e, path)]
        if dump_ast:
            print_many(messages, out=sys.stderr)
    else:
        if dump_ast:
            dump_fn = {
                OutputFormat.JSON: j_dump,
                OutputFormat.LABS: lambda x: x.as_labs(),
                OutputFormat.MASSEUR: lambda x: x.as_msur()  # TODO
            }.get(output_format, lambda _: pprint.pprint(ast, width=40))

            result = dump_fn(ast)
            if isinstance(result, str):
                print(result)
            raise typer.Exit()

        messages = run(ast)

    finally:
        if not dump_ast:
            print_many(messages, output_format)


if __name__ == "__main__":
    typer.run(main)
