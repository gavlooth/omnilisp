from __future__ import annotations

import argparse
import os
from pathlib import Path
import sys
import tempfile


def _existing_file(parser: argparse.ArgumentParser, value: str) -> str:
    path = Path(value)
    if not path.is_file():
        parser.error(f"not a file: {value}")
    return value


def single_path_arg(script_path: str, argv: list[str] | None = None) -> str:
    parser = argparse.ArgumentParser(
        prog=Path(script_path).name,
        description="Rewrite one existing source file in place.",
    )
    parser.add_argument("path", type=lambda value: _existing_file(parser, value))
    args = parser.parse_args(sys.argv[1:] if argv is None else argv)
    return args.path


def path_args(script_path: str, argv: list[str] | None = None) -> list[str]:
    parser = argparse.ArgumentParser(
        prog=Path(script_path).name,
        description="Rewrite one or more existing source files in place.",
    )
    parser.add_argument(
        "paths",
        nargs="+",
        type=lambda value: _existing_file(parser, value),
        metavar="path",
    )
    args = parser.parse_args(sys.argv[1:] if argv is None else argv)
    return args.paths


def rewrite_file(path: str, contents: str) -> None:
    target = Path(path)
    stat_result = target.stat()
    with tempfile.NamedTemporaryFile(
        "w",
        encoding="utf-8",
        dir=target.parent,
        prefix=f".{target.name}.",
        delete=False,
    ) as handle:
        temp_path = Path(handle.name)
        handle.write(contents)
    try:
        os.chmod(temp_path, stat_result.st_mode & 0o777)
        os.replace(temp_path, target)
    except Exception:
        temp_path.unlink(missing_ok=True)
        raise
