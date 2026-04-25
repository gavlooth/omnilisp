#!/usr/bin/env python3
"""Verify memory-sensitive ownership surfaces are classified by manifest."""

from __future__ import annotations

import fnmatch
import re
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
MANIFEST_PATH = ROOT / "scripts" / "memory_ownership_surface_manifest.tsv"


CALL_PATTERN = re.compile(r"(?:[A-Za-z_][A-Za-z0-9_]*::)?([A-Za-z_][A-Za-z0-9_]*)\s*\(")
DEVICE_FINALIZER_PATTERN = re.compile(r"\.device_finalizer\s*=\s*(?P<expr>[^;]+);")
MANIFEST_COMMENT_PREFIX = "#"

# Candidate call-site families that are treated as owning/memory-sensitive.
OWNING_CANDIDATE_PATTERNS = (
    "scope_register_dtor",
    "scope_retain",
    "scope_release",
    "make_ffi_handle*",
    "make_*handle*",
    "ffi_handle_retain",
    "ffi_handle_release",
    "ffi_handle_release_payload",
    "ffi_handle_release_raw_payload",
    "ffi_handle_release_raw_allowed",
    "ffi_handle_can_release_payload",
    "promote_escape_ffi_handle",
    "promote_to_root",
    "*callback*register*",
    "*callback*unregister*",
)

VALID_KINDS = {"call", "ffi-family", "ffi-dynamic", "tensor-finalizer"}
FFI_HANDLE_CALLS = {
    "make_ffi_handle",
    "make_ffi_handle_ex",
    "make_ffi_handle_ex_with_descriptor",
    "make_ffi_handle_with_descriptor",
}


def fail(message: str) -> int:
    print(f"FAIL: {message}", file=sys.stderr)
    return 1


def strip_comments(line: str) -> str:
    return re.sub(r"//.*", "", line)


def load_manifest(path: Path) -> list[dict[str, str]]:
    if not path.exists():
        raise SystemExit(fail(f"ownership manifest missing: {path.relative_to(ROOT)}"))

    rows: list[dict[str, str]] = []
    for raw in path.read_text(encoding="utf-8").splitlines():
        row = raw.strip()
        if not row or row.startswith(MANIFEST_COMMENT_PREFIX):
            continue
        parts = row.split("\t")
        if len(parts) != 6:
            raise SystemExit(fail(f"manifest row must have 6 tab-separated columns: {raw}"))
        kind, pattern, lane, authority, bridge, note = (part.strip() for part in parts)
        if kind not in VALID_KINDS:
            raise SystemExit(fail(f"unknown manifest kind {kind}: {raw}"))
        if not pattern or not lane or not authority or not bridge or not note:
            raise SystemExit(fail(f"manifest row has an empty required field: {raw}"))
        rows.append(
            {
                "kind": kind,
                "pattern": pattern,
                "lane": lane,
                "authority": authority,
                "bridge": bridge,
                "note": note,
            }
        )
    return rows


def is_candidate(symbol: str, patterns: tuple[str, ...]) -> bool:
    return any(fnmatch.fnmatch(symbol, p) for p in patterns)


def is_classified(kind: str, value: str, manifest_rows: list[dict[str, str]]) -> bool:
    return any(
        row["kind"] == kind and fnmatch.fnmatch(value, row["pattern"])
        for row in manifest_rows
    )


def strip_string_literals(line: str) -> str:
    out: list[str] = []
    in_string = False
    escaped = False
    for ch in line:
        if in_string:
            if escaped:
                escaped = False
            elif ch == "\\":
                escaped = True
            elif ch == '"':
                in_string = False
            out.append(" ")
        else:
            if ch == '"':
                in_string = True
                out.append(" ")
            else:
                out.append(ch)
    return "".join(out)


def split_top_level_args(args_text: str) -> list[str]:
    args: list[str] = []
    current: list[str] = []
    depth = 0
    in_string = False
    escaped = False
    for ch in args_text:
        if in_string:
            current.append(ch)
            if escaped:
                escaped = False
            elif ch == "\\":
                escaped = True
            elif ch == '"':
                in_string = False
            continue
        if ch == '"':
            in_string = True
            current.append(ch)
        elif ch in "([{":
            depth += 1
            current.append(ch)
        elif ch in ")]}":
            depth -= 1
            current.append(ch)
        elif ch == "," and depth == 0:
            args.append("".join(current).strip())
            current = []
        else:
            current.append(ch)
    if "".join(current).strip():
        args.append("".join(current).strip())
    return args


def iter_calls(text: str) -> list[tuple[int, str, str]]:
    calls: list[tuple[int, str, str]] = []
    idx = 0
    while True:
        match = CALL_PATTERN.search(text, idx)
        if match is None:
            break
        symbol = match.group(1)
        open_paren = text.find("(", match.end() - 1)
        if open_paren < 0:
            break
        depth = 0
        in_string = False
        escaped = False
        end = open_paren
        while end < len(text):
            ch = text[end]
            if in_string:
                if escaped:
                    escaped = False
                elif ch == "\\":
                    escaped = True
                elif ch == '"':
                    in_string = False
            else:
                if ch == '"':
                    in_string = True
                elif ch == "(":
                    depth += 1
                elif ch == ")":
                    depth -= 1
                    if depth == 0:
                        line = text.count("\n", 0, match.start()) + 1
                        calls.append((line, symbol, text[open_paren + 1:end]))
                        idx = end + 1
                        break
            end += 1
        else:
            break
    return calls


def literal_string(expr: str) -> str | None:
    expr = expr.strip()
    match = re.fullmatch(r'"([^"\\]*(?:\\.[^"\\]*)*)"', expr)
    if match:
        return match.group(1)
    return None


def dynamic_ffi_key(relative_file: str, symbol: str) -> str:
    return f"{relative_file}:{symbol}"


def finalizer_key(expr: str) -> str:
    if "null" == expr.strip():
        return "null"
    match = re.search(r"&\s*([A-Za-z_][A-Za-z0-9_]*)", expr)
    if match:
        return match.group(1)
    return expr.strip()


def scan_file(file_path: Path, manifest_rows: list[dict[str, str]]) -> list[tuple[str, str]]:
    results: list[tuple[str, str]] = []
    if not file_path.is_absolute():
        file_path = ROOT / file_path
    text = file_path.read_text(encoding="utf-8")
    relative_file = str(file_path.relative_to(ROOT))
    for line_number, raw_line in enumerate(text.splitlines(), start=1):
        line = strip_comments(raw_line).strip()
        if not line:
            continue
        if line.startswith("fn ") or line.startswith("extern fn "):
            continue

        for finalizer_match in DEVICE_FINALIZER_PATTERN.finditer(line):
            key = finalizer_key(finalizer_match.group("expr"))
            if not is_classified("tensor-finalizer", key, manifest_rows):
                results.append((f"{file_path}:{line_number}", f"tensor-finalizer:{key}"))

        for symbol_match in CALL_PATTERN.finditer(strip_string_literals(line)):
            symbol = symbol_match.group(1)
            if not is_candidate(symbol, OWNING_CANDIDATE_PATTERNS):
                continue
            if is_classified("call", symbol, manifest_rows):
                continue
            results.append((f"{file_path}:{line_number}", symbol))

    for line_number, symbol, args_text in iter_calls(text):
        if symbol not in FFI_HANDLE_CALLS:
            continue
        args = split_top_level_args(args_text)
        if len(args) < 3:
            results.append((f"{file_path}:{line_number}", f"ffi-family:<missing-name>"))
            continue
        family = literal_string(args[2])
        if family is None:
            key = dynamic_ffi_key(relative_file, symbol)
            if not is_classified("ffi-dynamic", key, manifest_rows):
                results.append((f"{file_path}:{line_number}", f"ffi-dynamic:{key}"))
            continue
        if not is_classified("ffi-family", family, manifest_rows):
            results.append((f"{file_path}:{line_number}", f"ffi-family:{family}"))
    return results


def default_files() -> list[Path]:
    return sorted((ROOT / "src").glob("**/*.c3"))


def main(argv: list[str]) -> int:
    files = [Path(p) for p in argv] if argv else default_files()
    if not files:
        print("OK: no files provided; ownership inventory guard skipped.")
        return 0

    manifest_rows = load_manifest(MANIFEST_PATH)
    if not manifest_rows:
        return fail(f"empty ownership manifest: {MANIFEST_PATH.relative_to(ROOT)}")

    unclassified: list[tuple[str, str]] = []
    seen: set[tuple[str, str]] = set()

    for file_path in files:
        if not file_path.exists():
            continue
        if file_path.suffix != ".c3":
            continue
        for record in scan_file(file_path, manifest_rows):
            if record not in seen:
                seen.add(record)
                unclassified.append(record)

    if unclassified:
        unclassified.sort(key=lambda item: item[0])
        print(
            f"FAIL: memory ownership inventory found {len(unclassified)} unclassified call-site(s)",
            file=sys.stderr,
        )
        for location, symbol in unclassified:
            print(f"  {location}: {symbol}", file=sys.stderr)
        return 1

    print(
        f"OK: ownership inventory guard classified all memory-sensitive call-sites in {len(files)} file(s)",
    )
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
