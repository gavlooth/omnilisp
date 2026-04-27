#!/usr/bin/env python3
from __future__ import annotations

from collections import defaultdict
from pathlib import Path
import json
import os
import sys


EXCLUDED_SOURCES = {
    "src/entry.c3",
    "src/entry_dev.c3",
    "src/entry_bind_dep_generation.c3",
    "src/entry_bind_mode.c3",
    "src/entry_bind_parse_helpers.c3",
    "src/entry_bind_paths.c3",
    "src/entry_bind_runtime_setup.c3",
    "src/entry_build_backend_compile.c3",
    "src/entry_build_helpers.c3",
    "src/entry_build_mode.c3",
    "src/entry_cli_help_version.c3",
    "src/entry_language_reference.c3",
    "src/entry_language_reference_payload.c3",
    "src/entry_runtime_modes.c3",
    "src/entry_runtime_modes_dev.c3",
    "src/entry_stack_affinity_mode.c3",
    "src/entry_compile_mode.c3",
    "src/entry_compile_reporting.c3",
    "src/entry_project_init_bind.c3",
    "src/entry_project_init_files.c3",
    "src/entry_project_init_writer_project_json.c3",
    "src/entry_project_init_writers.c3",
    "src/entry_test_modes.c3",
    "src/entry_test_runner_setup.c3",
    "src/lisp/eval_init_primitives.c3",
    "src/lisp/schema_validation.c3",
    "src/pika/lisp_pika_tests.c3",
    "src/lisp/aot.c3",
    "src/lisp/aot_runtime_bridge.c3",
    "src/lisp/bindgen.c3",
}


USAGE = "usage: generate_fast_dev_project.py <generate|profile|is-up-to-date> ..."

COMMAND_ARG_COUNTS = {
    "generate": 8,
    "profile": 2,
    "is-up-to-date": 7,
}


def write_if_changed(path: Path, text: str) -> None:
    if path.exists() and path.read_text(encoding="utf-8") == text:
        return
    path.write_text(text, encoding="utf-8")


def rel_to_fast_dir(path: Path, fast_project_dir: Path) -> str:
    return Path(os.path.relpath(path, fast_project_dir)).as_posix()


def unique_preserving_order(items: list[str]) -> list[str]:
    seen: set[str] = set()
    result: list[str] = []
    for item in items:
        if item in seen:
            continue
        seen.add(item)
        result.append(item)
    return result


def should_include_source(rel: str, profile: str) -> bool:
    if rel in EXCLUDED_SOURCES:
        return False
    if rel.startswith("src/pika/"):
        return False
    if rel.startswith("src/lisp/compiler_"):
        return False
    if rel.startswith("src/lisp/tests_"):
        return False
    if rel.startswith("src/scope_region_tests"):
        return False
    if rel.startswith("src/stack_engine_tests"):
        return False
    if profile == "nodeduce" and (
        rel == "src/lisp/deduce.c3"
        or rel == "src/lisp/unify.c3"
        or rel.startswith("src/lisp/deduce_")
    ):
        return False
    return True


def patched_cli_help_text(root: Path) -> str:
    original = (root / "src/entry_cli_help_version.c3").read_text(encoding="utf-8")
    return original.replace(
        '    io::printn("  omni --language-ref, --manual    Print built-in full language reference");\n',
        "",
    )


def group_name(path: Path) -> str:
    try:
        rel = path.relative_to(Path.cwd())
    except ValueError:
        return "generated"
    name = path.name
    if rel.parts[0] != "src":
        return "generated"
    if len(rel.parts) == 2 and name.startswith("entry_"):
        return "entry"
    if rel.parts[1] != "lisp":
        return rel.parts[1]
    for prefix in (
        "deduce_",
        "jit_",
        "scheduler_",
        "async_",
        "parser_",
        "compiler_",
        "eval_",
        "value_",
        "prim_",
        "primitives_",
        "schema_",
        "macros_",
        "json_",
        "tls_",
        "http_",
        "unicode_",
        "aot_",
    ):
        if name.startswith(prefix):
            return prefix[:-1]
    return "lisp-misc"


def cmd_generate(argv: list[str]) -> int:
    root = Path(argv[0]).resolve()
    project_json = Path(argv[1]).resolve()
    fast_project_json = Path(argv[2]).resolve()
    build_dir = Path(argv[3]).resolve()
    output_dir = Path(argv[4]).resolve()
    output_bin = Path(argv[5]).resolve()
    source_manifest = Path(argv[6]).resolve()
    profile = argv[7]
    fast_project_dir = fast_project_json.parent

    with open(project_json, "r", encoding="utf-8") as f:
        project = json.load(f)

    sources: list[Path] = []
    for path in sorted((root / "src").rglob("*.c3")):
        rel = path.relative_to(root).as_posix()
        if should_include_source(rel, profile):
            sources.append(path.resolve())

    sources.append((root / "tools/fast-dev/entry_dev.c3").resolve())
    sources.append((root / "tools/fast-dev/entry_runtime_modes_dev.c3").resolve())

    patched_cli_help = fast_project_dir / "entry_cli_help_version.c3"
    write_if_changed(patched_cli_help, patched_cli_help_text(root))
    sources.append(patched_cli_help)

    sources.append((root / "tools/fast-dev/lisp/eval_init_primitives.c3").resolve())
    sources.append((root / "tools/fast-dev/lisp/schema_validation.c3").resolve())

    target = dict(project["targets"]["main"])
    target["name"] = output_bin.name
    target["sources"] = [rel_to_fast_dir(path, fast_project_dir) for path in sources]
    target["c-sources"] = []
    target["linker-search-paths"] = [
        rel_to_fast_dir((root / path).resolve(), fast_project_dir)
        if not Path(path).is_absolute()
        else path
        for path in project["targets"]["main"]["linker-search-paths"]
    ]
    target["linked-libraries"] = unique_preserving_order([
        "omni_chelpers",
        "utf8proc",
        "deflate",
        "yyjson",
        "uv",
        "bearssl",
        "lmdb",
        *project["targets"]["main"]["linked-libraries"],
    ])
    target["build-dir"] = rel_to_fast_dir(build_dir, fast_project_dir)
    target["output"] = rel_to_fast_dir(output_dir, fast_project_dir)
    target.pop("c-sources-override", None)
    target.pop("sources-override", None)
    target.pop("linker-search-paths-override", None)

    fast_project = {
        "langrev": project["langrev"],
        "version": project["version"],
        "authors": project.get("authors", []),
        "warnings": project.get("warnings", []),
        "targets": {"main-dev": target},
    }

    for key in ("single-module", "linker", "link-args", "linked-libraries", "linux-libc"):
        if key in project:
            fast_project[key] = project[key]

    write_if_changed(fast_project_json, json.dumps(fast_project, indent=4) + "\n")
    write_if_changed(source_manifest, "".join(f"{path}\n" for path in sources))
    print(len(sources))
    return 0


def cmd_profile(argv: list[str]) -> int:
    manifest = Path(argv[0]).resolve()
    profile = argv[1]
    sources = [Path(line.strip()) for line in manifest.read_text(encoding="utf-8").splitlines() if line.strip()]
    existing = [path for path in sources if path.exists()]
    total_bytes = sum(path.stat().st_size for path in existing)

    groups: dict[str, list[int]] = defaultdict(lambda: [0, 0])
    for path in existing:
        group = group_name(path)
        groups[group][0] += 1
        groups[group][1] += path.stat().st_size

    print(f"fast-dev[{profile}] profile: {len(existing)} sources, {total_bytes / 1024:.1f} KiB total")
    print("top source groups:")
    for group, (count, size) in sorted(groups.items(), key=lambda kv: kv[1][1], reverse=True)[:12]:
        print(f"{size:7d}  {count:3d}  {group}")
    print("top included sources:")
    for path in sorted(existing, key=lambda p: p.stat().st_size, reverse=True)[:25]:
        print(f"{path.stat().st_size:6d}  {path}")
    return 0


def cmd_is_up_to_date(argv: list[str]) -> int:
    output = Path(argv[0]).resolve()
    project_json = Path(argv[1]).resolve()
    chelper_archive = Path(argv[2]).resolve()
    ftxui_archive = Path(argv[3]).resolve()
    source_manifest = Path(argv[4]).resolve()
    script_path = Path(argv[5]).resolve()
    generator_path = Path(argv[6]).resolve()

    if not output.exists():
        return 1

    output_mtime = output.stat().st_mtime
    deps = [project_json, chelper_archive, ftxui_archive, source_manifest, script_path, generator_path]
    for dep in deps:
        if not dep.exists():
            return 1

    deps.extend(Path(line.strip()) for line in source_manifest.read_text(encoding="utf-8").splitlines() if line.strip())

    for dep in deps:
        if not dep.exists():
            return 1
        if dep.stat().st_mtime > output_mtime:
            return 1
    return 0


def main(argv: list[str]) -> int:
    if len(argv) == 2 and argv[1] in {"-h", "--help"}:
        print(USAGE)
        return 0
    if len(argv) < 2:
        print(USAGE, file=sys.stderr)
        return 2
    command = argv[1]
    expected_arg_count = COMMAND_ARG_COUNTS.get(command)
    if expected_arg_count is None:
        print(f"unknown command: {command}", file=sys.stderr)
        print(USAGE, file=sys.stderr)
        return 2
    actual_arg_count = len(argv) - 2
    if actual_arg_count != expected_arg_count:
        print(
            f"{command}: expected {expected_arg_count} arguments, got {actual_arg_count}",
            file=sys.stderr,
        )
        print(USAGE, file=sys.stderr)
        return 2
    if command == "generate":
        return cmd_generate(argv[2:])
    if command == "profile":
        return cmd_profile(argv[2:])
    if command == "is-up-to-date":
        return cmd_is_up_to_date(argv[2:])
    raise AssertionError(f"unhandled command: {command}")


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
