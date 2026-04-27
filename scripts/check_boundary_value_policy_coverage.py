#!/usr/bin/env python3
"""Verify Omni boundary policy coverage for every ValueTag."""

from __future__ import annotations

import csv
import argparse
import re
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
VALUE_TAG_FILE = ROOT / "src/lisp/value_runtime_constants.c3"
POLICY_FILE = ROOT / "src/lisp/value_boundary_ownership_policy.c3"
COPY_FILE = ROOT / "src/lisp/eval_promotion_copy.c3"
MANIFEST_FILE = ROOT / "scripts/boundary_value_policy_manifest.tsv"

OWNERSHIP_VALUES = {
    "BOUNDARY_VALUE_IMMEDIATE",
    "BOUNDARY_VALUE_HEAP_CLONE",
    "BOUNDARY_VALUE_GRAPH_CONTAINER",
    "BOUNDARY_VALUE_CLOSURE_GRAPH",
    "BOUNDARY_VALUE_SHARED_RUNTIME_WRAPPER",
    "BOUNDARY_VALUE_OPAQUE_FOREIGN",
    "BOUNDARY_VALUE_INTERPRETER_STABLE",
    "BOUNDARY_VALUE_UNSUPPORTED_BOUNDARY",
}
EDGE_VALUES = {
    "BOUNDARY_VALUE_EDGE_NONE",
    "BOUNDARY_VALUE_EDGE_VALUE",
    "BOUNDARY_VALUE_EDGE_ENV",
}
GRAPH_AUDIT_BY_EDGE = {
    "BOUNDARY_VALUE_EDGE_NONE": "none",
    "BOUNDARY_VALUE_EDGE_VALUE": "value",
    "BOUNDARY_VALUE_EDGE_ENV": "env",
}


def read(path: Path) -> str:
    try:
        return path.read_text(encoding="utf-8")
    except FileNotFoundError:
        fail(f"missing required file: {path.relative_to(ROOT)}")


def fail(message: str) -> None:
    print(f"FAIL: {message}", file=sys.stderr)
    raise SystemExit(1)


def strip_comments(text: str) -> str:
    return re.sub(r"//.*", "", text)


def enum_tags(text: str) -> list[str]:
    match = re.search(r"enum\s+ValueTag\s*:\s*char\s*\{(?P<body>.*?)\n\}", text, re.S)
    if not match:
        fail("could not find enum ValueTag")
    tags: list[str] = []
    for raw in strip_comments(match.group("body")).splitlines():
        item = raw.strip().rstrip(",")
        if item:
            tags.append(item)
    return tags


def function_body(text: str, name: str) -> str:
    start = text.find(f"fn ")
    while start != -1:
        sig_start = start
        brace = text.find("{", sig_start)
        if brace == -1:
            break
        signature = text[sig_start:brace]
        if re.search(rf"\b{name}\s*\(", signature):
            depth = 0
            for idx in range(brace, len(text)):
                if text[idx] == "{":
                    depth += 1
                elif text[idx] == "}":
                    depth -= 1
                    if depth == 0:
                        return text[brace + 1:idx]
            break
        start = text.find("fn ", brace + 1)
    fail(f"could not find function body: {name}")


def switch_mapping(body: str, return_prefix: str) -> dict[str, str]:
    mapping: dict[str, str] = {}
    pending: list[str] = []
    for line in strip_comments(body).splitlines():
        case = re.search(r"\bcase\s+([A-Z0-9_]+)\s*:", line)
        if case:
            pending.append(case.group(1))
            continue
        ret = re.search(rf"\breturn\s+({re.escape(return_prefix)}[A-Z0-9_]+)\s*;", line)
        if ret and pending:
            for tag in pending:
                mapping[tag] = ret.group(1)
            pending = []
    return mapping


def load_manifest() -> list[dict[str, str]]:
    rows: list[dict[str, str]] = []
    with MANIFEST_FILE.open(encoding="utf-8", newline="") as fh:
        filtered = (line for line in fh if line.strip() and not line.startswith("#"))
        reader = csv.DictReader(
            filtered,
            delimiter="\t",
            fieldnames=[
                "tag",
                "ownership",
                "edge",
                "copy_route",
                "stable_materialize",
                "graph_audit",
                "destructor",
                "native_exclusion",
                "rollback",
            ],
        )
        rows.extend(reader)
    return rows


def expected_stable_materialize(row: dict[str, str]) -> str:
    ownership = row["ownership"]
    tag = row["tag"]
    if ownership in {
        "BOUNDARY_VALUE_GRAPH_CONTAINER",
        "BOUNDARY_VALUE_CLOSURE_GRAPH",
    }:
        return "yes"
    if ownership == "BOUNDARY_VALUE_HEAP_CLONE" and tag != "ERROR":
        return "yes"
    if ownership == "BOUNDARY_VALUE_IMMEDIATE" and tag == "TIME_POINT":
        return "yes"
    return "no"


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Verify Omni boundary policy coverage for every ValueTag.")
    parser.parse_args(argv)

    tags = enum_tags(read(VALUE_TAG_FILE))
    manifest = load_manifest()
    manifest_tags = [row["tag"] for row in manifest]
    if manifest_tags != tags:
        missing = [tag for tag in tags if tag not in manifest_tags]
        extra = [tag for tag in manifest_tags if tag not in tags]
        order = "yes" if set(tags) == set(manifest_tags) else "no"
        fail(
            "boundary policy manifest does not match ValueTag enum "
            f"(missing={missing}, extra={extra}, same_set={order})"
        )

    policy_text = read(POLICY_FILE)
    copy_text = read(COPY_FILE)
    ownership_map = switch_mapping(
        function_body(policy_text, "boundary_value_ownership_policy"),
        "BOUNDARY_VALUE_",
    )
    edge_map = switch_mapping(
        function_body(policy_text, "boundary_value_edge_policy"),
        "BOUNDARY_VALUE_EDGE_",
    )
    copy_map = switch_mapping(
        function_body(copy_text, "copy_parent_route_for_tag"),
        "CP_ROUTE_",
    )

    for row in manifest:
        tag = row["tag"]
        ownership = row["ownership"]
        edge = row["edge"]
        copy_route = row["copy_route"]
        stable = row["stable_materialize"]
        graph_audit = row["graph_audit"]

        if ownership not in OWNERSHIP_VALUES:
            fail(f"{tag}: unknown ownership policy {ownership}")
        if edge not in EDGE_VALUES:
            fail(f"{tag}: unknown edge policy {edge}")
        if stable not in {"yes", "no"}:
            fail(f"{tag}: stable_materialize must be yes/no, got {stable}")
        if ownership_map.get(tag) != ownership:
            fail(f"{tag}: ownership switch={ownership_map.get(tag)} manifest={ownership}")
        if edge_map.get(tag) != edge:
            fail(f"{tag}: edge switch={edge_map.get(tag)} manifest={edge}")
        if copy_map.get(tag) != copy_route:
            fail(f"{tag}: copy route switch={copy_map.get(tag)} manifest={copy_route}")
        expected_graph_audit = GRAPH_AUDIT_BY_EDGE[edge]
        if graph_audit != expected_graph_audit:
            fail(f"{tag}: graph_audit={graph_audit}, expected {expected_graph_audit}")
        expected_stable = expected_stable_materialize(row)
        if stable != expected_stable:
            fail(f"{tag}: stable_materialize={stable}, expected {expected_stable}")
        if ownership == "BOUNDARY_VALUE_OPAQUE_FOREIGN":
            if edge != "BOUNDARY_VALUE_EDGE_NONE" or stable != "no":
                fail(f"{tag}: opaque foreign values must have no Omni edges and no stable materialization")
            if row["native_exclusion"] != "opaque-foreign":
                fail(f"{tag}: opaque foreign values require native_exclusion=opaque-foreign")
        if edge != "BOUNDARY_VALUE_EDGE_NONE" and row["rollback"] == "none":
            fail(f"{tag}: graph-carrying values require an explicit rollback policy")

    print(f"PASS: boundary value policy coverage verified for {len(tags)} ValueTag entries")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
