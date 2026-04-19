#!/usr/bin/env python3
from __future__ import annotations

import hashlib
import json
import os
import re
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Any


SPECIAL_FORMS: dict[str, str] = {
    "define": "Bind a name or define a function. Function shorthand is (define (name args...) body).",
    "lambda": "Create an anonymous function with strict arity.",
    "let": "Introduce flat-pair bindings: (let (x 1 y 2) body).",
    "if": "Conditional form with explicit then and else branches.",
    "block": "Evaluate expressions in order and return the last value.",
    "set!": "Mutate a binding, pair slot, or generic collection location.",
    "quote": "Return datum without evaluation.",
    "match": "Pattern matching form with exhaustive branch-style clauses.",
    "handle": "Install algebraic effect handlers for a dynamic scope.",
    "signal": "Raise an effect to the nearest matching handler.",
    "resolve": "Resume a captured effect continuation from inside a handler.",
    "with-continuation": "Bind and reuse the continuation supplied by a handle clause.",
    "checkpoint": "Delimited continuation boundary.",
    "capture": "Capture the current delimited continuation.",
    "module": "Declare a module at top level.",
    "import": "Import another module or selected exports.",
    "export-from": "Re-export specific symbols from another module.",
    "partial": "Explicit partial application helper.",
    "explain": "Inspect dispatch or effect resolution using a quoted selector.",
}

BUILTIN_VALUES: dict[str, str] = {
    "nil": "The only absence value and one of the two falsy values.",
    "false": "Alias-level falsy value; only nil and false are falsy in Omni.",
    "true": "Truthy boolean value.",
    "Array": "Array constructor and descriptive type symbol.",
    "Dictionary": "Dictionary constructor and descriptive type symbol.",
    "List": "List constructor and descriptive type symbol.",
    "Integer": "Integer constructor and descriptive type symbol.",
    "Double": "Double constructor and descriptive type symbol.",
    "String": "String constructor and descriptive type symbol.",
    "Boolean": "Boolean constructor and descriptive type symbol.",
    "length": "Generic collection length operation.",
    "ref": "Generic keyed or indexed lookup operation.",
    "map": "Apply a function across a collection.",
    "filter": "Keep elements that satisfy a predicate.",
    "foldl": "Left fold over a collection.",
    "type-of": "Return the descriptive Omni type symbol for a value.",
}

SIGNATURE_ITEMS: dict[str, list[dict[str, Any]]] = {
    "define": [
        {
            "label": "(define name value)",
            "documentation": "Bind a name to a value.",
            "parameters": ["name", "value"],
        },
        {
            "label": "(define (name args...) body)",
            "documentation": "Define a function with strict arity.",
            "parameters": ["name", "args...", "body"],
        },
    ],
    "lambda": [
        {
            "label": "(lambda (args...) body)",
            "documentation": "Create an anonymous function with strict arity.",
            "parameters": ["args...", "body"],
        }
    ],
    "let": [
        {
            "label": "(let (name value ...) body)",
            "documentation": "Introduce flat-pair bindings, then evaluate the body.",
            "parameters": ["bindings", "body"],
        }
    ],
    "if": [
        {
            "label": "(if condition then else)",
            "documentation": "Evaluate the then or else branch based on the condition.",
            "parameters": ["condition", "then", "else"],
        }
    ],
    "block": [
        {
            "label": "(block expr ...)",
            "documentation": "Evaluate expressions in order and return the last result.",
            "parameters": ["expr", "..."],
        }
    ],
}

COMPLETION_ITEMS: list[dict[str, Any]] = []
for name, docs in SPECIAL_FORMS.items():
    COMPLETION_ITEMS.append(
        {
            "label": name,
            "kind": 14,
            "detail": "special form",
            "documentation": docs,
            "insertText": name,
        }
    )
for name, docs in BUILTIN_VALUES.items():
    COMPLETION_ITEMS.append(
        {
            "label": name,
            "kind": 3,
            "detail": "builtin",
            "documentation": docs,
            "insertText": name,
        }
    )
COMPLETION_ITEMS.sort(key=lambda item: item["label"])

SEMANTIC_TOKEN_TYPES = [
    "namespace",
    "type",
    "event",
    "function",
    "variable",
    "parameter",
    "macro",
    "keyword",
    "string",
    "number",
    "comment",
]
SEMANTIC_TOKEN_TYPE_INDEX = {
    token_type: index
    for index, token_type in enumerate(SEMANTIC_TOKEN_TYPES)
}
SEMANTIC_TOKEN_MODIFIERS = ["declaration"]
SEMANTIC_TOKEN_MODIFIER_INDEX = {
    modifier: index
    for index, modifier in enumerate(SEMANTIC_TOKEN_MODIFIERS)
}
SEMANTIC_TOKEN_TYPE_BY_DETAIL = {
    "module": "namespace",
    "type": "type",
    "abstract": "type",
    "union": "type",
    "alias": "type",
    "effect": "event",
    "function": "function",
    "define": "variable",
    "macro": "macro",
    "ffi": "variable",
    "relation": "type",
    "rule": "function",
    "schema": "type",
}
NUMERIC_ATOM_RE = re.compile(r"[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?$")

OMNI_BIN = os.environ.get("OMNI_LSP_OMNI_BIN", "omni")
OMNI_LD_LIBRARY_PATH = os.environ.get("OMNI_LSP_LD_LIBRARY_PATH")
FORM_PAIRS = {"(": ")", "[": "]", "{": "}"}
BLOCK_FORMAT_HEADS = {
    "define",
    "lambda",
    "let",
    "if",
    "block",
    "handle",
    "when",
    "unless",
    "raise",
    "match",
    "checkpoint",
    "module",
    "import",
    "syntax-match",
    "template",
}
SECOND_TOKEN_ALIGNMENT_HEADS = {"export", "export-from"}
HIGHER_ORDER_CALL_HEADS = {
    "map",
    "foldl",
    "foldr",
    "filter",
    "find",
    "partition",
    "sort-by",
    "for-each",
}
COROUTINE_WRAPPER_HEADS = {"Coroutine"}
WORKSPACE_ROOT_MARKERS = ("project.json", ".git")
WORKSPACE_SKIP_DIRS = {
    ".git",
    ".hg",
    ".svn",
    ".direnv",
    ".cache",
    "__pycache__",
    "build",
    "deps",
    "dist",
    "node_modules",
    "vendor",
}
WORKSPACE_SYMBOL_RESULT_LIMIT = 200
WORKSPACE_COMPLETION_RESULT_LIMIT = 200
DECLARATION_SYMBOL_KINDS: dict[str, int] = {
    "module": 2,
    "function": 12,
    "variable": 13,
    "type": 23,
    "abstract": 11,
    "union": 23,
    "alias": 26,
    "effect": 24,
    "macro": 12,
    "ffi": 13,
    "relation": 19,
    "rule": 19,
    "schema": 19,
}
COMPLETION_KIND_BY_SYMBOL_KIND: dict[int, int] = {
    2: 9,
    11: 8,
    12: 3,
    13: 6,
    19: 12,
    23: 22,
    24: 23,
    26: 25,
}
DECLARATION_DETAIL_LABELS: dict[str, str] = {
    "module": "module",
    "function": "function",
    "define": "binding",
    "type": "type",
    "abstract": "abstract type",
    "union": "union type",
    "alias": "type alias",
    "effect": "effect",
    "macro": "macro",
    "ffi": "ffi binding",
    "relation": "relation",
    "rule": "rule",
    "schema": "schema",
}
TYPE_LIKE_DETAILS = {"type", "abstract", "union", "alias"}
NON_CALLABLE_DECLARATION_DETAILS = {"module", *TYPE_LIKE_DETAILS, "relation", "rule", "schema"}


@dataclass
class Document:
    uri: str
    text: str
    version: int | None
    path: Path | None


@dataclass
class Declaration:
    name: str
    detail: str
    kind: int
    start: int
    end: int
    selection_start: int
    selection_end: int
    children: list["Declaration"] | None = None


@dataclass
class WorkspaceCacheEntry:
    symbol_records: dict[Path, list["WorkspaceSymbolRecord"]]
    declaration_records: dict[Path, list["WorkspaceDeclarationRecord"]]
    manifest: dict[Path, tuple[int, int]]


@dataclass
class WorkspaceSymbolRecord:
    name: str
    name_lower: str
    detail: str
    kind: int
    uri: str
    range: dict[str, Any]
    container_name: str | None
    snippet: str
    parameter_labels: list[str]


@dataclass
class CallSite:
    symbol: str
    range: dict[str, Any]


@dataclass
class WorkspaceDeclarationRecord:
    name: str
    name_lower: str
    detail: str
    kind: int
    uri: str
    range: dict[str, Any]
    selection_range: dict[str, Any]
    container_name: str | None
    snippet: str
    parameter_labels: list[str]
    call_sites: list[CallSite]


@dataclass
class FormatFrame:
    close_char: str
    open_column: int
    child_column: int
    let_binding_pending: bool = False
    binding_context: bool = False
    higher_order_lambda_context: bool = False
    coroutine_lambda_context: bool = False
