from __future__ import annotations

import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Any

from smoke_helpers import decode_semantic_tokens, expect, read_message, send_message


@dataclass
class SmokeContext:
    tempdir: tempfile.TemporaryDirectory[str]
    root: Path
    main_uri: str
    workspace_uri: str
    unopened_uri: str
    caller_uri: str
    bridge_uri: str
    actions_uri: str
    folds_uri: str
    linked_uri: str
    format_uri: str
    continuation_format_uri: str
    branch_format_uri: str
    generic_format_uri: str
    nested_if_format_uri: str
    control_forms_format_uri: str
    higher_order_format_uri: str
    coroutine_format_uri: str
    clause_data_format_uri: str
    crlf_format_uri: str
    importer_uri: str
    semantic_uri: str
    typed_uri: str


def _write(path: Path, text: str, newline: str | None = None) -> str:
    if newline is None:
        path.write_text(text, encoding="utf-8")
    else:
        with path.open("w", encoding="utf-8", newline=newline) as handle:
            handle.write(text)
    return path.resolve().as_uri()


def _bootstrap_workspace() -> SmokeContext:
    tempdir = tempfile.TemporaryDirectory(prefix="omni_lsp_workspace_")
    root = Path(tempdir.name)
    (root / "project.json").write_text("{}\n", encoding="utf-8")

    main_uri = _write(
        root / "main.omni",
        "(module demo (export ping)\n"
        "  (define answer 42)\n"
        "  (define (ping x) (+ x answer))\n"
        "  (define [effect] (io/ping (^String msg)))\n"
        "  (define [type] Point (^Integer x) (^Integer y)))\n",
    )
    workspace_uri = _write(
        root / "extra.omni",
        "(module extra\n"
        "  (define helper (lambda (value) value))\n"
        "  (define (describe-extra value) value))\n",
    )
    unopened_uri = _write(
        root / "library.omni",
        "(module library\n"
        "  (define (describe-file value) value)\n"
        "  (define stash 9))\n",
    )
    caller_uri = _write(root / "caller.omni", "(helper 2)\n(describe-updated 1)\n")
    bridge_uri = _write(
        root / "bridge.omni",
        "(module bridge\n"
        "  (define (bridge value)\n"
        "    (helper value)\n"
        "    (describe-updated value)))\n",
    )
    actions_uri = _write(
        root / "actions.omni",
        "(define (wrap-me x)\n"
        "  (+ x 1)\n"
        "  (+ x 2))\n\n"
        "(define unwrap-me\n"
        "  (lambda (value)\n"
        "    (block\n"
        "      value)))\n",
    )
    folds_uri = _write(
        root / "folds.omni",
        "(module folds\n"
        "  (define (complex x y)\n"
        "    (block\n"
        "      (+ x y)\n"
        "      (let (z (+ x y))\n"
        "        z))))\n",
    )
    linked_uri = _write(
        root / "linked.omni",
        "(define (echo (^String msg) suffix)\n"
        "  (block\n"
        "    msg\n"
        "    (list msg suffix)\n"
        "    (define (inner msg) msg)\n"
        "    suffix))\n",
    )
    format_uri = _write(
        root / "format.omni",
        "(define (sum x y)\n"
        "(block\n"
        "(+ x y)   \n"
        "(let (z (+ x y))\n"
        "z)))\n",
    )
    continuation_format_uri = _write(
        root / "continuation_format.omni",
        "(module sample\n"
        "(export alpha beta gamma\n"
        "delta epsilon)\n"
        "(define (pair-sum pairs)\n"
        "(let (left (car pairs)\n"
        "right (cadr pairs))\n"
        "(+ left right))))\n",
    )
    branch_format_uri = _write(
        root / "branch_format.omni",
        "(if (= 1 1)\n"
        "100\n"
        "200)\n\n"
        "(let ^rec (fact (lambda (n)\n"
        "(if (= n 0)\n"
        "1\n"
        "(* n (fact (- n 1))))))\n"
        "(fact 5))\n",
    )
    generic_format_uri = _write(
        root / "generic_format.omni",
        "(define (pipeline raw)\n"
        "(|> (tcp-read raw)\n"
        "decode-request\n"
        "execute\n"
        "respond))\n",
    )
    nested_if_format_uri = _write(
        root / "nested_if_format.omni",
        "(match method\n"
        "(\"GET\"    (if ready\n"
        "(let (item (repo/get-item ready))\n"
        "(if item (Ok item) (Err \"missing\")))\n"
        "(if (= method \"/items\")\n"
        "(Ok (repo/list))\n"
        "(Err \"route not found\"))))\n"
        "(_        (Err \"route not found\")))\n",
    )
    control_forms_format_uri = _write(
        root / "control_forms_format.omni",
        "(define (guarded retry value)\n"
        "(let (result (checkpoint\n"
        "(unless retry\n"
        "(raise err\n"
        "(recover err)))))\n"
        "(when value\n"
        "(emit value))\n"
        "result))\n",
    )
    higher_order_format_uri = _write(
        root / "higher_order_format.omni",
        "(define (collect prices)\n"
        "(foldl (lambda (acc price)\n"
        "(let (bucket (map (lambda (x)\n"
        "(* x price))\n"
        "acc))\n"
        "(cons bucket acc)))\n"
        "nil\n"
        "prices))\n",
    )
    coroutine_format_uri = _write(
        root / "coroutine_format.omni",
        "(define (make-worker seed)\n"
        "(Coroutine (lambda ()\n"
        "(let loop (n seed)\n"
        "(if (= n 0)\n"
        "n\n"
        "(block\n"
        "(yield n)\n"
        "(loop (- n 1)))))))\n",
    )
    clause_data_format_uri = _write(
        root / "clause_data_format.omni",
        "(define (summarize result pair)\n"
        "(let (payload (match result\n"
        "((Ok value)\n"
        "(let (scaled (+ value 1))\n"
        "(Ok scaled)))\n"
        "((Err msg) (Err msg))))\n"
        "(let ([left right] pair)\n"
        "{'payload payload\n"
        "'window [(- right left)\n"
        "(+ left right)]\n"
        "'total (+ left right)})))\n",
    )
    crlf_format_uri = _write(
        root / "crlf_format.omni",
        "(module windows\r\n"
        "(export alpha beta\r\n"
        "gamma)\r\n",
        newline="",
    )
    importer_uri = _write(root / "imports.omni", '(import "extra.omni" \'all)\n(export-from library \'all)\n')
    semantic_uri = _write(
        root / "semantic.omni",
        "; note\n"
        "(module semantic\n"
        "  (define answer 42)\n"
        "  (define (ping (^String msg) count)\n"
        "    (if true msg \"fallback\")))\n",
    )
    typed_uri = _write(
        root / "typed.omni",
        "(module typed\n"
        "  (define [type] Point (^Integer x) (^Integer y))\n"
        "  (define (point-id (^Point p)) p))\n",
    )
    return SmokeContext(
        tempdir=tempdir,
        root=root,
        main_uri=main_uri,
        workspace_uri=workspace_uri,
        unopened_uri=unopened_uri,
        caller_uri=caller_uri,
        bridge_uri=bridge_uri,
        actions_uri=actions_uri,
        folds_uri=folds_uri,
        linked_uri=linked_uri,
        format_uri=format_uri,
        continuation_format_uri=continuation_format_uri,
        branch_format_uri=branch_format_uri,
        generic_format_uri=generic_format_uri,
        nested_if_format_uri=nested_if_format_uri,
        control_forms_format_uri=control_forms_format_uri,
        higher_order_format_uri=higher_order_format_uri,
        coroutine_format_uri=coroutine_format_uri,
        clause_data_format_uri=clause_data_format_uri,
        crlf_format_uri=crlf_format_uri,
        importer_uri=importer_uri,
        semantic_uri=semantic_uri,
        typed_uri=typed_uri,
    )


def run_capabilities_and_workspace(proc, ctx: SmokeContext) -> None:
    send_message(proc, {"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {}})
    init = read_message(proc)
    capabilities = init["result"]["capabilities"]
    diagnostic_provider = capabilities.get("diagnosticProvider") or {}
    expect(diagnostic_provider.get("interFileDependencies") is False, "diagnostic provider inter-file flag mismatch")
    expect(diagnostic_provider.get("workspaceDiagnostics") is True, "diagnostic provider workspace flag mismatch")
    for key, message in [
        ("documentSymbolProvider", "document symbols not advertised"),
        ("monikerProvider", "monikers not advertised"),
        ("documentHighlightProvider", "document highlights not advertised"),
        ("declarationProvider", "declaration provider not advertised"),
        ("implementationProvider", "implementation provider not advertised"),
        ("typeDefinitionProvider", "type definition provider not advertised"),
        ("inlayHintProvider", "inlay hints not advertised"),
        ("foldingRangeProvider", "folding ranges not advertised"),
        ("selectionRangeProvider", "selection ranges not advertised"),
        ("linkedEditingRangeProvider", "linked editing not advertised"),
        ("documentFormattingProvider", "document formatting not advertised"),
        ("documentRangeFormattingProvider", "range formatting not advertised"),
        ("callHierarchyProvider", "call hierarchy not advertised"),
        ("codeActionProvider", "code actions not advertised"),
        ("workspaceSymbolProvider", "workspace symbols not advertised"),
    ]:
        expect(capabilities.get(key) is True, message)
    expect(capabilities.get("signatureHelpProvider") is not None, "signature help not advertised")
    on_type_provider = capabilities.get("documentOnTypeFormattingProvider") or {}
    expect(on_type_provider.get("firstTriggerCharacter") == "\n", "on-type formatting not advertised")
    expect(")" in (on_type_provider.get("moreTriggerCharacter") or []), "on-type formatting missing ) trigger")
    semantic_provider = capabilities.get("semanticTokensProvider") or {}
    expect(semantic_provider.get("full") is True, "semantic tokens not advertised")
    semantic_legend = semantic_provider.get("legend") or {}
    expect("keyword" in (semantic_legend.get("tokenTypes") or []), "semantic token legend missing keyword")
    expect(capabilities.get("documentLinkProvider") is not None, "document links not advertised")
    expect((capabilities.get("codeLensProvider") or {}).get("resolveProvider") is True, "code lens resolve not advertised")
    expect((capabilities.get("completionProvider") or {}).get("resolveProvider") is True, "completion resolve not advertised")

    send_message(proc, {"jsonrpc": "2.0", "method": "initialized", "params": {}})
    send_message(proc, {"jsonrpc": "2.0", "method": "textDocument/didOpen", "params": {"textDocument": {"uri": ctx.main_uri, "version": 1, "text": (ctx.root / "main.omni").read_text(encoding="utf-8")}}})
    expect(read_message(proc).get("method") == "textDocument/publishDiagnostics", "expected diagnostics notification after open")
    send_message(proc, {"jsonrpc": "2.0", "id": 11_1, "method": "textDocument/diagnostic", "params": {"textDocument": {"uri": ctx.main_uri}}})
    main_pull_diagnostics = read_message(proc)["result"]
    expect(main_pull_diagnostics.get("kind") == "full", "expected full pull diagnostics for clean document")
    expect(main_pull_diagnostics.get("items") == [], "expected empty pull diagnostics for clean document")
    expect(isinstance(main_pull_diagnostics.get("resultId"), str), "expected diagnostic result id on clean document")
    send_message(proc, {"jsonrpc": "2.0", "method": "textDocument/didOpen", "params": {"textDocument": {"uri": ctx.workspace_uri, "version": 1, "text": (ctx.root / "extra.omni").read_text(encoding="utf-8")}}})
    expect(read_message(proc).get("method") == "textDocument/publishDiagnostics", "expected diagnostics notification after second open")

    send_message(proc, {"jsonrpc": "2.0", "id": 2, "method": "textDocument/documentSymbol", "params": {"textDocument": {"uri": ctx.main_uri}}})
    symbols = read_message(proc)["result"]
    expect(len(symbols) == 1, "expected one top-level module symbol")
    expect(symbols[0]["name"] == "demo", "expected module symbol named demo")
    expect([child["name"] for child in symbols[0].get("children", [])] == ["answer", "ping", "io/ping", "Point"], "unexpected module child symbols")

    send_message(proc, {"jsonrpc": "2.0", "id": 2_1, "method": "textDocument/moniker", "params": {"textDocument": {"uri": ctx.main_uri}, "position": {"line": 2, "character": 12}}})
    expect(
        read_message(proc)["result"] == [{
            "scheme": "omni",
            "identifier": "demo:function:ping",
            "unique": "project",
            "kind": "export",
        }],
        "unexpected local moniker payload",
    )
    send_message(proc, {"jsonrpc": "2.0", "id": 3, "method": "textDocument/completion", "params": {"textDocument": {"uri": ctx.main_uri}, "position": {"line": 2, "character": 10}}})
    completion_items = read_message(proc)["result"]["items"]
    expect([item["label"] for item in completion_items[:5]] == ["demo", "answer", "ping", "io/ping", "Point"], "expected local completions first")

    send_message(proc, {"jsonrpc": "2.0", "method": "textDocument/didOpen", "params": {"textDocument": {"uri": ctx.importer_uri, "version": 1, "text": (ctx.root / "imports.omni").read_text(encoding="utf-8")}}})
    expect(read_message(proc).get("method") == "textDocument/publishDiagnostics", "expected diagnostics notification after imports open")
    send_message(proc, {"jsonrpc": "2.0", "id": 49, "method": "textDocument/documentLink", "params": {"textDocument": {"uri": ctx.importer_uri}}})
    document_links = read_message(proc)["result"]
    expect([item["target"] for item in document_links] == [ctx.workspace_uri, ctx.unopened_uri], "expected import and export-from links to resolve to workspace files")

    send_message(proc, {"jsonrpc": "2.0", "method": "textDocument/didOpen", "params": {"textDocument": {"uri": ctx.semantic_uri, "version": 1, "text": (ctx.root / "semantic.omni").read_text(encoding="utf-8")}}})
    expect(read_message(proc).get("method") == "textDocument/publishDiagnostics", "expected diagnostics notification after semantic open")
    send_message(proc, {"jsonrpc": "2.0", "id": 50, "method": "textDocument/semanticTokens/full", "params": {"textDocument": {"uri": ctx.semantic_uri}}})
    semantic_tokens = decode_semantic_tokens(read_message(proc)["result"]["data"], semantic_legend["tokenTypes"], semantic_legend["tokenModifiers"])
    def find_token(line: int, start: int, token_type: str) -> dict[str, Any] | None:
        for token in semantic_tokens:
            if token["line"] == line and token["start"] == start and token["type"] == token_type:
                return token
        return None
    semantic_lines = (ctx.root / "semantic.omni").read_text(encoding="utf-8").splitlines()
    expect(find_token(1, semantic_lines[1].index("module"), "keyword") is not None, "expected semantic token for module keyword")
    expect(find_token(1, semantic_lines[1].index("semantic"), "namespace") is not None, "expected semantic token for module name")
    expect(find_token(2, semantic_lines[2].index("answer"), "variable") is not None, "expected semantic token for binding name")
    expect(find_token(3, semantic_lines[3].index("ping"), "function") is not None, "expected semantic token for function name")
    expect(find_token(4, semantic_lines[4].index("\"fallback\""), "string") is not None, "expected semantic token for string literal")
    expect(find_token(0, 0, "comment") is not None, "expected semantic token for comment")


def run_navigation_and_workspace_fallback(proc, ctx: SmokeContext) -> None:
    send_message(proc, {"jsonrpc": "2.0", "id": 44, "method": "textDocument/codeLens", "params": {"textDocument": {"uri": ctx.main_uri}}})
    code_lenses = read_message(proc)["result"]
    answer_lens = next(lens for lens in code_lenses if lens["range"]["start"] == {"line": 1, "character": 10})
    point_lens = next(lens for lens in code_lenses if lens["range"]["start"] == {"line": 4, "character": 17})
    expect("command" not in answer_lens, "expected unresolved answer codelens payload")
    expect("command" not in point_lens, "expected unresolved Point codelens payload")
    send_message(proc, {"jsonrpc": "2.0", "id": 46, "method": "codeLens/resolve", "params": answer_lens})
    answer_lens = read_message(proc)["result"]
    send_message(proc, {"jsonrpc": "2.0", "id": 47, "method": "codeLens/resolve", "params": point_lens})
    point_lens = read_message(proc)["result"]
    expect(answer_lens["command"]["title"] == "1 ref", "expected one workspace-aware ref codelens for answer")
    expect(point_lens["command"]["title"] == "2 refs", "expected workspace-aware refs codelens for Point")
    send_message(proc, {"jsonrpc": "2.0", "id": 4, "method": "textDocument/definition", "params": {"textDocument": {"uri": ctx.main_uri}, "position": {"line": 2, "character": 24}}})
    expect(read_message(proc)["result"][0]["range"]["start"] == {"line": 1, "character": 10}, "unexpected definition target")
    send_message(proc, {"jsonrpc": "2.0", "id": 32, "method": "textDocument/declaration", "params": {"textDocument": {"uri": ctx.main_uri}, "position": {"line": 2, "character": 24}}})
    expect(read_message(proc)["result"][0]["range"]["start"] == {"line": 1, "character": 10}, "unexpected declaration target")
    send_message(proc, {"jsonrpc": "2.0", "id": 33, "method": "textDocument/implementation", "params": {"textDocument": {"uri": ctx.main_uri}, "position": {"line": 2, "character": 24}}})
    expect(read_message(proc)["result"][0]["range"]["start"] == {"line": 1, "character": 10}, "unexpected implementation target")
    send_message(proc, {"jsonrpc": "2.0", "id": 34, "method": "textDocument/typeDefinition", "params": {"textDocument": {"uri": ctx.typed_uri}, "position": {"line": 2, "character": 22}}})
    type_definition = read_message(proc)["result"]
    expect(
        type_definition is None
        or (isinstance(type_definition, list) and len(type_definition) == 1 and type_definition[0]["range"]["start"] == {"line": 1, "character": 17}),
        "unexpected type definition target",
    )
    send_message(proc, {"jsonrpc": "2.0", "id": 5, "method": "textDocument/references", "params": {"textDocument": {"uri": ctx.main_uri}, "position": {"line": 2, "character": 24}, "context": {"includeDeclaration": True}}})
    expect(len(read_message(proc)["result"]) == 2, "expected declaration + use references")
    send_message(proc, {"jsonrpc": "2.0", "id": 6, "method": "textDocument/documentHighlight", "params": {"textDocument": {"uri": ctx.main_uri}, "position": {"line": 2, "character": 24}}})
    highlights = read_message(proc)["result"]
    expect(len(highlights) == 2 and highlights[0]["kind"] == 3 and highlights[1]["kind"] == 2, "expected local highlights for answer")
    send_message(proc, {"jsonrpc": "2.0", "id": 7, "method": "workspace/symbol", "params": {"query": "ping"}})
    workspace_symbols = read_message(proc)["result"]
    expect([item["name"] for item in workspace_symbols] == ["ping", "io/ping", "ping"], "unexpected workspace symbol names for ping query")
    expect([item["containerName"] for item in workspace_symbols] == ["demo", "demo", "semantic"], "expected module container names on workspace symbols")

    _write(ctx.root / "library.omni", "(module library\n  (define (describe-updated value) value)\n  (define stash 9))\n")
    send_message(proc, {"jsonrpc": "2.0", "id": 8, "method": "workspace/symbol", "params": {"query": "updated"}})
    refreshed_symbols = read_message(proc)["result"]
    expect([item["name"] for item in refreshed_symbols] == ["describe-updated"], "expected workspace symbol cache refresh to notice unopened file changes")
    expect([item["location"]["uri"] for item in refreshed_symbols] == [ctx.unopened_uri], "expected refreshed workspace symbol to come from the unopened workspace file")
    send_message(proc, {"jsonrpc": "2.0", "method": "textDocument/didOpen", "params": {"textDocument": {"uri": ctx.caller_uri, "version": 1, "text": (ctx.root / "caller.omni").read_text(encoding="utf-8")}}})
    expect(read_message(proc).get("method") == "textDocument/publishDiagnostics", "expected diagnostics notification after caller open")
    send_message(proc, {"jsonrpc": "2.0", "id": 9, "method": "textDocument/definition", "params": {"textDocument": {"uri": ctx.caller_uri}, "position": {"line": 1, "character": 2}}})
    expect(read_message(proc)["result"][0]["uri"] == ctx.unopened_uri, "expected workspace fallback definition to resolve into unopened workspace file")
    send_message(proc, {"jsonrpc": "2.0", "id": 10, "method": "textDocument/hover", "params": {"textDocument": {"uri": ctx.caller_uri}, "position": {"line": 1, "character": 2}}})
    workspace_hover = read_message(proc)["result"]
    expect("workspace function" in workspace_hover["contents"]["value"], "expected workspace hover label")
    expect("(define (describe-updated value) value)" in workspace_hover["contents"]["value"], "expected workspace hover snippet from unopened workspace file")
    send_message(proc, {"jsonrpc": "2.0", "id": 10_1, "method": "textDocument/moniker", "params": {"textDocument": {"uri": ctx.caller_uri}, "position": {"line": 1, "character": 2}}})
    workspace_monikers = read_message(proc)["result"]
    expect(workspace_monikers[0] == {"scheme": "omni", "identifier": "library:function:describe-updated", "unique": "project", "kind": "import"}, "unexpected workspace fallback moniker payload")
    send_message(proc, {"jsonrpc": "2.0", "id": 11, "method": "textDocument/signatureHelp", "params": {"textDocument": {"uri": ctx.caller_uri}, "position": {"line": 1, "character": 17}}})
    workspace_signature_help = read_message(proc)["result"]
    expect([item["label"] for item in workspace_signature_help["signatures"]] == ["(describe-updated value)"], "expected workspace signature help label from unopened workspace file")
    expect(workspace_signature_help["activeParameter"] == 0, "expected first active parameter for single-argument workspace function")
    send_message(proc, {"jsonrpc": "2.0", "id": 12, "method": "textDocument/inlayHint", "params": {"textDocument": {"uri": ctx.caller_uri}, "range": {"start": {"line": 0, "character": 0}, "end": {"line": 1, "character": 19}}}})
    caller_inlay_hints = read_message(proc)["result"]
    expect([item["label"] for item in caller_inlay_hints] == ["value:", "value:"], "expected workspace-backed parameter inlay hints for helper and describe-updated")
    send_message(proc, {"jsonrpc": "2.0", "id": 13, "method": "textDocument/completion", "params": {"textDocument": {"uri": ctx.caller_uri}, "position": {"line": 0, "character": 10}}})
    workspace_completion_items = read_message(proc)["result"]["items"]
    expect("describe-updated" in [item["label"] for item in workspace_completion_items], "expected unopened-workspace completion item after cache refresh")
    send_message(proc, {"jsonrpc": "2.0", "id": 43, "method": "completionItem/resolve", "params": next(item for item in workspace_completion_items if item["label"] == "describe-updated")})
    resolved_describe_updated = read_message(proc)["result"]
    expect("documentation" in resolved_describe_updated and "describe-updated" in resolved_describe_updated["documentation"]["value"], "expected workspace completion resolve snippet for describe-updated")
    send_message(proc, {"jsonrpc": "2.0", "id": 35, "method": "textDocument/prepareCallHierarchy", "params": {"textDocument": {"uri": ctx.bridge_uri}, "position": {"line": 1, "character": 12}}})
    bridge_hierarchy_items = read_message(proc)["result"]
    if isinstance(bridge_hierarchy_items, list) and bridge_hierarchy_items:
        expect(bridge_hierarchy_items[0]["name"] == "bridge", "expected prepared bridge hierarchy item")
        send_message(proc, {"jsonrpc": "2.0", "id": 36, "method": "callHierarchy/outgoingCalls", "params": {"item": bridge_hierarchy_items[0]}})
        bridge_outgoing = read_message(proc)["result"]
        if isinstance(bridge_outgoing, list):
            expect([item["to"]["name"] for item in bridge_outgoing] == ["describe-updated", "helper"], "unexpected outgoing call target names")
    send_message(proc, {"jsonrpc": "2.0", "id": 37, "method": "textDocument/prepareCallHierarchy", "params": {"textDocument": {"uri": ctx.bridge_uri}, "position": {"line": 3, "character": 6}}})
    describe_hierarchy_items = read_message(proc)["result"]
    if isinstance(describe_hierarchy_items, list) and describe_hierarchy_items:
        expect(describe_hierarchy_items[0]["uri"] == ctx.unopened_uri, "expected call hierarchy preparation to fall back to unopened workspace declaration")
        send_message(proc, {"jsonrpc": "2.0", "id": 38, "method": "callHierarchy/incomingCalls", "params": {"item": describe_hierarchy_items[0]}})
        describe_incoming = read_message(proc)["result"]
        if isinstance(describe_incoming, list) and describe_incoming:
            expect(describe_incoming[0]["from"]["name"] == "bridge", "expected incoming call to come from bridge declaration")
    send_message(proc, {"jsonrpc": "2.0", "id": 45, "method": "textDocument/codeLens", "params": {"textDocument": {"uri": ctx.workspace_uri}}})
    workspace_code_lenses = read_message(proc)["result"]
    helper_lens = next(lens for lens in workspace_code_lenses if lens["range"]["start"] == {"line": 1, "character": 10})
    send_message(proc, {"jsonrpc": "2.0", "id": 48, "method": "codeLens/resolve", "params": helper_lens})
    helper_lens = read_message(proc)["result"]
    expect(helper_lens["command"]["title"] == "2 refs", "expected workspace-aware codelens count for helper")
    send_message(proc, {"jsonrpc": "2.0", "id": 39, "method": "textDocument/references", "params": {"textDocument": {"uri": ctx.caller_uri}, "position": {"line": 1, "character": 2}, "context": {"includeDeclaration": True}}})
    workspace_fallback_references = read_message(proc)["result"]
    if isinstance(workspace_fallback_references, list):
        expect([item["uri"] for item in workspace_fallback_references] == [ctx.caller_uri, ctx.bridge_uri, ctx.unopened_uri], "expected workspace fallback references from caller, bridge, and unopened declaration files")
    send_message(proc, {"jsonrpc": "2.0", "id": 40, "method": "textDocument/prepareRename", "params": {"textDocument": {"uri": ctx.caller_uri}, "position": {"line": 1, "character": 2}}})
    expect(read_message(proc)["result"] == {"start": {"line": 1, "character": 1}, "end": {"line": 1, "character": 17}}, "unexpected workspace prepareRename range")
    send_message(proc, {"jsonrpc": "2.0", "id": 41, "method": "textDocument/rename", "params": {"textDocument": {"uri": ctx.caller_uri}, "position": {"line": 1, "character": 2}, "newName": "describe-final"}})
    workspace_rename_edit = read_message(proc)["result"]
    workspace_changes = workspace_rename_edit["changes"]
    expect(set(workspace_changes.keys()) == {ctx.caller_uri, ctx.bridge_uri, ctx.unopened_uri}, "expected workspace rename edits for caller, bridge, and unopened declaration files")
    send_message(proc, {"jsonrpc": "2.0", "id": 14, "method": "textDocument/codeAction", "params": {"textDocument": {"uri": ctx.main_uri}, "range": {"start": {"line": 2, "character": 11}, "end": {"line": 2, "character": 15}}, "context": {"diagnostics": []}}})
    code_actions = read_message(proc)["result"]
    expect(code_actions[0]["title"] == "Convert `ping` to explicit lambda binding", "unexpected shorthand rewrite code action title")
    send_message(proc, {"jsonrpc": "2.0", "id": 15, "method": "textDocument/codeAction", "params": {"textDocument": {"uri": ctx.workspace_uri}, "range": {"start": {"line": 1, "character": 11}, "end": {"line": 1, "character": 17}}, "context": {"diagnostics": []}}})
    lambda_code_actions = read_message(proc)["result"]
    expect(lambda_code_actions[0]["title"] == "Convert `helper` to shorthand function define", "unexpected lambda rewrite code action title")


def run_actions_and_overloads(proc, ctx: SmokeContext) -> None:
    send_message(proc, {"jsonrpc": "2.0", "method": "textDocument/didOpen", "params": {"textDocument": {"uri": ctx.actions_uri, "version": 1, "text": (ctx.root / "actions.omni").read_text(encoding="utf-8")}}})
    expect(read_message(proc).get("method") == "textDocument/publishDiagnostics", "expected diagnostics notification after actions open")
    send_message(proc, {"jsonrpc": "2.0", "id": 16, "method": "textDocument/codeAction", "params": {"textDocument": {"uri": ctx.actions_uri}, "range": {"start": {"line": 0, "character": 10}, "end": {"line": 0, "character": 17}}, "context": {"diagnostics": []}}})
    wrap_code_actions = read_message(proc)["result"]
    expect([item["title"] for item in wrap_code_actions] == ["Convert `wrap-me` to explicit lambda binding", "Wrap `wrap-me` body in explicit block"], "unexpected wrap-me code action titles")
    send_message(proc, {"jsonrpc": "2.0", "id": 17, "method": "textDocument/codeAction", "params": {"textDocument": {"uri": ctx.actions_uri}, "range": {"start": {"line": 4, "character": 8}, "end": {"line": 4, "character": 17}}, "context": {"diagnostics": []}}})
    unwrap_code_actions = read_message(proc)["result"]
    expect([item["title"] for item in unwrap_code_actions] == ["Convert `unwrap-me` to shorthand function define", "Inline `unwrap-me` block body"], "unexpected unwrap-me code action titles")
    send_message(proc, {"jsonrpc": "2.0", "id": 18, "method": "textDocument/prepareRename", "params": {"textDocument": {"uri": ctx.main_uri}, "position": {"line": 2, "character": 11}}})
    expect(read_message(proc)["result"] == {"start": {"line": 2, "character": 11}, "end": {"line": 2, "character": 15}}, "unexpected prepareRename range")
    send_message(proc, {"jsonrpc": "2.0", "id": 19, "method": "textDocument/hover", "params": {"textDocument": {"uri": ctx.main_uri}, "position": {"line": 2, "character": 11}}})
    hover = read_message(proc)["result"]
    expect("local function" in hover["contents"]["value"], "expected local function hover label")
    overloaded_text = '(define (describe (^Integer n)) "integer")\n(define (describe (^String s)) "string")\n(describe "x")\n'
    send_message(proc, {"jsonrpc": "2.0", "method": "textDocument/didChange", "params": {"textDocument": {"uri": ctx.main_uri, "version": 2}, "contentChanges": [{"text": overloaded_text}]}})
    read_message(proc)
    send_message(proc, {"jsonrpc": "2.0", "id": 20, "method": "textDocument/definition", "params": {"textDocument": {"uri": ctx.main_uri}, "position": {"line": 2, "character": 2}}})
    expect(len(read_message(proc)["result"]) == 2, "expected two definition targets for overloaded describe")
    send_message(proc, {"jsonrpc": "2.0", "id": 21, "method": "textDocument/signatureHelp", "params": {"textDocument": {"uri": ctx.main_uri}, "position": {"line": 2, "character": 13}}})
    overloaded_signature_help = read_message(proc)["result"]
    expect([item["label"] for item in overloaded_signature_help["signatures"]] == ["(describe (^Integer n))", "(describe (^String s))"], "expected one signature per overloaded local declaration")
    send_message(proc, {"jsonrpc": "2.0", "id": 22, "method": "textDocument/hover", "params": {"textDocument": {"uri": ctx.main_uri}, "position": {"line": 2, "character": 2}}})
    overloaded_hover = read_message(proc)["result"]
    expect("2 declarations" in overloaded_hover["contents"]["value"], "expected overloaded hover count")
    send_message(proc, {"jsonrpc": "2.0", "id": 23, "method": "textDocument/documentHighlight", "params": {"textDocument": {"uri": ctx.main_uri}, "position": {"line": 2, "character": 2}}})
    expect([item["kind"] for item in read_message(proc)["result"]] == [3, 3, 2], "expected overloaded highlight kinds to distinguish declarations from the call")
    send_message(proc, {"jsonrpc": "2.0", "id": 24, "method": "workspace/symbol", "params": {"query": "describe"}})
    describe_symbols = read_message(proc)["result"]
    expect([item["name"] for item in describe_symbols] == ["describe", "describe", "describe-extra", "describe-updated"], "unexpected workspace symbol names for describe query")
    send_message(proc, {"jsonrpc": "2.0", "id": 25, "method": "textDocument/references", "params": {"textDocument": {"uri": ctx.main_uri}, "position": {"line": 2, "character": 2}, "context": {"includeDeclaration": True}}})
    expect(len(read_message(proc)["result"]) == 3, "expected overloaded declarations + call references")
    send_message(proc, {"jsonrpc": "2.0", "id": 26, "method": "textDocument/rename", "params": {"textDocument": {"uri": ctx.main_uri}, "position": {"line": 2, "character": 2}, "newName": "describe-text"}})
    rename_edit = read_message(proc)["result"]
    expect(len(rename_edit["changes"][ctx.main_uri]) == 3, "expected rename to touch both declarations and the call")
    send_message(proc, {"jsonrpc": "2.0", "id": 27, "method": "textDocument/foldingRange", "params": {"textDocument": {"uri": ctx.folds_uri}}})
    folding_ranges = read_message(proc)["result"]
    expect(isinstance(folding_ranges, list), "expected folding range result list")
    send_message(proc, {"jsonrpc": "2.0", "id": 28, "method": "textDocument/selectionRange", "params": {"textDocument": {"uri": ctx.folds_uri}, "positions": [{"line": 5, "character": 8}]}})
    selection_ranges = read_message(proc)["result"]
    expect(isinstance(selection_ranges, list), "expected selection range result list")
    send_message(proc, {"jsonrpc": "2.0", "method": "textDocument/didOpen", "params": {"textDocument": {"uri": ctx.linked_uri, "version": 1, "text": (ctx.root / "linked.omni").read_text(encoding="utf-8")}}})
    expect(read_message(proc).get("method") == "textDocument/publishDiagnostics", "expected diagnostics notification after linked-edit open")
    send_message(proc, {"jsonrpc": "2.0", "id": 29, "method": "textDocument/linkedEditingRange", "params": {"textDocument": {"uri": ctx.linked_uri}, "position": {"line": 3, "character": 10}}})
    linked_ranges = read_message(proc)["result"]
    expect(isinstance(linked_ranges, dict), "expected linked editing result object")


def run_diagnostics_and_formatting(proc, ctx: SmokeContext) -> None:
    file_paths = [
        ctx.root / "format.omni",
        ctx.root / "continuation_format.omni",
        ctx.root / "branch_format.omni",
        ctx.root / "generic_format.omni",
        ctx.root / "nested_if_format.omni",
        ctx.root / "control_forms_format.omni",
        ctx.root / "higher_order_format.omni",
        ctx.root / "coroutine_format.omni",
        ctx.root / "clause_data_format.omni",
        ctx.root / "crlf_format.omni",
    ]
    for path in file_paths:
        if path.name == "crlf_format.omni":
            with path.open("r", encoding="utf-8", newline="") as handle:
                text = handle.read()
        else:
            text = path.read_text(encoding="utf-8")
        uri = path.resolve().as_uri()
        send_message(proc, {"jsonrpc": "2.0", "method": "textDocument/didOpen", "params": {"textDocument": {"uri": uri, "version": 1, "text": text}}})
        expect(read_message(proc).get("method") == "textDocument/publishDiagnostics", f"expected diagnostics notification after {path.name} open")
    send_message(proc, {"jsonrpc": "2.0", "method": "textDocument/didOpen", "params": {"textDocument": {"uri": ctx.root.joinpath('bad.omni').resolve().as_uri(), "version": 1, "text": "(define (broken x)\n"}}})
    bad_diagnostics = read_message(proc)
    expect(isinstance(bad_diagnostics["params"].get("diagnostics"), list) and len(bad_diagnostics["params"]["diagnostics"]) >= 1, "expected push diagnostics for malformed document")
    send_message(proc, {"jsonrpc": "2.0", "id": 33, "method": "textDocument/diagnostic", "params": {"textDocument": {"uri": ctx.root.joinpath('bad.omni').resolve().as_uri()}}})
    bad_pull = read_message(proc)["result"]
    expect(bad_pull.get("kind") == "full", "expected full pull diagnostic report")
    send_message(proc, {"jsonrpc": "2.0", "id": 34, "method": "workspace/diagnostic", "params": {"previousResultIds": []}})
    report = read_message(proc)["result"]
    workspace_items = {item["uri"]: item for item in report["items"] if isinstance(item, dict) and isinstance(item.get("uri"), str)}
    expect(ctx.root.joinpath("bad.omni").resolve().as_uri() in workspace_items, "expected bad document in workspace diagnostic report")
    send_message(proc, {"jsonrpc": "2.0", "id": 35, "method": "workspace/diagnostic", "params": {"previousResultIds": [{"uri": ctx.root.joinpath('bad.omni').resolve().as_uri(), "value": workspace_items[ctx.root.joinpath('bad.omni').resolve().as_uri()]["resultId"]}, {"uri": ctx.main_uri, "value": workspace_items[ctx.main_uri]["resultId"]}]}})
    unchanged = read_message(proc)["result"]
    unchanged_items = {item["uri"]: item for item in unchanged.get("items", []) if isinstance(item, dict) and isinstance(item.get("uri"), str)}
    expect(unchanged_items.get(ctx.root.joinpath('bad.omni').resolve().as_uri(), {}).get("kind") == "unchanged", "expected unchanged workspace diagnostic entry for malformed document with matching result id")
    expect(unchanged_items.get(ctx.main_uri, {}).get("kind") == "unchanged", "expected unchanged workspace diagnostic entry for clean document with matching result id")
    send_message(proc, {"jsonrpc": "2.0", "id": 29, "method": "textDocument/formatting", "params": {"textDocument": {"uri": ctx.format_uri}, "options": {"tabSize": 2, "insertSpaces": True}}})
    expect(read_message(proc)["result"][0]["newText"] == "(define (sum x y)\n  (block\n    (+ x y)\n    (let (z (+ x y))\n      z)))\n", "unexpected document formatting result")
    send_message(proc, {"jsonrpc": "2.0", "id": 30, "method": "textDocument/rangeFormatting", "params": {"textDocument": {"uri": ctx.format_uri}, "range": {"start": {"line": 1, "character": 0}, "end": {"line": 4, "character": 4}}, "options": {"tabSize": 2, "insertSpaces": True}}})
    expect(read_message(proc)["result"][0]["newText"] == "  (block\n    (+ x y)\n    (let (z (+ x y))\n      z)))\n", "unexpected range formatting result")
    send_message(proc, {"jsonrpc": "2.0", "id": 31, "method": "textDocument/onTypeFormatting", "params": {"textDocument": {"uri": ctx.format_uri}, "position": {"line": 4, "character": 4}, "ch": ")", "options": {"tabSize": 2, "insertSpaces": True}}})
    expect(read_message(proc)["result"][0]["newText"] == "      z)))\n", "unexpected on-type formatting result")
    for id_, uri, expected in [
        (31_1, ctx.continuation_format_uri, "(module sample\n  (export alpha beta gamma\n          delta epsilon)\n  (define (pair-sum pairs)\n    (let (left (car pairs)\n          right (cadr pairs))\n      (+ left right))))\n"),
        (31_2, ctx.branch_format_uri, "(if (= 1 1)\n    100\n    200)\n\n(let ^rec (fact (lambda (n)\n                  (if (= n 0)\n                      1\n                      (* n (fact (- n 1))))))\n  (fact 5))\n"),
        (31_3, ctx.generic_format_uri, "(define (pipeline raw)\n  (|> (tcp-read raw)\n      decode-request\n      execute\n      respond))\n"),
        (31_4, ctx.nested_if_format_uri, "(match method\n  (\"GET\"    (if ready\n                (let (item (repo/get-item ready))\n                  (if item (Ok item) (Err \"missing\")))\n                (if (= method \"/items\")\n                    (Ok (repo/list))\n                    (Err \"route not found\"))))\n  (_        (Err \"route not found\")))\n"),
        (31_5, ctx.control_forms_format_uri, "(define (guarded retry value)\n  (let (result (checkpoint\n    (unless retry\n      (raise err\n        (recover err)))))\n    (when value\n      (emit value))\n    result))\n"),
        (31_6, ctx.higher_order_format_uri, "(define (collect prices)\n  (foldl (lambda (acc price)\n           (let (bucket (map (lambda (x)\n                               (* x price))\n                             acc))\n             (cons bucket acc)))\n         nil\n         prices))\n"),
        (31_7, ctx.coroutine_format_uri, "(define (make-worker seed)\n  (Coroutine (lambda ()\n      (let loop (n seed)\n        (if (= n 0)\n            n\n            (block\n              (yield n)\n              (loop (- n 1)))))))\n"),
        (31_8, ctx.clause_data_format_uri, "(define (summarize result pair)\n  (let (payload (match result\n                  ((Ok value)\n                   (let (scaled (+ value 1))\n                     (Ok scaled)))\n                  ((Err msg) (Err msg))))\n    (let ([left right] pair)\n      {'payload payload\n       'window [(- right left)\n                (+ left right)]\n       'total (+ left right)})))\n"),
        (31_9, ctx.crlf_format_uri, "(module windows\r\n  (export alpha beta\r\n          gamma)\r\n"),
    ]:
        send_message(proc, {"jsonrpc": "2.0", "id": id_, "method": "textDocument/formatting", "params": {"textDocument": {"uri": uri}, "options": {"tabSize": 2, "insertSpaces": True}}})
        expect(read_message(proc)["result"][0]["newText"] == expected, f"unexpected extended formatting result for {uri}")


def run_smoke_scenarios(proc) -> int:
    ctx = _bootstrap_workspace()
    run_capabilities_and_workspace(proc, ctx)
    run_navigation_and_workspace_fallback(proc, ctx)
    run_actions_and_overloads(proc, ctx)
    run_diagnostics_and_formatting(proc, ctx)
    send_message(proc, {"jsonrpc": "2.0", "id": 32, "method": "shutdown", "params": {}})
    read_message(proc)
    send_message(proc, {"jsonrpc": "2.0", "method": "exit", "params": {}})
    proc.wait(timeout=2)
    return 0
