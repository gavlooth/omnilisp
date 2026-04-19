from omni_lsp_shared import *


class CoreMixin:
    def __init__(self) -> None:
        self.documents: dict[str, Document] = {}
        self.shutdown_requested = False
        self.workspace_document_cache: dict[Path, WorkspaceCacheEntry] = {}

    def serve(self) -> int:
        while True:
            message = self._read_message()
            if message is None:
                return 0
            self._dispatch(message)

    def _dispatch(self, message: dict[str, Any]) -> None:
        method = message.get("method")
        params = message.get("params", {})
        msg_id = message.get("id")

        if method == "initialize":
            result = {
                "capabilities": {
                    "textDocumentSync": 1,
                    "diagnosticProvider": {
                        "interFileDependencies": False,
                        "workspaceDiagnostics": True,
                    },
                    "completionProvider": {
                        "triggerCharacters": ["(", "'", ".", "/"],
                        "resolveProvider": True,
                    },
                    "hoverProvider": True,
                    "signatureHelpProvider": {"triggerCharacters": [" ", "("]},
                    "inlayHintProvider": True,
                    "foldingRangeProvider": True,
                    "selectionRangeProvider": True,
                    "linkedEditingRangeProvider": True,
                    "documentFormattingProvider": True,
                    "documentRangeFormattingProvider": True,
                    "documentOnTypeFormattingProvider": {
                        "firstTriggerCharacter": "\n",
                        "moreTriggerCharacter": [")"],
                    },
                    "semanticTokensProvider": {
                        "legend": {
                            "tokenTypes": SEMANTIC_TOKEN_TYPES,
                            "tokenModifiers": SEMANTIC_TOKEN_MODIFIERS,
                        },
                        "full": True,
                    },
                    "documentLinkProvider": {"resolveProvider": False},
                    "codeLensProvider": {"resolveProvider": True},
                    "callHierarchyProvider": True,
                    "documentSymbolProvider": True,
                    "monikerProvider": True,
                    "documentHighlightProvider": True,
                    "definitionProvider": True,
                    "declarationProvider": True,
                    "implementationProvider": True,
                    "typeDefinitionProvider": True,
                    "referencesProvider": True,
                    "renameProvider": {"prepareProvider": True},
                    "codeActionProvider": True,
                    "workspaceSymbolProvider": True,
                },
                "serverInfo": {
                    "name": "omni-lsp",
                    "version": "0.1.0",
                },
            }
            self._reply(msg_id, result)
            return

        if method == "initialized":
            return

        if method == "shutdown":
            self.shutdown_requested = True
            self._reply(msg_id, None)
            return

        if method == "exit":
            raise SystemExit(0 if self.shutdown_requested else 1)

        if method == "textDocument/didOpen":
            text_document = params["textDocument"]
            self.documents[text_document["uri"]] = Document(
                uri=text_document["uri"],
                text=text_document["text"],
                version=text_document.get("version"),
                path=self._uri_to_path(text_document["uri"]),
            )
            self._invalidate_workspace_cache()
            self._publish_diagnostics(text_document["uri"])
            return

        if method == "textDocument/didChange":
            text_document = params["textDocument"]
            changes = params.get("contentChanges", [])
            if not changes:
                return
            current = self.documents.get(text_document["uri"])
            if current is None:
                current = Document(
                    uri=text_document["uri"],
                    text="",
                    version=text_document.get("version"),
                    path=self._uri_to_path(text_document["uri"]),
                )
            current.text = changes[-1].get("text", current.text)
            current.version = text_document.get("version")
            self.documents[text_document["uri"]] = current
            self._invalidate_workspace_cache()
            self._publish_diagnostics(text_document["uri"])
            return

        if method == "textDocument/didClose":
            text_document = params["textDocument"]
            self.documents.pop(text_document["uri"], None)
            self._invalidate_workspace_cache()
            self._notify(
                "textDocument/publishDiagnostics",
                {"uri": text_document["uri"], "diagnostics": []},
            )
            return

        if method == "textDocument/completion":
            self._reply(msg_id, {"isIncomplete": False, "items": self._completion_items(params)})
            return

        if method == "completionItem/resolve":
            self._reply(msg_id, self._resolve_completion_item(params))
            return

        if method == "textDocument/hover":
            hover = self._hover(params)
            self._reply(msg_id, hover)
            return

        if method == "textDocument/documentSymbol":
            self._reply(msg_id, self._document_symbols(params))
            return

        if method == "textDocument/diagnostic":
            self._reply(msg_id, self._document_diagnostic_report(params))
            return

        if method == "workspace/diagnostic":
            self._reply(msg_id, self._workspace_diagnostic_report(params))
            return

        if method == "textDocument/moniker":
            self._reply(msg_id, self._monikers(params))
            return

        if method == "textDocument/documentHighlight":
            self._reply(msg_id, self._document_highlights(params))
            return

        if method == "textDocument/signatureHelp":
            self._reply(msg_id, self._signature_help(params))
            return

        if method == "textDocument/inlayHint":
            self._reply(msg_id, self._inlay_hints(params))
            return

        if method == "textDocument/foldingRange":
            self._reply(msg_id, self._folding_ranges(params))
            return

        if method == "textDocument/selectionRange":
            self._reply(msg_id, self._selection_ranges(params))
            return

        if method == "textDocument/linkedEditingRange":
            self._reply(msg_id, self._linked_editing_ranges(params))
            return

        if method == "textDocument/formatting":
            self._reply(msg_id, self._formatting(params))
            return

        if method == "textDocument/rangeFormatting":
            self._reply(msg_id, self._range_formatting(params))
            return

        if method == "textDocument/onTypeFormatting":
            self._reply(msg_id, self._on_type_formatting(params))
            return

        if method == "textDocument/semanticTokens/full":
            self._reply(msg_id, self._semantic_tokens(params))
            return

        if method == "textDocument/documentLink":
            self._reply(msg_id, self._document_links(params))
            return

        if method == "textDocument/codeLens":
            self._reply(msg_id, self._code_lenses(params))
            return

        if method == "codeLens/resolve":
            self._reply(msg_id, self._resolve_code_lens(params))
            return

        if method == "textDocument/definition":
            self._reply(msg_id, self._definition(params))
            return

        if method == "textDocument/declaration":
            self._reply(msg_id, self._declaration(params))
            return

        if method == "textDocument/implementation":
            self._reply(msg_id, self._implementation(params))
            return

        if method == "textDocument/typeDefinition":
            self._reply(msg_id, self._type_definition(params))
            return

        if method == "textDocument/references":
            self._reply(msg_id, self._references(params))
            return

        if method == "textDocument/prepareRename":
            self._reply(msg_id, self._prepare_rename(params))
            return

        if method == "textDocument/rename":
            self._reply(msg_id, self._rename(params))
            return

        if method == "textDocument/codeAction":
            self._reply(msg_id, self._code_actions(params))
            return

        if method == "textDocument/prepareCallHierarchy":
            self._reply(msg_id, self._prepare_call_hierarchy(params))
            return

        if method == "callHierarchy/incomingCalls":
            self._reply(msg_id, self._incoming_calls(params))
            return

        if method == "callHierarchy/outgoingCalls":
            self._reply(msg_id, self._outgoing_calls(params))
            return

        if method == "workspace/symbol":
            self._reply(msg_id, self._workspace_symbols(params))
            return

        if msg_id is not None:
            self._reply(msg_id, None)
