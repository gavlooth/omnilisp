from omni_lsp_shared import *


class ActionsDiagnosticsMixin:
    def _code_actions(self, params: dict[str, Any]) -> list[dict[str, Any]]:
        text_document = params["textDocument"]
        range_params = params.get("range") or {}
        start = range_params.get("start") or {}
        document = self.documents.get(text_document["uri"])
        if document is None:
            return []

        offset = self._position_to_offset(
            document.text,
            int(start.get("line", 0)),
            int(start.get("character", 0)),
        )
        declaration = self._declaration_at_offset(document.text, offset)
        if declaration is None:
            return []

        actions: list[dict[str, Any]] = []
        rewrite = self._rewrite_declaration_action(document, declaration)
        if rewrite is not None:
            actions.append(rewrite)
        body_rewrite = self._body_rewrite_action(document, declaration)
        if body_rewrite is not None:
            actions.append(body_rewrite)
        return actions

    def _rewrite_declaration_action(
        self,
        document: Document,
        declaration: Declaration,
    ) -> dict[str, Any] | None:
        text = document.text
        items = self._container_items(text, declaration.start, declaration.end)
        if len(items) < 3 or items[0].get("text") != "define":
            return None

        target = items[1]
        replacement: str | None = None
        title: str | None = None

        if target["kind"] == "list":
            replacement = self._rewrite_shorthand_define_to_lambda(text, items)
            if replacement is not None:
                title = f"Convert `{declaration.name}` to explicit lambda binding"
        elif target["kind"] == "atom":
            replacement = self._rewrite_lambda_binding_to_shorthand(text, items)
            if replacement is not None:
                title = f"Convert `{declaration.name}` to shorthand function define"

        if replacement is None or title is None:
            return None

        return {
            "title": title,
            "kind": "refactor.rewrite",
            "edit": {
                "changes": {
                    document.uri: [
                        {
                            "range": self._offset_range(text, declaration.start, declaration.end),
                            "newText": replacement,
                        }
                    ]
                }
            },
        }

    def _body_rewrite_action(
        self,
        document: Document,
        declaration: Declaration,
    ) -> dict[str, Any] | None:
        text = document.text
        items = self._container_items(text, declaration.start, declaration.end)
        if len(items) < 3 or items[0].get("text") != "define":
            return None

        target = items[1]
        if target["kind"] == "list":
            return self._shorthand_body_rewrite_action(document, declaration, text, items)
        if target["kind"] == "atom" and items[2]["kind"] == "list":
            return self._lambda_body_rewrite_action(document, declaration, text, items)
        return None

    def _shorthand_body_rewrite_action(
        self,
        document: Document,
        declaration: Declaration,
        text: str,
        items: list[dict[str, Any]],
    ) -> dict[str, Any] | None:
        signature_text = text[items[1]["start"] : items[1]["end"]]
        body_items = items[2:]
        rewrite = self._body_rewrite_text(body_items, text)
        if rewrite is None:
            return None

        title, body_text = rewrite
        replacement = f"(define {signature_text} {body_text})"
        return self._rewrite_action(document.uri, declaration, text, replacement, title.format(name=declaration.name))

    def _lambda_body_rewrite_action(
        self,
        document: Document,
        declaration: Declaration,
        text: str,
        items: list[dict[str, Any]],
    ) -> dict[str, Any] | None:
        lambda_items = self._container_items(text, items[2]["start"], items[2]["end"])
        if len(lambda_items) < 3:
            return None
        if lambda_items[0]["kind"] != "atom" or lambda_items[0]["text"] not in {"lambda", "λ"}:
            return None
        params_item = lambda_items[1]
        if params_item["kind"] != "list":
            return None

        body_items = lambda_items[2:]
        rewrite = self._body_rewrite_text(body_items, text)
        if rewrite is None:
            return None

        title, body_text = rewrite
        params_text = text[params_item["start"] : params_item["end"]]
        replacement = f"(define {items[1]['text']} (lambda {params_text} {body_text}))"
        return self._rewrite_action(document.uri, declaration, text, replacement, title.format(name=declaration.name))

    def _body_rewrite_text(
        self,
        body_items: list[dict[str, Any]],
        text: str,
    ) -> tuple[str, str] | None:
        if len(body_items) > 1:
            joined = self._items_text(text, body_items)
            return "Wrap `{name}` body in explicit block", f"(block {joined})"

        if len(body_items) != 1 or body_items[0]["kind"] != "list":
            return None

        block_items = self._container_items(text, body_items[0]["start"], body_items[0]["end"])
        if len(block_items) < 2:
            return None
        if block_items[0]["kind"] != "atom" or block_items[0]["text"] != "block":
            return None
        return "Inline `{name}` block body", self._items_text(text, block_items[1:])

    def _rewrite_action(
        self,
        uri: str,
        declaration: Declaration,
        text: str,
        replacement: str,
        title: str,
    ) -> dict[str, Any]:
        return {
            "title": title,
            "kind": "refactor.rewrite",
            "edit": {
                "changes": {
                    uri: [
                        {
                            "range": self._offset_range(text, declaration.start, declaration.end),
                            "newText": replacement,
                        }
                    ]
                }
            },
        }

    def _rewrite_shorthand_define_to_lambda(
        self,
        text: str,
        items: list[dict[str, Any]],
    ) -> str | None:
        target_items = self._container_items(text, items[1]["start"], items[1]["end"])
        if not target_items or target_items[0]["kind"] != "atom":
            return None

        name = target_items[0]["text"]
        params = self._items_text(text, target_items[1:])
        body_items = items[2:]
        if not body_items:
            return None
        body_text = self._body_text(text, body_items)
        return f"(define {name} (lambda ({params}) {body_text}))"

    def _rewrite_lambda_binding_to_shorthand(
        self,
        text: str,
        items: list[dict[str, Any]],
    ) -> str | None:
        if items[1]["kind"] != "atom" or items[2]["kind"] != "list":
            return None

        lambda_items = self._container_items(text, items[2]["start"], items[2]["end"])
        if len(lambda_items) < 3:
            return None
        if lambda_items[0]["kind"] != "atom" or lambda_items[0]["text"] not in {"lambda", "λ"}:
            return None

        params_item = lambda_items[1]
        if params_item["kind"] != "list":
            return None

        params_items = self._container_items(text, params_item["start"], params_item["end"])
        params = self._items_text(text, params_items)
        body_text = self._body_text(text, lambda_items[2:])
        name = items[1]["text"]
        param_suffix = f" {params}" if params else ""
        return f"(define ({name}{param_suffix}) {body_text})"

    def _items_text(self, text: str, items: list[dict[str, Any]]) -> str:
        return " ".join(text[item["start"] : item["end"]] for item in items)

    def _body_text(self, text: str, items: list[dict[str, Any]]) -> str:
        if len(items) == 1:
            item = items[0]
            return text[item["start"] : item["end"]]
        joined = self._items_text(text, items)
        return f"(block {joined})"

    def _publish_diagnostics(self, uri: str) -> None:
        document = self.documents.get(uri)
        if document is None:
            return
        diagnostics = self._compute_diagnostics(document)
        self._notify(
            "textDocument/publishDiagnostics",
            {"uri": uri, "diagnostics": diagnostics},
        )

    def _compute_diagnostics(self, document: Document) -> list[dict[str, Any]]:
        text = document.text
        with tempfile.TemporaryDirectory(prefix="omni-lsp-") as tempdir:
            input_path = Path(tempdir) / "document.omni"
            input_path.write_text(text, encoding="utf-8")

            proc = subprocess.run(
                [OMNI_BIN, "--check", "--json", str(input_path)],
                capture_output=True,
                text=True,
                check=False,
                env=self._command_env(),
            )

        payload_text = proc.stdout.strip() or proc.stderr.strip()
        try:
            payload = json.loads(payload_text)
        except json.JSONDecodeError:
            message = payload_text or "Unknown Omni check error"
            return [self._fallback_diagnostic(message)]

        diagnostics = payload.get("diagnostics", [])
        if not isinstance(diagnostics, list):
            return [self._fallback_diagnostic("Malformed Omni check response")]

        converted: list[dict[str, Any]] = []
        for item in diagnostics:
            if not isinstance(item, dict):
                continue
            message = str(item.get("message", "Unknown Omni check error"))
            item_range = item.get("range") or {}
            start = (item_range.get("start") or {}) if isinstance(item_range, dict) else {}
            end = (item_range.get("end") or {}) if isinstance(item_range, dict) else {}
            converted.append(
                {
                    "range": {
                        "start": {
                            "line": int(start.get("line", 0)),
                            "character": int(start.get("character", 0)),
                        },
                        "end": {
                            "line": int(end.get("line", start.get("line", 0))),
                            "character": int(end.get("character", int(start.get("character", 0)) + 1)),
                        },
                    },
                    "severity": self._severity_to_lsp(item.get("severity")),
                    "source": "omni",
                    "code": item.get("code"),
                    "message": message,
                }
            )
        return converted

    def _fallback_diagnostic(self, message: str) -> dict[str, Any]:
        return {
            "range": {
                "start": {"line": 0, "character": 0},
                "end": {"line": 0, "character": 1},
            },
            "severity": 1,
            "source": "omni",
            "message": message,
        }

    def _severity_to_lsp(self, severity: Any) -> int:
        if severity == "warning":
            return 2
        if severity == "information":
            return 3
        if severity == "hint":
            return 4
        return 1
