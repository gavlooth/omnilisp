from omni_lsp_shared import *


class HoverCompletionMixin:
    def _hover(self, params: dict[str, Any]) -> dict[str, Any] | None:
        text_document = params["textDocument"]
        position = params["position"]
        document = self.documents.get(text_document["uri"])
        if document is None:
            return None

        symbol = self._symbol_at(document.text, position["line"], position["character"])
        if not symbol:
            return None

        declarations = self._declarations_for_symbol(document.text, symbol)
        if declarations:
            return self._local_hover(document.text, symbol, declarations)

        workspace_records = self._workspace_definition_records(symbol, document.uri)
        if workspace_records:
            return self._workspace_hover(symbol, workspace_records)

        doc = SPECIAL_FORMS.get(symbol) or BUILTIN_VALUES.get(symbol)
        if doc is None:
            return None

        kind = "special form" if symbol in SPECIAL_FORMS else "builtin"
        return {
            "contents": {
                "kind": "markdown",
                "value": f"```omni\n{symbol}\n```\n\n{kind}: {doc}",
            }
        }

    def _local_hover(
        self,
        text: str,
        symbol: str,
        declarations: list[Declaration],
    ) -> dict[str, Any]:
        first = declarations[0]
        label = DECLARATION_DETAIL_LABELS.get(first.detail, first.detail)
        if len(declarations) == 1:
            value = (
                f"local {label}\n\n"
                f"```omni\n{self._declaration_snippet(text, first)}\n```"
            )
        else:
            snippets = "\n\n".join(
                f"overload {index + 1}\n\n```omni\n{self._declaration_snippet(text, declaration)}\n```"
                for index, declaration in enumerate(declarations)
            )
            value = (
                f"local {label}: `{symbol}` ({len(declarations)} declarations)\n\n"
                f"{snippets}"
            )
        return {"contents": {"kind": "markdown", "value": value}}

    def _workspace_hover(
        self,
        symbol: str,
        records: list[WorkspaceSymbolRecord],
    ) -> dict[str, Any]:
        first = records[0]
        label = DECLARATION_DETAIL_LABELS.get(first.detail, first.detail)
        if len(records) == 1:
            value = (
                f"workspace {label}\n\n"
                f"```omni\n{first.snippet}\n```"
            )
        else:
            snippets = "\n\n".join(
                f"match {index + 1}\n\n```omni\n{record.snippet}\n```"
                for index, record in enumerate(records)
            )
            value = (
                f"workspace {label}: `{symbol}` ({len(records)} declarations)\n\n"
                f"{snippets}"
            )
        return {"contents": {"kind": "markdown", "value": value}}

    def _completion_items(self, params: dict[str, Any]) -> list[dict[str, Any]]:
        document_uri = (params.get("textDocument") or {}).get("uri")
        document = self.documents.get(document_uri)

        items: list[dict[str, Any]] = []
        seen_labels: set[str] = set()
        if document is not None:
            for declaration in self._flatten_declarations(self._scan_top_level_declarations(document.text)):
                if declaration.name in seen_labels:
                    continue
                seen_labels.add(declaration.name)
                items.append(
                    {
                        "label": declaration.name,
                        "kind": COMPLETION_KIND_BY_SYMBOL_KIND.get(declaration.kind, 6),
                        "detail": declaration.detail,
                        "insertText": declaration.name,
                        "sortText": f"0-{declaration.name}",
                        "data": self._local_completion_data(document, declaration),
                    }
                )

        items.extend(self._workspace_completion_items(document_uri, seen_labels))

        for item in COMPLETION_ITEMS:
            label = item["label"]
            if label in seen_labels:
                continue
            items.append(item)
        return items

    def _workspace_completion_items(
        self,
        current_uri: str | None,
        seen_labels: set[str],
    ) -> list[dict[str, Any]]:
        items: list[dict[str, Any]] = []

        for document in self.documents.values():
            if document.uri == current_uri:
                continue
            for record in self._workspace_symbol_records_from_document(document):
                if record.name in seen_labels:
                    continue
                seen_labels.add(record.name)
                items.append(self._workspace_completion_item(record))
                if len(items) >= WORKSPACE_COMPLETION_RESULT_LIMIT:
                    return items[:WORKSPACE_COMPLETION_RESULT_LIMIT]

        open_paths = {
            document.path.resolve()
            for document in self.documents.values()
            if document.path is not None and document.path.exists()
        }
        for root in self._workspace_roots():
            for path, records in self._cached_workspace_symbol_records(root).items():
                if path in open_paths:
                    continue
                for record in records:
                    if record.name in seen_labels:
                        continue
                    seen_labels.add(record.name)
                    items.append(self._workspace_completion_item(record))
                    if len(items) >= WORKSPACE_COMPLETION_RESULT_LIMIT:
                        return items[:WORKSPACE_COMPLETION_RESULT_LIMIT]
        return items

    def _workspace_completion_item(self, record: WorkspaceSymbolRecord) -> dict[str, Any]:
        detail = DECLARATION_DETAIL_LABELS.get(record.detail, record.detail)
        if record.container_name:
            detail = f"{detail} in {record.container_name}"
        else:
            detail = f"{detail} (workspace)"
        return {
            "label": record.name,
            "kind": COMPLETION_KIND_BY_SYMBOL_KIND.get(record.kind, 6),
            "detail": detail,
            "insertText": record.name,
            "sortText": f"1-{record.name}",
            "data": self._workspace_completion_data(record),
        }

    def _resolve_completion_item(self, item: dict[str, Any]) -> dict[str, Any]:
        if not isinstance(item, dict):
            return item

        data = item.get("data")
        if not isinstance(data, dict):
            return item

        source = data.get("source")
        if source not in {"local", "workspace"}:
            return item

        label = str(data.get("label", item.get("label", "")))
        detail = str(data.get("detail", item.get("detail", "")))
        snippet = str(data.get("snippet", "")).strip()
        if not snippet:
            return item

        kind = "local" if source == "local" else "workspace"
        resolved = dict(item)
        resolved["detail"] = detail or str(item.get("detail", ""))
        resolved["documentation"] = {
            "kind": "markdown",
            "value": f"{kind} {detail or 'declaration'}\n\n```omni\n{snippet}\n```",
        }
        return resolved

    def _local_completion_data(
        self,
        document: Document,
        declaration: Declaration,
    ) -> dict[str, Any]:
        detail = DECLARATION_DETAIL_LABELS.get(declaration.detail, declaration.detail)
        return {
            "source": "local",
            "uri": document.uri,
            "label": declaration.name,
            "detail": detail,
            "snippet": self._declaration_snippet(document.text, declaration),
        }

    def _workspace_completion_data(self, record: WorkspaceSymbolRecord) -> dict[str, Any]:
        detail = DECLARATION_DETAIL_LABELS.get(record.detail, record.detail)
        if record.container_name:
            detail = f"{detail} in {record.container_name}"
        return {
            "source": "workspace",
            "uri": record.uri,
            "label": record.name,
            "detail": detail,
            "snippet": record.snippet,
        }

    def _signature_help(self, params: dict[str, Any]) -> dict[str, Any] | None:
        text_document = params["textDocument"]
        position = params["position"]
        document = self.documents.get(text_document["uri"])
        if document is None:
            return None

        context = self._call_context_at(
            document.text,
            position["line"],
            position["character"],
        )
        if context is None:
            return None

        symbol, active_parameter = context
        signatures = self._signature_infos_for_symbol(document, symbol)
        if not signatures:
            return None

        return {
            "signatures": signatures,
            "activeSignature": 0,
            "activeParameter": max(0, active_parameter),
        }

    def _inlay_hints(self, params: dict[str, Any]) -> list[dict[str, Any]]:
        text_document = params["textDocument"]
        range_params = params.get("range") or {}
        start = range_params.get("start") or {}
        end = range_params.get("end") or {}
        document = self.documents.get(text_document["uri"])
        if document is None:
            return []

        start_offset = self._position_to_offset(
            document.text,
            int(start.get("line", 0)),
            int(start.get("character", 0)),
        )
        end_offset = self._position_to_offset(
            document.text,
            int(end.get("line", document.text.count("\n"))),
            int(end.get("character", len(document.text))),
        )

        hints: list[dict[str, Any]] = []
        for form_start, form_end in self._top_level_forms(document.text):
            self._collect_inlay_hints_in_container(
                document,
                form_start,
                form_end,
                start_offset,
                end_offset,
                hints,
            )
        return hints
