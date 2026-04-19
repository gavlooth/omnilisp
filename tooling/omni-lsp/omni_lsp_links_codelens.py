from omni_lsp_shared import *


class LinksCodeLensMixin:
    def _document_links(self, params: dict[str, Any]) -> list[dict[str, Any]]:
        text_document = params["textDocument"]
        document = self.documents.get(text_document["uri"])
        if document is None:
            return []

        links: list[dict[str, Any]] = []
        for form_start, form_end in self._top_level_forms(document.text):
            link = self._document_link_from_form(document, form_start, form_end)
            if link is not None:
                links.append(link)
        return links

    def _document_link_from_form(
        self,
        document: Document,
        form_start: int,
        form_end: int,
    ) -> dict[str, Any] | None:
        items = self._container_items(document.text, form_start, form_end)
        if len(items) < 2 or items[0]["kind"] != "atom":
            return None

        head = items[0]["text"]
        if head not in {"import", "export-from"}:
            return None

        target_item = items[1]
        target_uri = self._document_link_target_uri(document, target_item)
        if target_uri is None:
            return None

        label = "import target" if head == "import" else "export-from target"
        return {
            "range": self._offset_range(document.text, target_item["start"], target_item["end"]),
            "target": target_uri,
            "tooltip": f"Omni {label}",
        }

    def _document_link_target_uri(
        self,
        document: Document,
        item: dict[str, Any],
    ) -> str | None:
        if item["kind"] == "string":
            return self._string_import_target_uri(document, item["text"])
        if item["kind"] == "atom":
            return self._module_target_uri(item["text"], document.uri)
        return None

    def _string_import_target_uri(self, document: Document, raw_text: str) -> str | None:
        if document.path is None:
            return None
        if len(raw_text) < 2 or not raw_text.startswith('"') or not raw_text.endswith('"'):
            return None
        relative = raw_text[1:-1]
        if not relative:
            return None
        target = (document.path.parent / relative).resolve()
        if not target.exists():
            return None
        return target.as_uri()

    def _module_target_uri(self, module_name: str, current_uri: str) -> str | None:
        candidates = [
            record
            for record in self._all_declaration_records()
            if record.detail == "module" and record.name == module_name
        ]
        if not candidates:
            return None
        candidates.sort(key=lambda record: (0 if record.uri == current_uri else 1, record.uri))
        return candidates[0].uri

    def _document_link_at_position(
        self,
        document: Document,
        line: int,
        character: int,
    ) -> dict[str, Any] | None:
        for form_start, form_end in self._top_level_forms(document.text):
            link = self._document_link_from_form(document, form_start, form_end)
            if link is None:
                continue
            if self._range_contains_position(link["range"], line, character):
                return link
        return None

    def _range_contains_position(
        self,
        item_range: dict[str, Any],
        line: int,
        character: int,
    ) -> bool:
        start = item_range.get("start") or {}
        end = item_range.get("end") or {}
        start_line = int(start.get("line", 0))
        start_character = int(start.get("character", 0))
        end_line = int(end.get("line", 0))
        end_character = int(end.get("character", 0))
        if line < start_line or line > end_line:
            return False
        if line == start_line and character < start_character:
            return False
        if line == end_line and character >= end_character:
            return False
        return True

    def _navigation_target_for_uri(self, target_uri: str) -> dict[str, Any] | None:
        target_document = self.documents.get(target_uri)
        if target_document is not None:
            return self._document_entry_location(target_document.uri, target_document.text)

        path = self._uri_to_path(target_uri)
        if path is None:
            return None
        text = self._workspace_text_for_path(path)
        if text is None:
            return {
                "uri": target_uri,
                "range": {
                    "start": {"line": 0, "character": 0},
                    "end": {"line": 0, "character": 0},
                },
            }
        return self._document_entry_location(target_uri, text)

    def _document_entry_location(self, uri: str, text: str) -> dict[str, Any]:
        declarations = self._scan_top_level_declarations(text)
        if declarations:
            first = declarations[0]
            return {
                "uri": uri,
                "range": self._offset_range(text, first.selection_start, first.selection_end),
            }
        return {
            "uri": uri,
            "range": {
                "start": {"line": 0, "character": 0},
                "end": {"line": 0, "character": 0},
            },
        }

    def _code_lenses(self, params: dict[str, Any]) -> list[dict[str, Any]]:
        text_document = params["textDocument"]
        document = self.documents.get(text_document["uri"])
        if document is None:
            return []

        code_lenses: list[dict[str, Any]] = []
        for declaration in self._flatten_declarations(self._scan_top_level_declarations(document.text)):
            if declaration.detail == "module":
                continue
            code_lens = self._unresolved_code_lens_for_declaration(document, declaration)
            if code_lens is not None:
                code_lenses.append(code_lens)
        return code_lenses

    def _unresolved_code_lens_for_declaration(
        self,
        document: Document,
        declaration: Declaration,
    ) -> dict[str, Any]:
        return {
            "range": self._offset_range(
                document.text,
                declaration.selection_start,
                declaration.selection_end,
            ),
            "data": {
                "uri": document.uri,
                "name": declaration.name,
                "selectionStart": self._offset_to_position(document.text, declaration.selection_start),
            },
        }

    def _resolve_code_lens(self, code_lens: dict[str, Any]) -> dict[str, Any]:
        if not isinstance(code_lens, dict):
            return code_lens
        data = code_lens.get("data")
        if not isinstance(data, dict):
            return code_lens

        uri = data.get("uri")
        name = data.get("name")
        selection_start = data.get("selectionStart") or {}
        if not isinstance(uri, str) or not isinstance(name, str):
            return code_lens

        document = self.documents.get(uri)
        if document is None:
            return code_lens

        declaration = self._declaration_at_offset(
            document.text,
            self._position_to_offset(
                document.text,
                int(selection_start.get("line", 0)),
                int(selection_start.get("character", 0)),
            ),
        )
        if declaration is None or declaration.name != name:
            return code_lens

        resolved = dict(code_lens)
        resolved["command"] = self._resolved_code_lens_command(document, declaration)
        return resolved

    def _resolved_code_lens_command(
        self,
        document: Document,
        declaration: Declaration,
    ) -> dict[str, Any]:
        references, local_only = self._declaration_reference_locations(document, declaration)
        reference_count = len(references)
        if local_only:
            title = "1 local ref" if reference_count == 1 else f"{reference_count} local refs"
        else:
            title = "1 ref" if reference_count == 1 else f"{reference_count} refs"
        return {
            "title": title,
            "command": "omni.showReferences",
            "arguments": [
                document.uri,
                self._offset_to_position(document.text, declaration.selection_start),
                references,
            ],
        }

    def _declaration_reference_locations(
        self,
        document: Document,
        declaration: Declaration,
    ) -> tuple[list[dict[str, Any]], bool]:
        if len(self._declarations_for_symbol(document.text, declaration.name)) > 1:
            return self._document_references(document, declaration.name, False), True
        return self._workspace_references(document, declaration.name, False), False

    def _signature_infos_for_symbol(
        self,
        document: Document,
        symbol: str,
    ) -> list[dict[str, Any]]:
        declarations = self._declarations_for_symbol(document.text, symbol)
        if declarations:
            return self._signature_infos_for_declarations(document.text, declarations)

        workspace_records = self._workspace_definition_records(symbol, document.uri)
        workspace_signatures = self._signature_infos_for_workspace_records(workspace_records)
        if workspace_signatures:
            return workspace_signatures

        static_signatures = SIGNATURE_ITEMS.get(symbol)
        if static_signatures is None:
            return []
        return [
            {
                "label": item["label"],
                "documentation": {"kind": "markdown", "value": item["documentation"]},
                "parameters": [{"label": parameter} for parameter in item["parameters"]],
            }
            for item in static_signatures
        ]

    def _signature_infos_for_declarations(
        self,
        text: str,
        declarations: list[Declaration],
    ) -> list[dict[str, Any]]:
        signatures: list[dict[str, Any]] = []
        for declaration in declarations:
            signature = self._declaration_signature_info(text, declaration)
            if signature is None:
                continue
            signatures.append(signature)
        return signatures

    def _signature_infos_for_workspace_records(
        self,
        records: list[WorkspaceSymbolRecord],
    ) -> list[dict[str, Any]]:
        signatures: list[dict[str, Any]] = []
        for record in records:
            if not record.parameter_labels:
                continue
            signatures.append(
                {
                    "label": self._signature_label(record.name, record.parameter_labels),
                    "documentation": {"kind": "markdown", "value": f"```omni\n{record.snippet}\n```"},
                    "parameters": [{"label": parameter} for parameter in record.parameter_labels],
                }
            )
        return signatures

    def _declaration_signature_info(
        self,
        text: str,
        declaration: Declaration,
    ) -> dict[str, Any] | None:
        items = self._container_items(text, declaration.start, declaration.end)
        parameter_labels = self._declaration_parameter_labels(text, items)
        if not parameter_labels:
            return None
        return {
            "label": self._signature_label(declaration.name, parameter_labels),
            "documentation": {"kind": "markdown", "value": f"```omni\n{self._declaration_snippet(text, declaration)}\n```"},
            "parameters": [{"label": parameter} for parameter in parameter_labels],
        }

    def _declaration_parameter_labels(
        self,
        text: str,
        items: list[dict[str, Any]],
    ) -> list[str]:
        if len(items) < 2 or items[0].get("text") != "define":
            return []

        target = items[1]
        if target["kind"] == "list":
            target_items = self._container_items(text, target["start"], target["end"])
            return [text[item["start"] : item["end"]] for item in target_items[1:]]

        if target["kind"] == "atom" and len(items) > 2 and items[2]["kind"] == "list":
            lambda_items = self._container_items(text, items[2]["start"], items[2]["end"])
            if len(lambda_items) < 2:
                return []
            if lambda_items[0]["kind"] != "atom" or lambda_items[0]["text"] not in {"lambda", "λ"}:
                return []
            params_item = lambda_items[1]
            if params_item["kind"] != "list":
                return []
            params_items = self._container_items(text, params_item["start"], params_item["end"])
            return [text[item["start"] : item["end"]] for item in params_items]

        return []

    def _declaration_parameter_items(
        self,
        text: str,
        declaration: Declaration,
    ) -> list[dict[str, Any]]:
        items = self._container_items(text, declaration.start, declaration.end)
        if len(items) < 2 or items[0].get("text") != "define":
            return []

        target = items[1]
        if target["kind"] == "list":
            target_items = self._container_items(text, target["start"], target["end"])
            selections: list[dict[str, Any]] = []
            for item in target_items[1:]:
                selection = self._parameter_selection_item(text, item)
                if selection is not None:
                    selections.append(selection)
            return selections

        if target["kind"] != "atom" or len(items) <= 2 or items[2]["kind"] != "list":
            return []
        lambda_items = self._container_items(text, items[2]["start"], items[2]["end"])
        if len(lambda_items) < 2:
            return []
        if lambda_items[0]["kind"] != "atom" or lambda_items[0]["text"] not in {"lambda", "λ"}:
            return []
        params_item = lambda_items[1]
        if params_item["kind"] != "list":
            return []

        selections: list[dict[str, Any]] = []
        for item in self._container_items(text, params_item["start"], params_item["end"]):
            selection = self._parameter_selection_item(text, item)
            if selection is not None:
                selections.append(selection)
        return selections

    def _signature_label(self, name: str, parameter_labels: list[str]) -> str:
        joined = " ".join(parameter_labels)
        if joined:
            return f"({name} {joined})"
        return f"({name})"
