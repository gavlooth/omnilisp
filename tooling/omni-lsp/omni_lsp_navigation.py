from omni_lsp_shared import *


class NavigationMixin:
    def _definition(self, params: dict[str, Any]) -> list[dict[str, Any]] | None:
        text_document = params["textDocument"]
        position = params["position"]
        document = self.documents.get(text_document["uri"])
        if document is None:
            return None

        link = self._document_link_at_position(
            document,
            int(position["line"]),
            int(position["character"]),
        )
        if link is not None and isinstance(link.get("target"), str):
            target = self._navigation_target_for_uri(link["target"])
            if target is not None:
                return [target]

        symbol = self._symbol_at(document.text, position["line"], position["character"])
        if not symbol:
            return None

        matches: list[dict[str, Any]] = []
        for declaration in self._declarations_for_symbol(document.text, symbol):
            if declaration.name != symbol:
                continue
            matches.append(
                {
                    "uri": document.uri,
                    "range": self._offset_range(
                        document.text,
                        declaration.selection_start,
                        declaration.selection_end,
                    ),
                }
            )

        if matches:
            return matches

        for record in self._workspace_definition_records(symbol, document.uri):
            matches.append(
                {
                    "uri": record.uri,
                    "range": record.range,
                }
            )

        return matches or None

    def _declaration(self, params: dict[str, Any]) -> list[dict[str, Any]] | None:
        return self._definition(params)

    def _implementation(self, params: dict[str, Any]) -> list[dict[str, Any]] | None:
        return self._definition(params)

    def _type_definition(self, params: dict[str, Any]) -> list[dict[str, Any]] | None:
        text_document = params["textDocument"]
        position = params["position"]
        document = self.documents.get(text_document["uri"])
        if document is None:
            return None

        symbol = self._symbol_at(document.text, position["line"], position["character"])
        if not symbol:
            return None

        matches: list[dict[str, Any]] = []
        for declaration in self._declarations_for_symbol(document.text, symbol):
            if declaration.name != symbol or declaration.detail not in TYPE_LIKE_DETAILS:
                continue
            matches.append(
                {
                    "uri": document.uri,
                    "range": self._offset_range(
                        document.text,
                        declaration.selection_start,
                        declaration.selection_end,
                    ),
                }
            )

        if matches:
            return matches

        for record in self._workspace_definition_records(symbol, document.uri):
            if record.detail not in TYPE_LIKE_DETAILS:
                continue
            matches.append(
                {
                    "uri": record.uri,
                    "range": record.range,
                }
            )

        return matches or None

    def _incoming_calls(self, params: dict[str, Any]) -> list[dict[str, Any]] | None:
        item = params.get("item")
        if not isinstance(item, dict):
            return None

        target = self._call_hierarchy_record_for_item(item)
        if target is None:
            return None

        incoming: list[dict[str, Any]] = []
        for caller in self._all_declaration_records():
            if self._declaration_record_key(caller) == self._declaration_record_key(target):
                continue
            from_ranges = [
                call_site.range
                for call_site in caller.call_sites
                if call_site.symbol.lower() == target.name_lower
            ]
            if not from_ranges:
                continue
            incoming.append(
                {
                    "from": self._call_hierarchy_item(caller),
                    "fromRanges": from_ranges,
                }
            )
        return incoming or None

    def _outgoing_calls(self, params: dict[str, Any]) -> list[dict[str, Any]] | None:
        item = params.get("item")
        if not isinstance(item, dict):
            return None

        source = self._call_hierarchy_record_for_item(item)
        if source is None:
            return None

        outgoing_by_target: dict[tuple[str, int, int, int, int], dict[str, Any]] = {}
        for call_site in source.call_sites:
            for target in self._call_hierarchy_symbol_records(call_site.symbol, source.uri):
                if not self._call_hierarchy_targetable(target):
                    continue
                key = self._declaration_record_key(target)
                if key == self._declaration_record_key(source):
                    continue
                entry = outgoing_by_target.get(key)
                if entry is None:
                    entry = {
                        "to": self._call_hierarchy_item(target),
                        "fromRanges": [],
                    }
                    outgoing_by_target[key] = entry
                entry["fromRanges"].append(call_site.range)
        results = list(outgoing_by_target.values())
        results.sort(
            key=lambda entry: (
                str(entry["to"]["name"]).lower(),
                int(entry["to"]["selectionRange"]["start"]["line"]),
                int(entry["to"]["selectionRange"]["start"]["character"]),
            )
        )
        return results or None

    def _document_highlights(self, params: dict[str, Any]) -> list[dict[str, Any]] | None:
        text_document = params["textDocument"]
        position = params["position"]
        document = self.documents.get(text_document["uri"])
        if document is None:
            return None

        symbol = self._symbol_at(document.text, position["line"], position["character"])
        if not symbol:
            return None

        declarations = self._declarations_for_symbol(document.text, symbol)
        if not declarations:
            return None

        declaration_ranges = {
            (
                declaration.selection_start,
                declaration.selection_end,
            )
            for declaration in declarations
        }
        highlights: list[dict[str, Any]] = []
        for start, end, match_range in self._symbol_ranges(document.text, symbol):
            kind = 3 if (start, end) in declaration_ranges else 2
            highlights.append(
                {
                    "range": match_range,
                    "kind": kind,
                }
            )
        return highlights or None

    def _references(self, params: dict[str, Any]) -> list[dict[str, Any]] | None:
        text_document = params["textDocument"]
        position = params["position"]
        document = self.documents.get(text_document["uri"])
        if document is None:
            return None
        include_declaration = bool(((params.get("context") or {}).get("includeDeclaration", True)))

        symbol, symbol_range = self._symbol_with_range_at(
            document.text,
            position["line"],
            position["character"],
        )
        if not symbol or symbol_range is None:
            return None

        local_declarations = self._declarations_for_symbol(document.text, symbol)
        if local_declarations:
            return self._document_references(document, symbol, include_declaration) or None

        workspace_declarations = self._workspace_definition_records(symbol, document.uri)
        if not workspace_declarations:
            return None

        references = self._workspace_references(document, symbol, include_declaration)
        return references or None

    def _prepare_rename(self, params: dict[str, Any]) -> dict[str, Any] | None:
        text_document = params["textDocument"]
        position = params["position"]
        document = self.documents.get(text_document["uri"])
        if document is None:
            return None

        symbol, symbol_range = self._symbol_with_range_at(
            document.text,
            position["line"],
            position["character"],
        )
        if not symbol or symbol_range is None:
            return None
        if self._declarations_for_symbol(document.text, symbol):
            return symbol_range
        if not self._workspace_definition_records(symbol, document.uri):
            return None
        return symbol_range

    def _rename(self, params: dict[str, Any]) -> dict[str, Any] | None:
        text_document = params["textDocument"]
        position = params["position"]
        new_name = params.get("newName")
        document = self.documents.get(text_document["uri"])
        if document is None or not isinstance(new_name, str):
            return None
        if not re.fullmatch(r"[0-9A-Za-z_+\-*/!?<>=./:]+", new_name):
            return None

        symbol, _ = self._symbol_with_range_at(
            document.text,
            position["line"],
            position["character"],
        )
        if not symbol:
            return None
        if self._declarations_for_symbol(document.text, symbol):
            edits = [
                {
                    "range": match_range,
                    "newText": new_name,
                }
                for _, _, match_range in self._symbol_ranges(document.text, symbol)
            ]
            if not edits:
                return None
            return {"changes": {document.uri: edits}}

        if not self._workspace_definition_records(symbol, document.uri):
            return None

        references = self._workspace_references(document, symbol, True)
        if not references:
            return None

        changes: dict[str, list[dict[str, Any]]] = {}
        for item in references:
            uri = str(item["uri"])
            changes.setdefault(uri, []).append(
                {
                    "range": item["range"],
                    "newText": new_name,
                }
            )
        if not changes:
            return None
        return {"changes": changes}

    def _all_declaration_records(self) -> list[WorkspaceDeclarationRecord]:
        records: list[WorkspaceDeclarationRecord] = []
        seen: set[tuple[str, int, int, int, int]] = set()

        for document in self.documents.values():
            for record in self._workspace_declaration_records_from_document(document):
                key = self._declaration_record_key(record)
                if key in seen:
                    continue
                seen.add(key)
                records.append(record)

        open_paths = {
            document.path.resolve()
            for document in self.documents.values()
            if document.path is not None and document.path.exists()
        }
        for root in self._workspace_roots():
            for path, path_records in self._cached_workspace_declaration_records(root).items():
                if path in open_paths:
                    continue
                for record in path_records:
                    key = self._declaration_record_key(record)
                    if key in seen:
                        continue
                    seen.add(key)
                    records.append(record)
        return records

    def _call_hierarchy_record_for_item(
        self,
        item: dict[str, Any],
    ) -> WorkspaceDeclarationRecord | None:
        uri = item.get("uri")
        selection_range = item.get("selectionRange") or {}
        if not isinstance(uri, str) or not isinstance(selection_range, dict):
            return None
        key = self._call_hierarchy_item_key(uri, selection_range)
        if key is None:
            return None

        document = self.documents.get(uri)
        if document is not None:
            for record in self._workspace_declaration_records_from_document(document):
                if self._declaration_record_key(record) == key:
                    return record

        path = self._uri_to_path(uri)
        if path is not None:
            resolved = path.resolve()
            for root in self._workspace_roots():
                path_records = self._cached_workspace_declaration_records(root).get(resolved)
                if path_records is None:
                    continue
                for record in path_records:
                    if self._declaration_record_key(record) == key:
                        return record
        return None

    def _call_hierarchy_item_key(
        self,
        uri: str,
        selection_range: dict[str, Any],
    ) -> tuple[str, int, int, int, int] | None:
        start = selection_range.get("start") or {}
        end = selection_range.get("end") or {}
        try:
            return (
                uri,
                int(start.get("line", 0)),
                int(start.get("character", 0)),
                int(end.get("line", 0)),
                int(end.get("character", 0)),
            )
        except (TypeError, ValueError):
            return None

    def _declarations_for_symbol(self, text: str, symbol: str) -> list[Declaration]:
        return [
            declaration
            for declaration in self._flatten_declarations(self._scan_top_level_declarations(text))
            if declaration.name == symbol
        ]

    def _symbol_ranges(self, text: str, symbol: str) -> list[tuple[int, int, dict[str, Any]]]:
        pattern = re.compile(rf"(?<![0-9A-Za-z_+\-*/!?<>=./:]){re.escape(symbol)}(?![0-9A-Za-z_+\-*/!?<>=./:])")
        ranges: list[tuple[int, int, dict[str, Any]]] = []
        for match in pattern.finditer(text):
            ranges.append(
                (
                    match.start(),
                    match.end(),
                    self._offset_range(text, match.start(), match.end()),
                )
            )
        return ranges

    def _document_references(
        self,
        document: Document,
        symbol: str,
        include_declaration: bool,
    ) -> list[dict[str, Any]]:
        declaration_ranges = {
            (
                declaration.selection_start,
                declaration.selection_end,
            )
            for declaration in self._declarations_for_symbol(document.text, symbol)
        }
        references: list[dict[str, Any]] = []
        for start, end, match_range in self._symbol_ranges(document.text, symbol):
            if not include_declaration and (start, end) in declaration_ranges:
                continue
            references.append(
                {
                    "uri": document.uri,
                    "range": match_range,
                }
            )
        return references

    def _workspace_references(
        self,
        document: Document,
        symbol: str,
        include_declaration: bool,
    ) -> list[dict[str, Any]]:
        references: list[dict[str, Any]] = []
        seen: set[tuple[str, int, int, int, int]] = set()

        current_declaration_ranges = self._workspace_declaration_ranges(document.text, symbol)
        current_references = self._references_for_text(
            document.uri,
            document.text,
            symbol,
            include_declaration,
            current_declaration_ranges,
        )
        for item in current_references:
            key = self._location_key(item)
            if key in seen:
                continue
            seen.add(key)
            references.append(item)

        for other in self.documents.values():
            if other.uri == document.uri:
                continue
            declaration_ranges = self._workspace_declaration_ranges(other.text, symbol)
            for item in self._references_for_text(
                other.uri,
                other.text,
                symbol,
                include_declaration,
                declaration_ranges,
            ):
                key = self._location_key(item)
                if key in seen:
                    continue
                seen.add(key)
                references.append(item)

        open_paths = {
            other.path.resolve()
            for other in self.documents.values()
            if other.path is not None and other.path.exists()
        }
        for root in self._workspace_roots():
            for path, path_records in self._cached_workspace_declaration_records(root).items():
                if path in open_paths:
                    continue
                text = self._workspace_text_for_path(path)
                if text is None:
                    continue
                uri = path.resolve().as_uri()
                declaration_ranges = {
                    self._range_offsets_from_payload(text, record.range)
                    for record in path_records
                    if record.name == symbol
                }
                for item in self._references_for_text(
                    uri,
                    text,
                    symbol,
                    include_declaration,
                    declaration_ranges,
                ):
                    key = self._location_key(item)
                    if key in seen:
                        continue
                    seen.add(key)
                    references.append(item)
        return references

    def _references_for_text(
        self,
        uri: str,
        text: str,
        symbol: str,
        include_declaration: bool,
        declaration_ranges: set[tuple[int, int]],
    ) -> list[dict[str, Any]]:
        references: list[dict[str, Any]] = []
        for start, end, match_range in self._symbol_ranges(text, symbol):
            if not include_declaration and (start, end) in declaration_ranges:
                continue
            references.append(
                {
                    "uri": uri,
                    "range": match_range,
                }
            )
        return references

    def _workspace_declaration_ranges(self, text: str, symbol: str) -> set[tuple[int, int]]:
        return {
            (
                declaration.selection_start,
                declaration.selection_end,
            )
            for declaration in self._declarations_for_symbol(text, symbol)
        }

    def _workspace_text_for_path(self, path: Path) -> str | None:
        try:
            return path.read_text(encoding="utf-8")
        except (OSError, UnicodeDecodeError):
            return None

    def _range_offsets_from_payload(
        self,
        text: str,
        payload: dict[str, Any],
    ) -> tuple[int, int]:
        start = payload.get("start") or {}
        end = payload.get("end") or {}
        return (
            self._position_to_offset(text, int(start.get("line", 0)), int(start.get("character", 0))),
            self._position_to_offset(text, int(end.get("line", 0)), int(end.get("character", 0))),
        )

    def _location_key(self, item: dict[str, Any]) -> tuple[str, int, int, int, int]:
        item_range = item["range"]
        start = item_range["start"]
        end = item_range["end"]
        return (
            str(item["uri"]),
            int(start["line"]),
            int(start["character"]),
            int(end["line"]),
            int(end["character"]),
        )

    def _declaration_snippet(self, text: str, declaration: Declaration) -> str:
        snippet = text[declaration.start : declaration.end].strip()
        lines = snippet.splitlines()
        if len(lines) > 6:
            snippet = "\n".join(lines[:6] + ["..."])
        if len(snippet) > 320:
            snippet = snippet[:317].rstrip() + "..."
        return snippet

    def _declaration_call_sites(self, text: str, declaration: Declaration) -> list[CallSite]:
        items = self._container_items(text, declaration.start, declaration.end)
        body_items = self._declaration_body_items(text, items)
        call_sites: list[CallSite] = []
        for item in body_items:
            self._collect_call_sites(text, item, call_sites)
        return call_sites

    def _declaration_body_items(
        self,
        text: str,
        items: list[dict[str, Any]],
    ) -> list[dict[str, Any]]:
        if len(items) < 3 or items[0].get("text") != "define":
            return []

        target = items[1]
        if target["kind"] == "list":
            return items[2:]

        if target["kind"] == "atom" and len(items) > 2 and items[2]["kind"] == "list":
            lambda_items = self._container_items(text, items[2]["start"], items[2]["end"])
            if len(lambda_items) >= 3 and lambda_items[0]["kind"] == "atom" and lambda_items[0]["text"] in {"lambda", "λ"}:
                return lambda_items[2:]
        return items[2:]
