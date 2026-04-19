from omni_lsp_shared import *


class PresentationMixin:
    def _folding_ranges(self, params: dict[str, Any]) -> list[dict[str, Any]]:
        text_document = params["textDocument"]
        document = self.documents.get(text_document["uri"])
        if document is None:
            return []

        ranges: list[dict[str, Any]] = []
        seen: set[tuple[int, int]] = set()
        for form_start, form_end in self._top_level_forms(document.text):
            self._collect_folding_ranges(document.text, form_start, form_end, ranges, seen)
        return ranges

    def _selection_ranges(self, params: dict[str, Any]) -> list[dict[str, Any]]:
        text_document = params["textDocument"]
        positions = params.get("positions") or []
        document = self.documents.get(text_document["uri"])
        if document is None:
            return []

        ranges: list[dict[str, Any]] = []
        for position in positions:
            line = int((position or {}).get("line", 0))
            character = int((position or {}).get("character", 0))
            ranges.append(self._selection_range_at(document.text, line, character))
        return ranges

    def _linked_editing_ranges(self, params: dict[str, Any]) -> dict[str, Any] | None:
        text_document = params["textDocument"]
        position = params["position"]
        document = self.documents.get(text_document["uri"])
        if document is None:
            return None

        ranges = self._parameter_linked_ranges(
            document.text,
            int(position["line"]),
            int(position["character"]),
        )
        if not ranges:
            return None
        return {
            "ranges": ranges,
            "wordPattern": r"[0-9A-Za-z_+\-*/!?<>=./:]+",
        }

    def _formatting(self, params: dict[str, Any]) -> list[dict[str, Any]]:
        text_document = params["textDocument"]
        options = params.get("options") or {}
        document = self.documents.get(text_document["uri"])
        if document is None:
            return []

        formatted = self._formatted_text(document.text, options)
        if formatted == document.text:
            return []
        end_position = self._offset_to_position(document.text, len(document.text))
        return [
            {
                "range": {
                    "start": {"line": 0, "character": 0},
                    "end": end_position,
                },
                "newText": formatted,
            }
        ]

    def _range_formatting(self, params: dict[str, Any]) -> list[dict[str, Any]]:
        text_document = params["textDocument"]
        options = params.get("options") or {}
        range_params = params.get("range") or {}
        start = range_params.get("start") or {}
        end = range_params.get("end") or {}
        document = self.documents.get(text_document["uri"])
        if document is None:
            return []

        formatted = self._formatted_text(document.text, options)
        if formatted == document.text:
            return []

        original_offsets = self._line_start_offsets(document.text)
        formatted_offsets = self._line_start_offsets(formatted)
        start_line = int(start.get("line", 0))
        end_line = int(end.get("line", start_line))
        last_line = max(0, len(original_offsets) - 2)
        start_line = max(0, min(start_line, last_line))
        end_line = max(start_line, min(end_line, last_line))

        start_offset = original_offsets[start_line]
        end_offset = original_offsets[end_line + 1]
        formatted_start_offset = formatted_offsets[start_line]
        formatted_end_offset = formatted_offsets[end_line + 1]
        new_text = formatted[formatted_start_offset:formatted_end_offset]
        if new_text == document.text[start_offset:end_offset]:
            return []
        return [
            {
                "range": {
                    "start": {"line": start_line, "character": 0},
                    "end": self._offset_to_position(document.text, end_offset),
                },
                "newText": new_text,
            }
        ]

    def _on_type_formatting(self, params: dict[str, Any]) -> list[dict[str, Any]]:
        text_document = params["textDocument"]
        options = params.get("options") or {}
        position = params.get("position") or {}
        document = self.documents.get(text_document["uri"])
        if document is None:
            return []

        formatted = self._formatted_text(document.text, options)
        if formatted == document.text:
            return []

        line = int(position.get("line", 0))
        return self._line_formatting_edits(document.text, formatted, line)

    def _semantic_tokens(self, params: dict[str, Any]) -> dict[str, list[int]]:
        text_document = params["textDocument"]
        document = self.documents.get(text_document["uri"])
        if document is None:
            return {"data": []}

        tokens: list[tuple[int, int, int, int, int]] = []
        seen: set[tuple[int, int, int, int, int]] = set()
        declarations = self._scan_top_level_declarations(document.text)

        self._collect_lexical_semantic_tokens(document.text, tokens, seen)
        for form_start, form_end in self._top_level_forms(document.text):
            self._collect_form_semantic_tokens(document.text, form_start, form_end, tokens, seen)
        for declaration in self._flatten_declarations(declarations):
            token_type = SEMANTIC_TOKEN_TYPE_BY_DETAIL.get(declaration.detail)
            if token_type is not None:
                self._add_semantic_token_range(
                    document.text,
                    declaration.selection_start,
                    declaration.selection_end,
                    token_type,
                    tokens,
                    seen,
                    {"declaration"},
                )
            self._collect_declaration_parameter_semantic_tokens(document.text, declaration, tokens, seen)

        return {"data": self._encode_semantic_tokens(tokens)}

    def _collect_lexical_semantic_tokens(
        self,
        text: str,
        tokens: list[tuple[int, int, int, int, int]],
        seen: set[tuple[int, int, int, int, int]],
    ) -> None:
        index = 0
        limit = len(text)
        while index < limit:
            char = text[index]
            if char == ";":
                end = self._skip_comment(text, index)
                self._add_semantic_token_range(text, index, end, "comment", tokens, seen)
                index = end
                continue
            if char == '"':
                end = self._scan_string(text, index)
                self._add_semantic_token_range(text, index, end, "string", tokens, seen)
                index = end
                continue
            if char.isspace() or char in "()[]{}":
                index += 1
                continue

            end = index
            while end < limit:
                current = text[end]
                if current.isspace() or current in "()[]{}" or current == ";":
                    break
                end += 1
            atom = text[index:end]
            if NUMERIC_ATOM_RE.fullmatch(atom):
                self._add_semantic_token_range(text, index, end, "number", tokens, seen)
            elif atom in BUILTIN_VALUES:
                token_type = "type" if atom and atom[0].isupper() else "variable"
                self._add_semantic_token_range(text, index, end, token_type, tokens, seen)
            index = end

    def _collect_form_semantic_tokens(
        self,
        text: str,
        start: int,
        end: int,
        tokens: list[tuple[int, int, int, int, int]],
        seen: set[tuple[int, int, int, int, int]],
    ) -> None:
        items = self._container_items(text, start, end)
        if text[start] == "(" and items and items[0]["kind"] == "atom":
            head = items[0]["text"]
            if head in SPECIAL_FORMS or head == "λ":
                self._add_semantic_token_range(
                    text,
                    items[0]["start"],
                    items[0]["end"],
                    "keyword",
                    tokens,
                    seen,
                )

        for item in items:
            if item["kind"] not in {"list", "vector", "map"}:
                continue
            self._collect_form_semantic_tokens(text, item["start"], item["end"], tokens, seen)

    def _collect_declaration_parameter_semantic_tokens(
        self,
        text: str,
        declaration: Declaration,
        tokens: list[tuple[int, int, int, int, int]],
        seen: set[tuple[int, int, int, int, int]],
    ) -> None:
        items = self._container_items(text, declaration.start, declaration.end)
        if len(items) < 2 or items[0].get("text") != "define":
            return

        target = items[1]
        if target["kind"] == "list":
            target_items = self._container_items(text, target["start"], target["end"])
            for selection in self._declaration_parameter_items(text, declaration):
                self._add_semantic_token_range(
                    text,
                    selection["start"],
                    selection["end"],
                    "parameter",
                    tokens,
                    seen,
                    {"declaration"},
                )
            return

        for selection in self._declaration_parameter_items(text, declaration):
            self._add_semantic_token_range(
                text,
                selection["start"],
                selection["end"],
                "parameter",
                tokens,
                seen,
                {"declaration"},
            )

    def _parameter_selection_item(
        self,
        text: str,
        item: dict[str, Any],
    ) -> dict[str, Any] | None:
        if item["kind"] == "atom":
            return item
        if item["kind"] not in {"list", "vector", "map"}:
            return None
        inner_items = self._container_items(text, item["start"], item["end"])
        for candidate in reversed(inner_items):
            selection = self._parameter_selection_item(text, candidate)
            if selection is not None:
                return selection
        return None

    def _add_semantic_token_range(
        self,
        text: str,
        start: int,
        end: int,
        token_type: str,
        tokens: list[tuple[int, int, int, int, int]],
        seen: set[tuple[int, int, int, int, int]],
        modifiers: set[str] | None = None,
    ) -> None:
        if end <= start:
            return
        token_type_index = SEMANTIC_TOKEN_TYPE_INDEX.get(token_type)
        if token_type_index is None:
            return
        modifier_bits = self._semantic_token_modifier_bits(modifiers or set())
        current = start
        while current < end:
            newline = text.find("\n", current, end)
            fragment_end = end if newline == -1 else newline
            if fragment_end > current:
                position = self._offset_to_position(text, current)
                token = (
                    position["line"],
                    position["character"],
                    fragment_end - current,
                    token_type_index,
                    modifier_bits,
                )
                if token not in seen:
                    seen.add(token)
                    tokens.append(token)
            if newline == -1:
                break
            current = newline + 1

    def _semantic_token_modifier_bits(self, modifiers: set[str]) -> int:
        bits = 0
        for modifier in modifiers:
            index = SEMANTIC_TOKEN_MODIFIER_INDEX.get(modifier)
            if index is not None:
                bits |= 1 << index
        return bits

    def _encode_semantic_tokens(
        self,
        tokens: list[tuple[int, int, int, int, int]],
    ) -> list[int]:
        data: list[int] = []
        previous_line = 0
        previous_start = 0
        for line, start, length, token_type, modifiers in sorted(tokens):
            delta_line = line - previous_line
            delta_start = start if delta_line else start - previous_start
            data.extend([delta_line, delta_start, length, token_type, modifiers])
            previous_line = line
            previous_start = start
        return data
