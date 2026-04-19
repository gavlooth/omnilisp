from omni_lsp_shared import *


class FormatHelpersMixin:
    def _collect_inlay_hints_in_container(
        self,
        document: Document,
        start: int,
        end: int,
        range_start: int,
        range_end: int,
        hints: list[dict[str, Any]],
    ) -> None:
        text = document.text
        items = self._container_items(text, start, end)
        if text[start] == "(" and items and items[0]["kind"] == "atom":
            parameter_labels = self._parameter_hint_labels_for_symbol(document, items[0]["text"])
            for index, item in enumerate(items[1:]):
                if index >= len(parameter_labels):
                    break
                if item["start"] < range_start or item["start"] > range_end:
                    continue
                label = self._parameter_hint_display_label(parameter_labels[index])
                if label is None:
                    continue
                hints.append(
                    {
                        "position": self._offset_to_position(text, item["start"]),
                        "label": label,
                        "kind": 2,
                        "paddingRight": True,
                    }
                )

        for item in items:
            if item["kind"] not in {"list", "vector", "map"}:
                continue
            self._collect_inlay_hints_in_container(
                document,
                item["start"],
                item["end"],
                range_start,
                range_end,
                hints,
                )

    def _parameter_hint_labels_for_symbol(
        self,
        document: Document,
        symbol: str,
    ) -> list[str]:
        local_candidates = [
            self._declaration_parameter_labels(
                document.text,
                self._container_items(document.text, declaration.start, declaration.end),
            )
            for declaration in self._declarations_for_symbol(document.text, symbol)
        ]
        labels = self._resolve_parameter_label_candidates(local_candidates)
        if labels:
            return labels

        workspace_candidates = [
            record.parameter_labels
            for record in self._workspace_definition_records(symbol, document.uri)
            if record.parameter_labels
        ]
        labels = self._resolve_parameter_label_candidates(workspace_candidates)
        if labels:
            return labels

        return []

    def _resolve_parameter_label_candidates(
        self,
        candidates: list[list[str]],
    ) -> list[str] | None:
        normalized: list[list[str]] = []
        for candidate in candidates:
            if not candidate:
                continue
            labels = [self._parameter_label_name(label) for label in candidate]
            if not all(labels):
                continue
            normalized.append([label for label in labels if label is not None])
        if not normalized:
            return None
        first = normalized[0]
        for candidate in normalized[1:]:
            if candidate != first:
                return None
        return first

    def _parameter_label_name(self, parameter_label: str) -> str | None:
        tokens = re.findall(r"[0-9A-Za-z_+\-*/!?<>=./:]+", parameter_label)
        if not tokens:
            stripped = parameter_label.strip()
            return stripped or None
        return tokens[-1]

    def _parameter_hint_display_label(self, parameter_label: str) -> str | None:
        name = self._parameter_label_name(parameter_label)
        if not name:
            return None
        return f"{name}:"

    def _collect_folding_ranges(
        self,
        text: str,
        start: int,
        end: int,
        ranges: list[dict[str, Any]],
        seen: set[tuple[int, int]],
    ) -> None:
        start_pos = self._offset_to_position(text, start)
        end_pos = self._offset_to_position(text, max(start, end - 1))
        if start_pos["line"] < end_pos["line"]:
            key = (start_pos["line"], end_pos["line"])
            if key not in seen:
                seen.add(key)
                ranges.append(
                    {
                        "startLine": start_pos["line"],
                        "endLine": end_pos["line"],
                    }
                )

        items = self._container_items(text, start, end)
        for item in items:
            if item["kind"] not in {"list", "vector", "map"}:
                continue
            self._collect_folding_ranges(text, item["start"], item["end"], ranges, seen)

    def _selection_range_at(self, text: str, line: int, character: int) -> dict[str, Any]:
        offset = self._position_to_offset(text, line, character)
        symbol_range = self._range_for_symbol_at_offset(text, offset)
        container_ranges = self._container_ranges_at_offset(text, offset)

        ordered_ranges: list[tuple[int, int]] = []
        seen: set[tuple[int, int, int, int]] = set()

        if symbol_range is not None:
            ordered_ranges.append(symbol_range)
            symbol_range_value = self._offset_range(text, symbol_range[0], symbol_range[1])
            seen.add(
                (
                    int(symbol_range_value["start"]["line"]),
                    int(symbol_range_value["start"]["character"]),
                    int(symbol_range_value["end"]["line"]),
                    int(symbol_range_value["end"]["character"]),
                )
            )

        for start, end in container_ranges:
            candidate = self._offset_range(text, start, end)
            key = (
                int(candidate["start"]["line"]),
                int(candidate["start"]["character"]),
                int(candidate["end"]["line"]),
                int(candidate["end"]["character"]),
            )
            if key in seen:
                continue
            ordered_ranges.append((start, end))
            seen.add(key)

        if not ordered_ranges:
            ordered_ranges.append((offset, offset + 1))

        current: dict[str, Any] | None = None
        for start, end in reversed(ordered_ranges):
            current = self._selection_range_node(text, start, end, current)
        if current is None:
            current = self._selection_range_node(text, offset, offset + 1, None)
        return current

    def _selection_range_node(
        self,
        text: str,
        start: int,
        end: int,
        parent: dict[str, Any] | None,
    ) -> dict[str, Any]:
        node = {"range": self._offset_range(text, start, end)}
        if parent is not None:
            node["parent"] = parent
        return node

    def _parameter_linked_ranges(
        self,
        text: str,
        line: int,
        character: int,
    ) -> list[dict[str, Any]]:
        offset = self._position_to_offset(text, line, character)
        declaration = self._declaration_at_offset(text, offset)
        if declaration is None:
            return []

        items = self._container_items(text, declaration.start, declaration.end)
        body_items = self._declaration_body_items(text, items)
        for parameter_item in self._declaration_parameter_items(text, declaration):
            symbol = text[parameter_item["start"] : parameter_item["end"]]
            ranges = [self._offset_range(text, parameter_item["start"], parameter_item["end"])]
            seen = {(parameter_item["start"], parameter_item["end"])}
            for item in body_items:
                self._collect_linked_symbol_occurrences(text, item, symbol, ranges, seen)
            for token_range in ranges:
                start = token_range["start"]
                end = token_range["end"]
                start_offset = self._position_to_offset(
                    text,
                    int(start["line"]),
                    int(start["character"]),
                )
                end_offset = self._position_to_offset(
                    text,
                    int(end["line"]),
                    int(end["character"]),
                )
                if start_offset <= offset < end_offset:
                    return ranges
        return []

    def _collect_linked_symbol_occurrences(
        self,
        text: str,
        item: dict[str, Any],
        symbol: str,
        ranges: list[dict[str, Any]],
        seen: set[tuple[int, int]],
    ) -> None:
        if item["kind"] == "atom":
            if item["text"] == symbol and (item["start"], item["end"]) not in seen:
                seen.add((item["start"], item["end"]))
                ranges.append(self._offset_range(text, item["start"], item["end"]))
            return
        if item["kind"] not in {"list", "vector", "map"}:
            return

        inner_items = self._container_items(text, item["start"], item["end"])
        if item["kind"] == "list" and inner_items and inner_items[0]["kind"] == "atom":
            if inner_items[0]["text"] in {"define", "module", "quote"}:
                return

        for inner in inner_items:
            self._collect_linked_symbol_occurrences(text, inner, symbol, ranges, seen)

    def _range_for_symbol_at_offset(self, text: str, offset: int) -> tuple[int, int] | None:
        symbol, symbol_range = self._symbol_with_range_at_offset(text, offset)
        if not symbol or symbol_range is None:
            return None
        start = self._position_to_offset(
            text,
            int(symbol_range["start"]["line"]),
            int(symbol_range["start"]["character"]),
        )
        end = self._position_to_offset(
            text,
            int(symbol_range["end"]["line"]),
            int(symbol_range["end"]["character"]),
        )
        return start, end

    def _container_ranges_at_offset(self, text: str, offset: int) -> list[tuple[int, int]]:
        ranges: list[tuple[int, int]] = []
        for form_start, form_end in self._top_level_forms(text):
            self._collect_container_ranges_at_offset(text, form_start, form_end, offset, ranges)
        ranges.sort(key=lambda item: (item[1] - item[0], item[0]))
        return ranges

    def _collect_container_ranges_at_offset(
        self,
        text: str,
        start: int,
        end: int,
        offset: int,
        ranges: list[tuple[int, int]],
    ) -> None:
        if offset < start or offset >= end:
            return
        ranges.append((start, end))
        for item in self._container_items(text, start, end):
            if item["kind"] not in {"list", "vector", "map"}:
                continue
            self._collect_container_ranges_at_offset(text, item["start"], item["end"], offset, ranges)

    def _formatted_text(self, text: str, options: dict[str, Any]) -> str:
        lines = text.splitlines()
        if not lines:
            return text

        indent_unit = self._indent_unit(options)
        newline = self._preferred_newline(text)
        has_trailing_newline = text.endswith("\n")

        if options.get("insertSpaces", True) is not False:
            indent_width = len(indent_unit)
            frames: list[FormatFrame] = []
            formatted_lines: list[str] = []
            for line in lines:
                stripped = line.lstrip(" \t").rstrip()
                if not stripped:
                    formatted_lines.append("")
                    continue

                indent_column = self._line_indent_column(stripped, frames)
                formatted_lines.append(f"{' ' * indent_column}{stripped}")
                self._update_format_frames(stripped, indent_column, indent_width, frames)

            formatted = newline.join(formatted_lines)
            if has_trailing_newline:
                formatted += newline
            return formatted

        indent_level = 0
        formatted_lines: list[str] = []

        for line in lines:
            stripped = line.lstrip(" \t").rstrip()
            if not stripped:
                formatted_lines.append("")
                continue

            leading_closers = self._leading_closer_count(stripped)
            line_indent_level = max(0, indent_level - leading_closers)
            formatted_lines.append(f"{indent_unit * line_indent_level}{stripped}")
            indent_level = max(0, indent_level + self._line_indent_delta(stripped))

        formatted = newline.join(formatted_lines)
        if has_trailing_newline:
            formatted += newline
        return formatted

    def _preferred_newline(self, text: str) -> str:
        return "\r\n" if "\r\n" in text else "\n"

    def _line_indent_column(self, text: str, frames: list[FormatFrame]) -> int:
        temp_depth = len(frames)
        anchor: int | None = None
        index = 0
        while index < len(text) and text[index] in ")]}":
            if temp_depth <= 0 or frames[temp_depth - 1].close_char != text[index]:
                break
            anchor = frames[temp_depth - 1].open_column
            temp_depth -= 1
            index += 1

        if index >= len(text):
            return anchor or 0
        if anchor is not None:
            return anchor
        return frames[temp_depth - 1].child_column if temp_depth > 0 else 0

    def _update_format_frames(
        self,
        text: str,
        line_indent_column: int,
        indent_width: int,
        frames: list[FormatFrame],
    ) -> None:
        index = 0
        while index < len(text):
            char = text[index]
            if char == ";":
                return
            if char == '"':
                index = self._scan_string(text, index)
                continue
            if char in ")]}":
                if frames and frames[-1].close_char == char:
                    frames.pop()
                index += 1
                continue
            if char not in FORM_PAIRS:
                index += 1
                continue

            open_column = line_indent_column + index
            if char == "(" and frames and frames[-1].let_binding_pending:
                frames[-1].let_binding_pending = False
                binding_start = self._skip_horizontal_space(text, index + 1)
                child_column = line_indent_column + (binding_start if binding_start < len(text) else indent_width)
                frames.append(FormatFrame(FORM_PAIRS[char], open_column, child_column, binding_context=True))
                index += 1
                continue

            let_binding_pending = False
            higher_order_lambda_context = False
            coroutine_lambda_context = False
            if char == "(":
                child_column, let_binding_pending, higher_order_lambda_context, coroutine_lambda_context = self._list_child_column(
                    text,
                    index,
                    line_indent_column,
                    indent_width,
                )
                if frames and frames[-1].binding_context and self._prefers_binding_context_alignment(text, index):
                    child_column = open_column + indent_width
                if frames and frames[-1].higher_order_lambda_context:
                    head_start = self._skip_horizontal_space(text, index + 1)
                    head_end = self._token_end(text, head_start)
                    if head_end > head_start and text[head_start:head_end] == "lambda":
                        child_column = open_column + indent_width
                if frames and frames[-1].coroutine_lambda_context:
                    head_start = self._skip_horizontal_space(text, index + 1)
                    head_end = self._token_end(text, head_start)
                    if head_end > head_start and text[head_start:head_end] == "lambda":
                        child_column = line_indent_column + indent_width * 2
            elif char == "[":
                child_column = self._vector_child_column(text, index, line_indent_column, indent_width)
            else:
                child_column = self._map_child_column(text, index, line_indent_column, indent_width)
            frames.append(
                FormatFrame(
                    FORM_PAIRS[char],
                    open_column,
                    child_column,
                    let_binding_pending,
                    False,
                    higher_order_lambda_context,
                    coroutine_lambda_context,
                )
            )
            index += 1

    def _list_child_column(
        self,
        text: str,
        open_index: int,
        line_indent_column: int,
        indent_width: int,
    ) -> tuple[int, bool, bool, bool]:
        head_start = self._skip_horizontal_space(text, open_index + 1)
        if head_start >= len(text) or text[head_start] in ")]};":
            return line_indent_column + indent_width, False, False, False
        if text[head_start] in "([{":
            return line_indent_column + open_index + 1, False, False, False

        head_end = self._token_end(text, head_start)
        if head_end <= head_start:
            return line_indent_column + indent_width, False, False, False

        head = text[head_start:head_end]
        let_binding_pending = head == "let"
        higher_order_lambda_context = head in HIGHER_ORDER_CALL_HEADS
        coroutine_lambda_context = head in COROUTINE_WRAPPER_HEADS
        if head == "if":
            return line_indent_column + open_index + indent_width * 2, let_binding_pending, higher_order_lambda_context, coroutine_lambda_context
        if head in BLOCK_FORMAT_HEADS:
            return line_indent_column + indent_width, let_binding_pending, higher_order_lambda_context, coroutine_lambda_context

        value_start = self._skip_horizontal_space(text, head_end)
        if value_start >= len(text) or text[value_start] in ")]};":
            return line_indent_column + indent_width, let_binding_pending, higher_order_lambda_context, coroutine_lambda_context
        return line_indent_column + value_start, let_binding_pending, higher_order_lambda_context, coroutine_lambda_context

    def _vector_child_column(self, text: str, open_index: int, line_indent_column: int, indent_width: int) -> int:
        value_start = self._skip_horizontal_space(text, open_index + 1)
        if value_start >= len(text) or text[value_start] in ")]};":
            return line_indent_column + indent_width
        if text[value_start] in "([{":
            return line_indent_column + open_index + 1
        return line_indent_column + indent_width

    def _map_child_column(self, text: str, open_index: int, line_indent_column: int, indent_width: int) -> int:
        value_start = self._skip_horizontal_space(text, open_index + 1)
        if value_start >= len(text) or text[value_start] in ")]};":
            return line_indent_column + indent_width
        return line_indent_column + open_index + 1

    def _prefers_binding_context_alignment(self, text: str, open_index: int) -> bool:
        head_start = self._skip_horizontal_space(text, open_index + 1)
        if head_start >= len(text) or text[head_start] in ")]};":
            return False

        head_end = self._token_end(text, head_start)
        if head_end <= head_start:
            return False

        return text[head_start:head_end] in {
            "lambda",
            "let",
            "block",
            "handle",
            "match",
            "syntax-match",
            "template",
        }

    def _skip_horizontal_space(self, text: str, index: int) -> int:
        while index < len(text) and text[index] in " \t":
            index += 1
        return index

    def _token_end(self, text: str, index: int) -> int:
        while index < len(text):
            char = text[index]
            if char in " \t()[]{};\"" or char in ")]}":
                break
            index += 1
        return index

    def _indent_unit(self, options: dict[str, Any]) -> str:
        if options.get("insertSpaces", True) is False:
            return "\t"
        tab_size = int(options.get("tabSize", 2) or 2)
        return " " * max(1, tab_size)

    def _leading_closer_count(self, text: str) -> int:
        count = 0
        index = 0
        while index < len(text):
            char = text[index]
            if char.isspace():
                index += 1
                continue
            if char in ")]}":
                count += 1
                index += 1
                continue
            break
        return count

    def _line_indent_delta(self, text: str) -> int:
        delta = 0
        index = 0
        while index < len(text):
            char = text[index]
            if char == ";":
                break
            if char == '"':
                index = self._scan_string(text, index)
                continue
            if char in FORM_PAIRS:
                delta += 1
                index += 1
                continue
            if char in ")]}":
                delta -= 1
                index += 1
                continue
            index += 1
        return delta

    def _line_start_offsets(self, text: str) -> list[int]:
        offsets = [0]
        for index, char in enumerate(text):
            if char == "\n":
                offsets.append(index + 1)
        if offsets[-1] != len(text):
            offsets.append(len(text))
        return offsets

    def _line_formatting_edits(
        self,
        original: str,
        formatted: str,
        line: int,
    ) -> list[dict[str, Any]]:
        original_offsets = self._line_start_offsets(original)
        formatted_offsets = self._line_start_offsets(formatted)
        last_line = max(0, len(original_offsets) - 2)
        line = max(0, min(line, last_line))

        start_offset = original_offsets[line]
        end_offset = original_offsets[line + 1]
        formatted_start_offset = formatted_offsets[line]
        formatted_end_offset = formatted_offsets[line + 1]
        new_text = formatted[formatted_start_offset:formatted_end_offset]
        if new_text == original[start_offset:end_offset]:
            return []
        return [
            {
                "range": {
                    "start": {"line": line, "character": 0},
                    "end": self._offset_to_position(original, end_offset),
                },
                "newText": new_text,
            }
        ]

    def _call_context_at(self, text: str, line: int, character: int) -> tuple[str, int] | None:
        offset = self._position_to_offset(text, line, character)
        span = self._innermost_list_span(text, offset)
        if span is None:
            return None

        start, end = span
        items = self._container_items(text, start, end)
        if not items or items[0]["kind"] != "atom":
            return None

        arguments = items[1:]
        if not arguments:
            return items[0]["text"], 0

        active_parameter = max(0, len(arguments) - 1)
        for index, item in enumerate(arguments):
            if offset <= item["end"]:
                active_parameter = index
                break
        return items[0]["text"], active_parameter

    def _innermost_list_span(self, text: str, offset: int) -> tuple[int, int] | None:
        stack: list[tuple[str, int]] = []
        index = 0
        limit = min(max(offset, 0), len(text))
        while index < limit:
            char = text[index]
            if char == ";":
                index = self._skip_comment(text, index)
                continue
            if char == '"':
                index = self._scan_string(text, index)
                continue
            if char in FORM_PAIRS:
                stack.append((char, index))
                index += 1
                continue
            if stack and char == FORM_PAIRS[stack[-1][0]]:
                stack.pop()
                index += 1
                continue
            index += 1

        for opener, start in reversed(stack):
            if opener != "(":
                continue
            end = self._scan_balanced(text, start, len(text))
            if offset <= end:
                return start, end
        return None
