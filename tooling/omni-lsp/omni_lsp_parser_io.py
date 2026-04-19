from omni_lsp_shared import *


class ParserIOMixin:
    def _collect_call_sites(
        self,
        text: str,
        item: dict[str, Any],
        call_sites: list[CallSite],
    ) -> None:
        if item["kind"] not in {"list", "vector", "map"}:
            return

        items = self._container_items(text, item["start"], item["end"])
        if text[item["start"]] == "(" and items and items[0]["kind"] == "atom":
            symbol = items[0]["text"]
            if symbol not in SPECIAL_FORMS and symbol != "λ":
                call_sites.append(
                    CallSite(
                        symbol=symbol,
                        range=self._offset_range(text, items[0]["start"], items[0]["end"]),
                    )
                )

        for inner in items:
            if inner["kind"] not in {"list", "vector", "map"}:
                continue
            self._collect_call_sites(text, inner, call_sites)

    def _scan_top_level_declarations(self, text: str) -> list[Declaration]:
        declarations: list[Declaration] = []
        for form_start, form_end in self._top_level_forms(text):
            declaration = self._extract_declaration(text, form_start, form_end)
            if declaration is not None:
                declarations.append(declaration)
        return declarations

    def _flatten_declarations(self, declarations: list[Declaration]) -> list[Declaration]:
        flattened: list[Declaration] = []
        for declaration in declarations:
            flattened.append(declaration)
            if declaration.children:
                flattened.extend(self._flatten_declarations(declaration.children))
        return flattened

    def _flatten_declarations_with_containers(
        self,
        declarations: list[Declaration],
        container_name: str | None = None,
    ) -> list[tuple[Declaration, str | None]]:
        flattened: list[tuple[Declaration, str | None]] = []
        for declaration in declarations:
            flattened.append((declaration, container_name))
            if declaration.children:
                flattened.extend(
                    self._flatten_declarations_with_containers(
                        declaration.children,
                        declaration.name,
                    )
                )
        return flattened

    def _declaration_to_document_symbol(self, text: str, declaration: Declaration) -> dict[str, Any]:
        symbol = {
            "name": declaration.name,
            "detail": declaration.detail,
            "kind": declaration.kind,
            "range": self._offset_range(text, declaration.start, declaration.end),
            "selectionRange": self._offset_range(
                text,
                declaration.selection_start,
                declaration.selection_end,
            ),
        }
        if declaration.children:
            symbol["children"] = [
                self._declaration_to_document_symbol(text, child)
                for child in declaration.children
            ]
        return symbol

    def _top_level_forms(self, text: str) -> list[tuple[int, int]]:
        forms: list[tuple[int, int]] = []
        stack: list[str] = []
        top_form_start: int | None = None
        index = 0
        while index < len(text):
            char = text[index]
            if char == ";":
                index = self._skip_comment(text, index)
                continue
            if char == '"':
                index = self._scan_string(text, index)
                continue
            if char in FORM_PAIRS:
                if not stack and char == "(":
                    top_form_start = index
                stack.append(FORM_PAIRS[char])
                index += 1
                continue
            if stack and char == stack[-1]:
                stack.pop()
                index += 1
                if not stack and top_form_start is not None:
                    forms.append((top_form_start, index))
                    top_form_start = None
                continue
            index += 1
        return forms

    def _extract_declaration(self, text: str, form_start: int, form_end: int) -> Declaration | None:
        items = self._container_items(text, form_start, form_end)
        if not items or items[0]["kind"] != "atom":
            return None

        head = items[0]["text"]
        if head == "module":
            target = items[1] if len(items) > 1 else None
            name, selection = self._name_from_target(text, target)
            if not name or selection is None:
                return None
            children: list[Declaration] = []
            for item in items[2:]:
                if item["kind"] != "list":
                    continue
                child = self._extract_declaration(text, item["start"], item["end"])
                if child is not None:
                    children.append(child)
            return Declaration(
                name=name,
                detail="module",
                kind=DECLARATION_SYMBOL_KINDS["module"],
                start=form_start,
                end=form_end,
                selection_start=selection["start"],
                selection_end=selection["end"],
                children=children or None,
            )

        if head != "define" or len(items) < 2:
            return None

        detail = "define"
        kind = DECLARATION_SYMBOL_KINDS["variable"]
        target = items[1]
        if target["kind"] == "list":
            name, selection = self._name_from_target(text, target)
            if not name or selection is None:
                return None
            return Declaration(
                name=name,
                detail="function",
                kind=DECLARATION_SYMBOL_KINDS["function"],
                start=form_start,
                end=form_end,
                selection_start=selection["start"],
                selection_end=selection["end"],
            )

        if target["kind"] == "vector":
            attr_name = self._attribute_name(text, target)
            target = items[2] if len(items) > 2 else None
            name, selection = self._name_from_target(text, target)
            if not attr_name or not name or selection is None:
                return None
            detail = attr_name
            kind = DECLARATION_SYMBOL_KINDS.get(attr_name, DECLARATION_SYMBOL_KINDS["variable"])
            return Declaration(
                name=name,
                detail=detail,
                kind=kind,
                start=form_start,
                end=form_end,
                selection_start=selection["start"],
                selection_end=selection["end"],
            )

        if target["kind"] != "atom":
            return None
        return Declaration(
            name=target["text"],
            detail=detail,
            kind=kind,
            start=form_start,
            end=form_end,
            selection_start=target["start"],
            selection_end=target["end"],
        )

    def _attribute_name(self, text: str, item: dict[str, Any]) -> str | None:
        inner_items = self._container_items(text, item["start"], item["end"])
        if not inner_items or inner_items[0]["kind"] != "atom":
            return None
        return inner_items[0]["text"]

    def _name_from_target(
        self,
        text: str,
        item: dict[str, Any] | None,
    ) -> tuple[str | None, dict[str, Any] | None]:
        if item is None:
            return None, None
        if item["kind"] == "atom":
            return item["text"], item
        if item["kind"] not in {"list", "vector", "map"}:
            return None, None
        inner_items = self._container_items(text, item["start"], item["end"])
        if not inner_items:
            return None, None
        first = inner_items[0]
        if first["kind"] != "atom":
            return None, None
        return first["text"], first

    def _container_items(self, text: str, start: int, end: int) -> list[dict[str, Any]]:
        items: list[dict[str, Any]] = []
        index = start + 1
        limit = max(start + 1, end - 1)
        while index < limit:
            index = self._skip_ws_comments(text, index, limit)
            if index >= limit:
                break
            item = self._read_item(text, index, limit)
            if item is None:
                break
            items.append(item)
            index = item["end"]
        return items

    def _skip_ws_comments(self, text: str, index: int, limit: int) -> int:
        while index < limit:
            char = text[index]
            if char.isspace():
                index += 1
                continue
            if char == ";":
                index = self._skip_comment(text, index)
                continue
            break
        return index

    def _skip_comment(self, text: str, index: int) -> int:
        while index < len(text) and text[index] != "\n":
            index += 1
        return index

    def _scan_string(self, text: str, index: int) -> int:
        index += 1
        escaped = False
        while index < len(text):
            char = text[index]
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == '"':
                return index + 1
            index += 1
        return len(text)

    def _scan_balanced(self, text: str, index: int, limit: int) -> int:
        opener = text[index]
        stack = [FORM_PAIRS[opener]]
        index += 1
        while index < limit:
            char = text[index]
            if char == ";":
                index = self._skip_comment(text, index)
                continue
            if char == '"':
                index = self._scan_string(text, index)
                continue
            if char in FORM_PAIRS:
                stack.append(FORM_PAIRS[char])
                index += 1
                continue
            if stack and char == stack[-1]:
                stack.pop()
                index += 1
                if not stack:
                    return index
                continue
            index += 1
        return limit

    def _read_item(self, text: str, index: int, limit: int) -> dict[str, Any] | None:
        if index >= limit:
            return None
        char = text[index]
        if char in FORM_PAIRS:
            end = self._scan_balanced(text, index, limit)
            kind = {"(": "list", "[": "vector", "{": "map"}[char]
            return {"kind": kind, "text": text[index:end], "start": index, "end": end}
        if char == '"':
            end = self._scan_string(text, index)
            return {"kind": "string", "text": text[index:end], "start": index, "end": end}

        end = index
        while end < limit:
            current = text[end]
            if current.isspace() or current in "()[]{}" or current == ";":
                break
            end += 1
        return {"kind": "atom", "text": text[index:end], "start": index, "end": end}

    def _offset_range(self, text: str, start: int, end: int) -> dict[str, Any]:
        start_pos = self._offset_to_position(text, start)
        end_pos = self._offset_to_position(text, max(start, end))
        return {"start": start_pos, "end": end_pos}

    def _offset_to_position(self, text: str, offset: int) -> dict[str, int]:
        offset = max(0, min(offset, len(text)))
        line = text.count("\n", 0, offset)
        last_newline = text.rfind("\n", 0, offset)
        if last_newline == -1:
            character = offset
        else:
            character = offset - last_newline - 1
        return {"line": line, "character": character}

    def _position_to_offset(self, text: str, line_number: int, character: int) -> int:
        if line_number <= 0:
            return max(0, min(character, len(text)))

        offset = 0
        current_line = 0
        while current_line < line_number and offset < len(text):
            next_newline = text.find("\n", offset)
            if next_newline == -1:
                return len(text)
            offset = next_newline + 1
            current_line += 1

        line_end = text.find("\n", offset)
        if line_end == -1:
            line_end = len(text)
        return min(offset + max(character, 0), line_end)

    def _declaration_at_offset(self, text: str, offset: int) -> Declaration | None:
        best: Declaration | None = None
        best_size: int | None = None
        for declaration in self._flatten_declarations(self._scan_top_level_declarations(text)):
            if declaration.start <= offset < declaration.end:
                size = declaration.end - declaration.start
                if best is None or best_size is None or size < best_size:
                    best = declaration
                    best_size = size
        return best

    def _command_env(self) -> dict[str, str]:
        env = os.environ.copy()
        if not OMNI_LD_LIBRARY_PATH:
            return env
        current = env.get("LD_LIBRARY_PATH")
        if current:
            env["LD_LIBRARY_PATH"] = f"{OMNI_LD_LIBRARY_PATH}:{current}"
        else:
            env["LD_LIBRARY_PATH"] = OMNI_LD_LIBRARY_PATH
        return env

    def _symbol_at(self, text: str, line_number: int, character: int) -> str | None:
        symbol, _ = self._symbol_with_range_at(text, line_number, character)
        return symbol

    def _symbol_with_range_at_offset(
        self,
        text: str,
        offset: int,
    ) -> tuple[str | None, dict[str, Any] | None]:
        position = self._offset_to_position(text, offset)
        return self._symbol_with_range_at(
            text,
            int(position["line"]),
            int(position["character"]),
        )

    def _symbol_with_range_at(
        self,
        text: str,
        line_number: int,
        character: int,
    ) -> tuple[str | None, dict[str, Any] | None]:
        lines = text.splitlines()
        if line_number < 0 or line_number >= len(lines):
            return None, None
        line = lines[line_number]
        if not line:
            return None, None

        if character >= len(line):
            character = len(line) - 1
        if character < 0:
            return None, None

        allowed = set("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_+-*/!?<>=./:")
        if line[character] not in allowed:
            if character > 0 and line[character - 1] in allowed:
                character -= 1
            else:
                return None, None

        start = character
        while start > 0 and line[start - 1] in allowed:
            start -= 1
        end = character
        while end + 1 < len(line) and line[end + 1] in allowed:
            end += 1
        return (
            line[start : end + 1],
            {
                "start": {"line": line_number, "character": start},
                "end": {"line": line_number, "character": end + 1},
            },
        )

    def _uri_to_path(self, uri: str) -> Path | None:
        prefix = "file://"
        if not uri.startswith(prefix):
            return None
        return Path(uri[len(prefix) :])

    def _read_message(self) -> dict[str, Any] | None:
        content_length = 0
        while True:
            header = sys.stdin.buffer.readline()
            if not header:
                return None
            if header in (b"\r\n", b"\n"):
                break
            name, _, value = header.partition(b":")
            if name.lower() == b"content-length":
                content_length = int(value.strip())

        if content_length <= 0:
            return None

        payload = sys.stdin.buffer.read(content_length)
        if not payload:
            return None
        return json.loads(payload.decode("utf-8"))

    def _reply(self, msg_id: Any, result: Any) -> None:
        self._send({"jsonrpc": "2.0", "id": msg_id, "result": result})

    def _notify(self, method: str, params: dict[str, Any]) -> None:
        self._send({"jsonrpc": "2.0", "method": method, "params": params})

    def _send(self, payload: dict[str, Any]) -> None:
        body = json.dumps(payload, separators=(",", ":"), ensure_ascii=True).encode("utf-8")
        header = f"Content-Length: {len(body)}\r\n\r\n".encode("ascii")
        sys.stdout.buffer.write(header)
        sys.stdout.buffer.write(body)
        sys.stdout.buffer.flush()
