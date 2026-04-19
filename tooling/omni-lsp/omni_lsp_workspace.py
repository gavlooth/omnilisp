from omni_lsp_shared import *


class WorkspaceMixin:
    def _document_symbols(self, params: dict[str, Any]) -> list[dict[str, Any]]:
        document_uri = (params.get("textDocument") or {}).get("uri")
        document = self.documents.get(document_uri)
        if document is None:
            return []

        symbols: list[dict[str, Any]] = []
        for declaration in self._scan_top_level_declarations(document.text):
            symbols.append(self._declaration_to_document_symbol(document.text, declaration))
        return symbols

    def _document_diagnostic_report(self, params: dict[str, Any]) -> dict[str, Any]:
        text_document = params["textDocument"]
        document = self.documents.get(text_document["uri"])
        if document is None:
            return {"kind": "full", "items": [], "resultId": "missing"}
        previous_result_id = params.get("previousResultId")
        return self._document_diagnostic_payload(document, previous_result_id)

    def _workspace_diagnostic_report(self, params: dict[str, Any]) -> dict[str, Any]:
        previous_result_ids = {
            str(item.get("uri")): str(item.get("value"))
            for item in (params.get("previousResultIds") or [])
            if isinstance(item, dict) and isinstance(item.get("uri"), str)
        }
        items = [
            self._workspace_document_diagnostic_payload(
                document,
                previous_result_ids.get(document.uri),
            )
            for document in self._workspace_documents()
        ]
        return {"items": items}

    def _document_diagnostic_payload(
        self,
        document: Document,
        previous_result_id: str | None = None,
    ) -> dict[str, Any]:
        result_id = self._diagnostic_result_id(document)
        if previous_result_id == result_id:
            return {"kind": "unchanged", "resultId": result_id}
        return {
            "kind": "full",
            "resultId": result_id,
            "items": self._compute_diagnostics(document),
        }

    def _workspace_document_diagnostic_payload(
        self,
        document: Document,
        previous_result_id: str | None = None,
    ) -> dict[str, Any]:
        payload = self._document_diagnostic_payload(document, previous_result_id)
        payload["uri"] = document.uri
        payload["version"] = document.version
        return payload

    def _diagnostic_result_id(self, document: Document) -> str:
        digest = hashlib.sha1(document.text.encode("utf-8")).hexdigest()[:16]
        version = document.version if document.version is not None else "disk"
        return f"{version}:{digest}"

    def _workspace_documents(self) -> list[Document]:
        documents = list(self.documents.values())
        seen_paths = {
            document.path.resolve()
            for document in documents
            if document.path is not None and document.path.exists()
        }
        for root in self._workspace_roots():
            for path in self._workspace_omni_files(root):
                resolved = path.resolve()
                if resolved in seen_paths:
                    continue
                text = self._workspace_text_for_path(resolved)
                if text is None:
                    continue
                seen_paths.add(resolved)
                documents.append(
                    Document(
                        uri=resolved.as_uri(),
                        text=text,
                        version=None,
                        path=resolved,
                    )
                )
        documents.sort(key=lambda document: document.uri)
        return documents

    def _monikers(self, params: dict[str, Any]) -> list[dict[str, Any]]:
        text_document = params["textDocument"]
        position = params["position"]
        document = self.documents.get(text_document["uri"])
        if document is None:
            return []

        symbol, symbol_range = self._symbol_with_range_at(
            document.text,
            int(position["line"]),
            int(position["character"]),
        )
        if not symbol or symbol_range is None:
            return []

        local_records = [
            record
            for record in self._workspace_symbol_records_from_document(document)
            if record.name == symbol
        ]
        local_exact = [
            record
            for record in local_records
            if record.range == symbol_range
        ]
        if local_exact:
            return [self._moniker_from_record(record, "export") for record in local_exact]
        if local_records:
            return [self._moniker_from_record(record, "local") for record in local_records]

        workspace_records = self._workspace_definition_records(symbol, document.uri)
        return [self._moniker_from_record(record, "import") for record in workspace_records]

    def _moniker_from_record(
        self,
        record: WorkspaceSymbolRecord,
        kind: str,
    ) -> dict[str, Any]:
        return {
            "scheme": "omni",
            "identifier": self._moniker_identifier(record),
            "unique": "project",
            "kind": kind,
        }

    def _moniker_identifier(self, record: WorkspaceSymbolRecord) -> str:
        parts = []
        if record.container_name:
            parts.append(record.container_name)
        parts.append(record.detail)
        parts.append(record.name)
        return ":".join(parts)

    def _workspace_symbols(self, params: dict[str, Any]) -> list[dict[str, Any]]:
        query = str(params.get("query", "")).strip().lower()
        symbols: list[dict[str, Any]] = []

        for document in self.documents.values():
            symbols.extend(self._document_workspace_symbols(document, query))
            if len(symbols) >= WORKSPACE_SYMBOL_RESULT_LIMIT:
                return symbols[:WORKSPACE_SYMBOL_RESULT_LIMIT]

        seen_paths = {
            document.path.resolve()
            for document in self.documents.values()
            if document.path is not None and document.path.exists()
        }
        for root in self._workspace_roots():
            for path, records in self._cached_workspace_symbol_records(root).items():
                if path in seen_paths:
                    continue
                for record in records:
                    if query and query not in record.name_lower:
                        continue
                    symbols.append(self._workspace_symbol_record_payload(record))
                    if len(symbols) >= WORKSPACE_SYMBOL_RESULT_LIMIT:
                        return symbols[:WORKSPACE_SYMBOL_RESULT_LIMIT]
        return symbols

    def _prepare_call_hierarchy(self, params: dict[str, Any]) -> list[dict[str, Any]] | None:
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

        records = self._call_hierarchy_definition_records(symbol, symbol_range, document)
        if not records:
            return None
        return [self._call_hierarchy_item(record) for record in records]

    def _document_workspace_symbols(
        self,
        document: Document,
        query: str,
    ) -> list[dict[str, Any]]:
        symbols: list[dict[str, Any]] = []
        for record in self._workspace_symbol_records_from_document(document):
            if query and query not in record.name_lower:
                continue
            symbols.append(self._workspace_symbol_record_payload(record))
        return symbols

    def _cached_workspace_symbol_records(self, root: Path) -> dict[Path, list[WorkspaceSymbolRecord]]:
        resolved_root = root.resolve()
        manifest = self._workspace_omni_manifest(root)
        cached = self.workspace_document_cache.get(resolved_root)
        if cached is not None and cached.manifest == manifest:
            return cached.symbol_records

        symbol_records: dict[Path, list[WorkspaceSymbolRecord]] = {}
        declaration_records: dict[Path, list[WorkspaceDeclarationRecord]] = {}
        previous_records = cached.symbol_records if cached is not None else {}
        previous_declarations = cached.declaration_records if cached is not None else {}
        previous_manifest = cached.manifest if cached is not None else {}

        for path, signature in manifest.items():
            previous = previous_records.get(path)
            previous_declaration_set = previous_declarations.get(path)
            if (
                previous is not None
                and previous_declaration_set is not None
                and previous_manifest.get(path) == signature
            ):
                symbol_records[path] = previous
                declaration_records[path] = previous_declaration_set
                continue
            try:
                text = path.read_text(encoding="utf-8")
            except (OSError, UnicodeDecodeError):
                continue
            records = self._workspace_records_from_text(path.resolve().as_uri(), text)
            symbol_records[path] = records[0]
            declaration_records[path] = records[1]
        entry = WorkspaceCacheEntry(
            symbol_records=symbol_records,
            declaration_records=declaration_records,
            manifest=manifest,
        )
        self.workspace_document_cache[resolved_root] = entry
        return symbol_records

    def _cached_workspace_declaration_records(
        self,
        root: Path,
    ) -> dict[Path, list[WorkspaceDeclarationRecord]]:
        resolved_root = root.resolve()
        cached = self.workspace_document_cache.get(resolved_root)
        if cached is None or cached.manifest != self._workspace_omni_manifest(root):
            self._cached_workspace_symbol_records(root)
            cached = self.workspace_document_cache.get(resolved_root)
        if cached is None:
            return {}
        return cached.declaration_records

    def _invalidate_workspace_cache(self) -> None:
        self.workspace_document_cache.clear()

    def _workspace_roots(self) -> list[Path]:
        roots: list[Path] = []
        seen: set[Path] = set()
        for document in self.documents.values():
            path = document.path
            if path is None:
                continue
            root = self._workspace_root_for_path(path)
            if root is None:
                continue
            resolved = root.resolve()
            if resolved in seen:
                continue
            seen.add(resolved)
            roots.append(root)
        return roots

    def _workspace_root_for_path(self, path: Path) -> Path | None:
        try:
            current = path.resolve()
        except OSError:
            return None
        if current.is_file():
            current = current.parent
        for candidate in [current, *current.parents]:
            for marker in WORKSPACE_ROOT_MARKERS:
                if (candidate / marker).exists():
                    return candidate
        return None

    def _workspace_omni_files(self, root: Path) -> list[Path]:
        files: list[Path] = []
        try:
            for current_root, dirnames, filenames in os.walk(root):
                dirnames[:] = [
                    dirname
                    for dirname in dirnames
                    if dirname not in WORKSPACE_SKIP_DIRS and not dirname.startswith(".")
                ]
                current_path = Path(current_root)
                for filename in filenames:
                    if not filename.endswith(".omni"):
                        continue
                    files.append(current_path / filename)
        except OSError:
            return []
        files.sort()
        return files

    def _workspace_omni_manifest(self, root: Path) -> dict[Path, tuple[int, int]]:
        manifest: dict[Path, tuple[int, int]] = {}
        for path in self._workspace_omni_files(root):
            try:
                stat = path.stat()
            except OSError:
                continue
            manifest[path.resolve()] = (stat.st_mtime_ns, stat.st_size)
        return manifest

    def _workspace_symbol_records_from_document(self, document: Document) -> list[WorkspaceSymbolRecord]:
        return self._workspace_symbol_records_from_text(document.uri, document.text)

    def _workspace_symbol_records_from_text(self, uri: str, text: str) -> list[WorkspaceSymbolRecord]:
        return self._workspace_records_from_text(uri, text)[0]

    def _workspace_declaration_records_from_document(
        self,
        document: Document,
    ) -> list[WorkspaceDeclarationRecord]:
        return self._workspace_declaration_records_from_text(document.uri, document.text)

    def _workspace_declaration_records_from_text(
        self,
        uri: str,
        text: str,
    ) -> list[WorkspaceDeclarationRecord]:
        return self._workspace_records_from_text(uri, text)[1]

    def _workspace_records_from_text(
        self,
        uri: str,
        text: str,
    ) -> tuple[list[WorkspaceSymbolRecord], list[WorkspaceDeclarationRecord]]:
        records: list[WorkspaceSymbolRecord] = []
        declaration_records: list[WorkspaceDeclarationRecord] = []
        for declaration, container_name in self._flatten_declarations_with_containers(self._scan_top_level_declarations(text)):
            parameter_labels = self._declaration_parameter_labels(
                text,
                self._container_items(text, declaration.start, declaration.end),
            )
            records.append(
                WorkspaceSymbolRecord(
                    name=declaration.name,
                    name_lower=declaration.name.lower(),
                    detail=declaration.detail,
                    kind=declaration.kind,
                    uri=uri,
                    range=self._offset_range(
                        text,
                        declaration.selection_start,
                        declaration.selection_end,
                    ),
                    container_name=container_name,
                    snippet=self._declaration_snippet(text, declaration),
                    parameter_labels=parameter_labels,
                )
            )
            declaration_records.append(
                WorkspaceDeclarationRecord(
                    name=declaration.name,
                    name_lower=declaration.name.lower(),
                    detail=declaration.detail,
                    kind=declaration.kind,
                    uri=uri,
                    range=self._offset_range(text, declaration.start, declaration.end),
                    selection_range=self._offset_range(
                        text,
                        declaration.selection_start,
                        declaration.selection_end,
                    ),
                    container_name=container_name,
                    snippet=self._declaration_snippet(text, declaration),
                    parameter_labels=parameter_labels,
                    call_sites=self._declaration_call_sites(text, declaration),
                )
            )
        return records, declaration_records

    def _workspace_symbol_record_payload(self, record: WorkspaceSymbolRecord) -> dict[str, Any]:
        return {
            "name": record.name,
            "kind": record.kind,
            "location": {
                "uri": record.uri,
                "range": record.range,
            },
            "containerName": record.container_name,
        }

    def _workspace_record_key(self, record: WorkspaceSymbolRecord) -> tuple[str, int, int, int, int]:
        start = record.range["start"]
        end = record.range["end"]
        return (
            record.uri,
            int(start["line"]),
            int(start["character"]),
            int(end["line"]),
            int(end["character"]),
        )

    def _workspace_definition_records(
        self,
        symbol: str,
        current_uri: str,
    ) -> list[WorkspaceSymbolRecord]:
        symbol_lower = symbol.lower()
        records: list[WorkspaceSymbolRecord] = []
        seen: set[tuple[str, int, int, int, int]] = set()

        for document in self.documents.values():
            if document.uri == current_uri:
                continue
            for record in self._workspace_symbol_records_from_document(document):
                if record.name_lower != symbol_lower:
                    continue
                key = self._workspace_record_key(record)
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
            for path, path_records in self._cached_workspace_symbol_records(root).items():
                if path in open_paths:
                    continue
                for record in path_records:
                    if record.name_lower != symbol_lower:
                        continue
                    key = self._workspace_record_key(record)
                    if key in seen:
                        continue
                    seen.add(key)
                    records.append(record)

        return records

    def _call_hierarchy_definition_records(
        self,
        symbol: str,
        symbol_range: dict[str, Any],
        document: Document,
    ) -> list[WorkspaceDeclarationRecord]:
        local_matches = [record for record in self._call_hierarchy_symbol_records(symbol, document.uri) if record.uri == document.uri]
        local_exact = [
            record
            for record in local_matches
            if record.selection_range == symbol_range
        ]
        if local_exact:
            return local_exact
        local_callable = [record for record in local_matches if self._call_hierarchy_targetable(record)]
        if local_callable:
            return local_callable
        return [
            record
            for record in self._call_hierarchy_symbol_records(symbol, document.uri)
            if record.uri != document.uri and self._call_hierarchy_targetable(record)
        ]

    def _call_hierarchy_symbol_records(
        self,
        symbol: str,
        current_uri: str,
    ) -> list[WorkspaceDeclarationRecord]:
        symbol_lower = symbol.lower()
        records: list[WorkspaceDeclarationRecord] = []
        seen: set[tuple[str, int, int, int, int]] = set()
        for record in self._all_declaration_records():
            if record.name_lower != symbol_lower:
                continue
            key = self._declaration_record_key(record)
            if key in seen:
                continue
            seen.add(key)
            records.append(record)
        records.sort(
            key=lambda record: (
                0 if record.uri == current_uri else 1,
                record.name_lower,
                int(record.selection_range["start"]["line"]),
                int(record.selection_range["start"]["character"]),
            )
        )
        return records

    def _declaration_record_key(
        self,
        record: WorkspaceDeclarationRecord,
    ) -> tuple[str, int, int, int, int]:
        start = record.selection_range["start"]
        end = record.selection_range["end"]
        return (
            record.uri,
            int(start["line"]),
            int(start["character"]),
            int(end["line"]),
            int(end["character"]),
        )

    def _call_hierarchy_item(self, record: WorkspaceDeclarationRecord) -> dict[str, Any]:
        detail = DECLARATION_DETAIL_LABELS.get(record.detail, record.detail)
        if record.container_name:
            detail = f"{detail} in {record.container_name}"
        return {
            "name": record.name,
            "kind": record.kind,
            "detail": detail,
            "uri": record.uri,
            "range": record.range,
            "selectionRange": record.selection_range,
        }

    def _call_hierarchy_targetable(self, record: WorkspaceDeclarationRecord) -> bool:
        return record.detail not in NON_CALLABLE_DECLARATION_DETAILS
