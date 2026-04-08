from __future__ import annotations

import sqlite3
from typing import Any

from .utils import compact_json, make_id, now_iso


class Store:
    def __init__(self, db_path: str):
        self.db_path = db_path
        self.conn = sqlite3.connect(db_path)
        self.conn.row_factory = sqlite3.Row
        self.conn.execute("PRAGMA journal_mode=WAL;")
        self.conn.execute("PRAGMA foreign_keys=ON;")
        self._init_schema()

    def _init_schema(self) -> None:
        cursor = self.conn.cursor()
        cursor.executescript(
            """
            CREATE TABLE IF NOT EXISTS sessions (
              id TEXT PRIMARY KEY,
              title TEXT NOT NULL,
              source_text TEXT NOT NULL,
              created_at TEXT NOT NULL
            );

            CREATE TABLE IF NOT EXISTS artifacts (
              id TEXT PRIMARY KEY,
              session_id TEXT NOT NULL,
              kind TEXT NOT NULL,
              role TEXT,
              text TEXT NOT NULL,
              metadata_json TEXT NOT NULL,
              created_at TEXT NOT NULL,
              FOREIGN KEY(session_id) REFERENCES sessions(id) ON DELETE CASCADE
            );

            CREATE TABLE IF NOT EXISTS llm_runs (
              id TEXT PRIMARY KEY,
              session_id TEXT NOT NULL,
              artifact_id TEXT,
              role TEXT NOT NULL,
              provider TEXT NOT NULL,
              model TEXT NOT NULL,
              prompt_text TEXT NOT NULL,
              response_text TEXT NOT NULL,
              latency_ms INTEGER,
              created_at TEXT NOT NULL,
              FOREIGN KEY(session_id) REFERENCES sessions(id) ON DELETE CASCADE,
              FOREIGN KEY(artifact_id) REFERENCES artifacts(id) ON DELETE SET NULL
            );

            CREATE TABLE IF NOT EXISTS citations (
              id TEXT PRIMARY KEY,
              session_id TEXT NOT NULL,
              title TEXT,
              url TEXT,
              excerpt TEXT,
              quality_score REAL,
              created_at TEXT NOT NULL,
              FOREIGN KEY(session_id) REFERENCES sessions(id) ON DELETE CASCADE
            );

            CREATE TABLE IF NOT EXISTS artifact_citations (
              artifact_id TEXT NOT NULL,
              citation_id TEXT NOT NULL,
              PRIMARY KEY (artifact_id, citation_id),
              FOREIGN KEY(artifact_id) REFERENCES artifacts(id) ON DELETE CASCADE,
              FOREIGN KEY(citation_id) REFERENCES citations(id) ON DELETE CASCADE
            );

            CREATE TABLE IF NOT EXISTS scores (
              id TEXT PRIMARY KEY,
              artifact_id TEXT NOT NULL,
              score_type TEXT NOT NULL,
              value REAL NOT NULL,
              rationale TEXT,
              created_at TEXT NOT NULL,
              FOREIGN KEY(artifact_id) REFERENCES artifacts(id) ON DELETE CASCADE
            );
            """
        )
        self.conn.commit()

    def create_session(self, title: str, source_text: str) -> str:
        session_id = make_id("sess")
        self.conn.execute(
            "INSERT INTO sessions(id,title,source_text,created_at) VALUES(?,?,?,?)",
            (session_id, title, source_text, now_iso()),
        )
        self.conn.commit()
        return session_id

    def get_session(self, session_id: str) -> sqlite3.Row:
        row = self.conn.execute("SELECT * FROM sessions WHERE id=?", (session_id,)).fetchone()
        if row is None:
            raise ValueError(f"unknown session_id: {session_id}")
        return row

    def add_artifact(
        self,
        session_id: str,
        kind: str,
        text: str,
        role: str | None = None,
        metadata: dict[str, Any] | None = None,
    ) -> str:
        artifact_id = make_id("art")
        self.conn.execute(
            "INSERT INTO artifacts(id,session_id,kind,role,text,metadata_json,created_at) VALUES(?,?,?,?,?,?,?)",
            (artifact_id, session_id, kind, role, text, compact_json(metadata or {}), now_iso()),
        )
        self.conn.commit()
        return artifact_id

    def get_artifact(self, artifact_id: str) -> sqlite3.Row:
        row = self.conn.execute("SELECT * FROM artifacts WHERE id=?", (artifact_id,)).fetchone()
        if row is None:
            raise ValueError(f"unknown artifact_id: {artifact_id}")
        return row

    def list_artifacts(self, session_id: str) -> list[dict[str, Any]]:
        rows = self.conn.execute(
            "SELECT id,kind,role,created_at FROM artifacts WHERE session_id=? ORDER BY created_at",
            (session_id,),
        ).fetchall()
        return [dict(row) for row in rows]

    def add_llm_run(
        self,
        session_id: str,
        role: str,
        provider: str,
        model: str,
        prompt_text: str,
        response_text: str,
        artifact_id: str | None,
        latency_ms: int | None,
    ) -> str:
        run_id = make_id("run")
        self.conn.execute(
            """
            INSERT INTO llm_runs(
              id,session_id,artifact_id,role,provider,model,prompt_text,response_text,latency_ms,created_at
            ) VALUES(?,?,?,?,?,?,?,?,?,?)
            """,
            (run_id, session_id, artifact_id, role, provider, model, prompt_text, response_text, latency_ms, now_iso()),
        )
        self.conn.commit()
        return run_id

    def add_score(self, artifact_id: str, score_type: str, value: float, rationale: str) -> str:
        score_id = make_id("score")
        self.conn.execute(
            "INSERT INTO scores(id,artifact_id,score_type,value,rationale,created_at) VALUES(?,?,?,?,?,?)",
            (score_id, artifact_id, score_type, float(value), rationale, now_iso()),
        )
        self.conn.commit()
        return score_id
