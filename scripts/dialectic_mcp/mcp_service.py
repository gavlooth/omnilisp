from __future__ import annotations

import json
from typing import Any

from .audit_service import quick_fallacy_audit, quick_self_reflection
from .engine import DialecticEngine


class McpServerService:
    def __init__(self, engine: DialecticEngine):
        self.engine = engine

    def build(self):
        try:
            from mcp.server.fastmcp import FastMCP
        except Exception as exc:
            raise RuntimeError(
                "mcp.server.fastmcp is not available. Install an MCP Python SDK or use --cli mode."
            ) from exc

        mcp = FastMCP("dialectic-critique")

        @mcp.tool()
        def create_session(title: str, source_text: str) -> dict[str, Any]:
            session_id = self.engine.create_session(title, source_text)
            return {"session_id": session_id}

        @mcp.tool()
        def list_artifacts(session_id: str) -> dict[str, Any]:
            return {"items": self.engine.list_artifacts(session_id)}

        @mcp.tool()
        def run_role(session_id: str, role: str, prompt: str, override_json: str = "") -> dict[str, Any]:
            override = json.loads(override_json) if override_json.strip() else None
            return self.engine.generate_role_artifact(session_id, role, prompt, override)

        @mcp.tool()
        def run_cycle(
            session_id: str,
            thesis_prompt: str,
            antithesis_prompt: str,
            synthesis_prompt: str,
            overrides_json: str = "",
        ) -> dict[str, Any]:
            overrides = json.loads(overrides_json) if overrides_json.strip() else None
            return self.engine.run_cycle(session_id, thesis_prompt, antithesis_prompt, synthesis_prompt, overrides)

        @mcp.tool()
        def audit_fallacies(text: str) -> dict[str, Any]:
            return quick_fallacy_audit(text)

        @mcp.tool()
        def reflect_critique(text: str) -> dict[str, Any]:
            return quick_self_reflection(text)

        return mcp


def build_mcp(engine: DialecticEngine):
    return McpServerService(engine).build()
