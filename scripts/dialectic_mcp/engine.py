from __future__ import annotations

from typing import Any

from .audit_service import AuditService
from .config import AppConfig
from .llm_service import LLMService
from .store import Store


class DialecticEngine:
    def __init__(
        self,
        cfg: AppConfig,
        *,
        store: Store | None = None,
        llm_service: LLMService | None = None,
        audit_service: AuditService | None = None,
    ):
        self.cfg = cfg
        self.store = store or Store(cfg.database_path)
        self.llm = llm_service or LLMService(cfg)
        self.audit = audit_service or AuditService()

    def create_session(self, title: str, source_text: str) -> str:
        return self.store.create_session(title, source_text)

    def list_artifacts(self, session_id: str) -> list[dict[str, Any]]:
        return self.store.list_artifacts(session_id)

    def generate_role_artifact(
        self,
        session_id: str,
        role: str,
        prompt: str,
        override: dict[str, Any] | None = None,
    ) -> dict[str, Any]:
        session = self.store.get_session(session_id)
        out = self.llm.run_role(role, prompt, session["source_text"], override=override)
        artifact_id = self.store.add_artifact(
            session_id=session_id,
            kind="dialectic_output",
            role=role,
            text=out["text"],
            metadata={
                "provider": out["provider"],
                "model": out["model"],
                "temperature": out["temperature"],
            },
        )
        self.store.add_llm_run(
            session_id=session_id,
            role=role,
            provider=out["provider"],
            model=out["model"],
            prompt_text=prompt,
            response_text=out["text"],
            artifact_id=artifact_id,
            latency_ms=out.get("latency_ms"),
        )
        return {
            "artifact_id": artifact_id,
            "role": role,
            "text": out["text"],
            "provider": out["provider"],
            "model": out["model"],
            "latency_ms": out.get("latency_ms"),
        }

    def run_cycle(
        self,
        session_id: str,
        thesis_prompt: str,
        antithesis_prompt: str,
        synthesis_prompt: str,
        overrides: dict[str, dict[str, Any]] | None = None,
    ) -> dict[str, Any]:
        overrides = overrides or {}
        thesis = self.generate_role_artifact(session_id, "thesis", thesis_prompt, overrides.get("thesis"))
        antithesis = self.generate_role_artifact(
            session_id, "antithesis", antithesis_prompt, overrides.get("antithesis")
        )

        synthesis_task = (
            synthesis_prompt
            + "\n\nUse this thesis:\n"
            + thesis["text"]
            + "\n\nUse this antithesis:\n"
            + antithesis["text"]
        )
        synthesis = self.generate_role_artifact(
            session_id, "synthesis", synthesis_task, overrides.get("synthesis")
        )

        fallacy_audit = self.audit.audit_fallacies(antithesis["text"])
        self_reflection = self.audit.reflect_critique(antithesis["text"])
        confidence = self.audit.confidence(cited_claim_ratio=0.5, structure_score=0.7, calibration_factor=0.6)
        self.store.add_score(antithesis["artifact_id"], "confidence", confidence, "heuristic composite")

        return {
            "thesis": thesis,
            "antithesis": antithesis,
            "synthesis": synthesis,
            "fallacy_audit": fallacy_audit,
            "self_reflection": self_reflection,
            "confidence": confidence,
        }
