from __future__ import annotations

import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from dialectic_mcp.audit_service import AuditService
from dialectic_mcp.config import AppConfig
from dialectic_mcp.engine import DialecticEngine


class FakeLLMService:
    def run_role(self, role, user_prompt, context_text, override=None):
        return {
            "role": role,
            "provider": "fake",
            "model": "fake-model",
            "temperature": 0.0,
            "text": f"{role}:{user_prompt}|{context_text}",
            "latency_ms": 1,
            "system_prompt": "",
        }


class DialecticMcpServiceTests(unittest.TestCase):
    def test_run_cycle_uses_services_and_persists_artifacts(self):
        with tempfile.TemporaryDirectory() as tempdir:
            cfg = AppConfig.load(None)
            cfg.database_path = f"{tempdir}/dialectic.sqlite3"

            engine = DialecticEngine(
                cfg,
                llm_service=FakeLLMService(),
                audit_service=AuditService(),
            )
            session_id = engine.create_session("review", "source material")

            result = engine.run_cycle(
                session_id=session_id,
                thesis_prompt="build thesis",
                antithesis_prompt="build antithesis",
                synthesis_prompt="build synthesis",
            )

            self.assertIn("thesis", result)
            self.assertIn("antithesis", result)
            self.assertIn("synthesis", result)
            self.assertEqual(len(engine.list_artifacts(session_id)), 3)
            self.assertGreaterEqual(result["confidence"], 0.0)
            self.assertLessEqual(result["confidence"], 1.0)


if __name__ == "__main__":
    unittest.main()
