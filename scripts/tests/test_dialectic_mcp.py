from __future__ import annotations

import sys
import subprocess
import tempfile
import unittest
from contextlib import redirect_stdout
from io import StringIO
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from dialectic_mcp.audit_service import AuditService
from dialectic_mcp.cli import main as cli_main
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

    def test_cli_module_help_prints_usage_without_warning(self):
        stdout = StringIO()
        with self.assertRaises(SystemExit) as raised:
            with redirect_stdout(stdout):
                cli_main(["--help"])

        self.assertEqual(raised.exception.code, 0)
        self.assertIn("usage:", stdout.getvalue())

        repo_root = Path(__file__).resolve().parents[2]
        result = subprocess.run(
            [sys.executable, "-m", "scripts.dialectic_mcp.cli", "--help"],
            cwd=repo_root,
            check=False,
            capture_output=True,
            text=True,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("usage:", result.stdout)
        self.assertEqual(result.stderr, "")

    def test_cli_missing_command_does_not_create_default_database(self):
        repo_root = Path(__file__).resolve().parents[2]
        db_path = repo_root / "dialectic_mcp.sqlite3"
        wal_path = repo_root / "dialectic_mcp.sqlite3-wal"
        shm_path = repo_root / "dialectic_mcp.sqlite3-shm"
        for path in (db_path, wal_path, shm_path):
            path.unlink(missing_ok=True)

        commands = [
            [sys.executable, "-m", "scripts.dialectic_mcp.cli", "--mode", "cli"],
            [sys.executable, "scripts/dialectic_mcp_single.py", "--mode", "cli"],
        ]

        for command in commands:
            result = subprocess.run(
                command,
                cwd=repo_root,
                check=False,
                capture_output=True,
                text=True,
            )

            self.assertEqual(result.returncode, 2, result.stderr)
            self.assertIn("usage:", result.stdout)
            self.assertEqual(result.stderr, "")
            for path in (db_path, wal_path, shm_path):
                self.assertFalse(path.exists(), f"unexpected CLI artifact after {command}: {path}")


if __name__ == "__main__":
    unittest.main()
