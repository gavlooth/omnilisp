#!/usr/bin/env python3
from __future__ import annotations

import os
import subprocess
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
OMNI = ROOT / "build" / "main"


def expect(condition: bool, message: str) -> None:
    if not condition:
        raise AssertionError(message)


def run_init(cwd: Path, project_name: str, **extra_env: str) -> subprocess.CompletedProcess[str]:
    env = os.environ.copy()
    env.setdefault("LD_LIBRARY_PATH", "/usr/local/lib")
    env.update(extra_env)
    return subprocess.run(
        [str(OMNI), "--init", project_name],
        capture_output=True,
        text=True,
        check=False,
        env=env,
        cwd=cwd,
    )


def expect_failed_cleanup(proc: subprocess.CompletedProcess[str], target: Path, message_fragment: str) -> None:
    expect(proc.returncode == 1, f"expected rc=1, got {proc.returncode} with stdout={proc.stdout!r} stderr={proc.stderr!r}")
    expect(proc.stderr == "", f"expected empty stderr, got {proc.stderr!r}")
    expect(message_fragment in proc.stdout, f"expected {message_fragment!r} in stdout={proc.stdout!r}")
    expect(not target.exists(), f"expected rollback to remove partial project tree at {target}")


def main() -> int:
    expect(OMNI.exists(), f"missing Omni binary at {OMNI}")

    with tempfile.TemporaryDirectory(prefix="omni-init-smoke-") as tmp:
        base = Path(tmp)

        collision_target = base / "collision-proj"
        collision_proc = run_init(
            base,
            collision_target.name,
            OMNI_INIT_TEST_FILE_COLLISION_SUFFIX="build",
        )
        expect_failed_cleanup(collision_proc, collision_target, "non-directory already exists")
        expect(f"{collision_target.name}/build" in collision_proc.stdout, f"expected build path in stdout={collision_proc.stdout!r}")

        write_fail_target = base / "write-fail-proj"
        write_fail_proc = run_init(
            base,
            write_fail_target.name,
            OMNI_INIT_TEST_FAIL_WRITE_SUFFIX="build/project.json",
        )
        expect_failed_cleanup(write_fail_proc, write_fail_target, "Error: cannot write")
        expect(
            f"{write_fail_target.name}/build/project.json" in write_fail_proc.stdout,
            f"expected project.json path in stdout={write_fail_proc.stdout!r}",
        )

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
