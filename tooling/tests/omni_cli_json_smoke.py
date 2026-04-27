#!/usr/bin/env python3
from __future__ import annotations

import os
import subprocess
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
OMNI = ROOT / "build" / "main"
OMNI_BYTES = os.fsencode(str(OMNI))


def expect(condition: bool, message: str) -> None:
    if not condition:
        raise AssertionError(message)


def run_raw(*args: bytes) -> subprocess.CompletedProcess[bytes]:
    env = os.environ.copy()
    env.setdefault("LD_LIBRARY_PATH", "/usr/local/lib")
    return subprocess.run(
        [OMNI_BYTES, *args],
        capture_output=True,
        text=False,
        check=False,
        env=env,
    )


def expect_ascii_json(proc: subprocess.CompletedProcess[bytes], code: bytes, message_fragment: bytes | None = None) -> bytes:
    expect(proc.returncode == 1, f"expected rc=1, got {proc.returncode} with stdout={proc.stdout!r} stderr={proc.stderr!r}")
    expect(proc.stderr == b"", f"expected empty stderr, got {proc.stderr!r}")
    stdout = proc.stdout
    expect(stdout.startswith(b'{"ok":false,'), f"expected structured JSON error, got {stdout!r}")
    expect(code in stdout, f"expected code {code!r} in {stdout!r}")
    if message_fragment is not None:
        expect(message_fragment in stdout, f"expected message fragment {message_fragment!r} in {stdout!r}")
    expect(b"Error:" not in stdout, f"expected no plaintext error prefix in {stdout!r}")
    try:
        stdout.decode("ascii")
    except UnicodeDecodeError as exc:
        raise AssertionError(f"expected ASCII-safe JSON output, got undecodable bytes: {stdout!r}") from exc
    return stdout


def expect_escaped_byte(stdout: bytes, raw_byte: bytes = bytes([0xFF])) -> None:
    expect(raw_byte not in stdout, f"expected raw byte {raw_byte!r} to be escaped in {stdout!r}")
    expect(b"\\u00ff" in stdout, f"expected escaped byte marker in {stdout!r}")


def main() -> int:
    expect(OMNI.exists(), f"missing Omni binary at {OMNI}")

    invalid_path = b"/tmp/omni-json-smoke-" + bytes([0xFF]) + b"-missing.omni"

    check_proc = run_raw(b"--check", b"--json", invalid_path)
    check_stdout = expect_ascii_json(check_proc, b'"code":"io/file-not-found"', b"cannot read input file")
    expect_escaped_byte(check_stdout)

    eval_proc = run_raw(b"--eval", b"--json", b"(+ 1 2)", b"extra-" + bytes([0xFF]))
    eval_stdout = expect_ascii_json(eval_proc, b'"code":"cli/usage"', b"unexpected argument after --eval")
    expect_escaped_byte(eval_stdout)

    describe_proc = run_raw(b"--describe", b"--json", b"length", b"extra-" + bytes([0xFF]))
    describe_stdout = expect_ascii_json(describe_proc, b'"code":"cli/usage"', b"unexpected argument after --describe")
    expect_escaped_byte(describe_stdout)

    repl_proc = run_raw(b"--repl", b"--json", b"--project")
    expect_ascii_json(repl_proc, b'"code":"cli/usage"', b"REPL preload is not supported with --json")

    repl_unexpected_proc = run_raw(b"--repl", b"--json", b"extra-" + bytes([0xFF]))
    repl_unexpected_stdout = expect_ascii_json(repl_unexpected_proc, b'"code":"cli/usage"', b"unexpected argument after --repl")
    expect_escaped_byte(repl_unexpected_stdout)

    repl_server_usage_proc = run_raw(b"--repl-server")
    expect_ascii_json(repl_server_usage_proc, b'"code":"cli/usage"', b"usage: omni --repl-server")

    repl_server_unexpected_proc = run_raw(b"--repl-server", b"--stdio", b"extra-" + bytes([0xFF]))
    repl_server_unexpected_stdout = expect_ascii_json(repl_server_unexpected_proc, b'"code":"cli/usage"', b"unexpected argument after --repl-server")
    expect_escaped_byte(repl_server_unexpected_stdout)

    repl_server_socket_proc = run_raw(b"--repl-server", b"--socket", b"/proc/1/nope/omni-json.sock")
    expect_ascii_json(repl_server_socket_proc, b'"code":"io/listen-failed"', b"failed to listen on REPL socket")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
