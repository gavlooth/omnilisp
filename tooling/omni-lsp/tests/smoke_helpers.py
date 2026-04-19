from __future__ import annotations

import json
import os
import subprocess
from pathlib import Path
from typing import Any


ROOT = Path(__file__).resolve().parents[3]
SERVER = ROOT / "tooling" / "omni-lsp" / "omni_lsp.py"


def send_message(proc: subprocess.Popen[bytes], payload: dict[str, Any]) -> None:
    body = json.dumps(payload, separators=(",", ":"), ensure_ascii=True).encode("utf-8")
    proc.stdin.write(f"Content-Length: {len(body)}\r\n\r\n".encode("ascii"))
    proc.stdin.write(body)
    proc.stdin.flush()


def read_message(proc: subprocess.Popen[bytes]) -> dict[str, Any]:
    headers: dict[bytes, bytes] = {}
    while True:
        line = proc.stdout.readline()
        if not line:
            stderr = proc.stderr.read().decode("utf-8", errors="replace")
            raise RuntimeError(f"omni-lsp exited early: {stderr}")
        if line in (b"\r\n", b"\n"):
            break
        name, _, value = line.partition(b":")
        headers[name.lower()] = value.strip()
    content_length = int(headers[b"content-length"])
    payload = proc.stdout.read(content_length)
    return json.loads(payload.decode("utf-8"))


def expect(condition: bool, message: str) -> None:
    if not condition:
        raise AssertionError(message)


def decode_semantic_tokens(
    data: list[int],
    token_types: list[str],
    token_modifiers: list[str],
) -> list[dict[str, Any]]:
    tokens: list[dict[str, Any]] = []
    line = 0
    start = 0
    for index in range(0, len(data), 5):
        delta_line, delta_start, length, token_type_index, modifier_bits = data[index : index + 5]
        line += delta_line
        start = delta_start if delta_line else start + delta_start
        modifiers = [modifier for bit, modifier in enumerate(token_modifiers) if modifier_bits & (1 << bit)]
        tokens.append({"line": line, "start": start, "length": length, "type": token_types[token_type_index], "modifiers": modifiers})
    return tokens


def open_server() -> subprocess.Popen[bytes]:
    env = os.environ.copy()
    build_main = ROOT / "build" / "main"
    if build_main.exists():
        env.setdefault("OMNI_LSP_OMNI_BIN", str(build_main))
        env.setdefault("OMNI_LSP_LD_LIBRARY_PATH", "/usr/local/lib")
    return subprocess.Popen(
        ["python3", str(SERVER)],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        env=env,
    )
