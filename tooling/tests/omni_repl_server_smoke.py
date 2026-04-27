#!/usr/bin/env python3
from __future__ import annotations

import json
import os
import queue
import subprocess
import threading
import time
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
OMNI = ROOT / "build" / "main"


def expect(condition: bool, message: str) -> None:
    if not condition:
        raise AssertionError(message)


def start_server() -> subprocess.Popen[str]:
    env = os.environ.copy()
    env.setdefault("LD_LIBRARY_PATH", "/usr/local/lib")
    return subprocess.Popen(
        [str(OMNI), "--repl-server", "--stdio"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        env=env,
    )


def reader_thread(stream, sink: queue.Queue[dict | str], *, parse_json: bool) -> None:
    for line in stream:
        line = line.rstrip("\n")
        if parse_json:
            sink.put(json.loads(line))
        else:
            sink.put(line)


def send(proc: subprocess.Popen[str], payload: dict) -> None:
    expect(proc.stdin is not None, "missing stdin pipe")
    proc.stdin.write(json.dumps(payload) + "\n")
    proc.stdin.flush()


def read_until(q: queue.Queue[dict], predicate, timeout: float = 3.0) -> list[dict]:
    deadline = time.time() + timeout
    seen: list[dict] = []
    while time.time() < deadline:
        remaining = deadline - time.time()
        try:
            item = q.get(timeout=max(remaining, 0.01))
        except queue.Empty:
            continue
        seen.append(item)
        if predicate(item, seen):
            return seen
    raise AssertionError(f"timeout waiting for expected event; seen={seen!r}")


def main() -> int:
    expect(OMNI.exists(), f"missing Omni binary at {OMNI}")
    proc = start_server()
    expect(proc.stdout is not None, "missing stdout pipe")
    expect(proc.stderr is not None, "missing stderr pipe")

    out_q: queue.Queue[dict] = queue.Queue()
    err_q: queue.Queue[str] = queue.Queue()
    out_thread = threading.Thread(target=reader_thread, args=(proc.stdout, out_q), kwargs={"parse_json": True}, daemon=True)
    err_thread = threading.Thread(target=reader_thread, args=(proc.stderr, err_q), kwargs={"parse_json": False}, daemon=True)
    out_thread.start()
    err_thread.start()

    try:
        send(proc, {"id": "1", "op": "clone"})
        clone_events = read_until(out_q, lambda item, seen: item.get("id") == "1" and item.get("event") == "done")
        session_events = [item for item in clone_events if item.get("id") == "1" and item.get("event") == "session"]
        expect(len(session_events) == 1, f"expected one session event for clone, got {clone_events!r}")
        session_id = session_events[0]["session"]

        send(proc, {"id": "2", "op": "eval", "session": session_id, "code": "(read-line)", "mode": "expr"})
        time.sleep(0.05)

        send(proc, {"id": "3", "op": "clone"})
        clone_busy = read_until(out_q, lambda item, seen: item.get("id") == "3")
        busy_event = next(item for item in clone_busy if item.get("id") == "3")
        expect(busy_event.get("event") == "error", f"expected clone busy error, got {busy_event!r}")
        expect(busy_event.get("error", {}).get("code") == "protocol/server-busy", f"expected protocol/server-busy, got {busy_event!r}")

        send(proc, {"id": "4", "op": "stdin", "session": session_id, "target": "2", "data": "wake\n"})
        stdin_done = read_until(out_q, lambda item, seen: item.get("id") == "4" and item.get("event") == "done")
        expect(any(item.get("id") == "4" and item.get("event") == "done" for item in stdin_done), f"expected stdin done, got {stdin_done!r}")

        eval_events = read_until(out_q, lambda item, seen: item.get("id") == "2" and item.get("event") == "done")
        expect(any(item.get("id") == "2" and item.get("event") == "value" and item.get("value") == '"wake"' for item in eval_events), f"expected read-line eval value, got {eval_events!r}")

        send(proc, {"id": "5", "op": "clone"})
        second_clone = read_until(out_q, lambda item, seen: item.get("id") == "5" and item.get("event") == "done")
        second_session = [item for item in second_clone if item.get("id") == "5" and item.get("event") == "session"]
        expect(len(second_session) == 1, f"expected second clone session event, got {second_clone!r}")
        expect(second_session[0].get("session") != session_id, f"expected distinct second session, got {second_clone!r}")

    finally:
        if proc.stdin is not None:
            proc.stdin.close()
        try:
            proc.wait(timeout=3)
        except subprocess.TimeoutExpired:
            proc.kill()
            proc.wait(timeout=3)

    stderr_lines: list[str] = []
    while True:
        try:
            stderr_lines.append(err_q.get_nowait())
        except queue.Empty:
            break
    expect(not stderr_lines, f"expected empty stderr, got {stderr_lines!r}")
    expect(proc.returncode == 0, f"expected rc=0, got {proc.returncode}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
