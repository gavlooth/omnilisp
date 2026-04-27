#!/usr/bin/env python3
from __future__ import annotations

import argparse

from smoke_helpers import open_server
from smoke_scenarios import run_smoke_scenarios


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Run the Omni LSP protocol smoke test.")
    parser.parse_args(argv)

    proc = open_server()
    try:
        return run_smoke_scenarios(proc)
    finally:
        if proc.poll() is None:
            proc.terminate()
            proc.wait(timeout=2)


if __name__ == "__main__":
    raise SystemExit(main())
