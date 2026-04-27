#!/usr/bin/env python3
"""
Dialectic MCP entrypoint.

The service implementation lives in `scripts/dialectic_mcp/` so the same logic
can back both the CLI fallback and the MCP tool surface.
"""

from __future__ import annotations

import sys
import traceback
import urllib.error

from dialectic_mcp import AppConfig, DialecticEngine, build_mcp, make_parser, run_cli


def main() -> int:
    parser = make_parser()
    args = parser.parse_args()

    if args.mode == "cli" and not args.command:
        parser.print_help()
        return 2

    cfg = AppConfig.load(args.config or None)
    engine = DialecticEngine(cfg)

    if args.mode == "cli":
        return run_cli(engine, args)

    build_mcp(engine).run()
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except urllib.error.HTTPError as exc:
        print(f"HTTPError: {exc}", file=sys.stderr)
        if exc.fp is not None:
            try:
                print(exc.fp.read().decode("utf-8", errors="replace"), file=sys.stderr)
            except Exception:
                pass
        raise
    except Exception:
        traceback.print_exc()
        raise
