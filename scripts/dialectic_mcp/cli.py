from __future__ import annotations

import argparse
import json
import os

from .engine import DialecticEngine


def make_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Dialectic MCP single-file skeleton")
    parser.add_argument("--config", default=os.getenv("DIALECTIC_MCP_CONFIG", ""), help="Path to JSON config")
    parser.add_argument(
        "--mode",
        choices=["mcp", "cli"],
        default="mcp",
        help="Run as MCP server (default) or local CLI",
    )

    sub = parser.add_subparsers(dest="command")

    new_session = sub.add_parser("new-session")
    new_session.add_argument("--title", required=True)
    new_session.add_argument("--text", required=True)

    run_role = sub.add_parser("run-role")
    run_role.add_argument("--session-id", required=True)
    run_role.add_argument("--role", required=True)
    run_role.add_argument("--prompt", required=True)

    run_cycle = sub.add_parser("run-cycle")
    run_cycle.add_argument("--session-id", required=True)
    run_cycle.add_argument("--thesis-prompt", required=True)
    run_cycle.add_argument("--antithesis-prompt", required=True)
    run_cycle.add_argument("--synthesis-prompt", required=True)

    return parser


def run_cli(engine: DialecticEngine, args: argparse.Namespace) -> int:
    if args.command == "new-session":
        session_id = engine.create_session(args.title, args.text)
        print(json.dumps({"session_id": session_id}, indent=2))
        return 0

    if args.command == "run-cycle":
        output = engine.run_cycle(
            session_id=args.session_id,
            thesis_prompt=args.thesis_prompt,
            antithesis_prompt=args.antithesis_prompt,
            synthesis_prompt=args.synthesis_prompt,
        )
        print(json.dumps(output, indent=2))
        return 0

    if args.command == "run-role":
        output = engine.generate_role_artifact(args.session_id, args.role, args.prompt)
        print(json.dumps(output, indent=2))
        return 0

    raise ValueError(f"unknown command: {args.command}")
