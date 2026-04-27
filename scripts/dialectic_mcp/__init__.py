from .audit_service import AuditService, quick_confidence, quick_fallacy_audit, quick_self_reflection
from .config import AppConfig, ProviderConfig, RoleConfig
from .engine import DialecticEngine
from .mcp_service import McpServerService, build_mcp
from .store import Store


def __getattr__(name: str):
    if name in {"make_parser", "run_cli"}:
        from .cli import make_parser, run_cli

        return {"make_parser": make_parser, "run_cli": run_cli}[name]
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")


__all__ = [
    "AppConfig",
    "AuditService",
    "DialecticEngine",
    "McpServerService",
    "ProviderConfig",
    "RoleConfig",
    "Store",
    "build_mcp",
    "make_parser",
    "quick_confidence",
    "quick_fallacy_audit",
    "quick_self_reflection",
    "run_cli",
]
