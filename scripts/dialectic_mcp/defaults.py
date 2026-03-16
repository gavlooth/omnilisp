from __future__ import annotations

from typing import Any


DEFAULT_CONFIG: dict[str, Any] = {
    "database_path": "dialectic_mcp.sqlite3",
    "providers": {
        "openai_main": {
            "type": "openai_compatible",
            "base_url": "https://api.openai.com",
            "api_key_env": "OPENAI_API_KEY",
        },
        "anthropic_main": {
            "type": "anthropic",
            "base_url": "https://api.anthropic.com",
            "api_key_env": "ANTHROPIC_API_KEY",
        },
        "ollama_local": {
            "type": "ollama",
            "base_url": "http://localhost:11434",
        },
    },
    "roles": {
        "thesis": {
            "provider": "openai_main",
            "model": "gpt-4.1",
            "temperature": 0.5,
            "system_prompt": "Build the strongest technical thesis with concrete constraints.",
        },
        "antithesis": {
            "provider": "openai_main",
            "model": "gpt-4.1",
            "temperature": 0.4,
            "system_prompt": "Critique arguments without ad hominem or red herrings. Focus on claim-level failures.",
        },
        "synthesis": {
            "provider": "openai_main",
            "model": "gpt-4.1",
            "temperature": 0.4,
            "system_prompt": "Synthesize actionable architecture preserving constraints and tradeoffs.",
        },
        "self_reflection": {
            "provider": "openai_main",
            "model": "gpt-4.1-mini",
            "temperature": 0.2,
            "system_prompt": "Audit whether critique contributed decision-relevant information.",
        },
        "fallacy_audit": {
            "provider": "openai_main",
            "model": "gpt-4.1-mini",
            "temperature": 0.1,
            "system_prompt": "Detect only argument-structure fallacies with short rationales.",
        },
        "citation_assist": {
            "provider": "openai_main",
            "model": "gpt-4.1-mini",
            "temperature": 0.2,
            "system_prompt": "Suggest primary-source citation queries from claims.",
        },
    },
}
