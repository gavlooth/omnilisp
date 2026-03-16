from __future__ import annotations

import dataclasses
import json
from typing import Any

from .defaults import DEFAULT_CONFIG
from .utils import deep_merge


@dataclasses.dataclass
class ProviderConfig:
    name: str
    type: str
    base_url: str
    api_key_env: str | None = None
    default_headers: dict[str, str] | None = None


@dataclasses.dataclass
class RoleConfig:
    role: str
    provider: str
    model: str
    temperature: float
    system_prompt: str


@dataclasses.dataclass
class AppConfig:
    database_path: str
    providers: dict[str, ProviderConfig]
    roles: dict[str, RoleConfig]

    @staticmethod
    def load(path: str | None) -> "AppConfig":
        data: dict[str, Any] = DEFAULT_CONFIG
        if path:
            with open(path, "r", encoding="utf-8") as handle:
                user_data = json.load(handle)
            data = deep_merge(DEFAULT_CONFIG, user_data)

        providers: dict[str, ProviderConfig] = {}
        for name, value in data["providers"].items():
            providers[name] = ProviderConfig(
                name=name,
                type=value["type"],
                base_url=value["base_url"],
                api_key_env=value.get("api_key_env"),
                default_headers=value.get("default_headers"),
            )

        roles: dict[str, RoleConfig] = {}
        for role, value in data["roles"].items():
            roles[role] = RoleConfig(
                role=role,
                provider=value["provider"],
                model=value["model"],
                temperature=float(value.get("temperature", 0.3)),
                system_prompt=value.get("system_prompt", ""),
            )

        return AppConfig(database_path=data["database_path"], providers=providers, roles=roles)
