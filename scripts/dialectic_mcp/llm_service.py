from __future__ import annotations

import dataclasses
import datetime as dt
import json
import os
import urllib.request
from typing import Any

from .config import AppConfig, ProviderConfig


class LLMService:
    def __init__(self, cfg: AppConfig):
        self.cfg = cfg

    def run_role(
        self,
        role: str,
        user_prompt: str,
        context_text: str,
        override: dict[str, Any] | None = None,
    ) -> dict[str, Any]:
        if role not in self.cfg.roles:
            raise ValueError(f"unknown role: {role}")

        role_config = dataclasses.replace(self.cfg.roles[role])
        if override:
            if "provider" in override:
                role_config.provider = override["provider"]
            if "model" in override:
                role_config.model = override["model"]
            if "temperature" in override:
                role_config.temperature = float(override["temperature"])
            if "system_prompt" in override:
                role_config.system_prompt = override["system_prompt"]

        provider = self.cfg.providers[role_config.provider]
        system = role_config.system_prompt.strip()
        message = f"Context:\n{context_text}\n\nTask:\n{user_prompt}"

        if provider.type == "openai_compatible":
            text, latency = self._call_openai_compatible(
                provider, role_config.model, role_config.temperature, system, message
            )
        elif provider.type == "anthropic":
            text, latency = self._call_anthropic(provider, role_config.model, role_config.temperature, system, message)
        elif provider.type == "ollama":
            text, latency = self._call_ollama(provider, role_config.model, role_config.temperature, system, message)
        else:
            raise ValueError(f"unsupported provider type: {provider.type}")

        return {
            "role": role,
            "provider": role_config.provider,
            "model": role_config.model,
            "temperature": role_config.temperature,
            "text": text,
            "latency_ms": latency,
            "system_prompt": system,
        }

    def _http_post_json(
        self,
        url: str,
        payload: dict[str, Any],
        headers: dict[str, str],
    ) -> tuple[dict[str, Any], int]:
        body = json.dumps(payload).encode("utf-8")
        request = urllib.request.Request(url, data=body, headers=headers, method="POST")
        start = dt.datetime.utcnow()
        with urllib.request.urlopen(request, timeout=120) as response:
            raw = response.read().decode("utf-8")
            elapsed = int((dt.datetime.utcnow() - start).total_seconds() * 1000)
            return json.loads(raw), elapsed

    def _call_openai_compatible(
        self,
        provider: ProviderConfig,
        model: str,
        temperature: float,
        system: str,
        message: str,
    ) -> tuple[str, int]:
        api_key = os.getenv(provider.api_key_env or "", "")
        if not api_key:
            raise RuntimeError(f"missing API key env: {provider.api_key_env}")

        url = provider.base_url.rstrip("/") + "/v1/chat/completions"
        payload = {
            "model": model,
            "temperature": temperature,
            "messages": [
                {"role": "system", "content": system},
                {"role": "user", "content": message},
            ],
        }
        headers = {
            "Content-Type": "application/json",
            "Authorization": f"Bearer {api_key}",
        }
        if provider.default_headers:
            headers.update(provider.default_headers)

        data, elapsed = self._http_post_json(url, payload, headers)
        return data["choices"][0]["message"]["content"], elapsed

    def _call_anthropic(
        self,
        provider: ProviderConfig,
        model: str,
        temperature: float,
        system: str,
        message: str,
    ) -> tuple[str, int]:
        api_key = os.getenv(provider.api_key_env or "", "")
        if not api_key:
            raise RuntimeError(f"missing API key env: {provider.api_key_env}")

        url = provider.base_url.rstrip("/") + "/v1/messages"
        payload = {
            "model": model,
            "max_tokens": 2048,
            "temperature": temperature,
            "system": system,
            "messages": [{"role": "user", "content": message}],
        }
        headers = {
            "Content-Type": "application/json",
            "x-api-key": api_key,
            "anthropic-version": "2023-06-01",
        }
        if provider.default_headers:
            headers.update(provider.default_headers)

        data, elapsed = self._http_post_json(url, payload, headers)
        parts: list[str] = []
        for block in data.get("content", []):
            if block.get("type") == "text":
                parts.append(block.get("text", ""))
        return "\n".join(parts).strip(), elapsed

    def _call_ollama(
        self,
        provider: ProviderConfig,
        model: str,
        temperature: float,
        system: str,
        message: str,
    ) -> tuple[str, int]:
        url = provider.base_url.rstrip("/") + "/api/chat"
        payload = {
            "model": model,
            "stream": False,
            "options": {"temperature": temperature},
            "messages": [
                {"role": "system", "content": system},
                {"role": "user", "content": message},
            ],
        }
        headers = {"Content-Type": "application/json"}
        if provider.default_headers:
            headers.update(provider.default_headers)

        data, elapsed = self._http_post_json(url, payload, headers)
        return data["message"]["content"], elapsed
