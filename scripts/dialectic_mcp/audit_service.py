from __future__ import annotations

import re
from typing import Any


FALLACY_PATTERNS = {
    "ad_hominem": re.compile(r"\b(idiot|stupid|incompetent|you are the problem)\b", re.I),
    "red_herring": re.compile(r"\b(unrelated|irrelevant tangent|off topic)\b", re.I),
    "appeal_to_authority_only": re.compile(r"\bbecause \w+ says so\b", re.I),
}


def quick_fallacy_audit(text: str) -> dict[str, Any]:
    findings = []
    for name, pattern in FALLACY_PATTERNS.items():
        if pattern.search(text):
            findings.append({"fallacy": name, "present": True, "confidence": 0.55})
    return {
        "has_findings": bool(findings),
        "findings": findings,
        "note": "Heuristic only. Replace with model-backed claim-level audit.",
    }


def quick_self_reflection(critique_text: str) -> dict[str, Any]:
    words = len(critique_text.split())
    contributes = words > 50
    return {
        "contributes": contributes,
        "delta_information": "medium" if contributes else "low",
        "weakest_point": "N/A in heuristic mode",
        "note": "Replace with role=self_reflection model call for production.",
    }


def quick_confidence(cited_claim_ratio: float, structure_score: float, calibration_factor: float) -> float:
    score = (0.45 * cited_claim_ratio) + (0.35 * structure_score) + (0.20 * calibration_factor)
    return max(0.0, min(1.0, score))


class AuditService:
    def audit_fallacies(self, text: str) -> dict[str, Any]:
        return quick_fallacy_audit(text)

    def reflect_critique(self, text: str) -> dict[str, Any]:
        return quick_self_reflection(text)

    def confidence(self, cited_claim_ratio: float, structure_score: float, calibration_factor: float) -> float:
        return quick_confidence(cited_claim_ratio, structure_score, calibration_factor)
