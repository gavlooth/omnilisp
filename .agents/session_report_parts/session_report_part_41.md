## 2026-04-23 06:36:41 CEST - Slash Primitive Naming Policy Documented

Objective attempted:
- Record the owner decision that slash-qualified names are canonical for core
  primitive families such as `io/...`, `matrix/...`, `tensor/...`, `ml/...`,
  `nn/...`, and `ui/...`.

Relevant workspace or target:
- `/home/christos/Omni`
- `docs/LANGUAGE_SPEC.part-01.md`
- `docs/LANGUAGE_SPEC.part-04.md`
- `docs/reference/11-appendix-primitives.md`
- `AGENTS.md`

Code or configuration changes made:
- Expanded the language token docs to clarify slash names are single symbols,
  including quoted forms such as `'ml/plot`.
- Updated the module docs to state that slash-qualified primitive names are not
  module targets.
- Added the same convention to the primitive reference and agent naming policy.
- Added human-developer ergonomics guidance: use slash prefixes for compact
  dense primitive families, keep cross-cutting generic operations unprefixed,
  avoid deep or mechanically long slash pseudo-paths, and use real modules for
  optional or independently versioned surfaces.
- Recorded the decision in `memory/changelog_parts/changelog_part_37.md`.

Commands run:
- `git diff --check`
- `scripts/check_primitive_docs_parity.sh`
- `scripts/check_status_consistency.sh`
- `scripts/check_file_size_gate.sh`

Key results:
- No runtime behavior changed.
- Whitespace diff check passed.
- Primitive docs parity passed.
- Status consistency passed with TODO actionable count 0.
- Code file-size gate passed with no tracked code files above 1000 LOC.

Unresolved issues:
- None.

Signature: GPT-5 Codex
