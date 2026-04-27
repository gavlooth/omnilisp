# Omni Docs Entry Point

This file is the single starting point for documentation.
If two docs disagree, use the authority model below.

## Read Order

1. `docs/DOCS_CONTRACT.md`
2. `docs/LANGUAGE_SPEC.md`
3. `docs/ARCHITECTURE.md`
4. `docs/PROJECT_TOOLING.md`
5. `memory/CHANGELOG.md`

For compact LLM-facing syntax guidance, read `docs/LLM_LANGUAGE_DIGEST.md`
after the normative sources above. It is a digest, not an authority.

## Authority Model

### Normative Contract (language/runtime rules)

- `docs/LANGUAGE_SPEC.md`
- `docs/ARCHITECTURE.md`
- `docs/ERROR_MODEL.md`

### Operational Truth (what is currently validated)

- `memory/CHANGELOG.md`
- `docs/RELEASE_STATUS.md`
- `docs/areas/README.md` and `docs/areas/*.md`
- `docs/VALIDATION_TOOLS.md` for regression/debugging tool selection and
  platform install notes

### User Reference / Cookbook

- `docs/OMNI_REFERENCE.md`
- `docs/LLM_LANGUAGE_DIGEST.md`
- `docs/reference/*`
- `examples/README.md`

## Canonical Coverage Matrix

For complete aspect-by-aspect coverage, see:

- `docs/DOCS_CONTRACT.md`

That file explicitly maps each major aspect to:
- authoritative document,
- status (`normative`, `operational`, `in-progress`),
- verification source.

## Compatibility / Migration

Language surface removals and replacements live in:

- `docs/SURFACE_COMPATIBILITY.md`

Use this before adding aliases or reviving old syntax.

## Planning / Work Queues

- Active plans: `docs/plans/README.md`
- Area status hub: `docs/areas/README.md`
- Deduce runtime status: `docs/areas/deduce-runtime.md`
- Session checkpoints: `docs/SESSION_REPORT.md`

## Redundancy Policy

- `docs/LANGUAGE_SPEC.md` is the language contract.
- `docs/FEATURES.md` is an index, not a second spec.
- `docs/OMNI_REFERENCE.md` is a navigation surface for chapter docs, not
  a competing source of truth.
