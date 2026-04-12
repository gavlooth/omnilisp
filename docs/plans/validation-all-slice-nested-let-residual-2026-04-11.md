# Validation All-Slice Nested-Let Residual (2026-04-11)

Status: `closed`
Owner: Codex workflow
Mode: validation triage plan
TODO lane: `VALIDATION-ALL-SLICE-NESTED-LET-2026-04-11`

## Purpose

Track the one remaining bounded all-slice failure after the TOML, Deduce, and
zero-length pattern allocation residuals were repaired. This residual is now
closed.

The current bounded all-slice result is:

```bash
scripts/run_validation_container.sh bash -lc 'c3c build --warn-deprecation=no && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=all ./build/main --test-suite lisp'
```

Scope note: this command intentionally skips TLS integration via
`OMNI_SKIP_TLS_INTEGRATION=1`, so it records the bounded all-slice-without-TLS
baseline rather than the TLS integration gate.

Original result:

- unified: `pass=2797 fail=1`
- compiler: `pass=208 fail=0`

Remaining emitted failure:

```text
[FAIL] nested let 10 levels => 55 (interp=FAIL, jit=FAIL)
  JIT debug [nested let 10 levels => 55]: stage=parse value=nil
```

## Resolution

Root cause: the `nested let 10 levels => 55` memory-stress fixture had two
extra closing parentheses. Both the interpreter and JIT harnesses failed at the
parser stage because the source was malformed, not because the parser state
leaked across all-slice order.

Fix: balance the fixture expression in `tests_core_groups.c3`. A direct
`--eval` of the corrected expression returns `55`.

The corrected memory-stress slice result is:

```bash
scripts/run_validation_container.sh bash -lc 'c3c build --warn-deprecation=no && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-stress ./build/main --test-suite lisp'
```

Result:

- unified: `pass=15 fail=0`

The corrected bounded all-slice result is:

```bash
scripts/run_validation_container.sh bash -lc 'c3c build --warn-deprecation=no && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=all ./build/main --test-suite lisp'
```

Result:

- unified: `pass=2798 fail=0`
- compiler: `pass=208 fail=0`

## Next Steps

None for this lane. Keep this plan closed unless the same fixture regresses.
