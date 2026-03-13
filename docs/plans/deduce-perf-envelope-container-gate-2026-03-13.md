# Deduce Perf Envelope Container Gate (2026-03-13)

Purpose: close O7.9 container-gated perf-envelope TODO by adding a dedicated
Docker-bound benchmark gate that asserts benchmark summary invariants across
Deduce performance suites.

## Gate Script

Added:
- `scripts/run_deduce_perf_envelope.sh`

Behavior:
- outside validation containers:
  - enforces Docker-bound execution by re-entering through
    `scripts/run_validation_container.sh`.
- inside validation containers:
  - optional build (`OMNI_DEDUCE_PERF_BUILD=1` default),
  - runs:
    - `OMNI_LISP_TEST_SLICE=deduce`
    - `OMNI_DEDUCE_BENCH=1`
    - `./build/main --test-suite lisp`
  - parses benchmark summaries and asserts per-suite envelope invariants for:
    - `deduce_scan_query_count`
    - `deduce_incremental_mutation`
    - `deduce_selective_join`
    - `deduce_recursive_closure`
    - `deduce_skewed_cardinality`

Container compatibility mounts:
- script auto-exports `OMNI_VALIDATION_EXTRA_ARGS` mounts for required
  host-provided headers/libs when launching validation container:
  - `/usr/include/yyjson.h`
  - `/usr/include/bearssl*.h`
  - `/usr/include/uv.h`
  - `/usr/include/uv/`
  - `/usr/include/ffi.h`
  - `/usr/lib/libreplxx.so.0`

## Validation Run

Command:

```bash
scripts/run_deduce_perf_envelope.sh
```

Result:

```text
Deduce perf envelope checks passed.
  log: build/deduce_perf_envelope.log
```

Captured benchmark summaries are in:
- `build/deduce_perf_envelope.log`
