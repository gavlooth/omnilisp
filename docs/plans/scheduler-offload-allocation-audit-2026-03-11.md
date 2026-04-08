# Scheduler Offload Allocation Audit (2026-03-11)

Scope: `src/lisp/scheduler_offload_worker.c3` per-request allocation sites.

Goal: complete O4.2 by classifying each allocation as either:
- reusable worker-local scratch/infrastructure, or
- required owned output that must remain per-request.

## Allocation Classification

### A) Required Owned Outputs (Do Not Pool As Scratch)

These allocations cross the worker boundary or become completion-owned results.
They must keep current ownership semantics.

- `offload_compress` output payload:
  - `out = mem::malloc(bound + 1)` at `scheduler_offload_worker.c3:21`
  - transferred through `scheduler_set_offload_bytes_owned(...)` at `:37`
  - classification: **required owned output**

- `offload_http_get` response payload (HTTPS path):
  - `resp_buf = mem::malloc(resp_cap + 1)` at `:153`
  - transferred through `scheduler_set_offload_bytes_owned(...)` at `:166`
  - classification: **required owned output**

- `offload_http_get` response payload (HTTP path):
  - `resp_buf = mem::malloc(resp_cap + 1)` at `:191`
  - transferred through `scheduler_set_offload_bytes_owned(...)` at `:204`
  - classification: **required owned output**

- `offload_read_file` file content buffer:
  - `io_uv_read_all_file(...)` fills `content_buf` at `:230-233`
  - transferred through `scheduler_set_offload_bytes_owned(...)` at `:237`
  - classification: **required owned output**

- Completion objects:
  - `completion = mem::malloc(OffloadCompletion.sizeof)` at `:336`
  - fallback completion alloc at `:385`
  - completion is delivered to pending-offload/task join paths and freed by
    completion lifecycle (`scheduler_free_offload_completion`)
  - classification: **required owned output**

### B) Reusable Worker-Local Scratch/Infrastructure Candidates

These allocations are temporary/infrastructure-scoped and do not need to cross
domain boundaries as owned payload bytes.

- Deflate compressor:
  - `libdeflate_alloc_compressor(6)` at `:11` / freed at `:16`
  - classification: **reusable worker-local scratch candidate**
  - candidate shape: per-worker cached compressor handle

- HTTPS transient TLS I/O contexts:
  - `client_ctx`, `x509_ctx`, `sslio_ctx`, `iobuf`, `fd_ptr` at `:129-133`
  - freed within function at `:181-185`
  - classification: **reusable worker-local scratch candidates**
  - candidate shape: per-worker TLS scratch bundle

- Default trust store load/free:
  - `omni_tls_trust_store_load_default()` at `:134`
  - freed at `:186`
  - classification: **reusable worker-local cache candidate**
  - candidate shape: per-worker/process cache with explicit reload policy

- Queue request envelope:
  - `QueuedOffloadWork* queued = mem::malloc(...)` at `:469`
  - freed in after-callback at `:448`
  - classification: **reusable infrastructure candidate**
  - candidate shape: narrow freelist/slab for queued envelopes

## Summary

- Required-owned outputs are concentrated in payload bytes and completion
  objects; these should not be pooled as mutable scratch because they must
  preserve cross-boundary ownership semantics.
- Reuse opportunities are concentrated in:
  - queue envelope scaffolding (`QueuedOffloadWork`),
  - per-worker TLS scratch objects,
  - compressor/trust-store caching.

This classification is suitable input for O4.3 prototype scope.
