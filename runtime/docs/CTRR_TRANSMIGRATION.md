# CTRR Transmigration (Metadata-Driven)

**Status:** Runtime implementation contract (detailed)  
**Applies to:** `runtime/src/memory/transmigrate.c`, type metadata, and all `TAG_*` types  
**Guarantee enforced:** Region Closure Property (no stale pointers into dead regions)  
**Non-goal:** stop-the-world GC, heap-wide tracing collectors

This document specifies the **metadata-driven transmigration** mechanism required
by OmniLisp’s CTRR memory model.

For the high-level CTRR contract, see `docs/CTRR.md`.

---

## 1) Why transmigration exists

CTRR allows **any value** to escape a region boundary (return, capture, global
store, cross-thread send) and remain valid after the source region is exited and
reclaimed/reused.

This is only possible if the runtime provides a correct escape operation:

- `transmigrate(root, src_region, dest_region)`

Tethering is complementary but different:

- tethering pins a region alive during **bounded borrows**
- transmigration repairs the heap graph for **escapes**

Tethering cannot fix dangling pointers; transmigration (or an ownership transfer
like adoption/splicing) must do the pointer rewriting.

---

## 2) Required correctness property (normative)

### 2.1 Region Closure Property (restated)

After `region_exit(src_region)` and once the runtime may reclaim/reuse `src_region`:

- no reachable value may contain any pointer into memory owned by `src_region`

### 2.2 Transmigration contract

Given:

- `Obj* out = transmigrate(in, src, dst)`

then:

1. `out` is semantically equivalent to `in`.
2. No pointer reachable from `out` points into memory owned by `src`.
3. Shared substructure and cycles are preserved:
   - if `in` has two references to the same object, `out` must also share
   - if `in` is cyclic, `out` must be cyclic with the same shape

### 2.3 Forbidden fallback

There MUST NOT be a “default shallow copy” fallback for unhandled tags.

- Missing support for a runtime tag is a runtime bug, not an optimization gap.

---

## 3) Why metadata-driven (not a `switch(tag)` forever)

`switch(tag)` transmigration fails in practice because:

- new tags get added and silently fall into a shallow-copy default
- tags with payload buffers (arrays, dicts, tuples, etc.) require deep knowledge
  of external memory layout, which becomes unmaintainable in a monolithic switch

Metadata-driven transmigration makes “missing support” impossible to ignore:

- each tag/type registers:
  - how to **clone** into a destination region
  - how to **trace** all `Obj*` child slots (including those inside payload buffers)

This also enables reuse of metadata for:

- debug “no pointers into dead region” checks
- future incremental transmigration
- future region adoption/splicing correctness checks

---

## 4) Authoritative metadata API (required)

This guide is written in terms of `Obj` and `TAG_*` from `runtime/include/omni.h`
and type metadata from `runtime/src/memory/region_metadata.h`.

### 4.1 Visitor types

```c
typedef struct Obj Obj;
typedef struct Region Region;

/* Called for each child slot that is an Obj* pointer. */
typedef void (*OmniVisitSlotFn)(Obj** slot, void* ctx);

/*
 * TraceFn enumerates *all* child Obj* slots reachable from `obj`.
 *
 * IMPORTANT: This includes Obj* stored inside payload buffers (arrays/dicts),
 * not just pointers in the Obj union itself.
 */
typedef void (*TraceFn)(Obj* obj, OmniVisitSlotFn visit_slot, void* ctx);

/*
 * CloneFn allocates and copies a single object into dest_region.
 *
 * CloneFn MUST:
 * - allocate the destination Obj in dest_region
 * - allocate/copy any payload structs/buffers into dest_region
 * - copy scalar fields and install pointers to the newly copied payload
 *
 * CloneFn MUST NOT recursively transmigrate children.
 * The generic transmigrate loop performs child rewriting by calling TraceFn
 * on the new object and scheduling its slots.
 */
typedef Obj* (*CloneFn)(Obj* old_obj, Region* dest_region, void* tmp_ctx);
```

### 4.2 Required metadata fields

`TypeMetadata` must contain the following fields and they must be filled for
every tag/type that can appear in user-visible values:

- `TraceFn trace`
- `CloneFn clone`

If the existing metadata struct does not contain these fields (or contains stub
fields that are not used), it must be extended and made authoritative.

### 4.3 Why `pointer_offsets` is insufficient by itself

Many OmniLisp values store pointers in *payload memory*, not in `Obj`:

- `TAG_ARRAY`: `obj->ptr` points to an `Array` struct whose `data` points to an
  `Obj**` buffer
- `TAG_DICT`: buckets/entries may store `Obj*` keys/values outside of `Obj`
- `TAG_NAMED_TUPLE`: keys/values arrays

Therefore, metadata must be able to trace **through payload buffers**, not just
through fields inside `Obj`.

---

## 5) Generic transmigration algorithm (required behavior)

Transmigration is an explicit operation on one root, not a collector.

### 5.1 Internal structures (conceptual)

- Worklist of `WorkItem { Obj** slot; Obj* old_ptr; }`
- Remap mapping `old_ptr -> new_ptr` (preserve sharing and cycles)
- Optional bitmap visited acceleration for pointers inside `src_region`
- Temporary allocation arena/context (freed at end of transmigrate)

### 5.2 Normative algorithm (pseudocode)

```c
Obj* transmigrate(Obj* root, Region* src, Region* dst) {
  if (!root) return NULL;
  if (IS_IMMEDIATE(root)) return root;
  if (!dst) return root; /* caller error; prefer assert in debug */

  TmpCtx tmp = tmpctx_init();
  Remap remap = remap_init(&tmp);
  Worklist wl = wl_init(&tmp);

  Obj* out_root = NULL;
  wl_push(&wl, &out_root, root);

  while (!wl_empty(&wl)) {
    WorkItem it = wl_pop(&wl);
    Obj* old_obj = it.old_ptr;

    if (!old_obj) { *it.slot = NULL; continue; }
    if (IS_IMMEDIATE(old_obj)) { *it.slot = old_obj; continue; }

    Obj* existing = remap_get(&remap, old_obj);
    if (existing) { *it.slot = existing; continue; }

    const TypeMetadata* meta = meta_for_tag(old_obj->tag);
    if (!meta || !meta->clone || !meta->trace) fatal_unhandled_tag(old_obj->tag);

    Obj* new_obj = meta->clone(old_obj, dst, &tmp);
    if (!new_obj) fatal_oom();

    remap_put(&remap, old_obj, new_obj);
    *it.slot = new_obj;

    /* Schedule child slot rewrites on the NEW object */
    meta->trace(new_obj, visit_slot_push_worklist, &wl);
  }

  tmpctx_free(&tmp);
  return out_root;
}
```

### 5.3 Critical rule: trace the destination object

`trace()` is called on the **destination** object so that slot rewriting updates
the correct graph in-place.

---

## 6) Canonical layout requirement (must be enforced)

Transmigration is only correct if each tag has a single, canonical meaning for:

- which union field(s) are used (`a`/`b`/`ptr`/etc.)
- what `ptr` points to (if anything)
- where child `Obj*` references live (direct fields vs payload buffers)

If a tag is represented inconsistently (e.g., sometimes BOX stores its child in
`a`, sometimes in `ptr`), then no `trace/clone` implementation can be correct.

**Therefore, CTRR requires canonicalization per tag.**

This must be documented and enforced by constructors/accessors.

---

## 7) Per-type implementation obligations (examples)

The following are examples of the minimum required behavior; they are not
complete unless the runtime’s type set matches these examples.

### 7.1 Pair / cons cell (`TAG_PAIR`)

- `clone`: allocate new `Obj` in `dst` and shallow-copy fields; keep `a` and `b`
  as old pointers initially
- `trace`: visit `&obj->a` and `&obj->b`

### 7.2 Box (`TAG_BOX`)

Assuming BOX stores its child in `obj->a`:

- `clone`: allocate new `Obj` in `dst`, copy scalar fields, copy `a` as old ptr
- `trace`: visit `&obj->a`

If BOX uses `ptr` instead, trace that as an `Obj**` slot and canonicalize.

### 7.3 Closure (`TAG_CLOSURE`)

Assuming `obj->ptr` points to a `Closure` with `Obj** captures`:

- `clone`:
  - allocate destination `Obj` + `Closure` struct in `dst`
  - allocate destination capture array in `dst`
  - copy capture pointers (old pointers initially)
- `trace`:
  - visit each `&closure->captures[i]`

### 7.4 Array (`TAG_ARRAY`)

Assuming `obj->ptr` points to `Array { Obj** data; int len; int capacity; }`:

- `clone`:
  - allocate destination `Obj` in `dst`
  - allocate `Array` struct in `dst`
  - allocate `data` buffer in `dst` sized to `capacity`
  - copy `len` slots from old buffer into new buffer (old pointers initially)
  - set `new_obj->ptr = new_array`
- `trace`:
  - visit `&array->data[i]` for `0 <= i < len`

### 7.5 Dict (`TAG_DICT`)

Whatever dict implementation is used, the rule is:

- `clone` duplicates all structural storage into `dst` (buckets + entries)
- `trace` visits all `Obj*` key/value slots stored in those entries

---

## 8) Debug verification (required for confidence)

Add a debug-only verifier that uses the same metadata tracing to enforce the
Region Closure Property:

- `assert_no_ptrs_into_region(root, forbidden_region)`

Implementation outline:

1. Traverse `root` using `trace()` and a worklist (similar to transmigrate, but
   without cloning).
2. For each boxed pointer, check whether its address lies inside the forbidden
   region’s arena range.
3. If any pointer lies inside, abort with a clear diagnostic (tag + path).

This is not GC; it is a correctness assertion for escapes.

---

## 9) Interaction with ownership-transfer optimizations (allowed)

CTRR may later add ownership-transfer optimizations that avoid copying large
graphs, such as region adoption or arena chunk splicing.

Requirements for any such optimization:

- it must still satisfy the Region Closure Property
- it must preserve sharing/cycles semantics
- it must be reflected in the compiler’s escape codegen so the optimization is
  actually reachable

Transmigration correctness is a prerequisite. Do not rely on splicing to “hide”
incomplete type coverage.

---

## 10) “Done means” checklist

Transmigration is considered correct/complete only when:

1. Every user-visible runtime tag has metadata (`clone` + `trace`).
2. `transmigrate()` has no shallow-copy fallback for unknown tags.
3. Runtime tests cover mixed graphs (arrays + dicts + closures + pairs).
4. Debug checker confirms no pointers into a dead/reused region after an escape.

---

## 11) Current repository implementation status (informative)

This document is **normative** about what CTRR requires. The current repository
code should be treated as **work-in-progress** until it matches this contract.

As of **2026-01-09**, `runtime/src/memory/transmigrate.c`:

- Implements a worklist + bitmap “visited” set, but still uses a
  `switch (old_obj->tag)` for cloning and child scheduling.
- Contains **forbidden fallbacks**:
  - returns the original `root` if bitmap creation fails (with a warning)
  - uses a `default:` case that performs a shallow copy for unknown tags
- Does not provide total coverage of tags that exist in `runtime/include/omni.h`
  (e.g., arrays/dicts/tuples and several runtime handles).

These gaps mean the “everything can escape” guarantee is **not yet enforced** by
the runtime.

Implementation direction to become compliant:

1. Extend region metadata to include **both** `clone` and `trace` callbacks.
2. Refactor transmigration to use those callbacks and to **fail loudly** when a
   tag is missing metadata (in debug builds).
3. Add a debug-only verifier (`assert_no_ptrs_into_region`) that reuses `trace`
   metadata to assert the Region Closure Property after escapes.
