# Memory Runtime: End-to-End Architecture and Cycle

As of: 2026-03-07

This file is the end-to-end visual map for the current memory architecture and
runtime lifecycle, aligned to:

- `src/scope_region.c3`
- `src/lisp/eval_boundary_api.c3`
- `src/lisp/jit_jit_eval_scopes.c3`
- `src/lisp/eval_promotion_escape.c3`
- `src/lisp/value_interp_state.c3`

## 0) Master Single-File Diagram

Source:
- `docs/areas/diagrams/memory-runtime-cycle-master.mmd`

Rendered SVG:
- `docs/areas/diagrams/memory-runtime-cycle-master.svg`

## 1) End-to-End Architecture

```mermaid
flowchart TB
    subgraph Frontend["Eval/JIT Frontend"]
        EVAL["eval/apply/jit call entry"]
        FINALIZE["jit_finalize_scoped_result(...)"]
    end

    subgraph Boundary["Boundary Facade (ownership-safe handoff)"]
        BAPI["boundary_* helpers"]
        PUSH["boundary_push_child_scope"]
        PROMOTE_ESCAPE["boundary_promote_to_escape"]
        COPY_REL["boundary_copy_from_releasing_scope"]
        SPLICE["boundary_try_scope_splice_escapes"]
    end

    subgraph Runtime["ScopeRegion Runtime (RC-owned)"]
        ROOT_SCOPE["root_scope (permanent runtime root)"]
        PARENT_SCOPE["parent scope (caller)"]
        CHILD_SCOPE["child scope (callee)"]
        TEMP["TEMP lane: alloc + dtors"]
        ESCAPE["ESCAPE lane: alloc_escape + escape_dtors"]
        RETAIN_PARENT["scope_create(child,parent)\nscope_retain(parent)"]
        RELEASE["scope_release(child)"]
        DESTROY["scope_destroy when RC==0\nrun dtors\nscope_release(parent)"]
        CLOSURE_RETAIN["escape closure keeps env_scope alive\nscope_retain(env_scope)"]
    end

    subgraph AstLifetime["AST Lifetime (separate ownership lane)"]
        ROOT_REGION["root_region (Expr/Pattern only)"]
    end

    EVAL --> BAPI
    BAPI --> PUSH
    PUSH --> CHILD_SCOPE
    PUSH --> RETAIN_PARENT
    RETAIN_PARENT --> PARENT_SCOPE
    CHILD_SCOPE --> TEMP
    CHILD_SCOPE --> ESCAPE
    EVAL --> FINALIZE
    FINALIZE --> PROMOTE_ESCAPE
    FINALIZE --> COPY_REL
    FINALIZE --> SPLICE
    FINALIZE --> RELEASE
    PROMOTE_ESCAPE --> CLOSURE_RETAIN
    CLOSURE_RETAIN --> PARENT_SCOPE
    SPLICE --> PARENT_SCOPE
    COPY_REL --> PARENT_SCOPE
    RELEASE --> DESTROY
    DESTROY --> PARENT_SCOPE
    PARENT_SCOPE --> ROOT_SCOPE

    ROOT_REGION -. separate from Value/Env scope ownership .- ROOT_SCOPE
```

## 2) Call-to-Return Memory Cycle

```mermaid
sequenceDiagram
    participant Caller as Caller scope
    participant Boundary as boundary_*
    participant Scope as scope_region
    participant Eval as jit/eval body
    participant Promote as promotion/copy

    Caller->>Boundary: boundary_push_child_scope(interp, &saved_scope)
    Boundary->>Scope: scope_create(saved_scope)
    Scope-->>Boundary: child_scope (RC=1)
    Boundary->>Eval: interp.current_scope = child_scope

    Eval->>Scope: alloc(...) in TEMP lane
    Eval->>Scope: alloc_escape(...) in ESCAPE lane (optional)
    Eval-->>Boundary: result
    Boundary->>Boundary: jit_finalize_scoped_result(...)

    alt RC==1 and result not yet in ESCAPE lane
        Boundary->>Promote: boundary_promote_to_escape(result)
        Promote->>Scope: alloc_escape + register_dtor_escape
    end

    alt promotion budget aborts
        Boundary->>Promote: boundary_copy_from_releasing_scope(original_result)
        Boundary->>Scope: scope_release(child_scope)
        Boundary-->>Caller: copied result in parent/root lifetime
    else result in child ESCAPE lane
        Boundary->>Scope: scope_splice_escapes(parent, child)
        Scope->>Scope: run TEMP dtors + free TEMP chunks
        Scope->>Scope: transfer ESCAPE chunks/dtors to parent
        Boundary-->>Caller: same result now reachable via parent
    else result already outside releasing scope
        Boundary->>Scope: scope_release(child_scope)
        Boundary-->>Caller: reuse result as-is
    else fallback
        Boundary->>Promote: boundary_copy_from_releasing_scope(result)
        Boundary->>Scope: scope_release(child_scope)
        Boundary-->>Caller: copied result
    end
```

## 3) ScopeRegion Lifecycle State Machine

```mermaid
stateDiagram-v2
    [*] --> Created: scope_create(parent)
    Created --> Active: refcount=1\nTEMP initialized\nESCAPE empty

    Active --> TempAlloc: alloc(...)
    TempAlloc --> Active

    Active --> EscapeAlloc: alloc_escape(...)
    EscapeAlloc --> Active

    Active --> TempReset: scope_reset_temp_lane\n(TCO recycle path)
    TempReset --> Active: ESCAPE lane preserved

    Active --> Spliced: scope_splice_escapes(parent, child)
    Spliced --> Recycled: child detached and recycled/freed

    Active --> Release: scope_release(...)
    Release --> Active: refcount > 0
    Release --> Destroying: refcount == 0
    Destroying --> Recycled: run dtors\nfree TEMP + ESCAPE chunks\nrelease parent
    Recycled --> [*]
```

## 4) RC Ownership Cycle (Explicit)

```mermaid
sequenceDiagram
    participant Parent as parent scope
    participant Child as child scope
    participant Closure as escaping closure/env

    Note over Parent,Child: create child scope under parent
    Parent->>Child: scope_create(parent)
    Child->>Parent: scope_retain(parent) (+1)

    alt closure capture or escaped closure promotion
        Child->>Closure: closure/env references env_scope
        Closure->>Parent: scope_retain(env_scope) (+1)
    end

    Note over Parent,Child: call boundary finalization
    Child->>Child: scope_release(child) (-1)

    alt child RC reaches 0
        Child->>Child: scope_destroy(child)
        Child->>Parent: scope_release(parent) (-1)
    else child RC still > 0 (captured/liveness path)
        Child-->>Parent: child survives as independent heap island
    end

    alt later closure/env teardown
        Closure->>Parent: scope_release(env_scope) (-1)
    end
```

## 5) Invariants (diagram reading key)

- Ownership authority is `ScopeRegion` RC (`scope_retain`/`scope_release`).
- Language values do not use per-type RC lifetime as the primary ownership
  mechanism.
- Root pinning is not a general correctness mechanism; only explicit
  process-lifetime singletons may use it.
- Boundary paths are the source of truth for return/env/mutation/promotion
  crossings.
