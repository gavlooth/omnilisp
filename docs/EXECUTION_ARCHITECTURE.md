# OmniLisp Execution Architecture Plan

## Goal

Decouple Go from runtime execution. Go is only for tooling (parser, compiler, codegen).
All execution happens via native code - either JIT compiled or AOT compiled.

## Current State

```
┌─────────────────────────────────────────────────────────────────┐
│                        CURRENT ARCHITECTURE                      │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Source.omni ──► Go Parser ──► AST                            │
│                                    │                             │
│                    ┌───────────────┼───────────────┐            │
│                    ▼               ▼               ▼            │
│              Go Interpreter   Go Compiler     Go Compiler       │
│              (pkg/eval/)      + GCC           (C output)        │
│                    │               │               │            │
│                    ▼               ▼               ▼            │
│               [Execute]      [Native Binary]  [.c file]         │
│                                                                  │
│  Problems:                                                       │
│  - Go interpreter is slow                                        │
│  - GCC invocation has overhead                                   │
│  - Runtime recompiled every time                                 │
│  - No caching                                                    │
└─────────────────────────────────────────────────────────────────┘
```

## Target Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        TARGET ARCHITECTURE                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │                    GO TOOLING LAYER                       │   │
│  │  (Parser, Compiler, Codegen - development/build only)    │   │
│  └──────────────────────────────────────────────────────────┘   │
│                              │                                   │
│              ┌───────────────┼───────────────┐                  │
│              ▼               ▼               ▼                  │
│         ┌────────┐     ┌──────────┐    ┌──────────┐            │
│         │  AOT   │     │   JIT    │    │  Cached  │            │
│         │ Mode   │     │   Mode   │    │   Mode   │            │
│         └────────┘     └──────────┘    └──────────┘            │
│              │               │               │                  │
│              ▼               ▼               ▼                  │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │                  NATIVE EXECUTION LAYER                  │   │
│  │           (C Runtime - compiled once, cached)            │   │
│  └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

## Execution Modes

### Mode 1: AOT (Ahead-of-Time) - Production

```
Build Time:
  runtime.c ──► GCC/Clang ──► runtime.o (shipped with omnilisp)

Compile Time:
  source.omni ──► Go Compiler ──► user.c
  user.c + runtime.o ──► GCC/Clang ──► binary

Result: Standalone native binary, no dependencies
```

**Use case:** Distribution, deployment, maximum performance

### Mode 2: JIT with Cached Runtime - Interactive/REPL

```
Startup (once per process):
  runtime.c ──► libtcc ──► [runtime in memory]

Per Expression:
  expr ──► Go Compiler ──► user.c (tiny)
  user.c ──► libtcc ──► link with cached runtime ──► execute

Result: Fast iteration, no disk I/O after startup
```

**Use case:** REPL, development, scripting

### Mode 3: Cached AOT - Hybrid

```
First Run:
  source.omni ──► Go Compiler ──► user.c
  user.c ──► libtcc ──► user.so (cached to disk)

Subsequent Runs:
  Load user.so ──► execute (instant)

Cache Invalidation:
  source.omni hash changed ──► recompile
```

**Use case:** Scripts that run frequently, serverless

## Implementation Phases

### Phase 1: Fix Current Bugs (Priority: HIGH)

Before any architecture work, fix the recursive cons bug in native compilation.

**Tasks:**
1. Debug recursive closure memory issue
2. Fix `mk_pair` ownership/refcounting for recursive calls
3. Ensure all Violet demo features work natively
4. Add regression tests for recursive patterns

**Files:**
- `pkg/codegen/runtime.go` - fix closure/cons interaction
- `pkg/compiler/compiler.go` - ensure proper inc_ref generation

### Phase 2: Runtime Separation (Priority: HIGH)

Separate runtime into independently compilable unit.

**Tasks:**
1. Extract runtime.c as standalone file (not embedded in Go)
2. Create runtime.h with public API
3. Define clean interface between runtime and user code
4. Add build system for runtime.o

**Structure:**
```
runtime/
├── runtime.h          # Public API declarations
├── runtime.c          # Full runtime implementation
├── runtime_core.c     # Memory management (ASAP)
├── runtime_types.c    # Object types, constructors
├── runtime_prims.c    # Primitive operations
├── runtime_io.c       # I/O primitives
├── runtime_closure.c  # Closure implementation
├── runtime_conc.c     # Concurrency (threads, channels)
└── Makefile           # Build runtime.o, runtime.a
```

**Runtime API (runtime.h):**
```c
// Object system
typedef struct Obj Obj;
Obj* mk_int(long i);
Obj* mk_float(double f);
Obj* mk_char(long c);
Obj* mk_pair(Obj* a, Obj* b);
Obj* mk_sym(const char* s);
Obj* mk_closure(ClosureFn fn, Obj** captures, int count, int arity);

// Memory management
void inc_ref(Obj* x);
void dec_ref(Obj* x);
void free_obj(Obj* x);

// Primitives
Obj* prim_add(Obj* a, Obj* b);
Obj* prim_display(Obj* x);
// ... etc

// Closure calling
Obj* call_closure(Obj* clos, Obj** args, int argc);
```

### Phase 3: AOT Pipeline (Priority: HIGH)

Create production-ready AOT compilation.

**Tasks:**
1. Pre-compile runtime.o at build time
2. Generate user code that links with runtime.o
3. Single command: `omnilisp build source.omni -o binary`
4. Support static linking (single binary output)

**Build flow:**
```bash
# Build omnilisp tooling (includes pre-compiled runtime.o)
go build -o omnilisp .

# Compile user program
./omnilisp build program.omni -o program
# Internally:
#   1. Parse program.omni → AST
#   2. Compile AST → program.c
#   3. gcc -c program.c -o program.o
#   4. gcc program.o runtime.o -o program
```

### Phase 4: JIT Integration (Priority: MEDIUM)

Integrate libtcc for JIT execution.

**Tasks:**
1. Add cgo bindings for libtcc (or use existing)
2. Compile runtime once at startup
3. JIT compile user code on demand
4. Implement symbol resolution between runtime and user code

**Dependencies:**
- libtcc (Tiny C Compiler library)
- cgo for Go bindings

**Integration:**
[Go code removed]

### Phase 5: Cached Compilation (Priority: MEDIUM)

Add caching layer for compiled code.

**Tasks:**
1. Hash source files for cache keys
2. Cache compiled objects to disk
3. Implement cache invalidation
4. Support incremental compilation

**Cache structure:**
```
~/.omni/cache/
├── runtime.o              # Pre-compiled runtime
├── programs/
│   ├── <hash1>.o          # Cached user program
│   ├── <hash2>.o
│   └── ...
└── cache.json             # Metadata (source hash → object file)
```

### Phase 6: Remove Go Interpreter (Priority: LOW)

Once JIT is stable, deprecate Go interpreter.

**Tasks:**
1. Mark pkg/eval as deprecated
2. Route all execution through JIT or AOT
3. Keep Go interpreter only for bootstrap/testing
4. Eventually remove pkg/eval

**Execution routing:**
[Go code removed]

## File Structure (Target)

```
omnilisp/
├── cmd/
│   └── omnilisp/
│       └── main.go           # CLI entry point
├── pkg/
│   ├── parser/               # Go: Source → AST
│   ├── ast/                  # Go: AST definitions
│   ├── compiler/             # Go: AST → C code
│   ├── codegen/              # Go: C code generation
│   ├── jit/                  # Go+cgo: JIT execution
│   ├── aot/                  # Go: AOT build orchestration
│   ├── cache/                # Go: Compilation cache
│   └── eval/                 # DEPRECATED: Go interpreter
├── runtime/
│   ├── include/
│   │   └── omnilisp.h          # Public runtime API
│   ├── src/
│   │   ├── core.c            # Memory management
│   │   ├── types.c           # Object types
│   │   ├── prims.c           # Primitives
│   │   ├── closure.c         # Closures
│   │   ├── io.c              # I/O
│   │   └── conc.c            # Concurrency
│   ├── Makefile
│   └── runtime.a             # Pre-compiled static library
├── lib/
│   └── violet/               # Standard library (OmniLisp code)
└── docs/
    └── EXECUTION_ARCHITECTURE.md  # This document
```

## CLI Interface (Target)

```bash
# REPL with JIT (default interactive mode)
omnilisp
> (+ 1 2)
3

# Execute file with JIT
omnilisp run program.omni

# AOT compile to binary
omnilisp build program.omni -o program
omnilisp build program.omni -o program --static  # Static linking

# Emit C code (for debugging/inspection)
omnilisp emit program.omni -o program.c

# Cache management
omnilisp cache clear
omnilisp cache stats
```

## Performance Targets

| Mode | Startup | Execution | Use Case |
|------|---------|-----------|----------|
| Go Interpreter | ~10ms | 1x (baseline) | DEPRECATED |
| JIT (cold) | ~50ms | 50-100x | First run |
| JIT (warm) | ~1ms | 50-100x | REPL iteration |
| JIT Cached | ~5ms | 50-100x | Repeated scripts |
| AOT | 0ms | 100-200x | Production |

## Dependencies

**Required:**
- libtcc (Tiny C Compiler) - for JIT
- GCC or Clang - for AOT
- pthreads - for concurrency

**Optional:**
- libgccjit - alternative JIT (better optimization)
- LLVM - alternative backend (best optimization)

## Migration Path

1. **Now:** Fix bugs, stabilize native compilation
2. **Phase 2-3:** Runtime separation + AOT (production ready)
3. **Phase 4:** JIT integration (fast development)
4. **Phase 5:** Caching (best of both worlds)
5. **Phase 6:** Deprecate Go interpreter

## Open Questions

1. **libtcc vs libgccjit:** TCC is simpler but less optimized. Worth supporting both?
2. **Cross-compilation:** How to handle AOT for different targets?
3. **Debugging:** How to debug JIT-compiled code? Source maps?
4. **Hot reloading:** Can we reload code without restarting? (Advanced)

## References

- [Collapsing Towers of Interpreters](https://www.cs.purdue.edu/homes/rompf/papers/amin-popl18.pdf) - Amin & Rompf
- [libtcc documentation](https://bellard.org/tcc/tcc-doc.html)
- [ASAP Memory Management](https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-908.pdf)
