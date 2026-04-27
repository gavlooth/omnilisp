# Omni Lisp

Omni Lisp is a Lisp dialect with strict-arity lambdas, multiple dispatch,
algebraic effects, and delimited continuations. The runtime is deterministic
and scope/region-based, implemented in C3 with JIT and AOT compilation paths.

This README is aligned with the current syntax and language docs:
- [Syntax Spec](docs/SYNTAX_SPEC.md)
- [Language Spec](docs/LANGUAGE_SPEC.md)
- [Docs Map](docs/README.md)

## Status

Release status is tracked in [Release Status](docs/RELEASE_STATUS.md).

As of 2026-03-10:
- language feature work is considered complete at current baseline,
- full release validation is expected to run via Docker-bound gates.

## Syntax Quick Start

The canonical syntax details are in [docs/SYNTAX_SPEC.md](docs/SYNTAX_SPEC.md).
The examples below match that spec.

### Core Expressions

```lisp
(define x 10)
(define y 20)
(+ x y)

(if (> x 5) "big" "small")
(block (println "a") (println "b") 42)
```

### `let` Bindings and Destructuring

```lisp
;; flat pair bindings
(let (x 1 y 2) (+ x y))

;; array destructuring
(let ([a b .. rest] [10 20 30 40]) (+ a b))

;; dict destructuring
(let ({name age} {name "Alice" age 30}) name)
```

### Functions and Arity

```lisp
;; strict arity
(define (add2 a b) (+ a b))

;; variadic parameter with ..
(define (collect first .. rest) rest)

;; canonical lambda form (`lambda` remains accepted as a long alias)
(λ (x y) (+ x y))
```

### Collections and Access

```lisp
;; list
'(1 2 3)

;; array literal (desugars to (Array ...))
[1 2 3]

;; dict literal (desugars to (Dictionary ...)); bare symbol keys auto-quote
{a 1 b 2}

;; path and index access
point.x
arr.[0]
dict.['key]
```

### Concise Literals and Strings

```lisp
#xFF                         ;; => 255
#b1010                       ;; => 10
#o755                        ;; => 493

(str "hello {name}")          ;; interpolates through String coercion
#hex "ff 0a 1b 00"            ;; => [255 10 27 0]
#time "2024-01-15T10:30:00Z"  ;; => TimePoint
#uuid "550e8400-e29b-41d4-a716-446655440000"
```

### Mutation

```lisp
(set! name value)
(set! pair.car value)
(set! pair.cdr value)
(set! collection key value) ; generic Array/Dictionary update
```

`set!` returns `Void` on success.

### Pattern Matching

```lisp
(match expr
  ((Some x) x)
  (None 0)
  (_ -1))
```

### Effects and Continuations

```lisp
;; delimited continuations
(checkpoint (+ 1 (capture k (k 10))))

;; algebraic effects
(handle
  (signal 'ask "name")
  (ask arg (resolve "ok")))
```

## Language Rules That Matter

- Truthiness:
  - falsy: `nil`, `false`
  - truthy: everything else
- Collections: list, array, dict.
- Dispatch is explicit and type-driven; no implicit widening/coercion in method match.
- Constructors are explicit conversion points.

Canonical type names in docs/runtime are descriptive:
`Integer`, `Double`, `String`, `Symbol`, `List`, `Array`, `Dictionary`,
`Set`, `Iterator`, `Coroutine`, `TimePoint`, `Boolean`, `Nil`, `Void`.

Canonical naming is preferred in all surface docs and examples.
`Dict` remains the only shorthand for `Dictionary` where needed.

## Build and Run

```bash
# full integration build
OMNI_HOST_TOOLCHAIN_LIB_PATH="${OMNI_HOST_TOOLCHAIN_LIB_PATH:-/usr/local/lib}"
LIBRARY_PATH="$OMNI_HOST_TOOLCHAIN_LIB_PATH${LIBRARY_PATH:+:$LIBRARY_PATH}" c3c build

# run the repo-local main binary directly
# installed/user-facing CLI examples elsewhere in the docs use `omni`
LD_LIBRARY_PATH="$OMNI_HOST_TOOLCHAIN_LIB_PATH${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" ./build/main
```

`project.json` searches `build`, `/usr/local/lib`, and `deps/lib` for native
link dependencies such as `lightning`, `replxx`, `omni_chelpers`, and
`omni_ftxui`. Set `OMNI_HOST_TOOLCHAIN_LIB_PATH` when those libraries live in a
different local toolchain prefix, for example `$HOME/.local/lib`.

For routine iteration, prefer the fast dev build:

```bash
# fast developer build
scripts/build_fast_dev.sh

# run the lean dev binary
LD_LIBRARY_PATH="$OMNI_HOST_TOOLCHAIN_LIB_PATH${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" ./build/dev-fast/main-dev --eval '(+ 1 2)'
```

If you are explicitly not working on the deduce runtime, there is a narrower
optional profile:

```bash
scripts/build_fast_nodeduce_dev.sh
LD_LIBRARY_PATH="$OMNI_HOST_TOOLCHAIN_LIB_PATH${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" ./build/dev-fast-nodeduce/main-dev-nodeduce --eval '(+ 1 2)'
```

Current local baseline on this repo:
- `c3c build`: about 15s
- `scripts/build_fast_dev.sh` clean build: about 2.0s
- `scripts/build_fast_nodeduce_dev.sh` clean build: about 2.0s
- `scripts/build_fast_dev.sh` unchanged no-op: about 0.06s
- `scripts/build_fast_nodeduce_dev.sh` unchanged no-op: about 0.06s

To inspect what still dominates the lean target without compiling:

```bash
scripts/build_fast_dev.sh --profile
```

That reports the included source count, total included C3 size, and the largest
files still linked into `main-dev`, along with the largest source groups.

The deduce-free profile is available through:

```bash
OMNI_FAST_DEV_PROFILE=nodeduce scripts/build_fast_dev.sh --profile
```

Current profile output shows the remaining weight is dominated by `eval` and
`jit`, not by optional surfaces like `deduce`.

Use `c3c build` as the integration/full-runtime build. Use
`scripts/build_fast_dev.sh` for the default edit/build loop when you do not
need embedded test-suite support.

Do not run multiple `c3c build` processes against the same `build/` tree at the
same time. Parallel builds can collide on shared object outputs and fail during
link.

`build_fast_dev.sh` omits the test-entry wiring and all in-tree test sources, so
it avoids recompiling the test surface, reuses the prebuilt helper archive, and
it does not contend with the main `build/obj` tree.

`main-dev` currently supports the default iteration loop:
- `--eval`
- `--repl`
- `--repl --project [dir]`
- `--repl --load <file>`
- `--check`
- script execution (`./build/dev-fast/main-dev file.omni`)

`main-dev-nodeduce` supports the same loop, but `deduce`/`deduce/*` are not
registered and evaluate as unbound names.

Use the full `build/main` binary for
`--test-suite`, `--gen-e2e`, `--stack-affinity-probe`, `--language-ref`,
`--lang-ref`, `--manual`, `--init`, `--bind`, `--build`, and `--compile`.

`main-dev` also strips the `pika` module from the fast-build path. Core
evaluation, REPL, script execution, and non-regex schema flows still work, but
`pika`-backed regex schema validation is not a parity target for the lean dev
binary.

The fast target also omits the compiler/AOT/bindgen source families entirely.
That is why it is materially faster, and also why the full
`build/main` binary remains the required
integration build.

## Validation

```bash
# generate + compile + run e2e compiler output parity
scripts/run_e2e.sh
```

For full gate policy and Docker-capped validation paths, use:
- [Project Tooling](docs/PROJECT_TOOLING.md)
- [AGENTS guidance](AGENTS.md)

## Editor Tooling

First-party editor integration scaffolds live under `tooling/`:

- `tooling/tree-sitter-omni` for Tree-sitter grammar and queries
- `tooling/omni-lsp` for a thin stdio language server
- `tooling/omni-nvim` for Neovim REPL workflow integration

Quick bootstrap (from this repo checkout):

```bash
# 1) verify CLI is available
omni --version

# 2) optional: verify Tree-sitter grammar parses a sample
cd tooling/tree-sitter-omni
npm run generate
npm run parse

# 3) run first-party LSP server
cd /path/to/Omni
python3 tooling/omni-lsp/omni_lsp.py
```

Neovim setup is documented in:
- `tooling/omni-nvim/README.md`
- `docs/PROJECT_TOOLING.md` (editor tooling bootstrap section)

## Documentation Index

- [Docs Map](docs/README.md)
- [Language Spec](docs/LANGUAGE_SPEC.md)
- [Syntax Spec](docs/SYNTAX_SPEC.md)
- [Type and Dispatch Syntax](docs/type-system-syntax.md)
- [Effects Guide](docs/EFFECTS_GUIDE.md)
- [Architecture](docs/ARCHITECTURE.md)
- [Project Tooling](docs/PROJECT_TOOLING.md)
- [Reference Manual](docs/OMNI_REFERENCE.md)
- [Memory Changelog (implementation truth)](memory/CHANGELOG.md)
