# Omni Lisp — Project Tooling

**Last updated:** 2026-04-12

Omni provides CLI commands for creating projects and auto-generating FFI bindings from C headers.

---

## Install CLI + Man Page

Install `omni` under a prefix (default `~/.local`) and install both manual pages:
- `omni(1)` CLI reference
- `omni-language(7)` language quick reference

```bash
scripts/install_omni.sh install
```

Uninstall from the same prefix:

```bash
scripts/install_omni.sh uninstall
```

Useful overrides:

```bash
scripts/install_omni.sh install --prefix /usr/local
scripts/install_omni.sh install --binary /path/to/omni-build
```

Verify:

```bash
omni --version
man -M "$HOME/.local/share/man" 1 omni
man -M "$HOME/.local/share/man" 7 omni-language
```

---

## Editor Tooling

First-party editor tooling scaffolds are kept in `tooling/` so they can track
the current Omni syntax and CLI behavior:

- `tooling/tree-sitter-omni/` for Tree-sitter grammar and highlight queries
- `tooling/omni-lsp/` for diagnostics, completion, hover, and document symbols over stdio LSP
- `tooling/omni-nvim/` for structured REPL-driven Neovim workflows

These packages are intentionally thin. They reuse the installed `omni` binary
where possible instead of carrying a second parser/runtime stack inside editor
plugins.

### Editor Tooling Bootstrap (Roadmap Slice)

Minimal setup from a repo checkout:

```bash
# Omni CLI availability
omni --version
```

Tree-sitter grammar sanity check:

```bash
cd tooling/tree-sitter-omni
npm run generate
npm run parse
```

Run the first-party LSP server over stdio:

```bash
cd /path/to/Omni
python3 tooling/omni-lsp/omni_lsp.py
```

Neovim plugin wiring:

```lua
{
  dir = "/path/to/Omni/tooling/omni-nvim",
  init = function()
    vim.filetype.add({
      extension = {
        omni = "omni",
      },
    })
  end,
  ft = { "omni" },
  config = function()
    require("omni").setup({
      cmd = { "omni", "--repl", "--json" },
      repl = { mode = "json" },
      eval = { cmd = { "omni", "--eval", "--json" } },
      lsp = { auto_setup = true },
    })
  end,
}
```

If your plugin manager already exposes the plugin's `ftdetect/` files before
the lazy-load point, you can omit the `init` block. Keep it when `.omni`
filetype detection is otherwise missing.

Related package docs:
- `tooling/tree-sitter-omni/README.md`
- `tooling/omni-lsp/README.md`
- `tooling/omni-nvim/README.md`

---

## Formatting and Indentation Policy

Current policy is intentionally phased:

- phase 1: indentation-first tooling (`omni-lsp` document/range/on-type
  indentation behavior),
- phase 2: canonical full formatter behind a dedicated CLI entrypoint
  (`omni --fmt` or equivalent) once rule coverage and migration are stable.

Canonical formatting rules to preserve across editor tooling:

- special forms:
  - keep head symbol first (`if`, `let`, `block`, `handle`, `match`, etc.),
  - single-line forms may stay single-line when concise,
  - multiline forms use one argument/form per line with aligned indentation.
- array/dictionary literals:
  - keep inline when short and legible,
  - for multiline literals, one element/entry per line,
  - align closing delimiter with the opening form indentation.
- `handle` clauses:
  - one clause per line/block,
  - clause body forms are indented under the clause head,
  - preserve explicit `(resolve ...)` placement instead of collapsing handler
    control-flow forms into one line.
- type/method declarations:
  - keep declaration head and name grouped for readability,
  - place parameter/type annotations consistently with one parameter per line in
    multiline declarations,
  - avoid reflow that obscures dispatch-relevant annotation structure.
- macro/template blocks:
  - keep `syntax-match` and `template` structures vertically explicit,
  - keep each pattern arm visually isolated,
  - preserve `(insert ...)` and `(splice ...)` boundaries without inline
    collapsing that harms readability.

Implementation note:
- the current `omni --fmt` path is intentionally conservative:
  normalize leading indentation from structural depth, align wrapped
  `export`/`export-from` payloads, align wrapped `let` binding lists, preserve
  repo-style `if` branch indentation including nested `if` bodies that start
  later in a clause line, keep block-style `when` / `unless` / `raise` /
  `checkpoint` bodies out of generic continuation alignment, keep inline block
  forms inside `let` binding lists aligned to their binding context, keep
  higher-order collection-call lambda bodies (`map` / `foldl` / `foldr` /
  `filter` / `find` / `partition` / `sort-by` / `for-each`) aligned from the
  lambda's own opening column, keep multiline `Coroutine (λ ...)` bodies
  on the current in-tree wrapper-lambda layout, keep multiline clause bodies
  and inline dictionary/array payload entries aligned from their opening delimiter
  in current `match` arms and data literals, align wrapped generic call and
  pipeline continuations under their first argument, trim trailing whitespace,
  preserve blank lines, preserve original line-ending style, and avoid
  aggressive intra-line rewrites.

---

## Validation Lanes

Keep validation scope aligned with the change family:

- `memory-lifetime-smoke`, `memory-lifetime-bench`, `memory-lifetime-soak`,
  and `memory-stress` are the container-bound lanes for boundary/lifetime
  changes.
- `allocator-validation` and `allocator-bench` are separate lanes for AST
  allocator correctness and throughput work.
- Syntax/compiler-only changes should stay on their own non-memory lane and do
  not implicitly require memory-ownership coverage.
- FTXUI surface changes should run `scripts/run_ftxui_smoke.sh` after rebuild
  and before broader gates so the example subtree stays exercised.
- AArch64 stack backend evidence for `STACK-AARCH64-CONT-001` should be recorded
  with the exact host/runtime checks that passed on 2026-04-08:
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite stack` -> `Stack engine: 23 passed, 0 failed`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(resume (Coroutine (λ () (+ 1 2))))"` -> `3`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(handle (+ 1 (signal ask 0)) (ask x (resolve 10)))"` -> `11`
  - continuation multi-shot parity closure checks:
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(handle (+ 1 (signal ask 0)) (ask x (with-continuation k (k 41))))"` -> `42`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(checkpoint (+ 1 (capture k (k 10))))"` -> `11`
    - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-effect-continuation OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=56 fail=0`

---

## CLI Commands

### `--repl --project` — Start a Project-Aware REPL

```bash
omni --repl --project
omni --repl --project myproject
```

Starts the normal REPL after preloading the project's `src/main.omni`.

Current behavior:

- with no directory argument, `--project` uses the current working directory as
  the project root,
- with a directory argument, it uses that directory as the project root,
- it requires `<project>/omni.toml`,
- it loads and evaluates `<project>/src/main.omni` before entering the REPL,
- relative imports inside that entry file resolve from `src/`, matching normal
  script/load behavior,
- `--json` is intentionally not supported with REPL preload, because loading
  the project can emit ordinary program stdout before the JSON transport begins.

### `--repl --load` — Start a File-Preloaded REPL

```bash
omni --repl --load demo.omni
omni --repl --load /path/to/file.omni
```

Starts the normal REPL after loading one Omni file.

Current behavior:

- accepts one Omni source file path,
- evaluates that file before the REPL loop starts,
- resolves relative imports from the loaded file's directory,
- works for standalone example trees that are not full Omni projects,
- text-mode preload read failures preserve their concrete file-read cause
  (`file not found`, `permission denied`, `invalid path`, or generic
  `read failed`),
- is also intentionally incompatible with `--json` for the same stdout/protocol
  reason as `--project`.

### `--check` — Parse/Check Source Without Executing It

```bash
omni --check script.omni
omni --check --json script.omni
```

Runs a non-executing parse/check pass over an Omni source file.

Current behavior:

- does not execute user code,
- returns exit code `0` for clean input,
- returns non-zero for syntax, lowering, and read failures,
- `--json` emits structured diagnostics with 0-based ranges.

### `--fmt` — Apply the Conservative Formatter

```bash
omni --fmt script.omni
omni --fmt --write script.omni
omni --fmt --check script.omni
```

Runs Omni's current first-party formatter contract over one source file.

Current behavior:

- default mode prints formatted source to stdout,
- `--write` rewrites the file in place,
- `--check` prints nothing and returns `0` when the file is already formatted,
  `1` when formatting changes would be applied,
- formatting is intentionally conservative:
  - normalize leading indentation from structural depth,
  - align wrapped `export` / `export-from` payload lines,
  - align wrapped `let` binding-list continuation lines,
  - preserve repo-style `if` branch indentation, including nested `if` bodies
    that begin later in a clause line,
  - keep `when` / `unless` / `raise` / `checkpoint` bodies on block-style
    indentation instead of treating them as generic continuations,
  - keep inline block forms inside `let` binding lists aligned to their
    binding context,
  - keep multiline higher-order lambda bodies aligned from the lambda's own
    opening column for current in-tree collection calls such as `map`,
    `foldl`, `foldr`, `filter`, `find`, `partition`, `sort-by`, and
    `for-each`,
  - keep multiline `Coroutine (λ ...)` bodies on the current in-tree
    wrapper-lambda layout instead of flattening them toward generic lambda
    indentation,
  - keep multiline clause bodies and inline dict/array payload entries aligned
    from their opening delimiter in current `match` arms and data literals,
  - align wrapped generic call and pipeline continuation lines under their
    first argument,
  - trim trailing whitespace,
  - preserve blank lines,
  - preserve the source file's existing newline style (`LF` vs `CRLF`),
  - avoid aggressive intra-line rewrites.

### `--eval` — Evaluate One Expression

```bash
omni --eval '(+ 1 2)'
omni --eval --json '(+ 1 2)'
```

Runs one Omni expression through the normal runtime path.

Current behavior:

- evaluates exactly one CLI-supplied expression string,
- returns rendered value output in text mode,
- returns structured success/error payloads in `--json` mode,
- reserves stdout for the JSON payload in `--json` mode; user-facing
  `(print ...)`/`(display ...)` output is emitted on stderr instead,
- returns non-zero on evaluation failure.

### `--describe` — Symbol Help Surface

```bash
omni --describe define
omni --describe --json define
```

Returns first-party help for core special forms and selected builtin symbols.

Current behavior:

- text mode prints symbol, kind, and documentation,
- `--json` emits machine-readable payload:
  - `ok`,
  - `symbol`,
  - `kind`,
  - `documentation`,
  - `error` for unknown symbols.
- returns exit code `0` for known symbols and non-zero for unknown/usage errors.

### `--test-suite` — Structured Test Execution

```bash
omni --test-suite all
omni --test-suite all --json
```

`--json` emits one machine-readable payload that includes per-suite pass/fail and exit
code:

- `ok` (`true`/`false`)
- `requested_suite`
- `suites` array with:
  - `name`
  - `pass`
  - `fail`
  - `code` (`0` success, `1` suite failure)
- `totals` object with aggregate counters and final status code

Current behavior:

- JSON mode runs the same suite selection semantics as text mode:
  - `all` (default when omitted), `stack`, `scope`, `lisp`
- non-JSON mode keeps the existing human output
- returns non-zero on suite failures, same as text mode
- invalid suite selection still returns exit code `2`

### `--repl --json` — Structured JSON REPL Transport

```bash
printf '%s\n' '{"id":"1","input":"(+ 1 2)","mode":"expr"}' | omni --repl --json
```

Runs a persistent newline-delimited JSON REPL transport for tools.

Current behavior:

- accepts one JSON request per input line,
- supports `mode: "expr"` and `mode: "program"`,
- returns one JSON response per request line,
- reserves stdout for protocol JSON only; user-facing `(print ...)`/`(display ...)`
  output is emitted on stderr instead,
- exits cleanly on stdin EOF,
- emits one machine-readable startup error object on stdout when preflight fails
  before request processing; incompatible preload flags currently return
  `cli/usage` and interpreter bootstrap failure returns
  `runtime/bootstrap-failed`.

Current first-party usage:

- `tooling/omni-nvim/` uses this transport for persistent editor requests,
- current-line and current-form eval still prefer `omni --eval --json` for
  one-shot calls,
- whole-buffer sends and REPL fallback calls now go through the structured
  session instead of scraping text REPL output.

Future direction:

- for a real network-capable tool protocol, see
  `docs/plans/repl-server-protocol-2026-03-30.md`; phase 1 of that direction
  now exists as `--repl-server --socket <path>`,
  `--repl-server --stdio`, and `--repl-server --tcp <host> <port>`.
  The main remaining work is richer transport/runtime concurrency.

### `--repl-server --socket` — Unix-Socket REPL Server

```bash
omni --repl-server --socket /tmp/omni.sock
omni --repl-server --socket /tmp/omni.sock --project
omni --repl-server --socket /tmp/omni.sock --project myproject
```

Runs a local REPL server over a Unix domain socket using newline-delimited JSON.

Current phase-1 behavior:

- binds a filesystem Unix socket at the requested path,
- makes the filesystem Unix socket owner read/write only (`0600`) before
  listening, so other local users cannot attach to unauthenticated socket
  sessions through a permissive parent directory,
- emits one machine-readable startup error object on stdout when CLI/preflight
  setup fails before any client attaches; invalid invocation uses `cli/usage`
  and bind failure uses `io/listen-failed`,
- accepts one JSON request per line and returns one or more JSON events,
- supports explicit per-connection sessions via `clone` and `close`,
- accepts `--project [dir]`; when present, each new `clone` session resolves
  the project root the same way as `omni --repl --project` and preloads
  `src/main.omni` before the session is announced,
- when that per-clone preload fails, `clone` emits a structured `error`
  event before the normal `session` / `done` announcement, and the session
  stays alive for follow-up requests,
- supports the current ops:
  - `describe`,
  - `clone`,
  - `close`,
  - `complete`,
  - `eval`,
  - `interrupt`,
  - `stdin`,
  - `load-file`,
- supports `mode: "expr"` and `mode: "program"` on `eval`,
- captures user-facing `(print ...)` / `(display ...)` output as protocol `out`
  events instead of writing raw stdout/stderr on the socket stream,
- routes `stdin` request data into the session input queue used by
  `(read-line)`, with optional `eof: true` to close that routed input,
- ends each successful request with `done`,
- returns structured `error` events for invalid requests, unknown sessions, and
  evaluation failures,
- removes the socket path on process exit.

Current limitations:

- one runtime worker services one stream at a time, so non-`interrupt`
  requests receive `protocol/server-busy` while another request is in flight,
- one server process still services one client connection at a time,
- socket listeners support sequential client reuse, but concurrent
  cross-connection servicing is still deferred because interpreter attachment
  remains owner-thread constrained,
- first-party Neovim integration still uses `--repl --json`; the socket server
  is available now for new tool integrations.

### `--repl-server --stdio` — Stdio REPL Server

```bash
printf '%s\n' '{"id":"1","op":"describe"}' | omni --repl-server --stdio
printf '%s\n' '{"id":"1","op":"clone"}' | omni --repl-server --stdio --project ./demo
```

Runs the same JSON-line REPL server protocol directly on stdin/stdout.

Current behavior:

- uses the same request/event protocol as `--repl-server --socket`,
- emits one machine-readable startup error object on stdout instead of
  plaintext CLI usage text when preflight fails before request processing,
- accepts `--project [dir]` with the same per-clone preload behavior as the
  socket and TCP transports,
- emits a structured preload `error` event before the normal `session` /
  `done` announcement when that per-clone preload fails,
- supports the same current ops:
  - `describe`,
  - `clone`,
  - `close`,
  - `complete`,
  - `eval`,
  - `interrupt`,
  - `stdin`,
  - `load-file`,
- advertises `stdio` in the `describe` transport list,
- is useful for tool adapters that want multi-event responses without managing
  a socket path,
- returns `interrupted` for cancelled in-flight `eval` / `load-file` requests,
- routes `stdin` request data into `(read-line)` for the targeted in-flight
  request, and accepts `eof: true` to close routed input,
- returns `protocol/server-busy` for non-`interrupt` requests submitted while a
  prior request is still running,
- still differs from the older `--repl --json` transport:
  - `--repl-server --stdio` is session/ops/event based,
  - `--repl --json` is the older request/result stdio REPL transport.

### `--repl-server --tcp` — TCP REPL Server

```bash
OMNI_REPL_TCP_AUTH_TOKEN=dev-token omni --repl-server --tcp 127.0.0.1 5555
OMNI_REPL_TCP_AUTH_TOKEN=dev-token omni --repl-server --tcp 127.0.0.1 5555 --project
OMNI_REPL_TCP_AUTH_TOKEN=dev-token omni --repl-server --tcp 127.0.0.1 5555 --project myproject
```

Runs the same JSON-line REPL server protocol over a TCP listener.

Current behavior:

- uses the same request/event protocol as the Unix-socket server,
- accepts only loopback bind hosts (`127.0.0.1`, `localhost`, and `::1`) and
  fails preflight for non-loopback TCP hosts,
- requires `OMNI_REPL_TCP_AUTH_TOKEN` at startup; TCP clients must authenticate
  with the configured token before normal request handling,
- emits one machine-readable startup error object on stdout when preflight
  fails before any client attaches; listener and discovery-file startup
  failures now use stable JSON error codes instead of plaintext text,
- advertises `tcp` in the `describe` transport list,
- supports the same current ops, including routed `stdin`,
- accepts `--project [dir]`; each new `clone` session preloads the resolved
  project `src/main.omni` before emitting its `session` event,
- emits a structured preload `error` event before the normal `session` /
  `done` announcement when that per-clone preload fails,
- writes `.omni-repl-port` in the current working directory after the TCP
  listener binds successfully; the file currently contains the bound port
  followed by a newline,
- is intended for explicit local/remote attach experiments where a filesystem
  socket path is awkward.

Current limitations:

- token authentication is built in for TCP mode, but no TLS layer is built into
  the transport; use loopback binding or an external tunnel when confidentiality
  is required,
- accepted clients are handled by per-client threads, but each session still
  follows the same interpreter attachment and session ownership constraints as
  the socket and stdio server transports,
- abrupt termination can leave a stale `.omni-repl-port`, so clients should
  treat it as a discovery hint and still verify connect,
- richer shared-interpreter concurrency remains constrained by interpreter
  attachment ownership.

---

## Exit Status Map

The CLI mostly follows a simple `0` success / `1` failure convention.
`--test-suite` is the main exception because it distinguishes invalid suite
selection from normal suite failure.

- `--check`
  - `0` on clean input
  - `1` on usage, read, parse, or lowering/check failure
- `--eval`
  - `0` on successful evaluation
  - `1` on usage or evaluation failure
- `--describe`
  - `0` on known symbol
  - `1` on unknown symbol or usage failure
- `--test-suite`
  - `0` on success
  - `1` on suite failure or stack-affinity harness failure
  - `2` on invalid suite name
  - `--json` returns the same exit status semantics plus the JSON envelope:
    - top-level `code` inside `totals`
- `--build`, `--bind`, `--compile`, `--init`, `--repl`, `--gen-e2e`,
  `--language-ref`, `--lang-ref`, `--manual`, `--version`, and `--help`
  - `0` on success
  - `1` on failure or missing required inputs

### `--language-ref` — Print Built-In Full Language Reference

```bash
omni --language-ref
```

Alias flags: `--lang-ref`, `--manual`.

Prints the built-in full language reference directly from the executable
without requiring external docs on disk.

### `--init` — Scaffold a New Project

```bash
omni --init myproject
```

Creates a complete project directory:

```
myproject/
  omni.toml             # Project config (the only file you edit)
  src/
    main.omni           # Entry point
  lib/
    ffi/                # Auto-generated FFI bindings go here
  include/              # Drop C headers here for local use
  build/                # Build artifacts
    project.json        # C3 build config (generated — do not edit)
```

`--init` now fails explicitly if a scaffold subpath already exists as a non-directory and rolls back the new project root if a later file write fails, so a failed run does not leave a half-created tree behind.

The generated `src/main.omni` contains a hello-world program:

```lisp
(println "Hello from myproject!")
```

Recommended editor/tooling setup after `--init`:

```bash
cd myproject
omni --check src/main.omni
```

If developing from this repo checkout, then wire first-party tooling from
`tooling/`:

- Tree-sitter grammar: `tooling/tree-sitter-omni/`
- LSP server: `tooling/omni-lsp/omni_lsp.py`
- Neovim plugin: `tooling/omni-nvim/`

For exact setup snippets, use:
- `docs/PROJECT_TOOLING.md` (`Editor Tooling Bootstrap (Roadmap Slice)`)
- `tooling/omni-lsp/README.md`
- `tooling/omni-nvim/README.md`

### `--bind` — Generate FFI Bindings

```bash
omni --bind myproject/
omni --bind                 # uses current directory
```

Reads `omni.toml`, parses C headers using libclang, and writes Omni FFI modules to `lib/ffi/`. The generator now fails closed on overlong header paths and on header sets that exceed the current fixed parse scratch limit, instead of truncating the path or emitting a partial module.
Generated modules use declarative `ffi` forms. Interpreter/JIT enforces the
current `ForeignHandle` metadata dictionary policy, and compiler/AOT lowering
carries that policy into generated FFI declarations with handle descriptors for
parameters and returns.

**Requires:** libclang installed on the system (see [Dependencies](#dependencies) below).

### `--build` — AOT Binary Build

```bash
omni --build input.omni -o output
```

Current shipped backend contract:

- loads the input Omni source, lowers it to generated C3, and stages that
  generated file under `build/_aot_temp_*.c3`,
- invokes `c3c compile` against the checked-in runtime sources plus the staged
  generated file,
- excludes `src/lisp/tests*` from production AOT source collection,
- links the runtime support libraries required by the shipped backend rather
  than a tiny standalone-only subset: `omni_chelpers`, GNU Lightning, libffi,
  libuv, replxx, utf8proc, libdeflate, yyjson, BearSSL, LMDB, `libdl`, and
  `libm`.

Practical consequence:

- `--build` produces a standalone executable artifact, but it is not a
  minimal-libc-only backend; it currently ships with the same runtime support
  stack the generated program depends on inside this repo.

---
