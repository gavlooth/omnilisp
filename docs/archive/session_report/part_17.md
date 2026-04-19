    - `scripts/run_validation_container.sh bash -lc 'set +e; env LD_LIBRARY_PATH=/usr/local/lib ./build/main --init > /tmp/omni_init_missing2.out 2>&1; rc=$?; ...'` -> `rc=1`, `Usage: omni --init <project-name>`
- Canonical CLI alias cleanup follow-up:
  - The dispatcher still accepted three undocumented single-dash aliases, `-help`, `-compile`, and `-repl`, even though the active public surface had already standardized on `--help`, `--compile`, and `--repl`. Keeping those odd aliases live would preserve non-canonical surface drift for no real benefit.
  - `src/entry.c3` now removes those alias spellings from dispatch, and `docs/LANGUAGE_SPEC.md` no longer advertises `-repl` as an alternate entrypoint. The canonical double-dash forms remain unchanged.
  - Bounded validation for this closure was:
    - `scripts/run_validation_container.sh c3c build`
    - `./build/main -help` -> `rc=1`, `Error: unknown option -help`
    - `./build/main -compile` -> `rc=1`, `Error: unknown option -compile`
    - `./build/main -repl` -> `rc=1`, `Error: unknown option -repl`
    - Allow-side check remained green: `./build/main --compile "$tmpfile" /tmp/omni_compile_alias_check.c3` -> `rc=0`, `Compilation successful: /tmp/omni_compile_alias_check.c3`
- Signature: Codex (GPT-5)

2026-03-28 - Example clone build convenience and import binding

- Objectives attempted:
  - Make the standalone example-only clone convenient to build with `./omni --build ...` instead of requiring the full Omni checkout workflow.
  - Audit the `--build` error path so the failure explains the repo-root requirement instead of only saying root resolution failed.
  - Fix the AOT import lowering so top-level module aliases like `ui` are actually bound in generated C3, instead of compiling to an undeclared symbol.
- Code/config changes made:
  - Added a local wrapper script in the example clone (`/home/heefoo/Documents/code/ftxui-example-clone/omni`) that forwards to a usable Omni build binary and prefers `OMNI_REPO_ROOT` when set.
  - Updated the example clone README to document `./omni --build ./demo.omni -o out` and the `OMNI_REPO_ROOT` override.
  - Improved `src/entry_build_mode.c3` so the repo-root resolution failure now includes a concrete hint about using a full Omni checkout, setting `OMNI_REPO_ROOT`, or using the example clone's wrapper.
  - Taught `src/entry_build_helpers.c3` to honor `OMNI_REPO_ROOT` during build-root resolution.
  - Added `lisp::aot::import_module(...)` in `src/lisp/aot_runtime_bridge.c3` to bridge top-level imports back into the existing runtime import machinery.
  - Changed `src/lisp/compiler_program_top_level_helpers.c3` and `src/lisp/compiler_program_top_level.c3` so top-level `import` forms emit real runtime import initialization and declare imported module aliases/selective bindings as globals.
- Experiment commands and key metrics:
  - `cd /home/heefoo/Documents/code/Omni && c3c build` -> pass, executable linked to `build/main`.
  - `cd /home/heefoo/Documents/code/ftxui-example-clone && ./omni --build ./demo.omni -o out` -> pass, executable linked to `/home/heefoo/Documents/code/ftxui-example-clone/out`.
  - `cd /home/heefoo/Documents/code/ftxui-example-clone && printf 'q' | LD_LIBRARY_PATH=/usr/local/lib ./out` -> pass, rendered the FTXUI demo and exited cleanly.
- Best current checkpoint/config recommendation:
  - For the example clone, use `./omni --build ./demo.omni -o out` and then run `printf 'q' | LD_LIBRARY_PATH=/usr/local/lib ./out`.
  - For explicit full-repo builds from other working directories, set `OMNI_REPO_ROOT=/home/heefoo/Documents/code/Omni` so the wrapper can find the build binary and the compiler can resolve the repo root.
- Unresolved issues and next actions:
  - Selective-import and `import all` AOT coverage still deserves follow-up validation; the current fix covers the convenience path used by the example demo (`import "ui.omni"` and `import ui`).
  - If new import patterns fail, the next step is to extend the AOT import bridge to serialize selective bindings more explicitly instead of relying on the current module-alias path.
- Signature: Codex (GPT-5)

2026-03-28 - CLI and wrapper error-message audit

- Objectives attempted:
  - Tighten the remaining user-facing error messages in the build, script, eval, and example-clone wrapper paths so failures report the exact mode, file, or step that broke.
  - Remove the last generic “bootstrap failed” / “compilation failed” / “could not find a usable build binary” style messages that forced the user to guess what to check next.
- Code/config changes made:
  - Changed `src/lisp/aot_runtime_bridge.c3` so AOT import failures now mention the import target and, when available, the underlying runtime import error.
  - Changed `src/entry_script_mode.c3` so runtime bootstrap and source-directory allocation failures name the script file they were handling.
  - Changed `src/entry_check_mode.c3`, `src/entry_eval_mode.c3`, and `src/entry_runtime_modes.c3` so runtime-interpreter bootstrap failures name the specific CLI mode they belong to.
  - Changed `src/entry_build_mode.c3` so AOT backend compile failures mention both the generated temp C3 source and the output binary.
  - Changed `src/lisp/prim_io_file.c3` so `load` source-directory allocation failures include the loaded path.
  - Changed `/home/heefoo/Documents/code/ftxui-example-clone/omni` so the wrapper now prints the exact build-binary locations it checked before failing.
- Experiment commands and key metrics:
  - `cd /home/heefoo/Documents/code/Omni && c3c build` -> pass, executable linked to `build/main`.
  - `cd /home/heefoo/Documents/code/ftxui-example-clone && ./omni --build ./demo.omni -o out` -> pass, executable linked to `/home/heefoo/Documents/code/ftxui-example-clone/out`.
  - `cd /home/heefoo/Documents/code/ftxui-example-clone && printf 'q' | LD_LIBRARY_PATH=/usr/local/lib ./out` -> pass, rendered the FTXUI demo and exited cleanly.
  - Negative-path wrapper/build check: `cd /home/heefoo/Documents/code/ftxui-example-clone && ./omni --build /tmp/omni-bad-build.omni -o /tmp/omni-bad-build-out` -> parse error surfaced from the broken source, confirming the wrapper now reaches the build binary cleanly.
- Best current checkpoint/config recommendation:
  - Keep the clone workflow as `cd /home/heefoo/Documents/code/ftxui-example-clone && ./omni --build ./demo.omni -o out`.
  - If the user hits a failure, the next layer now reports the actual mode/file/step, so the remaining debugging path should be much shorter.
- Unresolved issues and next actions:
  - The audit did not cover every low-level runtime error in the tree; it focused on the CLI/build/wrapper messages that were actually observed as too generic.
  - If another generic message shows up in a new path, patch it directly instead of reusing a catch-all bootstrap/build phrase.
- Signature: Codex (GPT-5)

2026-03-28 - Broader message cleanup pass

- Objectives attempted:
  - Push the remaining obvious generic runtime/build/init errors toward mode- and path-specific wording instead of leaving them as bare “failed” summaries.
  - Cover the next layer of user-facing messages that showed up during the broader scan: JIT compile failures, bind/init composition failures, and the example-clone wrapper fallback.
- Code/config changes made:
  - Changed `src/lisp/runtime_backend_hooks.c3` so runtime JIT compile failures now say the failure happened while compiling an expression for runtime evaluation.
  - Changed `src/lisp/eval_run_pipeline.c3` so run/eval JIT compile failures now say they happened during program execution rather than as a generic compile error.
  - Changed `src/entry_bind_mode.c3` so the FFI summary error names the project directory it was binding.
  - Changed `src/entry_project_init_writers.c3` so init composition failures mention which generated file and project name they were composing.
  - Changed `src/entry_build_backend_compile.c3` so AOT backend command composition failures name both the temp source file and output binary.
  - Tightened the example-clone wrapper fallback in `/home/heefoo/Documents/code/ftxui-example-clone/omni` so it says the binary was not found in the checked locations.
- Experiment commands and key metrics:
  - `cd /home/heefoo/Documents/code/Omni && c3c build` -> pass, executable linked to `build/main`.
  - `cd /home/heefoo/Documents/code/ftxui-example-clone && ./omni --build ./demo.omni -o out` -> pass, executable linked to `/home/heefoo/Documents/code/ftxui-example-clone/out`.
  - `cd /home/heefoo/Documents/code/ftxui-example-clone && printf 'q' | LD_LIBRARY_PATH=/usr/local/lib ./out` -> pass, rendered the FTXUI demo and exited cleanly.
- Best current checkpoint/config recommendation:
  - Keep the clone wrapper as the convenience path for example work.
  - Treat remaining generic message fixes as opt-in follow-up if a user hits a new weak spot; the currently audited build/script/wrapper paths are now materially more descriptive than before.
- Unresolved issues and next actions:
  - Some low-level internal errors still use short wording because they are only surfaced in deep runtime failure paths and already carry enough local context for debugging.
  - If the user wants a second audit sweep, the next target should be the remaining `JIT compilation failed`-style runtime internals and the init/bind helpers that still summarize multi-step failures.
- Signature: Codex (GPT-5)

2026-03-29 - REPL preload commands

- Objectives attempted:
  - Add a first-class CLI path for starting the REPL with the current Omni project already loaded instead of requiring a manual `(load "src/main.omni")`.
  - Cover standalone example/workspace trees that are not full Omni projects, so REPL preload is not gated on `omni.toml`.
  - Keep the plain text REPL behavior intact while preserving the structured `--repl --json` transport contract.
  - Document the shipped REPL/project contract in the CLI help and user-facing docs.
- Code/config changes made:
  - Extended `src/entry_runtime_modes.c3` with REPL option parsing for both `--project [dir]` and `--load <file>`, plus preload execution before entering the interactive loop.
  - Reused the existing script/import source-dir behavior during preload so relative imports inside `src/main.omni` resolve from the entry file directory.
  - Kept `--project` as the strict `omni.toml` path and added `--load` for standalone files such as the FTXUI example clone's `demo.omni` / `smoke.omni`.
  - Added explicit rejection for REPL preload on `--json` because preload code can emit ordinary stdout before the JSON transport begins, which would corrupt the protocol.
  - Updated `src/entry_cli_help_version.c3`, `docs/LANGUAGE_SPEC.md`, `docs/PROJECT_TOOLING.md`, and `README.md` to advertise both preload entrypoints and the text-only restriction.
- Experiment commands and key metrics:
  - `c3c build` -> pass, executable linked to `build/main`.
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --repl --project demo </dev/null` -> `rc=0`; preloaded `demo/src/main.omni`, printed `"Hello from demo!"`, entered the REPL banner, and exited cleanly on EOF.
  - `cd demo && env LD_LIBRARY_PATH=/usr/local/lib ../build/main --repl --project </dev/null` -> `rc=0`; confirmed the no-arg current-directory project resolution path loads `/home/heefoo/Documents/code/Omni/demo/src/main.omni`.
  - `cd /home/heefoo/Documents/code/ftxui-example-clone && omni --repl --load smoke.omni </dev/null` -> `rc=0`; confirmed standalone example trees without `omni.toml` can preload one file and still reach the REPL banner cleanly.
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --repl --project --bogus` -> `rc=1`, `Error: unexpected argument after --repl: --bogus`.
  - `cd /home/heefoo/Documents/code/ftxui-example-clone && omni --repl --load demo.omni </dev/null` -> preload reached the live FTXUI demo event loop as expected; this confirmed the file preload path works, but the UI example is not suitable as an EOF-to-banner smoke.
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --repl --project demo --json` -> `rc=1`, `Error: REPL preload is not supported with --json`.
  - `cd /home/heefoo/Documents/code/ftxui-example-clone && omni --repl --load demo.omni --json` -> `rc=1`, `Error: REPL preload is not supported with --json`.
- Best current checkpoint/config recommendation:
  - Use `omni --repl --project` from inside a project root, or `omni --repl --project <dir>` from elsewhere, when the goal is an interactive session with `src/main.omni` already evaluated.
  - Use `omni --repl --load <file>` for standalone example/workspace trees that are not full Omni projects.
  - Keep editor/tool integrations on plain `omni --repl --json`; do not layer REPL preload onto the JSON transport until there is a structured bootstrap protocol that can carry preload stdout safely.
- Unresolved issues and next actions:
  - Project preload currently targets `src/main.omni` directly and does not consult richer `omni.toml` entrypoint metadata because that contract does not exist yet.
  - If project-aware editor transport becomes necessary, the next step is a dedicated JSON bootstrap handshake or structured preload result envelope rather than allowing raw preload stdout onto the JSON channel.
  - If the repo eventually grows a real per-project entrypoint field in `omni.toml`, `--project` should resolve through that metadata instead of hardcoding `src/main.omni`.
- Signature: Codex (GPT-5)

2026-03-29 - omni-nvim setup defaults and docs fix

- Objectives attempted:
  - Audit the first-party Neovim plugin after the user reported that the Omni plugin was not set up properly.
  - Verify whether the break was in the plugin runtime, its LSP defaults, or the documented lazy.nvim setup.
  - Align the plugin’s root detection and setup examples with the real Omni project layout.
  - Make the documented default `auto_start = true` behavior real on first Omni buffer open and clarify the Tree-sitter setup boundary.
- Code/config changes made:
  - Updated `tooling/omni-nvim/lua/omni/init.lua` and `tooling/omni-nvim/lua/omni/lsp.lua` so Omni LSP now defaults to root markers `omni.toml`, `project.json`, and `.git` instead of only `project.json` and `.git`.
  - Updated `tooling/omni-nvim/lua/omni/init.lua` so `apply_buffer()` now actually honors `auto_start` by scheduling `repl.start(config, { focus = false })` on the first Omni buffer activation.
  - Updated `tooling/omni-nvim/lua/omni/repl.lua` so background REPL startup can restore the source window after opening the transcript split instead of stealing focus from the edited Omni buffer.
  - Updated `tooling/omni-nvim/README.md`, `tooling/omni-nvim/doc/omni.nvim.txt`, and `docs/PROJECT_TOOLING.md` so the lazy.nvim example includes an `init = function() vim.filetype.add({ extension = { omni = "omni" } }) end` hook for setups where `.omni` filetype detection is not exposed before lazy loading.
  - Updated the same docs to advertise the corrected LSP root-marker defaults, the default REPL auto-start behavior, and the requirement to install the Omni Tree-sitter parser before expecting syntax highlighting.
- Experiment commands and key metrics:
  - `tooling/omni-nvim/scripts/run_smoke.sh` -> pass, `omni-nvim smoke: ok`.
  - `nvim --headless --clean -u NONE -i NONE --cmd 'set runtimepath^=/home/heefoo/Documents/code/Omni/tooling/omni-nvim' '+lua local omni=require("omni"); local cfg=omni.setup({}); print(table.concat(cfg.lsp.root_markers, ","))' '+qa'` -> printed `omni.toml,project.json,.git`.
  - `nvim --headless --clean -u NONE -i NONE --cmd 'set runtimepath^=/home/heefoo/Documents/code/Omni/tooling/omni-nvim' --cmd 'filetype plugin on' '+lua require("omni").setup({})' '+edit /home/heefoo/Documents/code/ftxui-example-clone/smoke.omni' '+lua vim.wait(300, function() return vim.fn.bufnr("Omni REPL") ~= -1 end)' '+lua print("ft=" .. vim.bo.filetype)' '+lua print("mapped=" .. tostring(vim.b.omni_nvim_mapped))' '+lua print("repl_buf=" .. vim.fn.bufnr("Omni REPL"))' '+qa'` -> printed `ft=omni`, `mapped=true`, `repl_buf=2`, confirming first-buffer auto-start now creates the REPL transcript without manual `:OmniReplStart`.
  - Local code inspection result: the stale root markers, the too-minimal lazy example, and the previously unwired `auto_start` path were the concrete repo-side plugin defects.
- Best current checkpoint/config recommendation:
  - For lazy.nvim, use the plugin with both `init = function() vim.filetype.add({ extension = { omni = "omni" } }) end` and `ft = { "omni" }`, then call `require("omni").setup(...)` in `config`.
  - Treat `omni.toml` as the primary Omni project root marker for editor/LSP setup; `project.json` remains a secondary compatibility/root hint, not the main project contract.
  - Treat REPL transcript startup and Tree-sitter parser installation as separate setup steps: `require("omni").setup(...)` should start the REPL wiring, but syntax highlighting still depends on the parser being installed and available to Neovim.
- Unresolved issues and next actions:
  - The plugin smoke still validates bootstrap/filetype wiring rather than a full lazy.nvim-managed installation path.
  - Some current Neovim and `nvim-treesitter` combinations still do not expose a fully automatic Omni parser-install path from the repo plugin alone; broader parser-install compatibility deserves its own follow-up slice instead of being folded into the REPL/LSP setup lane.
  - If the user still sees setup problems after this patch, the next step is to inspect their actual Neovim plugin-manager spec and startup ordering rather than changing the repo plugin blindly.
- Signature: Codex (GPT-5)

2026-03-30 - omni-nvim value pretty printing

- Objectives attempted:
  - Improve Omni Neovim transcript readability after the user reported that evaluated values had no pretty printing.
  - Keep the structured REPL and `--eval --json` protocol unchanged while making nested collection output easier to scan in-editor.
- Code/config changes made:
  - Updated `tooling/omni-nvim/lua/omni/repl.lua` with a plugin-side Omni value pretty-printer that parses rendered value strings and reflows nested lists, arrays, and dictionaries into an indented multiline layout when they exceed the configured width.
  - Wired that formatter into the transcript append path for both one-shot `--eval --json` responses and structured REPL replies.
  - Added output config defaults in `tooling/omni-nvim/lua/omni/init.lua` for `output.pretty_values = true`, `output.pretty_width = 72`, and `output.pretty_indent = 2`.
  - Updated `tooling/omni-nvim/README.md` and `tooling/omni-nvim/doc/omni.nvim.txt` to document the new pretty-printing behavior and the tuning knobs.
- Experiment commands and key metrics:
  - `tooling/omni-nvim/scripts/run_smoke.sh` -> pass, `omni-nvim smoke: ok`.
  - `c3c build` -> pass, executable linked to `build/main`.
  - Headless transcript probe with a larger nested value and `output.pretty_width = 60` -> the `Omni REPL` buffer now renders:
    - `=>`
    - `{`
    - `  nested`
    - `    {`
    - `      alpha [10 20 30 40]`
    - `      gamma "long-long-long-string"`
    - `      beta {deep [100 200 300 400]}`
    - `    }`
    - `  nums [1 2 3 4 5 6 7 8 9 10]`
    - `  name "omni"`
    - `  tags [ui repl nvim pretty]`
    - `}`
- Best current checkpoint/config recommendation:
  - Keep plugin-side pretty printing enabled by default so the user-facing transcript becomes readable without changing the REPL JSON contract.
  - Tune transcript shape with `output.pretty_width` before considering protocol changes; width is the main lever that determines when values break across lines.
- Unresolved issues and next actions:
  - The current pretty-printer is syntax-aware and bounded, but it is still a client-side layout pass over the rendered value string rather than a first-class runtime pretty printer.
  - Dictionary rendering currently preserves the runtime print order rather than imposing a sorted key order.
  - If stronger layout guarantees are needed later, the next step is a real runtime-side pretty-printer contract rather than piling more policy into the Neovim client.
- Signature: Codex (GPT-5)

2026-03-30 - Structured JSON stdio separation for print/display

- Objectives attempted:
  - Fix the broken Omni Neovim eval transcript the user hit when `(print ...)` wrote onto the same stdout channel as `--eval --json` and `--repl --json`.
  - Keep the structured JSON contract intact while still surfacing user-directed console output in editor integrations.
  - Make sure print-without-newline results show up in the Neovim transcript instead of staying buffered until process exit.
- Code/config changes made:
  - Added `InterpFlags.console_to_stderr` in `src/lisp/value_interp_state.c3` and initialized it in `src/lisp/value_interp_init_helpers.c3`.
  - Updated `src/lisp/prim_io.c3` so `print`, `println`, `display`, and `newline` continue to use the shared console path but target stderr whenever the interpreter is running inside a structured JSON transport.
  - Updated `src/entry_eval_mode.c3` and `src/entry_runtime_modes.c3` so `--eval --json` and `--repl --json` enable that stderr-routing flag before any user code executes.
  - Updated `tooling/omni-nvim/lua/omni/repl.lua` so one-shot eval appends successful stderr output before the eval result, and the persistent JSON REPL flushes partial stderr before appending the JSON reply. This fixes `(print 1)` in the transcript even without a trailing newline.
  - Updated `docs/PROJECT_TOOLING.md` and `docs/man/omni.1` to document the stdout/stderr split for structured transports.
- Experiment commands and key metrics:
  - `c3c build` -> pass, executable linked to `build/main`.
  - `tooling/omni-nvim/scripts/run_smoke.sh` -> pass, `omni-nvim smoke: ok`.
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval --json '(print 1)' >stdout 2>stderr` -> stdout now contains only `{"ok":true,"input":"(print 1)","value":"#<void>","error":null}`, stderr contains `1`.
  - `printf '%s\n' '{"id":"8","input":"(print 1)","mode":"expr"}' | env LD_LIBRARY_PATH=/usr/local/lib ./build/main --repl --json >stdout 2>stderr` -> stdout now contains only `{"id":"8","ok":true,"value":"#<void>","error":null}`, stderr contains `1`.
  - Headless Neovim eval probe over a file containing `(print 1)` now renders the transcript as:
    - `stderr| 1`
    - `>> form`
    - `(print 1)`
    - `=> #<void>`
- Best current checkpoint/config recommendation:
  - Treat stdout as protocol-only for `--eval --json` and `--repl --json`.
  - Keep user-directed console output on stderr in structured transports; first-party editor tooling already has a channel for displaying that stream without corrupting JSON parsing.
- Unresolved issues and next actions:
  - This fix covers the shared console primitives (`print`, `println`, `display`, `newline`). If other future user-visible runtime features write directly to stdout in structured mode, they need to follow the same contract instead of bypassing the console helper.
  - The transport contract is now explicit, so any future JSON-mode preload/bootstrap design should preserve the same stdout reservation rule.
- Signature: Codex (GPT-5)

2026-03-30 - REPL server protocol recommendation

- Objectives attempted:
  - Turn the “should Omni mimic nREPL, pREPL, or something else?” question into a concrete protocol recommendation instead of a loose comparison.
  - Define an Omni-native REPL server contract that is suitable for editor and tool integrations rather than only a human terminal REPL.
- Code/config changes made:
  - Added `docs/plans/repl-server-protocol-2026-03-30.md`, a concrete protocol proposal with:
    - recommended architecture (`nREPL` control plane + `pREPL` eval event model),
    - transport recommendation (Unix socket + newline-delimited JSON),
    - proposed CLI surface (`--repl-server`, optional `--stdio`/`--tcp`),
    - exact request/response examples for `describe`, `clone`, `close`, `eval`, `interrupt`, `stdin`, `load-file`, and `complete`,
    - migration guidance from the current `--repl --json` transport.
  - Updated `docs/PROJECT_TOOLING.md` to point from the current structured stdio transport to the new REPL server protocol note.
  - Updated `docs/README.md` to index the new protocol proposal directly.
- Experiment commands and key metrics:
  - Inspection-only design pass; no runtime execution was required because this slice records a protocol decision and recommended next implementation boundary rather than shipped server code.
- Best current checkpoint/config recommendation:
  - Treat `nREPL` as the right model for sessions, ops, interrupts, and capability discovery.
  - Treat `pREPL` as the right model for streamed `out` / `err` / `value` style eval events.
  - Use JSON lines over a Unix socket as the first Omni-native wire protocol instead of copying nREPL’s exact bencode/EDN transports.
- Unresolved issues and next actions:
 - The protocol is still proposed, not implemented.
 - The next practical implementation slice is a minimal `--repl-server` with Unix-socket transport plus `describe`, `clone`, `close`, `eval`, `interrupt`, and `load-file`.
 - TCP and TLS should remain deferred until bind/auth policy is explicit.
- Signature: Codex (GPT-5)

2026-03-30 - Phase-1 Unix-socket REPL server

- Objectives attempted:
  - Turn the protocol recommendation into a real shipped server slice instead of leaving `--repl --json` as the only structured tool transport.
  - Preserve the core transport invariant that program output must stay inside protocol events rather than corrupting the wire stream.
  - Ship the smallest credible nREPL-style control slice first: discovery, sessions, eval, and file loading over a local Unix socket.
- Code/config changes made:
  - Added `src/entry_repl_server_mode.c3` and wired `src/entry.c3` plus `src/entry_cli_help_version.c3` so `omni --repl-server --socket <path>` is now a first-class CLI mode.
  - Added `src/lisp/eval_repl_server.c3`, implementing a newline-delimited JSON REPL server with phase-1 ops:
    - `describe`
    - `clone`
    - `close`
    - `eval`
    - `load-file`
  - Added REPL-server console capture fields to `src/lisp/value_interp_state.c3`, initialized them in `src/lisp/value_interp_init_helpers.c3`, cleaned them up in `src/lisp/value_interp_lifecycle.c3`, and updated `src/lisp/prim_io.c3` so `(print ...)` / `(display ...)` are captured as protocol `out` events during server eval instead of writing raw process stdio.
  - Added `omni_unix_socket_listen_fd(...)` in `csrc/uv_helpers_pipe.c` and used it from the REPL server so the server binds a real filesystem Unix socket path instead of depending on the earlier detached libuv pipe helper.
  - Hardened the server write path against peer-close `SIGPIPE` by sending protocol frames with `MSG_NOSIGNAL`, and fixed `close` so the final `done` event retains the correct session id after the session storage is freed.
  - Updated `docs/PROJECT_TOOLING.md`, `docs/man/omni.1`, `docs/README.md`, and `docs/plans/repl-server-protocol-2026-03-30.md` to record the shipped phase-1 boundary rather than leaving the feature purely proposed.
- Experiment commands and key metrics:
  - `c3c build` -> pass, executable linked to `build/main`.
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --repl-server --socket /tmp/omni-repl-test.sock` -> bound a real filesystem Unix socket at `/tmp/omni-repl-test.sock`.
  - Python AF_UNIX probe over one persistent connection verified:
    - `describe` -> `describe`, then `done`
    - `clone` -> `session`, then `done`
    - `eval` with `(print 1)` -> `out` (`1`), `value` (`#<void>`), then `done`
    - `eval` with `(+ 40 2)` -> `value` (`42`), then `done`
    - `load-file` for `/tmp/omni-repl-load-test.omni` containing `(print 7)` and `(+ 1 2)` -> `out` (`7`), `value` (`3`), `loaded`, then `done`
    - invalid session request -> structured `error` with `protocol/unknown-session`
    - `close` -> `done` with the correct `session` id preserved
- Best current checkpoint/config recommendation:
  - Use `--repl --json` for current first-party Neovim integration and one-shot tool work.
  - Use `--repl-server --socket <path>` for new local integrations that want explicit sessions and multi-event replies over a stable transport.
  - Treat the current server contract as phase 1 only: good for discovery/session/eval/load-file, not yet for interrupts or richer editor IDE flows.
- Unresolved issues and next actions:
  - `interrupt`, `stdin`, and completion are still missing and should be the next protocol ops added before broader editor migration.
  - The current server handles one client connection at a time; multi-client concurrency is intentionally deferred.
  - TCP and stdio server transports remain deferred until the local Unix-socket contract settles.
- Signature: Codex (GPT-5)

2026-03-30 - REPL server follow-up: completion + stdio transport

- Objectives attempted:
  - Continue the REPL server beyond the first Unix-socket eval slice without pretending that the not-yet-supported `interrupt` / `stdin` semantics already exist.
  - Reuse existing REPL/runtime capabilities for the next honest protocol additions instead of inventing a parallel completion mechanism.
  - Make the server protocol usable both over a socket path and directly over stdin/stdout.
- Code/config changes made:
  - Extended `src/entry_repl_server_mode.c3` and `src/entry_cli_help_version.c3` so the CLI now supports both:
    - `omni --repl-server --socket <path>`
    - `omni --repl-server --stdio`
  - Refactored `src/lisp/eval_repl_server.c3` so the request loop works over a generic read/write stream instead of assuming every transport fd is a socket.
    - Socket-backed transports still use `send(..., MSG_NOSIGNAL)` for protocol writes.
    - Stdio-backed transports use plain fd reads/writes via the existing fs helpers.
  - Added `complete` to the shipped server op set, reusing the existing prefix-matching semantics already present in the interactive REPL completion logic.
    - `describe` now advertises `complete`.
    - Completion responses now return sorted `candidate` items with a coarse `kind` (`function`, `module`, or `binding`).
  - Updated `docs/PROJECT_TOOLING.md`, `docs/man/omni.1`, and `docs/plans/repl-server-protocol-2026-03-30.md` so the shipped boundary now reflects both transports and the new `complete` op.
- Experiment commands and key metrics:
  - `c3c build` -> pass, executable linked to `build/main`.
  - `printf '%s\n%s\n%s\n' '{"id":"1","op":"clone"}' '{"id":"2","op":"eval","session":"s1","code":"(define zebra 1)","mode":"expr"}' '{"id":"3","op":"complete","session":"s1","prefix":"ze"}' | env LD_LIBRARY_PATH=/usr/local/lib ./build/main --repl-server --stdio`
    - returned `session`, `value`, and `complete` events on stdout as expected
    - completion payload included `zebra` and builtin `zero?`
  - Unix-socket Python probe against `./build/main --repl-server --socket /tmp/omni-repl-test.sock`
    - `describe` now advertises `complete`
    - after defining `print-server-test`, `complete` with prefix `print-s` returned `print-server-test`
- Best current checkpoint/config recommendation:
  - Use `--repl-server --socket` when you want a stable local endpoint and explicit attachment semantics.
  - Use `--repl-server --stdio` when you want the richer server protocol but do not want to manage a socket path.
  - Keep treating `interrupt` and `stdin` as genuinely unimplemented rather than papering over them in clients.
- Unresolved issues and next actions:
  - `interrupt` still requires a truer in-flight evaluation model than the current single-connection synchronous server loop.
  - `stdin` still needs a runtime-level input routing contract; the language does not yet expose a server-routable stdin read surface to plug into this op honestly.
  - TCP and multi-client concurrency remain deferred.
- Signature: Codex (GPT-5)

2026-03-30 - REPL server follow-up: cooperative interrupt + honest stdin deferral

- Objectives attempted:
  - Land a truthful `interrupt` op for the shipped REPL server instead of continuing to advertise it only in the protocol note.
  - Keep `stdin` honest: do not pretend it works until the runtime has a real server-routable input surface.
  - Preserve the current JIT/thread-affinity constraints while still allowing control requests to arrive during long-running evaluation.
- Code/config changes made:
  - Extended interpreter state and runtime polling so evaluation can stop cooperatively when a REPL-server session raises an interrupt request.
    - `src/lisp/value_interp_state.c3` now carries an optional interrupt flag pointer.
    - `src/lisp/eval_signal.c3`, `src/lisp/value_interp_runtime_helpers.c3`, and `src/lisp/runtime_backend_hooks.c3` now expose and check cooperative interrupt state, returning the distinct runtime message `evaluation interrupted`.
  - Refactored `src/lisp/eval_repl_server.c3` from a purely synchronous request loop into a stream-local worker model:
    - one worker thread owns runtime/session work for the stream,
    - the main read loop can still accept `interrupt` while `eval` / `load-file` is running,
    - successful interrupt requests now end with `done`, while the cancelled target request terminates with `interrupted`,
    - `describe` now advertises `interrupt`,
    - `stdin` now returns an explicit `protocol/unsupported-op` error instead of being implied by the design note.
  - Added per-stream serialized request behavior instead of faking broader concurrency:
    - while one runtime request is in flight, other non-`interrupt` requests return `protocol/server-busy`.
  - Updated `docs/PROJECT_TOOLING.md`, `docs/man/omni.1`, and `docs/plans/repl-server-protocol-2026-03-30.md` so the shipped contract reflects `interrupt` support and keeps `stdin` explicitly deferred.
- Experiment commands and key metrics:
  - `c3c build` -> pass, executable linked to `build/main`.
  - Python stdio probe against `./build/main --repl-server --stdio`
    - `clone` -> `done`
    - `eval` defining a recursive loop -> `done`
    - long-running `(loop)` request interrupted by `{"op":"interrupt","target":"3"}` -> target request emitted `{"event":"interrupted"}` and interrupt request emitted `{"event":"done"}`
  - Python Unix-socket probe against `./build/main --repl-server --socket /tmp/omni-repl-test.sock`
    - `describe` advertised `interrupt` and no longer advertised `stdin`
    - interrupting a recursive `(loop)` request returned the same `done` + `interrupted` pair
    - `stdin` returned `protocol/unsupported-op`
- Best current checkpoint/config recommendation:
  - Use `interrupt` now for long-running `eval` / `load-file` requests on either `--repl-server --stdio` or `--repl-server --socket`.
  - Treat the current server as serialized per stream: do not pipeline non-`interrupt` requests and expect them to queue.
  - Keep treating `stdin` as unimplemented until the runtime grows a real input-routing surface.
- Unresolved issues and next actions:
  - `stdin` still needs a language/runtime input abstraction before the server can route request data into blocked evaluation honestly.
  - The current server remains one-client-per-process on the socket listener and one-runtime-request-at-a-time per stream.
  - TCP and richer multi-client concurrency remain deferred.
- Signature: Codex (GPT-5)

2026-03-30 - REPL server follow-up: routed stdin + TCP listener boundary

- Objectives attempted:
  - Replace the placeholder `stdin` op with a real runtime-backed input path.
  - Add the next transport slice with a TCP listener mode.
  - Keep the shipped boundary honest instead of pretending cross-connection concurrency is solved.
- Code/config changes made:
  - Added a real runtime input surface:
    - `src/lisp/value_interp_state.c3` now exposes an optional `input_state` on `Interp`.
    - `src/lisp/prim_io.c3` now owns the buffered `InterpInputState`, the shared runtime line reader, and the new `prim_read_line` primitive.
    - `stdlib/stdlib.lisp` now exports `read-line` through the new `io/read-line` effect.
  - Wired REPL-server `stdin` requests into per-session input queues in `src/lisp/eval_repl_server.c3`:
    - `stdin` now accepts `data` and optional `eof: true`,
    - blocked `(read-line)` evaluations consume that routed input,
    - interrupting a blocked `(read-line)` request now produces `interrupted`.
  - Added `omni --repl-server --tcp <host> <port>` in `src/entry_repl_server_mode.c3` and updated `src/entry_cli_help_version.c3`.
  - Updated compiler/runtime surface parity for the new `read-line` surface in:
    - `src/lisp/eval_init_primitives.c3`
    - `src/lisp/compiler_free_vars_utils.c3`
    - `src/lisp/compiler_let_set_flat.c3`
    - `src/lisp/compiler_primitive_variable_hash_table_domains.c3`
  - Attempted per-connection socket concurrency, then reverted it after hitting the existing JIT owner-thread invariant during interpreter attachment.
- Experiment commands and key metrics:
  - `c3c build` -> pass, executable linked to `build/main`.
  - `printf 'abc\n' | ./build/main --eval '(read-line)' --json`
    - returned `{"ok":true,...,"value":"\"abc\""}`.
  - Python stdio probe against `./build/main --repl-server --stdio`
    - `clone` -> `(read-line)` -> `stdin data` returned `"hello"`,
    - `clone` -> `(read-line)` -> `stdin eof` returned `nil`,
    - `clone` -> `(read-line)` -> `interrupt` returned `interrupted`.
  - Python Unix-socket probe against `./build/main --repl-server --socket /tmp/omni-repl-sequential.sock`
    - two sequential client connections each completed `clone` + `eval` successfully.
  - Python TCP probe against `./build/main --repl-server --tcp 127.0.0.1 48763`
    - `describe` succeeded and advertised `stdin`.
- Best current checkpoint/config recommendation:
  - Use `stdin` now when a running `eval` or `load-file` needs line-oriented input.
  - Use `--repl-server --tcp <host> <port>` when a filesystem socket path is awkward and you are on a trusted network boundary.
  - Treat Unix-socket/TCP listeners as sequential-client servers for now.
- Unresolved issues and next actions:
  - Cross-connection concurrency is still blocked by the runtime/JIT owner-thread invariant at interpreter attachment time.
  - Queueing beyond one in-flight runtime request per stream is still unimplemented.
  - Authentication/TLS policy for TCP transport is still deferred.
- Signature: Codex (GPT-5)

2026-03-30 - TCP REPL port-file discovery

- Objectives attempted:
  - Make the TCP REPL server discoverable to editor plugins without forcing them
    to scrape process arguments or duplicate the configured port in editor
    config.
  - Keep the shipped discovery surface simple and nREPL-like instead of
    introducing a new side-channel protocol.
- Code/config changes made:
  - Updated `src/lisp/eval_repl_server.c3` so
    `omni --repl-server --tcp <host> <port>` now writes
    `.omni-repl-port` in the current working directory after the listener binds
    successfully.
  - Implemented the publish step as an atomic temp-write + rename instead of an
    in-place write.
  - Updated `src/entry_cli_help_version.c3`,
    `docs/PROJECT_TOOLING.md`, `docs/man/omni.1`, and
    `docs/plans/repl-server-protocol-2026-03-30.md` to document the new
    discovery-file contract.
- Experiment commands and key metrics:
  - `c3c build` -> pass, executable linked to `build/main`.
  - `./build/main --repl-server --tcp 127.0.0.1 48763`
    - wrote `.omni-repl-port` containing `48763`.
- Best current checkpoint/config recommendation:
  - Start the TCP REPL server from the project root when you want editor/plugin
    discovery; plugins can read `.omni-repl-port` from that directory and then
    attempt a normal TCP connect.
  - Treat the port file as a discovery hint, not a liveness guarantee.
- Unresolved issues and next actions:
  - Abrupt termination can still leave a stale `.omni-repl-port`.
  - The discovery file currently exists only for TCP mode; Unix-socket mode
    still requires the socket path directly.
- Signature: Codex (GPT-5)

2026-03-30 - omni-nvim TCP REPL discovery

- Objectives attempted:
  - Make the first-party Neovim plugin attach to a discovered TCP REPL server
    automatically when `.omni-repl-port` is present.
  - Preserve the existing local `omni --repl --json` workflow as the fallback
    path when discovery is missing or broken.
- Code/config changes made:
  - Updated `tooling/omni-nvim/lua/omni/repl.lua`:
    - added upward `.omni-repl-port` discovery rooted by
      `omni.toml`, `project.json`, or `.git`,
    - added TCP channel attachment via `sockconnect()`,
    - cloned a REPL-server session automatically after connect,
    - routed persistent eval requests through REPL-server `op = "eval"` when
      attached over TCP,
    - preserved the legacy local process path as fallback.
  - Exposed the discovery defaults in `tooling/omni-nvim/lua/omni/init.lua`.
  - Updated `tooling/omni-nvim/README.md` and
    `tooling/omni-nvim/doc/omni.nvim.txt` to document the new attach behavior.
- Experiment commands and key metrics:
  - `tooling/omni-nvim/scripts/run_smoke.sh` -> pass.
  - headless Neovim probe against a live
    `./build/main --repl-server --tcp 127.0.0.1 48763`
    listener with `.omni-repl-port`
    - plugin auto-attached to the TCP server,
    - `send_buffer(...)` returned `=> 3` through the REPL-server session.
- Best current checkpoint/config recommendation:
  - If you want plugin discovery, start the TCP REPL server from the Omni
    project root so `.omni-repl-port` lands where the plugin will find it.
  - Leave `repl.discovery.enabled = true` unless you explicitly want the older
    local-process-only behavior.
- Unresolved issues and next actions:
  - Discovery currently targets TCP only; Unix-socket attach is still a future
    follow-up if we want the same convenience for local-only servers.
  - The plugin treats `.omni-repl-port` as a hint and falls back on connect
    failure, but it does not currently try to rewrite or clean stale files.
- Signature: Codex (GPT-5)

2026-03-30 - REPL server project preload parity

- Objectives attempted:
  - Make `omni --repl-server` accept the same project preload flag as the
    interactive REPL instead of rejecting `--project` as an unexpected
    argument.
  - Keep the preload contract aligned across interactive REPL and REPL-server
    transports so tooling does not need a separate notion of "project REPL".
- Code/config changes made:
  - Updated `src/entry_repl_server_mode.c3`:
    - added `--project [dir]` parsing for `--socket`, `--stdio`, and `--tcp`,
    - threaded the preload choice into the REPL-server transport entrypoints.
  - Refactored `src/entry_runtime_modes.c3`:
    - added shared project-entry resolution helpers so the interactive REPL and
      REPL server both resolve `omni.toml` plus `src/main.omni` the same way.
  - Updated `src/lisp/eval_repl_server.c3`:
    - each new `clone` session now optionally preloads the resolved project
      entry before the session is announced,
    - preload output is surfaced as protocol `out` events,
    - preload failure aborts the clone and returns a structured `error`
      instead of exposing a partially initialized session.
  - Updated `src/entry_cli_help_version.c3`,
    `docs/PROJECT_TOOLING.md`, `docs/man/omni.1`, and
    `docs/plans/repl-server-protocol-2026-03-30.md` so the documented REPL
    server boundary matches the shipped CLI.
- Experiment commands and key metrics:
  - `c3c build` -> pass, executable linked to `build/main`.
  - `./build/main --repl-server --tcp 127.0.0.1 48763 --project ./demo`
    - accepted the preload flag,
    - wrote `.omni-repl-port`,
    - a TCP `clone` request emitted `out` for `"Hello from demo!"`,
      then `session`, then `done`.
- Best current checkpoint/config recommendation:
  - Use `omni --repl-server --tcp <host> <port> --project [dir]` when you want
    editor/plugin attach plus per-session project preload.
  - Start the server from the project root when you also want `.omni-repl-port`
    discovery to line up with plugin search.
- Unresolved issues and next actions:
  - Preload is currently tied to the existing fixed project entry contract
    (`omni.toml` plus `src/main.omni`); if Omni grows a configured project
    entrypoint later, both REPL paths should resolve through that metadata.
  - The server still serializes one connected client at a time.
- Signature: Codex (GPT-5)

2026-04-08 08:41 CEST - Omni language surface audit and external criticism scan

- Objectives attempted:
  - Audit Omni's current language surface for likely external reception,
    especially syntax and feature-density concerns.
  - Compare the current design against public criticism patterns from Reddit,
    Hacker News, blog posts, and one recent algebraic-effects verification
    paper.
- Relevant workspace/target:
  - Language/docs surface only; no runtime or compiler behavior was modified.
- Code/config changes made:
  - None.
- Commands run:
  - `sed -n '1,220p' docs/LANGUAGE_SPEC.md`
  - `sed -n '1,220p' docs/README.md`
  - `sed -n '1,220p' docs/C3_STYLE.md`
  - `sed -n '1,220p' docs/syntax-decision.md`
  - `sed -n '1,220p' docs/type-system-syntax.md`
  - `sed -n '1,220p' docs/OMNI_REFERENCE.md`
  - `sed -n '1,220p' docs/reference/02-functions.md`
  - `sed -n '1,220p' docs/reference/03-collections.md`
  - `sed -n '1,220p' docs/reference/04-type-system.md`
  - `sed -n '1,220p' docs/reference/05-macros-modules.md`
  - `sed -n '1,220p' docs/reference/06-effects.md`
  - targeted `rg` scans across `docs/`, `src/`, and `test/`
  - targeted `nl -ba ... | sed -n ...` line-anchor extraction for final audit
- Key results and conclusions:
  - Omni's strongest design assets are the coherent expression model, generic
    collection operations, explicit effect/control operators, strict arity,
    and the choice to keep macro authoring word-based instead of punctuation
    heavy.
  - The biggest adoption risk is not any single feature; it is the cumulative
    stack: Lisp syntax + multiple dispatch + structural typing + algebraic
    effects + delimited continuations + deterministic region memory. Public
    criticism patterns found on similar systems line up with exactly that kind
    of cognitive load and debugging fear.
  - The current surface is powerful but not visually minimal. In addition to
    ordinary s-expressions it uses square/curly literals, leading-dot accessor
    lambdas, postfix `.[...]`, quoted-symbol callable accessors, `^Type`,
    `^(Value ...)`, attribute brackets, slash-qualified effect names, and
    dot-qualified module access. This reads more like a dense PL toolkit than
    a small Lisp.
  - The most criticism-prone local semantics are:
    - `false` collapsing to `nil`,
    - callable quoted symbols like `('name dict)`,
    - multiple overlapping partial-application mechanisms,
    - optional effect declarations,
    - multi-shot continuations with replayed side effects,
    - the trust burden created by pairing advanced control flow with explicit
      lifetime management.
- Invalidated assumptions or failed approaches worth preserving:
  - A prior memory note claiming that `Dict` was docs-only was stale. Current
    source registers `Dict` in runtime and compiler surfaces and has coverage
    for it.
- Best current recommendation/checkpoint:
  - Position Omni externally around a much narrower "core profile" instead of
    leading with the full advanced feature inventory.
  - If syntax simplification is pursued, prefer removing or de-emphasizing
    cute secondary surfaces before touching the core expression model. The
    highest-friction candidates are callable quoted-symbol accessors and the
    combination of leading-dot accessor lambdas with postfix indexing.
  - If feature pruning is considered, the hardest sell to mainstream users is
    likely the simultaneous presence of both algebraic effects and delimited
    continuations rather than either feature in isolation.
- Unresolved issues and next actions:
  - Decide whether the owner wants Omni optimized first for PL enthusiasts,
    Lisp users, systems programmers, or mainstream application developers; the
    right syntax/features message depends heavily on that audience choice.
  - If another session continues this work, convert the audit into a ranked
    simplification plan rather than another broad critique pass.
- Signature: Codex (GPT-5)

2026-04-08 08:49 CEST - Omni syntax cleanup direction locked by owner

- Objectives attempted:
  - Convert the audit conclusions into an explicit owner-approved cleanup
    direction for syntax and semantics.
- Code/config changes made:
  - None.
- Decision checkpoint:
  - Backward compatibility is not a goal for this cleanup. Removed surfaces
    should not linger as legacy aliases or migration spellings.
  - Approved direction:
    - remove leading-dot accessor lambdas,
    - remove callable quoted-symbol accessors,
    - keep path expressions as access-only sugar over `ref`,
    - keep pipeline `|>`,
    - split `false` from `nil` while keeping both falsy,
    - treat effects as the normal application-facing control surface and
      continuations as advanced.
- Best current recommendation/checkpoint:
  - Implement the cleanup as a fail-closed language contract pass, not as a
    compatibility layer.
  - Prefer one canonical surface per concept and hard errors for removed forms.
- Signature: Codex (GPT-5)

2026-04-08 09:35 CEST - Omni cleanup implementation slices 1-3 started

- Objectives attempted:
  - Land the first concrete cleanup slices from the owner-approved language
    simplification pass.
- Code/config changes made:
  - Accessor cleanup:
    - removed leading-dot accessor parsing and dead accessor-lambda AST/parser
      plumbing from parser-facing code,
    - removed quoted-symbol callable lookup behavior from runtime apply,
    - updated affected tests to use `ref`, path access, or fail-closed errors.
  - Primitive call tightening:
    - ordinary one-argument primitive calls no longer auto-create
      `PARTIAL_PRIM`,
    - interpreter and JIT single-arg primitive apply now report arity
      mismatch instead.
  - `false`/`nil` split:
    - `false` now binds to a distinct symbol value instead of `nil`,
    - Boolean/Nil constructor semantics were updated around the split,
    - parser pattern/value-literal handling and JSON false conversion were
      updated so `false` no longer aliases `nil`.
  - Docs/planning artifacts updated:
    - `.agents/PLAN.md`
    - `memory/CHANGELOG.md`
    - public spec/reference docs for access syntax, truthiness, partial
      application, and boolean/nil semantics.
- Commands run:
  - multiple `rg`/`sed` inspection passes across `docs/` and `src/lisp/`
  - `c3c build`
    - failed locally because `c3c` is not installed in this environment
- Key results and conclusions:
  - Slice 1 is now fail-closed in parser/runtime/test surfaces touched here:
    removed accessor forms should error instead of silently surviving as
    legacy spellings.
  - Slice 2 is materially implemented: bare `(+ 3)` style calls should now be
    arity errors, while pipe syntax remains intact because `|>` rewrites call
    shapes before evaluation.
  - Slice 3 has started and the core contract changed on disk, but it still
    needs a broader consistency sweep across remaining docs/tests and any
    runtime corners not covered by the files touched in this session.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep assuming the accessor cleanup is “just parser sugar”; quoted
    symbol calls and test corpus usage made it a broader runtime/doc sweep.
  - Do not assume primitive auto-partial is required for pipe syntax; `|>`
