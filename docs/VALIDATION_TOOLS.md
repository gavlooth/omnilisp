# Validation Tools Guide

This guide is the default tool map for closing Omni regressions. Use it with the
Regression Closure Rule in `AGENTS.md`: first name the broken contract, then pick
the smallest tool that can falsify or prove the fix.

## Installed Tool Baseline

On the current Ubuntu 24.04 aarch64 host, these tools are available:

- `valgrind` 3.22.0
- `gdb` 15.0.50
- `perf` 6.17.9
- `hyperfine` 1.18.0
- `heaptrack` 1.5.0
- `rr` 5.7.0
- `strace` 6.8

Current host caveat: `kernel.perf_event_paranoid` is `4`, so normal `perf`
recording and normal `rr record` are permission-blocked for unprivileged users.
On this aarch64 host, `rr record -n` also fails because rr does not recognize
the current CPU microarchitecture. Treat `rr` as installed but not usable here
until the host/kernel support changes.

If a future host is missing optional tools, install the Ubuntu packages when
available:

```sh
sudo apt-get install -y valgrind gdb linux-tools-$(uname -r) hyperfine heaptrack rr strace
```

If `linux-tools-$(uname -r)` is unavailable, use the distro-provided `perf`
package or the matching kernel tools package. If `perf` or `rr` is blocked by
`perf_event_paranoid`, do not silently skip the evidence: record the blocked
command and either run it on a capable host or switch to `gdb`, `strace`,
Valgrind, or targeted logging for the current slice. Changing sysctl security
settings is an explicit host administration action, not a routine repo step.

## Tool Selection

| Failure class | First tool | Escalation |
| --- | --- | --- |
| Build or type integration | `c3c build`, `git diff --check` | focused slice test |
| Language semantics regression | focused Lisp slice | interpreter/JIT/AOT differential test |
| JIT/eval divergence | parity fixture | `gdb` on the narrowed executable path |
| Memory lifetime, UAF, double-free | ASAN build when supported | `valgrind memcheck` |
| Heap growth or leaks | targeted leak test | `valgrind massif`, `heaptrack` |
| Performance regression | `hyperfine` before/after | `perf record/report`, `cachegrind` |
| Native crash | `gdb` | `rr` if record works on the host |
| FFI or process boundary | focused test | `valgrind --trace-children=yes`, `strace -f` |
| Deduce/LMDB persistence | restart/restore fixture | fault injection plus `strace` for filesystem/syscalls |

## Command Templates

Targeted Valgrind memory run:

```sh
valgrind --trace-children=yes \
  --leak-check=full \
  --show-leak-kinds=definite,indirect,possible \
  --error-exitcode=99 \
  env LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp
```

Heap profiling:

```sh
heaptrack env LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp
```

Performance timing:

```sh
hyperfine --warmup 3 \
  'env LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp'
```

CPU profiling:

```sh
perf record --call-graph dwarf -- \
  env LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp
perf report
```

Crash debugging:

```sh
gdb --args env LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp
```

Syscall tracing:

```sh
strace -f -o /tmp/omni.strace \
  env LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp
```

Replay debugging when supported:

```sh
rr record env LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp
rr replay
```

## Omni-Specific Rules

- Use focused tests first; broad runs are confirmation, not diagnosis.
- For memory/lifetime changes, prefer ASAN first when supported, then Valgrind
  Memcheck for native lifetime evidence.
- Keep `--trace-children=yes` when Valgrind wraps `env`, otherwise the wrapper
  can hide the actual runtime process.
- For JIT, AOT, parser, and evaluator work, add parity coverage rather than
  testing only the changed path.
- For persistence bugs, use restart/open/restore fixtures and include failure
  injection for the error path.
- For performance work, collect a before/after command and metric. Do not close
  performance work from static inspection alone.
- Do not run broad/high-memory validation directly on the host. Use the bounded
  Docker validation path from `AGENTS.md` for full-suite, all-slice,
  memory-stress, or broad Valgrind runs.
- If a tool cannot run locally because of platform, kernel, permissions, or
  container limits, record the result as `UNVERIFIED` and name the exact command
  that should be run on a capable host.
