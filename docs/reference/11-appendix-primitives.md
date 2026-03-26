# Appendix A: Primitive Reference

**[Back to Index](../OMNI_REFERENCE.md)**

---

### Primitive Surface Audit: Command vs Query (2026-03-17)

Audit source of truth:
- registered primitive table in `src/lisp/eval_init_primitives.c3`.

Normative classification contract:
- command-style primitive: successful completion returns `Void` (or is a non-returning process command such as `exit`).
- query-style primitive: returns data/handle/predicate values; when absence semantics apply, absence remains `nil` (not `Void`).

Audit snapshot:
- total registered primitive names: `263`
- command-style names: `42`
- query-style names: `221`

#### Core Mutation Classification

| Primitive | Style | Success contract |
|------|-------|-------------|
| `set!` | command-style | Returns `Void` after successful mutation dispatch (`array`/`dict`) |
| `push!` | command-style | Returns `Void` after append |
| `remove!` | command-style | Returns `Void` after key removal |
| `set-add` | command-style | Returns `Void` after insert |
| `set-remove` | command-style | Returns `Void` after delete |
| `unsafe-free!` | command-style | Returns `Void` after releasing supported heap backing |
| `ref` | query-style | Returns looked-up value, or `nil` on absence |
| `has?` | query-style | Returns truthy on hit, `nil` on miss |
| `set-contains?` | query-style | Returns truthy on hit, `nil` on miss |
| `keys` | query-style | Returns key list |
| `values` | query-style | Returns value list |

#### I/O Classification

| Primitive(s) | Style | Success contract |
|------|-------|-------------|
| `__raw-print`, `__raw-println`, `__raw-newline`, `__raw-display` | command-style | Return `Void` after console side effect |
| `__raw-write-file` | command-style | Returns `Void` after write completion |
| `__raw-fs-close`, `__raw-fs-rename`, `__raw-fs-unlink` | command-style | Return `Void` after lifecycle/path mutation |
| `fs-close`, `fs-rename`, `fs-unlink` | command-style | Return `Void` (public alias surface) |
| `filesystem-close`, `filesystem-rename`, `filesystem-unlink` | command-style | Return `Void` (long-form alias surface) |
| `__raw-udp-bind`, `__raw-udp-close`, `__raw-tcp-close`, `__raw-process-kill`, `__raw-signal-unhandle`, `__raw-async-sleep`, `__raw-tls-close` | command-style | Return `Void` after side-effecting operation |
| `sleep`, `exit` | command-style | `sleep` returns `Void`; `exit` is process-terminal |
| `__raw-read-file`, `__raw-file-exists?`, `__raw-read-lines`, `__raw-fs-open`, `__raw-fs-read`, `__raw-fs-write`, `__raw-fs-stat`, `__raw-fs-readdir` | query-style | Return data/handles/metrics (`nil` only for absence-style queries) |
| `fs-open`, `fs-read`, `fs-write`, `fs-stat`, `fs-readdir`, `filesystem-open`, `filesystem-read`, `filesystem-write`, `filesystem-stat`, `filesystem-read-directory` | query-style | Return handles/data/metrics |
| `__raw-tcp-connect`, `__raw-tcp-listen`, `__raw-tcp-accept`, `__raw-tcp-read`, `__raw-tcp-write`, `__raw-udp-socket`, `__raw-udp-send`, `__raw-udp-recv`, `__raw-pipe-connect`, `__raw-pipe-listen`, `__raw-process-spawn`, `__raw-process-wait`, `__raw-signal-handle`, `__raw-dns-resolve`, `__raw-tls-connect`, `__raw-tls-server-wrap`, `__raw-tls-read`, `__raw-tls-write`, `__raw-http-get`, `__raw-http-request` | query-style | Return handles, payloads, or result dictionaries |

#### Scheduler Classification

| Primitive(s) | Style | Success contract |
|------|-------|-------------|
| `fiber-cancel`, `run-fibers` | command-style | Return `Void` on successful scheduler command completion |
| `__raw-thread-cancel`, `__raw-task-cancel` | command-style | Return `Void` on successful cancellation command |
| `spawn`, `await`, `__raw-offload`, `__raw-offload-batch`, `__raw-thread-spawn`, `__raw-thread-spawn-batch`, `__raw-thread-join`, `__raw-thread-join-timeout`, `__raw-task-spawn`, `__raw-task-spawn-batch`, `__raw-task-join`, `__raw-task-join-timeout` | query-style | Return ids/handles/results (`join-timeout` may return `nil` timeout absence) |

#### Exhaustive Classification Rule

Command-style primitive set (`42` names):
`set!`, `push!`, `remove!`, `set-add`, `set-remove`, `unsafe-free!`, `deduce/commit`, `deduce/abort`, `deduce/fact!`, `deduce/retract!`, `deduce/clear!`, `deduce/drop!`, `deduce/rule!`, `__raw-print`, `__raw-println`, `__raw-newline`, `__raw-display`, `__raw-write-file`, `__raw-fs-close`, `__raw-fs-rename`, `__raw-fs-unlink`, `fs-close`, `fs-rename`, `fs-unlink`, `filesystem-close`, `filesystem-rename`, `filesystem-unlink`, `__raw-udp-bind`, `__raw-udp-close`, `__raw-tcp-close`, `__raw-process-kill`, `__raw-signal-unhandle`, `__raw-async-sleep`, `__raw-thread-cancel`, `__raw-task-cancel`, `__raw-tls-close`, `sleep`, `exit`, `fiber-cancel`, `run-fibers`.

All remaining registered primitive names are query-style.

### Dispatched Primitives (33)

These support user extension via method tables.

| Name | Arity | Category |
|------|-------|----------|
| `+` | 2 | Arithmetic |
| `-` | 2 | Arithmetic |
| `*` | 2 | Arithmetic |
| `/` | 2 | Arithmetic |
| `%` | 2 | Arithmetic |
| `=` | 2 | Comparison |
| `<` | 2 | Comparison |
| `>` | 2 | Comparison |
| `<=` | 2 | Comparison |
| `>=` | 2 | Comparison |
| `length` | 1 | Collection |
| `ref` | 2 | Collection |
| `push!` | 2 | Collection |
| `keys` | 1 | Collection |
| `values` | 1 | Collection |
| `has?` | 2 | Collection |
| `remove!` | 2 | Collection |
| `sort` | 1 | Sorting |
| `sort-by` | 2 | Sorting |
| `string-append` | variadic | String |
| `string-contains?` | 2 | String |
| `string-upcase` | 1 | String |
| `string-downcase` | 1 | String |
| `string-casefold` | 1 | String |
| `string-titlecase` | 1 | String |
| `abs` | 1 | Math |
| `floor` | 1 | Math |
| `ceiling` | 1 | Math |
| `round` | 1 | Math |
| `truncate` | 1 | Math |
| `sqrt` | 1 | Math |
| `min` | 2 | Math |
| `max` | 2 | Math |

### Regular Primitives

**List Operations:**

| Name | Arity | Description |
|------|-------|-------------|
| `cons` | 2 | Construct pair |
| `car` | 1 | First element |
| `cdr` | 1 | Rest |
| `List` | variadic | Canonical list constructor / conversion surface |
| `null?` | 1 | Is nil? |
| `pair?` | 1 | Is cons? |
| `list` | variadic | Public helper + compatibility alias for list construction/conversion |

**Boolean:**

| Name | Arity | Description |
|------|-------|-------------|
| `not` | 1 | Logical negation |

**I/O (raw, used by effect fast path):**

| Name | Arity | Description |
|------|-------|-------------|
| `__raw-print` | 1 | Raw print |
| `__raw-println` | 1 | Raw println |
| `__raw-newline` | 0 | Raw newline |
| `__raw-display` | 1 | Raw display |
| `__raw-read-file` | 1 | Raw file read |
| `__raw-write-file` | 2 | Raw file write |
| `__raw-file-exists?` | 1 | Raw file check |
| `__raw-read-lines` | 1 | Raw line reader |

**Filesystem Handles:**

| Name | Arity | Description |
|------|-------|-------------|
| `filesystem-open` | variadic | Open file handle with mode/options |
| `filesystem-read` | 2 | Read bytes/chars from handle |
| `filesystem-write` | 2 | Write bytes/chars to handle |
| `filesystem-close` | 1 | Close file handle |
| `filesystem-stat` | 1 | File metadata lookup |
| `filesystem-read-directory` | 1 | Read directory entries |
| `filesystem-rename` | 2 | Rename/move file |
| `filesystem-unlink` | 1 | Remove file path |

Compatibility shorthands (same behavior/arity):
`fs-open`, `fs-read`, `fs-write`, `fs-close`, `fs-stat`, `fs-readdir`,
`fs-rename`, and `fs-unlink`.

**String:**

| Name | Arity | Description |
|------|-------|-------------|
| `string-join` | 2 | Join list with separator |
| `substring` | 3 | Extract substring |
| `string-split` | 2 | Split by delimiter |
| `string-length` | 1 | Length |
| `string-byte-length` | 1 | Byte length |
| `string->list` | 1 | To list of chars |
| `list->string` | 1 | List to string |
| `string-trim` | 1 | Trim whitespace |
| `string-index-of` | 2 | Find index |
| `string-replace` | 3 | Replace occurrences |
| `char-at` | 2 | Char at index |
| `string-repeat` | 2 | Repeat N times |

**Type & Introspection:**

| Name | Arity | Description |
|------|-------|-------------|
| `type-of` | 1 | Type name as symbol |
| `is?` | 2 | Type/subtype check |
| `instance?` | 1 | Is type instance? |
| `continuation?` | 1 | Is continuation? |
| `procedure?` | 1 | Is callable? |
| `bound?` | 1 | Is name defined? |
| `type-args` | 1 | Parametric type args |
| `iterator?` | 1 | Is iterator value? |

`Nil` is the language-level empty/false value type. `Void` is now a real
singleton value constructor with zero arguments, and FFI `^Void` returns map to
that same runtime value.
Value-level `false` remains a stable alias of `nil` (quoted `'false` is still a
symbol literal).

**Conversion:**

| Name | Arity | Description |
|------|-------|-------------|
| `string->number` | 1 | Parse number |
| `number->string` | 1 | Number to string |
| `exact->inexact` | 1 | Integer to double |
| `inexact->exact` | 1 | Double to int |
| `string->symbol` | 1 | String to symbol |
| `symbol->string` | 1 | Symbol to string |

Callable core type symbols also provide constructor/coercion surface here:
`Integer`, `Int`, `Double`, `String`, `Symbol`, `Boolean`, `Bool`, `Nil`,
`Void`, and `Closure`.

**Math:**

| Name | Arity | Description |
|------|-------|-------------|
| `sin` | 1 | Sine |
| `cos` | 1 | Cosine |
| `tan` | 1 | Tangent |
| `asin` | 1 | Arc sine |
| `acos` | 1 | Arc cosine |
| `atan` | 1 | Arc tangent |
| `atan2` | 2 | Two-arg arctangent |
| `exp` | 1 | Exponential |
| `log` | 1 | Natural log |
| `log10` | 1 | Log base 10 |
| `pow` | 2 | Power |
| `gcd` | 2 | Greatest common divisor |
| `lcm` | 2 | Least common multiple |

**Bitwise:**

| Name | Arity | Description |
|------|-------|-------------|
| `bitwise-and` | 2 | AND |
| `bitwise-or` | 2 | OR |
| `bitwise-xor` | 2 | XOR |
| `bitwise-not` | 1 | NOT |
| `lshift` | 2 | Left shift |
| `rshift` | 2 | Right shift |

**Collections:**

| Name | Arity | Description |
|------|-------|-------------|
| `Array` | variadic | Canonical array constructor / conversion surface |
| `Dictionary` | variadic | Create dictionary |
| `Dict` | variadic | Public shorthand alias for `Dictionary` |
| `Set` | variadic | Create set |
| `set-add` | 2 | Add to set |
| `set-remove` | 2 | Remove from set |
| `set-contains?` | 2 | Set membership |
| `set-size` | 1 | Set cardinality |
| `set->list` | 1 | Convert set to list |

**Coroutines:**

| Name | Arity | Description |
|------|-------|-------------|
| `Coroutine` | 1 | Canonical coroutine constructor |
| `resume` | 1 | Resume coroutine |
| `yield` | 1 | Yield value |
| `coroutine?` | 1 | Type check |

**Error:**

| Name | Arity | Description |
|------|-------|-------------|
| `error` | 1 | Create error value |
| `error-message` | 1 | Extract message |

**Iterator:**

| Name | Arity | Description |
|------|-------|-------------|
| `next` | 1 | Get next (value . rest) pair |

Preferred forcing style uses collection constructors:
`(List it)` and `(Array it)`.

**Misc:**

| Name | Arity | Description |
|------|-------|-------------|
| `gensym` | 0 | Generate unique symbol |
| `format` | variadic | Printf-style formatting |
| `cl-format` | variadic | CL-style formatting |
| `read-string` | 1 | Parse string to Lisp |
| `eval` | 1 | Evaluate expression |
| `apply` | 2 | Apply function to arg list |
| `macroexpand` | 1 | Expand macro |
| `load` | 1 | Load and evaluate file |
| `unsafe-free!` | 1 | Free heap backing; returns `Void` |

**Schema:**

| Name | Arity | Description |
|------|-------|-------------|
| `validate` | 2 | Validate value against schema |
| `schema-explain` | 2 | Explain schema validation failures |

**Explainability:**

| Name | Arity | Description |
|------|-------|-------------|
| `explain` | 2 | Selector-locked explain surface: `(explain 'dispatch <form>)` or `(explain 'effect <form>)` |

**System / OS:**

| Name | Arity | Description |
|------|-------|-------------|
| `shell` | 1-2 | Execute shell command |
| `getenv` | 1 | Read environment variable |
| `time` | 0 | UNIX timestamp (seconds) |
| `time-ms` | 0 | UNIX timestamp (milliseconds) |
| `sleep` | 1 | Sleep for N seconds |
| `exit` | 0-1 | Exit process with optional status code |
| `random` | 0 | Random floating-point in [0, 1) |
| `random-int` | 1 | Random integer in [0, n) |

**Concurrency:**

| Name | Arity | Description |
|------|-------|-------------|
| `atomic` | 1 | Create atomic integer |
| `atomic-add!` | 2 | Atomic fetch-and-add |
| `atomic-read` | 1 | Atomic load |
| `atomic-cas!` | 3 | Compare-and-swap |
| `spawn` | 1 | Spawn fiber task |
| `await` | 1 | Await spawned task result |
| `run-fibers` | 0 | Run scheduled fibers to completion (returns `Void`) |
| `fiber-cancel` | 1 | Cancel a fiber (always returns `Void` for valid fiber ids; already-done/running targets are no-op completions) |

**Regex (Pika):**

| Name | Arity | Description |
|------|-------|-------------|
| `re-match` | 2 | First match |
| `re-fullmatch` | 2 | Full string match |
| `re-find-all` | 2 | All matches |
| `re-split` | 2 | Split by pattern |
| `re-replace` | 3-4 | Replace matches |
| `re-match-pos` | 2 | Match with positions |
| `re-find-all-pos` | 2 | All matches with positions |

**PEG Grammar (Pika):**

| Name | Arity | Description |
|------|-------|-------------|
| `pika/grammar` | variadic | Define grammar |
| `pika/parse` | 2 | Parse input |
| `pika/fold` | 3 | Fold over parse tree |
| `pika/grammar-rules` | 1 | List rule names |
| `pika/match-span` | 3 | Match specific rule |
| `pika/parse-lisp` | 1 | Parse Lisp syntax |

**Unicode:**

| Name | Arity | Description |
|------|-------|-------------|
| `string-normalize` | 2 | Unicode normalize |
| `string-graphemes` | 1 | Split into graphemes |
| `string-codepoints` | 1 | List of codepoints |
| `char-category` | 1 | Unicode category |
| `char-width` | 1 | Display width hint (-1/0/1/2) |
| `char-property` | 2 | Unicode property lookup by symbol |

**JSON:**

| Name | Arity | Description |
|------|-------|-------------|
| `json-parse` | 1-2 | Parse JSON string |
| `json-emit` | 1-2 | Emit compact JSON (optional options list for writer flags) |
| `json-emit-pretty` | 1-2 | Emit pretty JSON (optional options list for writer flags) |
| `json-get` | 2 | JSON Pointer-style lookup on Omni JSON values |

**TOML / CSV:**

| Name | Arity | Description |
|------|-------|-------------|
| `TimePoint` | variadic | Canonical TimePoint constructor |
| `time-point?` | 1 | TimePoint predicate |
| `toml-parse` | 1-2 | Parse TOML string (optional options list: `((check-utf8 false))`)
| `csv-parse` | 1-2 | Parse CSV text into rows (`delimiter` string or option list including `strict`; strict/default enforces RFC-4180 CRLF row endings) |
| `csv-emit` | 1-2 | Emit rows as CSV text (`delimiter`, `line-ending`, `quote-char`, `quote-style`, `nil-as`, `strict`; strict/default line ending is `\\r\\n`) |

**Compression:**

| Name | Arity | Description |
|------|-------|-------------|
| `gzip` | 1-2 | Gzip compress (optional level 0..12) |
| `gunzip` | 1 | Gzip decompress |
| `deflate` | 1-2 | Deflate compress (optional level 0..12) |
| `inflate` | 1-2 | Deflate decompress |
| `zlib-compress` | 1-2 | Zlib compress (optional level 0..12) |
| `zlib-decompress` | 1-2 | Zlib decompress |
| `adler32` | 1 | Adler-32 checksum |
| `crc32` | 1 | CRC-32 checksum |

**Networking:**

| Name | Arity | Description |
|------|-------|-------------|
| `__raw-tcp-connect` | 2 | Raw TCP connect |
| `__raw-tcp-listen` | variadic | Raw TCP listen |
| `__raw-tcp-accept` | 1 | Raw TCP accept |
| `__raw-tcp-read` | variadic | Raw TCP read |
| `__raw-tcp-write` | 2 | Raw TCP write |
| `__raw-tcp-close` | 1 | Raw TCP close |
| `__raw-udp-socket` | 0 | Raw UDP socket create |
| `__raw-udp-bind` | variadic | Raw UDP bind |
| `__raw-udp-send` | variadic | Raw UDP send |
| `__raw-udp-recv` | variadic | Raw UDP receive |
| `__raw-udp-close` | 1 | Raw UDP close |
| `__raw-pipe-connect` | 1 | Raw Unix socket connect |
| `__raw-pipe-listen` | 1 | Raw Unix socket listen |
| `__raw-process-spawn` | variadic | Raw process spawn |
| `__raw-process-wait` | 1 | Raw process wait |
| `__raw-process-kill` | variadic | Raw process kill |
| `__raw-signal-handle` | variadic | Raw signal watcher install |
| `__raw-signal-unhandle` | 1 | Raw signal watcher remove |
| `__raw-dns-resolve` | 1 | Raw DNS resolve |
| `__raw-async-sleep` | 1 | Raw sleep |
| `__raw-offload` | 1 | Raw pooled offload job |
| `__raw-offload-batch` | 1 | Raw pooled offload job batch |
| `__raw-thread-spawn` | 1 | Raw OS thread spawn |
| `__raw-thread-spawn-batch` | 1 | Raw OS thread spawn batch |
| `__raw-thread-join` | 1 | Raw OS thread join |
| `__raw-thread-join-timeout` | 1 | Raw OS thread join with timeout pair |
| `__raw-thread-cancel` | 1 | Raw OS thread cancel |
| `__raw-task-spawn` | 1 | Raw pooled task spawn |
| `__raw-task-spawn-batch` | 1 | Raw pooled task spawn batch |
| `__raw-task-join` | 1 | Raw pooled task join |
| `__raw-task-join-timeout` | 1 | Raw pooled task join with timeout pair |
| `__raw-task-cancel` | 1 | Raw pooled task cancel |
| `__raw-tls-connect` | variadic | Raw TLS connect |
| `__raw-tls-server-wrap` | 3 | Raw TLS server wrap |
| `__raw-tls-read` | variadic | Raw TLS read |
| `__raw-tls-write` | 2 | Raw TLS write |
| `__raw-tls-close` | 1 | Raw TLS close |
| `__raw-http-get` | 1 | Raw HTTP GET |
| `__raw-http-request` | variadic | Raw HTTP request |

**Deduce (Database):**

| Name | Arity | Description |
|------|-------|-------------|
| `deduce` | variadic | Unified database interface |
| `deduce/open` | variadic | Open deduce database handle |
| `deduce/open-named` | variadic | Open/register named relation handle |
| `deduce/block` | variadic | Open deduce transaction handle (`'read`, `'write`, `'write-deferred`) |
| `deduce/commit` | variadic | Commit transaction (`Void`) |
| `deduce/abort` | variadic | Abort transaction (`Void`) |
| `deduce/scan` | variadic | Scan relation tuples |
| `deduce/scan-range` | variadic | Bounded relation scan |
| `deduce/query` | variadic | Query rows by predicate |
| `deduce/count` | variadic | Count relation tuples |
| `deduce/match` | variadic | Pattern match relation rows |
| `deduce/fact!` | variadic | Assert fact (`Void`) |
| `deduce/retract!` | variadic | Retract fact (`Void`) |
| `deduce/clear!` | variadic | Clear relation (`Void`) |
| `deduce/drop!` | variadic | Drop relation (`Void`) |
| `deduce/rule!` | variadic | Install rule (`Void`) |
| `deduce/explain` | variadic | Explain deduce query/rule plan |
| `deduce/analyze` | variadic | Analyze deduce relation/rule surface |
