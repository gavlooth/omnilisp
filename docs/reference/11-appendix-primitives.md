# Appendix A: Primitive Reference

**[Back to Index](../OMNI_REFERENCE.md)**

---

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
| `null?` | 1 | Is nil? |
| `pair?` | 1 | Is cons? |
| `list` | variadic | Create list / convert collection->list (array/iterator) |

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
| `fs-open` | variadic | Open file handle with mode/options |
| `fs-read` | 2 | Read bytes/chars from handle |
| `fs-write` | 2 | Write bytes/chars to handle |
| `fs-close` | 1 | Close file handle |
| `fs-stat` | 1 | File metadata lookup |
| `fs-readdir` | 1 | Read directory entries |
| `fs-rename` | 2 | Rename/move file |
| `fs-unlink` | 1 | Remove file path |

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

`Nil` is the language-level empty/false value type. `Void` is not a value
constructor here; it is reserved for FFI/no-result annotation positions.

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
`Integer`, `Int`, `Double`, `String`, `Symbol`, `Boolean`, `Bool`, `Nil`, and `Closure`.

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
| `array` | variadic | Create array / convert collection->array (list/iterator) |
| `array-set!` | 3 | Set element at index |
| `Dictionary` | variadic | Create dictionary |
| `Dict` | variadic | Dictionary shorthand |
| `dict` | variadic | Dictionary shorthand |
| `dict-set!` | 3 | Set key-value |
| `Set` | variadic | Create set |
| `set-add` | 2 | Add to set |
| `set-remove` | 2 | Remove from set |
| `set-contains?` | 2 | Set membership |
| `set-size` | 1 | Set cardinality |
| `set->list` | 1 | Convert set to list |

**Coroutines:**

| Name | Arity | Description |
|------|-------|-------------|
| `coroutine` | 1 | Create coroutine |
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
| `make-iterator` | 1 | Raw thunk helper; prefer `(Iterator coll)` / `(Iterator thunk)` as public surface |
| `next` | 1 | Get next (value . rest) pair |
| `collect` | 1 | Compatibility helper: collect iterator into list |
| `to-array` | 1 | Compatibility helper: collect iterator into array |

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
| `unsafe-free!` | 1 | Free heap backing |

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
| `run-fibers` | variadic | Run thunks as fibers |
| `fiber-cancel` | 1 | Cancel a fiber |

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
| `TimePoint` | variadic | Canonical TimePoint constructor (`time-point` remains a compatibility alias) |
| `time-point` | variadic | Compatibility alias for `TimePoint` |
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
| `__raw-thread-spawn` | 1 | Raw OS thread spawn |
| `__raw-thread-join` | 1 | Raw OS thread join |
| `__raw-thread-join-timeout` | 1 | Raw OS thread join with timeout pair |
| `__raw-thread-cancel` | 1 | Raw OS thread cancel |
| `__raw-task-spawn` | 1 | Raw pooled task spawn |
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
