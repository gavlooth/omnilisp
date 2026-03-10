# finwatch — TODO

Financial watch dashboard in Omni Lisp.
Fetches live FX rates, crypto prices, and news; serves a JSON API.

## Data sources (free, no API key)
- `api.frankfurter.app`            — FX rates (USD → EUR/GBP/JPY)
- `api.coingecko.com`              — Crypto spot prices (BTC/ETH in USD)
- `hacker-news.firebaseio.com`     — Top story titles for news feed

---

## Files

- [x] `http.omni`        — generic HTTPS GET client
- [x] `models.omni`      — domain types, union, dispatch
- [x] `feed.omni`        — fetch + parse each data source
- [x] `cache.omni`       — Deduce-backed quote/news cache with TTL
- [x] `portfolio.omni`   — P&L calculation over user holdings
- [x] `logging.omni`     — append-only request log with fs + process primitives
- [x] `server.omni`      — background poller + HTTP API server
- [x] `smoke_test.omni`  — offline exercise of all modules + idiom coverage
- [x] `types.omni`       — abstract types, aliases, type reflection, conversions
- [x] `analytics.omni`   — set operations, zip, flatten, find, remove, predicates, string ops
- [x] `events.omni`      — coroutines, iterators, delay/force, reset/shift, unless, assert!
- [x] `rules.omni`       — macro DSL, gensym, eval/read-string, apply, quasiquote
- [x] `alerts.omni`      — custom effects, handle composition, alert dispatch

---

## Feature coverage checklist

### Language idioms
- [x] `|>` pipe                         — request + cache projection pipelines (server.omni, cache.omni)
- [x] `_` placeholder                   — `(+ 10 _)`, `(> _ 2)` create lambdas (portfolio.omni, smoke_test)
- [x] `match (Ok/Err)`                  — FetchResult throughout, never null? (all files)
- [x] `match [h .. t]`                  — list destructuring in decode-request (server.omni, cache.omni)
- [x] `match [a b]`                     — structural required-field checks for payloads (feed.omni)
- [x] `match [method path]`             — declarative route dispatch patterns (server.omni)
- [x] `match (nil/...)`                 — request-body, dynamic-route, and optional-handle/path branching (server.omni, logging.omni)
- [x] `match` literal branches          — status/exit-code classification without guard conditionals (http.omni, logging.omni)
- [x] `sort-by`                         — curried comparator, replaces insertion sort (cache.omni, portfolio.omni)
- [x] `partition`                       — split gainers/losers in one pass (portfolio.omni)
- [x] `for-each`                        — side-effect iteration for cache stores (server.omni)
- [x] `try`                             — stdlib error catching (feed.omni, logging.omni)
- [x] `{..}` dict destructuring params  — extract fields from POST body dicts (server.omni)
- [x] `foldr` + `match`                 — collect Ok results, drop Err in one pass (portfolio.omni, feed.omni)
- [x] `foldl` + `[a b]` destructuring   — accumulate totals with array pair (portfolio.omni)
- [x] `or` for defaults                 — `(or ts 0)` instead of `(if ts ts 0)` (feed.omni)
- [x] `compose`                         — `(compose (= _ sym) 'symbol)` predicate chains (cache.omni)
- [x] `partial`                         — bind socket in handle-client (server.omni)
- [x] `set!`                            — mutable log handle state (logging.omni)
- [x] `unless`                          — guard clause in validate-monotonic (events.omni)
- [x] `assert!`                         — validate timestamp monotonicity (events.omni)
- [x] `apply`                           — apply rule to arg list (rules.omni)
- [x] `eval` / `read-string`            — dynamic rule loading from strings (rules.omni)
- [x] `define [macro]`                  — `rule` macro for price-checking lambdas (rules.omni)
- [x] `gensym`                          — hygienic variable names in macro expansion (rules.omni)
- [x] `macroexpand`                     — debug/inspect rule expansion (rules.omni)
- [x] quasiquote (`` ` ``, `,`)         — template construction for rule descriptions (rules.omni)
- [x] `let ^rec`                        — recursive let binding (smoke_test.omni, consecutive-cross)

### Type system
- [x] `[type]` + dot field access        — Quote/Holding/PnL (models.omni)
- [x] `[union]` + `match`               — FetchResult sum type (models.omni)
- [x] typed dispatch                     — to-json per type (models.omni)
- [x] `[abstract]`                       — abstract Event base type (types.omni)
- [x] `[alias]`                          — Timestamp=Int, Price=Double (types.omni)
- [x] `type-of`                          — runtime type tag inspection (types.omni)
- [x] `is?`                              — type hierarchy checks (types.omni)
- [x] `instance?`                        — struct vs primitive check (types.omni)
- [x] dot-bracket `.[`                   — indexed access on arrays/dicts (smoke_test.omni)
- [x] `import (sym 'as alias)`           — selective import with renaming (types.omni)
- [x] `define [effect]`                  — custom alert/notify effect (alerts.omni)

### Collections + HOFs
- [x] `map` / `filter`                  — transform + select collections (all files)
- [x] `take`                            — first N elements (cache.omni, feed.omni)
- [x] `zip`                             — pair historical vs current prices (analytics.omni)
- [x] `flatten`                         — flatten nested sector groupings (analytics.omni)
- [x] `find`                            — find holding by symbol (analytics.omni)
- [x] `remove`                          — filter inverse, exclude symbols (analytics.omni)
- [x] `any?`                            — check if any holding exceeds threshold (analytics.omni)
- [x] `every?`                          — verify all positions have prices (analytics.omni)
- [x] `string-join`                     — format symbol lists (analytics.omni)
- [x] `string-contains?`               — search news titles for keywords (analytics.omni)
- [x] Sets: `set-add/contains?/remove/size/->list` — unique symbol tracking (analytics.omni)

### Concurrency + Effects
- [x] `coroutine` / `resume` / `yield` / `coroutine?` — price tick generator (events.omni)
- [x] `reset` / `shift`                 — delimited continuations, backpressure (events.omni)
- [x] `delay` / `force`                 — lazy moving average computation (events.omni)
- [x] `handle` composition              — compose alert sinks (alerts.omni)
- [x] `make-iterator` / `next` / `range-from` — lazy iterator sequences (events.omni, smoke_test)
- [x] `repeat`                          — infinite repetition iterator (smoke_test.omni)

### Conversion functions
- [x] `string->number`                  — parse number from string (smoke_test.omni)
- [x] `number->string`                  — format number as string (smoke_test.omni)
- [x] `symbol->string`                  — convert symbol to string (types.omni)

### I/O + effects
- [x] `handle` / `raise`                — per-connection error boundary (server.omni)
- [x] `spawn` + `async-sleep`           — background refresh loop (server.omni)
- [x] `run-fibers`                      — hand control to fiber scheduler (server.omni)
- [x] `http-get`                        — offloaded HTTPS GET (http.omni)
- [x] `tcp-listen` / `tcp-accept` / `tcp-read` / `tcp-write` / `tcp-close` — server (server.omni)

### Filesystem
- [x] `fs-open` / `fs-write` / `fs-close` — append-only request log (logging.omni)
- [x] `fs-stat`                         — log file size (logging.omni)
- [x] `fs-readdir`                      — directory listing (smoke_test.omni)
- [x] `fs-unlink`                       — cleanup temp files (smoke_test.omni)
- [x] `read-file`                       — read log contents (logging.omni)

### Process management
- [x] `process-spawn`                   — shell out for system uptime (logging.omni)
- [x] `process-wait`                    — wait for exit code (logging.omni)
- [x] `fs-read` on stdout handle        — capture process output (logging.omni)

### Data + parsing
- [x] `[schema]` + `validate`           — /portfolio POST body (server.omni)
- [x] `format`                          — HTTP request + response building (server.omni)
- [x] `json-parse` / `json-emit`        — API responses + server responses (feed.omni, server.omni)
- [x] `re-match`                        — path param extraction for /prices/:symbol (server.omni)
- [x] `deduce` relations                — quote + news + meta cache (cache.omni)
- [x] `module` + `import`               — all files
- [x] `string-split` / `string-trim`    — request parsing, output cleanup (server.omni, logging.omni)

---

## Runtime status (previous limitations audited)

- **Resolved (2026-03-07): `stream-yield` macro expansion to `shift`**
  Macro-expanded `shift` is now preserved as a special form during value→AST conversion.
- **Resolved (2026-03-07): `let` inside `define` inside `unless`**
  `let` bindings now round-trip correctly through macro expansion, so nested `define` bodies keep local bindings.
- **Resolved (2026-03-07): continuation resume beyond two yields**
  Multi-shot `reset`/`shift` generator flow now works for `stream-yield` + `stream-take` beyond 2 items.
- **Not reproduced in current runtime (2026-03-07): nil/false module-boundary corruption**
  Deep recursive/mutual-recursive module exports returning falsy values were re-tested and behaved correctly.

---

## Completed runtime closeout

- [x] **Live integration test** — run server, curl /health, verify 200 before first poll completes (2026-03-09: `main.omni` + `server/start` verified; `/health` returned HTTP 200 while process remained alive)
- [x] **UDP/pipe I/O** — `udp-socket`/`udp-send`/`udp-recv`, `pipe-connect`/`pipe-listen` (2026-03-09: added passing smoke coverage in `smoke_test.omni`: `pipe io: OK`, `udp io: OK`)
- [x] **`signal-handle` / `signal-unhandle`** — OS signal handlers (2026-03-09: added passing smoke coverage in `smoke_test.omni`: `signal io: OK`)
- [x] **`process-kill`** — kill child process (2026-03-09: added passing smoke coverage in `smoke_test.omni`: `process-kill: OK`)

---

## Run

```sh
LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/finwatch/main.omni
```

```sh
curl http://127.0.0.1:8181/prices
curl http://127.0.0.1:8181/prices/BTC
curl http://127.0.0.1:8181/news
curl http://127.0.0.1:8181/health
curl http://127.0.0.1:8181/logs
curl -X POST http://127.0.0.1:8181/portfolio \
     -H 'content-type: application/json' \
     -d '[{"symbol":"BTC","qty":0.5,"cost":30000},{"symbol":"ETH","qty":2,"cost":2000}]'
```
