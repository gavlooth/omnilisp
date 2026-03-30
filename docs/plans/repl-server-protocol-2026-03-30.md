# Omni REPL Server Protocol Proposal (2026-03-30)

Status: phase 1 partially implemented

## Current Implementation Boundary

Shipped now:

- `omni --repl-server --socket <path>`
- `omni --repl-server --stdio`
- `omni --repl-server --tcp <host> <port>`
- `--project`
- `--project <dir>`
- Unix domain socket transport
- stdio transport
- TCP transport
- nREPL-style TCP discovery file:
  - `--repl-server --tcp` writes `.omni-repl-port` in the current working
    directory after bind succeeds
- per-clone project preload:
  - when the server starts with `--project [dir]`, each new `clone` session
    resolves an Omni project the same way as `omni --repl --project` and
    evaluates `src/main.omni` before emitting the `session` event
- newline-delimited JSON protocol framing
- ops:
  - `describe`
  - `clone`
  - `close`
  - `complete`
  - `eval`
  - `interrupt`
  - `stdin`
  - `load-file`
- eval modes:
  - `expr`
  - `program`
- streamed events currently used by the server:
  - `describe`
  - `session`
  - `complete`
  - `out`
  - `value`
  - `loaded`
  - `done`
  - `interrupted`
  - `error`

Not shipped yet:

- concurrent multi-client handling
- request queueing beyond one in-flight runtime operation per stream

The rest of this document remains the target design direction for the next
slices.

## Objective

Define a real network-capable Omni REPL protocol for editor/tool integrations.
The target is not a plain socket terminal REPL. The target is a structured,
multi-message protocol that supports:

- persistent sessions,
- streamed stdout/stderr/value events,
- interrupts,
- capability discovery,
- editor-facing ops such as eval, load-file, and completion.

## Recommendation

Omni should mimic:

- `nREPL` for the control plane:
  - request/response ops,
  - explicit sessions,
  - asynchronous multi-message replies,
  - capability discovery via `describe`,
  - interruptable evaluation.
- `pREPL` for the eval event plane:
  - distinct `out`, `err`, `value`, and terminal status events,
  - no scraping of human REPL text,
  - no ambiguity between protocol output and user program output.

Omni should not copy the exact nREPL wire format unless compatibility with
existing nREPL clients becomes a product goal.

## Transport Choice

Preferred first transport:

- Unix domain socket
- newline-delimited JSON (JSON Lines / NDJSON)

Current discovery convenience:

- TCP mode also emits a `.omni-repl-port` file in the current working
  directory so editor tooling can find the active listener without scraping
  process arguments.

Why:

- fits local editor integration on Linux/macOS,
- simpler than bencode/EDN for Omni-first tooling,
- easy to inspect manually,
- preserves the current one-line structured transport model from
  `omni --repl --json`.

Deferred transport options:

- TLS for remote attach
- richer stdio/socket/TCP multi-client concurrency

## Proposed CLI Surface

Primary new mode:

```bash
omni --repl-server --socket /tmp/omni.sock
omni --repl-server --socket /tmp/omni.sock --project
```

Optional convenience modes:

```bash
omni --repl-server                    # chooses a default local socket path
omni --repl-server --stdio           # protocol on stdin/stdout for tools
omni --repl-server --tcp 127.0.0.1 5555
omni --repl-server --tcp 127.0.0.1 5555 --project ./demo
```

Recommended policy:

- ship Unix socket first,
- keep TCP unauthenticated only for explicit trusted-network use until
  authentication/bind policy is explicit,
- treat `--repl --json` as the legacy single-client stdio transport.

## Framing Rules

Each protocol message is one JSON object per line.

Rules:

- UTF-8 only
- one complete JSON object per line
- no pretty-printed multi-line JSON on the wire
- stdout/stderr from user code must never appear raw on the transport stream
- all user-visible output must be wrapped as protocol events

This preserves the key invariant missing from the current local transport:

- transport bytes and program output bytes must never share the same channel

## Message Model

Each request has:

- `id`: client-generated request identifier
- `op`: operation name
- optional `session`: session identifier
- operation-specific fields

Each server message has:

- `id`: original request identifier
- `session`: session identifier when relevant
- `event`: event kind
- event-specific fields

One request may produce many server messages.

Every request must end with exactly one terminal event:

- `done`
- `error`
- `interrupted`

## Core Operations

### `describe`

Purpose:

- discover protocol version,
- list supported ops,
- list advertised capabilities.

Request:

```json
{"id":"1","op":"describe"}
```

Response:

```json
{
  "id":"1",
  "event":"describe",
  "protocol":"omni-repl/1",
  "server":{
    "implementation":"omni",
    "version":"0.2.0"
  },
  "transports":["unix","stdio"],
  "ops":{
    "clone":{},
    "close":{},
    "describe":{},
    "eval":{"modes":["expr","program"]},
    "interrupt":{},
    "load-file":{},
    "complete":{}
  }
}
```

Terminal:

```json
{"id":"1","event":"done"}
```

### `clone`

Purpose:

- create a new logical session with isolated dynamic REPL state.

Request:

```json
{"id":"2","op":"clone"}
```

Response:

```json
{"id":"2","event":"session","session":"s1"}
{"id":"2","event":"done","session":"s1"}
```

### `close`

Purpose:

- close one session and release its resources.

Request:

```json
{"id":"3","op":"close","session":"s1"}
```

Terminal:

```json
{"id":"3","event":"done","session":"s1"}
```

### `eval`

Purpose:

- evaluate one expression or one program in a session.

Request:

```json
{"id":"4","op":"eval","session":"s1","code":"(print 1)","mode":"expr"}
```

Program-mode request:

```json
{"id":"5","op":"eval","session":"s1","code":"(define x 10)\n(print x)","mode":"program"}
```

Streamed response example:

```json
{"id":"4","session":"s1","event":"out","text":"1"}
{"id":"4","session":"s1","event":"value","value":"#<void>"}
{"id":"4","session":"s1","event":"done"}
```

Error example:

```json
{
  "id":"4",
  "session":"s1",
  "event":"error",
  "error":{
    "code":"runtime/eval-error",
    "message":"unknown symbol: x",
    "range":{
      "start":{"line":0,"character":3},
      "end":{"line":0,"character":4}
    }
  }
}
```

### `interrupt`

Purpose:

- request cancellation of an in-flight eval in the same session.

Request:

```json
{"id":"6","op":"interrupt","session":"s1","target":"4"}
```

Response:

```json
{"id":"6","session":"s1","event":"done"}
```

Target eval terminal event:

```json
{"id":"4","session":"s1","event":"interrupted"}
```

### `stdin`

Purpose:

- feed standard input to an eval that is currently blocked on input.

Current status:

- shipped in the current server for `--stdio`, Unix-socket, and TCP
  transports,
- request data is routed into the session input queue consumed by
  `(read-line)`,
- optional `eof: true` closes the routed input stream for the session.

Request:

```json
{"id":"7","op":"stdin","session":"s1","target":"4","data":"hello\n"}
```

Terminal:

```json
{"id":"7","session":"s1","event":"done"}
```

### `load-file`

Purpose:

- preload one file into an existing session with source-path context.

Request:

```json
{"id":"8","op":"load-file","session":"s1","path":"/abs/path/demo.omni"}
```

Suggested response shape:

- emit `out` / `err` as needed,
- emit one final `loaded` or `error`,
- terminate with `done` only on success.

Success example:

```json
{"id":"8","session":"s1","event":"loaded","path":"/abs/path/demo.omni"}
{"id":"8","session":"s1","event":"done"}
```

### `complete`

Purpose:

- editor completion for the current namespace/session.

Request:

```json
{"id":"9","op":"complete","session":"s1","prefix":"prin"}
```

Response:

```json
{
  "id":"9",
  "session":"s1",
  "event":"complete",
  "items":[
    {"candidate":"print","kind":"function"},
    {"candidate":"println","kind":"function"}
  ]
}
```

Terminal:

```json
{"id":"9","session":"s1","event":"done"}
```

## Event Taxonomy

The eval stream should use a small, explicit event set.

Non-terminal events:

- `out`
- `err`
- `value`
- `tap`
- `loaded`
- `complete`
- `describe`
- `session`

Terminal events:

- `done`
- `error`
- `interrupted`

Recommended meanings:

- `out`: user program standard output
- `err`: user program standard error
- `value`: rendered final evaluation value
- `tap`: auxiliary observational data stream
- `error`: structured protocol or evaluation failure
- `done`: normal terminal marker with no failure
- `interrupted`: normal terminal marker caused by explicit cancellation

## Session Semantics

Recommended default:

- sessions are explicit, not implicit,
- every stateful op except `describe` and `clone` requires `session`,
- one session may have at most one active `eval` at a time initially.

Reason:

- simpler interruption semantics,
- simpler stdin routing,
- easier editor model.

Possible future extension:

- multiple concurrent evals per session,
- or per-request child task identifiers inside one session.

That should be deferred until the single-eval-per-session contract is solid.

## Error Policy

Protocol errors and evaluation errors must stay distinct.

Protocol-shape failure example:

```json
{
  "id":"10",
  "event":"error",
  "error":{
    "code":"protocol/invalid-request",
    "message":"missing field 'op'"
  }
}
```

Evaluation failure example:

```json
{
  "id":"11",
  "session":"s1",
  "event":"error",
  "error":{
    "code":"runtime/eval-error",
    "message":"division by zero"
  }
}
```

Capability failure example:

```json
{
  "id":"12",
  "session":"s1",
  "event":"error",
  "error":{
    "code":"protocol/unsupported-op",
    "message":"operation 'inspect' is not supported by this server"
  }
}
```

## Compatibility With Current `--repl --json`

Current shipped transport:

- single request shape: `{"id","input","mode"}`
- single reply shape: `{"id","ok","value","error"}`
- stderr is out-of-band at the process level

Recommended migration:

### Phase 1

- keep `--repl --json` unchanged,
- add `--repl-server --stdio` using the new protocol,
- ship `interrupt` for in-flight `eval` / `load-file`.

### Phase 2

- move first-party editor tooling to `--repl-server --stdio` or Unix socket.

### Phase 3

- either:
  - keep `--repl --json` as a stable legacy compatibility transport,
  - or add a negotiated `protocol` field to let one binary serve both surfaces.

## Why Not Exact nREPL?

Reasons not to copy exact nREPL wire semantics:

- Omni is not in the Clojure middleware ecosystem,
- JSON lines are easier for Omni-first tools than bencode or EDN,
- Omni should keep a smaller initial op surface,
- exact compatibility would pressure Omni into Clojure-specific assumptions.

Reasons to copy nREPL’s architecture anyway:

- sessions are a proven editor model,
- multiple responses per request are required for real eval tooling,
- `describe`-driven capability discovery scales better than ad hoc flags,
- interrupt/load-file/complete are not optional if editor integration is the target.

## Recommended First Shipped Boundary

If implementation starts now, the narrowest useful slice is:

- transport: Unix socket and stdio
- ops:
  - `describe`
  - `clone`
  - `close`
  - `eval`
  - `interrupt`
  - `complete`
  - `load-file`
- events:
  - `out`
  - `err`
  - `value`
  - `error`
  - `done`
  - `interrupted`

Do not block the first shipped slice on:

- TCP,
- TLS,
- rich introspection,
- debugger-specific ops,
- exact nREPL client compatibility.

## Concrete Recommendation

Best protocol to mimic:

- `nREPL` for operation/session structure
- `pREPL` for streamed eval event categories

Best Omni-native wire contract:

- JSON lines
- Unix socket first
- explicit `describe`
- explicit sessions
- multi-message eval replies
- no raw program stdout/stderr on the protocol transport
