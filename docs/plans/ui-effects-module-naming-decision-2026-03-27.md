# UI Effects Module Naming Decision (2026-03-27)

Status: `decided`  
Owner: Codex workflow

## Problem

The planned high-level UI wrapper needs one canonical naming model for:

- view/data constructors,
- runtime helpers,
- effect tags used with `signal` / `handle`.

The ambiguity was whether a real `ui` module should expose effect tags as:

- `ui/open`, or
- `ui.open`.

## Decision

For real module-owned public UI surface, use dot-qualified names consistently.

This means:

- view/data constructors use `ui.*`
- runtime helpers use `ui.*`
- effect tags exported by the `ui` module also use `ui.*`

Canonical examples:

- `ui.text`
- `ui.button`
- `ui.window`
- `ui.graph`
- `ui.open`
- `ui.render`
- `ui.read_event`
- `ui.invalidate`
- `ui.close`
- `ui.post_event`

Usage shape:

```lisp
;; in ui.omni
(module ui (export text window open render close))

(define [effect] (open (^Any spec)))
(define [effect] (render (^Any view)))
(define [effect] (close (^Any request)))

;; outside the module
(signal ui.open spec)
(signal ui.render view)

(handle
  (signal ui.open spec)
  (ui.open x
    (resolve nil)))
```

## Rationale

1. Dot notation is the language's module-access mechanism.
2. A real `ui` module should not introduce a second namespacing convention for
   its own public symbols.
3. Mixed surfaces like `ui.text` plus `ui/open` create avoidable inconsistency.
4. Existing slash-form I/O effect names are canonical repo truth today, but
   they behave as effect-tag spellings rather than evidence that slash-form is
   the right naming rule for real module-owned UI surface.

## Rejected Options

### 1. Mixed model

Rejected:

- constructors as `ui.text`
- effects as `ui/open`

Reason:

- two namespace conventions for one module-owned surface.

### 2. Bare effect names

Rejected:

- `open`
- `render`
- `close`

Reason:

- too collision-prone,
- loses the owning family/module in external code.

### 3. Slash-form for all new UI effects

Rejected:

- `ui/open`
- `ui/render`
- `ui/close`

Reason:

- conflicts with the decision that dot notation expresses real module access.

## Consequences

1. The future high-level wrapper should expose:
   - builders like `ui.text`, `ui.window`, `ui.graph`
   - effect tags like `ui.open`, `ui.render`, `ui.close`
2. Raw FTXUI ABI names remain backend/internal and do not constrain the public
   wrapper naming.
3. If the repo later introduces a real `io` module, that should be handled as a
   separate migration decision rather than reusing this note implicitly.
