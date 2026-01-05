# OmniLisp Implementation Plan

## Overview

This document details the complete implementation plan for OmniLisp.
The C runtime remains unchanged. The Go compiler frontend is rewritten 100%.

---

## Type System: Julia-Style Optional Types

**OmniLisp uses Julia's approach to types: types are OPTIONAL everywhere.**

### Type Annotations Are Always Optional

```lisp
;; No types - fully inferred (like Julia's duck typing)
(define (add x y)
  (+ x y))

;; Partial types - annotate what you want
(define (add [x {Int}] y)
  (+ x y))

;; Full types - for documentation or dispatch
(define (add [x {Int}] [y {Int}]) {Int}
  (+ x y))
```

### Key Principles (from Julia)

1. **Types don't affect runtime behavior** - they guide dispatch and documentation
2. **Multiple dispatch on all arguments** - not just first argument
3. **Parametric types** - `{Array T}`, `{Option T}`, `{Result T E}`
4. **Abstract type hierarchy** - for organizing dispatch
5. **No class-based OOP** - structs + multiple dispatch

### Syntax Forms

| Form | Meaning |
|------|---------|
| `x` | Untyped parameter |
| `[x {Int}]` | Typed parameter |
| `[x 10]` | Parameter with default |
| `[x {Int} 10]` | Typed parameter with default |
| `{Int}` after params | Return type annotation |

---

## Architecture: What Changes

| Component | Change Level | Description |
|-----------|--------------|-------------|
| **src/runtime/reader/pika.c** | New Implementation | Pika Parser: New bracket semantics, dot notation, string interpolation (C Implementation) |
| **pkg/ast/value.go** | 80% Rewrite | New types: Array, Dict, Tuple, Struct, Enum, TypeAnnotation |
| **pkg/eval/eval.go** | 95% Rewrite | New special forms, multiple dispatch, match semantics |
| **pkg/eval/primitives.go** | 60% Rewrite | New primitive operations |
| **pkg/codegen/codegen.go** | 50% Modify | Code generation for new constructs |
| **pkg/analysis/*.go** | 30% Modify | Adapt analysis for new AST |
| **runtime/** | 0% | Unchanged - C runtime stays exactly the same |

---

## Phase 1: Parser Implementation (src/runtime/reader/pika.c)

We will implement the Pika parser directly in C to support the "Tower of Interpreters" model (Level 0 parsing).

### 1.1 C Tokenizer

```c
typedef enum {
    TOK_LPAREN,   // (
    TOK_RPAREN,   // )
    TOK_LBRACKET, // [
    TOK_RBRACKET, // ]
    TOK_LBRACE,   // {
    TOK_RBRACE,   // }
    TOK_HASHBRACE,// #{
    TOK_DOT,      // .
    TOK_STRING,   // "..."
    TOK_INT,      // 123
    TOK_FLOAT,    // 123.456
    TOK_SYMBOL,   // name
    TOK_KEYWORD,  // :key
    TOK_EOF
} TokenType;

// Tokenizer state
typedef struct {
    const char* input;
    size_t pos;
    size_t len;
} Lexer;
```

### 1.2 Parser Changes (C)

The parser will construct `OmniValue` objects directly using the runtime's object system.

| Feature | Description |
|---------|-------------|
| `parse_list` | `()` -> List or Function Call |
| `parse_array` | `[]` -> Array Object |
| `parse_type` | `{}` -> Type Object |
| `parse_dict` | `#{}` -> Dictionary Object |
| `parse_dot` | `obj.field` -> Accessor form |

### 1.3 Dot Notation Parsing (C)

```c
// Logic to transform tokens into 'get' forms:
// obj.field     -> (get obj 'field)
// obj.(expr)    -> (get obj expr)
```

### 1.4 String Interpolation (C)

The C parser must handle `$` inside strings and generate `(string-concat ...)` forms.

### 1.5 Files to Create/Modify

```
src/runtime/reader/pika.c       # Core parser logic
src/runtime/reader/pika.h       # Public API
src/runtime/reader/token.h      # Token definitions
src/runtime/include/omnilisp.h  # Expose omni_read()
```

---

## Phase 2: AST Rewrite (pkg/ast/value.go)

### 2.1 New Value Tags

[Go code removed]

### 2.2 Value Struct Extensions

[Go code removed]

### 2.3 Files to Modify

```
pkg/ast/value.go      # Add new tags and constructors
pkg/ast/types.go      # New file for type system structures
```

---

## Phase 3: Evaluator Rewrite (pkg/eval/eval.go)

### 3.1 New Special Forms

| Form | Description |
|------|-------------|
| `define` | Unified binding (values, functions, types, macros) |
| `let` | With `:seq`, `:rec`, named-let modifiers |
| `match` | Pattern matching (special form, not macro) |
| `if` | Binary conditional |
| `lambda` | Anonymous function |
| `set!` | Mutation |
| `begin` | Sequencing |
| `quote` | Quoting |
| `prompt` / `control` | Delimited continuations |
| `spawn` | Green thread |
| `channel` | Create channel |

### 3.2 Match as Special Form

[Go code removed]

### 3.3 Pattern Matching Implementation

[Go code removed]

### 3.4 Multiple Dispatch

[Go code removed]

### 3.5 Type System

[Go code removed]

### 3.6 Files to Modify

```
pkg/eval/eval.go        # Major rewrite
pkg/eval/pattern.go     # Pattern matching (expand)
pkg/eval/dispatch.go    # New file for multiple dispatch
pkg/eval/types.go       # New file for type system
pkg/eval/primitives.go  # Update primitives
```

---

## Phase 4: Define Form Implementation

### 4.1 Unified Define Syntax

[Go code removed]

---

## Phase 5: Let Form Implementation

### 5.1 Let Modifiers

[Go code removed]

---

## Phase 6: Hygienic Macros

### 6.1 Syntax Objects

[Go code removed]

### 6.2 Macro Expansion

[Go code removed]

---

## Phase 7: Module System

### 7.1 Module Structure

[Go code removed]

### 7.2 Module Forms

```lisp
(module MyModule
  (export f1 f2 MyType)

  (import [OtherModule :only (helper)])

  (define (f1 x) ...)
  (define (f2 y) ...))

(import MyModule)
(import [MyModule :as M])
(import [MyModule :only (f1)])
```

---

## Phase 8: Concurrency

### 8.1 Green Threads

[Go code removed]

### 8.2 Channels

[Go code removed]

---

## Phase 9: Code Generation Updates

### 9.1 Array Code Gen

```c
// [1 2 3] becomes:
OmniArray* arr = omni_array_new(3);
omni_array_set(arr, 0, mk_int(1));
omni_array_set(arr, 1, mk_int(2));
omni_array_set(arr, 2, mk_int(3));
```

### 9.2 Struct Code Gen

```c
// (Point 10.0 20.0) becomes:
Obj* point = mk_struct("Point", 2);
struct_set_field(point, 0, mk_float(10.0));
struct_set_field(point, 1, mk_float(20.0));
```

### 9.3 Match Code Gen

```c
// Pattern matching becomes switch/if chains
switch (get_type(value)) {
    case TYPE_INT:
        if (get_int(value) == 0) { ... }
        break;
    case TYPE_STRUCT:
        if (strcmp(get_struct_name(value), "Point") == 0) {
            Obj* x = struct_get_field(value, 0);
            Obj* y = struct_get_field(value, 1);
            ...
        }
        break;
}
```

---

## Implementation Order

### Phase 1: Parser Foundation (C) ✅
- [ ] Implement tokenizer in C (`src/runtime/reader/`)
- [ ] Implement array literal parsing `[]`
- [ ] Implement type literal parsing `{}`
- [ ] Implement dict literal parsing `#{}`
- [ ] Implement dot notation `obj.field`
- [ ] Implement string interpolation
- [ ] Bind `omni_read` to runtime

### Phase 2: AST Extensions ✅ COMPLETE
- [x] Add new Value tags (TArray, TDict, TTuple, etc.)
- [x] Implement constructors and predicates
- [x] Add type annotation structures
- [x] AST tests

### Phase 3: Evaluator Primitives ✅ COMPLETE
- [x] Array operations (make-array, array-ref, etc.)
- [x] Dict operations (make-dict, dict-ref, etc.)
- [x] Tuple operations
- [x] Keyword operations
- [x] Nothing operations
- [x] Generic get for dot notation

### Phase 4: Pattern Matching ✅ COMPLETE
- [x] Array patterns `[a b .. rest]`
- [x] Dict patterns `#{:key pat}`
- [x] Predicate patterns `(? pred)`
- [x] Guards `:when`
- [x] OmniLisp branch syntax `[pattern result]`

### Phase 5: Multiple Dispatch ✅ COMPLETE
- [x] Type registry (abstract, concrete, parametric)
- [x] Subtype checking
- [x] Multiple dispatch implementation
- [x] Method definition and lookup
- [x] `(define (f [x {Type}]) ...)` syntax

### Phase 6: Module System ✅ COMPLETE
- [x] `(module Name (export ...) body)`
- [x] `(import Module)`
- [x] `:as`, `:only`, `:except`, `:refer` modifiers
- [x] Qualified name lookup

### Phase 7: Hygienic Macros ✅ COMPLETE
- [x] `(define [macro name] (params) body)`
- [x] Syntax objects with lexical context
- [x] `#'` / `syntax-quote`
- [x] `~` / `unquote`
- [x] `~@` / `unquote-splicing`

### Phase 8: Let Modifiers ✅ COMPLETE
- [x] Array-style bindings `[x 1 y 2]`
- [x] `:seq` modifier (sequential binding)
- [x] `:rec` modifier (recursive binding)
- [x] Named let `(let loop [i 0] ...)`

### Phase 9: Code Generation ✅ COMPLETE
- [x] Update codegen for new AST
- [x] Array/Dict/Tuple code generation
- [x] Keyword/Nothing code generation
- [x] Type literal code generation

### Phase 10: Tower Semantics ✅ COMPLETE
- [x] `lift` - value to code
- [x] `run` - execute code at base
- [x] `EM` - escape to meta
- [x] `shift` - go up n levels
- [x] Handler system (get-meta, set-meta!, with-handlers)

### Pending
- [ ] Struct/Enum code generation
- [ ] Error messages polish
- [ ] Documentation
- [ ] Performance tuning
- [ ] Full test suite

---

## File Change Summary

### Complete Rewrites
- `pkg/eval/eval.go` - New evaluator core

### New Implementations (C)
- `src/runtime/reader/pika.c` - C Parser (Pika)
- `src/runtime/reader/pika.h` - C Parser Headers

### Major Modifications
- `pkg/ast/value.go` - New types
- `pkg/eval/primitives.go` - New operations
- `pkg/codegen/codegen.go` - New constructs

### New Files
- `pkg/eval/dispatch.go` - Multiple dispatch
- `pkg/eval/types.go` - Type system
- `pkg/eval/module.go` - Module system
- `pkg/eval/macro.go` - Hygienic macros
- `pkg/ast/types.go` - Type structures

### Unchanged
- `runtime/src/*.c` - C runtime (no changes)
- `runtime/tests/*.c` - Runtime tests (no changes)

---

## Verification Checklist

### Parser (C)
- [ ] `()` parses as forms
- [ ] `[]` parses as arrays/bindings
- [ ] `{}` parses as type forms
- [ ] `#{}` parses as dicts
- [ ] `:symbol` shorthand works
- [ ] `$interpolation` works
- [ ] `obj.field` parses correctly
- [ ] `.field` creates accessor lambda

### Evaluator ✅
- [x] `define` handles all cases
- [x] `let` with `:seq`, `:rec` works
- [x] Named `let` (loop) works
- [x] `match` with patterns works
- [x] Guards (`:when`) work
- [x] Multiple dispatch works
- [x] Type hierarchy respects

### Types (Partial)
- [x] Abstract types define hierarchy
- [ ] Structs instantiate correctly
- [ ] Enums with data work
- [x] Parametric types work (basic)
- [x] Optional type annotations work

### Code Gen ✅
- [x] Arrays generate correct C
- [x] Dicts generate correct C
- [x] Tuples generate correct C
- [ ] Structs generate correct C
- [ ] Match generates efficient code
- [x] ASAP memory management preserved

### Tower Semantics ✅
- [x] `lift` works
- [x] `run` works
- [x] `EM` works
- [x] Handler system works
- [x] `with-handlers` scoping works
