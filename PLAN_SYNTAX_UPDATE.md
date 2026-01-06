# OmniLisp Syntax Update Plan (Detailed)

This plan maps the requirements of `LANGUAGE_REFERENCE.md` to specific code changes.

## Phase 1: Reader & Parser Extensions
**Goal**: Enable the parser to read `{}` types, `.` paths, and `^` metadata without error.

### 1.1 Grammar Updates (`csrc/parser/parser.c`)
*   **Add Tokens**:
    *   Add `R_LBRACE` (`{`) and `R_RBRACE` (`}`).
    *   Add `R_CARET` (`^`).
    *   Add `R_DOT` (`.`) - distinct from `R_FLOAT` decimal point.
*   **Add Rules**:
    *   `R_TYPE_EXPR`: `{ WS LIST_INNER }` (Reuse list logic for contents).
        *   **Action**: `act_type_expr` -> Produces `(type ...)` list structure (internally `T_CELL` with head `type`).
    *   `R_PATH_SEGMENT`: `R_SYM | R_INT | R_STRING | R_ARRAY`.
    *   `R_PATH`: `R_SYM (R_DOT R_PATH_SEGMENT)+`.
        *   **Action**: `act_path` -> Produces `(path root segment...)`.
*   **Update `R_EXPR`**:
    *   Include `R_TYPE_EXPR` and `R_PATH` as alternatives.

### 1.2 AST/Value Updates (`src/runtime/omni_eval.h`)
*   No new C types needed yet if we map syntax to list forms:
    *   `{Int}` -> `(type Int)`
    *   `obj.field` -> `(path obj field)`
    *   `^:meta` -> `(metadata :meta)`

## Phase 2: Compiler & Evaluator Updates
**Goal**: Handle the new list forms produced by the parser.

### 2.1 Function Definition (`src/runtime/eval/omni_eval.c`)
*   **Function**: `eval_define` (approx. line 6600)
*   **Change**: Update detection logic for function definitions.
    *   *Current*: Checks if first arg is `T_CELL` -> `(define (name args) body)`.
    *   *New*: Check if first arg is `T_SYM` (name) AND second arg is `(array ...)` -> `(define name [args] body)`.
    *   **Logic**:
        ```c
        if (is_sym(first) && is_array(second)) {
            // Function definition
            Value* name = first;
            Value* params = cdr(second); // Unwrap array
            Value* body = args->next->next;
            return eval_lambda_impl(params, body, env);
        }
        ```

### 2.2 Path Resolution (`src/runtime/eval/omni_eval.c`)
*   **New Function**: `eval_path(Value* args, Env* env)`
    *   Handle `(path obj segment...)`.
    *   Resolve `obj`.
    *   Iterate segments. If segment is symbol `field`, treat as `:field`. If segment is `(array expr)`, evaluate `expr` as index.
    *   Generate `get` calls or direct field access.

### 2.3 Mutation Operators (`src/runtime/eval/omni_eval.c`)
*   **New Special Forms**:
    *   `put!`: `eval_put_bang`
    *   `update!`: `eval_update_bang`
    *   `update`: `eval_update`
*   **Implementation**:
    *   `put! target path value`:
        *   Resolve target.
        *   Resolve path to parent object and final slot.
        *   Perform mutation.
        *   Return target.
    *   `update! target path fn`:
        *   Resolve value at path.
        *   Apply `fn(value)`.
        *   `put!` result back.
        *   Return target.

## Phase 3: Codegen Updates (`csrc/codegen/codegen.c`)
**Goal**: Ensure compiled C code reflects the new semantics.

### 3.1 Define (`codegen_define`, line 450)
*   **Change**: Add branch for `SYMBOL` name + `ARRAY` params.
    *   Generate `static Obj* name(Obj* args...) { ... }`.

### 3.2 Path Access
*   **Change**: Detect `(path ...)` forms in `codegen_expr`.
    *   Emit C code for property access: `get_field(obj, "field")`.

## Phase 4: Migration
*   **Action**: Update `tests/test_simple.c` and other tests to use `[ ]` for arguments.