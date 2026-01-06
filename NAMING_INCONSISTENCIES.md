# OmniLisp Syntax Ambiguity & Consistency Report (Final Authorization)

## 1. Problem Statement
To ensure total consistency across documentation and implementation, OmniLisp adopts a strict functional mapping for all delimiters. This eliminates ambiguity between value-level code, the type system, and the metadata system.

## 2. Definitive Character Roles

| Character | Domain | Role | Specific Uses |
| :--- | :--- | :--- | :--- |
| **`()`** | **Execution** | **Logic & Links** | Function calls, special forms, persistent Linked Lists. |
| **`[]`** | **Definition** | **Slots & Blocks** | Argument vectors, binding pairs, contiguous Arrays. |
| **`{}`** | **Types** | **Kinds & Structure** | Type annotations, type constructors, type headers. |
| **`^`**  | **Metadata** | **Hints & Tags** | Visibility, compiler hints, docstrings. |

---

## 3. Implementation Rules

### Rule 1: Flat Match Forms
`match` branches use an even number of forms. Grouping with `[]` is optional but encouraged for complex patterns.
*   *Correct*: `(match x (Some v) v None default)`

### Rule 2: The Argument Vector
Function arguments use `[]`. This distinguishes the "binding site" from the function logic.
*   *Correct*: `(define add [x y] (+ x y))`

### Rule 3: Iterators as Type Objects
Sequence operations (`map`, `filter`, `range`) are functions that return instances of the `{Iter T}` type.
*   *Lazy Default*: `(map f xs)` returns a transient `{Iter T}` object.
*   *Materialization*: To realize an iterator into storage, wrap it in a constructor: `(list (range 5))` or `(array (range 5))`.

### Rule 4: Type Braces `{}`
Curly braces are reserved strictly for **Types and Type Annotations**.
*   *Annotation*: `[x {Int}]`
*   *Type Construction*: `{Option {List String}}`
*   *Generic Header*: `{struct Pair [T]}`

### Rule 5: Metadata Prefix `^`
The caret is reserved strictly for **Metadata and Hints**. This information is typically "extra-logical"—it doesn't change the type of the value, but changes how the compiler or environment treats the binding.
*   *Visibility*: `^:private`
*   *Optimization*: `^:hot`, `^:inline`
*   *Documentation*: `^"Calculates the sum"`

---

## 4. Syntax Examples (Final Spec)

### 4.1 Type Annotation vs Metadata
```lisp
;; {Float} is the Type (Kind).
;; ^:private is the Metadata (Hint).
(define ^:private [version {Float} 1.0])
```

### 4.2 Function with Type Signatures
```lisp
(define process [xs {Iter Int}] {Iter Int}
  (filter (-> n (> n 10)) 
    (map (-> n (* n 2)) xs)))
```

---

## 5. Architectural Intent
1.  **Code** `()`: What the program **does**.
2.  **Bindings** `[]`: Where the program **stores** it.
3.  **Types** `{}`: What the data **is**.
4.  **Metadata** `^`: How the program **behaves** (hints).
