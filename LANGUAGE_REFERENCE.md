# OmniLisp Language Reference (Refined)

This document defines the authoritative, consistent syntax for OmniLisp. It synthesizes the refinements from the `NAMING_INCONSISTENCIES.md` report into a clean specification.

---

## 1. The Character Calculus (Delimiters)

To ensure total consistency, every delimiter has a fixed semantic "Charge".

| Character | Domain | Purpose |
| :--- | :--- | :--- |
| **`()`** | **Flow** | Execution of logic, function calls, special forms, and **Persistent Linked Lists**. |
| **`[]`** | **Slot** | Argument vectors, `let` bindings, contiguous **Realized Arrays**, and Path indexing. |
| **`{}`** | **Kind** | Type system names, annotations, and parametric type construction. |
| **`.`**  | **Path** | The access operator used to reach or locate data. |
| **`^`**  | **Tag**  | Out-of-band metadata, visibility hints, and compiler instructions. |
| **`#`**  | **Sign** | Reader tags for literals (Dicts, Formats). |

---

## 2. Bindings & Functions

### 2.1 Function Definitions
Functions use the **Argument Vector `[]`** to clearly separate names from logic.

```lisp
;; Standard definition
(define add [x y] (+ x y))

;; With type annotations (Kinds in {})
(define add [x {Int} y {Int}] {Int}
  (+ x y))

;; Lambda shorthand
(lambda [x] (* x x))
(-> [x y] (+ x y))
```

### 2.2 Local Bindings (`let`)
`let` uses flat, even-numbered forms inside the **Slot Bracket `[]`**.

```lisp
(let [x 10
      y 20]
  (+ x y))
```

---

## 3. The Trinity of Sequences

| Type | Syntax | Nature |
| :--- | :--- | :--- |
| **List** | `'(1 2 3)` | Persistent, recursive, uses `()`. |
| **Array** | `[1 2 3]` | Contiguous, realized, indexed, uses `[]`. |
| **Iterator**| `(range 10)`| Transient, one-shot process, returns `{Iter T}`. |

### 3.1 Lazy Pipelines
Operations like `map` and `filter` return objects of type `{Iter T}`. They do not allocate storage unless explicitly "Materialized".

```lisp
(define lazy-flow (map inc (range 100)))  ; No allocation
(define realized  (array lazy-flow))      ; Materialized to Array []
(define linked    (list lazy-flow))       ; Materialized to List ()
```

---

## 4. Access & Mutation

### 4.1 Path-as-Locator
The dot `.` bridges **Flow** and **Slot**. It identifies a location in memory.

*   **Static**: `person.name`
*   **Dynamic**: `arr.[idx]`
*   **Symbolic**: `obj.[:key]`

### 4.2 Mutation Operators
*   **`set!`**: Modifies a **Name** (Binding). Returns the **Value**.
    *   `(set! x 10)`
*   **`put!`**: Modifies a **Slot** (Path). Returns the **Root Object**.
    *   `(put! person.age 30)`
*   **`update!`**: Transforms a **Slot** in-place. Returns the **Root Object**.
    *   `(update! player.hp dec)`
*   **`update`**: Transforms a **Slot** functionally. Returns a **New Object**.
    *   `(define p2 (update p1.x inc))`

---

## 5. Metadata & Strings

### 5.1 The `^` Tag
Used for information that does not change the "Kind" (Type) but changes the context.
```lisp
(define ^:private [version {Float} 1.0])
```

### 5.2 String Formatting
*   **`#fmt"..."`**: Modern interpolation (`"Count: {n}"`).
*   **`#clf"..."`**: Traditional Common Lisp `cl-format` (`"Count: ~D"`).

---

## 6. Logic & Pattern Matching

### 6.1 Flat Match
`match` uses an even number of forms for branches.
```lisp
(match opt
  (Some v) v
  None default)
```
