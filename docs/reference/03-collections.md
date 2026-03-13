# Collections, Strings & Math

**[Back to Index](../OMNI_REFERENCE.md)**

---

## 9. Collections

Omni has three collection types plus sets.

### Lists (Linked)

```lisp
'(1 2 3)                   ;; quoted list
(list 1 2 3)               ;; explicit construction
(List [1 2 3])             ;; canonical constructor/conversion surface
(cons 1 (cons 2 nil))      ;; manual construction
(car '(1 2 3))             ;; => 1
(cdr '(1 2 3))             ;; => (2 3)
(length '(1 2 3))          ;; => 3
```

### Arrays (Contiguous, Mutable)

```lisp
[1 2 3]                    ;; array literal (desugars to (array 1 2 3))
(Array 1 2 3)              ;; canonical construction
(array 1 2 3)              ;; compatibility alias
(ref arr 0)                ;; => first element
(push! arr 4)              ;; append element (mutates, returns Void)
(set! arr 0 99)            ;; generic update form (returns Void)
(array-set! arr 0 99)      ;; set element at index (returns Void)
(length arr)               ;; => size
(Array '(1 2 3))           ;; list -> array conversion
(list [1 2 3])             ;; array -> list conversion
```

### Dicts (Hash Map, Mutable)

```lisp
{'name "Alice" 'age 30}    ;; dict literal (desugars to (dict ...))
(Dictionary 'name "Alice")  ;; canonical construction
(dict 'name "Alice")        ;; compatibility alias
(ref d 'name)               ;; => "Alice"
(set! d 'email "a@b")       ;; generic update form (returns Void)
(dict-set! d 'email "a@b")  ;; set key (returns Void)
(has? d 'age)               ;; => true
(keys d)                    ;; => (name age)
(values d)                  ;; => ("Alice" 30)
(remove! d 'age)            ;; remove key (returns Void)
(length d)                  ;; => number of entries
```

### Sets

```lisp
(Set 1 2 3)                ;; create set
(set-add s 4)              ;; => Void (mutates set in place)
(set-remove s 2)           ;; => Void (mutates set in place)
(set-contains? s 1)        ;; => true
(set-size s)               ;; => number of elements
```

Sets now have a distinct builtin `Set` runtime type symbol. `type-of` reports
`Set` for `(Set ...)` values, and printed set values use constructor-shaped
syntax such as `(Set 1 2 3)`.

`array-set!` and `dict-set!` remain available as compatibility aliases, but
`set!` is the preferred generic update surface.

Naming policy for new code/examples:
- prefer `List`, `Array`, and `Dictionary` as constructor/coercion surfaces
- keep `list` as an idiomatic public helper
- treat lowercase `array`/`dict` as compatibility aliases

### Generic Operations

These work across collection types:

| Operation | Lists | Arrays | Dicts | Strings |
|-----------|-------|--------|-------|---------|
| `ref` | by index | by index | by key | char at index |
| `length` | count | count | count | char count |
| `push!` | — | append | — | — |

### Dot-Bracket Index Access

```lisp
lst.[0]             ;; first element of list
arr.[2]             ;; third element of array
dict.['key]         ;; dict key lookup
str.[0]             ;; character at index
matrix.[i].[j]      ;; chained indexing
arr.[-1]            ;; last element (negative indexing)
```

---

## 10. String Operations

```lisp
(string-append "hello" " " "world")     ;; => "hello world"
(string-join ", " '("a" "b" "c"))       ;; => "a, b, c"
(substring "hello" 1 3)                 ;; => "el"
(substring "hello" -3 -1)              ;; => "ll" (negative indices)
(string-split "a,b,c" ",")             ;; => ("a" "b" "c")
(string-length "hello")                 ;; => 5
(string->list "abc")                    ;; => ("a" "b" "c")
(list->string '("a" "b" "c"))          ;; => "abc"
(string-upcase "hello")                 ;; => "HELLO"
(string-downcase "HELLO")               ;; => "hello"
(string-trim "  hello  ")               ;; => "hello"
(string-contains? "hello world" "world") ;; => true
(string-index-of "hello" "ll")          ;; => 2
(string-replace "hello" "l" "r")        ;; => "herro"
(char-at "hello" 0)                     ;; => "h"
(string-repeat "ha" 3)                  ;; => "hahaha"
```

### Conversion

```lisp
(string->number "42")       ;; => 42
(string->number "3.14")     ;; => 3.14
(number->string 42)         ;; => "42"
(string->symbol "foo")      ;; => foo (symbol)
(symbol->string 'foo)       ;; => "foo"
```

### Formatting

```lisp
;; printf-style
(format "Hello %s, you are %d" "Alice" 30)
;; => "Hello Alice, you are 30"

;; CL-style
(cl-format "~a is ~a" "pi" 3.14)
;; => "pi is 3.14"
```

---

## 11. Math & Numeric

### Arithmetic

| Op | Description | Example |
|----|-------------|---------|
| `+` | Addition | `(+ 1 2)` => `3` |
| `-` | Subtraction / negate | `(- 5 3)` => `2`, `(- 5)` => `-5` |
| `*` | Multiplication | `(* 3 4)` => `12` |
| `/` | Division | `(/ 10 3)` => `3` (integer), `(/ 10.0 3)` => `3.33..` |
| `%` | Modulo | `(% 10 3)` => `1` |

### Math Library

```lisp
(sin 1.0)   (cos 1.0)   (tan 1.0)       ;; trig
(asin 0.5)  (acos 0.5)  (atan 1.0)      ;; inverse trig
(atan2 1.0 1.0)                           ;; two-arg arctangent
(exp 1.0)   (log 2.718) (log10 100.0)    ;; exponential/log
(pow 2.0 10.0)  (sqrt 4.0)               ;; power/root
(floor 3.7) (ceiling 3.2) (round 3.5) (truncate 3.9) ;; rounding
(abs -42)   (min 3 7)   (max 3 7)        ;; misc
(gcd 12 8)  (lcm 4 6)                    ;; number theory
```

### Bitwise

```lisp
(bitwise-and 0xFF 0x0F)    ;; => 15
(bitwise-or 0xF0 0x0F)     ;; => 255
(bitwise-xor 0xFF 0x0F)    ;; => 240
(bitwise-not 0)             ;; => -1
(lshift 1 10)               ;; => 1024
(rshift 1024 5)             ;; => 32
```

### Numeric Predicates

```lisp
(zero? 0)        ;; => true
(positive? 5)    ;; => true
(negative? -1)   ;; => true
(even? 4)        ;; => true
(odd? 3)         ;; => true
```

### Constants

```lisp
pi   ;; => 3.141592653589793
e    ;; => 2.718281828459045
```
