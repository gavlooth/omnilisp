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
