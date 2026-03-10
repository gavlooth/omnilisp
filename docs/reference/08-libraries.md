# Regex, PEG Grammar, Compression, Unicode & Deduce

**[Back to Index](../OMNI_REFERENCE.md)**

---

## 24. Regex

Omni uses the Pika regex engine (not PCRE). It compiles PCRE-like syntax to
Pika grammars internally and provides linear/polynomial time guarantees — no
catastrophic backtracking.

```lisp
;; Match — returns first match or nil
(re-match "[0-9]+" "abc123def")
;; => "123"

;; Full match — must match entire string
(re-fullmatch "[0-9]+" "123")     ;; => "123"
(re-fullmatch "[0-9]+" "abc123")  ;; => nil

;; Find all matches
(re-find-all "[0-9]+" "a1b22c333")
;; => ("1" "22" "333")

;; Split by pattern
(re-split "\\s+" "hello   world   foo")
;; => ("hello" "world" "foo")

;; Replace
(re-replace "[0-9]+" "abc123def456" "NUM")
;; => "abcNUMdefNUM"

;; With limit
(re-replace "[0-9]+" "a1b2c3" "X" 2)
;; => "aXbXc3"

;; Position-returning variants
(re-match-pos "[0-9]+" "abc123")
;; => (3 . 6)  — (start . end)

(re-find-all-pos "[0-9]+" "a1b22")
;; => ((1 . 2) (3 . 5))
```

### Supported Syntax

Character classes (`[a-z]`, `\d`, `\w`, `\s`), quantifiers (`*`, `+`, `?`,
`{n,m}`), alternation (`|`), groups (`(...)`), anchors (`^`, `$`),
lookahead (`(?=...)`, `(?!...)`).

**Not supported**: backreferences (`\1`) — these make matching NP-complete.
Use PEG grammar or Omni code for context-sensitive matching.

---

## 25. PEG Grammar

The Pika grammar engine handles recursive parsing, left recursion, and
structural transformations — anything beyond what regex can do.

### Define a Grammar

```lisp
(define json-grammar
  (pika/grammar
    "value"  "object | array | string | number | 'true' | 'false' | 'null'"
    "object" "'{' (pair (',' pair)*)? '}'"
    "pair"   "string ':' value"
    "array"  "'[' (value (',' value)*)? ']'"
    "string" "'\"' [^\"]* '\"'"
    "number" "[0-9]+"))
```

### Parse

```lisp
(pika/parse json-grammar "[1, 2, 3]")
;; => parse tree
```

### Fold (Transform)

```lisp
(pika/fold grammar input fold-fn)
```

### Additional Operations

```lisp
(pika/grammar-rules grammar)              ;; list rule names
(pika/match-span grammar input rule-name)  ;; match specific rule
```

---

## 26. Compression

```lisp
;; Gzip
(define compressed (gzip "hello world"))
(gunzip compressed)   ;; => "hello world"
;; Optional level: 0..12 (default 6)
(gunzip (gzip "hello world" 1))

;; Raw deflate
(define deflated (deflate "hello world"))
(inflate deflated)     ;; => "hello world"
(inflate (deflate "hello world" 12))

;; Zlib wrapper format
(define z (zlib-compress "hello world"))
(zlib-decompress z)    ;; => "hello world"

;; Checksums
(adler32 "hello world") ;; => 436929629
(crc32 "hello world")   ;; => 222957957
```

---

## 27. Unicode

```lisp
;; Case conversion (Unicode-aware via utf8proc)
(string-upcase "hello")           ;; => "HELLO"
(string-downcase "HELLO")         ;; => "hello"
(string-casefold "Straße")        ;; => "strasse"
(string-titlecase "hELLO wORLD")  ;; => "Hello World"

;; Normalization
(string-normalize "cafe\u0301" 'NFC)   ;; => "cafe" (composed)
(string-normalize "cafe" 'NFD)          ;; => decomposes accents
;; Forms: NFC, NFD, NFKC, NFKD

;; Grapheme clusters
(string-graphemes "hello")   ;; => ("h" "e" "l" "l" "o")

;; Codepoints
(string-codepoints "abc")    ;; => (97 98 99)

;; Character category
(char-category "A")    ;; => "Lu" (uppercase letter)
(char-category "a")    ;; => "Ll" (lowercase letter)
(char-category "1")    ;; => "Nd" (decimal digit)

;; Display width hint
(char-width 65)        ;; => 1

;; Property lookup
(char-property 65 'category)  ;; => "Lu"
(char-property 48 'digit?)    ;; => true
```

---

## 28. Deduce (Database)

Deduce is an embedded relational database backed by LMDB.

### Open a Database

```lisp
(define db (deduce 'open "app.db"))       ;; persistent
(define tmp (deduce 'open 'memory))        ;; ephemeral

;; explicit LMDB named DB exposure for relation storage
;; (relation symbol remains language-facing; db-name selects LMDB dbi name)
(define events (deduce 'open-named db 'event-log "events_v1" 'ts 'kind 'payload))
```

### Define Relations

```lisp
(define [relation db] person
  (^String name) (^Int age) (^String email))

(define [relation db] edge (from to))
```

### Assert and Retract Facts

```lisp
(deduce 'fact! person "Alice" 30 "alice@b.com")
(deduce 'fact! person "Bob" 25 "bob@b.com")
(deduce 'fact! edge "A" "B")

(deduce 'retract! person "Alice" 30 "alice@b.com")
```

### Transactions

```lisp
;; explicit write transaction
(define tx (deduce 'block db))
(deduce 'fact! tx person "Carol" 44 "carol@b.com")
(deduce 'retract! tx edge "A" "B")
(deduce 'commit tx)

;; explicit read transaction (for future read APIs that accept txn handles)
(define ro (deduce 'block db 'read))
(deduce 'abort ro)
```

### Relation Maintenance

```lisp
;; remove all rows, keep relation schema/handle valid
(deduce 'clear! person)

;; drop relation backing store; existing relation handle becomes invalid for scans/writes
(deduce 'drop! person)
```

### Query

```lisp
(deduce 'scan person)           ;; all rows
(deduce 'count person)          ;; number of rows
(deduce 'query person
  (lambda (row) (> (ref row 'age) 28)))   ;; filtered
(deduce 'match person '("Alice" _ _))     ;; pattern match
(deduce 'scan-range person '("Alice" 0 "a@x") '("Charles" 999 "zzz")) ;; bounded scan
```
