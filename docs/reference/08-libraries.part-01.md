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
(re-replace "[0-9]+" "NUM" "abc123def456")
;; => "abcNUMdef456"

;; Replace all matches with 'global
(re-replace "[0-9]+" "NUM" "abc123def456" 'global)
;; => "abcNUMdefNUM"

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
(define expr-grammar
  (pika/grammar 'expr
    '(rule expr (seq number op number))
    '(rule number (scan "[0-9]+"))
    '(rule op (first "+" "-" "*" "/"))))
```

Grammar definitions use a grammar name followed by quoted `(rule name clause)`
forms. The first declared rule is the grammar start rule; later rules may be
referenced before they are declared.

### Parse

```lisp
(pika/parse expr-grammar "1+2")
;; => parse tree rooted at expr
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
