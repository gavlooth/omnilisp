# Grammar DSL Documentation

## Overview

The OmniLisp Grammar DSL allows you to define Pika grammars directly in OmniLisp code. This provides full access to Pika's pattern matching capabilities, including left recursion and all PEG combinators.

## Syntax

### Defining a Grammar

```lisp
(define [grammar name]
  [rule-name clause]
  [rule-name2 clause2]
  ...)
```

Each rule is defined as `[rule-name clause]` where `rule-name` is a symbol and `clause` is a Pika clause expression.

### Clause Forms

The following clause forms are supported:

#### Literals
- `"string"` - Match literal string
- `(char #\x)` - Match single character

#### Combinators
- `(seq a b c)` - Sequence (match a, then b, then c)
- `(first a b c)` - Ordered choice (match a, or b, or c)
- `(ref rule-name)` - Reference another rule

#### Quantifiers
- `(zero-or-more x)` or `(* x)` - Match x zero or more times
- `(one-or-more x)` or `(+ x)` - Match x one or more times
- `(optional x)` or `(? x)` - Match x zero or one time

#### Character Classes
- `(charset "a-z")` - Match any character in range/pattern
- `(charset-not "0-9")` - Match any character NOT in range/pattern
- `(any)` - Match any character except NULL

#### Lookahead
- `(followed-by x)` or `(& x)` - Positive lookahead (match if x follows)
- `(not-followed-by x)` or `(! x)` - Negative lookahead (match if x doesn't follow)

#### Labels
- `(label :name x)` - Assign AST label to clause

## Runtime API

### pika-match

Match input against a grammar rule.

```lisp
(pika-match grammar rule-name input-string)
```

Returns the matched string, or nil if no match.

**Example:**
```lisp
(define [grammar simple]
  [greeting "hello"])

(pika-match simple greeting "helloworld")  ; => "hello"
```

### pika-find-all

Find all non-overlapping matches of a grammar rule.

```lisp
(pika-find-all grammar rule-name input-string)
```

Returns a list of matched strings, or nil if no matches.

**Example:**
```lisp
(define [grammar numbers]
  [number (one-or-more (charset "0-9"))])

(pika-find-all numbers number "abc123def456")  ; => ("123" "456")
```

## Left Recursion

Pika supports left recursion, which allows natural grammar definitions:

```lisp
(define [grammar arithmetic]
  [expr (first (seq (ref expr) "+" (ref term))
               (ref term))]
  [term (first (seq (ref term) "*" (ref factor))
               (ref factor))]
  [factor (first (one-or-more (charset "0-9")))])

(pika-match arithmetic expr "1+2+3")  ; => "1+2+3"
```

## Clause Reference

| Clause | Description | Example |
|--------|-------------|---------|
| `"str"` | Match literal string | `"hello"` |
| `(char c)` | Match single character | `(char #\a)` |
| `(seq a b)` | Sequence | `(seq "a" "b")` |
| `(first a b)` | Choice | `(first "x" "y")` |
| `(ref rule)` | Rule reference | `(ref expr)` |
| `(* x)` | Zero or more | `(* (charset "0-9"))` |
| `(+ x)` | One or more | `(+ (charset "a-z"))` |
| `(? x)` | Optional | `(? "opt")` |
| `(charset pat)` | Character class | `(charset "a-z")` |
| `(charset-not pat)` | Negated class | `(charset-not "0-9")` |
| `(& x)` | Positive lookahead | `(& (charset "0-9"))` |
| `(! x)` | Negative lookahead | `(! (charset "a-z"))` |
| `(label :name x)` | AST label | `(label :val (ref expr))` |

## Implementation Notes

- Grammars are compiled at definition time to PikaGrammar* structures
- Grammar values are stored as T_GRAMMAR type
- The compiler uses the Pika C API directly
- All memory is managed by ASAP (compile-time static memory management)

## Known Limitations

- Keywords: OmniLisp doesn't natively support keyword symbols starting with `:`. Rule names are passed as regular symbols, not keywords.

## Future Enhancements

- Add support for extracting labeled AST nodes from matches
- Add support for grammar composition and modular grammars
- Add support for parameterized grammars
- Improve error messages for grammar compilation failures
