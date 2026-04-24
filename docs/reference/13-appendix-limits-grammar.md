# Appendix C: Limits & Appendix D: EBNF Grammar

**[Back to Index](../OMNI_REFERENCE.md)**

---

## Appendix C: Limits

| Resource | Limit |
|----------|-------|
| Symbol name length | Dynamic (heap-allocated) |
| Total symbols | 8,192 |
| Bindings per env frame | 512 |
| Match clauses | Dynamic (no fixed limit) |
| Pattern elements | Dynamic (no fixed limit) |
| Effect handler clauses | Dynamic (no fixed limit) |
| Handler stack depth | 16 |
| Call arguments | Dynamic AST (JIT: up to 16 native) |
| Path segments | 8 |
| Block expressions | Dynamic (no fixed limit) |
| Lambda params | Dynamic (no fixed limit) |
| String literal (inline) | 63 bytes (lexer limit) |
| String value | Dynamic (no limit) |
| Macros | 64 |
| Macro transformer branches | Dynamic (inside `syntax-match`) |
| Modules | 32 |
| Module exports | 128 per module |
| Eval depth | 5,000 |
| Registered types | 256 |
| Type fields | 16 |
| Method table entries | 64 |

---

## Appendix D: EBNF Grammar

```ebnf
program     = { expr } ;
expr        = literal | symbol | path | quoted | quasiquoted
            | list | array_lit | dict_lit | indexed | accessor | reader_tag ;

literal     = integer | radix_integer | float | string ;
integer     = [ "-" ] digit { digit } ;
radix_integer = ( "#x" | "#b" | "#o" ) [ "-" ] radix_digit { radix_digit } ;
(* hex radix alpha digits are uppercase A through F; lowercase symbol-like
   forms such as #xface, #b101tag, and #o777tag are reader tags. *)
float       = [ "-" ] digit { digit } "." digit { digit } ;
string      = '"' { char | escape } '"' ;
symbol      = symbol_char { symbol_char } ;
path        = symbol "." symbol { "." symbol } ;

quoted      = "'" datum ;
quasiquoted = "`" datum ;
list        = "(" { expr } ")" ;
array_lit   = "[" { expr } "]" ;           (* equivalent to Array constructor call *)
dict_lit    = "{" { dict_key expr } "}" ;  (* equivalent to Dictionary constructor call; must be even *)
dict_key    = symbol | expr ;              (* bare symbol keys auto-quote *)
indexed     = expr ".[" expr "]" ;
postfix_field = expr "." symbol ;          (* after an already parsed expression, for example row.[0].name *)
accessor    = "." expr ;                   (* removed leading-dot form; must hard-error *)
reader_tag  = "#" symbol expr ;            (* #tag form parses as (tag form) *)

datum       = literal | symbol | "(" { datum } ")" | "'" datum ;

symbol_char = letter | digit | "_" | "-" | "+" | "*" | "/"
            | "=" | "<" | ">" | "!" | "?" | ":" | "@" | "#"
            | "$" | "%" | "&" | "|" | "^" | "~" ;
```

---

*Omni Lisp -- A Lisp with delimited continuations, algebraic effects,
strict-arity lambdas, multiple dispatch, and structural types.*
