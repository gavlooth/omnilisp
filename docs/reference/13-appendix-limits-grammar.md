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
            | list | array_lit | dict_lit | indexed | accessor ;

literal     = integer | float | string ;
integer     = [ "-" ] digit { digit } ;
float       = [ "-" ] digit { digit } "." digit { digit } ;
string      = '"' { char | escape } '"' ;
symbol      = symbol_char { symbol_char } ;
path        = symbol "." symbol { "." symbol } ;

quoted      = "'" datum ;
quasiquoted = "`" datum ;
list        = "(" { expr } ")" ;
array_lit   = "[" { expr } "]" ;           (* equivalent to Array constructor call *)
dict_lit    = "{" { expr expr } "}" ;      (* equivalent to Dictionary constructor call; must be even *)
indexed     = expr ".[" expr "]" ;
accessor    = "." expr

datum       = literal | symbol | "(" { datum } ")" | "'" datum ;

symbol_char = letter | digit | "_" | "-" | "+" | "*" | "/"
            | "=" | "<" | ">" | "!" | "?" | ":" | "@" | "#"
            | "$" | "%" | "&" | "|" | "^" | "~" ;
```

---

*Omni Lisp -- A Lisp with delimited continuations, algebraic effects,
strict-arity lambdas, multiple dispatch, and structural types.*
