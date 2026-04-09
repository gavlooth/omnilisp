module lisp;


import std::io;
import std::collections::list;
// =============================================================================
// SECTION 3: TOP-LEVEL PARSE
// =============================================================================

/**
 * Parse a string into an expression.
 */
fn Expr* parse_single_expr(Parser* p) {
    Expr* expr = p.parse_expr();
    if (p.has_error || expr == null) return null;
    if (p.lexer.current.type != T_EOF) {
        p.set_error("unexpected trailing tokens after expression");
        return null;
    }
    return expr;
}

fn Expr* parse(char[] source, Interp* interp) {
    Lexer lex;
    lex.init(source);
    defer lex.destroy();

    Parser p;
    p.init(&lex, interp);

    return parse_single_expr(&p);
}

/**
 * Parse multiple expressions (a program).
 */
fn List{Expr*} parse_program(char[] source, Interp* interp) {
    Lexer lex;
    lex.init(source);
    defer lex.destroy();

    Parser p;
    p.init(&lex, interp);

    List{Expr*} exprs;
    while (!lex.at_end() && !p.has_error) {
        Expr* e = p.parse_expr();
        if (e != null && !p.has_error) {
            exprs.push(e);
        }
    }

    return exprs;
}
