module lisp;

import std::io;
import std::collections::list;
import main;

fn bool Compiler.parse_program_exprs(Compiler* self, char[] full, List{Expr*}* exprs) {
    Lexer lex;
    lex.init(full);
    defer lex.destroy();
    Parser p;
    p.init(&lex, self.interp);

    while (!lex.at_end() && !p.has_error) {
        Expr* e = p.parse_expr();
        if (e != null) {
            exprs.push(e);
        }
    }

    if (p.has_error) {
        io::printfn("Syntax Error at line %d, column %d: %s", (int)p.error_line, (int)p.error_col, (ZString)&p.error_msg[0]);
        return false;
    }
    return true;
}

fn void Compiler.prepare_program_analysis(Compiler* self, List{Expr*}* exprs) {
    self.collect_top_level_defines(exprs);
    foreach (expr : *exprs) {
        self.prescan_mutable_captures(expr);
    }
    foreach (expr : *exprs) {
        self.scan_lambdas(expr);
    }
}
