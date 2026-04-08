module lisp;

import std::collections::list;
import main;

fn usz compiler_source_line_offset() @inline {
    usz lines = 1;
    foreach (c : STDLIB_PRELUDE) {
        if (c == '\n') lines++;
    }
    return lines;
}

fn bool Compiler.parse_program_exprs(Compiler* self, char[] full, List{Expr*}* exprs) {
    Lexer lex;
    lex.init(full);
    defer lex.destroy();

    if (lex.current.type == T_ERROR && lex.current.text == null && lex.current.text_cap == 0) {
        self.set_compile_error_formatted(
            "compiler syntax error",
            1,
            1,
            "out of memory while initializing lexer token buffer"
        );
        return false;
    }

    Parser p;
    p.init(&lex, self.interp);

    while (!lex.at_end() && !p.has_error) {
        Expr* e = p.parse_expr();
        if (e != null && !p.has_error) {
            exprs.push(e);
        }
    }

    if (p.has_error) {
        usz source_line = p.error_line;
        usz line_offset = compiler_source_line_offset();
        if (source_line > line_offset) {
            source_line -= line_offset;
        } else {
            source_line = 1;
        }
        self.set_compile_error_formatted("compiler syntax error", source_line, p.error_col, p.error_msg[:p.error_msg_len]);
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
