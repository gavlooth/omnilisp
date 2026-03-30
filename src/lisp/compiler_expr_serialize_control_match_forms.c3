module lisp;

import std::collections::list;
import main;

// =============================================================================
// SECTION 4c: EXPRESSION SERIALIZER (control + match forms)
// =============================================================================
fn void Compiler.serialize_handle_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    self.buf_append(buf, "(handle ");
    self.serialize_expr_to_buf(expr.handle.body, buf);
    for (usz i = 0; i < expr.handle.clause_count; i++) {
        self.buf_append(buf, " (");
        if (expr.handle.clauses[i].has_effect_tag_expr) {
            self.serialize_expr_to_buf(expr.handle.clauses[i].effect_tag_expr, buf);
        } else {
            char[] etag = self.interp.symbols.get_name(expr.handle.clauses[i].effect_tag);
            self.buf_append(buf, etag);
        }
        buf.push(' ');
        char[] ea = self.interp.symbols.get_name(expr.handle.clauses[i].arg_name);
        self.buf_append(buf, ea);
        buf.push(' ');
        self.serialize_expr_to_buf(expr.handle.clauses[i].handler_body, buf);
        buf.push(')');
    }
    buf.push(')');
}

fn void Compiler.serialize_match_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    self.buf_append(buf, "(match ");
    self.serialize_expr_to_buf(expr.match.scrutinee, buf);
    for (usz i = 0; i < expr.match.clause_count; i++) {
        self.buf_append(buf, " (");
        self.serialize_pattern_to_buf(expr.match.clauses[i].pattern, buf);
        buf.push(' ');
        self.serialize_expr_to_buf(expr.match.clauses[i].result, buf);
        buf.push(')');
    }
    buf.push(')');
}
