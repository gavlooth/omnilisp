module lisp;

import std::collections::list;
import main;
// =============================================================================
// SECTION 4c: EXPRESSION SERIALIZER (callable/control forms)
// =============================================================================
fn void Compiler.serialize_lambda_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    self.buf_append(buf, "(lambda (");
    usz fixed_count = expr.lambda.param_count;
    if (fixed_count == 0 && expr.lambda.param != INVALID_SYMBOL_ID) {
        fixed_count = 1;
    }

    for (usz i = 0; i < fixed_count; i++) {
        if (i > 0) buf.push(' ');
        SymbolId param = fixed_count == 1 && expr.lambda.param_count == 0
            ? expr.lambda.param
            : expr.lambda.params[i];

        bool has_typed = expr.lambda.has_typed_params &&
            expr.lambda.param_annotations != null &&
            expr.lambda.param_annotations[i].has_annotation;

        if (has_typed) {
            buf.push('(');
            self.serialize_type_annotation_to_buf(&expr.lambda.param_annotations[i], buf);
            buf.push(' ');
            char[] pname = self.interp.symbols.get_name(param);
            self.buf_append(buf, pname);
            buf.push(')');
        } else {
            char[] pname = self.interp.symbols.get_name(param);
            self.buf_append(buf, pname);
        }
    }

    if (expr.lambda.has_rest) {
        if (fixed_count > 0) buf.push(' ');
        self.buf_append(buf, ".. ");
        char[] rest_name = self.interp.symbols.get_name(expr.lambda.rest_param);
        self.buf_append(buf, rest_name);
    }
    self.buf_append(buf, ") ");
    self.serialize_expr_to_buf(expr.lambda.body, buf);
    buf.push(')');
}

fn void Compiler.serialize_let_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    if (expr.let_expr.is_recursive) {
        self.buf_append(buf, "(let ^rec (");
    } else {
        self.buf_append(buf, "(let (");
    }
    char[] let_name = self.interp.symbols.get_name(expr.let_expr.name);
    self.buf_append(buf, let_name);
    buf.push(' ');
    self.serialize_expr_to_buf(expr.let_expr.init, buf);
    self.buf_append(buf, ") ");
    self.serialize_expr_to_buf(expr.let_expr.body, buf);
    buf.push(')');
}

fn void Compiler.serialize_call_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    bool is_explain_thunked = false;
    if (expr.call.func != null && expr.call.func.tag == E_VAR && expr.call.arg_count == 2) {
        Expr* thunk = expr.call.args[1];
        if (thunk != null && thunk.tag == E_LAMBDA && thunk.lambda != null) {
            char[] fname = self.interp.symbols.get_name(expr.call.func.var_expr.name);
            is_explain_thunked = self.str_eq(fname, "explain")
                && thunk.lambda.param_count == 0
                && !thunk.lambda.has_rest;
        }
    }

    if (is_explain_thunked) {
        // parse_explain rewrites (explain 'selector <form>) to a thunked call.
        // Reconstruct canonical surface syntax so delegated eval preserves semantics.
        Expr* thunk = expr.call.args[1];
        self.buf_append(buf, "(explain ");
        self.serialize_expr_to_buf(expr.call.args[0], buf);
        buf.push(' ');
        self.serialize_expr_to_buf(thunk.lambda.body, buf);
        buf.push(')');
        return;
    }

    buf.push('(');
    self.serialize_expr_to_buf(expr.call.func, buf);
    for (usz i = 0; i < expr.call.arg_count; i++) {
        buf.push(' ');
        self.serialize_expr_to_buf(expr.call.args[i], buf);
    }
    buf.push(')');
}
