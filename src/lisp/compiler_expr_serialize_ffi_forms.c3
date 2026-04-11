module lisp;

import std::collections::list;
import main;

fn void Compiler.serialize_ffi_lib_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    self.buf_append(buf, "(define [ffi lib] ");
    char[] lib_name = self.interp.symbols.get_name(expr.ffi_lib.name);
    self.buf_append(buf, lib_name);
    buf.push(' ');
    self.serialize_expr_to_buf(expr.ffi_lib.path_expr, buf);
    buf.push(')');
}

fn void Compiler.serialize_ffi_fn_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    ExprFfiFn* ff = expr.ffi_fn;
    if (ff == null) {
        self.buf_append(buf, "nil");
        return;
    }

    self.buf_append(buf, "(define [ffi λ ");
    char[] lib_name = self.interp.symbols.get_name(ff.lib_name);
    self.buf_append(buf, lib_name);
    if (ff.is_variadic) self.buf_append(buf, " variadic");
    self.buf_append(buf, "] (");
    char[] fn_name = self.interp.symbols.get_name(ff.fn_name);
    self.buf_append(buf, fn_name);

    for (usz i = 0; i < ff.param_count; i++) {
        self.buf_append(buf, " (");
        self.serialize_type_annotation_to_buf(&ff.param_types[i], buf);
        buf.push(' ');
        char[] param_name = self.interp.symbols.get_name(ff.param_names[i]);
        self.buf_append(buf, param_name);
        buf.push(')');
    }

    buf.push(')');
    if (ff.has_return_type) {
        buf.push(' ');
        self.serialize_type_annotation_to_buf(&ff.return_type, buf);
    }
    buf.push(')');
}
