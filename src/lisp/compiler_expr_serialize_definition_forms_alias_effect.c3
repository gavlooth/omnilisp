module lisp;

import std::collections::list;
import main;

fn void Compiler.serialize_defalias_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    self.buf_append(buf, "(define [alias] ");
    char[] aname = self.interp.symbols.get_name(expr.defalias.name);
    self.buf_append(buf, aname);
    buf.push(' ');
    if (expr.defalias.target.has_annotation) {
        self.serialize_type_annotation_to_buf(&expr.defalias.target, buf);
    } else {
        self.buf_append(buf, "nil");
    }
    buf.push(')');
}

fn void Compiler.serialize_defeffect_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    self.buf_append(buf, "(define [effect] (");
    char[] ename = self.interp.symbols.get_name(expr.defeffect.name);
    self.buf_append(buf, ename);
    if (expr.defeffect.has_arg_type) {
        self.buf_append(buf, " (");
        self.serialize_type_annotation_to_buf(&expr.defeffect.arg_type, buf);
        self.buf_append(buf, " arg)");
    }
    self.buf_append(buf, "))");
}
