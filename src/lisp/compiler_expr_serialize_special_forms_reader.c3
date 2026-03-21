module lisp;

import std::collections::list;
import main;

fn void Compiler.serialize_begin_expr_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    self.buf_append(buf, "(block");
    for (usz i = 0; i < expr.block.expr_count; i++) {
        buf.push(' ');
        self.serialize_expr_to_buf(expr.block.exprs[i], buf);
    }
    buf.push(')');
}

fn void Compiler.serialize_index_expr_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    self.serialize_expr_to_buf(expr.index.collection, buf);
    self.buf_append(buf, ".[");
    self.serialize_expr_to_buf(expr.index.index, buf);
    buf.push(']');
}

fn void Compiler.serialize_path_expr_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    for (usz i = 0; i < expr.path.segment_count; i++) {
        if (i > 0) buf.push('.');
        char[] seg = self.interp.symbols.get_name(expr.path.segments[i]);
        self.buf_append(buf, seg);
    }
}

fn bool Compiler.serialize_expr_specialized_reader(Compiler* self, Expr* expr, List{char}* buf) {
    switch (expr.tag) {
        case E_BLOCK:
            self.serialize_begin_expr_to_buf(expr, buf);
            return true;
        case E_SET:
            self.serialize_named_expr_form("(set! ", expr.set_expr.name, expr.set_expr.value, buf);
            return true;
        case E_INDEX:
            self.serialize_index_expr_to_buf(expr, buf);
            return true;
        case E_PATH:
            self.serialize_path_expr_to_buf(expr, buf);
            return true;
        case E_QUASIQUOTE:
            buf.push('`');
            self.serialize_expr_to_buf(expr.quasiquote.body, buf);
            return true;
        case E_UNQUOTE:
            buf.push(',');
            self.serialize_expr_to_buf(expr.unquote.body, buf);
            return true;
        case E_UNQUOTE_SPLICING:
            self.buf_append(buf, ",@");
            self.serialize_expr_to_buf(expr.unquote_splicing.body, buf);
            return true;
        case E_DEFTYPE:
            self.serialize_deftype_to_buf(expr, buf);
            return true;
        case E_DEFABSTRACT:
            self.serialize_defabstract_to_buf(expr, buf);
            return true;
        case E_DEFUNION:
            self.serialize_defunion_to_buf(expr, buf);
            return true;
        case E_DEFALIAS:
            self.serialize_defalias_to_buf(expr, buf);
            return true;
        case E_DEFEFFECT:
            self.serialize_defeffect_to_buf(expr, buf);
            return true;
        default:
            return false;
    }
}
