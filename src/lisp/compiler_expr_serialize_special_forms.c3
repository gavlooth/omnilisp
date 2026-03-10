module lisp;

import std::collections::list;
import main;

fn void Compiler.serialize_unary_expr_form(Compiler* self, char[] head, Expr* body, List{char}* buf) {
    self.buf_append(buf, head);
    self.serialize_expr_to_buf(body, buf);
    buf.push(')');
}

fn void Compiler.serialize_binary_expr_form(Compiler* self, char[] head, Expr* left, Expr* right, List{char}* buf) {
    self.buf_append(buf, head);
    self.serialize_expr_to_buf(left, buf);
    buf.push(' ');
    self.serialize_expr_to_buf(right, buf);
    buf.push(')');
}

fn void Compiler.serialize_named_expr_form(Compiler* self, char[] head, SymbolId name, Expr* value, List{char}* buf) {
    self.buf_append(buf, head);
    char[] val_name = self.interp.symbols.get_name(name);
    self.buf_append(buf, val_name);
    buf.push(' ');
    self.serialize_expr_to_buf(value, buf);
    buf.push(')');
}

fn void Compiler.serialize_symbol_and_arg_form(Compiler* self, char[] head, SymbolId sym, Expr* arg, List{char}* buf) {
    self.buf_append(buf, head);
    char[] sym_name = self.interp.symbols.get_name(sym);
    self.buf_append(buf, sym_name);
    buf.push(' ');
    self.serialize_expr_to_buf(arg, buf);
    buf.push(')');
}

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

fn bool Compiler.serialize_expr_specialized_core(Compiler* self, Expr* expr, List{char}* buf) {
    switch (expr.tag) {
        case E_APP:
            self.serialize_binary_expr_form("(", expr.app.func, expr.app.arg, buf);
            return true;
        case E_IF:
            self.buf_append(buf, "(if ");
            self.serialize_expr_to_buf(expr.if_expr.test, buf);
            buf.push(' ');
            self.serialize_expr_to_buf(expr.if_expr.then_branch, buf);
            buf.push(' ');
            self.serialize_expr_to_buf(expr.if_expr.else_branch, buf);
            buf.push(')');
            return true;
        case E_DEFINE:
            self.serialize_named_expr_form("(define ", expr.define.name, expr.define.value, buf);
            return true;
        case E_QUOTE:
            buf.push('\'');
            self.serialize_value_to_buf(expr.quote.datum, buf);
            return true;
        case E_RESET:
            self.serialize_unary_expr_form("(reset ", expr.reset.body, buf);
            return true;
        case E_SHIFT:
            self.serialize_symbol_and_arg_form("(shift ", expr.shift.k_name, expr.shift.body, buf);
            return true;
        case E_PERFORM:
            self.serialize_symbol_and_arg_form("(perform ", expr.perform.tag, expr.perform.arg, buf);
            return true;
        case E_RESOLVE:
            self.serialize_unary_expr_form("(resolve ", expr.resolve.value, buf);
            return true;
        case E_AND:
            self.serialize_binary_expr_form("(and ", expr.and_expr.left, expr.and_expr.right, buf);
            return true;
        case E_OR:
            self.serialize_binary_expr_form("(or ", expr.or_expr.left, expr.or_expr.right, buf);
            return true;
        default:
            return false;
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

fn bool Compiler.serialize_expr_specialized(Compiler* self, Expr* expr, List{char}* buf) {
    if (self.serialize_expr_specialized_core(expr, buf)) return true;
    return self.serialize_expr_specialized_reader(expr, buf);
}
