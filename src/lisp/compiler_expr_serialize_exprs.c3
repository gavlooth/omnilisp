module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 4c: EXPRESSION SERIALIZER (Expr forms)
// =============================================================================
fn void Compiler.serialize_lambda_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    self.buf_append(buf, "(lambda (");
    // Check for zero-arg lambda
    if ((uint)expr.lambda.param == 0xFFFFFFFF) {
        // zero-arg
    } else {
        char[] pname = self.interp.symbols.get_name(expr.lambda.param);
        self.buf_append(buf, pname);
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
    buf.push('(');
    self.serialize_expr_to_buf(expr.call.func, buf);
    for (usz i = 0; i < expr.call.arg_count; i++) {
        buf.push(' ');
        self.serialize_expr_to_buf(expr.call.args[i], buf);
    }
    buf.push(')');
}

fn void Compiler.serialize_handle_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    self.buf_append(buf, "(handle ");
    self.serialize_expr_to_buf(expr.handle.body, buf);
    for (usz i = 0; i < expr.handle.clause_count; i++) {
        self.buf_append(buf, " ((");
        char[] etag = self.interp.symbols.get_name(expr.handle.clauses[i].effect_tag);
        self.buf_append(buf, etag);
        buf.push(' ');
        char[] ek = self.interp.symbols.get_name(expr.handle.clauses[i].k_name);
        self.buf_append(buf, ek);
        buf.push(' ');
        char[] ea = self.interp.symbols.get_name(expr.handle.clauses[i].arg_name);
        self.buf_append(buf, ea);
        self.buf_append(buf, ") ");
        self.serialize_expr_to_buf(expr.handle.clauses[i].handler_body, buf);
        buf.push(')');
    }
    buf.push(')');
}

fn void Compiler.serialize_match_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    self.buf_append(buf, "(match ");
    self.serialize_expr_to_buf(expr.match.scrutinee, buf);
    for (usz i = 0; i < expr.match.clause_count; i++) {
        buf.push(' ');
        self.serialize_pattern_to_buf(expr.match.clauses[i].pattern, buf);
        buf.push(' ');
        self.serialize_expr_to_buf(expr.match.clauses[i].result, buf);
    }
    buf.push(')');
}

fn void Compiler.serialize_defmacro_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    self.buf_append(buf, "(define [macro] ");
    char[] macro_name = self.interp.symbols.get_name(expr.define_macro.name);
    self.buf_append(buf, macro_name);
    for (usz i = 0; i < expr.define_macro.clause_count; i++) {
        self.buf_append(buf, " (");
        self.serialize_pattern_to_buf(expr.define_macro.clauses[i].pattern, buf);
        buf.push(' ');
        self.buf_append(buf, "(");
        self.serialize_value_to_buf(expr.define_macro.clauses[i].tmpl, buf);
        self.buf_append(buf, "))");
    }
    buf.push(')');
}

fn void Compiler.serialize_module_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    self.buf_append(buf, "(module ");
    char[] mod_name = self.interp.symbols.get_name(expr.module_expr.name);
    self.buf_append(buf, mod_name);
    self.buf_append(buf, " (export");
    for (usz i = 0; i < expr.module_expr.export_count; i++) {
        buf.push(' ');
        char[] exp_name = self.interp.symbols.get_name(expr.module_expr.exports[i]);
        self.buf_append(buf, exp_name);
    }
    buf.push(')');
    for (usz i = 0; i < expr.module_expr.body_count; i++) {
        buf.push(' ');
        self.serialize_expr_to_buf(expr.module_expr.body[i], buf);
    }
    buf.push(')');
}

fn void Compiler.serialize_import_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    self.buf_append(buf, "(import ");
    char[] imp_name = self.interp.symbols.get_name(expr.import_expr.name);
    self.buf_append(buf, imp_name);
    if (expr.import_expr.import_all) {
        self.buf_append(buf, " 'all");
    } else if (expr.import_expr.import_count > 0) {
        self.buf_append(buf, " (");
        for (usz i = 0; i < expr.import_expr.import_count; i++) {
            if (i > 0) buf.push(' ');
            char[] sel_name = self.interp.symbols.get_name(expr.import_expr.imports[i]);
            self.buf_append(buf, sel_name);
            if ((uint)expr.import_expr.aliases[i] != 0) {
                self.buf_append(buf, " 'as ");
                char[] alias_name = self.interp.symbols.get_name(expr.import_expr.aliases[i]);
                self.buf_append(buf, alias_name);
            }
        }
        buf.push(')');
    }
    buf.push(')');
}

fn void Compiler.serialize_export_from_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    self.buf_append(buf, "(export-from ");
    char[] efmod = self.interp.symbols.get_name(expr.export_from.source_module);
    self.buf_append(buf, efmod);
    if (expr.export_from.all) {
        self.buf_append(buf, " 'all)");
    } else {
        self.buf_append(buf, " (");
        for (usz i = 0; i < expr.export_from.name_count; i++) {
            if (i > 0) buf.push(' ');
            char[] ef_name = self.interp.symbols.get_name(expr.export_from.names[i]);
            self.buf_append(buf, ef_name);
        }
        self.buf_append(buf, "))");
    }
}

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
    self.buf_append(buf, "(begin");
    for (usz i = 0; i < expr.begin.expr_count; i++) {
        buf.push(' ');
        self.serialize_expr_to_buf(expr.begin.exprs[i], buf);
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
        case E_BEGIN:
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
        default:
            return false;
    }
}

fn bool Compiler.serialize_expr_specialized(Compiler* self, Expr* expr, List{char}* buf) {
    if (self.serialize_expr_specialized_core(expr, buf)) return true;
    return self.serialize_expr_specialized_reader(expr, buf);
}

fn void Compiler.serialize_expr_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    if (expr == null) {
        self.buf_append(buf, "nil");
        return;
    }

    switch (expr.tag) {
        case E_LIT:
            self.serialize_value_to_buf(expr.lit.value, buf);

        case E_VAR:
            char[] name = self.interp.symbols.get_name(expr.var_expr.name);
            self.buf_append(buf, name);

        case E_LAMBDA:
            self.serialize_lambda_to_buf(expr, buf);

        case E_LET:
            self.serialize_let_to_buf(expr, buf);

        case E_HANDLE:
            self.serialize_handle_to_buf(expr, buf);

        case E_CALL:
            self.serialize_call_to_buf(expr, buf);

        case E_MATCH:
            self.serialize_match_to_buf(expr, buf);

        case E_DEFMACRO:
            self.serialize_defmacro_to_buf(expr, buf);

        case E_MODULE:
            self.serialize_module_to_buf(expr, buf);

        case E_IMPORT:
            self.serialize_import_to_buf(expr, buf);

        case E_EXPORT_FROM:
            self.serialize_export_from_to_buf(expr, buf);

        default:
            if (self.serialize_expr_specialized(expr, buf)) return;
            self.buf_append(buf, "nil");
    }
}
