module lisp;

import std::collections::list;
import main;

fn void Compiler.serialize_defmacro_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    self.buf_append(buf, "(define [macro] ");
    char[] macro_name = self.interp.symbols.get_name(expr.define_macro.name);
    self.buf_append(buf, macro_name);
    self.buf_append(buf, " (syntax-match");
    for (usz i = 0; i < expr.define_macro.clause_count; i++) {
        self.buf_append(buf, " (");
        self.serialize_pattern_to_buf(expr.define_macro.clauses[i].pattern, buf);
        buf.push(' ');
        self.buf_append(buf, "(template ");
        self.serialize_value_to_buf(expr.define_macro.clauses[i].tmpl, buf);
        self.buf_append(buf, "))");
    }
    self.buf_append(buf, "))");
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
    if (expr.import_expr.has_path && expr.import_expr.path != null) {
        self.serialize_quoted_text_to_buf(expr.import_expr.path[:expr.import_expr.path_len], buf);
    } else {
        char[] imp_name = self.interp.symbols.get_name(expr.import_expr.name);
        self.buf_append(buf, imp_name);
    }
    if (expr.import_expr.import_all) {
        self.buf_append(buf, " 'all");
    } else if (expr.import_expr.import_count > 0) {
        self.buf_append(buf, " (");
        for (usz i = 0; i < expr.import_expr.import_count; i++) {
            if (i > 0) buf.push(' ');
            char[] sel_name = self.interp.symbols.get_name(expr.import_expr.imports[i]);
            if ((uint)expr.import_expr.aliases[i] != 0) {
                buf.push('(');
                self.buf_append(buf, sel_name);
                self.buf_append(buf, " 'as ");
                char[] alias_name = self.interp.symbols.get_name(expr.import_expr.aliases[i]);
                self.buf_append(buf, alias_name);
                buf.push(')');
            } else {
                self.buf_append(buf, sel_name);
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
