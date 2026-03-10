module lisp;

import std::collections::list;
import main;
// =============================================================================
// SECTION 4c: EXPRESSION SERIALIZER (definition/module forms)
// =============================================================================
fn void Compiler.serialize_deftype_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    self.buf_append(buf, "(define [type] ");
    ExprDefType* dt = expr.deftype;

    if (dt.type_param_count > 0 || dt.has_parent) {
        buf.push('(');
        char[] tname = self.interp.symbols.get_name(dt.name);
        self.buf_append(buf, tname);
        if (dt.has_parent) {
            buf.push(' ');
            char[] pname = self.interp.symbols.get_name(dt.parent);
            self.buf_append(buf, pname);
        }
        for (usz i = 0; i < dt.type_param_count; i++) {
            buf.push(' ');
            char[] p = self.interp.symbols.get_name(dt.type_params[i]);
            self.buf_append(buf, p);
        }
        buf.push(')');
    } else {
        char[] tname = self.interp.symbols.get_name(dt.name);
        self.buf_append(buf, tname);
    }

    for (usz i = 0; i < dt.field_count; i++) {
        buf.push(' ');
        if (dt.fields[i].type_ann.has_annotation) {
            buf.push('(');
            self.serialize_type_annotation_to_buf(&dt.fields[i].type_ann, buf);
            buf.push(' ');
            char[] fname = self.interp.symbols.get_name(dt.fields[i].name);
            self.buf_append(buf, fname);
            buf.push(')');
        } else {
            char[] fname = self.interp.symbols.get_name(dt.fields[i].name);
            self.buf_append(buf, fname);
        }
    }
    buf.push(')');
}

fn void Compiler.serialize_defabstract_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    self.buf_append(buf, "(define [abstract] ");
    if (expr.defabstract.has_parent) {
        buf.push('(');
        char[] n = self.interp.symbols.get_name(expr.defabstract.name);
        self.buf_append(buf, n);
        buf.push(' ');
        char[] p = self.interp.symbols.get_name(expr.defabstract.parent);
        self.buf_append(buf, p);
        buf.push(')');
    } else {
        char[] n = self.interp.symbols.get_name(expr.defabstract.name);
        self.buf_append(buf, n);
    }
    buf.push(')');
}

fn void Compiler.serialize_defunion_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    self.buf_append(buf, "(define [union] ");
    ExprDefUnion* du = expr.defunion;
    if (du.type_param_count > 0) {
        buf.push('(');
        char[] uname = self.interp.symbols.get_name(du.name);
        self.buf_append(buf, uname);
        for (usz i = 0; i < du.type_param_count; i++) {
            buf.push(' ');
            char[] p = self.interp.symbols.get_name(du.type_params[i]);
            self.buf_append(buf, p);
        }
        buf.push(')');
    } else {
        char[] uname = self.interp.symbols.get_name(du.name);
        self.buf_append(buf, uname);
    }

    for (usz i = 0; i < du.variant_count; i++) {
        buf.push(' ');
        UnionVariant* variant = &du.variants[i];
        if (variant.field_count == 0) {
            char[] vname = self.interp.symbols.get_name(variant.name);
            self.buf_append(buf, vname);
            continue;
        }
        buf.push('(');
        char[] vname = self.interp.symbols.get_name(variant.name);
        self.buf_append(buf, vname);
        for (usz fi = 0; fi < variant.field_count; fi++) {
            buf.push(' ');
            char[] fname = self.interp.symbols.get_name(variant.fields[fi]);
            self.buf_append(buf, fname);
        }
        buf.push(')');
    }
    buf.push(')');
}

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
