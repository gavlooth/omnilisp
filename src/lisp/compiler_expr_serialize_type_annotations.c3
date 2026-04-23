module lisp;

import std::collections::list;
import main;

fn void Compiler.serialize_quoted_text_to_buf(Compiler* self, char[] text, List{char}* buf) {
    buf.push('"');
foreach (item : text) {
        char c = item;
        if (c == '"') {
            buf.push('\\');
            buf.push('"');
        } else if (c == '\\') {
            buf.push('\\');
            buf.push('\\');
        } else if (c == '\n') {
            buf.push('\\');
            buf.push('n');
        } else if (c == '\t') {
            buf.push('\\');
            buf.push('t');
        } else {
            buf.push(c);
        }
}
    buf.push('"');
}

fn void Compiler.serialize_type_val_literal_to_buf(Compiler* self, TypeAnnotation* ann, List{char}* buf) {
    switch (ann.val_literal.tag) {
        case INT:
            self.serialize_int_value_to_buf(ann.val_literal.int_value, buf);
        case STRING:
            char[] str_lit = self.interp.symbols.get_name(ann.val_literal.sym_value);
            self.serialize_quoted_text_to_buf(str_lit, buf);
        case SYMBOL:
            char[] sym_name = self.interp.symbols.get_name(ann.val_literal.sym_value);
            if ((uint)ann.val_literal.sym_value == (uint)self.interp.sym_true ||
                (uint)ann.val_literal.sym_value == (uint)self.interp.sym_false) {
                self.buf_append(buf, sym_name);
            } else {
                buf.push('\'');
                self.buf_append(buf, sym_name);
            }
        case NIL:
            if ((uint)ann.val_literal.sym_value == (uint)self.interp.sym_false) {
                self.buf_append(buf, "false");
            } else {
                self.buf_append(buf, "nil");
            }
        default:
            self.buf_append(buf, "nil");
    }
}

fn void Compiler.serialize_type_annotation_to_buf(Compiler* self, TypeAnnotation* ann, List{char}* buf) {
    if (ann == null || !ann.has_annotation) return;

    if (ann.is_dict) {
        self.buf_append(buf, "^{");
        for (usz i = 0; i < ann.meta_count; i++) {
            if (i > 0) buf.push(' ');
            buf.push('\'');
            char[] key_name = self.interp.symbols.get_name(ann.meta[i].key);
            self.buf_append(buf, key_name);
            buf.push(' ');
            if (ann.meta[i].is_int) {
                self.serialize_int_value_to_buf(ann.meta[i].int_value, buf);
            } else {
                char[] val_name = self.interp.symbols.get_name(ann.meta[i].sym_value);
                self.buf_append(buf, val_name);
            }
        }
        buf.push('}');
        return;
    }

    if (ann.is_compound) {
        self.buf_append(buf, "^(");
        char[] base_name = self.interp.symbols.get_name(ann.base_type);
        self.buf_append(buf, base_name);
        if (ann.has_val_literal) {
            buf.push(' ');
            self.serialize_type_val_literal_to_buf(ann, buf);
        } else {
            for (usz i = 0; i < ann.param_count; i++) {
                buf.push(' ');
                char[] p_name = self.interp.symbols.get_name(ann.params[i]);
                self.buf_append(buf, p_name);
            }
        }
        buf.push(')');
        return;
    }

    self.buf_append(buf, "^");
    char[] base_name = self.interp.symbols.get_name(ann.base_type);
    self.buf_append(buf, base_name);
}
