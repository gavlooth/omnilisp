module lisp;

import std::collections::list;
import main;
// =============================================================================
// SECTION 4c: EXPRESSION SERIALIZER (pattern forms)
// =============================================================================
fn void Compiler.serialize_pattern_to_buf(Compiler* self, Pattern* pat, List{char}* buf) {
    if (pat == null) {
        buf.push('_');
        return;
    }

    switch (pat.tag) {
        case PAT_WILDCARD:
            buf.push('_');

        case PAT_VAR:
            char[] vname = self.interp.symbols.get_name(pat.var_name);
            self.buf_append(buf, vname);

        case PAT_LIT:
            self.serialize_value_to_buf(pat.lit_value, buf);

        case PAT_QUOTE:
            buf.push('\'');
            self.serialize_value_to_buf(pat.quote_datum, buf);

        case PAT_SEQ:
            buf.push('[');
            for (usz i = 0; i < pat.elem_count; i++) {
                if (i > 0) buf.push(' ');
                self.serialize_pattern_to_buf(pat.elements[i], buf);
            }
            if (pat.rest_pos == REST_MIDDLE) {
                self.buf_append(buf, " .. ");
                char[] rname = self.interp.symbols.get_name(pat.rest_binding);
                self.buf_append(buf, rname);
            }
            buf.push(']');

        case PAT_AS:
            buf.push('(');
            self.serialize_pattern_to_buf(pat.as_sub, buf);
            self.buf_append(buf, " 'as ");
            char[] as_name = self.interp.symbols.get_name(pat.as_name);
            self.buf_append(buf, as_name);
            buf.push(')');

        case PAT_CONS:
            buf.push('(');
            self.serialize_pattern_to_buf(pat.car_pat, buf);
            self.buf_append(buf, " . ");
            self.serialize_pattern_to_buf(pat.cdr_pat, buf);
            buf.push(')');

        case PAT_CONSTRUCTOR:
            buf.push('(');
            self.buf_append(buf, self.interp.symbols.get_name(pat.constructor_name));
            for (usz i = 0; i < pat.ctor_sub_count; i++) {
                buf.push(' ');
                self.serialize_pattern_to_buf(pat.ctor_sub_patterns[i], buf);
            }
            buf.push(')');

        case PAT_DICT:
            buf.push('{');
            for (usz i = 0; i < pat.dict_key_count; i++) {
                if (i > 0) buf.push(' ');
                self.buf_append(buf, self.interp.symbols.get_name(pat.dict_keys[i]));
            }
            buf.push('}');

        case PAT_GUARD:
            buf.push('(');
            self.buf_append(buf, "? ");
            self.serialize_expr_to_buf(pat.guard_pred, buf);
            if (pat.guard_sub != null) {
                buf.push(' ');
                self.serialize_pattern_to_buf(pat.guard_sub, buf);
            }
            buf.push(')');

        default:
            self.set_compile_error("compiler: cannot serialize unknown pattern tag");
    }
}
