module lisp;

import std::io;
import std::collections::list;
import main;

// =============================================================================
// SECTION 8a: STATEMENT-LEVEL COMPILATION (quasiquote lowering)
// =============================================================================
/**
 * compile_qq_flat — Flat-style quasiquote compilation.
 * Emits statements that build the quoted structure, storing result in _rN.
 * At depth 0, unquote sub-expressions are compiled as normal expressions.
 */
fn usz Compiler.compile_qq_marker_pair(Compiler* self, char[] marker, usz inner_r) {
    usz nil_r = self.emit_nil_temp();

    usz rest_r = self.next_result();
    self.emit_temp_decl(rest_r);
    self.emit(" = aot::cons(");
    self.emit_temp_ref(inner_r);
    self.emit(", ");
    self.emit_temp_ref(nil_r);
    self.emit(");\n");

    usz sym_r = self.next_result();
    self.emit_temp_decl(sym_r);
    self.emit(" = aot::make_symbol(\"");
    self.emit(marker);
    self.emit("\");\n");

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::cons(");
    self.emit_temp_ref(sym_r);
    self.emit(", ");
    self.emit_temp_ref(rest_r);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_qq_var_symbol(Compiler* self, SymbolId sym) {
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::make_symbol(\"");
    char[] name = self.interp.symbols.get_name(sym);
    self.emit_escaped(name);
    self.emit("\");\n");
    return id;
}

fn usz Compiler.compile_qq_app_list(Compiler* self, Expr* tmpl, usz depth) {
    usz func_r = self.compile_qq_flat(tmpl.app.func, depth);
    usz arg_r = self.compile_qq_flat(tmpl.app.arg, depth);
    usz nil_r = self.emit_nil_temp();

    usz inner_r = self.next_result();
    self.emit_temp_decl(inner_r);
    self.emit(" = aot::cons(");
    self.emit_temp_ref(arg_r);
    self.emit(", ");
    self.emit_temp_ref(nil_r);
    self.emit(");\n");

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::cons(");
    self.emit_temp_ref(func_r);
    self.emit(", ");
    self.emit_temp_ref(inner_r);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_qq_flat(Compiler* self, Expr* tmpl, usz depth) {
    if (tmpl == null) {
        return self.emit_nil_temp();
    }

    switch (tmpl.tag) {
        case E_UNQUOTE:
            if (depth == 0) return self.compile_to_temp(tmpl.unquote.body);
            return self.compile_qq_marker_pair("unquote", self.compile_qq_flat(tmpl.unquote.body, depth - 1));
        case E_UNQUOTE_SPLICING:
            if (depth == 0) {
                usz id = self.emit_nil_temp();
                self.emit_indent();
                self.emit("// ERROR: ,@ in non-list context\n");
                return id;
            }
            return self.compile_qq_marker_pair("unquote-splicing", self.compile_qq_flat(tmpl.unquote_splicing.body, depth - 1));
        case E_QUASIQUOTE:
            return self.compile_qq_marker_pair("quasiquote", self.compile_qq_flat(tmpl.quasiquote.body, depth + 1));
        case E_LIT: {
            usz id = self.next_result();
            self.emit_temp_decl(id);
            self.emit(" = ");
            self.compile_literal(tmpl.lit.value);
            self.emit(";\n");
            return id;
        }
        case E_VAR:
            return self.compile_qq_var_symbol(tmpl.var_expr.name);
        case E_QUOTE: {
            usz id = self.next_result();
            self.emit_temp_decl(id);
            self.emit(" = ");
            self.compile_literal(tmpl.quote.datum);
            self.emit(";\n");
            return id;
        }
        case E_APP:
            return self.compile_qq_app_list(tmpl, depth);
        case E_CALL:
            return self.compile_qq_call_flat(tmpl, depth);
        default:
            return self.emit_nil_temp();
    }
}

fn bool Compiler.qq_call_has_splice(Compiler* self, Expr*[] elements, usz total, usz depth) {
    if (depth != 0) return false;
    for (usz i = 0; i < total; i++) {
        if (elements[i].tag == E_UNQUOTE_SPLICING) return true;
    }
    return false;
}

fn usz Compiler.qq_emit_cons_pair(Compiler* self, usz head_r, usz tail_r) {
    usz new_r = self.next_result();
    self.emit_temp_decl(new_r);
    self.emit(" = aot::cons(");
    self.emit_temp_ref(head_r);
    self.emit(", ");
    self.emit_temp_ref(tail_r);
    self.emit(");\n");
    return new_r;
}

fn usz Compiler.qq_emit_append_pair(Compiler* self, usz left_r, usz right_r) {
    usz new_r = self.next_result();
    self.emit_temp_decl(new_r);
    self.emit(" = aot::list_append(");
    self.emit_temp_ref(left_r);
    self.emit(", ");
    self.emit_temp_ref(right_r);
    self.emit(");\n");
    return new_r;
}

fn usz Compiler.qq_emit_nil_result(Compiler* self) {
    usz result_r = self.next_result();
    self.emit_temp_decl(result_r);
    self.emit(" = aot::make_nil();\n");
    return result_r;
}

fn usz Compiler.compile_qq_call_no_splice(Compiler* self, Expr*[] elements, usz total, usz depth) {
    usz result_r = self.qq_emit_nil_result();
    for (usz i = total; i > 0; i--) {
        usz elem_r = self.compile_qq_flat(elements[i - 1], depth);
        result_r = self.qq_emit_cons_pair(elem_r, result_r);
    }
    return result_r;
}

fn usz Compiler.compile_qq_call_with_splice(Compiler* self, Expr*[] elements, usz total, usz depth) {
    usz result_r = self.qq_emit_nil_result();
    for (usz i = total; i > 0; i--) {
        Expr* elem = elements[i - 1];
        if (elem.tag == E_UNQUOTE_SPLICING && depth == 0) {
            usz splice_r = self.compile_to_temp(elem.unquote_splicing.body);
            result_r = self.qq_emit_append_pair(splice_r, result_r);
        } else {
            usz elem_r = self.compile_qq_flat(elem, depth);
            result_r = self.qq_emit_cons_pair(elem_r, result_r);
        }
    }
    return result_r;
}

/**
 * compile_qq_call_flat — Flat-style compilation of E_CALL in QQ context.
 * Builds cons chain from elements, handling ,@ splicing via rt_list_append.
 */
fn usz Compiler.compile_qq_call_flat(Compiler* self, Expr* tmpl, usz depth) {
    usz total = tmpl.call.arg_count + 1;
    Expr*[17] elements;
    elements[0] = tmpl.call.func;
    for (usz i = 0; i < tmpl.call.arg_count && i < 16; i++) {
        elements[i + 1] = tmpl.call.args[i];
    }

    if (!self.qq_call_has_splice(elements[..], total, depth)) {
        return self.compile_qq_call_no_splice(elements[..], total, depth);
    }
    return self.compile_qq_call_with_splice(elements[..], total, depth);
}
