module lisp;

import std::io;
import std::collections::list;
import main;

// =============================================================================
// SECTION 8a: STATEMENT-LEVEL COMPILATION (quasiquote call lowering)
// =============================================================================
fn Expr* Compiler.qq_call_element(Compiler* self, Expr* tmpl, usz index) @inline {
    if (index == 0) return tmpl.call.func;
    return tmpl.call.args[index - 1];
}

fn bool Compiler.qq_call_has_splice(Compiler* self, Expr* tmpl, usz total, usz depth) {
    if (depth != 0) return false;
    for (usz i = 0; i < total; i++) {
        if (self.qq_call_element(tmpl, i).tag == E_UNQUOTE_SPLICING) return true;
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

fn usz Compiler.compile_qq_call_no_splice(Compiler* self, Expr* tmpl, usz total, usz depth) {
    usz result_r = self.qq_emit_nil_result();
    for (usz i = total; i > 0; i--) {
        usz elem_r = self.compile_qq_flat(self.qq_call_element(tmpl, i - 1), depth);
        result_r = self.qq_emit_cons_pair(elem_r, result_r);
    }
    return result_r;
}

fn usz Compiler.compile_qq_call_with_splice(Compiler* self, Expr* tmpl, usz total, usz depth) {
    usz result_r = self.qq_emit_nil_result();
    for (usz i = total; i > 0; i--) {
        Expr* elem = self.qq_call_element(tmpl, i - 1);
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
    if (!self.qq_call_has_splice(tmpl, total, depth)) {
        return self.compile_qq_call_no_splice(tmpl, total, depth);
    }
    return self.compile_qq_call_with_splice(tmpl, total, depth);
}
