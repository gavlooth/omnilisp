module lisp;

import std::io;
import std::collections::list;
import main;

// =============================================================================
// SECTION 8a: STATEMENT-LEVEL COMPILATION (core temp dispatch)
// =============================================================================
fn usz Compiler.next_result(Compiler* self) {
    usz id = self.temp_counter;
    self.temp_counter++;
    return id;
}

fn void Compiler.emit_temp_decl(Compiler* self, usz id) {
    self.emit_indent();
    self.emit("lisp::Value* _r");
    self.emit_usz(id);
}

fn void Compiler.emit_temp_ref(Compiler* self, usz id) {
    self.emit("_r");
    self.emit_usz(id);
}

fn usz Compiler.emit_nil_temp(Compiler* self) {
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::make_nil();\n");
    return id;
}

fn usz Compiler.compile_leaf_expr_to_temp(Compiler* self, Expr* expr) {
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = ");
    self.compile_expr(expr);
    self.emit(";\n");
    return id;
}

/**
 * Compile an expression as statements, storing result in _rN.
 * Returns N. All temp declarations are emitted as proper C3 statements.
 */
fn usz Compiler.compile_to_temp_non_null(Compiler* self, Expr* expr) {
    switch (expr.tag) {
        case E_CALL: return self.compile_call_flat(expr);
        case E_LET: return self.compile_let_flat(expr);
        case E_AND: return self.compile_and_flat(expr);
        case E_OR: return self.compile_or_flat(expr);
        case E_BLOCK: return self.compile_block_flat(expr);
        case E_SET: return self.compile_set_flat(expr);
        case E_RESET: return self.compile_reset_flat(expr);
        case E_SHIFT: return self.compile_shift_flat(expr);
        case E_PERFORM: return self.compile_perform_flat(expr);
        case E_RESOLVE: return self.compile_resolve_flat(expr);
        case E_HANDLE: return self.compile_handle_flat(expr);
        case E_QUASIQUOTE: return self.compile_qq_flat(expr.quasiquote.body, 0);
        case E_DEFMACRO:
            usz defmacro_id = self.next_result();
            self.emit_temp_decl(defmacro_id);
            self.emit(" = aot::make_nil(); // macro: expanded at compile time\n");
            return defmacro_id;
        case E_MODULE: return self.compile_module_flat(expr);
        case E_IMPORT: return self.compile_import_flat(expr);
        case E_EXPORT_FROM: return self.compile_export_from_flat(expr);
        case E_IF: return self.compile_if_flat(expr);
        case E_APP: return self.compile_app_flat(expr);
        case E_LAMBDA: return self.compile_lambda_flat(expr);
        case E_MATCH: return self.compile_match_flat(expr);
        case E_INDEX: return self.compile_index_flat(expr);
        case E_DEFINE: return self.compile_define_flat(expr);
        case E_DEFTYPE:
        case E_DEFABSTRACT:
        case E_DEFUNION:
        case E_DEFALIAS:
        case E_DEFEFFECT:
            return self.compile_type_form_direct(expr);
        default: return self.compile_leaf_expr_to_temp(expr);
    }
}

fn usz Compiler.compile_to_temp(Compiler* self, Expr* expr) {
    if (expr == null) {
        return self.emit_nil_temp();
    }
    return self.compile_to_temp_non_null(expr);
}

/**
 * compile_to_temp for tail position. For calls, emits make_thunk instead of rt_invoke.
 */
fn usz Compiler.compile_to_temp_tail(Compiler* self, Expr* expr) {
    if (expr == null) {
        return self.emit_nil_temp();
    }

    switch (expr.tag) {
        case E_APP:
            return self.compile_app_tail_flat(expr);

        case E_CALL:
            return self.compile_call_tail_flat(expr);

        case E_IF:
            return self.compile_tail_if_flat(expr);

        case E_BLOCK:
            return self.compile_tail_block_flat(expr);

        case E_LET:
            return self.compile_let_flat_tail(expr);

        case E_AND:
            return self.compile_tail_and_flat(expr);

        case E_OR:
            return self.compile_tail_or_flat(expr);

        default:
            // Non-tail forms: just use compile_to_temp
            return self.compile_to_temp(expr);
    }
}
