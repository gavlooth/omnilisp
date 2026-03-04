module lisp;

import std::core::mem;
import main;

// JIT expression lowering: core expression forms and call/app lowering.
fn void? jit_compile_expr_group_core(void* s, Expr* expr, Interp* interp,
    JitLocals* locals, bool is_tail, bool* handled) {
    *handled = true;
    switch (expr.tag) {
        case E_LIT:
            jit_compile_lit(s, expr, interp)!;
        case E_VAR:
            jit_compile_var(s, expr, interp, locals)!;
        case E_IF:
            jit_compile_if(s, expr, interp, locals, is_tail)!;
        case E_AND:
            jit_compile_and(s, expr, interp, locals, is_tail)!;
        case E_OR:
            jit_compile_or(s, expr, interp, locals, is_tail)!;
        case E_BEGIN:
            jit_compile_begin(s, expr, interp, locals, is_tail)!;
        case E_APP:
            jit_compile_app(s, expr, interp, locals, is_tail)!;
        case E_CALL:
            jit_compile_call(s, expr, interp, locals, is_tail)!;
        case E_LET:
            jit_compile_let(s, expr, interp, locals, is_tail)!;
        case E_LAMBDA:
            jit_compile_lambda(s, expr, interp, locals)!;
        case E_SET:
            jit_compile_set(s, expr, interp, locals)!;
        case E_DEFINE:
            jit_compile_define(s, expr, interp, locals)!;
        default:
            *handled = false;
    }
}

fn void? jit_compile_expr_group_effects(void* s, Expr* expr, Interp* interp,
    JitLocals* locals, bool is_tail, bool* handled) {
    *handled = true;
    switch (expr.tag) {
        case E_QUASIQUOTE:
            jit_compile_quasiquote(s, expr, interp, locals)!;
        case E_MATCH:
            jit_compile_match(s, expr, interp, locals)!;
        case E_DEFMACRO:
            jit_compile_define_macro(s, expr, interp, locals)!;
        case E_RESET:
            jit_compile_reset(s, expr, interp, locals)!;
        case E_SHIFT:
            jit_compile_shift(s, expr, interp, locals)!;
        case E_HANDLE:
            jit_compile_handle(s, expr, interp, locals)!;
        case E_PERFORM:
            jit_compile_perform(s, expr, interp, locals)!;
        case E_RESOLVE:
            jit_compile_resolve(s, expr, interp, locals)!;
        case E_QUOTE:
            jit_compile_quote(s, expr, interp)!;
        case E_INDEX:
            jit_compile_index(s, expr, interp, locals)!;
        case E_PATH:
            jit_compile_path(s, expr, interp, locals)!;
        case E_MODULE:
            jit_compile_module(s, expr, interp, locals)!;
        case E_IMPORT:
            jit_compile_import(s, expr, interp, locals)!;
        default:
            *handled = false;
    }
}

fn void? jit_compile_expr_group_types_ffi(void* s, Expr* expr, Interp* interp,
    JitLocals* locals, bool* handled) {
    *handled = true;
    switch (expr.tag) {
        case E_DEFTYPE:
            jit_compile_deftype(s, expr, interp, locals)!;
        case E_DEFABSTRACT:
            jit_compile_defabstract(s, expr, interp, locals)!;
        case E_DEFUNION:
            jit_compile_defunion(s, expr, interp, locals)!;
        case E_DEFALIAS:
            jit_compile_defalias(s, expr, interp, locals)!;
        case E_DEFEFFECT:
            jit_compile_defeffect(s, expr, interp, locals)!;
        case E_EXPORT_FROM:
            jit_compile_export_from(s, expr, interp, locals)!;
        case E_FFI_LIB:
            jit_compile_3arg_helper(s, expr, interp, locals, (void*)&jit_do_ffi_lib)!;
        case E_FFI_FN:
            jit_compile_3arg_helper(s, expr, interp, locals, (void*)&jit_do_ffi_fn)!;
        case E_UNQUOTE:
        case E_UNQUOTE_SPLICING:
            // These should never appear outside quasiquote
            return JIT_COMPILE_FAILED~;
        default:
            *handled = false;
    }
}

/**
 * Compile an expression node. Result is left in JIT_R0.
 * Returns a fault if the expression can't be compiled.
 * locals tracks JIT-compiled let bindings for stack-based variable access.
 */
fn void? jit_compile_expr(void* s, Expr* expr, Interp* interp, JitLocals* locals, bool is_tail = false) {
    if (expr == null) {
        // null expr -> nil
        emit_call_1(s, (void*)&jit_make_nil);  // R0 = make_nil(V0=interp)
        return;
    }

    bool handled = false;
    jit_compile_expr_group_core(s, expr, interp, locals, is_tail, &handled)!;
    if (handled) return;

    jit_compile_expr_group_effects(s, expr, interp, locals, is_tail, &handled)!;
    if (handled) return;

    jit_compile_expr_group_types_ffi(s, expr, interp, locals, &handled)!;
    if (handled) return;

    jit_compile_fallback(s, expr, interp, locals)!;
}

// -----------------------------------------------------------------------------
// Expression compilers
// -----------------------------------------------------------------------------

fn void? jit_compile_lit(void* s, Expr* expr, Interp* interp) {
    Value* lit = expr.lit.value;
    if (lit == null || lit.tag == NIL) {
        emit_call_1(s, (void*)&jit_make_nil);
        return;
    }
    if (lit.tag == INT) {
        emit_call_2i(s, (void*)&jit_make_int, JIT_V0, lit.int_val);
        return;
    }
    // For other literal types, use the literal value directly
    // (it's a pointer to a pre-existing Value in the expr pool)
    _jit_new_node_ww(s, CODE_MOVI, JIT_R0, (long)lit);
}

fn void? jit_compile_var(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    uint sym_id = (uint)expr.var_expr.name;

    // Check JIT locals first (reverse order for proper scoping of shadowed names)
    for (usz i = locals.locals.len(); i > 0; i--) {
        if ((uint)locals.locals[i - 1].name == sym_id) {
            if (locals.locals[i - 1].is_mutable) {
                // Mutable local: stack slot holds Env* (the box). Lookup value in it.
                _jit_new_node_www(s, CODE_LDXI_L, (long)JIT_V1, (long)JIT_FP, (long)locals.locals[i - 1].stack_offset);
                emit_call_2i(s, (void*)&jit_env_lookup_local, JIT_V1, (long)sym_id);
                return;
            }
            // Non-mutable: load value directly from stack slot
            _jit_new_node_www(s, CODE_LDXI_L, (long)JIT_R0, (long)JIT_FP, (long)locals.locals[i - 1].stack_offset);
            return;
        }
    }

    // Fall back to jit_lookup_var: checks jit_env (for sub-expression local bindings), then global
    emit_call_2i(s, (void*)&jit_lookup_var, JIT_V0, (long)sym_id);
}

fn void? jit_compile_if(void* s, Expr* expr, Interp* interp, JitLocals* locals, bool is_tail) {
    // Compile test expression -> R0 (test is never in tail position)
    jit_compile_expr(s, expr.if_expr.test, interp, locals)!;

    // Save test result to V1, call is_falsy(V1, interp)
    _jit_new_node_ww(s, CODE_MOVR, JIT_V1, JIT_R0);  // V1 = test result
    emit_call_2(s, (void*)&jit_is_falsy, JIT_V1, JIT_V0);

    // If is_falsy returned true (non-zero), jump to else branch
    void* jump_else = _jit_new_node_pww(s, CODE_BNEI, null, (long)JIT_R0, 0);

    // Then branch — inherits tail position
    jit_compile_expr(s, expr.if_expr.then_branch, interp, locals, is_tail)!;
    void* jump_end = _jit_new_node_p(s, CODE_JMPI, null);

    // Else branch — inherits tail position
    _jit_patch(s, jump_else);
    jit_compile_expr(s, expr.if_expr.else_branch, interp, locals, is_tail)!;

    // End
    _jit_patch(s, jump_end);
}

fn void? jit_compile_and(void* s, Expr* expr, Interp* interp, JitLocals* locals, bool is_tail) {
    // Evaluate left -> R0 (left is never in tail position)
    jit_compile_expr(s, expr.and_expr.left, interp, locals)!;

    // Check if left is falsy
    _jit_new_node_ww(s, CODE_MOVR, JIT_V1, JIT_R0);  // V1 = left result
    emit_call_2(s, (void*)&jit_is_falsy, JIT_V1, JIT_V0);

    // If falsy, short-circuit: result = left (V1)
    void* jump_done = _jit_new_node_pww(s, CODE_BNEI, null, (long)JIT_R0, 0);

    // Not falsy: evaluate right — inherits tail position
    jit_compile_expr(s, expr.and_expr.right, interp, locals, is_tail)!;
    void* jump_end = _jit_new_node_p(s, CODE_JMPI, null);

    // Short-circuit: restore V1 to R0
    _jit_patch(s, jump_done);
    _jit_new_node_ww(s, CODE_MOVR, JIT_R0, JIT_V1);

    _jit_patch(s, jump_end);
}

fn void? jit_compile_or(void* s, Expr* expr, Interp* interp, JitLocals* locals, bool is_tail) {
    // Evaluate left -> R0 (left is never in tail position)
    jit_compile_expr(s, expr.or_expr.left, interp, locals)!;

    // Check if left is falsy
    _jit_new_node_ww(s, CODE_MOVR, JIT_V1, JIT_R0);  // V1 = left result
    emit_call_2(s, (void*)&jit_is_falsy, JIT_V1, JIT_V0);

    // If NOT falsy, short-circuit: result = left (V1)
    void* jump_done = _jit_new_node_pww(s, CODE_BEQI, null, (long)JIT_R0, 0);

    // Falsy: evaluate right — inherits tail position
    jit_compile_expr(s, expr.or_expr.right, interp, locals, is_tail)!;
    void* jump_end = _jit_new_node_p(s, CODE_JMPI, null);

    // Short-circuit: restore V1 to R0
    _jit_patch(s, jump_done);
    _jit_new_node_ww(s, CODE_MOVR, JIT_R0, JIT_V1);

    _jit_patch(s, jump_end);
}

fn void? jit_compile_begin(void* s, Expr* expr, Interp* interp, JitLocals* locals, bool is_tail) {
    usz count = expr.begin.expr_count;
    if (count == 0) {
        emit_call_1(s, (void*)&jit_make_nil);
        return;
    }
    // Evaluate all expressions; last inherits tail position
    for (usz i = 0; i < count; i++) {
        bool tail = is_tail && (i == count - 1);
        jit_compile_expr(s, expr.begin.exprs[i], interp, locals, tail)!;
    }
}

fn void? jit_compile_app(void* s, Expr* expr, Interp* interp, JitLocals* locals, bool is_tail) {
    // Compile function -> R0, save to V1 (not in tail position)
    jit_compile_expr(s, expr.app.func, interp, locals)!;
    _jit_new_node_ww(s, CODE_MOVR, JIT_V1, JIT_R0);  // V1 = func

    // Compile argument -> R0, save to V2 (not in tail position)
    jit_compile_expr(s, expr.app.arg, interp, locals)!;
    _jit_new_node_ww(s, CODE_MOVR, JIT_V2, JIT_R0);  // V2 = arg

    // Call apply — use _tail variant when in tail position
    void* apply_fn = is_tail ? (void*)&jit_apply_value_tail : (void*)&jit_apply_value;
    emit_call_3(s, apply_fn, JIT_V1, JIT_V2, JIT_V0);
}

/**
 * Check if a symbol names a known arithmetic primitive, return its direct helper.
 */
fn void* jit_get_direct_prim(SymbolId name, Interp* interp) {
    // If the symbol has been overloaded (METHOD_TABLE), don't use direct path
    Value* val = interp.global_env.lookup(name);
    if (val != null && val.tag == METHOD_TABLE) return null;

    // Check against interned primitive symbols
    char[] sym_name = interp.symbols.get_name(name);
    if (sym_name.len == 1) {
        switch (sym_name[0]) {
            case '+': return (void*)&jit_prim_add;
            case '-': return (void*)&jit_prim_sub;
            case '*': return (void*)&jit_prim_mul;
            case '<': return (void*)&jit_prim_lt;
            case '>': return (void*)&jit_prim_gt;
            case '=': return (void*)&jit_prim_eq;
            default: return null;
        }
    }
    return null;
}

/**
 * Check if an expression is "simple" (doesn't clobber V1/V2 during compilation).
 */
fn bool jit_is_simple_expr(Expr* expr) {
    if (expr == null) return true;
    return expr.tag == E_LIT || expr.tag == E_VAR;
}

fn void? jit_compile_call(void* s, Expr* expr, Interp* interp, JitLocals* locals, bool is_tail) {
    // Check for macro expansion at compile time
    if (expr.call.func.tag == E_VAR) {
        MacroDef* macro_def = lookup_macro(expr.call.func.var_expr.name, interp);
        if (macro_def != null) {
            EvalResult macro_result = expand_pattern_macro(macro_def, expr, interp);
            if (macro_result.error.has_error) return JIT_COMPILE_FAILED~;
            Expr* expanded = value_to_expr(macro_result.value, interp);
            jit_compile_expr(s, expanded, interp, locals, is_tail)!;
            return;
        }
    }

    // Check if this is a known binary primitive: (+ a b), (- a b), etc.
    if (expr.call.arg_count == 2 && expr.call.func.tag == E_VAR) {
        void* direct_fn = jit_get_direct_prim(expr.call.func.var_expr.name, interp);
        if (direct_fn != null) {
            jit_compile_inline_binop(s, expr, interp, locals, direct_fn)!;
            return;
        }
    }

    // General case: compile function and args, dispatch through apply
    jit_compile_multi_arg(s, expr, interp, locals, is_tail)!;
}

// Inline 2-arg call for known binary primitives (+, -, *, /, =, <, >).
// Supports both simple (E_LIT/E_VAR) and complex (nested calls) arguments
// by spilling intermediate results to the stack frame.
fn void? jit_compile_inline_binop(void* s, Expr* expr, Interp* interp, JitLocals* locals, void* direct_fn) {
    bool arg0_simple = jit_is_simple_expr(expr.call.args[0]);
    bool arg1_simple = jit_is_simple_expr(expr.call.args[1]);

    if (arg0_simple && arg1_simple) {
        // Fast path: both args are simple, no spilling needed
        // Compile arg0 -> R0, save to V1
        jit_compile_expr(s, expr.call.args[0], interp, locals)!;
        _jit_new_node_ww(s, CODE_MOVR, JIT_V1, JIT_R0);  // V1 = arg0

        // Compile arg1 -> R0, save to V2
        jit_compile_expr(s, expr.call.args[1], interp, locals)!;
        _jit_new_node_ww(s, CODE_MOVR, JIT_V2, JIT_R0);  // V2 = arg1
    } else {
        // Nested path: at least one arg is complex (e.g. a function call).
        // Compile arg0, spill to stack, compile arg1, restore arg0.
        // This avoids V1/V2 clobbering when nested jit_compile_expr
        // uses those registers internally.

        // Allocate a stack slot for spilling arg0 result
        int spill_offset = _jit_allocai(s, 8);  // 8 bytes for a pointer

        // Compile first arg -> result in R0
        jit_compile_expr(s, expr.call.args[0], interp, locals)!;
        // Spill arg0 result to stack: *(FP + spill_offset) = R0
        _jit_new_node_www(s, CODE_STXI_L, (long)spill_offset, (long)JIT_FP, (long)JIT_R0);

        // Compile second arg -> result in R0, move to V2
        jit_compile_expr(s, expr.call.args[1], interp, locals)!;
        _jit_new_node_ww(s, CODE_MOVR, JIT_V2, JIT_R0);  // V2 = arg1

        // Restore arg0 from stack into V1: V1 = *(FP + spill_offset)
        _jit_new_node_www(s, CODE_LDXI_L, (long)JIT_V1, (long)JIT_FP, (long)spill_offset);
    }

    // Call direct_fn(interp=V0, a=V1, b=V2) -> R0
    emit_call_3(s, direct_fn, JIT_V0, JIT_V1, JIT_V2);
}

// General multi-arg call: handles 0, 1, 2+ arguments.
// Compiles function and args, spills to stack slots, builds cons arg list
// right-to-left, dispatches through jit_apply_multi_args / jit_apply_value.
fn void jit_emit_call_name_for_expr(void* s, Expr* expr, Interp* interp) {
    if (expr.call.func.tag == E_VAR) {
        emit_call_2i(s, (void*)&jit_set_call_name, JIT_V0, (long)expr.call.func.var_expr.name);
    } else {
        emit_call_1(s, (void*)&jit_clear_call_name);
    }
}

fn void? jit_compile_multi_arg_zero(void* s, Expr* expr, Interp* interp, JitLocals* locals, bool is_tail) {
    jit_compile_expr(s, expr.call.func, interp, locals)!;
    _jit_new_node_ww(s, CODE_MOVR, JIT_V1, JIT_R0);

    emit_call_1(s, (void*)&jit_make_nil);
    _jit_new_node_ww(s, CODE_MOVR, JIT_V2, JIT_R0);

    void* apply_fn = is_tail ? (void*)&jit_apply_multi_args_tail : (void*)&jit_apply_multi_args;
    emit_call_4_rrri(s, apply_fn, JIT_V0, JIT_V1, JIT_V2, 0);
}

fn void? jit_compile_multi_arg_one(void* s, Expr* expr, Interp* interp, JitLocals* locals, bool is_tail) {
    int spill_fn = _jit_allocai(s, 8);

    jit_compile_expr(s, expr.call.func, interp, locals)!;
    _jit_new_node_www(s, CODE_STXI_L, (long)spill_fn, (long)JIT_FP, (long)JIT_R0);

    jit_compile_expr(s, expr.call.args[0], interp, locals)!;
    _jit_new_node_ww(s, CODE_MOVR, JIT_V2, JIT_R0);
    _jit_new_node_www(s, CODE_LDXI_L, (long)JIT_V1, (long)JIT_FP, (long)spill_fn);

    void* apply_fn = is_tail ? (void*)&jit_apply_value_tail : (void*)&jit_apply_value;
    emit_call_3(s, apply_fn, JIT_V1, JIT_V2, JIT_V0);
}

fn void? jit_compile_multi_arg_many(void* s, Expr* expr, Interp* interp, JitLocals* locals, bool is_tail) {
    usz argc = expr.call.arg_count;
    if (argc > 16) return JIT_COMPILE_FAILED~;

    int[16] arg_slots;
    for (usz i = 0; i < argc; i++) {
        arg_slots[i] = _jit_allocai(s, 8);
    }

    for (usz i = 0; i < argc; i++) {
        jit_compile_expr(s, expr.call.args[i], interp, locals)!;
        _jit_new_node_www(s, CODE_STXI_L, (long)arg_slots[i], (long)JIT_FP, (long)JIT_R0);
    }

    int func_slot = _jit_allocai(s, 8);
    jit_compile_expr(s, expr.call.func, interp, locals)!;
    _jit_new_node_www(s, CODE_STXI_L, (long)func_slot, (long)JIT_FP, (long)JIT_R0);

    emit_call_1(s, (void*)&jit_make_nil);
    _jit_new_node_ww(s, CODE_MOVR, JIT_V2, JIT_R0);

    for (usz i = argc; i > 0; i--) {
        _jit_new_node_www(s, CODE_LDXI_L, (long)JIT_V1, (long)JIT_FP, (long)arg_slots[i - 1]);
        emit_call_3(s, (void*)&jit_cons, JIT_V0, JIT_V1, JIT_V2);
        _jit_new_node_ww(s, CODE_MOVR, JIT_V2, JIT_R0);
    }

    _jit_new_node_www(s, CODE_LDXI_L, (long)JIT_V1, (long)JIT_FP, (long)func_slot);

    void* apply_fn = is_tail ? (void*)&jit_apply_multi_args_tail : (void*)&jit_apply_multi_args;
    emit_call_4_rrri(s, apply_fn, JIT_V0, JIT_V1, JIT_V2, (long)argc);
}

fn void? jit_compile_multi_arg(void* s, Expr* expr, Interp* interp, JitLocals* locals, bool is_tail) {
    usz argc = expr.call.arg_count;
    jit_emit_call_name_for_expr(s, expr, interp);

    if (argc == 0) {
        jit_compile_multi_arg_zero(s, expr, interp, locals, is_tail)!;
        return;
    }
    if (argc == 1) {
        jit_compile_multi_arg_one(s, expr, interp, locals, is_tail)!;
        return;
    }
    jit_compile_multi_arg_many(s, expr, interp, locals, is_tail)!;
}
