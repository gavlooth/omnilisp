module lisp;

import std::core::mem;
import main;

// -----------------------------------------------------------------------------
// Call expression compilers
// -----------------------------------------------------------------------------

/**
 * Check if a symbol names a known arithmetic primitive, return its direct helper.
 */
fn void* jit_get_direct_prim(SymbolId name, Interp* interp) {
    // Only use direct path for canonical primitive global bindings.
    Value* val = interp.global_env.lookup(name);
    if (val == null || val.tag != PRIMITIVE) return null;

    // Check against interned primitive symbols
    char[] sym_name = interp.symbols.get_name(name);
    if (!primitive_name_equals(val, sym_name)) return null;
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

fn bool jit_locals_shadow_symbol(JitLocals* locals, SymbolId name) @inline {
    if (locals == null) return false;
    for (usz i = locals.locals.len(); i > 0; i--) {
        if ((uint)locals.locals[i - 1].name == (uint)name) return true;
    }
    return false;
}

/**
 * Check if an expression is "simple" (doesn't clobber V1/V2 during compilation).
 */
fn bool jit_is_simple_expr(Expr* expr) {
    if (expr == null) return true;
    return expr.tag == E_LIT || expr.tag == E_VAR;
}

enum JitTailConstructorKind : char {
    JIT_TAIL_CTOR_NONE,
    JIT_TAIL_CTOR_CONS,
    JIT_TAIL_CTOR_LIST,
    JIT_TAIL_CTOR_ARRAY,
}

fn bool jit_symbol_matches_global_primitive(Interp* interp, SymbolId name, char[] expected) {
    if (!ffi_char_slice_equals(interp.symbols.get_name(name), expected)) return false;
    Value* val = interp.global_env.lookup(name);
    return primitive_name_equals(val, expected);
}

fn JitTailConstructorKind jit_tail_constructor_kind(Expr* expr, Interp* interp, JitLocals* locals, bool is_tail) {
    if (!is_tail || expr == null || expr.call.func.tag != E_VAR) return JIT_TAIL_CTOR_NONE;
    SymbolId fn_name = expr.call.func.var_expr.name;
    if (jit_locals_shadow_symbol(locals, fn_name)) return JIT_TAIL_CTOR_NONE;
    if (jit_symbol_matches_global_primitive(interp, fn_name, "cons")) return JIT_TAIL_CTOR_CONS;
    if (expr.call.arg_count != 1 &&
        (jit_symbol_matches_global_primitive(interp, fn_name, "List") ||
        jit_symbol_matches_global_primitive(interp, fn_name, "list"))) return JIT_TAIL_CTOR_LIST;
    if (expr.call.arg_count != 1 && jit_symbol_matches_global_primitive(interp, fn_name, "Array")) return JIT_TAIL_CTOR_ARRAY;
    return JIT_TAIL_CTOR_NONE;
}

fn void? jit_compile_tail_cons_escape(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    if (expr.call.arg_count != 2) {
        jit_compile_multi_arg(s, expr, interp, locals, true)!;
        return;
    }

    int car_slot = _jit_allocai(s, 8);
    jit_compile_expr(s, expr.call.args[0], interp, locals)!;
    _jit_new_node_www(s, CODE_STXI_L, (long)car_slot, (long)JIT_FP, (long)JIT_R0);

    jit_compile_expr(s, expr.call.args[1], interp, locals)!;
    _jit_new_node_ww(s, CODE_MOVR, JIT_V2, JIT_R0);
    _jit_new_node_www(s, CODE_LDXI_L, (long)JIT_V1, (long)JIT_FP, (long)car_slot);
    emit_call_3(s, locals, (void*)&jit_cons_escape, JIT_V0, JIT_V1, JIT_V2);
}

fn void? jit_compile_tail_list_escape(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    usz argc = expr.call.arg_count;
    int* arg_slots = argc > 0
        ? (int*)mem::malloc(int.sizeof * argc)
        : null;
    if (argc > 0 && arg_slots == null) return (void)fault;
    defer if (arg_slots != null) mem::free(arg_slots);
    for (usz i = 0; i < argc; i++) {
        arg_slots[i] = _jit_allocai(s, 8);
        jit_compile_expr(s, expr.call.args[i], interp, locals)!;
        _jit_new_node_www(s, CODE_STXI_L, (long)arg_slots[i], (long)JIT_FP, (long)JIT_R0);
    }

    emit_call_1(s, locals, (void*)&jit_make_nil_escape);
    _jit_new_node_ww(s, CODE_MOVR, JIT_V2, JIT_R0);

    for (usz i = argc; i > 0; i--) {
        _jit_new_node_www(s, CODE_LDXI_L, (long)JIT_V1, (long)JIT_FP, (long)arg_slots[i - 1]);
        emit_call_3(s, locals, (void*)&jit_cons_escape, JIT_V0, JIT_V1, JIT_V2);
        _jit_new_node_ww(s, CODE_MOVR, JIT_V2, JIT_R0);
    }
    _jit_new_node_ww(s, CODE_MOVR, JIT_R0, JIT_V2);
}

fn void? jit_compile_tail_array_escape(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    usz argc = expr.call.arg_count;
    int* arg_slots = argc > 0
        ? (int*)mem::malloc(int.sizeof * argc)
        : null;
    if (argc > 0 && arg_slots == null) return (void)fault;
    defer if (arg_slots != null) mem::free(arg_slots);
    for (usz i = 0; i < argc; i++) {
        arg_slots[i] = _jit_allocai(s, 8);
        jit_compile_expr(s, expr.call.args[i], interp, locals)!;
        _jit_new_node_www(s, CODE_STXI_L, (long)arg_slots[i], (long)JIT_FP, (long)JIT_R0);
    }

    emit_call_2i(s, locals, (void*)&jit_make_array_escape, JIT_V0, (long)argc);
    _jit_new_node_ww(s, CODE_MOVR, JIT_V2, JIT_R0);

    for (usz i = 0; i < argc; i++) {
        _jit_new_node_www(s, CODE_LDXI_L, (long)JIT_V1, (long)JIT_FP, (long)arg_slots[i]);
        emit_call_4_rrir(s, locals, (void*)&jit_array_escape_set, JIT_V0, JIT_V2, (long)i, JIT_V1);
        _jit_new_node_ww(s, CODE_MOVR, JIT_V2, JIT_R0);
    }
    _jit_new_node_ww(s, CODE_MOVR, JIT_R0, JIT_V2);
}

fn void? jit_compile_tail_constructor_escape(void* s, Expr* expr, Interp* interp, JitLocals* locals, JitTailConstructorKind kind) {
    switch (kind) {
        case JIT_TAIL_CTOR_CONS:
            jit_compile_tail_cons_escape(s, expr, interp, locals)!;
        case JIT_TAIL_CTOR_LIST:
            jit_compile_tail_list_escape(s, expr, interp, locals)!;
        case JIT_TAIL_CTOR_ARRAY:
            jit_compile_tail_array_escape(s, expr, interp, locals)!;
        case JIT_TAIL_CTOR_NONE:
            jit_compile_multi_arg(s, expr, interp, locals, true)!;
    }
}

fn void? jit_compile_call(void* s, Expr* expr, Interp* interp, JitLocals* locals, bool is_tail) {
    // Check for macro expansion at compile time
    EvalResult macro_result;
    if (try_expand_macro_call_expr(expr, interp, &macro_result)) {
        Expr* expanded = macro_result_to_expr(&macro_result, expr, interp);
        jit_compile_expr(s, expanded, interp, locals, is_tail)!;
        return;
    }

    bool continuation_sensitive = expr_contains_shift(expr) || expr_contains_perform(expr);
    if (continuation_sensitive) {
        // Continuation-sensitive calls may suspend/resume during argument
        // evaluation. Route through a runtime helper that stages evaluated
        // arguments outside JIT spill slots before apply.
        jit_emit_call_name_for_expr(s, expr, interp, locals);
        bool has_env = emit_build_locals_env(s, interp, locals);
        if (!has_env) emit_load_env(s, locals, JIT_V2);
        void* helper = is_tail ? (void*)&jit_eval_call_cont_safe_tail : (void*)&jit_eval_call_cont_safe;
        emit_call_3i(s, locals, helper, JIT_V0, (long)expr, JIT_V2);
        return;
    }

    JitTailConstructorKind tail_ctor = jit_tail_constructor_kind(expr, interp, locals, is_tail);
    if (tail_ctor != JIT_TAIL_CTOR_NONE) {
        jit_compile_tail_constructor_escape(s, expr, interp, locals, tail_ctor)!;
        return;
    }

    // Check if this is a known binary primitive: (+ a b), (- a b), etc.
    // Disable the direct fast path for continuation-sensitive calls.
    if (expr.call.arg_count == 2 && expr.call.func.tag == E_VAR) {
        SymbolId fn_name = expr.call.func.var_expr.name;
        if (!jit_locals_shadow_symbol(locals, fn_name)) {
            void* direct_fn = jit_get_direct_prim(fn_name, interp);
            if (direct_fn != null) {
                jit_compile_inline_binop(s, expr, interp, locals, direct_fn)!;
                return;
            }
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
    emit_call_3(s, locals, direct_fn, JIT_V0, JIT_V1, JIT_V2);
}

// General multi-arg call: handles 0, 1, 2+ arguments.
// Compiles function and args, spills to stack slots, builds cons arg list
// right-to-left, dispatches through jit_apply_multi_args / jit_apply_value.
fn void jit_emit_call_name_for_expr(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    if (expr.call.func.tag == E_VAR) {
        emit_call_2i(s, locals, (void*)&jit_set_call_name, JIT_V0, (long)expr.call.func.var_expr.name);
    } else {
        emit_call_1(s, locals, (void*)&jit_clear_call_name);
    }
}

fn void? jit_compile_multi_arg_zero(void* s, Expr* expr, Interp* interp, JitLocals* locals, bool is_tail) {
    int spill_fn = _jit_allocai(s, 8);

    jit_compile_expr(s, expr.call.func, interp, locals)!;
    _jit_new_node_www(s, CODE_STXI_L, (long)spill_fn, (long)JIT_FP, (long)JIT_R0);

    emit_call_1(s, locals, (void*)&jit_make_nil);
    _jit_new_node_ww(s, CODE_MOVR, JIT_V2, JIT_R0);
    _jit_new_node_www(s, CODE_LDXI_L, (long)JIT_V1, (long)JIT_FP, (long)spill_fn);

    void* apply_fn = is_tail ? (void*)&jit_apply_multi_args_tail : (void*)&jit_apply_multi_args;
    emit_call_4_rrri(s, locals, apply_fn, JIT_V0, JIT_V1, JIT_V2, 0);
}

fn void? jit_compile_multi_arg_one(void* s, Expr* expr, Interp* interp, JitLocals* locals, bool is_tail) {
    int spill_fn = _jit_allocai(s, 8);

    jit_compile_expr(s, expr.call.func, interp, locals)!;
    _jit_new_node_www(s, CODE_STXI_L, (long)spill_fn, (long)JIT_FP, (long)JIT_R0);

    jit_compile_expr(s, expr.call.args[0], interp, locals)!;
    _jit_new_node_ww(s, CODE_MOVR, JIT_V2, JIT_R0);
    _jit_new_node_www(s, CODE_LDXI_L, (long)JIT_V1, (long)JIT_FP, (long)spill_fn);

    void* apply_fn = is_tail ? (void*)&jit_apply_value_tail : (void*)&jit_apply_value;
    emit_call_3(s, locals, apply_fn, JIT_V1, JIT_V2, JIT_V0);
}

fn void? jit_compile_multi_arg_many(void* s, Expr* expr, Interp* interp, JitLocals* locals, bool is_tail) {
    usz argc = expr.call.arg_count;
    int* arg_slots = argc > 0
        ? (int*)mem::malloc(int.sizeof * argc)
        : null;
    if (argc > 0 && arg_slots == null) return (void)fault;
    defer if (arg_slots != null) mem::free(arg_slots);
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

    void* nil_fn = is_tail ? (void*)&jit_make_nil_escape : (void*)&jit_make_nil;
    emit_call_1(s, locals, nil_fn);
    _jit_new_node_ww(s, CODE_MOVR, JIT_V2, JIT_R0);

    void* cons_fn = is_tail ? (void*)&jit_cons_escape : (void*)&jit_cons;
    for (usz i = argc; i > 0; i--) {
        _jit_new_node_www(s, CODE_LDXI_L, (long)JIT_V1, (long)JIT_FP, (long)arg_slots[i - 1]);
        emit_call_3(s, locals, cons_fn, JIT_V0, JIT_V1, JIT_V2);
        _jit_new_node_ww(s, CODE_MOVR, JIT_V2, JIT_R0);
    }

    _jit_new_node_www(s, CODE_LDXI_L, (long)JIT_V1, (long)JIT_FP, (long)func_slot);

    void* apply_fn = is_tail ? (void*)&jit_apply_multi_args_tail : (void*)&jit_apply_multi_args;
    emit_call_4_rrri(s, locals, apply_fn, JIT_V0, JIT_V1, JIT_V2, (long)argc);
}

fn void? jit_compile_multi_arg(void* s, Expr* expr, Interp* interp, JitLocals* locals, bool is_tail) {
    usz argc = expr.call.arg_count;
    jit_emit_call_name_for_expr(s, expr, interp, locals);

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
