module lisp;

const usz JIT_FALLBACK_TAG_BUCKETS = (usz)ExprTag.E_FFI_FN + 2;
const usz JIT_FALLBACK_TAG_UNKNOWN = JIT_FALLBACK_TAG_BUCKETS - 1;

struct JitFallbackAuditStats {
    usz total;
    usz[JIT_FALLBACK_TAG_BUCKETS] by_tag;
    ExprTag last_tag;
}

enum JitExprFamily : char {
    JIT_EXPR_FAMILY_CORE,
    JIT_EXPR_FAMILY_EFFECTS,
    JIT_EXPR_FAMILY_TYPES_FFI,
    JIT_EXPR_FAMILY_INVALID,
}

const usz JIT_EXPR_FAMILY_COUNT =
    (usz)JitExprFamily.JIT_EXPR_FAMILY_INVALID + 1;

const char[][] JIT_EXPR_FAMILY_NAMES = {
    "core",
    "effects",
    "types_ffi",
    "invalid",
};

$assert(JIT_EXPR_FAMILY_NAMES.len == JIT_EXPR_FAMILY_COUNT);

JitFallbackAuditStats g_jit_fallback_audit_stats;

fn usz jit_fallback_bucket_for_tag(ExprTag tag) @inline {
    isz raw = (isz)tag;
    if (raw < 0) return JIT_FALLBACK_TAG_UNKNOWN;
    usz idx = (usz)raw;
    if (idx > (usz)ExprTag.E_FFI_FN) return JIT_FALLBACK_TAG_UNKNOWN;
    return idx;
}

fn void jit_fallback_audit_note(Expr* expr) @inline {
    g_jit_fallback_audit_stats.total++;
    if (expr == null) {
        g_jit_fallback_audit_stats.by_tag[JIT_FALLBACK_TAG_UNKNOWN]++;
        return;
    }
    usz bucket = jit_fallback_bucket_for_tag(expr.tag);
    g_jit_fallback_audit_stats.by_tag[bucket]++;
    g_jit_fallback_audit_stats.last_tag = expr.tag;
}

fn void jit_fallback_audit_reset() {
    g_jit_fallback_audit_stats = {};
}

fn usz jit_fallback_audit_total() @inline {
    return g_jit_fallback_audit_stats.total;
}

fn usz jit_fallback_audit_tag_count(ExprTag tag) @inline {
    usz bucket = jit_fallback_bucket_for_tag(tag);
    return g_jit_fallback_audit_stats.by_tag[bucket];
}

fn JitFallbackAuditStats jit_fallback_audit_snapshot() @inline {
    return g_jit_fallback_audit_stats;
}

fn JitExprFamily jit_expr_family_for_tag(ExprTag tag) @inline {
    switch (tag) {
        case E_LIT:
        case E_VAR:
        case E_LAMBDA:
        case E_APP:
        case E_IF:
        case E_LET:
        case E_DEFINE:
        case E_AND:
        case E_OR:
        case E_CALL:
        case E_BLOCK:
        case E_SET:
            return JitExprFamily.JIT_EXPR_FAMILY_CORE;
        case E_QUOTE:
        case E_RESET:
        case E_SHIFT:
        case E_PERFORM:
        case E_HANDLE:
        case E_RESOLVE:
        case E_INDEX:
        case E_PATH:
        case E_MATCH:
        case E_QUASIQUOTE:
        case E_DEFMACRO:
        case E_MODULE:
        case E_IMPORT:
            return JitExprFamily.JIT_EXPR_FAMILY_EFFECTS;
        case E_UNQUOTE:
        case E_UNQUOTE_SPLICING:
        case E_DEFTYPE:
        case E_DEFABSTRACT:
        case E_DEFUNION:
        case E_DEFALIAS:
        case E_DEFEFFECT:
        case E_EXPORT_FROM:
        case E_FFI_LIB:
        case E_FFI_FN:
            return JitExprFamily.JIT_EXPR_FAMILY_TYPES_FFI;
    }
}

// JIT expression lowering: core expression forms and call/app lowering.
fn void? jit_compile_expr_group_core(void* s, Expr* expr, Interp* interp,
    JitLocals* locals, bool is_tail, bool* handled) {
    *handled = true;
    switch (expr.tag) {
        case E_LIT:
            jit_compile_lit(s, expr, interp, locals, is_tail)!;
        case E_VAR:
            jit_compile_var(s, expr, interp, locals)!;
        case E_IF:
            jit_compile_if(s, expr, interp, locals, is_tail)!;
        case E_AND:
            jit_compile_and(s, expr, interp, locals, is_tail)!;
        case E_OR:
            jit_compile_or(s, expr, interp, locals, is_tail)!;
        case E_BLOCK:
            jit_compile_block(s, expr, interp, locals, is_tail)!;
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
            jit_compile_quote(s, expr, interp, locals)!;
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
        emit_call_1(s, locals, (void*)&jit_make_nil);  // R0 = make_nil(V0=interp)
        return;
    }

    bool handled = false;
    switch (jit_expr_family_for_tag(expr.tag)) {
        case JitExprFamily.JIT_EXPR_FAMILY_CORE:
            jit_compile_expr_group_core(s, expr, interp, locals, is_tail, &handled)!;
        case JitExprFamily.JIT_EXPR_FAMILY_EFFECTS:
            jit_compile_expr_group_effects(s, expr, interp, locals, is_tail, &handled)!;
        case JitExprFamily.JIT_EXPR_FAMILY_TYPES_FFI:
            jit_compile_expr_group_types_ffi(s, expr, interp, locals, &handled)!;
        case JitExprFamily.JIT_EXPR_FAMILY_INVALID:
            handled = false;
    }
    if (handled) return;

    jit_fallback_audit_note(expr);
    jit_compile_fallback(s, expr, interp, locals)!;
}
