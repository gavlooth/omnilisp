module lisp;


import std::io;
import std::collections::list;
import main;

const uint COPY_SITE_GENERIC = 0;
const uint COPY_SITE_PROMOTE_TO_ROOT = 1;
const uint COPY_SITE_COPY_ENV_TO_SCOPE = 2;
const uint COPY_SITE_RUN_JIT_RESULT = 3;
const uint COPY_SITE_REPL_LINE_RESULT = 4;
const uint COPY_SITE_PRIM_RESUME = 5;
const uint COPY_SITE_CONS_BARRIER_CAR = 6;
const uint COPY_SITE_CONS_BARRIER_CDR = 7;
const uint COPY_SITE_ENV_PROMOTE_FOR_TARGET = 8;
const uint COPY_SITE_JIT_SINGLE_SCOPE_RETURN = 9;
const uint COPY_SITE_JIT_CALL_SCOPE_STEP2 = 10;
const uint COPY_SITE_JIT_TCO_FRAME_COPY = 11;
const uint COPY_SITE_JIT_EVAL_SET = 12;
const uint COPY_SITE_JIT_EVAL_DEFINE = 13;
const uint COPY_SITE_JIT_ENV_EXTEND_ROOT = 14;
const uint COPY_SITE_COUNT = 15;
// Return-boundary promotion budget:
// raised to reduce pre-aborted finalize contexts on deep unwind/large graph
// returns while preserving explicit boundary provenance contracts.
const usz PROMOTION_CTX_BUDGET_RETURN = 16384;
const usz PROMOTION_CTX_BUDGET_TCO = 2048;

struct CopyToParentStats {
    usz total_calls;
    usz fast_reuse_hits;
    usz defensive_copies;

    usz nil_calls;
    usz int_calls;
    usz double_calls;
    usz string_calls;
    usz symbol_calls;
    usz cons_calls;
    usz closure_calls;
    usz other_calls;
    usz cons_spine_copy_samples;
    usz cons_spine_copy_total_len;
    usz cons_spine_copy_peak_len;

    usz[COPY_SITE_COUNT] site_calls;
}

CopyToParentStats g_copy_to_parent_stats;

fn bool promotion_stats_quiet_requested() @inline {
    ZString quiet = main::getenv("OMNI_TEST_QUIET");
    if (quiet != null && quiet[0] != 0) return true;
    ZString summary = main::getenv("OMNI_TEST_SUMMARY");
    if (summary == null || summary[0] == 0) return false;
    ZString verbose = main::getenv("OMNI_TEST_VERBOSE");
    return verbose == null || verbose[0] == 0;
}

fn void copy_to_parent_note_tag(ValueTag tag) @inline {
    switch (tag) {
        case NIL:
            g_copy_to_parent_stats.nil_calls++;
        case INT:
            g_copy_to_parent_stats.int_calls++;
        case DOUBLE:
            g_copy_to_parent_stats.double_calls++;
        case FLOAT32:
            g_copy_to_parent_stats.double_calls++;
        case STRING:
            g_copy_to_parent_stats.string_calls++;
        case SYMBOL:
            g_copy_to_parent_stats.symbol_calls++;
        case CONS:
            g_copy_to_parent_stats.cons_calls++;
        case CLOSURE:
            g_copy_to_parent_stats.closure_calls++;
        default:
            g_copy_to_parent_stats.other_calls++;
    }
}

fn usz copy_to_parent_cons_spine_len(Value* v) @inline {
    usz len = 0;
    Value* cur = v;
    while (cur != null && cur.tag == CONS) {
        len++;
        cur = cur.cons_val.cdr;
    }
    return len;
}

fn void copy_to_parent_note_cons_spine(Value* v) @inline {
    if (v == null || v.tag != CONS) return;
    usz len = copy_to_parent_cons_spine_len(v);
    g_copy_to_parent_stats.cons_spine_copy_samples++;
    g_copy_to_parent_stats.cons_spine_copy_total_len += len;
    if (len > g_copy_to_parent_stats.cons_spine_copy_peak_len) {
        g_copy_to_parent_stats.cons_spine_copy_peak_len = len;
    }
}

fn void print_memory_promotion_stats() {
    if (promotion_stats_quiet_requested()) return;
    io::printn("=== copy_to_parent Stats ===");
    io::printfn("copy_to_parent calls: %d", (long)g_copy_to_parent_stats.total_calls);
    io::printfn("copy_to_parent fast reuse hits: %d", (long)g_copy_to_parent_stats.fast_reuse_hits);
    io::printfn("copy_to_parent defensive copies: %d", (long)g_copy_to_parent_stats.defensive_copies);
    io::printfn("copy_to_parent by tag: nil=%d int=%d double=%d string=%d symbol=%d cons=%d closure=%d other=%d",
        (long)g_copy_to_parent_stats.nil_calls,
        (long)g_copy_to_parent_stats.int_calls,
        (long)g_copy_to_parent_stats.double_calls,
        (long)g_copy_to_parent_stats.string_calls,
        (long)g_copy_to_parent_stats.symbol_calls,
        (long)g_copy_to_parent_stats.cons_calls,
        (long)g_copy_to_parent_stats.closure_calls,
        (long)g_copy_to_parent_stats.other_calls);
    io::printfn("copy_to_parent by site: promote_to_root=%d copy_env=%d run_jit=%d repl=%d prim_resume=%d",
        (long)g_copy_to_parent_stats.site_calls[COPY_SITE_PROMOTE_TO_ROOT],
        (long)g_copy_to_parent_stats.site_calls[COPY_SITE_COPY_ENV_TO_SCOPE],
        (long)g_copy_to_parent_stats.site_calls[COPY_SITE_RUN_JIT_RESULT],
        (long)g_copy_to_parent_stats.site_calls[COPY_SITE_REPL_LINE_RESULT],
        (long)g_copy_to_parent_stats.site_calls[COPY_SITE_PRIM_RESUME]);
    io::printfn("copy_to_parent by site: cons_barrier_car=%d cons_barrier_cdr=%d env_promote=%d",
        (long)g_copy_to_parent_stats.site_calls[COPY_SITE_CONS_BARRIER_CAR],
        (long)g_copy_to_parent_stats.site_calls[COPY_SITE_CONS_BARRIER_CDR],
        (long)g_copy_to_parent_stats.site_calls[COPY_SITE_ENV_PROMOTE_FOR_TARGET]);
    io::printfn("copy_to_parent by site: jit_single=%d jit_call_step2=%d",
        (long)g_copy_to_parent_stats.site_calls[COPY_SITE_JIT_SINGLE_SCOPE_RETURN],
        (long)g_copy_to_parent_stats.site_calls[COPY_SITE_JIT_CALL_SCOPE_STEP2]);
    io::printfn("copy_to_parent by site: jit_tco_frame=%d jit_set=%d jit_define=%d",
        (long)g_copy_to_parent_stats.site_calls[COPY_SITE_JIT_TCO_FRAME_COPY],
        (long)g_copy_to_parent_stats.site_calls[COPY_SITE_JIT_EVAL_SET],
        (long)g_copy_to_parent_stats.site_calls[COPY_SITE_JIT_EVAL_DEFINE]);
    io::printfn("copy_to_parent by site: jit_env_extend_root=%d",
        (long)g_copy_to_parent_stats.site_calls[COPY_SITE_JIT_ENV_EXTEND_ROOT]);
    io::printfn("copy_to_parent cons-spine copied: samples=%d total_len=%d peak_len=%d",
        (long)g_copy_to_parent_stats.cons_spine_copy_samples,
        (long)g_copy_to_parent_stats.cons_spine_copy_total_len,
        (long)g_copy_to_parent_stats.cons_spine_copy_peak_len);
}

fn void reset_memory_promotion_stats() {
    g_copy_to_parent_stats = {};
}

fn usz copy_to_parent_total_calls() @inline {
    return g_copy_to_parent_stats.total_calls;
}

fn usz copy_to_parent_site_calls(uint site) @inline {
    if (site >= COPY_SITE_COUNT) return 0;
    return g_copy_to_parent_stats.site_calls[(usz)site];
}

fn CopyToParentStats copy_to_parent_stats_snapshot() @inline {
    return g_copy_to_parent_stats;
}

// =============================================================================
// SECTION 1: EVALUATOR CORE
// =============================================================================

/**
 * EvalError - Error result from evaluation.
 */
struct EvalError {
    bool        has_error;
    char[256]   message;
    usz         line;     // Source line where error occurred (0 if unknown)
    usz         column;   // Source column where error occurred (0 if unknown)
}

/**
 * EvalResult - Result of evaluation (value or error).
 */
struct EvalResult {
    Value*     value;
    EvalError  error;
}

fn EvalResult eval_ok(Value* v) @inline {
    return { .value = v, .error = { .has_error = false } };
}

fn EvalResult eval_error(char[] msg) {
    EvalResult r;
    r.value = null;
    r.error.has_error = true;
    r.error.line = 0;
    r.error.column = 0;
    usz len = msg.len;
    if (len > 255) len = 255;
    for (usz i = 0; i < len; i++) {
        r.error.message[i] = msg[i];
    }
    r.error.message[len] = 0;
    return r;
}

/**
 * Create an error with source location from an expression.
 */
fn EvalResult eval_error_expr(char[] msg, Expr* expr) {
    EvalResult r;
    r.value = null;
    r.error.has_error = true;
    if (expr != null) {
        r.error.line = expr.loc_line;
        r.error.column = expr.loc_column;
    } else {
        r.error.line = 0;
        r.error.column = 0;
    }
    usz len = msg.len;
    if (len > 255) len = 255;
    for (usz i = 0; i < len; i++) {
        r.error.message[i] = msg[i];
    }
    r.error.message[len] = 0;
    return r;
}

// Expression evaluation — all handled by JIT (jit.c3)

// =============================================================================
// SECTION 6: HELPER FUNCTIONS
// =============================================================================

/**
 * Check if a value is falsy (nil or false symbol).
 */
fn bool is_falsy(Value* v, Interp* interp) {
    if (v == null) return true;
    if (v.tag == NIL) return true;
    if (v.tag == SYMBOL && v.sym_val == interp.sym_false) return true;
    return false;
}

// =============================================================================
// SECTION 8: INTERPRETER INITIALIZATION
// =============================================================================
