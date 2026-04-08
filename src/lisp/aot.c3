module lisp::aot;

import lisp;
import main;

// =============================================================================
// AOT Wrapper Module
// =============================================================================
// This module provides a clean API for AOT-compiled code to call the JIT
// infrastructure. Generated C3 code calls aot::* functions instead of
// runtime::* functions.

tlocal lisp::Interp* g_aot_interp = null;

fn bool aot_init() {
    main::thread_registry_init();
    g_aot_interp = (lisp::Interp*)mem::malloc(lisp::Interp.sizeof);
    if (g_aot_interp == null) {
        main::thread_registry_shutdown();
        return false;
    }
    g_aot_interp.init();
    if (!lisp::bootstrap_runtime_interp(g_aot_interp)) {
        g_aot_interp.destroy();
        mem::free(g_aot_interp);
        g_aot_interp = null;
        main::thread_registry_shutdown();
        return false;
    }
    g_aot_interp.flags.jit_enabled = true;
    return true;
}

fn void aot_shutdown() {
    if (g_aot_interp != null) {
        g_aot_interp.destroy();
        mem::free(g_aot_interp);
        g_aot_interp = null;
    }
    main::thread_registry_shutdown();
}

fn lisp::Interp* aot_interp() @inline {
    return g_aot_interp;
}

fn lisp::Value* aot_eval_result_value(lisp::EvalResult r) @inline {
    if (r.error.has_error) return lisp::make_error(g_aot_interp, r.error.message[:256]);
    return r.value != null ? r.value : lisp::make_nil(g_aot_interp);
}

fn lisp::SymbolId aot_symbol_from_text(char[] text) @inline {
    if (text.len == 0) return (lisp::SymbolId)0;
    return g_aot_interp.symbols.intern(text);
}


// =============================================================================
// Value Constructors
// =============================================================================

fn lisp::Value* make_nil() @inline { return lisp::make_nil(g_aot_interp); }
fn lisp::Value* make_void() @inline { return lisp::make_void(g_aot_interp); }
fn lisp::Value* make_int(long n) @inline { return lisp::make_int(g_aot_interp, n); }
fn lisp::Value* make_double(double d) @inline { return lisp::make_double(g_aot_interp, d); }
fn lisp::Value* make_string(char[] s) @inline { return lisp::make_string(g_aot_interp, s); }
fn lisp::Value* make_error(char[] message) @inline { return lisp::make_error(g_aot_interp, message); }
fn lisp::Value* make_symbol(char[] s) @inline {
    lisp::SymbolId sid = g_aot_interp.symbols.intern(s);
    return lisp::make_symbol(g_aot_interp, sid);
}
fn lisp::Value* make_time_point(int kind, int year, int month, int day, int hour, int minute, int second, int microsecond, int tz_offset) @inline {
    return lisp::make_time_point(g_aot_interp, (lisp::TimePointKind)kind, year, month, day, hour, minute, second, microsecond, tz_offset);
}
fn lisp::Value* make_true() @inline { return lisp::make_symbol(g_aot_interp, g_aot_interp.sym_true); }
fn lisp::Value* make_false() @inline { return lisp::make_symbol(g_aot_interp, g_aot_interp.sym_false); }

// =============================================================================
// List Operations
// =============================================================================

fn lisp::Value* cons(lisp::Value* car, lisp::Value* cdr) @inline { return lisp::make_cons(g_aot_interp, car, cdr); }
fn lisp::Value* car(lisp::Value* v) @inline {
    if (v == null || v.tag != lisp::ValueTag.CONS) return lisp::make_nil(g_aot_interp);
    return v.cons_val.car;
}
fn lisp::Value* cdr(lisp::Value* v) @inline {
    if (v == null || v.tag != lisp::ValueTag.CONS) return lisp::make_nil(g_aot_interp);
    return v.cons_val.cdr;
}

// =============================================================================
// Function Invocation
// =============================================================================

