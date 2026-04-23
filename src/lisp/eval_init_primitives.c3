module lisp;

import main;
import pika;

struct FastPathReg {
    char[] effect_name;
    char[] raw_prim_name;
}

fn void register_effect_fast_paths(Interp* interp) {
    const FAST_PATH_COUNT = 56;
    FastPathReg[FAST_PATH_COUNT] fast_paths = {
        { "io/print",      "__raw-print" },
        { "io/println",     "__raw-println" },
        { "io/display",     "__raw-display" },
        { "io/newline",     "__raw-newline" },
        { "io/read-line",   "__raw-read-line" },
        { "io/read-file",   "__raw-read-file" },
        { "io/write-file",  "__raw-write-file" },
        { "io/file-exists?","__raw-file-exists?" },
        { "io/read-lines",  "__raw-read-lines" },
        { "io/fs-open",     "__raw-fs-open" },
        { "io/fs-read",     "__raw-fs-read" },
        { "io/fs-write",    "__raw-fs-write" },
        { "io/fs-close",    "__raw-fs-close" },
        { "io/fs-stat",     "__raw-fs-stat" },
        { "io/fs-readdir",  "__raw-fs-readdir" },
        { "io/fs-rename",   "__raw-fs-rename" },
        { "io/fs-unlink",   "__raw-fs-unlink" },
        { "io/tcp-connect", "__raw-tcp-connect" },
        { "io/tcp-listen",  "__raw-tcp-listen" },
        { "io/tcp-accept",  "__raw-tcp-accept" },
        { "io/tcp-read",    "__raw-tcp-read" },
        { "io/tcp-write",   "__raw-tcp-write" },
        { "io/tcp-close",   "__raw-tcp-close" },
        { "io/udp-socket",  "__raw-udp-socket" },
        { "io/udp-bind",    "__raw-udp-bind" },
        { "io/udp-send",    "__raw-udp-send" },
        { "io/udp-recv",    "__raw-udp-recv" },
        { "io/udp-close",   "__raw-udp-close" },
        { "io/pipe-connect","__raw-pipe-connect" },
        { "io/pipe-listen", "__raw-pipe-listen" },
        { "io/process-spawn","__raw-process-spawn" },
        { "io/process-wait", "__raw-process-wait" },
        { "io/process-kill", "__raw-process-kill" },
        { "io/signal-handle", "__raw-signal-handle" },
        { "io/signal-unhandle", "__raw-signal-unhandle" },
        { "io/dns-resolve", "__raw-dns-resolve" },
        { "io/async-sleep", "__raw-async-sleep" },
        { "io/offload",     "__raw-offload" },
        { "io/offload-batch", "__raw-offload-batch" },
        { "io/thread-spawn","__raw-thread-spawn" },
        { "io/thread-spawn-batch","__raw-thread-spawn-batch" },
        { "io/thread-join", "__raw-thread-join" },
        { "io/thread-join-timeout", "__raw-thread-join-timeout" },
        { "io/thread-cancel", "__raw-thread-cancel" },
        { "io/task-spawn","__raw-task-spawn" },
        { "io/task-spawn-batch","__raw-task-spawn-batch" },
        { "io/task-join", "__raw-task-join" },
        { "io/task-join-timeout", "__raw-task-join-timeout" },
        { "io/task-cancel", "__raw-task-cancel" },
        { "io/tls-connect", "__raw-tls-connect" },
        { "io/tls-server-wrap", "__raw-tls-server-wrap" },
        { "io/tls-read",    "__raw-tls-read" },
        { "io/tls-write",   "__raw-tls-write" },
        { "io/tls-close",   "__raw-tls-close" },
        { "io/http-get",    "__raw-http-get" },
        { "io/http-request","__raw-http-request" },
    };
    $assert(fast_paths.len == FAST_PATH_COUNT);

    interp.fast_path_count = 0;
    foreach (&fp : fast_paths) {
        register_fast_path(interp, fp.effect_name, fp.raw_prim_name);
    }
}

/**
 * Register all primitive functions in the interpreter.
 * Uses data-driven tables to separate declaration from registration logic.
 */
fn bool register_primitives(Interp* interp) {
    if (!register_language_constants(interp)) return false;
    if (!register_dispatched_primitives(interp)) return false;
    if (!register_regular_primitives(interp)) return false;
    if (!register_math_stats_core_modules(interp)) return false;
    if (!register_builtin_types(interp)) return false;
    if (!pika::register_pika_primitives(interp)) return false;
    register_effect_fast_paths(interp);
    return true;
}

fn void register_fast_path(Interp* interp, char[] effect_name, char[] raw_prim_name) {
    SymbolId prim_sym = interp.symbols.intern(raw_prim_name);
    SymbolId effect_sym = interp.symbols.intern(effect_name);
    if (prim_sym == INVALID_SYMBOL_ID || effect_sym == INVALID_SYMBOL_ID) return;
    Value* prim = interp.global_env.lookup(prim_sym);
    if (prim != null && interp.fast_path_count < interp.fast_path_table.len) {
        interp.fast_path_table[interp.fast_path_count].tag = effect_sym;
        interp.fast_path_table[interp.fast_path_count].prim = prim;
        interp.fast_path_count++;
    }
}

fn bool register_prim(Interp* interp, char[] name, PrimitiveFn func, int arity) {
    SymbolId sym = interp.symbols.intern(name);
    if (sym == INVALID_SYMBOL_ID) return false;
    Value* prim = make_primitive(interp, name, func, arity);
    if (prim == null) return false;
    return env_define_with_barrier_checked(interp.global_env, sym, prim, interp);
}

fn bool register_dispatched_prim(Interp* interp, char[] name, PrimitiveFn func, int arity) {
    SymbolId sym = interp.symbols.intern(name);
    if (sym == INVALID_SYMBOL_ID) return false;
    Value* prim = make_primitive(interp, name, func, arity);
    if (prim == null) return false;

    // Create method table with no typed entries, primitive as fallback
    MethodTable* mt = alloc_empty_method_table_bootstrap(sym, prim);
    if (mt == null) return false;

    Value* mt_val = boundary_alloc_value_in_root(interp, true);
    if (mt_val == null) {
        free_empty_method_table_bootstrap(mt);
        return false;
    }
    mt_val.tag = METHOD_TABLE;
    mt_val.method_table_val = mt;

    if (!env_define_with_barrier_checked(interp.global_env, sym, mt_val, interp)) {
        free_empty_method_table_bootstrap(mt);
        mt_val.method_table_val = null;
        return false;
    }
    return true;
}
