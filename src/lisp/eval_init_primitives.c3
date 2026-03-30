module lisp;

import main;
import pika;

// Primitive registration table entry
struct PrimReg {
    char[] name;
    PrimitiveFn func;
    int arity;
}

struct FastPathReg {
    char[] effect_name;
    char[] raw_prim_name;
}

fn bool register_prim_table(Interp* interp, PrimReg[] table) {
    foreach (&r : table) {
        if (!register_prim(interp, r.name, r.func, r.arity)) return false;
    }
    return true;
}

fn bool register_dispatched_prim_table(Interp* interp, PrimReg[] table) {
    foreach (&r : table) {
        if (!register_dispatched_prim(interp, r.name, r.func, r.arity)) return false;
    }
    return true;
}

fn bool register_language_constants(Interp* interp) {
    Value* true_val = make_symbol(interp, interp.sym_true);
    Value* false_val = make_nil(interp);  // false is nil
    Value* nil_val = make_nil(interp);
    if (!env_define_with_barrier_checked(interp.global_env, interp.sym_true, true_val, interp)) return false;
    if (!env_define_with_barrier_checked(interp.global_env, interp.sym_false, false_val, interp)) return false;
    SymbolId sym_nil = interp.symbols.intern("nil");
    return env_define_with_barrier_checked(interp.global_env, sym_nil, nil_val, interp);
}

fn bool register_dispatched_primitives(Interp* interp) {
    const DISPATCHED_PRIM_COUNT = 43;
    PrimReg[DISPATCHED_PRIM_COUNT] dispatched_prims = {
        // Arithmetic
        { "+", &prim_add, 2 }, { "-", &prim_sub, 2 }, { "*", &prim_mul, 2 },
        { "/", &prim_div, 2 }, { "%", &prim_mod, 2 },
        // Comparison
        { "=", &prim_eq, 2 }, { "<", &prim_lt, 2 }, { ">", &prim_gt, 2 },
        { "<=", &prim_le, 2 }, { ">=", &prim_ge, 2 },
        // Collections
        { "length", &prim_length, 1 }, { "ref", &prim_ref, 2 },
        { "set!", &prim_set_bang, 3 },
        { "push!", &prim_push, 2 }, { "keys", &prim_keys, 1 },
        { "values", &prim_values, 1 }, { "has?", &prim_has, 2 },
        { "remove!", &prim_remove, 2 },
        // Sorting
        { "sort", &prim_sort, 1 }, { "sort-by", &prim_sort_by, 2 },
        // String ops (extensible for user-defined string-like types)
        { "string-append", &prim_string_append, -1 },
        { "string-contains?", &prim_string_contains, 2 },
        { "string-upcase", &prim_string_upcase_unicode, 1 },
        { "string-downcase", &prim_string_downcase_unicode, 1 },
        { "string-casefold", &prim_string_casefold_unicode, 1 },
        { "string-titlecase", &prim_string_titlecase_unicode, 1 },
        // Math (extensible for user-defined numeric types)
        { "abs", &prim_abs, 1 }, { "floor", &prim_floor, 1 },
        { "ceiling", &prim_ceiling, 1 }, { "round", &prim_round, 1 },
        { "truncate", &prim_truncate, 1 }, { "sqrt", &prim_sqrt, 1 },
        { "min", &prim_min, 2 }, { "max", &prim_max, 2 },
        // Callable type symbols / constructors
        { "Integer", &prim_ctor_int, 1 },
        { "Double", &prim_ctor_double, 1 },
        { "String", &prim_ctor_string, 1 }, { "Symbol", &prim_ctor_symbol, 1 },
        { "Boolean", &prim_ctor_bool, 1 },
        { "Nil", &prim_ctor_nil, 1 }, { "Void", &prim_ctor_void, -1 },
        { "Closure", &prim_ctor_closure, 1 }, { "Iterator", &prim_ctor_iterator, 1 },
    };
    $assert(dispatched_prims.len == DISPATCHED_PRIM_COUNT);
    return register_dispatched_prim_table(interp, dispatched_prims[0:dispatched_prims.len]);
}

fn bool register_regular_primitives_core(Interp* interp) {
    PrimReg[] core_prims = {
        // List operations
        { "cons", &prim_cons, 2 }, { "car", &prim_car, 1 }, { "cdr", &prim_cdr, 1 },
        { "null?", &prim_null, 1 }, { "pair?", &prim_pair, 1 },
        { "List", &prim_list, -1 }, // Canonical constructor/conversion surface.
        { "list", &prim_list, -1 }, // Public Lisp-style helper.
        // Boolean
        { "not", &prim_not, 1 },
        // I/O (raw primitives for effect fast path)
        { "__raw-print", &prim_print, 1 }, { "__raw-println", &prim_println, 1 },
        { "__raw-newline", &prim_newline, 0 }, { "__raw-display", &prim_display, 1 },
        { "__raw-read-line", &prim_read_line, 0 },
        { "__ui-ftxui-run", &prim_ui_ftxui_run, 1 },
        // String primitives
        { "string-join", &prim_string_join, 2 },
        { "substring", &prim_substring, 3 }, { "string-split", &prim_string_split, 2 },
        { "string-length", &prim_string_length, 1 }, { "string-byte-length", &prim_string_byte_length, 1 },
        { "string->list", &prim_string_to_list, 1 },
        { "list->string", &prim_list_to_string, 1 },
        { "string-trim", &prim_string_trim, 1 },
        { "continuation?", &prim_is_continuation, 1 },
        { "__iterator-from-list", &prim_iterator_from_list, 1 },
        { "__iterator-from-array", &prim_iterator_from_array, 1 },
        { "__iterator-from-dict", &prim_iterator_from_dict, 1 },
        { "__iterator-map", &prim_iterator_map, 2 },
        { "__iterator-filter", &prim_iterator_filter, 2 },
        { "__iterator-take", &prim_iterator_take, 2 },
        { "__iterator-zip", &prim_iterator_zip, 2 },
        { "__iterator-foldl", &prim_iterator_foldl, 3 },
        { "__iterator-range-from", &prim_iterator_range_from, 1 },
        { "__iterator-repeat", &prim_iterator_repeat, 1 },
        { "__iterator-cycle", &prim_iterator_cycle, 1 },
        { "__reverse-list", &prim_reverse_list, 1 },
        // File I/O
        { "__raw-read-file", &prim_read_file, 1 }, { "__raw-write-file", &prim_write_file, 2 },
        { "__raw-file-exists?", &prim_file_exists, 1 }, { "__raw-read-lines", &prim_read_lines, 1 },
        { "__raw-fs-open", &prim_fs_open, -1 }, { "__raw-fs-read", &prim_fs_read, 2 },
        { "__raw-fs-write", &prim_fs_write, 2 }, { "__raw-fs-close", &prim_fs_close, 1 },
        { "__raw-fs-stat", &prim_fs_stat, 1 }, { "__raw-fs-readdir", &prim_fs_readdir, 1 },
        { "__raw-fs-rename", &prim_fs_rename, 2 }, { "__raw-fs-unlink", &prim_fs_unlink, 1 },
        { "fs-open", &prim_fs_open, -1 }, { "fs-read", &prim_fs_read, 2 },
        { "fs-write", &prim_fs_write, 2 }, { "fs-close", &prim_fs_close, 1 },
        { "fs-stat", &prim_fs_stat, 1 }, { "fs-readdir", &prim_fs_readdir, 1 },
        { "fs-rename", &prim_fs_rename, 2 }, { "fs-unlink", &prim_fs_unlink, 1 },
        { "load", &prim_load, 1 },
        // Conversion
        { "string->number", &prim_string_to_number, 1 }, { "number->string", &prim_number_to_string, 1 },
        { "exact->inexact", &prim_exact_to_inexact, 1 }, { "inexact->exact", &prim_inexact_to_exact, 1 },
        // Gensym
        { "gensym", &prim_gensym, 0 },
        // Dictionary
        { "Dictionary", &prim_dict, -1 }, // Canonical constructor.
        // Math library
        { "sin", &prim_sin, 1 }, { "cos", &prim_cos, 1 }, { "tan", &prim_tan, 1 },
        { "asin", &prim_asin, 1 }, { "acos", &prim_acos, 1 }, { "atan", &prim_atan, 1 },
        { "atan2", &prim_atan2, 2 }, { "exp", &prim_exp, 1 }, { "log", &prim_log, 1 },
        { "log10", &prim_log10, 1 }, { "pow", &prim_pow, 2 },
        { "gcd", &prim_gcd, 2 }, { "lcm", &prim_lcm, 2 },
        // Bitwise
        { "bitwise-and", &prim_bitwise_and, 2 }, { "bitwise-or", &prim_bitwise_or, 2 },
        { "bitwise-xor", &prim_bitwise_xor, 2 }, { "bitwise-not", &prim_bitwise_not, 1 },
        { "lshift", &prim_lshift, 2 }, { "rshift", &prim_rshift, 2 },
        // String operations & predicates
        { "string-index-of", &prim_string_index_of, 2 },
        { "string-replace", &prim_string_replace, 3 }, { "char-at", &prim_char_at, 2 },
        { "string-repeat", &prim_string_repeat, 2 }, { "procedure?", &prim_is_procedure, 1 },
        { "format", &prim_format, -1 }, { "cl-format", &prim_cl_format, -1 },
    };
    return register_prim_table(interp, core_prims[0:core_prims.len]);
}

fn bool register_regular_primitives_collections(Interp* interp) {
    PrimReg[] collection_prims = {
        // Introspection & metaprogramming
        { "macroexpand", &prim_macroexpand, 1 }, { "eval", &prim_eval, 1 },
        { "apply", &prim_apply, 2 }, { "bound?", &prim_bound, 1 },
        { "error", &prim_error, 1 }, { "error-message", &prim_error_message, 1 },
        // Arrays
        { "Array", &prim_array, -1 }, // Canonical constructor/conversion surface.
        // Sets
        { "Set", &prim_set, -1 },
        { "set-add", &prim_set_add, 2 },
        { "set-remove", &prim_set_remove, 2 }, { "set-contains?", &prim_set_contains, 2 },
        { "set-size", &prim_set_size, 1 }, { "set->list", &prim_set_to_list, 1 },
        // Additional I/O and convenience
        { "read-string", &prim_read_string, 1 }, { "string->symbol", &prim_string_to_symbol, 1 },
        { "symbol->string", &prim_symbol_to_string, 1 },
        // Type system
        { "^", &prim_ctor_type_apply, 2 },
        { "type-of", &prim_type_of, 1 },
        { "is?", &prim_is_type, 2 },
        { "instance?", &prim_is_instance, 1 }, { "type-args", &prim_type_args, 1 },
        // Memory reclamation
        { "unsafe-free!", &prim_free_bang, 1 },
        // Iterators
        { "iterator?", &prim_iterator_p, 1 }, { "__make-iterator", &prim_make_iterator, 1 }, // Internal low-level helper; prefer Iterator.
        { "next", &prim_next, 1 },
        // Coroutines
        { "Coroutine", &prim_coroutine, 1 },
        { "resume", &prim_resume, -1 },
        { "yield", &prim_yield, -1 },
        { "coroutine?", &prim_coroutine_p, 1 },
        // System / OS
        { "shell", &prim_shell, 1 },
        { "random", &prim_random, 0 },
        { "random-int", &prim_random_int, 1 },
        { "getenv", &prim_getenv, 1 },
        { "time", &prim_time, 0 },
        { "time-ms", &prim_time_ms, 0 },
        { "exit", &prim_exit, -1 },
        { "sleep", &prim_sleep, 1 },
        // Unicode
        { "string-normalize", &prim_string_normalize, 2 },
        { "string-graphemes", &prim_string_graphemes, 1 },
        { "string-codepoints", &prim_string_codepoints, 1 },
        { "char-category", &prim_char_category, 1 },
        { "char-width", &prim_char_width, 1 },
        { "char-property", &prim_char_property, 2 },
        // Compression
        { "gzip", &prim_gzip, -1 },
        { "gunzip", &prim_gunzip, 1 },
        { "deflate", &prim_deflate, -1 },
        { "inflate", &prim_inflate, -1 },
        { "zlib-compress", &prim_zlib_compress, -1 },
        { "zlib-decompress", &prim_zlib_decompress, -1 },
        { "adler32", &prim_adler32, 1 },
        { "crc32", &prim_crc32, 1 },
        // JSON
        { "json-parse", &prim_json_parse, -1 },
        { "json-emit", &prim_json_emit, -1 },
        { "json-emit-pretty", &prim_json_emit_pretty, -1 },
        { "json-get", &prim_json_get, 2 },
        { "time-point?", &prim_time_point_p, 1 },
        { "TimePoint", &prim_time_point, -1 }, // Canonical constructor.
        // TOML / CSV
        { "toml-parse", &prim_toml_parse, 1 },
        { "csv-parse", &prim_csv_parse, -1 },
        { "csv-emit", &prim_csv_emit, -1 },
    };
    return register_prim_table(interp, collection_prims[0:collection_prims.len]);
}

fn bool register_regular_primitives_io_and_deduce(Interp* interp) {
    PrimReg[] io_deduce_prims = {
        // Networking / async I/O (raw primitives for effect fast path)
        { "__raw-tcp-connect", &prim_tcp_connect, 2 },
        { "__raw-tcp-listen", &prim_tcp_listen, -1 },
        { "__raw-tcp-accept", &prim_tcp_accept, 1 },
        { "__raw-tcp-read", &prim_tcp_read, -1 },
        { "__raw-tcp-write", &prim_tcp_write, 2 },
        { "__raw-tcp-close", &prim_tcp_close, 1 },
        { "__raw-udp-socket", &prim_udp_socket, 0 },
        { "__raw-udp-bind", &prim_udp_bind, -1 },
        { "__raw-udp-send", &prim_udp_send, -1 },
        { "__raw-udp-recv", &prim_udp_recv, -1 },
        { "__raw-udp-close", &prim_udp_close, 1 },
        { "__raw-pipe-connect", &prim_pipe_connect, 1 },
        { "__raw-pipe-listen", &prim_pipe_listen, 1 },
        { "__raw-process-spawn", &prim_process_spawn, -1 },
        { "__raw-process-wait", &prim_process_wait, 1 },
        { "__raw-process-kill", &prim_process_kill, -1 },
        { "__raw-signal-handle", &prim_signal_handle, -1 },
        { "__raw-signal-unhandle", &prim_signal_unhandle, 1 },
        { "__raw-dns-resolve", &prim_dns_resolve, 1 },
        { "__raw-async-sleep", &prim_async_sleep, 1 },
        { "__raw-offload", &prim_offload, 1 },
        { "__raw-offload-batch", &prim_offload_batch, 1 },
        { "__raw-thread-spawn", &prim_thread_spawn, 1 },
        { "__raw-thread-spawn-batch", &prim_thread_spawn_batch, 1 },
        { "__raw-thread-join", &prim_thread_join, 1 },
        { "__raw-thread-join-timeout", &prim_thread_join_timeout, 1 },
        { "__raw-thread-cancel", &prim_thread_cancel, 1 },
        { "__raw-task-spawn", &prim_task_spawn, 1 },
        { "__raw-task-spawn-batch", &prim_task_spawn_batch, 1 },
        { "__raw-task-join", &prim_task_join, 1 },
        { "__raw-task-join-timeout", &prim_task_join_timeout, 1 },
        { "__raw-task-cancel", &prim_task_cancel, 1 },
        { "fiber-cancel", &prim_fiber_cancel, 1 },
        // TLS
        { "__raw-tls-connect", &prim_tls_connect, -1 },
        { "__raw-tls-server-wrap", &prim_tls_server_wrap, -1 },
        { "__raw-tls-read", &prim_tls_read, -1 },
        { "__raw-tls-write", &prim_tls_write, 2 },
        { "__raw-tls-close", &prim_tls_close, 1 },
        // Schema validation
        { "validate", &prim_validate, 2 },
        { "explain", &prim_explain, 2 },
        { "schema-explain", &prim_schema_explain, 2 },
        // Deduce — unified dispatch + namespaced command aliases
        { "deduce", &prim_deduce, -1 },
        { "deduce/open", &prim_deduce_ns_open, -1 },
        { "deduce/open-named", &prim_deduce_ns_open_named, -1 },
        { "deduce/block", &prim_deduce_ns_block, -1 },
        { "deduce/commit", &prim_deduce_ns_commit, -1 },
        { "deduce/abort", &prim_deduce_ns_abort, -1 },
        { "deduce/scan", &prim_deduce_ns_scan, -1 },
        { "deduce/scan-range", &prim_deduce_ns_scan_range, -1 },
        { "deduce/query", &prim_deduce_ns_query, -1 },
        { "deduce/schema", &prim_deduce_ns_schema, -1 },
        { "deduce/indexes", &prim_deduce_ns_indexes, -1 },
        { "deduce/stats", &prim_deduce_ns_stats, -1 },
        { "deduce/count", &prim_deduce_ns_count, -1 },
        { "deduce/match", &prim_deduce_ns_match, -1 },
        { "deduce/fact!", &prim_deduce_ns_fact, -1 },
        // Canonical cleanup/admin verbs: retract! (tuple), clear! (contents), drop! (lifecycle).
        { "deduce/retract!", &prim_deduce_ns_retract, -1 },
        { "deduce/clear!", &prim_deduce_ns_clear, -1 },
        { "deduce/drop!", &prim_deduce_ns_drop, -1 },
        { "deduce/rule!", &prim_deduce_ns_rule, -1 },
        { "deduce/explain", &prim_deduce_ns_explain, -1 },
        { "deduce/why-result", &prim_deduce_ns_why_result, -1 },
        { "deduce/analyze", &prim_deduce_ns_analyze, -1 },
        { "deduce/materialize!", &prim_deduce_ns_materialize, -1 },
        { "deduce/dematerialize!", &prim_deduce_ns_dematerialize, -1 },
        { "deduce/refresh!", &prim_deduce_ns_refresh, -1 },
        { "__define-relation", &prim_define_relation, 1 },
        // Scheduler
        { "spawn", &prim_spawn, 1 },
        { "await", &prim_await, 1 },
        { "run-fibers", &prim_run_fibers, 0 },
        // HTTP
        { "__raw-http-get", &prim_http_get, 1 },
        { "__raw-http-request", &prim_http_request, -1 },
        // Atomics
        { "atomic", &prim_atomic, 1 },
        { "atomic-add!", &prim_atomic_add, 2 },
        { "atomic-read", &prim_atomic_read, 1 },
        { "atomic-cas!", &prim_atomic_cas, 3 },
    };
    return register_prim_table(interp, io_deduce_prims[0:io_deduce_prims.len]);
}

fn bool register_regular_primitives(Interp* interp) {
    if (!register_regular_primitives_core(interp)) return false;
    if (!register_regular_primitives_collections(interp)) return false;
    return register_regular_primitives_io_and_deduce(interp);
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
    if (!register_builtin_types(interp)) return false;
    if (!pika::register_pika_primitives(interp)) return false;
    register_effect_fast_paths(interp);
    return true;
}

fn void register_fast_path(Interp* interp, char[] effect_name, char[] raw_prim_name) {
    Value* prim = interp.global_env.lookup(interp.symbols.intern(raw_prim_name));
    if (prim != null && interp.fast_path_count < interp.fast_path_table.len) {
        interp.fast_path_table[interp.fast_path_count].tag = interp.symbols.intern(effect_name);
        interp.fast_path_table[interp.fast_path_count].prim = prim;
        interp.fast_path_count++;
    }
}

fn bool register_prim(Interp* interp, char[] name, PrimitiveFn func, int arity) {
    SymbolId sym = interp.symbols.intern(name);
    Value* prim = make_primitive(interp, name, func, arity);
    if (prim == null) return false;
    return env_define_with_barrier_checked(interp.global_env, sym, prim, interp);
}

fn bool register_dispatched_prim(Interp* interp, char[] name, PrimitiveFn func, int arity) {
    SymbolId sym = interp.symbols.intern(name);
    Value* prim = make_primitive(interp, name, func, arity);
    if (prim == null) return false;

    // Create method table with no typed entries, primitive as fallback
    MethodTable* mt = alloc_empty_method_table_bootstrap(sym, prim);
    if (mt == null) return false;

    Value* mt_val = boundary_alloc_value_in_root(interp, true);
    mt_val.tag = METHOD_TABLE;
    mt_val.method_table_val = mt;

    return env_define_with_barrier_checked(interp.global_env, sym, mt_val, interp);
}
