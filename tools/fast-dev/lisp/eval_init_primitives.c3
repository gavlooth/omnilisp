module lisp;

import main;

// Primitive registration table entry
struct PrimReg {
    char[] name;
    PrimitiveFn func;
    int arity;
}

fn void register_prim_table(Interp* interp, PrimReg[] table) {
    foreach (&r : table) {
        register_prim(interp, r.name, r.func, r.arity);
    }
}

fn void register_dispatched_prim_table(Interp* interp, PrimReg[] table) {
    foreach (&r : table) {
        register_dispatched_prim(interp, r.name, r.func, r.arity);
    }
}

fn void register_language_constants(Interp* interp) {
    Value* true_val = make_symbol(interp, interp.sym_true);
    Value* false_val = make_nil(interp);  // false is nil
    Value* nil_val = make_nil(interp);
    env_define_with_barrier(interp.global_env, interp.sym_true, true_val, interp);
    env_define_with_barrier(interp.global_env, interp.sym_false, false_val, interp);
    SymbolId sym_nil = interp.symbols.intern("nil");
    env_define_with_barrier(interp.global_env, sym_nil, nil_val, interp);
}

fn void register_dispatched_primitives(Interp* interp) {
    const DISPATCHED_PRIM_COUNT = 45;
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
        { "Integer", &prim_ctor_int, 1 }, { "Int", &prim_ctor_int, 1 },
        { "Double", &prim_ctor_double, 1 },
        { "String", &prim_ctor_string, 1 }, { "Symbol", &prim_ctor_symbol, 1 },
        { "Boolean", &prim_ctor_bool, 1 }, { "Bool", &prim_ctor_bool, 1 },
        { "Nil", &prim_ctor_nil, 1 }, { "Void", &prim_ctor_void, -1 },
        { "Closure", &prim_ctor_closure, 1 }, { "Iterator", &prim_ctor_iterator, 1 },
    };
    $assert(dispatched_prims.len == DISPATCHED_PRIM_COUNT);
    register_dispatched_prim_table(interp, dispatched_prims[0:dispatched_prims.len]);
}

fn void register_regular_primitives_core(Interp* interp) {
    PrimReg[] core_prims = {
        // List operations
        { "cons", &prim_cons, 2 }, { "car", &prim_car, 1 }, { "cdr", &prim_cdr, 1 },
        { "null?", &prim_null, 1 }, { "pair?", &prim_pair, 1 },
        { "List", &prim_list, -1 }, // Canonical constructor/conversion surface.
        { "list", &prim_list, -1 }, // Public Lisp-style helper + compatibility alias.
        // Boolean
        { "not", &prim_not, 1 },
        // I/O (raw primitives for effect fast path)
        { "__raw-print", &prim_print, 1 }, { "__raw-println", &prim_println, 1 },
        { "__raw-newline", &prim_newline, 0 }, { "__raw-display", &prim_display, 1 },
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
        { "filesystem-open", &prim_fs_open, -1 }, { "filesystem-read", &prim_fs_read, 2 },
        { "filesystem-write", &prim_fs_write, 2 }, { "filesystem-close", &prim_fs_close, 1 },
        { "filesystem-stat", &prim_fs_stat, 1 }, { "filesystem-read-directory", &prim_fs_readdir, 1 },
        { "filesystem-rename", &prim_fs_rename, 2 }, { "filesystem-unlink", &prim_fs_unlink, 1 },
        { "load", &prim_load, 1 },
        // Conversion
        { "string->number", &prim_string_to_number, 1 }, { "number->string", &prim_number_to_string, 1 },
        { "exact->inexact", &prim_exact_to_inexact, 1 }, { "inexact->exact", &prim_inexact_to_exact, 1 },
        // Gensym
        { "gensym", &prim_gensym, 0 },
        // Dictionary
        { "Dictionary", &prim_dict, -1 }, // Canonical constructor.
        { "Dict", &prim_dict, -1 },       // Explicit shorthand alias (kept public).
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
    register_prim_table(interp, core_prims[0:core_prims.len]);
}

fn void register_regular_primitives_collections(Interp* interp) {
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
    register_prim_table(interp, collection_prims[0:collection_prims.len]);
}

fn void register_regular_primitives(Interp* interp) {
    register_regular_primitives_core(interp);
    register_regular_primitives_collections(interp);
}

/**
 * Register all primitive functions in the interpreter.
 * Uses data-driven tables to separate declaration from registration logic.
 */
fn void register_primitives(Interp* interp) {
    register_language_constants(interp);
    register_dispatched_primitives(interp);
    register_regular_primitives(interp);
    register_builtin_types(interp);
}

fn void register_prim(Interp* interp, char[] name, PrimitiveFn func, int arity) {
    SymbolId sym = interp.symbols.intern(name);
    Value* prim = make_primitive(interp, name, func, arity);
    env_define_with_barrier(interp.global_env, sym, prim, interp);
}

fn void register_dispatched_prim(Interp* interp, char[] name, PrimitiveFn func, int arity) {
    SymbolId sym = interp.symbols.intern(name);
    Value* prim = make_primitive(interp, name, func, arity);

    // Create method table with no typed entries, primitive as fallback
    MethodTable* mt = (MethodTable*)mem::malloc(MethodTable.sizeof);
    mt.name = sym;
    mt.entries = (MethodEntry*)mem::malloc(MethodEntry.sizeof * METHOD_INITIAL_CAPACITY);
    mt.entry_count = 0;
    mt.capacity = METHOD_INITIAL_CAPACITY;
    mt.fallback = prim;

    Value* mt_val = boundary_alloc_value_in_root(interp, true);
    mt_val.tag = METHOD_TABLE;
    mt_val.method_table_val = mt;

    env_define_with_barrier(interp.global_env, sym, mt_val, interp);
}
