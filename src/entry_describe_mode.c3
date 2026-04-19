module main;

import std::io;

struct SymbolDescribeInfo {
    bool found;
    char[] kind;
    char[] documentation;
}

fn char* find_describe_symbol_arg(int argc, char** argv, int describe_idx) {
    for (int i = describe_idx + 1; i < argc; i++) {
        if (str_eq(argv[i], "--json")) continue;
        if (cli_arg_starts_with_double_dash(argv[i])) return null;
        return argv[i];
    }
    return null;
}

fn char* find_describe_option_like_arg(int argc, char** argv, int describe_idx) {
    for (int i = describe_idx + 1; i < argc; i++) {
        if (str_eq(argv[i], "--json")) continue;
        if (cli_arg_starts_with_double_dash(argv[i])) return argv[i];
        return null;
    }
    return null;
}

fn SymbolDescribeInfo describe_lookup_symbol(char* symbol_arg) {
    if (str_eq(symbol_arg, "define")) return { .found = true, .kind = "special-form", .documentation = "Bind a name or define a function. Function shorthand is (define (name args...) body)." };
    if (str_eq(symbol_arg, "lambda")) return { .found = true, .kind = "special-form", .documentation = "Create an anonymous function with strict arity." };
    if (str_eq(symbol_arg, "let")) return { .found = true, .kind = "special-form", .documentation = "Introduce flat-pair bindings: (let (x 1 y 2) body)." };
    if (str_eq(symbol_arg, "if")) return { .found = true, .kind = "special-form", .documentation = "Conditional form with explicit then and else branches." };
    if (str_eq(symbol_arg, "block")) return { .found = true, .kind = "special-form", .documentation = "Evaluate expressions in order and return the last value." };
    if (str_eq(symbol_arg, "set!")) return { .found = true, .kind = "special-form", .documentation = "Mutate a binding, pair slot, or generic collection location." };
    if (str_eq(symbol_arg, "quote")) return { .found = true, .kind = "special-form", .documentation = "Return datum without evaluation." };
    if (str_eq(symbol_arg, "match")) return { .found = true, .kind = "special-form", .documentation = "Pattern matching form with exhaustive branch-style clauses." };
    if (str_eq(symbol_arg, "handle")) return { .found = true, .kind = "special-form", .documentation = "Install algebraic effect handlers for a dynamic scope." };
    if (str_eq(symbol_arg, "signal")) return { .found = true, .kind = "special-form", .documentation = "Raise an effect to the nearest matching handler." };
    if (str_eq(symbol_arg, "resolve")) return { .found = true, .kind = "special-form", .documentation = "Resume a captured effect continuation from inside a handler." };
    if (str_eq(symbol_arg, "with-continuation")) return { .found = true, .kind = "special-form", .documentation = "Bind and reuse the continuation supplied by a handle clause." };
    if (str_eq(symbol_arg, "checkpoint")) return { .found = true, .kind = "special-form", .documentation = "Delimited continuation boundary." };
    if (str_eq(symbol_arg, "capture")) return { .found = true, .kind = "special-form", .documentation = "Capture the current delimited continuation." };
    if (str_eq(symbol_arg, "module")) return { .found = true, .kind = "special-form", .documentation = "Declare a module at top level." };
    if (str_eq(symbol_arg, "import")) return { .found = true, .kind = "special-form", .documentation = "Import another module or selected exports." };
    if (str_eq(symbol_arg, "export-from")) return { .found = true, .kind = "special-form", .documentation = "Re-export specific symbols from another module." };
    if (str_eq(symbol_arg, "partial")) return { .found = true, .kind = "special-form", .documentation = "Explicit partial application helper." };
    if (str_eq(symbol_arg, "explain")) return { .found = true, .kind = "special-form", .documentation = "Inspect dispatch or effect resolution using a quoted selector." };

    if (str_eq(symbol_arg, "nil")) return { .found = true, .kind = "builtin", .documentation = "The only absence value and one of the two falsy values." };
    if (str_eq(symbol_arg, "false")) return { .found = true, .kind = "builtin", .documentation = "Value-level alias of nil; only nil and false are falsy in Omni." };
    if (str_eq(symbol_arg, "true")) return { .found = true, .kind = "builtin", .documentation = "Truthy boolean value." };
    if (str_eq(symbol_arg, "Array")) return { .found = true, .kind = "builtin", .documentation = "Array constructor and descriptive type symbol." };
    if (str_eq(symbol_arg, "Dictionary")) return { .found = true, .kind = "builtin", .documentation = "Dictionary constructor and descriptive type symbol." };
    if (str_eq(symbol_arg, "Dict")) return { .found = true, .kind = "builtin", .documentation = "Allowed shorthand alias for the canonical Dictionary constructor/type." };
    if (str_eq(symbol_arg, "List")) return { .found = true, .kind = "builtin", .documentation = "List constructor and descriptive type symbol." };
    if (str_eq(symbol_arg, "Integer")) return { .found = true, .kind = "builtin", .documentation = "Integer constructor and descriptive type symbol." };
    if (str_eq(symbol_arg, "Float")) return { .found = true, .kind = "builtin", .documentation = "Float constructor; defaults to Float64 and accepts precision 32 or 64." };
    if (str_eq(symbol_arg, "Float32")) return { .found = true, .kind = "builtin", .documentation = "Float32 constructor and descriptive type symbol for native 32-bit floating-point scalar values." };
    if (str_eq(symbol_arg, "Float64")) return { .found = true, .kind = "builtin", .documentation = "Float64 constructor and descriptive type symbol." };
    if (str_eq(symbol_arg, "String")) return { .found = true, .kind = "builtin", .documentation = "String constructor and descriptive type symbol." };
    if (str_eq(symbol_arg, "Boolean")) return { .found = true, .kind = "builtin", .documentation = "Boolean constructor and descriptive type symbol." };
    if (str_eq(symbol_arg, "length")) return { .found = true, .kind = "builtin", .documentation = "Generic collection length operation." };
    if (str_eq(symbol_arg, "ref")) return { .found = true, .kind = "builtin", .documentation = "Generic keyed or indexed lookup operation." };
    if (str_eq(symbol_arg, "map")) return { .found = true, .kind = "builtin", .documentation = "Apply a function across a collection." };
    if (str_eq(symbol_arg, "filter")) return { .found = true, .kind = "builtin", .documentation = "Keep elements that satisfy a predicate." };
    if (str_eq(symbol_arg, "foldl")) return { .found = true, .kind = "builtin", .documentation = "Left fold over a collection." };
    if (str_eq(symbol_arg, "type-of")) return { .found = true, .kind = "builtin", .documentation = "Return the descriptive Omni type symbol for a value." };

    return { .found = false, .kind = "", .documentation = "" };
}

fn int run_describe_mode(int argc, char** argv, int describe_idx) {
    bool json = find_flag_index(argc, argv, "--json") >= 0;
    char* symbol_arg = find_describe_symbol_arg(argc, argv, describe_idx);
    if (symbol_arg == null) {
        char* unexpected = find_describe_option_like_arg(argc, argv, describe_idx);
        if (unexpected != null) {
            if (json) {
                print_describe_unexpected_arg_json(unexpected);
                return 1;
            }
            return print_unexpected_mode_arg("--describe", unexpected);
        }
        if (json) {
            print_describe_usage_json();
            return 1;
        }
        return print_describe_usage();
    }
    char* extra = find_extra_mode_arg(argc, argv, describe_idx, 1, true);
    if (extra != null) {
        if (json) {
            print_describe_unexpected_arg_json(extra);
            return 1;
        }
        return print_unexpected_mode_arg("--describe", extra);
    }

    SymbolDescribeInfo info = describe_lookup_symbol(symbol_arg);
    if (json) {
        print_describe_result_json(symbol_arg[:cstr_len(symbol_arg)], info);
    } else {
        print_describe_result_text(symbol_arg[:cstr_len(symbol_arg)], info);
    }
    return info.found ? 0 : 1;
}
