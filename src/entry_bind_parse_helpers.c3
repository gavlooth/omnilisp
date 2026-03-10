module main;

import lisp;

fn usz parse_ffi_dep_headers(lisp::LibclangHandle* clang, lisp::TomlFfiDep* dep,
    lisp::ParsedFunc* funcs, usz max_funcs) {
    usz func_count = 0;
    for (usz hi = 0; hi < dep.header_count; hi++) {
        char* header = &dep.headers[hi];
        usz hlen = cstr_len(header);
        usz found = lisp::parse_header(clang, header[:hlen],
            &funcs[func_count], max_funcs - func_count,
            &dep.functions, dep.function_count, dep.filter_functions);
        func_count += found;
    }
    return func_count;
}

fn void free_parsed_funcs(lisp::ParsedFunc* funcs, usz count) {
    for (usz i = 0; i < count; i++) {
        if (funcs[i].params != null) {
            mem::free(funcs[i].params);
            funcs[i].params = null;
        }
        funcs[i].param_count = 0;
        funcs[i].param_capacity = 0;
    }
}
