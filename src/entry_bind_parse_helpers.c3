module main;

import lisp;

struct BindHeaderParseResult {
    usz func_count;
    bool path_too_long;
    bool function_limit_hit;
    bool param_allocation_failed;
    bool unsupported_type;
    bool parse_failed;
    char* failed_header;
    char[64] unsupported_type_name;
}

fn BindHeaderParseResult parse_ffi_dep_headers(lisp::LibclangHandle* clang, lisp::TomlFfiDep* dep,
    lisp::ParsedFunc* funcs, usz max_funcs) {
    BindHeaderParseResult result = {};
    for (usz hi = 0; hi < dep.header_count; hi++) {
        char* header = &dep.headers[hi];
        usz hlen = cstr_len(header);
        lisp::ParsedFunc* out_ptr = result.func_count < max_funcs ? &funcs[result.func_count] : null;
        lisp::ParseHeaderResult parsed = lisp::parse_header_checked(clang, header[:hlen],
            out_ptr, max_funcs - result.func_count,
            &dep.functions, dep.function_count, dep.filter_functions);
        if (parsed.path_too_long) {
            result.path_too_long = true;
            result.failed_header = header;
            return result;
        }
        if (parsed.output_limit_hit) {
            result.function_limit_hit = true;
            result.failed_header = header;
            result.func_count += parsed.count;
            return result;
        }
        if (parsed.param_allocation_failed) {
            result.param_allocation_failed = true;
            result.failed_header = header;
            result.func_count += parsed.count;
            return result;
        }
        if (parsed.unsupported_type) {
            result.unsupported_type = true;
            result.failed_header = header;
            lisp::cstr_copy(&parsed.unsupported_type_name, &result.unsupported_type_name, result.unsupported_type_name.len - 1);
            result.func_count += parsed.count;
            return result;
        }
        if (parsed.parse_failed) {
            result.parse_failed = true;
            result.failed_header = header;
            return result;
        }
        result.func_count += parsed.count;
    }
    return result;
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
