module lisp;

fn usz bindgen_append_facade_helper_stubs(char[] buf, usz pos, ParsedFunc* funcs, usz func_count, bool* ok) {
    return bindgen_append_facade_helper_stubs_impl(buf, pos, funcs, func_count, ok);
}
