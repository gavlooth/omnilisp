module lisp;

/**
 * Register standard library functions defined in Pika itself.
 * Called after register_primitives().
 */
fn void register_stdlib(Interp* interp) {
    // Standard library loaded from embedded stdlib/stdlib.lisp at compile time.
    // Edit that file (with syntax highlighting) instead of inline strings here.
    // Supports multi-line forms — tracks paren/bracket/brace depth.
    char[] src = $embed("../../stdlib/stdlib.lisp");
    usz pos = 0;
    while (pos < src.len) {
        // Skip whitespace and comments
        while (pos < src.len) {
            char c = src[pos];
            if (c == ' ' || c == '\t' || c == '\n' || c == '\r') { pos++; continue; }
            if (c == ';') { while (pos < src.len && src[pos] != '\n') pos++; continue; }
            break;
        }
        if (pos >= src.len) break;

        // Read one complete s-expression by tracking depth
        usz start = pos;
        int depth = 0;
        bool in_string = false;
        bool started = false;
        while (pos < src.len) {
            char c = src[pos];
            if (in_string) {
                if (c == '\\' && pos + 1 < src.len) { pos += 2; continue; }
                if (c == '"') in_string = false;
                pos++;
                continue;
            }
            if (c == '"') { in_string = true; pos++; continue; }
            if (c == '(' || c == '[' || c == '{') { depth++; started = true; pos++; continue; }
            if (c == ')' || c == ']' || c == '}') { depth--; pos++; if (started && depth == 0) break; continue; }
            if (c == ';') { while (pos < src.len && src[pos] != '\n') pos++; continue; }
            pos++;
            // Atom without parens (bare symbol/number) — single token
            if (!started && depth == 0) {
                while (pos < src.len && src[pos] != ' ' && src[pos] != '\t' &&
                       src[pos] != '\n' && src[pos] != '\r' && src[pos] != ')' &&
                       src[pos] != ']' && src[pos] != '}' && src[pos] != ';') pos++;
                break;
            }
        }
        if (pos > start) {
            run(src[start..pos - 1], interp);
        }
    }
}

fn bool bootstrap_runtime_interp(Interp* interp) {
    if (interp == null) return false;
    if (!register_primitives(interp)) return false;
    register_stdlib(interp);
    return true;
}
