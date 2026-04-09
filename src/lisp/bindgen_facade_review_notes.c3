module lisp;

fn usz bindgen_append_facade_review_issue(char[] buf, usz pos, char[] text, bool* wrote_issue, bool* ok) {
    if (*wrote_issue) pos = buf_append(buf, pos, "; ", ok);
    pos = buf_append(buf, pos, text, ok);
    *wrote_issue = true;
    return pos;
}

fn usz bindgen_append_facade_review_note(char[] buf, usz pos, ParsedFunc* f, bool* ok) {
    pos = buf_append(buf, pos, "  ;; REVIEW(bindgen): review ", ok);
    pos = buf_append_kebab(buf, pos, &f.name, ok);
    pos = buf_append(buf, pos, " — ", ok);
    bool wrote_issue = false;

    if (f.return_role == BINDGEN_ROLE_STRING_RETURN) {
        pos = bindgen_append_facade_review_issue(
            buf, pos, "string return ownership/nullability is not automatic", &wrote_issue, ok);
    }
    if (f.return_role == BINDGEN_ROLE_OPAQUE_POINTER) {
        pos = bindgen_append_facade_review_issue(
            buf, pos, "opaque return ownership/finalizer policy needs an Omni wrapper", &wrote_issue, ok);
    }
    for (usz i = 0; i < f.param_count; i++) {
        ParsedParam* p = &f.params[i];
        switch (p.role) {
            case BINDGEN_ROLE_STRING_BUFFER:
                pos = bindgen_append_facade_review_issue(
                    buf, pos, "mutable char* buffer direction/size must be wrapped explicitly", &wrote_issue, ok);
                break;
            case BINDGEN_ROLE_CALLBACK:
                pos = bindgen_append_facade_review_issue(
                    buf, pos, "callback registration and teardown must be wrapped explicitly", &wrote_issue, ok);
                break;
            case BINDGEN_ROLE_OPAQUE_POINTER:
                pos = bindgen_append_facade_review_issue(
                    buf, pos, "opaque pointer ownership/lifetime must be reviewed", &wrote_issue, ok);
                break;
            case BINDGEN_ROLE_STRING_INPUT:
                break;
            case BINDGEN_ROLE_STRING_RETURN:
                break;
            case BINDGEN_ROLE_VALUE:
                break;
        }
    }
    if (!wrote_issue) {
        pos = buf_append(buf, pos, "non-trivial FFI ownership/nullability metadata needs review before exposing it as stable Omni API.", ok);
    } else {
        pos = buf_append_char(buf, pos, '.', ok);
    }
    return buf_append_char(buf, pos, '\n', ok);
}

fn usz bindgen_append_facade_wrapper_body_notes(char[] buf, usz pos, char[] indent, ParsedFunc* f, bool* ok) {
    if (f.return_role == BINDGEN_ROLE_STRING_RETURN) {
        pos = buf_append(buf, pos, indent, ok);
        pos = buf_append(buf, pos, ";; REVIEW: decide whether to copy, borrow, or null-check the returned string.\n", ok);
    }
    if (f.return_role == BINDGEN_ROLE_OPAQUE_POINTER) {
        pos = buf_append(buf, pos, indent, ok);
        pos = buf_append(buf, pos, ";; REVIEW: wrap opaque returns in a handle/finalizer policy before exposing them broadly.\n", ok);
    }
    for (usz i = 0; i < f.param_count; i++) {
        ParsedParam* p = &f.params[i];
        switch (p.role) {
            case BINDGEN_ROLE_STRING_INPUT:
                break;
            case BINDGEN_ROLE_STRING_BUFFER:
                pos = buf_append(buf, pos, indent, ok);
                pos = buf_append(buf, pos, ";; REVIEW: mutable char* buffers need explicit allocation, size, and output semantics.\n", ok);
                break;
            case BINDGEN_ROLE_STRING_RETURN:
                break;
            case BINDGEN_ROLE_OPAQUE_POINTER:
                pos = buf_append(buf, pos, indent, ok);
                pos = buf_append(buf, pos, ";; REVIEW: confirm opaque pointer ownership/lifetime at the Omni boundary.\n", ok);
                break;
            case BINDGEN_ROLE_CALLBACK:
                pos = buf_append(buf, pos, indent, ok);
                pos = buf_append(buf, pos, ";; REVIEW: confirm callback-handle registration/unregister policy matches the target subsystem contract.\n", ok);
                break;
            case BINDGEN_ROLE_VALUE:
                break;
        }
    }
    return pos;
}
