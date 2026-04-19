module lisp;

fn bool bindgen_param_needs_callback_unregister(ParsedParam* p) {
    return p.role == BINDGEN_ROLE_CALLBACK && p.teardown == BINDGEN_TEARDOWN_CALLBACK_UNREGISTER;
}

fn bool bindgen_param_needs_buffer_manual_review(ParsedParam* p) {
    return p.role == BINDGEN_ROLE_STRING_BUFFER && p.teardown == BINDGEN_TEARDOWN_MANUAL_REVIEW;
}

fn bool bindgen_param_needs_buffer_none(ParsedParam* p) {
    return p.role == BINDGEN_ROLE_STRING_BUFFER && p.teardown == BINDGEN_TEARDOWN_NONE;
}

fn void bindgen_collect_facade_helper_flags(ParsedFunc* funcs, usz func_count,
    bool* need_buffer_helper, bool* need_buffer_manual_review_helper, bool* need_buffer_none_helper,
    bool* need_callback_helper, bool* need_opaque_arg_helper, bool* need_string_return_helper,
    bool* need_opaque_return_helper, bool* need_callback_cleanup_helper) {
    *need_buffer_helper = false;
    *need_buffer_manual_review_helper = false;
    *need_buffer_none_helper = false;
    *need_callback_helper = false;
    *need_opaque_arg_helper = false;
    *need_string_return_helper = false;
    *need_opaque_return_helper = false;
    *need_callback_cleanup_helper = false;

    for (usz i = 0; i < func_count; i++) {
        ParsedFunc* f = &funcs[i];
        if (f.filtered_out || f.is_variadic || !bindgen_function_needs_facade_scaffold(f)) continue;

        if (f.return_role == BINDGEN_ROLE_STRING_RETURN) *need_string_return_helper = true;
        if (f.return_role == BINDGEN_ROLE_OPAQUE_POINTER) *need_opaque_return_helper = true;

        for (usz j = 0; j < f.param_count; j++) {
            switch (f.params[j].role) {
                case BINDGEN_ROLE_STRING_BUFFER:
                    *need_buffer_helper = true;
                    if (bindgen_param_needs_buffer_manual_review(&f.params[j])) {
                        *need_buffer_manual_review_helper = true;
                    } else if (bindgen_param_needs_buffer_none(&f.params[j])) {
                        *need_buffer_none_helper = true;
                    }
                    break;
                case BINDGEN_ROLE_CALLBACK:
                    *need_callback_helper = true;
                    if (f.params[j].teardown == BINDGEN_TEARDOWN_CALLBACK_UNREGISTER) {
                        *need_callback_cleanup_helper = true;
                    }
                    break;
                case BINDGEN_ROLE_OPAQUE_POINTER:
                    *need_opaque_arg_helper = true;
                    break;
                case BINDGEN_ROLE_VALUE:
                case BINDGEN_ROLE_STRING_INPUT:
                case BINDGEN_ROLE_STRING_RETURN:
                    break;
            }
        }
    }
}

fn usz bindgen_append_facade_helper_stubs_impl(char[] buf, usz pos, ParsedFunc* funcs, usz func_count, bool* ok) {
    bool need_buffer_helper;
    bool need_buffer_manual_review_helper;
    bool need_buffer_none_helper;
    bool need_callback_helper;
    bool need_opaque_arg_helper;
    bool need_string_return_helper;
    bool need_opaque_return_helper;
    bool need_callback_cleanup_helper;
    bindgen_collect_facade_helper_flags(funcs, func_count, &need_buffer_helper, &need_buffer_manual_review_helper,
        &need_buffer_none_helper, &need_callback_helper, &need_opaque_arg_helper, &need_string_return_helper,
        &need_opaque_return_helper, &need_callback_cleanup_helper);

    if (!need_buffer_helper && !need_callback_helper && !need_opaque_arg_helper &&
        !need_string_return_helper && !need_opaque_return_helper && !need_callback_cleanup_helper) {
        return pos;
    }

    pos = buf_append(buf, pos, "  ;; Generated helper templates for non-trivial FFI contracts.\n", ok);
    if (need_buffer_helper) {
        pos = buf_append(buf, pos, "  (define (bindgen-validate-string-buffer-size buffer-size direction)\n", ok);
        pos = buf_append(buf, pos, "    ;; Concrete policy: mutable out/inout buffers require a positive integer size input.\n", ok);
        pos = buf_append(buf, pos, "    (if (== direction \"in\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (if (== direction \"out\")\n", ok);
        pos = buf_append(buf, pos, "            nil\n", ok);
        pos = buf_append(buf, pos, "            (if (== direction \"inout\")\n", ok);
        pos = buf_append(buf, pos, "                nil\n", ok);
        pos = buf_append(buf, pos, "                (raise \"bindgen/string-buffer-direction-mismatch\"))))\n", ok);
        pos = buf_append(buf, pos, "    (if (== direction \"in\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (if (nil? buffer-size)\n", ok);
        pos = buf_append(buf, pos, "            (raise \"bindgen/string-buffer-size-required\")\n", ok);
        pos = buf_append(buf, pos, "            nil))\n", ok);
        pos = buf_append(buf, pos, "    (if (== direction \"in\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (if (int? buffer-size)\n", ok);
        pos = buf_append(buf, pos, "            nil\n", ok);
        pos = buf_append(buf, pos, "            (raise \"bindgen/string-buffer-size-type-mismatch\")))\n", ok);
        pos = buf_append(buf, pos, "    (if (== direction \"in\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (if (> buffer-size 0)\n", ok);
        pos = buf_append(buf, pos, "            nil\n", ok);
        pos = buf_append(buf, pos, "            (raise \"bindgen/string-buffer-size-nonpositive\")))\n", ok);
        pos = buf_append(buf, pos, "    nil\n", ok);
        pos = buf_append(buf, pos, "  )\n\n", ok);

        pos = buf_append(buf, pos, "  (define (bindgen-prepare-string-buffer buffer buffer-size role ownership nullability teardown direction)\n", ok);
        pos = buf_append(buf, pos, "    ;; Fail closed on nil buffers so wrapper edits cannot silently pass invalid pointer state.\n", ok);
        pos = buf_append(buf, pos, "    (if (nil? buffer)\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/string-buffer-nil\")\n", ok);
        pos = buf_append(buf, pos, "        nil)\n", ok);
        pos = buf_append(buf, pos, "    ;; metadata arguments keep role/ownership/nullability/teardown and direction explicit at the wrapper boundary.\n", ok);
        pos = buf_append(buf, pos, "    (bindgen-validate-string-buffer-size buffer-size direction)\n", ok);
        if (need_buffer_manual_review_helper && need_buffer_none_helper) {
            pos = buf_append(buf, pos, "    (if (== teardown \"manual-review\")\n", ok);
            pos = buf_append(buf, pos, "        (bindgen-prepare-string-buffer-manual-review buffer buffer-size role ownership nullability teardown direction)\n", ok);
            pos = buf_append(buf, pos, "        (if (== teardown \"none\")\n", ok);
            pos = buf_append(buf, pos, "            (bindgen-prepare-string-buffer-none buffer buffer-size role ownership nullability teardown direction)\n", ok);
            pos = buf_append(buf, pos, "            (raise \"bindgen/string-buffer-unsupported-teardown\")))\n", ok);
        } else if (need_buffer_manual_review_helper) {
            pos = buf_append(buf, pos, "    (if (== teardown \"manual-review\")\n", ok);
            pos = buf_append(buf, pos, "        (bindgen-prepare-string-buffer-manual-review buffer buffer-size role ownership nullability teardown direction)\n", ok);
            pos = buf_append(buf, pos, "        (raise \"bindgen/string-buffer-unsupported-teardown\"))\n", ok);
        } else if (need_buffer_none_helper) {
            pos = buf_append(buf, pos, "    (if (== teardown \"none\")\n", ok);
            pos = buf_append(buf, pos, "        (bindgen-prepare-string-buffer-none buffer buffer-size role ownership nullability teardown direction)\n", ok);
            pos = buf_append(buf, pos, "        (raise \"bindgen/string-buffer-unsupported-teardown\"))\n", ok);
        } else {
            pos = buf_append(buf, pos, "    (raise \"bindgen/string-buffer-policy-missing\")\n", ok);
        }
        pos = buf_append(buf, pos, "  )\n\n", ok);
    }
    if (need_buffer_manual_review_helper) {
        pos = buf_append(buf, pos, "  (define (bindgen-prepare-string-buffer-manual-review buffer buffer-size role ownership nullability teardown direction)\n", ok);
        pos = buf_append(buf, pos, "    ;; Fail closed if metadata does not match the expected mutable string-buffer contract.\n", ok);
        pos = buf_append(buf, pos, "    (if (== role \"string-buffer\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/string-buffer-role-mismatch\"))\n", ok);
        pos = buf_append(buf, pos, "    (if (== ownership \"borrowed\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/string-buffer-ownership-mismatch\"))\n", ok);
        pos = buf_append(buf, pos, "    ;; REVIEW(bindgen): manual-review teardown still requires explicit allocation/writeback policy edits.\n", ok);
        pos = buf_append(buf, pos, "    (raise \"bindgen/string-buffer-manual-review\")\n", ok);
        pos = buf_append(buf, pos, "  )\n\n", ok);
    }
    if (need_buffer_none_helper) {
        pos = buf_append(buf, pos, "  (define (bindgen-prepare-string-buffer-none buffer buffer-size role ownership nullability teardown direction)\n", ok);
        pos = buf_append(buf, pos, "    ;; none teardown keeps pointer ownership with the caller after size/direction validation.\n", ok);
        pos = buf_append(buf, pos, "    (if (== role \"string-buffer\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/string-buffer-role-mismatch\"))\n", ok);
        pos = buf_append(buf, pos, "    (if (== ownership \"borrowed\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/string-buffer-ownership-mismatch\"))\n", ok);
        pos = buf_append(buf, pos, "    buffer\n", ok);
        pos = buf_append(buf, pos, "  )\n\n", ok);
    }
    if (need_callback_helper) {
        pos = buf_append(buf, pos, "  (define (bindgen-register-callback callback role ownership nullability teardown)\n", ok);
        pos = buf_append(buf, pos, "    ;; Fail closed until this facade is edited for a concrete callback handle shim.\n", ok);
        pos = buf_append(buf, pos, "    (if (nil? callback)\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/callback-nil\")\n", ok);
        pos = buf_append(buf, pos, "        nil)\n", ok);
        pos = buf_append(buf, pos, "    (if (== role \"callback\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/callback-role-mismatch\"))\n", ok);
        pos = buf_append(buf, pos, "    (if (== ownership \"borrowed\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/callback-ownership-mismatch\"))\n", ok);
        pos = buf_append(buf, pos, "    (if (== teardown \"callback-unregister\")\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/callback-registration-requires-subsystem-shim\")\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/callback-unsupported-teardown\"))\n", ok);
        pos = buf_append(buf, pos, "  )\n\n", ok);
    }
    if (need_callback_cleanup_helper) {
        pos = buf_append(buf, pos, "  (define (bindgen-unregister-callback callback-handle role ownership nullability teardown)\n", ok);
        pos = buf_append(buf, pos, "    ;; Fail closed for unknown callback teardown contracts; tolerate nil so wrapper cleanup can be idempotent.\n", ok);
        pos = buf_append(buf, pos, "    (if (nil? callback-handle)\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (if (== role \"callback\")\n", ok);
        pos = buf_append(buf, pos, "            (if (== ownership \"borrowed\")\n", ok);
        pos = buf_append(buf, pos, "                (if (== teardown \"callback-unregister\")\n", ok);
        pos = buf_append(buf, pos, "                    (raise \"bindgen/callback-unregister-requires-subsystem-shim\")\n", ok);
        pos = buf_append(buf, pos, "                    (raise \"bindgen/callback-unsupported-teardown\"))\n", ok);
        pos = buf_append(buf, pos, "                (raise \"bindgen/callback-ownership-mismatch\"))\n", ok);
        pos = buf_append(buf, pos, "            (raise \"bindgen/callback-role-mismatch\")))\n", ok);
        pos = buf_append(buf, pos, "  )\n\n", ok);
    }
    if (need_opaque_arg_helper) {
        pos = buf_append(buf, pos, "  (define (bindgen-wrap-opaque-arg handle role ownership nullability teardown)\n", ok);
        pos = buf_append(buf, pos, "    ;; Concrete policy: opaque args are borrowed handles in this generated layer.\n", ok);
        pos = buf_append(buf, pos, "    (if (== role \"opaque-pointer\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/opaque-arg-role-mismatch\"))\n", ok);
        pos = buf_append(buf, pos, "    (if (== ownership \"borrowed\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/opaque-arg-ownership-mismatch\"))\n", ok);
        pos = buf_append(buf, pos, "    (if (== teardown \"none\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/opaque-arg-unsupported-teardown\"))\n", ok);
        pos = buf_append(buf, pos, "    (if (nil? handle)\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/opaque-arg-nil\")\n", ok);
        pos = buf_append(buf, pos, "        nil)\n", ok);
        pos = buf_append(buf, pos, "    handle\n", ok);
        pos = buf_append(buf, pos, "  )\n\n", ok);
    }
    if (need_string_return_helper) {
        pos = buf_append(buf, pos, "  (define (bindgen-wrap-string-return-borrowed text role ownership nullability teardown)\n", ok);
        pos = buf_append(buf, pos, "    ;; Concrete policy: copy borrowed C strings at the boundary to avoid aliasing borrowed storage.\n", ok);
        pos = buf_append(buf, pos, "    (if (== role \"string-return\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/string-return-role-mismatch\"))\n", ok);
        pos = buf_append(buf, pos, "    (if (== ownership \"borrowed\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/string-return-ownership-mismatch\"))\n", ok);
        pos = buf_append(buf, pos, "    (if (== teardown \"manual-review\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/string-return-unsupported-teardown\"))\n", ok);
        pos = buf_append(buf, pos, "    (if (nil? text)\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/string-return-nil\")\n", ok);
        pos = buf_append(buf, pos, "        nil)\n", ok);
        pos = buf_append(buf, pos, "    (string-append \"\" text)\n", ok);
        pos = buf_append(buf, pos, "  )\n\n", ok);
        pos = buf_append(buf, pos, "  (define (bindgen-wrap-string-return-unknown text role ownership nullability teardown)\n", ok);
        pos = buf_append(buf, pos, "    ;; Fail closed for unknown ownership until an explicit lifetime contract is provided.\n", ok);
        pos = buf_append(buf, pos, "    (if (== role \"string-return\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/string-return-role-mismatch\"))\n", ok);
        pos = buf_append(buf, pos, "    (if (== ownership \"unknown\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/string-return-ownership-mismatch\"))\n", ok);
        pos = buf_append(buf, pos, "    (if (== teardown \"manual-review\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/string-return-unsupported-teardown\"))\n", ok);
        pos = buf_append(buf, pos, "    (raise \"bindgen/string-return-unknown-ownership\")\n", ok);
        pos = buf_append(buf, pos, "  )\n\n", ok);
    }
    if (need_opaque_return_helper) {
        pos = buf_append(buf, pos, "  (define (bindgen-wrap-opaque-return-manual-review handle role ownership nullability teardown)\n", ok);
        pos = buf_append(buf, pos, "    ;; Fail closed for opaque manual-review returns until a finalizer/ownership policy is defined.\n", ok);
        pos = buf_append(buf, pos, "    (if (== role \"opaque-pointer\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/opaque-return-role-mismatch\"))\n", ok);
        pos = buf_append(buf, pos, "    (if (== teardown \"manual-review\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/opaque-return-unsupported-teardown\"))\n", ok);
        pos = buf_append(buf, pos, "    (raise \"bindgen/opaque-return-manual-review\")\n", ok);
        pos = buf_append(buf, pos, "  )\n\n", ok);
        pos = buf_append(buf, pos, "  (define (bindgen-wrap-opaque-return-none handle role ownership nullability teardown)\n", ok);
        pos = buf_append(buf, pos, "    ;; Concrete policy: none-teardown opaque returns are pass-through borrowed handles.\n", ok);
        pos = buf_append(buf, pos, "    (if (== role \"opaque-pointer\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/opaque-return-role-mismatch\"))\n", ok);
        pos = buf_append(buf, pos, "    (if (== teardown \"none\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/opaque-return-unsupported-teardown\"))\n", ok);
        pos = buf_append(buf, pos, "    (if (== ownership \"borrowed\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/opaque-return-ownership-mismatch\"))\n", ok);
        pos = buf_append(buf, pos, "    (if (nil? handle)\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/opaque-return-nil\")\n", ok);
        pos = buf_append(buf, pos, "        nil)\n", ok);
        pos = buf_append(buf, pos, "    handle\n", ok);
        pos = buf_append(buf, pos, "  )\n\n", ok);
    }
    return pos;
}
