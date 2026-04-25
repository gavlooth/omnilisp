module lisp;

import std::io;
import std::core::mem;

fn Value* ffi_metadata_dict_lookup(Value* dict, Interp* interp, char[] key) {
    if (dict == null || dict.tag != HASHMAP || dict.hashmap_val == null) return null;
    return hashmap_get(dict.hashmap_val, make_symbol(interp, interp.symbols.intern(key)));
}

fn bool ffi_metadata_handle_contract(Value* handle_v, Interp* interp, char[] expected_name) {
    if (handle_v == null || handle_v.tag != FFI_HANDLE) return false;
    Value*[1] args = { handle_v };
    Value* before = prim_foreign_describe(args[..], null, interp);
    Value* runtime = before != null ? ffi_metadata_dict_lookup(before, interp, "runtime") : null;
    Value* kind = before != null ? ffi_metadata_dict_lookup(before, interp, "kind") : null;
    Value* ownership = before != null ? ffi_metadata_dict_lookup(before, interp, "ownership") : null;
    Value* name = before != null ? ffi_metadata_dict_lookup(before, interp, "name") : null;
    Value* live = before != null ? ffi_metadata_dict_lookup(before, interp, "live") : null;
    Value* capabilities = before != null ? ffi_metadata_dict_lookup(before, interp, "capabilities") : null;
    bool before_ok = before != null && before.tag == HASHMAP &&
        runtime != null && runtime.tag == SYMBOL && runtime.sym_val == interp.symbols.intern("native") &&
        kind != null && kind.tag == SYMBOL && kind.sym_val == interp.symbols.intern("opaque") &&
        ownership != null && ownership.tag == SYMBOL && ownership.sym_val == interp.symbols.intern("owned") &&
        ((expected_name.len == 0 && name != null && name.tag == NIL) ||
            (name != null && name.tag == SYMBOL && name.sym_val == interp.symbols.intern(expected_name))) &&
        live != null && live.tag == SYMBOL && live.sym_val == interp.sym_true &&
        capabilities != null && capabilities.tag == ARRAY && capabilities.array_val != null &&
        capabilities.array_val.length == 1 &&
        capabilities.array_val.items[0] != null &&
        capabilities.array_val.items[0].tag == SYMBOL &&
        capabilities.array_val.items[0].sym_val == interp.symbols.intern("release");
    if (!before_ok) return false;

    Value* released = prim_foreign_release(args[..], null, interp);
    if (released == null || released.tag == ERROR) return false;

    Value* after = prim_foreign_describe(args[..], null, interp);
    Value* after_runtime = after != null ? ffi_metadata_dict_lookup(after, interp, "runtime") : null;
    Value* after_kind = after != null ? ffi_metadata_dict_lookup(after, interp, "kind") : null;
    Value* after_name = after != null ? ffi_metadata_dict_lookup(after, interp, "name") : null;
    Value* after_live = after != null ? ffi_metadata_dict_lookup(after, interp, "live") : null;
    Value* after_caps = after != null ? ffi_metadata_dict_lookup(after, interp, "capabilities") : null;
    bool after_ok = after != null && after.tag == HASHMAP &&
        after_runtime != null && after_runtime.tag == SYMBOL && after_runtime.sym_val == interp.symbols.intern("native") &&
        after_kind != null && after_kind.tag == SYMBOL && after_kind.sym_val == interp.symbols.intern("opaque") &&
        ((expected_name.len == 0 && after_name != null && after_name.tag == NIL) ||
            (after_name != null && after_name.tag == SYMBOL && after_name.sym_val == interp.symbols.intern(expected_name))) &&
        after_live != null && after_live.tag == NIL &&
        after_caps != null && after_caps.tag == ARRAY && after_caps.array_val != null &&
        after_caps.array_val.length == 0;
    return after_ok;
}

fn void run_advanced_ffi_system_foreign_handle_metadata_dict_tests(Interp* interp, int* pass, int* fail) {
    setup(interp, "(define [ffi lib] libc \"libc.so.6\")");
    setup(interp, "(define [ffi λ libc] (fopen (^String path) (^String mode)) ^{'type ForeignHandle 'name File 'ownership owned 'finalizer fclose})");
    test_tag(interp, "ffi dict fopen return is foreign handle", "(fopen \"/etc/hosts\" \"r\")", FFI_HANDLE, pass, fail);
    test_truthy(interp, "foreign-describe FFI return handle signature metadata",
        "(block "
        "(define m (foreign-describe fopen)) "
        "(define r (ref m 'returns)) "
        "(and (= (ref r 'type) 'ForeignHandle) "
        "     (= (ref r 'abi-type) 'ptr) "
        "     (= (ref r 'name) 'File) "
        "     (= (ref r 'ownership) 'owned) "
        "     (= (ref r 'nullability) 'nullable) "
        "     (= (ref r 'finalizer) 'fclose)))",
        pass, fail);
    test_truthy(interp, "foreign-describe owned return handle metadata",
        "(block "
        "(define h (fopen \"/etc/hosts\" \"r\")) "
        "(define m (foreign-describe h)) "
        "(define ok (and (= (ref m 'runtime) 'c-abi) "
        "                (= (ref m 'kind) 'opaque) "
        "                (= (ref m 'name) 'File) "
        "                (= (ref m 'ownership) 'owned) "
        "                (array? (ref m 'capabilities)) "
        "                (= (ref (ref m 'capabilities) 0) 'release))) "
        "ok)",
        pass, fail);
    test_truthy(interp, "foreign-release closes owned return handle once",
        "(block "
        "(define h (fopen \"/etc/hosts\" \"r\")) "
        "(define before (foreign-describe h)) "
        "(foreign-release h) "
        "(foreign-release h) "
        "(define after (foreign-describe h)) "
        "(and (ref before 'live) "
        "     (not (ref after 'live)) "
        "     (not (= (ref after 'ownership) 'owned)) "
        "     (= (length (ref after 'capabilities)) 0)))",
        pass, fail);
    test_truthy(interp, "foreign-describe owned return handle includes release capability and runtime",
        "(let (h (fopen \"/etc/hosts\" \"r\"))"
        "  (let (m (foreign-describe h))"
        "    (and (= (ref m 'type) 'ForeignHandle)"
        "         (= (ref m 'kind) 'opaque)"
        "         (= (ref m 'runtime) 'c-abi)"
        "         (= (ref m 'name) 'File)"
        "         (array? (ref m 'capabilities))"
        "         (= (length (ref m 'capabilities)) 1)"
        "         (= (ref (ref m 'capabilities) 0) 'release))))",
        pass, fail);
    {
        int saved_force = g_boundary_force_alloc_value_fail_nth;
        g_boundary_force_alloc_value_fail_nth = 1;
        test_error_contains(interp, "ffi owned return handle wrapper alloc failure finalizes return",
            "(fopen \"/etc/hosts\" \"r\")",
            "ffi: out of memory while wrapping return ForeignHandle",
            pass, fail);
        g_boundary_force_alloc_value_fail_nth = saved_force;
    }
    {
        Value* bad_library_handle = make_ffi_handle_ex_with_descriptor(
            interp,
            null,
            "bad-library-release",
            null,
            false,
            FOREIGN_RUNTIME_C_ABI,
            FOREIGN_HANDLE_LIBRARY,
            FOREIGN_CAP_RELEASE,
            true);
        bool setup_ok = bad_library_handle != null && bad_library_handle.tag == FFI_HANDLE && bad_library_handle.ffi_val != null;
        bool release_rejected = false;
        if (setup_ok) {
            Value*[] args = { bad_library_handle };
            Value* released = prim_foreign_release(args, null, interp);
            release_rejected = ffi_test_error_contains(released, "handle is not releasable");
        }
        if (setup_ok && release_rejected) {
            emit_pass_literal("[PASS] ffi release rejects malformed c-abi library descriptor with release capability");
            (*pass)++;
        } else {
            io::printn("[FAIL] ffi release rejects malformed c-abi library descriptor with release capability");
            (*fail)++;
        }
    }
    {
        Value* bad_opaque_handle = make_ffi_handle_ex_with_descriptor(
            interp,
            (void*)0x1234,
            "bad-opaque-release",
            null,
            false,
            FOREIGN_RUNTIME_C_ABI,
            FOREIGN_HANDLE_OPAQUE,
            FOREIGN_CAP_RELEASE,
            false);
        bool setup_ok = bad_opaque_handle != null && bad_opaque_handle.tag == FFI_HANDLE && bad_opaque_handle.ffi_val != null;
        bool release_rejected = false;
        if (setup_ok) {
            Value*[] args = { bad_opaque_handle };
            Value* before = prim_foreign_describe(args, null, interp);
            Value* ownership_key = make_symbol(interp, interp.symbols.intern("ownership"));
            Value* capabilities_key = make_symbol(interp, interp.symbols.intern("capabilities"));
            Value* ownership = before != null && before.tag == HASHMAP && before.hashmap_val != null
                ? hashmap_get(before.hashmap_val, ownership_key)
                : null;
            Value* capabilities = before != null && before.tag == HASHMAP && before.hashmap_val != null
                ? hashmap_get(before.hashmap_val, capabilities_key)
                : null;
            bool reflected_release = false;
            if (capabilities != null && capabilities.tag == ARRAY && capabilities.array_val != null &&
                capabilities.array_val.length > 0) {
                Value* first_capability = capabilities.array_val.items[0];
                reflected_release = first_capability != null && first_capability.tag == SYMBOL &&
                    first_capability.sym_val == interp.symbols.intern("release");
            }
            Value* released = prim_foreign_release(args, null, interp);
            release_rejected = ffi_test_error_contains(released, "handle is not releasable") &&
                before != null &&
                before.tag == HASHMAP &&
                before.hashmap_val != null &&
                bad_opaque_handle.ffi_val.lib_handle == (void*)0x1234 &&
                (ownership == null || ownership.tag != SYMBOL || ownership.sym_val != interp.symbols.intern("owned")) &&
                !reflected_release;
        }
        if (setup_ok && release_rejected) {
            emit_pass_literal("[PASS] ffi release rejects release-capability handle without release authority");
            (*pass)++;
        } else {
            io::printn("[FAIL] ffi release rejects release-capability handle without release authority");
            (*fail)++;
        }
    }

    setup(interp,
        "(block "
        "  (define [ffi λ libc] (fopen (^String path) (^String mode)) ^{'name File 'ownership borrowed 'nullability non-null}) "
        "  (define [ffi λ libc] (fclose (^{'name File 'ownership borrowed 'nullability non-null} file))))");
    test_error_contains(interp, "foreign-release rejects borrowed FFI return handle",
        "(block"
        " (define h (fopen \"/etc/hosts\" \"r\"))"
        " (fclose h)"
        " (foreign-release h))",
        "handle is not releasable",
        pass, fail);
    {
        int* payload = (int*)mem::malloc(int.sizeof);
        int* before_release_payload = payload;
        bool payload_ok = payload != null;
        if (payload_ok) {
            *payload = 0xBEEF;
        }
        g_double_release_finalizer_calls = 0;
        Value* mixed = payload_ok
            ? make_ffi_handle_ex(interp, payload, "mixed-release-test", &ffi_double_release_test_finalizer, true, false)
            : null;
        bool setup_ok = payload_ok && mixed != null && mixed.tag == FFI_HANDLE && mixed.ffi_val != null &&
            mixed.ffi_val.free_lib_handle == false;
        bool release_ok = false;
        if (setup_ok) {
            Value*[] args = { mixed };
            Value* released = prim_foreign_release(args, null, interp);
            release_ok = released != null && released.tag != ERROR &&
                g_double_release_finalizer_calls == 1 &&
                (before_release_payload != null) && (*before_release_payload == 0);
        }
        if (setup_ok && release_ok) {
            emit_pass_literal("[PASS] ffi handle mixed finalizer/free construction is normalized before release");
            (*pass)++;
        } else {
            io::printn("[FAIL] ffi handle mixed finalizer/free construction is normalized before release");
            (*fail)++;
        }
        if (payload != null) {
            mem::free(payload);
        }
    }
    test_truthy(interp, "foreign-describe borrowed return handle has no release capability",
        "(let (h (fopen \"/etc/hosts\" \"r\"))"
        "  (let (m (foreign-describe h))"
        "    (and (= (ref m 'type) 'ForeignHandle)"
        "         (= (ref m 'runtime) 'c-abi)"
        "         (= (length (ref m 'capabilities)) 0))))",
        pass, fail);
    setup(interp,
        "(define [ffi λ libc] (fopen (^String path) (^String mode))"
        " ^{'name File 'ownership manual})");
    test_truthy(interp, "foreign-describe manual return handle preserves manual ownership",
        "(let (h (fopen \"/etc/hosts\" \"r\"))"
        "  (let (m (foreign-describe h))"
        "    (and (= (ref m 'type) 'ForeignHandle)"
        "         (= (ref m 'runtime) 'c-abi)"
        "         (= (ref m 'ownership) 'manual)"
        "         (= (length (ref m 'capabilities)) 0))))",
        pass, fail);
    test_error_contains(interp, "foreign-release rejects manual return handle",
        "(let (h (fopen \"/etc/hosts\" \"r\"))"
        "  (foreign-release h))",
        "handle is not releasable",
        pass, fail);

    {
        Value* fs_handle_v = make_fs_handle(interp, -1);
        Value* tcp_handle_v = make_tcp_stream_handle(interp, -1);
        Value* udp_handle_v = make_udp_handle(interp, -1);
        Value* process_handle_v = make_process_handle(interp, null);
        TlsHandle* tls_handle = (TlsHandle*)mem::malloc(TlsHandle.sizeof);
        Value* tls_handle_v = null;
        if (tls_handle != null) {
            *tls_handle = {};
            tls_handle_v = make_tls_handle_val(interp, tls_handle, "ffi/tls-handle-out-of-memory", "ffi tls-handle out of memory");
        }

        bool ok = fs_handle_v != null && fs_handle_v.tag == FFI_HANDLE &&
            tcp_handle_v != null && tcp_handle_v.tag == FFI_HANDLE &&
            udp_handle_v != null && udp_handle_v.tag == FFI_HANDLE &&
            process_handle_v != null && process_handle_v.tag == FFI_HANDLE &&
            tls_handle_v != null && tls_handle_v.tag == FFI_HANDLE &&
            ffi_metadata_handle_contract(fs_handle_v, interp, "fs-handle") &&
            ffi_metadata_handle_contract(tcp_handle_v, interp, "tcp-handle") &&
            ffi_metadata_handle_contract(udp_handle_v, interp, "udp-handle") &&
            ffi_metadata_handle_contract(process_handle_v, interp, "process-handle") &&
            ffi_metadata_handle_contract(tls_handle_v, interp, "tls-handle");
        if (ok) {
            emit_pass_literal("[PASS] ffi native wrapper families keep explicit foreign metadata and release cleanly");
            (*pass)++;
        } else {
            io::printn("[FAIL] ffi native wrapper families keep explicit foreign metadata and release cleanly");
            (*fail)++;
        }
    }

    setup(interp, "(define [ffi λ libc] (fclose (^{'name File 'ownership borrowed 'nullability non-null} file)))");
    test_error_contains(interp, "ffi dict fclose rejects nil for non-null handle",
        "(fclose nil)",
        "expected non-null ForeignHandle", pass, fail);

    setup(interp, "(block (define [ffi λ libc] (fopen (^String path) (^String mode)) ^{'type ForeignHandle 'name File 'ownership owned 'finalizer fclose}) (define [ffi λ libc] (free (^{'name SqliteDb 'ownership borrowed 'nullability non-null} p))))");
    test_error_contains(interp, "ffi dict wrong family is rejected",
        "(block (define h (fopen \"/etc/hosts\" \"r\")) (free h))",
        "expected ForeignHandle SqliteDb", pass, fail);

    test_error_contains(interp, "ffi dict unknown key is rejected",
        "(define [ffi λ libc] (fopen (^String path) (^String mode)) ^{'type ForeignHandle 'name File 'ownership owned 'finalizer fclose 'bogus true})",
        "unsupported return type annotation metadata dictionary", pass, fail);
    test_error_contains(interp, "ffi dict invalid ownership value is rejected",
        "(define [ffi λ libc] (fopen (^String path) (^String mode)) ^{'type ForeignHandle 'name File 'ownership lease 'finalizer fclose})",
        "unsupported return type annotation metadata dictionary", pass, fail);
    test_error_contains(interp, "ffi dict owned without finalizer is rejected",
        "(define [ffi λ libc] (fopen (^String path) (^String mode)) ^{'type ForeignHandle 'name File 'ownership owned})",
        "unsupported return type annotation metadata dictionary", pass, fail);
}
