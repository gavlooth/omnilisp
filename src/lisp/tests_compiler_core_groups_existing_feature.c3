module lisp;

import std::collections::list;
import std::core::mem;
import std::io;
import main;
import lisp::aot;

fn bool compiler_test_parse_exprs_append(char[] source, Interp* interp, List{Expr*}* out) {
    Lexer lex;
    lex.init(source);
    defer lex.destroy();

    Parser p;
    p.init(&lex, interp);
    while (!lex.at_end() && !p.has_error) {
        Expr* expr = p.parse_expr();
        if (expr != null && !p.has_error) out.push(expr);
    }
    return !p.has_error;
}

fn void run_compiler_group_existing_feature_tests(Interp* interp, usz* pass, usz* fail) {
    // --- Existing features still work ---
    {
        char[] code = compile_to_c3("(match (list 1 2 3) ([a b c] (+ a c)))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: match", "[FAIL] Compiler: match", pass, fail);
    }
    {
        char[] code = compile_to_c3("(handle (+ 1 (signal ask 0)) (ask x (resolve 10)))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: handle/perform", "[FAIL] Compiler: handle/perform", pass, fail);
    }
    {
        char[] code = compile_to_c3("(let (x 42) (let (f (lambda () x)) (f)))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: zero-arg closure capture", "[FAIL] Compiler: zero-arg closure capture", pass, fail);
    }
    {
        char[] code = compile_to_c3("(id 5)", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: id", "[FAIL] Compiler: id", pass, fail);
    }
    {
        char[] code = compile_to_c3("(range 5)", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: range", "[FAIL] Compiler: range", pass, fail);
    }
    {
        char[] code = compile_to_c3("(define [ffi lib] libc \"libc.so.6\")", interp);
        bool ok = code.len > 0
            && str_contains(code, "aot::ffi_declare_lib(\"libc\", \"libc.so.6\")")
            && !str_contains(code, "not supported by AOT lowering");
        compiler_test_finish_bool(
            ok,
            "Compiler: declarative ffi lib lowers through AOT runtime bridge",
            "[FAIL] Compiler: declarative ffi lib lowers through AOT runtime bridge",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3("(define [ffi λ libc] (strlen (^String s)) ^Integer)", interp);
        bool ok = code.len > 0
            && str_contains(code, "aot::ffi_declare_fn(\"libc\", \"strlen\", \"strlen\"")
            && !str_contains(code, "not supported by AOT lowering");
        compiler_test_finish_bool(
            ok,
            "Compiler: declarative ffi function lowers through AOT runtime bridge",
            "[FAIL] Compiler: declarative ffi function lowers through AOT runtime bridge",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3(
            "(define [ffi lib] libc \"libc.so.6\") "
            "(define [ffi λ libc] (strlen (^String s)) ^Integer)",
            interp
        );
        bool ok = code.len > 0
            && str_contains(code, "// Startup FFI preload")
            && str_contains(code, "aot::ffi_declare_lib(\"libc\", \"libc.so.6\", true, true)")
            && str_contains(code, "aot::ffi_declare_fn(\"libc\", \"strlen\", \"strlen\"")
            && str_contains(code, ", true, true);")
            && str_contains(code, "aot::aot_process_shutdown(); return 1;");
        compiler_test_finish_bool(
            ok,
            "Compiler: declarative ffi preload emits explicit load policy",
            "[FAIL] Compiler: declarative ffi preload emits explicit load policy",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3(
            "(define [ffi module] libc \"libc.so.6\" "
            "  (strlen (^String s)) ^Integer "
            "  (abs (^Integer n)) ^Integer)",
            interp
        );
        bool ok = code.len > 0
            && str_contains(code, "// Startup FFI preload")
            && str_contains(code, "aot::ffi_declare_lib(\"libc\", \"libc.so.6\", true, true)")
            && str_contains(code, "aot::ffi_declare_fn(\"libc\", \"strlen\", \"strlen\"")
            && str_contains(code, "aot::ffi_declare_fn(\"libc\", \"abs\", \"abs\"")
            && str_contains(code, "\"format\": \"omni.ffi-contract/v1\"")
            && str_contains(code, "\"symbol\": \"strlen\"")
            && str_contains(code, "\"symbol\": \"abs\"");
        compiler_test_finish_bool(
            ok,
            "Compiler: grouped ffi module lowers through declarative FFI AOT path",
            "[FAIL] Compiler: grouped ffi module lowers through declarative FFI AOT path",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3(
            "(module scoped-aot-leak (export leak-value) (define leak-value 41)) "
            "(block (with scoped-aot-leak leak-value) leak-value)",
            interp
        );
        bool ok = code.len > 0
            && compiler_code_contains_all(code, {
                "aot::make_void()",
                "lisp::Value* _aot_with_",
                "aot::lookup_var(\"leak-value\")"
            })
            && compiler_code_contains_none(code, {
                "lisp::Value* leak_value;",
                "leak_value =",
                " = leak_value;",
                "unsupported expr type",
                "not supported by AOT lowering"
            });
        compiler_test_finish_bool(
            ok,
            "Compiler: inline scoped module open does not leak public AOT globals",
            "[FAIL] Compiler: inline scoped module open does not leak public AOT globals",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3("(if true (define nested-global-value 42) nil)", interp);
        bool ok = code.len > 0
            && str_contains(code, "lisp::Value* _omni_sym_nested_global_value_")
            && str_contains(code, "_omni_sym_nested_global_value_")
            && !str_contains(code, "not supported by AOT lowering");
        compiler_test_finish_bool(
            ok,
            "Compiler: nested control-flow definitions declare AOT globals",
            "[FAIL] Compiler: nested control-flow definitions declare AOT globals",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3(
            "(if true "
            "  (block "
            "    (define [ffi lib] libc \"libc.so.6\") "
            "    (define [ffi λ libc] (strlen (^String s)) ^Integer)) "
            "  nil)",
            interp
        );
        bool ok = code.len > 0
            && str_contains(code, "// Startup FFI preload")
            && str_contains(code, "const char[] OMNI_FFI_CONTRACT_JSON = `")
            && str_contains(code, "\"format\": \"omni.ffi-contract/v1\"")
            && str_contains(code, "\"binding\": \"libc\"")
            && str_contains(code, "\"binding\": \"strlen\"")
            && str_contains(code, "aot::ffi_declare_lib(\"libc\", \"libc.so.6\", true, true)")
            && str_contains(code, "aot::ffi_declare_fn(\"libc\", \"strlen\", \"strlen\"");
        compiler_test_finish_bool(
            ok,
            "Compiler: nested control-flow ffi feeds startup preload and embedded contract",
            "[FAIL] Compiler: nested control-flow ffi feeds startup preload and embedded contract",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3(
            "((lambda () "
            "  (block "
            "    (define [ffi lib] tailc \"libc.so.6\") "
            "    (define [ffi λ tailc] (tailstrlen (^String s)) ^Integer)))) "
            "tailstrlen",
            interp
        );
        bool ok = code.len > 0
            && str_contains(code, "aot::ffi_declare_lib(\"tailc\", \"libc.so.6\"")
            && str_contains(code, "tailc = _r")
            && str_contains(code, "aot::ffi_declare_fn(\"tailc\", \"tailstrlen\", \"tailstrlen\"")
            && str_contains(code, "tailstrlen = _r")
            && !str_contains(code, "unsupported expr type");
        compiler_test_finish_bool(
            ok,
            "Compiler: tail block ffi declarations bind direct results",
            "[FAIL] Compiler: tail block ffi declarations bind direct results",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3(
            "(if true (define [ffi lib] branchc \"libc.so.6\") nil) "
            "branchc",
            interp
        );
        bool ok = code.len > 0
            && str_contains(code, "aot::ffi_declare_lib(\"branchc\", \"libc.so.6\"")
            && str_contains(code, "branchc = _r")
            && !str_contains(code, "unsupported expr type");
        compiler_test_finish_bool(
            ok,
            "Compiler: branch ffi declarations bind direct results",
            "[FAIL] Compiler: branch ffi declarations bind direct results",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3(
            "(match 1 (1 (define [ffi lib] matchc \"libc.so.6\")) (_ nil)) "
            "matchc",
            interp
        );
        bool ok = code.len > 0
            && str_contains(code, "aot::ffi_declare_lib(\"matchc\", \"libc.so.6\"")
            && str_contains(code, "matchc = _r")
            && !str_contains(code, "unsupported expr type");
        compiler_test_finish_bool(
            ok,
            "Compiler: match ffi declarations bind direct results",
            "[FAIL] Compiler: match ffi declarations bind direct results",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3(
            "(let (x 1) (define [ffi lib] letc \"libc.so.6\")) "
            "letc",
            interp
        );
        bool ok = code.len > 0
            && str_contains(code, "aot::ffi_declare_lib(\"letc\", \"libc.so.6\"")
            && str_contains(code, "letc = _r")
            && !str_contains(code, "unsupported expr type");
        compiler_test_finish_bool(
            ok,
            "Compiler: let-body ffi declarations bind direct results",
            "[FAIL] Compiler: let-body ffi declarations bind direct results",
            pass,
            fail
        );
    }
    {
        List{Expr*} exprs;
        defer exprs.free();
        bool parsed = compiler_test_parse_exprs_append(
            "(module ffi-opened (export strlen) (with math "
            "  (define [ffi lib] libc \"libc.so.6\") "
            "  (define [ffi λ libc] (strlen (^String s)) ^Integer) "
            "  (strlen \"abc\")))",
            interp,
            &exprs
        );

        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        if (parsed) {
            compiler.emit_ffi_contract_manifest(&exprs);
            compiler.emit_ffi_startup_preload(&exprs);
            char[] code = compiler.get_output();
            bool ok = str_contains(code, "// Startup FFI preload")
                && str_contains(code, "\"format\": \"omni.ffi-contract/v1\"")
                && str_contains(code, "aot::ffi_declare_lib(\"libc\", \"libc.so.6\", true, true)")
                && str_contains(code, "aot::ffi_declare_fn(\"libc\", \"strlen\", \"strlen\"");
            compiler_test_finish_bool(
                ok,
                "Compiler: scoped module open bodies feed declarative ffi preload and manifest discovery",
                "[FAIL] Compiler: scoped module open bodies feed declarative ffi preload and manifest discovery",
                pass,
                fail
            );
        } else {
            compiler_test_finish_bool(
                false,
                "Compiler: scoped module open bodies feed declarative ffi preload and manifest discovery",
                "[FAIL] Compiler: scoped module open bodies feed declarative ffi preload and manifest discovery",
                pass,
                fail
            );
        }
    }
    {
        char[512] manifest_output_path_buf;
        int manifest_pid = compiler_test_getpid();
        usz manifest_seq = g_compiler_bindgen_temp_seq++;
        char[] manifest_output_path;
        if (try rendered = io::bprintf(
            &manifest_output_path_buf,
            "/tmp/omni_compile_with_ffi_manifest-%d-%d.c3",
            manifest_pid,
            (int)manifest_seq
        )) {
            manifest_output_path = rendered;
            char[] manifest_path = "";
            main::EntryFileWriteKind manifest_write_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
            defer {
                if (manifest_path.len > 0) {
                    (void)io::file::delete((String)manifest_path);
                    mem::free(manifest_path.ptr);
                }
            }
            bool wrote_manifest = main::write_compile_ffi_manifest(
                "(with math "
                "  (define [ffi lib] libc \"libc.so.6\") "
                "  (define [ffi λ libc] (strlen (^String s)) ^Integer))",
                "with-ffi.omni",
                manifest_output_path,
                interp,
                &manifest_path,
                &manifest_write_kind
            );
            bool manifest_ok = false;
            if (wrote_manifest && manifest_path.len > 0) {
                if (try generated = io::file::load_temp((String)manifest_path)) {
                    manifest_ok = str_contains(generated, "\"schema\": \"omni-ffi-manifest-v1\"")
                        && str_contains(generated, "\"binding\": \"libc\"")
                        && str_contains(generated, "\"binding\": \"strlen\"");
                }
            }
            compiler_test_finish_bool(
                wrote_manifest && manifest_ok,
                "Compiler: scoped module open ffi emits compile sidecar manifest",
                "[FAIL] Compiler: scoped module open ffi emits compile sidecar manifest",
                pass,
                fail
            );
        } else {
            fail++;
            io::printn("[FAIL] Compiler: scoped module open ffi sidecar manifest path");
        }
    }
    {
        char[512] manifest_output_path_buf;
        int manifest_pid = compiler_test_getpid();
        usz manifest_seq = g_compiler_bindgen_temp_seq++;
        char[] manifest_output_path;
        if (try rendered = io::bprintf(
            &manifest_output_path_buf,
            "/tmp/omni_compile_grouped_ffi_manifest-%d-%d.c3",
            manifest_pid,
            (int)manifest_seq
        )) {
            manifest_output_path = rendered;
            char[] manifest_path = "";
            main::EntryFileWriteKind manifest_write_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
            defer {
                if (manifest_path.len > 0) {
                    (void)io::file::delete((String)manifest_path);
                    mem::free(manifest_path.ptr);
                }
            }
            bool wrote_manifest = main::write_compile_ffi_manifest(
                "(define [ffi module] libc \"libc.so.6\" "
                "  (strlen (^String s)) ^Integer "
                "  (abs (^Integer n)) ^Integer)",
                "grouped-ffi.omni",
                manifest_output_path,
                interp,
                &manifest_path,
                &manifest_write_kind
            );
            bool manifest_ok = false;
            if (wrote_manifest && manifest_path.len > 0) {
                if (try generated = io::file::load_temp((String)manifest_path)) {
                    manifest_ok = str_contains(generated, "\"schema\": \"omni-ffi-manifest-v1\"")
                        && str_contains(generated, "\"binding\": \"libc\"")
                        && str_contains(generated, "\"soname\": \"libc.so.6\"")
                        && str_contains(generated, "\"binding\": \"strlen\"")
                        && str_contains(generated, "\"binding\": \"abs\"");
                }
            }
            compiler_test_finish_bool(
                wrote_manifest && manifest_ok,
                "Compiler: grouped ffi module emits compile sidecar manifest",
                "[FAIL] Compiler: grouped ffi module emits compile sidecar manifest",
                pass,
                fail
            );
        } else {
            fail++;
            io::printn("[FAIL] Compiler: grouped ffi module compose sidecar manifest path");
        }
    }
    {
        char[512] manifest_output_path_buf;
        int manifest_pid = compiler_test_getpid();
        usz manifest_seq = g_compiler_bindgen_temp_seq++;
        char[] manifest_output_path;
        if (try rendered = io::bprintf(
            &manifest_output_path_buf,
            "/tmp/omni_compile_grouped_ffi_manifest_nested-%d-%d.c3",
            manifest_pid,
            (int)manifest_seq
        )) {
            manifest_output_path = rendered;
            char[] manifest_path = "";
            main::EntryFileWriteKind manifest_write_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
            defer {
                if (manifest_path.len > 0) {
                    (void)io::file::delete((String)manifest_path);
                    mem::free(manifest_path.ptr);
                }
            }
            bool wrote_manifest = main::write_compile_ffi_manifest(
                "(block "
                "  (define [ffi module] libc \"libc.so.6\" "
                "    (strlen (^String s)) ^Integer "
                "    (abs (^Integer n)) ^Integer))",
                "grouped-ffi-nested.omni",
                manifest_output_path,
                interp,
                &manifest_path,
                &manifest_write_kind
            );
            bool manifest_ok = false;
            if (wrote_manifest && manifest_path.len > 0) {
                if (try generated = io::file::load_temp((String)manifest_path)) {
                    manifest_ok = str_contains(generated, "\"schema\": \"omni-ffi-manifest-v1\"")
                        && str_contains(generated, "\"binding\": \"libc\"")
                        && str_contains(generated, "\"soname\": \"libc.so.6\"")
                        && str_contains(generated, "\"binding\": \"strlen\"")
                        && str_contains(generated, "\"binding\": \"abs\"");
                }
            }
            compiler_test_finish_bool(
                wrote_manifest && manifest_ok,
                "Compiler: nested grouped ffi module emits compile sidecar manifest",
                "[FAIL] Compiler: nested grouped ffi module emits compile sidecar manifest",
                pass,
                fail
            );
    } else {
        fail++;
        io::printn("[FAIL] Compiler: nested grouped ffi module compose sidecar manifest path");
    }
}
    {
        char[512] manifest_output_path_buf;
        int manifest_pid = compiler_test_getpid();
        usz manifest_seq = g_compiler_bindgen_temp_seq++;
        char[] manifest_output_path;
        if (try rendered = io::bprintf(
            &manifest_output_path_buf,
            "/tmp/omni_compile_grouped_ffi_manifest_escape-%d-%d.c3",
            manifest_pid,
            (int)manifest_seq
        )) {
            manifest_output_path = rendered;
            char[] manifest_path = "";
            main::EntryFileWriteKind manifest_write_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
            defer {
                if (manifest_path.len > 0) {
                    (void)io::file::delete((String)manifest_path);
                    mem::free(manifest_path.ptr);
                }
            }
            bool wrote_manifest = main::write_compile_ffi_manifest(
                "(define [ffi module] libc \"libc.so.6\" (strlen (^String s)) ^Integer)",
                "manifest-\b-\f-\v-source.omni",
                manifest_output_path,
                interp,
                &manifest_path,
                &manifest_write_kind
            );
            bool manifest_ok = false;
            if (wrote_manifest && manifest_path.len > 0) {
                if (try generated = io::file::load_temp((String)manifest_path)) {
                    manifest_ok = str_contains(generated, "\"source\": \"manifest-\\b-\\f-\\u000b-source.omni\"")
                        && str_contains(generated, "\"generated_for\": ");
                }
            }
            compiler_test_finish_bool(
                wrote_manifest && manifest_ok,
                "Compiler: grouped ffi manifest escapes control characters in JSON fields",
                "[FAIL] Compiler: grouped ffi manifest escapes control characters in JSON fields",
                pass,
                fail
            );
        } else {
            fail++;
            io::printn("[FAIL] Compiler: compose escaped manifest output path");
        }
    }
    {
        char[512] manifest_output_path_buf;
        int manifest_pid = compiler_test_getpid();
        usz manifest_seq = g_compiler_bindgen_temp_seq++;
        char[] manifest_output_path;
        if (try rendered = io::bprintf(
            &manifest_output_path_buf,
            "/tmp/omni_compile_macro_ffi_manifest-%d-%d.c3",
            manifest_pid,
            (int)manifest_seq
        )) {
            manifest_output_path = rendered;
            char[] manifest_path = "";
            main::EntryFileWriteKind manifest_write_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
            defer {
                if (manifest_path.len > 0) {
                    (void)io::file::delete((String)manifest_path);
                    mem::free(manifest_path.ptr);
                }
            }
            bool wrote_manifest = main::write_compile_ffi_manifest(
                "(define [macro] emit-ffi "
                "  (syntax-match ([] (template (block "
                "    (define [ffi lib] libc \"libc.so.6\") "
                "    (define [ffi λ libc] (strlen (^String s)) ^Integer)))))) "
                "(emit-ffi)",
                "macro-ffi.omni",
                manifest_output_path,
                interp,
                &manifest_path,
                &manifest_write_kind
            );
            bool manifest_ok = false;
            if (wrote_manifest && manifest_path.len > 0) {
                if (try generated = io::file::load_temp((String)manifest_path)) {
                    manifest_ok = str_contains(generated, "\"binding\": \"libc\"")
                        && str_contains(generated, "\"binding\": \"strlen\"");
                }
            }
            compiler_test_finish_bool(
                wrote_manifest && manifest_ok,
                "Compiler: macro-generated ffi emits compile sidecar manifest",
                "[FAIL] Compiler: macro-generated ffi emits compile sidecar manifest",
                pass,
                fail
            );
        } else {
            fail++;
            io::printn("[FAIL] Compiler: macro-generated ffi sidecar manifest path");
        }
    }
    {
        char[512] manifest_output_path_buf;
        char[576] manifest_path_buf;
        int manifest_pid = compiler_test_getpid();
        usz manifest_seq = g_compiler_bindgen_temp_seq++;
        char[] manifest_output_path;
        char[] expected_manifest_path;
        if (try rendered_output = io::bprintf(
            &manifest_output_path_buf,
            "/tmp/omni_compile_stale_ffi_manifest-%d-%d.c3",
            manifest_pid,
            (int)manifest_seq
        )) {
            manifest_output_path = rendered_output;
            if (try rendered_manifest = io::bprintf(
                &manifest_path_buf,
                "%s.ffi-manifest.json",
                (ZString)manifest_output_path.ptr
            )) {
                expected_manifest_path = rendered_manifest;
                if (try file = io::file::open((String)expected_manifest_path, "w")) {
                    (void)file.write("stale manifest");
                    (void)file.close();
                }
                char[] manifest_path = "";
                main::EntryFileWriteKind manifest_write_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
                bool wrote_manifest = main::write_compile_ffi_manifest(
                    "(+ 1 2)",
                    "no-ffi.omni",
                    manifest_output_path,
                    interp,
                    &manifest_path,
                    &manifest_write_kind
                );
                defer {
                    if (manifest_path.len > 0) {
                        (void)io::file::delete((String)manifest_path);
                        mem::free(manifest_path.ptr);
                    }
                    (void)io::file::delete((String)expected_manifest_path);
                }
                bool stale_removed = false;
                if (catch err = io::file::load_temp((String)expected_manifest_path)) {
                    stale_removed = true;
                }
                compiler_test_finish_bool(
                    wrote_manifest && manifest_path.len == 0 && stale_removed,
                    "Compiler: no-ffi compile removes stale sidecar manifest",
                    "[FAIL] Compiler: no-ffi compile removes stale sidecar manifest",
                    pass,
                    fail
                );
            } else {
                fail++;
                io::printn("[FAIL] Compiler: stale ffi manifest compose expected path");
            }
        } else {
            fail++;
            io::printn("[FAIL] Compiler: stale ffi manifest compose output path");
        }
    }
    {
        char[512] manifest_output_path_buf;
        char[576] manifest_path_buf;
        int manifest_pid = compiler_test_getpid();
        usz manifest_seq = g_compiler_bindgen_temp_seq++;
        char[] manifest_output_path;
        char[] expected_manifest_path;
        if (try rendered_output = io::bprintf(
            &manifest_output_path_buf,
            "/tmp/omni_compile_empty_stale_ffi_manifest-%d-%d.c3",
            manifest_pid,
            (int)manifest_seq
        )) {
            manifest_output_path = rendered_output;
            if (try rendered_manifest = io::bprintf(
                &manifest_path_buf,
                "%s.ffi-manifest.json",
                (ZString)manifest_output_path.ptr
            )) {
                expected_manifest_path = rendered_manifest;
                if (try file = io::file::open((String)expected_manifest_path, "w")) {
                    (void)file.write("stale manifest");
                    (void)file.close();
                }
                char[] manifest_path = "";
                main::EntryFileWriteKind manifest_write_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
                bool wrote_manifest = main::write_compile_ffi_manifest(
                    "",
                    "empty.omni",
                    manifest_output_path,
                    interp,
                    &manifest_path,
                    &manifest_write_kind
                );
                defer {
                    if (manifest_path.len > 0) {
                        (void)io::file::delete((String)manifest_path);
                        mem::free(manifest_path.ptr);
                    }
                    (void)io::file::delete((String)expected_manifest_path);
                }
                bool stale_removed = false;
                if (catch err = io::file::load_temp((String)expected_manifest_path)) {
                    stale_removed = true;
                }
                compiler_test_finish_bool(
                    wrote_manifest && manifest_path.len == 0 && stale_removed,
                    "Compiler: empty compile removes stale sidecar manifest",
                    "[FAIL] Compiler: empty compile removes stale sidecar manifest",
                    pass,
                    fail
                );
            } else {
                fail++;
                io::printn("[FAIL] Compiler: empty stale ffi manifest compose expected path");
            }
        } else {
            fail++;
            io::printn("[FAIL] Compiler: empty stale ffi manifest compose output path");
        }
    }
    {
        char[512] manifest_output_path_buf;
        char[576] manifest_path_buf;
        int manifest_pid = compiler_test_getpid();
        usz manifest_seq = g_compiler_bindgen_temp_seq++;
        char[] manifest_output_path;
        char[] expected_manifest_path;
        if (try rendered_output = io::bprintf(
            &manifest_output_path_buf,
            "/tmp/omni_compile_unlink_fail_ffi_manifest-%d-%d.c3",
            manifest_pid,
            (int)manifest_seq
        )) {
            manifest_output_path = rendered_output;
            if (try rendered_manifest = io::bprintf(
                &manifest_path_buf,
                "%s.ffi-manifest.json",
                (ZString)manifest_output_path.ptr
            )) {
                expected_manifest_path = rendered_manifest;
                int mkdir_rc = compiler_test_mkdir((char*)expected_manifest_path.ptr, 0o755);
                char[] manifest_path = "";
                main::EntryFileWriteKind manifest_write_kind = ENTRY_FILE_WRITE_OK;
                bool wrote_manifest = main::write_compile_ffi_manifest(
                    "(+ 1 2)",
                    "no-ffi-unlink-fail.omni",
                    manifest_output_path,
                    interp,
                    &manifest_path,
                    &manifest_write_kind
                );
                defer {
                    if (manifest_path.len > 0) {
                        mem::free(manifest_path.ptr);
                    }
                    (void)compiler_test_rmdir((char*)expected_manifest_path.ptr);
                }
                compiler_test_finish_bool(
                    mkdir_rc == 0
                        && !wrote_manifest
                        && manifest_path.len > 0
                        && manifest_write_kind != ENTRY_FILE_WRITE_OK,
                    "Compiler: no-ffi compile reports stale sidecar removal failure",
                    "[FAIL] Compiler: no-ffi compile reports stale sidecar removal failure",
                    pass,
                    fail
                );
            } else {
                fail++;
                io::printn("[FAIL] Compiler: stale ffi unlink failure compose expected path");
            }
        } else {
            fail++;
            io::printn("[FAIL] Compiler: stale ffi unlink failure compose output path");
        }
    }
    {
        char[512] manifest_output_path_buf;
        int manifest_pid = compiler_test_getpid();
        usz manifest_seq = g_compiler_bindgen_temp_seq++;
        char[] manifest_output_path;
        if (try rendered = io::bprintf(
            &manifest_output_path_buf,
            "/tmp/omni_compile_nested_ffi_manifest-%d-%d.c3",
            manifest_pid,
            (int)manifest_seq
        )) {
            manifest_output_path = rendered;
            char[] manifest_path = "";
            main::EntryFileWriteKind manifest_write_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
            defer {
                if (manifest_path.len > 0) {
                    (void)io::file::delete((String)manifest_path);
                    mem::free(manifest_path.ptr);
                }
            }
            bool wrote_manifest = main::write_compile_ffi_manifest(
                "(if true "
                "  (block "
                "    (define [ffi lib] libc \"libc.so.6\") "
                "    (define [ffi λ libc] (strlen (^String s)) ^Integer)) "
                "  nil)",
                "nested-ffi.omni",
                manifest_output_path,
                interp,
                &manifest_path,
                &manifest_write_kind
            );
            bool manifest_ok = false;
            if (wrote_manifest && manifest_path.len > 0) {
                if (try generated = io::file::load_temp((String)manifest_path)) {
                    manifest_ok = str_contains(generated, "\"binding\": \"libc\"")
                        && str_contains(generated, "\"binding\": \"strlen\"");
                }
            }
            compiler_test_finish_bool(
                wrote_manifest && manifest_ok,
                "Compiler: nested control-flow ffi emits compile sidecar manifest",
                "[FAIL] Compiler: nested control-flow ffi emits compile sidecar manifest",
                pass,
                fail
            );
        } else {
            fail++;
            io::printn("[FAIL] Compiler: nested control-flow ffi sidecar manifest path");
        }
    }
    {
        char[] code = compile_to_c3(
            "(define [ffi lib] libc \"libc.so.6\") "
            "(define [ffi λ libc] (strlen (^String s)) ^Integer) "
            "(define [ffi λ libc] (strchr (^String s) (^Integer c)) ^ForeignHandle) "
            "(define [ffi λ libc] (isalpha (^Integer c)) ^Boolean) "
            "(define [ffi λ libc] (cos (^Float64 x)) ^Float64)",
            interp
        );
        bool ok = code.len > 0
            && str_contains(code, "const char[] OMNI_FFI_CONTRACT_JSON = `")
            && str_contains(code, "\"format\": \"omni.ffi-contract/v1\"")
            && str_contains(code, "\"libraries\": [")
            && str_contains(code, "\"functions\": [")
            && str_contains(code, "\"binding\": \"libc\"")
            && str_contains(code, "\"soname\": \"libc.so.6\"")
            && str_contains(code, "\"eager_load\": true")
            && str_contains(code, "\"required_library\": true")
            && str_contains(code, "\"symbol\": \"strlen\"")
            && str_contains(code, "\"eager_resolve\": true")
            && str_contains(code, "\"required_symbol\": true")
            && str_contains(code, "\"params\": [")
            && str_contains(code, "\"String\"")
            && str_contains(code, "\"ForeignHandle\"")
            && str_contains(code, "\"Boolean\"")
            && str_contains(code, "\"Float64\"");
        compiler_test_finish_bool(
            ok,
            "Compiler: declarative ffi emits manifest contract with loading policy and ABI tags",
            "[FAIL] Compiler: declarative ffi emits manifest contract with loading policy and ABI tags",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3(
            "(define [ffi lib] libc \"libc.so.6\") "
            "(define [ffi λ libc] (mixed_abi (^Float32 f) (^Int32 i) (^Int64 j) (^UInt32 u) (^UInt64 v) (^Buffer b) (^Struct s)) ^Float32)",
            interp
        );
        bool ok = code.len > 0
            && str_contains(code, "\"Float32\"")
            && str_contains(code, "\"Int32\"")
            && str_contains(code, "\"Int64\"")
            && str_contains(code, "\"UInt32\"")
            && str_contains(code, "\"UInt64\"")
            && str_contains(code, "\"Buffer\"")
            && str_contains(code, "\"Struct\"")
            && !str_contains(code, "\"params\": [\"Void\"");
        compiler_test_finish_bool(
            ok,
            "Compiler: ffi manifest preserves extended ABI tag names",
            "[FAIL] Compiler: ffi manifest preserves extended ABI tag names",
            pass,
            fail
        );
    }
    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        char[4] json_string = { 'a', (char)8, (char)1, '`' };
        compiler.emit_json_string(json_string[..]);
        char[] code = compiler.get_output();
        bool ok = compiler_str_eq(code, "\"a\\b\\u0001\\u0060\"");
        compiler_test_finish_bool(
            ok,
            "Compiler: ffi manifest JSON string emitter escapes control characters",
            "[FAIL] Compiler: ffi manifest JSON string emitter escapes control characters",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3(
            "(define [ffi λ libc] (strchr (^String s) (^Integer c)) ^ForeignHandle) "
            "(define [ffi λ libc] (isalpha (^Integer c)) ^Boolean) "
            "(define [ffi λ libc] (cos (^Float64 x)) ^Float64)",
            interp
        );
        bool ok = code.len > 0
            && !str_contains(code, "unsupported")
            && str_contains(code, "aot::ffi_declare_fn(\"libc\", \"strchr\", \"strchr\"")
            && str_contains(code, "aot::ffi_declare_fn(\"libc\", \"isalpha\", \"isalpha\"")
            && str_contains(code, "aot::ffi_declare_fn(\"libc\", \"cos\", \"cos\"");
        compiler_test_finish_bool(
            ok,
            "Compiler: declarative ffi lowers foreign-handle/bool/double ABI tags",
            "[FAIL] Compiler: declarative ffi lowers foreign-handle/bool/double ABI tags",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3(
            "(define [ffi λ libc] (fopen (^String path) (^String mode)) ^{'name File 'ownership owned 'finalizer fclose}) "
            "(define [ffi λ libc] (fflush (^{'name File 'ownership borrowed 'nullability non-null} file)) ^Integer)",
            interp
        );
        bool ok = code.len > 0
            && str_contains(code, "lisp::aot::AotFfiHandlePolicySpec")
            && str_contains(code, ".name = \"File\";")
            && str_contains(code, ".ownership = 1;")
            && str_contains(code, ".nullability = 1;")
            && str_contains(code, ".finalizer = \"fclose\";")
            && str_contains(code, "aot::ffi_declare_fn(\"libc\", \"fopen\", \"fopen\"")
            && str_contains(code, "aot::ffi_declare_fn(\"libc\", \"fflush\", \"fflush\"")
            && str_contains(code, ", true, true, ")
            && str_contains(code, "&_ffi_return_policy_");
        compiler_test_finish_bool(
            ok,
            "Compiler: declarative ffi lowers ForeignHandle metadata policy descriptors into AOT bridge",
            "[FAIL] Compiler: declarative ffi lowers ForeignHandle metadata policy descriptors into AOT bridge",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3(
            "(define [ffi λ libc] (bad (^{'name File 'ownership owned 'finalizer fclose} file)) ^Integer)",
            interp
        );
        bool ok = code.len == 0;
        compiler_test_finish_bool(
            ok,
            "Compiler: declarative ffi rejects owned ForeignHandle parameter policy in AOT lowering",
            "[FAIL] Compiler: declarative ffi rejects owned ForeignHandle parameter policy in AOT lowering",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3("(define soname \"libc.so.6\") (define [ffi lib] libc soname)", interp);
        bool ok = code.len == 0;
        compiler_test_finish_bool(
            ok,
            "Compiler: declarative ffi lib fails closed for non-literal soname in AOT mode",
            "[FAIL] Compiler: declarative ffi lib fails closed for non-literal soname in AOT mode",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3("(define [ffi λ libc variadic] (printf (^String fmt)) ^Integer)", interp);
        bool ok = code.len == 0;
        compiler_test_finish_bool(
            ok,
            "Compiler: declarative variadic ffi function is rejected in AOT mode",
            "[FAIL] Compiler: declarative variadic ffi function is rejected in AOT mode",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3(
            "(module ui (export open) (define [effect] (open (^Any payload)))) "
            "(import ui) "
            "(signal ui.open 1)",
            interp
        );
        bool ok = str_contains(code, "aot::compiled_signal(")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        compiler_test_finish_bool(
            ok,
            "Compiler: module-qualified signal lowers directly",
            "[FAIL] Compiler: module-qualified signal lowers directly",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3(
            "(module ui (export open) (define [effect] (open (^Any payload)))) "
            "(import ui) "
            "(handle (signal ui.open 1) (ui.open x (resolve x)))",
            interp
        );
        bool ok = str_contains(code, "aot::compiled_signal(")
            && str_contains(code, "aot::compiled_handle(")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        compiler_test_finish_bool(
            ok,
            "Compiler: module-qualified handle lowers directly",
            "[FAIL] Compiler: module-qualified handle lowers directly",
            pass,
            fail
        );
    }
}
