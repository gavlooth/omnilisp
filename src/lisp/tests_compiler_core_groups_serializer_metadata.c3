module lisp;

import std::collections::list;
import std::core::mem;
import std::io;
import main;
import lisp::aot;

fn void run_compiler_group_serializer_roundtrip_tests(Interp* interp, usz* pass, usz* fail) {
    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        List{char} buf;
        defer buf.free();
        compiler.serialize_value_to_buf(make_int(interp, long.min), &buf);

        char[] actual = "";
        if (buf.len() > 0) {
            actual = buf.entries[:buf.len()];
        }
        bool ok = compiler_str_eq(actual, "-9223372036854775808");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: integer serializer preserves long.min"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: integer serializer preserves long.min"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(lambda (x) x)",
            "(\xCE\xBB (x) x)"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: lambda serializer emits canonical unicode spelling"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: lambda serializer emits canonical unicode spelling"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(import \"pkg/math.omni\" ((sum 'as plus) diff))",
            "(import \"pkg/math.omni\" ((sum 'as plus) diff))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: import serializer preserves path and alias syntax"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: import serializer preserves path and alias syntax"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(handle (+ 1 (signal ask 0)) (ask x (resolve x)))",
            "(handle (+ 1 (perform ask 0)) (ask x (resolve x)))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: handle serializer emits parser-compatible clause syntax"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: handle serializer emits parser-compatible clause syntax"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(match (list 1 2) ([a b] (+ a b)) (_ 0))",
            "(match (list 1 2) ([a b] (+ a b)) (_ 0))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: match serializer preserves clause parens"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: match serializer preserves clause parens"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(match (cons 1 2) ((a . b) a) (_ 0))",
            "(match (cons 1 2) ((a . b) a) (_ 0))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: match serializer preserves cons patterns"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: match serializer preserves cons patterns"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(match (Dictionary 'name 7) ({name} name) (_ 0))",
            "(match (Dictionary 'name 7) ({name} name) (_ 0))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: match serializer preserves dict patterns"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: match serializer preserves dict patterns"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(match (Some 1) ((Some x) x) (None 0))",
            "(match (Some 1) ((Some x) x) (None 0))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: match serializer preserves constructor patterns"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: match serializer preserves constructor patterns"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(match (Some 7) ((? (> x 0) (Some x)) x) (_ 0))",
            "(match (Some 7) ((? (> x 0) (Some x)) x) (_ 0))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: match serializer preserves guarded patterns with subpatterns"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: match serializer preserves guarded patterns with subpatterns"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(match 7 ((? (> x 0)) 1) (_ 0))",
            "(match 7 ((? (> x 0)) 1) (_ 0))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: match serializer preserves guard-only patterns"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: match serializer preserves guard-only patterns"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(match (Some 7) ((? (> x 0) (? (> x 0) (Some x))) x) (_ 0))",
            "(match (Some 7) ((? (> x 0) (? (> x 0) (Some x))) x) (_ 0))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: match serializer preserves nested guarded patterns"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: match serializer preserves nested guarded patterns"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(signal ui.open 1)",
            "(perform ui.open 1)"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: serializer preserves module-qualified signal tag paths"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: serializer preserves module-qualified signal tag paths"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(handle (signal ui.open 1) (ui.open x (resolve x)))",
            "(handle (perform ui.open 1) (ui.open x (resolve x)))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: serializer preserves module-qualified handle tag paths"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: serializer preserves module-qualified handle tag paths"); }
    }
}

fn void run_compiler_group_foreign_handle_metadata_dict_tests(Interp* interp, usz* pass, usz* fail) {
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(define [ffi λ libc] (fopen (^String path) (^String mode)) ^{'type ForeignHandle 'name File 'ownership owned 'finalizer fclose})",
            "(define [ffi λ libc] (fopen (^String path) (^String mode)) ^{'type ForeignHandle 'name File 'ownership owned 'finalizer fclose})"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: serializer preserves canonical ForeignHandle metadata dict return annotations"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: serializer preserves canonical ForeignHandle metadata dict return annotations"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(define [ffi λ libc] (strchr (^String s) (^Integer c)) ^ForeignHandle)",
            "(define [ffi λ libc] (strchr (^String s) (^Integer c)) ^ForeignHandle)"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: serializer preserves standalone ForeignHandle return annotations"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: serializer preserves standalone ForeignHandle return annotations"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(define [ffi λ libc] (fclose (^{'name File 'ownership borrowed 'nullability non-null} file)) ^Integer)",
            "(define [ffi λ libc] (fclose (^{'name File 'ownership borrowed 'nullability non-null} file)) ^Integer)"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: serializer preserves ForeignHandle metadata dict parameters"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: serializer preserves ForeignHandle metadata dict parameters"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(define [ffi λ libc] (strtol (^String s) (^{'name File 'ownership borrowed 'nullability non-null} endptr) (^Integer base)) ^Integer)",
            "(define [ffi λ libc] (strtol (^String s) (^{'name File 'ownership borrowed 'nullability non-null} endptr) (^Integer base)) ^Integer)"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: serializer preserves ForeignHandle metadata dict optional-output annotations"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: serializer preserves ForeignHandle metadata dict optional-output annotations"); }
    }
    {
        char[512] manifest_output_path_buf;
        int manifest_pid = compiler_test_getpid();
        usz manifest_seq = g_compiler_bindgen_temp_seq++;
        char[] manifest_output_path;
        if (try rendered = io::bprintf(
            &manifest_output_path_buf,
            "/tmp/omni_compile_manifest_atomic-%d-%d.c3",
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

            char[512] final_manifest_path_buf;
            char[] final_manifest_path;
            if (try final_rendered = io::bprintf(
                &final_manifest_path_buf,
                "%s.ffi-manifest.json",
                (ZString)manifest_output_path
            )) {
                final_manifest_path = final_rendered;
                defer (void)io::file::delete((String)final_manifest_path);

                if (try file = io::file::open((String)final_manifest_path, "w")) {
                    defer (void)file.close();
                    (void)file.write("old manifest");
                }

                main::g_compile_manifest_force_fail_before_rename = true;
                bool wrote_manifest = main::write_compile_ffi_manifest(
                    "(define [ffi module] libc \"libc.so.6\" (strlen (^String s)) ^Integer)",
                    "manifest-\b-\f-\v-source.omni",
                    manifest_output_path,
                    interp,
                    &manifest_path,
                    &manifest_write_kind
                );
                main::g_compile_manifest_force_fail_before_rename = false;

                bool final_manifest_removed = false;
                if (catch err = io::file::load_temp((String)final_manifest_path)) {
                    final_manifest_removed = true;
                }
                compiler_test_finish_bool(
                    !wrote_manifest && final_manifest_removed,
                    "Compiler: grouped ffi manifest write removes stale final file on pre-rename failure",
                    "[FAIL] Compiler: grouped ffi manifest write removes stale final file on pre-rename failure",
                    pass,
                    fail
                );
            } else {
                fail++;
                io::printn("[FAIL] Compiler: compose atomic manifest final path");
            }
        } else {
            fail++;
            io::printn("[FAIL] Compiler: compose atomic manifest output path");
        }
    }
    {
        int manifest_pid = compiler_test_getpid();
        usz manifest_seq = g_compiler_bindgen_temp_seq++;
        char[512] manifest_path_buf;
        if (try manifest_rendered = io::bprintf(
            &manifest_path_buf,
            "/tmp/omni_compile_output_commit_fail-%d-%d.c3.ffi-manifest.json",
            manifest_pid,
            (int)manifest_seq
        )) {
            char[] manifest_path = manifest_rendered;
            defer (void)io::file::delete((String)manifest_path);
            bool setup = compiler_write_text_file(manifest_path, "stale manifest");
            main::EntryFileWriteKind manifest_cleanup_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
            bool cleanup_ok = main::cleanup_compile_sidecar_on_output_commit_failure(manifest_path, &manifest_cleanup_kind);

            bool manifest_missing = false;
            if (try ignored = io::file::load_temp((String)manifest_path)) {
                (void)ignored;
                manifest_missing = false;
            } else {
                manifest_missing = true;
            }
            compiler_test_finish_bool(
                setup && cleanup_ok && manifest_missing,
                "Compiler: compile output commit failure removes sidecar manifest",
                "[FAIL] Compiler: compile output commit failure removes sidecar manifest",
                pass,
                fail
            );
        } else {
            fail++;
            io::printn("[FAIL] Compiler: compose output commit failure manifest path");
        }
    }
    {
        char[512] manifest_path_buf;
        int manifest_pid = compiler_test_getpid();
        usz manifest_seq = g_compiler_bindgen_temp_seq++;
        if (try manifest_rendered = io::bprintf(
            &manifest_path_buf,
            "/tmp/omni_compile_output_commit_cleanup_fail-%d-%d.c3.ffi-manifest.json",
            manifest_pid,
            (int)manifest_seq
        )) {
            char[] manifest_path = manifest_rendered;
            bool setup = compiler_write_text_file(manifest_path, "stale manifest");
            main::EntryFileWriteKind manifest_cleanup_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
            main::g_compile_runner_force_sidecar_cleanup_fail = true;
            bool cleanup_ok = main::cleanup_compile_sidecar_on_output_commit_failure(manifest_path, &manifest_cleanup_kind);
            main::g_compile_runner_force_sidecar_cleanup_fail = false;
            defer (void)io::file::delete((String)manifest_path);
            compiler_test_finish_bool(
                setup && !cleanup_ok && manifest_cleanup_kind != ENTRY_FILE_WRITE_OK,
                "Compiler: compile output commit failure reports sidecar cleanup failure",
                "[FAIL] Compiler: compile output commit failure reports sidecar cleanup failure",
                pass,
                fail
            );
        } else {
            fail++;
            io::printn("[FAIL] Compiler: compose output commit cleanup failure manifest path");
        }
    }
    {
        char[512] output_path_buf;
        char[512] temp_path_buf;
        int output_pid = compiler_test_getpid();
        usz output_seq = g_compiler_bindgen_temp_seq++;
        if (try output_rendered = io::bprintf(
            &output_path_buf,
            "/tmp/omni_build_publish-%d-%d.bin",
            output_pid,
            (int)output_seq
        )) {
            char[] output_path = output_rendered;
            char[] temp_path = "";
            if (main::entry_build_atomic_temp_path(output_path, temp_path_buf[..], &temp_path)) {
                bool staged_ok = compiler_write_text_file(temp_path, "new binary");
                main::EntryFileWriteKind publish_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
                bool publish_ok = false;
                bool published = false;
                defer {
                    if (temp_path.len > 0) {
                        (void)io::file::delete((String)temp_path);
                    }
                    (void)io::file::delete((String)output_path);
                }

                publish_ok = main::entry_commit_staged_text(temp_path, output_path, &publish_kind);
                if (try generated = io::file::load_temp((String)output_path)) {
                    published = compiler_str_eq(generated, "new binary");
                }
                compiler_test_finish_bool(
                    staged_ok && publish_ok && published && publish_kind == ENTRY_FILE_WRITE_OK,
                    "Compiler: build output publishes staged binary on backend success",
                    "[FAIL] Compiler: build output publishes staged binary on backend success",
                    pass,
                    fail
                );
            } else {
                fail++;
                io::printn("[FAIL] Compiler: compose staged build output path");
            }
        } else {
            fail++;
            io::printn("[FAIL] Compiler: compose build output path");
        }
    }
}
