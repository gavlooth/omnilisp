module lisp;

import std::collections::list;
import std::core::mem;
import std::io;
import main;
import lisp::aot;

fn void run_compiler_group_serializer_roundtrip_tests(Interp* interp, usz* pass, usz* fail) {
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

                bool final_manifest_untouched = false;
                if (try generated = io::file::load_temp((String)final_manifest_path)) {
                    final_manifest_untouched = compiler_str_eq(generated, "old manifest");
                }
                compiler_test_finish_bool(
                    !wrote_manifest && final_manifest_untouched,
                    "Compiler: grouped ffi manifest write preserves existing final file on pre-rename failure",
                    "[FAIL] Compiler: grouped ffi manifest write preserves existing final file on pre-rename failure",
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
}
