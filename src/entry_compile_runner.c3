module main;

import std::io;
import lisp;

bool g_compile_runner_force_output_commit_fail = false;
bool g_compile_runner_force_sidecar_cleanup_fail = false;

fn bool cleanup_compile_sidecar_on_output_commit_failure(char[] manifest_path, EntryFileWriteKind* kind_out = null) {
    if (manifest_path.len == 0) return true;
    if (g_compile_runner_force_sidecar_cleanup_fail) {
        if (kind_out != null) *kind_out = ENTRY_FILE_WRITE_GENERIC_FAILURE;
        return false;
    }
    char[4096] manifest_path_buf;
    char[] manifest_path_cstr = "";
    if (!entry_copy_path_cstr(manifest_path, manifest_path_buf[..], &manifest_path_cstr)) {
        if (kind_out != null) *kind_out = ENTRY_FILE_WRITE_PATH_FAILURE;
        return false;
    }
    if (entry_file_uv_unlink((ZString)manifest_path_cstr.ptr) == 0) return true;

    EntryFileReadKind read_kind = ENTRY_FILE_READ_GENERIC_FAILURE;
    if (!classify_file_read_path(manifest_path, &read_kind) && read_kind == ENTRY_FILE_READ_NOT_FOUND) {
        return true;
    }

    if (kind_out != null) {
        *kind_out = compile_manifest_unlink_failure_kind(manifest_path);
        if (*kind_out == ENTRY_FILE_WRITE_GENERIC_FAILURE) *kind_out = classify_file_write_errno();
        return false;
    }
    return false;
}

fn int run_compile_mode(char* input_file, char* output_file) {
    if (input_file == null || cli_arg_starts_with_dash(input_file)) {
        if (input_file != null) {
            io::printfn("Error: missing compile input file before option %s", (ZString)input_file);
        }
        return print_compile_usage();
    }
    if (output_file == null || cli_arg_starts_with_dash(output_file)) {
        if (output_file != null) {
            io::printfn("Error: missing compile output file before option %s", (ZString)output_file);
        }
        return print_compile_usage();
    }

    char[] input_path;
    if (!cli_cstr_slice(input_file, &input_path)) return print_cli_cstr_too_long("--compile input file");
    char[] output_path;
    if (!cli_cstr_slice(output_file, &output_path)) return print_cli_cstr_too_long("--compile output file");
    if (try source = io::file::load_temp((String)input_path)) {
        EntryFileWriteKind output_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
        if (!validate_output_target_path(output_file, &output_kind)) {
            print_compile_write_error(output_path, output_kind);
            return 1;
        }

        print_compile_start(input_file, output_file);
        thread_registry_init();

        lisp::Interp* interp = create_plain_interp();
        if (interp == null) {
            print_compile_alloc_error();
            thread_registry_shutdown();
            return 1;
        }

        if (!lisp::bootstrap_runtime_interp(interp)) {
            destroy_plain_interp(interp);
            thread_registry_shutdown();
            return 1;
        }

        char[] compile_error = "";
        char[] c3_code = lisp::compile_to_c3(source, interp, &compile_error);
        if (c3_code.len == 0) {
            if (compile_error.len > 0) print_compile_error_message(compile_error);
            destroy_plain_interp(interp);
            thread_registry_shutdown();
            return 1;
        }

        char[4160] output_temp_path_buf;
        char[] output_temp_path = "";
        EntryFileWriteKind write_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
        if (!entry_write_text_staged(output_path, c3_code, output_temp_path_buf[..], &output_temp_path, &write_kind)) {
            print_compile_write_error(output_path, write_kind);
            destroy_plain_interp(interp);
            thread_registry_shutdown();
            return 1;
        }
        bool output_committed = false;
        defer if (!output_committed) entry_cleanup_staged_text(output_temp_path);

        char[] manifest_path = "";
        EntryFileWriteKind manifest_write_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
        defer if (manifest_path.len > 0) mem::free(manifest_path.ptr);
        if (!write_compile_ffi_manifest(source, input_path, output_path, interp, &manifest_path, &manifest_write_kind)) {
            if (manifest_path.len > 0) {
                print_compile_write_error(manifest_path, manifest_write_kind);
            } else {
                print_compile_write_error(output_path, manifest_write_kind);
            }
            destroy_plain_interp(interp);
            thread_registry_shutdown();
            return 1;
        }

        if (g_compile_runner_force_output_commit_fail || !entry_commit_staged_text(output_temp_path, output_path, &write_kind)) {
            EntryFileWriteKind manifest_cleanup_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
            if (!cleanup_compile_sidecar_on_output_commit_failure(manifest_path, &manifest_cleanup_kind)) {
                print_compile_write_error(manifest_path, manifest_cleanup_kind);
            }
            print_compile_write_error(output_path, write_kind);
            destroy_plain_interp(interp);
            thread_registry_shutdown();
            return 1;
        }
        output_committed = true;

        print_compile_success(output_file);
        destroy_plain_interp(interp);
        thread_registry_shutdown();
        return 0;
    } else {
        EntryFileReadKind read_kind = ENTRY_FILE_READ_GENERIC_FAILURE;
        if (!classify_file_read_path(input_path, &read_kind)) {
            return print_compile_read_error(input_path, read_kind);
        }
        return print_compile_read_error(input_path, ENTRY_FILE_READ_GENERIC_FAILURE);
    }
}
