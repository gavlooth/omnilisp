module lisp;

import std::core::mem;
import std::io;
import main;

fn Value* prim_read_file(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return io_file_raise(interp, "io/read-file-expected-path", "read-file: expected (read-file path)");
    }

    Value* runtime_err = io_file_require_async_runtime(
        interp,
        "read-file",
        "io/read-file-fiber-required",
        "io/read-file-libuv-unavailable"
    );
    if (runtime_err != null) return runtime_err;

    char[] path = args[0].str_chars[:args[0].str_len];
    return io_async_read_file_value(path, interp);
}

/**
 * (write-file path content) -> Void on success
 */
fn Value* prim_write_file(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_string(args[0]) || !is_string(args[1])) {
        return io_file_raise(interp, "io/write-file-expected-path-content", "write-file: expected (write-file path content)");
    }

    Value* runtime_err = io_file_require_async_runtime(
        interp,
        "write-file",
        "io/write-file-fiber-required",
        "io/write-file-libuv-unavailable"
    );
    if (runtime_err != null) return runtime_err;

    char[] path = args[0].str_chars[:args[0].str_len];
    char[] content = args[1].str_chars[:args[1].str_len];

    usz payload_len = path.len + 1 + content.len;
    char* payload = (char*)mem::malloc(payload_len + 1);
    if (payload == null) return io_file_raise(interp, "io/write-file-out-of-memory", "write-file: out of memory");
    for (usz i = 0; i < path.len; i++) payload[i] = path[i];
    payload[path.len] = 0;
    for (usz i = 0; i < content.len; i++) payload[path.len + 1 + i] = content[i];
    payload[payload_len] = 0;

    OffloadWork work = {};
    work.op = OFFLOAD_WRITE_FILE;
    work.shared = scheduler_domain_publish_shared_owned(scheduler_current_domain(), payload, payload_len);
    if (work.shared == null) {
        mem::free(payload);
        return io_file_raise(interp, "io/write-file-out-of-memory", "write-file: out of memory");
    }

    Value* out = io_run_fiber_offload(&work, interp);
    if (out != null && out.tag == ERROR) {
        return io_file_wrap_runtime_error(interp, out, "io/write-file-failed", "write-file: write failed");
    }
    if (is_int(out) && out.int_val >= 0) return make_void(interp);
    return io_file_raise(interp, "io/write-file-failed", "write-file: write failed");
}

/**
 * (file-exists? path) -> true if file exists, nil otherwise
 */
fn Value* prim_file_exists(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return io_file_raise(interp, "io/file-exists?-expected-path", "file-exists?: expected (file-exists? path)");
    }

    Value* runtime_err = io_file_require_async_runtime(
        interp,
        "file-exists?",
        "io/file-exists?-fiber-required",
        "io/file-exists?-libuv-unavailable"
    );
    if (runtime_err != null) return runtime_err;

    char[] path = args[0].str_chars[:args[0].str_len];

    OffloadWork work = {};
    work.op = OFFLOAD_FILE_EXISTS;
    work.shared = scheduler_domain_publish_shared(scheduler_current_domain(), path);
    if (work.shared == null) return io_file_raise(interp, "io/file-exists?-out-of-memory", "file-exists?: out of memory");

    Value* out = io_run_fiber_offload(&work, interp);
    if (out != null && out.tag == ERROR) return out;
    if (is_int(out) && out.int_val != 0) return make_symbol(interp, interp.sym_true);
    if (is_int(out) && out.int_val == 0) return make_nil(interp);
    if (out == null || is_nil(out)) return make_nil(interp);
    return io_file_raise(interp, "io/file-exists?-failed", "file-exists?: existence probe failed");
}

/**
 * (read-lines path) -> list of strings (one per line)
 */
fn Value* prim_read_lines(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return io_file_raise(interp, "io/read-lines-expected-path", "read-lines: expected (read-lines path)");
    }

    Value* runtime_err = io_file_require_async_runtime(
        interp,
        "read-lines",
        "io/read-lines-fiber-required",
        "io/read-lines-libuv-unavailable"
    );
    if (runtime_err != null) return runtime_err;

    char[] path = args[0].str_chars[:args[0].str_len];
    IoReadFileResult read_result = io_async_read_file_result(path, interp);
    if (read_result.status == IO_READ_FILE_OUT_OF_MEMORY) {
        return io_file_raise(interp, "io/read-lines-out-of-memory", "read-lines: out of memory");
    }
    if (read_result.status == IO_READ_FILE_RUNTIME_ERROR) return read_result.value;
    if (read_result.status != IO_READ_FILE_OK || !is_string(read_result.value)) {
        return io_file_raise(interp, "io/read-lines-failed", "read-lines: read failed");
    }

    Value* content_v = read_result.value;
    char[] content = content_v.str_chars[:content_v.str_len];
    usz content_len = content.len;

    Value* lines = make_nil(interp);
    usz start = 0;
    for (usz i = 0; i <= content_len; i++) {
        if (i == content_len || content[i] == '\n') {
            usz line_len = i - start;
            if (line_len > 0 && content[start + line_len - 1] == '\r') {
                line_len--;
            }
            Value* line = make_string(interp, content[start:line_len]);
            lines = make_cons(interp, line, lines);
            start = i + 1;
        }
    }

    Value* result = make_nil(interp);
    while (is_cons(lines)) {
        result = make_cons(interp, lines.cons_val.car, result);
        lines = lines.cons_val.cdr;
    }
    return result;
}

/**
 * (load path) -> evaluates all expressions in the file, returns last result
 * Reads a script file and evaluates it in the current environment.
 */
fn Value* prim_load(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return raise_error(interp, "load: expected a string path");
    }

    char[] rel_path = args[0].str_chars[:args[0].str_len];
    char[512] resolved;
    usz rlen = 0;
    if (!resolve_import_path_checked(rel_path, interp, &resolved, 512, &rlen)) {
        return raise_error(interp, "load: path too long");
    }
    char[] path = resolved[:rlen];

    if (try content = io::file::load_temp((String)path)) {
        char[] source = content;

        if (!push_source_dir(path, interp)) {
            char[512] ebuf;
            if (try char[] msg = io::bprintf(&ebuf, "load: failed to allocate source directory context for '%s'", (ZString)path.ptr)) {
                return raise_error(interp, msg);
            }
            return raise_error(interp, "load: failed to allocate source directory context");
        }

        EvalResult r = run_program(source, interp);

        pop_source_dir(interp);

        if (r.error.has_error) {
            char[512] ebuf;
            if (try char[] msg = io::bprintf(&ebuf, "load: error in '%s': %s", (ZString)path.ptr, (ZString)&r.error.message)) {
                return raise_error(interp, msg);
            }
            return raise_error(interp, "load: error while evaluating file");
        }
        return r.value;
    }
    main::EntryFileReadKind read_kind;
    (void)main::classify_file_read_path(path, &read_kind);
    char[384] ebuf;
    if (try char[] msg = io::bprintf(
        &ebuf,
        "load: cannot read '%s': %s",
        (ZString)path.ptr,
        (ZString)main::file_read_kind_reason(read_kind)
    )) {
        return raise_error(interp, msg);
    }
    return raise_error(interp, "load: cannot read file");
}
