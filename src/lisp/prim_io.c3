module lisp;

import std::io;
import std::core::mem;
import main;

// =============================================================================
// I/O PRIMITIVES
// =============================================================================

fn Value* prim_print(Value*[] args, Env* env, Interp* interp) {
    if (args.len >= 1) {
        print_value(args[0], &interp.symbols);
    }
    return make_nil(interp);
}

fn Value* prim_println(Value*[] args, Env* env, Interp* interp) {
    if (args.len >= 1) {
        println_value(args[0], &interp.symbols);
    }
    return make_nil(interp);
}

fn Value* prim_newline(Value*[] args, Env* env, Interp* interp) {
    io::printn("");
    return make_nil(interp);
}

// =============================================================================
// SECTION 7.6: FILE I/O PRIMITIVES
// =============================================================================

/**
 * (read-file path) -> string contents or nil on error
 */
fn Value* prim_read_file(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return make_nil(interp);
    }

    // Get path as slice (using :len for exclusive end)
    char[] path = args[0].str_chars[:args[0].str_len];

    // Try to load file using C3's file::load_temp
    if (try content = io::file::load_temp((String)path)) {
        return make_string(interp, content);
    }
    return make_nil(interp);
}

/**
 * (write-file path content) -> true on success, nil on error
 */
fn Value* prim_write_file(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_string(args[0]) || !is_string(args[1])) {
        return make_nil(interp);
    }

    // Get path and content as slices (using :len for exclusive end)
    char[] path = args[0].str_chars[:args[0].str_len];
    char[] content = args[1].str_chars[:args[1].str_len];

    // Open file for writing
    if (try file = io::file::open((String)path, "w")) {
        defer (void)file.close();
        if (try bytes_written = file.write(content)) {
            return make_symbol(interp, interp.sym_true);
        }
    }
    return make_nil(interp);
}

/**
 * (file-exists? path) -> true if file exists, nil otherwise
 */
fn Value* prim_file_exists(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return make_nil(interp);
    }

    // Get path as slice (using :len for exclusive end)
    char[] path = args[0].str_chars[:args[0].str_len];

    // Use file::is_file to check existence
    if (io::file::is_file((String)path)) {
        return make_symbol(interp, interp.sym_true);
    }
    return make_nil(interp);
}

/**
 * (read-lines path) -> list of strings (one per line)
 */
fn Value* prim_read_lines(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return make_nil(interp);
    }

    // Get path as slice (using :len for exclusive end)
    char[] path = args[0].str_chars[:args[0].str_len];

    // Try to load file using C3's file::load_temp
    if (try content = io::file::load_temp((String)path)) {
        // Split content into lines
        Value* lines = make_nil(interp);
        usz start = 0;
        usz content_len = content.len;

        for (usz i = 0; i <= content_len; i++) {
            if (i == content_len || content[i] == '\n') {
                // Extract line [start..i)
                usz line_len = i - start;
                // Skip trailing \r if present (Windows line endings)
                if (line_len > 0 && content[start + line_len - 1] == '\r') {
                    line_len--;
                }
                Value* line = make_string(interp, content[start:line_len]);
                lines = make_cons(interp, line, lines);
                start = i + 1;
            }
        }

        // Reverse the list to get correct order
        Value* result = make_nil(interp);
        while (is_cons(lines)) {
            result = make_cons(interp, lines.cons_val.car, result);
            lines = lines.cons_val.cdr;
        }
        return result;
    }
    return make_nil(interp);
}

/**
 * (load path) -> evaluates all expressions in the file, returns last result
 * Reads a script file and evaluates it in the current environment.
 */
fn Value* prim_load(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return raise_error(interp, "load: expected a string path");
    }

    // Resolve path relative to current source directory
    char[] rel_path = args[0].str_chars[:args[0].str_len];
    char[512] resolved;
    usz rlen = resolve_import_path(rel_path, interp, &resolved, 512);
    char[] path = resolved[:rlen];

    // Try to load file
    if (try content = io::file::load_temp((String)path)) {
        char[] source = content;

        // Push source directory for nested loads/imports
        if (!push_source_dir(path, interp)) {
            return raise_error(interp, "load: failed to allocate source directory context");
        }

        // Run all expressions in the file
        EvalResult r = run_program(source, interp);

        pop_source_dir(interp);

        if (r.error.has_error) {
            char[512] ebuf;
            char[] msg = io::bprintf(&ebuf, "load: error in '%s': %s",
                (ZString)path.ptr, (ZString)&r.error.message)!!;
            return raise_error(interp, msg);
        }
        return r.value;
    }
    char[256] ebuf;
    char[] msg = io::bprintf(&ebuf, "load: file not found '%s'", (ZString)path.ptr)!!;
    return raise_error(interp, msg);
}

// === System primitives (libc-backed) ===

extern fn void* c_popen(char* command, char* mode) @extern("popen");
extern fn int c_pclose(void* stream) @extern("pclose");
extern fn usz c_fread(void* ptr, usz size, usz count, void* stream) @extern("fread");
extern fn int c_feof(void* stream) @extern("feof");
extern fn char* c_getenv(char* name) @extern("getenv");
extern fn long c_time(long* t) @extern("time");
extern fn int c_usleep(uint usec) @extern("usleep");
extern fn void c_exit(int status) @extern("_exit");
/**
 * (shell cmd) -> string (stdout of command)
 * (shell cmd true) -> list (stdout-string exit-code)
 * Runs cmd via popen, captures stdout, returns output as string.
 */
fn Value* prim_shell(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return raise_error(interp, "shell: expected string command");
    }

    void* fp = c_popen((ZString)args[0].str_chars, "r");
    if (fp == null) {
        return raise_error(interp, "shell: popen failed");
    }

    // Read output into buffer
    char[8192] buf;
    usz total = 0;
    while (c_feof(fp) == 0) {
        usz n = c_fread(&buf[total], 1, 8192 - total, fp);
        if (n == 0) break;
        total += n;
        if (total >= 8192) break;
    }
    int status = c_pclose(fp);
    int exit_code = status >> 8;  // WEXITSTATUS

    // Strip trailing newline
    if (total > 0 && buf[total - 1] == '\n') total--;

    Value* output = make_string(interp, buf[:total]);

    // If second arg is truthy, return (output exit-code)
    if (args.len >= 2 && !is_falsy(args[1], interp)) {
        Value* code = make_int(interp, exit_code);
        return make_cons(interp, output, make_cons(interp, code, make_nil(interp)));
    }
    return output;
}

/**
 * (getenv name) -> string or nil
 */
fn Value* prim_getenv(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return raise_error(interp, "getenv: expected string");
    }
    char* val = c_getenv((ZString)args[0].str_chars);
    if (val == null) return make_nil(interp);
    // Measure length
    usz len = 0;
    while (val[len] != 0) len++;
    return make_string(interp, val[:len]);
}

/**
 * (time) -> integer epoch seconds
 * (time-ms) -> integer epoch milliseconds (via clock_gettime)
 */
fn Value* prim_time(Value*[] args, Env* env, Interp* interp) {
    long t = c_time(null);
    return make_int(interp, t);
}

// clock_gettime for millisecond precision
extern fn int c_clock_gettime(int clk_id, void* tp) @extern("clock_gettime");
const int CLOCK_REALTIME = 0;

fn Value* prim_time_ms(Value*[] args, Env* env, Interp* interp) {
    long[2] ts;  // tv_sec, tv_nsec
    c_clock_gettime(CLOCK_REALTIME, &ts);
    long ms = ts[0] * 1000 + ts[1] / 1000000;
    return make_int(interp, ms);
}

/**
 * (exit) or (exit code) -> exits process
 */
fn Value* prim_exit(Value*[] args, Env* env, Interp* interp) {
    int code = 0;
    if (args.len >= 1 && args[0].tag == INT) code = (int)args[0].int_val;
    c_exit(code);
    return make_nil(interp);  // unreachable
}

/**
 * (sleep secs) -> nil
 * secs can be int or double (fractional seconds supported)
 */
fn Value* prim_sleep(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "sleep: expected number of seconds");
    double secs;
    if (args[0].tag == INT) { secs = (double)args[0].int_val; }
    else if (args[0].tag == DOUBLE) { secs = args[0].double_val; }
    else { return raise_error(interp, "sleep: expected number"); }
    uint usec = (uint)(secs * 1000000.0);
    c_usleep(usec);
    return make_nil(interp);
}

fn Value* prim_display(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "display: expected argument");
    Value* v = args[0];
    if (v == null || is_nil(v)) {
        io::print("nil");
    } else if (v.tag == STRING) {
        io::printf("%s", (ZString)v.str_chars); // No quotes for display
    } else {
        print_value(v, &interp.symbols);
    }
    return make_nil(interp);
}
