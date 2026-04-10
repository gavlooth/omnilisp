module lisp;

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
    if (output == null || output.tag == ERROR) return output;

    // If second arg is truthy, return (output exit-code)
    if (args.len >= 2 && !is_falsy(args[1], interp)) {
        Value* code = make_int(interp, exit_code);
        return make_list2_or_error(interp, output, code, "shell: failed to construct result list");
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
    return make_void(interp);  // unreachable
}

/**
 * (sleep secs) -> Void
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
    return make_void(interp);
}
