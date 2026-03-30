module main;

import std::io;
import lisp;

extern fn void* c_popen(char* command, char* mode) @extern("popen");
extern fn int c_pclose(void* stream) @extern("pclose");
extern fn usz c_fread(void* ptr, usz size, usz count, void* stream) @extern("fread");
extern fn int c_feof(void* stream) @extern("feof");

fn bool stack_affinity_append_char(char[] buf, usz* n, char c) {
    if (*n >= buf.len - 1) return false;
    buf[*n] = c;
    *n += 1;
    return true;
}

fn bool stack_affinity_append_str(char[] buf, usz* n, char[] src) {
    for (usz i = 0; i < src.len; i++) {
        if (src[i] == 0) break;
        if (!stack_affinity_append_char(buf, n, src[i])) return false;
    }
    return true;
}

fn bool stack_affinity_append_shell_single_quote(char[] buf, usz* n, char* raw) {
    if (!stack_affinity_append_char(buf, n, '\'')) return false;
    usz len = cstr_len(raw);
    for (usz i = 0; i < len; i++) {
        char c = raw[i];
        if (c == '\'') {
            if (!stack_affinity_append_str(buf, n, "'\\''")) return false;
            continue;
        }
        if (!stack_affinity_append_char(buf, n, c)) return false;
    }
    return stack_affinity_append_char(buf, n, '\'');
}

fn bool run_stack_affinity_harness(char* self_exe) {
    if (self_exe == null || self_exe[0] == 0) return false;

    char[1024] cmd;
    usz n = 0;
    char[] prefix = "LD_LIBRARY_PATH=/usr/local/lib ";
    char[] suffix = " --stack-affinity-probe 2>&1";
    if (!stack_affinity_append_str(cmd[..], &n, prefix)) return false;
    if (!stack_affinity_append_shell_single_quote(cmd[..], &n, self_exe)) return false;
    if (!stack_affinity_append_str(cmd[..], &n, suffix)) return false;
    cmd[n] = 0;

    void* fp = c_popen(&cmd[0], "r");
    if (fp == null) return false;

    char[8192] out_buf;
    usz total = 0;
    while (c_feof(fp) == 0 && total < out_buf.len) {
        usz nread = c_fread(&out_buf[total], 1, out_buf.len - total, fp);
        if (nread == 0) break;
        total += nread;
    }
    int status = c_pclose(fp);
    if (status < 0) return false;
    // Treat any non-zero wait status as probe failure (exit code or signal).
    bool nonzero = status != 0;
    bool saw_marker = cstr_contains(out_buf[:total], "stack-engine thread-affinity violation");
    return nonzero && saw_marker;
}

fn int run_stack_affinity_probe_mode(int argc, char** argv, int probe_idx) {
    char* unexpected = find_unexpected_mode_arg(argc, argv, probe_idx);
    if (unexpected != null) {
        return print_unexpected_mode_arg("--stack-affinity-probe", unexpected);
    }

    if (!lisp::test_quiet_requested()) {
        io::printn("Running stack affinity violation probe...");
    }
    return run_stack_engine_affinity_violation_probe();
}
