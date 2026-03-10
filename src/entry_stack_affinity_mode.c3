module main;

import std::io;

fn bool run_stack_affinity_harness(char* self_exe) {
    if (self_exe == null || self_exe[0] == 0) return false;

    char[] log_path = "/tmp/omni_stack_affinity_probe.log";
    char[2048] cmd;
    usz n = 0;
    char[] prefix = "LD_LIBRARY_PATH=/usr/local/lib ";
    for (usz i = 0; i < prefix.len && n < cmd.len - 1; i++) cmd[n++] = prefix[i];
    usz self_len = cstr_len(self_exe);
    for (usz i = 0; i < self_len && n < cmd.len - 1; i++) cmd[n++] = self_exe[i];
    char[] suffix = " --stack-affinity-probe > /tmp/omni_stack_affinity_probe.log 2>&1";
    for (usz i = 0; i < suffix.len && n < cmd.len - 1; i++) cmd[n++] = suffix[i];
    cmd[n] = 0;

    int code = system(&cmd[0]);
    bool nonzero = code != 0;
    bool saw_marker = false;

    if (try probe_out = io::file::load_temp((String)log_path)) {
        saw_marker = cstr_contains(probe_out, "stack-engine thread-affinity violation");
    }
    return nonzero && saw_marker;
}

fn int run_stack_affinity_probe_mode() {
    io::printn("Running stack affinity violation probe...");
    return run_stack_engine_affinity_violation_probe();
}
