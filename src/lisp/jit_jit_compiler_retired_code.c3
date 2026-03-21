module lisp;

import std::io;

struct JitRetiredCode {
    void* code;
    ulong owner_attach_serial;
}

const usz JIT_RETIRED_CODE_MAX = 262144;
JitRetiredCode[JIT_RETIRED_CODE_MAX] g_jit_retired_codes;
usz g_jit_retired_code_count = 0;
bool g_jit_retired_code_saturated = false;

fn bool jit_retired_code_contains(void* code, ulong owner_attach_serial) {
    if (code == null || owner_attach_serial == 0) return false;
    for (usz i = 0; i < g_jit_retired_code_count; i++) {
        if (g_jit_retired_codes[i].code == code &&
            g_jit_retired_codes[i].owner_attach_serial == owner_attach_serial) {
            return true;
        }
    }
    return false;
}

fn bool jit_retired_code_insert(void* code, ulong owner_attach_serial) {
    if (code == null || owner_attach_serial == 0) return true;
    for (usz i = 0; i < g_jit_retired_code_count; i++) {
        if (g_jit_retired_codes[i].code == code &&
            g_jit_retired_codes[i].owner_attach_serial == owner_attach_serial) {
            return true;
        }
    }
    if (g_jit_retired_code_count < JIT_RETIRED_CODE_MAX) {
        g_jit_retired_codes[g_jit_retired_code_count].code = code;
        g_jit_retired_codes[g_jit_retired_code_count].owner_attach_serial = owner_attach_serial;
        g_jit_retired_code_count++;
        return true;
    }
    if (!g_jit_retired_code_saturated) {
        io::eprintfn("[warn] jit retired-code tombstone table saturated; pointer reuse guard degraded");
        g_jit_retired_code_saturated = true;
    }
    return false;
}

fn void jit_retired_code_forget_serial(ulong owner_attach_serial) {
    if (owner_attach_serial == 0) return;
    usz write = 0;
    for (usz read = 0; read < g_jit_retired_code_count; read++) {
        if (g_jit_retired_codes[read].owner_attach_serial == owner_attach_serial) continue;
        if (write != read) {
            g_jit_retired_codes[write] = g_jit_retired_codes[read];
        }
        write++;
    }
    for (usz i = write; i < g_jit_retired_code_count; i++) {
        g_jit_retired_codes[i].code = null;
        g_jit_retired_codes[i].owner_attach_serial = 0;
    }
    g_jit_retired_code_count = write;
}

fn void jit_retired_code_reset() {
    for (usz i = 0; i < g_jit_retired_code_count; i++) {
        g_jit_retired_codes[i].code = null;
        g_jit_retired_codes[i].owner_attach_serial = 0;
    }
    g_jit_retired_code_count = 0;
    g_jit_retired_code_saturated = false;
}

fn void jit_retired_code_prune_detached_serials() {
    usz write = 0;
    for (usz read = 0; read < g_jit_retired_code_count; read++) {
        ulong serial = g_jit_retired_codes[read].owner_attach_serial;
        if (!jit_is_attached_serial(serial)) continue;
        if (write != read) {
            g_jit_retired_codes[write] = g_jit_retired_codes[read];
        }
        write++;
    }
    for (usz i = write; i < g_jit_retired_code_count; i++) {
        g_jit_retired_codes[i].code = null;
        g_jit_retired_codes[i].owner_attach_serial = 0;
    }
    g_jit_retired_code_count = write;
}
