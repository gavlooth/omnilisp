module pika;

import std::core::mem;
import std::thread;

const int REGEX_CACHE_INIT_CAP = 32;
const int REGEX_CACHE_MAX_ENTRIES = 128;

struct RegexCacheEntry {
    char[] pattern;
    CompiledRegex* rx;
    usz last_used;
}

RegexCacheEntry[] g_regex_cache;
int g_regex_cache_len = 0;
int g_regex_cache_cap = 0;
usz g_regex_cache_tick = 0;
usz g_regex_cache_hits = 0;
usz g_regex_cache_misses = 0;
usz g_regex_cache_evictions = 0;
usz g_regex_fallback_uses = 0;
usz g_regex_compiled_unusable_compile = 0;
usz g_regex_compiled_unusable_cache = 0;
tlocal char[256] g_regex_last_compile_error;
tlocal bool g_regex_last_compile_error_set = false;
thread::Mutex g_regex_cache_mu;
thread::OnceFlag g_regex_cache_mu_once;

fn void regex_cache_mu_init_once() {
    if (catch err = g_regex_cache_mu.init()) {
        unreachable("regex cache mutex init failed");
    }
}

fn void regex_cache_lock() @inline {
    g_regex_cache_mu_once.call(&regex_cache_mu_init_once);
    g_regex_cache_mu.lock();
}

fn void regex_cache_unlock() @inline {
    g_regex_cache_mu.unlock();
}

fn void regex_clear_last_compile_error() @inline {
    g_regex_last_compile_error_set = false;
    g_regex_last_compile_error[0] = 0;
}

fn void regex_record_compile_error(char[] msg) {
    usz len = msg.len;
    if (len > 255) len = 255;
    for (usz i = 0; i < len; i++) {
        g_regex_last_compile_error[i] = msg[i];
    }
    g_regex_last_compile_error[len] = 0;
    g_regex_last_compile_error_set = true;
}

fn bool regex_take_last_compile_error(char[]* out) {
    if (!g_regex_last_compile_error_set) return false;
    usz len = 0;
    while (len < 255 && g_regex_last_compile_error[len] != 0) len++;
    *out = g_regex_last_compile_error[:len];
    g_regex_last_compile_error_set = false;
    return true;
}

fn void regex_cache_ensure_init() {
    if (g_regex_cache_cap > 0) return;
    g_regex_cache_cap = REGEX_CACHE_INIT_CAP;
    g_regex_cache = mem::new_array(RegexCacheEntry, (usz)g_regex_cache_cap);
}

fn void regex_cache_grow() {
    if (g_regex_cache_cap >= REGEX_CACHE_MAX_ENTRIES) return;
    int new_cap = g_regex_cache_cap * 2;
    if (new_cap > REGEX_CACHE_MAX_ENTRIES) new_cap = REGEX_CACHE_MAX_ENTRIES;
    if (new_cap <= g_regex_cache_cap) return;
    RegexCacheEntry[] next = mem::new_array(RegexCacheEntry, (usz)new_cap);
    for (int i = 0; i < g_regex_cache_len; i++) {
        next[i] = g_regex_cache[i];
    }
    if (g_regex_cache_cap > 0) mem::free(g_regex_cache.ptr);
    g_regex_cache = next;
    g_regex_cache_cap = new_cap;
}

fn void regex_cache_reset() {
    regex_cache_lock();
    defer regex_cache_unlock();

    for (int i = 0; i < g_regex_cache_len; i++) {
        if (g_regex_cache[i].pattern.len > 0) mem::free(g_regex_cache[i].pattern.ptr);
        regex_free(g_regex_cache[i].rx);
    }
    if (g_regex_cache_cap > 0) mem::free(g_regex_cache.ptr);

    RegexCacheEntry[] empty;
    g_regex_cache = empty;
    g_regex_cache_len = 0;
    g_regex_cache_cap = 0;
    g_regex_cache_tick = 0;
    g_regex_cache_hits = 0;
    g_regex_cache_misses = 0;
    g_regex_cache_evictions = 0;
    g_regex_fallback_uses = 0;
    g_regex_compiled_unusable_compile = 0;
    g_regex_compiled_unusable_cache = 0;
    regex_clear_last_compile_error();
}

fn bool regex_cache_pattern_eq(char[] a, char[] b) {
    if (a.len != b.len) return false;
    for (usz i = 0; i < a.len; i++) {
        if (a[i] != b[i]) return false;
    }
    return true;
}

fn void regex_cache_touch(int idx) {
    g_regex_cache_tick++;
    g_regex_cache[idx].last_used = g_regex_cache_tick;
}

fn int regex_cache_find(char[] pattern) {
    for (int i = 0; i < g_regex_cache_len; i++) {
        if (regex_cache_pattern_eq(g_regex_cache[i].pattern, pattern)) {
            g_regex_cache_hits++;
            regex_cache_touch(i);
            return i;
        }
    }
    g_regex_cache_misses++;
    return -1;
}

fn int regex_cache_find_lru_index() {
    if (g_regex_cache_len <= 0) return -1;
    int lru_idx = 0;
    usz oldest = g_regex_cache[0].last_used;
    for (int i = 1; i < g_regex_cache_len; i++) {
        if (g_regex_cache[i].last_used < oldest) {
            oldest = g_regex_cache[i].last_used;
            lru_idx = i;
        }
    }
    return lru_idx;
}

fn char[] regex_cache_copy_pattern(char[] pattern) {
    if (pattern.len == 0) {
        char[] empty;
        return empty;
    }
    char[] stored = mem::new_array(char, pattern.len);
    for (usz i = 0; i < pattern.len; i++) {
        stored[i] = pattern[i];
    }
    return stored;
}

fn void regex_set_compiled_error(CompiledRegex* rx, char[] msg) {
    rx.valid = false;
    usz len = msg.len;
    if (len > 255) len = 255;
    for (usz i = 0; i < len; i++) {
        rx.error[i] = msg[i];
    }
    rx.error[len] = 0;
}
