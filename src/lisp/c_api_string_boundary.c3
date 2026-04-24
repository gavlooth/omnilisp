module lisp;

import std::core::mem;

struct CApiStringResult {
    char* ptr;
    bool ok;
    bool contains_nul;
    bool too_long;
    bool out_of_memory;
}

fn bool c_api_string_contains_nul(char[] src) {
    for (usz i = 0; i < src.len; i++) {
        if (src[i] == (char)0) return true;
    }
    return false;
}

fn CApiStringResult c_api_string_dup_no_nul(char[] src, usz max_len = usz.max) {
    CApiStringResult result = {};
    if (src.len > max_len) {
        result.too_long = true;
        return result;
    }
    if (c_api_string_contains_nul(src)) {
        result.contains_nul = true;
        return result;
    }
    if (src.len == usz.max) {
        result.out_of_memory = true;
        return result;
    }
    char* out = (char*)mem::malloc(src.len + 1);
    if (out == null) {
        result.out_of_memory = true;
        return result;
    }
    for (usz i = 0; i < src.len; i++) out[i] = src[i];
    out[src.len] = 0;
    result.ptr = out;
    result.ok = true;
    return result;
}

fn void c_api_string_free(CApiStringResult* result) {
    if (result == null || result.ptr == null) return;
    mem::free(result.ptr);
    result.ptr = null;
    result.ok = false;
}
