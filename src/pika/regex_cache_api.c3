module pika;

// Public regex query surface; cache/compile internals are split into
// regex_cache_state.c3 and regex_cache_compile.c3.

fn RegexMatch regex_search(char[] pattern, char[] input) {
    regex_clear_last_compile_error();
    bool usable = false;
    RegexMatch m = regex_search_compiled(pattern, input, &usable);
    if (!usable) {
        regex_cache_lock();
        g_regex_fallback_uses++;
        regex_cache_unlock();
    }
    return m;
}

// Match pattern against entire input
fn RegexMatch regex_fullmatch(char[] pattern, char[] input) {
    regex_clear_last_compile_error();
    bool usable = false;
    RegexMatch m = regex_fullmatch_compiled(pattern, input, &usable);
    if (!usable) {
        regex_cache_lock();
        g_regex_fallback_uses++;
        regex_cache_unlock();
    }
    return m;
}

// Find all non-overlapping matches
fn RegexMatch[] regex_find_all(char[] pattern, char[] input, int* count) {
    regex_clear_last_compile_error();
    bool usable = false;
    RegexMatch[] matches = regex_find_all_compiled(pattern, input, count, &usable);
    if (!usable) {
        regex_cache_lock();
        g_regex_fallback_uses++;
        regex_cache_unlock();
    }
    return matches;
}
