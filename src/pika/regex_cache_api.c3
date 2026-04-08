module pika;

import std::core::mem;
import std::io;
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
    g_regex_cache_mu.init()!!;
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

fn bool regex_generated_name_is_heap(String name) {
    if (name.len < 3) return false;
    if (name[0] == '_') return false;

    isz underscore = -1;
    for (usz i = 0; i < name.len; i++) {
        if (name[i] == '_') {
            underscore = (isz)i;
            break;
        }
    }
    if (underscore <= 0 || (usz)(underscore + 1) >= name.len) return false;

    for (usz i = (usz)(underscore + 1); i < name.len; i++) {
        char ch = name[i];
        if (ch < '0' || ch > '9') return false;
    }
    return true;
}

fn void regex_free_char_class_ctx(void* raw_ctx) {
    CharClassData* cc = (CharClassData*)raw_ctx;
    if (cc == null) return;
    if (cc.ranges_cap > 0) mem::free(cc.ranges.ptr);
    if (cc.chars_cap > 0) mem::free(cc.chars.ptr);
    mem::free(cc);
}

fn void regex_release_rule_builder(RuleBuilder* rb, bool transferred_to_grammar) {
    for (int i = 0; i < rb.len; i++) {
        Clause* clause = &rb.rules[i].clause;

        switch (clause.tag) {
            case SEQ:
            case FIRST:
                if (clause.children.len > 0) mem::free(clause.children.ptr);
            default:
                ;
        }

        if (transferred_to_grammar) continue;

        if (regex_generated_name_is_heap(rb.rules[i].name)) {
            mem::free(rb.rules[i].name.ptr);
        }

        if (clause.tag == SCAN_CTX && clause.scan_ctx != null) {
            regex_free_char_class_ctx(clause.scan_ctx);
            clause.scan_ctx = null;
        }
    }

    if (rb.cap > 0) mem::free(rb.rules.ptr);

    RuleEntry[] empty_rules;
    rb.rules = empty_rules;
    rb.len = 0;
    rb.cap = 0;
}

fn void regex_release_grammar_allocations(Grammar* grammar) {
    if (grammar.clauses.len > 0) {
        for (usz i = 0; i < grammar.clauses.len; i++) {
            Clause* clause = &grammar.clauses[i];
            switch (clause.tag) {
                case SEQ:
                case FIRST:
                    if (clause.children.len > 0) mem::free(clause.children.ptr);
                case SCAN_CTX:
                    if (clause.scan_ctx != null) {
                        regex_free_char_class_ctx(clause.scan_ctx);
                        clause.scan_ctx = null;
                    }
                default:
                    ;
            }
        }
    }

    if (grammar.seed_clauses.len > 0) {
        for (usz i = 0; i < grammar.seed_clauses.len; i++) {
            if (grammar.seed_clauses[i].len > 0) mem::free(grammar.seed_clauses[i].ptr);
        }
        mem::free(grammar.seed_clauses.ptr);
    }

    if (grammar.terminals.len > 0) mem::free(grammar.terminals.ptr);
    if (grammar.can_match_epsilon.len > 0) mem::free(grammar.can_match_epsilon.ptr);
    if (grammar.name_to_idx.len > 0) mem::free(grammar.name_to_idx.ptr);

    if (grammar.names.len > 0) {
        for (usz i = 0; i < grammar.names.len; i++) {
            if (regex_generated_name_is_heap(grammar.names[i])) {
                mem::free(grammar.names[i].ptr);
            }
        }
        mem::free(grammar.names.ptr);
    }

    if (grammar.clauses.len > 0) mem::free(grammar.clauses.ptr);

    String[] empty_names;
    int[] empty_name_to_idx;
    Clause[] empty_clauses;
    bool[] empty_eps;
    int[][] empty_seed;
    int[] empty_terminals;
    grammar.names = empty_names;
    grammar.name_to_idx = empty_name_to_idx;
    grammar.clauses = empty_clauses;
    grammar.can_match_epsilon = empty_eps;
    grammar.seed_clauses = empty_seed;
    grammar.terminals = empty_terminals;
    grammar.num_clauses = 0;
}

// Compile a regex pattern to PikaParser grammar
fn CompiledRegex* regex_compile(char[] pattern) {
    CompiledRegex* rx = mem::new(CompiledRegex);
    rx.valid = true;
    rx.start_clause = -1;
    rx.anchored_start = false;
    rx.anchored_end = false;

    RegexTokenizer tok = regex_tokenizer_new(pattern);

    // Avoid tokenizer side-allocation for leading-anchor probe.
    if (tok.peek() == '^') {
        rx.anchored_start = true;
        tok.advance();
    }

    if (tok.has_error) {
        regex_set_compiled_error(rx, regex_tokenizer_error_view(&tok));
        return rx;
    }

    // Compile the regex to grammar rules
    RegexCompiler rc = regex_compiler_new(&tok);
    int top_rule = compile_regex_alternation(&rc);
    if (tok.has_error) {
        regex_set_compiled_error(rx, regex_tokenizer_error_view(&tok));
        regex_release_rule_builder(&rc.rb, false);
        return rx;
    }

    // Add end-of-input for a trailing unescaped '$' anchor.
    if (tok.pos + 1 == (int)tok.pattern.len && tok.peek() == '$') {
        rx.anchored_end = true;
        tok.advance();
        int end_rule = rc.rb.add("_end", mk_end_of_input());
        int[] seq = make_children_2(top_rule, end_rule);
        top_rule = rc.rb.add("_top", mk_seq(seq));
    }

    RegexToken end_tok = tok.next_token();
    if (tok.has_error) {
        regex_set_compiled_error(rx, regex_tokenizer_error_view(&tok));
        regex_release_rule_builder(&rc.rb, false);
        return rx;
    }
    if (end_tok.type != RE_END) {
        regex_set_compiled_error(rx, regex_trailing_token_message(end_tok.type));
        regex_release_rule_builder(&rc.rb, false);
        return rx;
    }

    // Use a stable exported top rule name for traversal/lookups.
    top_rule = rc.rb.add("_re_top", mk_tie(top_rule));

    // Build the grammar
    if (rc.rb.len > 0) {
        String[] starts = mem::new_array(String, 1);
        starts[0] = "_re_top";
        rx.grammar = make_grammar(starts, rc.rb.to_array());
        mem::free(starts.ptr);
        rx.start_clause = find_name_index(&rx.grammar, "_re_top");
        if (rx.start_clause < 0) {
            rx.valid = false;
        }
    }
    regex_release_rule_builder(&rc.rb, true);

    return rx;
}

// Free compiled regex
fn void regex_free(CompiledRegex* rx) {
    if (rx == null) return;
    regex_release_grammar_allocations(&rx.grammar);
    mem::free(rx);
}

fn CompiledRegex* regex_get_compiled_cached(char[] pattern) {
    regex_cache_ensure_init();

    int existing = regex_cache_find(pattern);
    if (existing >= 0) return g_regex_cache[existing].rx;

    CompiledRegex* rx = regex_compile(pattern);
    if (rx == null) {
        g_regex_compiled_unusable_compile++;
        regex_record_compile_error("regex compile failed: out of memory");
        regex_free(rx);
        return null;
    }

    if (!rx.valid || rx.start_clause < 0 || rx.grammar.num_clauses <= 0 || rx.grammar.names.len == 0) {
        g_regex_compiled_unusable_compile++;
        usz err_len = 0;
        while (err_len < 255 && rx.error[err_len] != 0) err_len++;
        if (err_len > 0) {
            regex_record_compile_error(rx.error[:err_len]);
        } else {
            regex_record_compile_error("invalid regex pattern");
        }
        regex_free(rx);
        return null;
    }

    if (g_regex_cache_len < REGEX_CACHE_MAX_ENTRIES) {
        if (g_regex_cache_len >= g_regex_cache_cap) regex_cache_grow();
        if (g_regex_cache_len < g_regex_cache_cap) {
            g_regex_cache[g_regex_cache_len].pattern = regex_cache_copy_pattern(pattern);
            g_regex_cache[g_regex_cache_len].rx = rx;
            regex_cache_touch(g_regex_cache_len);
            g_regex_cache_len++;
            return rx;
        }
    }

    int evict_idx = regex_cache_find_lru_index();
    if (evict_idx >= 0) {
        if (g_regex_cache[evict_idx].pattern.len > 0) mem::free(g_regex_cache[evict_idx].pattern.ptr);
        regex_free(g_regex_cache[evict_idx].rx);
        g_regex_cache[evict_idx].pattern = regex_cache_copy_pattern(pattern);
        g_regex_cache[evict_idx].rx = rx;
        regex_cache_touch(evict_idx);
        g_regex_cache_evictions++;
        return rx;
    }

    // Conservative fallback: if cache bookkeeping is inconsistent, do not cache this entry.
    g_regex_compiled_unusable_cache++;
    regex_free(rx);
    return null;
}

fn void regex_release_parser_state(ParserState* st) {
    st.q.free();
    if (st.matches_cap > 0) mem::free(st.matches.ptr);
    if (st.submatches_cap > 0) mem::free(st.submatches.ptr);
    st.matches_cap = 0;
    st.submatches_cap = 0;
}

fn void regex_matches_push(RegexMatch[]* matches, int* count, int* cap, RegexMatch match) {
    if (*count >= *cap) {
        int new_cap = *cap < 8 ? 8 : *cap * 2;
        RegexMatch[] next = mem::new_array(RegexMatch, (usz)new_cap);
        for (int i = 0; i < *count; i++) {
            next[i] = (*matches)[i];
        }
        if (*cap > 0) mem::free((*matches).ptr);
        *matches = next;
        *cap = new_cap;
    }
    (*matches)[*count] = match;
    (*count)++;
}

fn RegexMatch regex_search_compiled(char[] pattern, char[] input, bool* usable) {
    regex_cache_lock();
    defer regex_cache_unlock();

    *usable = false;
    CompiledRegex* rx = regex_get_compiled_cached(pattern);
    if (rx == null) return no_match();

    *usable = true;
    if (input.len == 0) return no_match();

    ParserState st = parse_simple(&rx.grammar, input);
    defer regex_release_parser_state(&st);

    int start = rx.anchored_start ? 0 : 0;
    int end = rx.anchored_start ? 0 : (int)input.len - 1;
    for (int i = start; i <= end; i++) {
        int mid = lookup_best_match_id(i, rx.start_clause, &st);
        if (mid >= 0) {
            Match* m = &st.matches[mid];
            return regex_mk_match(m.first, m.last, input);
        }
        if (rx.anchored_start) break;
    }
    return no_match();
}

fn RegexMatch regex_fullmatch_compiled(char[] pattern, char[] input, bool* usable) {
    regex_cache_lock();
    defer regex_cache_unlock();

    *usable = false;
    CompiledRegex* rx = regex_get_compiled_cached(pattern);
    if (rx == null) return no_match();

    *usable = true;
    ParserState st = parse_simple(&rx.grammar, input);
    defer regex_release_parser_state(&st);

    int mid = lookup_best_match_id(0, rx.start_clause, &st);
    if (mid < 0) return no_match();

    Match* m = &st.matches[mid];
    if (m.first == 0 && m.last == (int)input.len - 1) {
        return regex_mk_match(m.first, m.last, input);
    }
    return no_match();
}

fn RegexMatch[] regex_find_all_compiled(char[] pattern, char[] input, int* count, bool* usable) {
    regex_cache_lock();
    defer regex_cache_unlock();

    RegexMatch[] matches;
    int cap = 0;
    int match_count = 0;
    *count = 0;
    *usable = false;

    CompiledRegex* rx = regex_get_compiled_cached(pattern);
    if (rx == null) return matches;

    *usable = true;
    if (input.len == 0) return matches;

    ParserState st = parse_simple(&rx.grammar, input);
    defer regex_release_parser_state(&st);

    int pos = 0;
    while (pos < (int)input.len) {
        bool found = false;
        int start = rx.anchored_start ? 0 : pos;
        int end = rx.anchored_start ? 0 : (int)input.len - 1;

        for (int i = start; i <= end; i++) {
            if (i < pos) continue;
            int mid = lookup_best_match_id(i, rx.start_clause, &st);
            if (mid < 0) {
                if (rx.anchored_start) break;
                continue;
            }

            Match* mm = &st.matches[mid];
            RegexMatch m = regex_mk_match(mm.first, mm.last, input);
            regex_matches_push(&matches, &match_count, &cap, m);
            pos = m.end + 1;
            if (pos <= m.start) pos = m.start + 1; // Avoid infinite loop on empty match
            found = true;
            break;
        }

        if (!found) break;
        if (rx.anchored_start && pos > 0) break;
    }
    *count = match_count;
    return match_count > 0 ? matches[:(usz)match_count] : matches;
}

// Search for pattern anywhere in input
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
