module pika;

import std::core::mem;

fn CompiledRegex* regex_compile(char[] pattern) {
    CompiledRegex* rx = mem::new(CompiledRegex);
    rx.valid = true;
    rx.start_clause = -1;
    rx.active_refs = 0;
    rx.anchored_start = false;
    rx.anchored_end = false;
    rx.cached = false;

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
    if (existing >= 0) return regex_cache_retain_locked(g_regex_cache[existing].rx);

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
            rx.cached = true;
            regex_cache_touch(g_regex_cache_len);
            g_regex_cache_len++;
            return regex_cache_retain_locked(rx);
        }
    }

    int evict_idx = regex_cache_find_lru_index();
    if (evict_idx >= 0) {
        if (g_regex_cache[evict_idx].pattern.len > 0) mem::free(g_regex_cache[evict_idx].pattern.ptr);
        regex_cache_detach_locked(g_regex_cache[evict_idx].rx);
        g_regex_cache[evict_idx].pattern = regex_cache_copy_pattern(pattern);
        g_regex_cache[evict_idx].rx = rx;
        rx.cached = true;
        regex_cache_touch(evict_idx);
        g_regex_cache_evictions++;
        return regex_cache_retain_locked(rx);
    }

    // Conservative fallback: if cache bookkeeping is inconsistent, do not cache this entry.
    g_regex_compiled_unusable_cache++;
    regex_free(rx);
    return null;
}

fn void pika_release_parser_state(ParserState* st) {
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
    *usable = false;
    CompiledRegex* rx = regex_get_compiled_cached(pattern);
    regex_cache_unlock();
    if (rx == null) return no_match();
    defer regex_cache_release(rx);

    *usable = true;

    ParserState st = parse_simple(&rx.grammar, input);
    defer pika_release_parser_state(&st);

    int start = rx.anchored_start ? 0 : 0;
    int end = rx.anchored_start ? 0 : (int)input.len;
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
    *usable = false;
    CompiledRegex* rx = regex_get_compiled_cached(pattern);
    regex_cache_unlock();
    if (rx == null) return no_match();
    defer regex_cache_release(rx);

    *usable = true;
    ParserState st = parse_simple(&rx.grammar, input);
    defer pika_release_parser_state(&st);

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

    RegexMatch[] matches = {};
    int cap = 0;
    int match_count = 0;
    *count = 0;
    *usable = false;

    CompiledRegex* rx = regex_get_compiled_cached(pattern);
    regex_cache_unlock();
    if (rx == null) return matches;
    defer regex_cache_release(rx);

    *usable = true;

    ParserState st = parse_simple(&rx.grammar, input);
    defer pika_release_parser_state(&st);

    int pos = 0;
    while (pos <= (int)input.len) {
        bool found = false;
        int start = rx.anchored_start ? 0 : pos;
        int end = rx.anchored_start ? 0 : (int)input.len;

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
            if (m.end < m.start) {
                pos = m.start + 1;
            } else {
                pos = m.end + 1;
            }
            found = true;
            break;
        }

        if (!found) break;
        if (rx.anchored_start && pos > 0) break;
    }
    *count = match_count;
    return match_count > 0 ? matches[:(usz)match_count] : matches;
}
