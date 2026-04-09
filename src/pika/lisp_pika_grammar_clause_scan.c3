module pika;

import std::core::mem;

fn int compile_scan_bounded_quantifier(GrammarCompiler* gc, int atom, BoundsData bounds) {
    int min = bounds.min;
    int max = bounds.max;
    int required_parts = max < 0 ? min : max;
    if (max < 0) {
        if (min == int.max) {
            gc_set_error(gc, "parser/grammar-invalid-scan-pattern", "pika/grammar: invalid scan pattern");
            return gc.rb.add("_fail", mk_fail());
        }
        required_parts = min + 1;
    }
    if (required_parts > REGEX_QUANTIFIER_COMPILE_PARTS_MAX) {
        gc_set_error(gc, "parser/grammar-invalid-scan-pattern", "pika/grammar: invalid scan pattern");
        return gc.rb.add("_fail", mk_fail());
    }

    int[] parts;
    int part_count = 0;
    int part_cap = 0;
    for (int i = 0; i < min; i++) {
        regex_parts_push(&parts, &part_count, &part_cap, atom);
    }

    if (max < 0) {
        int rest = compile_clause_named_many(gc, atom);
        regex_parts_push(&parts, &part_count, &part_cap, rest);
    } else if (max > min) {
        int eps = gc.rb.add("_eps", mk_epsilon());
        int[] opt_alts = make_children_2(atom, eps);
        int opt = gc.rb.add("_scan_bnd", mk_first(opt_alts));
        for (int i = min; i < max; i++) {
            regex_parts_push(&parts, &part_count, &part_cap, opt);
        }
    }

    if (part_count == 0) {
        if (part_cap > 0) mem::free(parts.ptr);
        return gc.rb.add("_eps", mk_epsilon());
    }
    if (part_count == 1) {
        int one = parts[0];
        if (part_cap > 0) mem::free(parts.ptr);
        return one;
    }

    int[] children = mem::new_array(int, (usz)part_count);
    for (int i = 0; i < part_count; i++) children[i] = parts[i];
    if (part_cap > 0) mem::free(parts.ptr);
    return gc.rb.add("_scan_seq", mk_seq(children));
}

fn int make_char_class_scanner(GrammarCompiler* gc, char[] pattern) {
    RegexTokenizer tok = regex_tokenizer_new(pattern);
    RegexToken first = tok.next_token();
    if (tok.has_error || first.type != RE_CHAR_CLASS) {
        gc_set_error(gc, "parser/grammar-invalid-scan-pattern", "pika/grammar: invalid scan pattern");
        return gc.rb.add("_fail", mk_fail());
    }

    int atom = store_char_class_scanner(gc, first.cclass, "_scan_class");
    RegexToken next = tok.next_token();
    if (tok.has_error) {
        gc_set_error(gc, "parser/grammar-invalid-scan-pattern", "pika/grammar: invalid scan pattern");
        return gc.rb.add("_fail", mk_fail());
    }

    int compiled = atom;
    switch (next.type) {
        case RE_PLUS:
        case RE_PLUS_POSS:
            compiled = compile_clause_named_some(gc, atom);
        case RE_STAR:
        case RE_STAR_POSS:
            compiled = compile_clause_named_many(gc, atom);
        case RE_QUESTION:
        case RE_QUEST_POSS:
            compiled = compile_clause_named_opt(gc, atom);
        case RE_LBRACE:
            compiled = compile_scan_bounded_quantifier(gc, atom, next.bounds);
        case RE_END:
            break;
        default:
            gc_set_error(gc, "parser/grammar-invalid-scan-pattern", "pika/grammar: invalid scan pattern");
            return gc.rb.add("_fail", mk_fail());
    }

    if (next.type != RE_END) {
        RegexToken end_tok = tok.next_token();
        if (tok.has_error || end_tok.type != RE_END) {
            gc_set_error(gc, "parser/grammar-invalid-scan-pattern", "pika/grammar: invalid scan pattern");
            return gc.rb.add("_fail", mk_fail());
        }
    }

    return compiled;
}

fn int scan_char_class_ctx(void* ctx, char[] view) {
    return scan_char_in_class((CharClassData*)ctx, view);
}

fn int store_char_class_scanner(GrammarCompiler* gc, CharClassData* cc, String name) {
    return gc.rb.add(name, mk_scan_ctx(&scan_char_class_ctx, (void*)cc));
}
