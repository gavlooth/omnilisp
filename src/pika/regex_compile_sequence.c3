module pika;

import std::core::mem;

fn int compile_regex_sequence(RegexCompiler* rc) {
    int[] parts;
    int part_count = 0;
    int part_cap = 0;

    while (true) {
        int saved_pos = rc.tok.pos;
        int elem = compile_regex_quantified(rc);
        if (elem < 0) {
            rc.tok.pos = saved_pos;
            break;
        }
        if (part_count >= part_cap) {
            int new_cap = part_cap < 16 ? 16 : part_cap * 2;
            int[] new_parts = mem::new_array(int, (usz)new_cap);
            for (int i = 0; i < part_count; i++) {
                new_parts[i] = parts[i];
            }
            if (part_cap > 0) mem::free(parts.ptr);
            parts = new_parts;
            part_cap = new_cap;
        }
        parts[part_count++] = elem;
    }

    if (part_count == 0) {
        if (part_cap > 0) mem::free(parts.ptr);
        return rc.rb.add("_eps", mk_epsilon());
    }
    if (part_count == 1) {
        int one = parts[0];
        if (part_cap > 0) mem::free(parts.ptr);
        return one;
    }

    int[] children = mem::new_array(int, (usz)part_count);
    for (int i = 0; i < part_count; i++) {
        children[i] = parts[i];
    }
    if (part_cap > 0) mem::free(parts.ptr);
    return rc.rb.add(regex_gen_name(rc, "seq"), mk_seq(children));
}

fn int compile_regex_alternation(RegexCompiler* rc) {
    int[] alts;
    int alt_count = 0;
    int alt_cap = 0;

    int first_alt = compile_regex_sequence(rc);
    alt_cap = 8;
    alts = mem::new_array(int, (usz)alt_cap);
    alts[0] = first_alt;
    alt_count = 1;

    while (true) {
        int saved_pos = rc.tok.pos;
        RegexToken tok = rc.tok.next_token();
        if (tok.type != RE_PIPE) {
            rc.tok.pos = saved_pos;
            break;
        }
        if (alt_count >= alt_cap) {
            int new_cap = alt_cap * 2;
            int[] new_alts = mem::new_array(int, (usz)new_cap);
            for (int i = 0; i < alt_count; i++) {
                new_alts[i] = alts[i];
            }
            mem::free(alts.ptr);
            alts = new_alts;
            alt_cap = new_cap;
        }
        alts[alt_count++] = compile_regex_sequence(rc);
    }

    if (alt_count == 1) {
        int one = alts[0];
        mem::free(alts.ptr);
        return one;
    }

    int[] children = mem::new_array(int, (usz)alt_count);
    for (int i = 0; i < alt_count; i++) {
        children[i] = alts[i];
    }
    mem::free(alts.ptr);
    return rc.rb.add(regex_gen_name(rc, "alt"), mk_first(children));
}
