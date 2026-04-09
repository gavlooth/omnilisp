module pika;

import std::core::mem;

fn int compile_regex_quantified(RegexCompiler* rc) {
    int atom = compile_regex_atom(rc);
    if (atom < 0) return -1;

    int saved_pos = rc.tok.pos;
    RegexToken qtok = rc.tok.next_token();

    switch (qtok.type) {
        case RE_STAR:
        case RE_STAR_POSS:
            return rc.rb.add(regex_gen_name(rc, "star"), mk_many(atom));

        case RE_PLUS:
        case RE_PLUS_POSS:
            return rc.rb.add(regex_gen_name(rc, "plus"), mk_some(atom));

        case RE_QUESTION:
        case RE_QUEST_POSS:
            int eps = rc.rb.add("_eps", mk_epsilon());
            int[] alts = make_children_2(atom, eps);
            return rc.rb.add(regex_gen_name(rc, "opt"), mk_first(alts));

        case RE_LBRACE:
            int min = qtok.bounds.min;
            int max = qtok.bounds.max;
            int required_parts = max < 0 ? min : max;
            if (max < 0) {
                if (min == int.max) {
                    rc.tok.set_error("Bounded quantifier too large for compiled regex");
                    return rc.rb.add(regex_gen_name(rc, "err"), mk_fail());
                }
                required_parts = min + 1;
            }
            if (required_parts > REGEX_QUANTIFIER_COMPILE_PARTS_MAX) {
                rc.tok.set_error("Bounded quantifier too large for compiled regex");
                return rc.rb.add(regex_gen_name(rc, "err"), mk_fail());
            }

            int[] parts;
            int part_count = 0;
            int part_cap = 0;

            for (int i = 0; i < min; i++) {
                regex_parts_push(&parts, &part_count, &part_cap, atom);
            }

            if (max < 0) {
                int rest = rc.rb.add(regex_gen_name(rc, "many"), mk_many(atom));
                regex_parts_push(&parts, &part_count, &part_cap, rest);
            } else if (max > min) {
                int eps2 = rc.rb.add("_eps", mk_epsilon());
                int[] opt_alts = make_children_2(atom, eps2);
                int opt = rc.rb.add(regex_gen_name(rc, "bnd"), mk_first(opt_alts));
                for (int i = min; i < max; i++) {
                    regex_parts_push(&parts, &part_count, &part_cap, opt);
                }
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

        default:
            rc.tok.pos = saved_pos;
            return atom;
    }
}
