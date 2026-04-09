module pika;

// ============================================================
// Clause property queries
// ============================================================

fn bool is_terminal(Clause* c) {
    switch (c.tag) {
        case SATISFY: return true;
        case SCAN: return true;
        case SCAN_CTX: return true;
        case TOKEN: return true;
        case TOKENS: return true;
        default: return false;
    }
}

fn int[] child_clauses(Clause* c) {
    switch (c.tag) {
        case SEQ: return c.children;
        case FIRST: return c.children;
        case NOT_FOLLOWED_BY: return (&c.child)[0..0];
        case FOLLOWED_BY: return (&c.child)[0..0];
        case SOME_CLAUSE: return (&c.child)[0..0];
        case MANY_CLAUSE: return (&c.child)[0..0];
        case TIE: return (&c.child)[0..0];
        default:
            int[] empty;
            return empty;
    }
}

fn bool clause_can_match_epsilon(Clause* c, bool[] child_eps) {
    switch (c.tag) {
        case SATISFY: return false;
        case SCAN: return false;
        case SCAN_CTX: return false;
        case TOKEN: return false;
        case TOKENS: return false;
        case FAIL: return false;
        case EPSILON: return true;
        case END_OF_INPUT: return true;
        case SEQ:
            // all children must be emptiable
            for (int i = 0; i < (int)child_eps.len; i++) {
                if (!child_eps[i]) return false;
            }
            return true;
        case FIRST:
            if (child_eps.len == 0) return false;
            // non-last children must NOT be emptiable (enforced)
            for (int i = 0; i < (int)child_eps.len - 1; i++) {
                if (child_eps[i]) return false; // error condition
            }
            return child_eps[child_eps.len - 1];
        case NOT_FOLLOWED_BY:
            if (child_eps[0]) return false; // error: contents must not be epsilon
            return true;
        case FOLLOWED_BY:
            return child_eps[0];
        case SOME_CLAUSE:
            if (child_eps[0]) return false; // error: contents must not be epsilon
            return false;
        case MANY_CLAUSE:
            if (child_eps[0]) return false; // error: contents must not be epsilon
            return true;
        case TIE:
            return child_eps[0];
    }
}

// seeded_by: which children seed this clause
fn int[] seeded_by(Clause* c, bool[] child_eps) {
    switch (c.tag) {
        case SEQ:
            if (c.children.len == 0) {
                int[] empty;
                return empty;
            }
            // seed children up to first non-epsilon-matchable
            int first_nonempty = -1;
            bool all_eps = true;
            for (int i = 0; i < (int)child_eps.len; i++) {
                if (!child_eps[i]) {
                    all_eps = false;
                    first_nonempty = i;
                    break;
                }
            }
            if (all_eps) first_nonempty = (int)child_eps.len - 1;
            return c.children[0..first_nonempty];
        case FIRST: return c.children;
        case FOLLOWED_BY: return (&c.child)[0..0];
        case SOME_CLAUSE: return (&c.child)[0..0];
        case MANY_CLAUSE: return (&c.child)[0..0];
        case TIE: return (&c.child)[0..0];
        default:
            int[] empty;
            return empty;
    }
}

fn bool better_match_than(Clause* clause, Match* new_m, Match* old_m) {
    if (clause.tag == FIRST) {
        if (new_m.option_idx == old_m.option_idx) {
            return new_m.last > old_m.last;
        }
        return new_m.option_idx < old_m.option_idx;
    }
    return new_m.last > old_m.last;
}
