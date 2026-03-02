module pika;

import std::io;

// ============================================================
// Frontend API - Rule building functions
// Equivalent to Julia's satisfy(), token(), seq(), first(), etc.
//
// These create RuleEntry arrays ready for make_grammar().
// ============================================================

// Helper: create a Clause with SATISFY tag
fn Clause mk_satisfy(SatisfyFn f) {
    Clause c;
    c.tag = ClauseTag.SATISFY;
    c.satisfy_fn = f;
    return c;
}

fn Clause mk_scan(ScanFn f) {
    Clause c;
    c.tag = ClauseTag.SCAN;
    c.scan_fn = f;
    return c;
}

fn Clause mk_token(char ch) {
    Clause c;
    c.tag = ClauseTag.TOKEN;
    c.token_val = ch;
    return c;
}

fn Clause mk_tokens(char[] toks) {
    Clause c;
    c.tag = ClauseTag.TOKENS;
    c.tokens_val = toks;
    return c;
}

fn Clause mk_epsilon() {
    Clause c;
    c.tag = ClauseTag.EPSILON;
    return c;
}

fn Clause mk_fail() {
    Clause c;
    c.tag = ClauseTag.FAIL;
    return c;
}

fn Clause mk_end_of_input() {
    Clause c;
    c.tag = ClauseTag.END_OF_INPUT;
    return c;
}

fn Clause mk_seq(int[] children) {
    Clause c;
    c.tag = ClauseTag.SEQ;
    c.children = children;
    return c;
}

fn Clause mk_first(int[] children) {
    Clause c;
    c.tag = ClauseTag.FIRST;
    c.children = children;
    return c;
}

fn Clause mk_some(int child) {
    Clause c;
    c.tag = ClauseTag.SOME_CLAUSE;
    c.child = child;
    return c;
}

fn Clause mk_many(int child) {
    Clause c;
    c.tag = ClauseTag.MANY_CLAUSE;
    c.child = child;
    return c;
}

fn Clause mk_tie(int child) {
    Clause c;
    c.tag = ClauseTag.TIE;
    c.child = child;
    return c;
}

fn Clause mk_followed_by(int child) {
    Clause c;
    c.tag = ClauseTag.FOLLOWED_BY;
    c.child = child;
    return c;
}

fn Clause mk_not_followed_by(int child) {
    Clause c;
    c.tag = ClauseTag.NOT_FOLLOWED_BY;
    c.child = child;
    return c;
}

// ============================================================
// RuleBuilder - convenience for building rule arrays
// ============================================================

struct RuleBuilder {
    RuleEntry[] rules;
    int len;
    int cap;
}

fn RuleBuilder rb_new() {
    RuleBuilder rb;
    rb.cap = 32;
    rb.rules = mem::new_array(RuleEntry, 32);
    rb.len = 0;
    return rb;
}

fn int RuleBuilder.add(RuleBuilder* self, String name, Clause clause) {
    if (self.len >= self.cap) {
        int new_cap = self.cap * 2;
        RuleEntry[] new_rules = mem::new_array(RuleEntry, (usz)new_cap);
        for (int i = 0; i < self.len; i++) {
            new_rules[i] = self.rules[i];
        }
        mem::free(self.rules.ptr);
        self.rules = new_rules;
        self.cap = new_cap;
    }
    int idx = self.len;
    RuleEntry entry;
    entry.name = name;
    entry.clause = clause;
    self.rules[idx] = entry;
    self.len++;
    return idx;
}

fn int RuleBuilder.find(RuleBuilder* self, String name) {
    for (int i = 0; i < self.len; i++) {
        if (str_eq(self.rules[i].name, name)) return i;
    }
    return -1;
}

fn RuleEntry[] RuleBuilder.to_array(RuleBuilder* self) {
    if (self.len == 0) {
        RuleEntry[] empty;
        return empty;
    }
    return self.rules[0..(usz)(self.len - 1)];
}

// ============================================================
// Convenience: build int[] for children
// ============================================================

fn int[] make_children_2(int a, int b) {
    int[] ch = mem::new_array(int, 2);
    ch[0] = a;
    ch[1] = b;
    return ch;
}

fn int[] make_children_3(int a, int b, int c) {
    int[] ch = mem::new_array(int, 3);
    ch[0] = a;
    ch[1] = b;
    ch[2] = c;
    return ch;
}

fn int[] make_children_4(int a, int b, int c, int d) {
    int[] ch = mem::new_array(int, 4);
    ch[0] = a;
    ch[1] = b;
    ch[2] = c;
    ch[3] = d;
    return ch;
}

fn int[] make_children_1(int a) {
    int[] ch = mem::new_array(int, 1);
    ch[0] = a;
    return ch;
}
