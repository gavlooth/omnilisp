module pika;

import std::core::mem;
import lisp;

// Grammar compiler context
struct GrammarCompiler {
    RuleBuilder rb;
    lisp::Interp* interp;
    // Map rule names to indices
    lisp::SymbolId* rule_names;
    int* rule_indices;
    int rule_count;
    int rule_capacity;
    bool has_error;
    char[] error_code;
    char[] error_message;
}

fn void GrammarCompiler.init(GrammarCompiler* self, lisp::Interp* interp) {
    self.rb = rb_new();
    self.interp = interp;
    self.rule_names = null;
    self.rule_indices = null;
    self.rule_count = 0;
    self.rule_capacity = 0;
    self.has_error = false;
    self.error_code = "";
    self.error_message = "";

    self.rule_capacity = 32;
    self.rule_names = (lisp::SymbolId*)mem::malloc(lisp::SymbolId.sizeof * self.rule_capacity);
    self.rule_indices = (int*)mem::malloc(int.sizeof * self.rule_capacity);
    if (self.rule_names == null || self.rule_indices == null) {
        if (self.rule_names != null) mem::free(self.rule_names);
        if (self.rule_indices != null) mem::free(self.rule_indices);
        self.rule_names = null;
        self.rule_indices = null;
        self.rule_capacity = 0;
        self.has_error = true;
        self.error_code = "parser/grammar-oom";
        self.error_message = "pika/grammar: failed to allocate rule table";
    }
}

fn void gc_set_error(GrammarCompiler* gc, char[] code, char[] message) {
    if (gc.has_error) return;
    gc.has_error = true;
    gc.error_code = code;
    gc.error_message = message;
}

fn bool gc_ensure_rule_capacity(GrammarCompiler* gc) {
    if (gc.rule_count < gc.rule_capacity) return true;
    int new_cap = gc.rule_capacity < 32 ? 32 : gc.rule_capacity * 2;
    lisp::SymbolId* new_names = (lisp::SymbolId*)mem::malloc(lisp::SymbolId.sizeof * new_cap);
    int* new_indices = (int*)mem::malloc(int.sizeof * new_cap);
    if (new_names == null || new_indices == null) {
        if (new_names != null) mem::free(new_names);
        if (new_indices != null) mem::free(new_indices);
        gc_set_error(gc, "parser/grammar-oom", "pika/grammar: failed to grow rule table");
        return false;
    }
    for (int i = 0; i < gc.rule_count; i++) {
        new_names[i] = gc.rule_names[i];
        new_indices[i] = gc.rule_indices[i];
    }
    mem::free(gc.rule_names);
    mem::free(gc.rule_indices);
    gc.rule_names = new_names;
    gc.rule_indices = new_indices;
    gc.rule_capacity = new_cap;
    return true;
}

fn int GrammarCompiler.find_rule(GrammarCompiler* self, lisp::SymbolId name) {
    for (int i = 0; i < self.rule_count; i++) {
        if ((uint)self.rule_names[i] == (uint)name) {
            return self.rule_indices[i];
        }
    }
    return -1;
}

fn int GrammarCompiler.add_rule(GrammarCompiler* self, lisp::SymbolId name, Clause clause) {
    char[] name_str = self.interp.symbols.get_name(name);
    int idx = self.rb.add((String)name_str, clause);
    if (!gc_ensure_rule_capacity(self)) return idx;
    self.rule_names[self.rule_count] = name;
    self.rule_indices[self.rule_count] = idx;
    self.rule_count++;
    return idx;
}

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

// Helper: compile scan patterns that start with a regex-style character class.
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

fn int count_clause_elements(lisp::Value* rest) {
    int elem_count = 0;
    lisp::Value* curr = rest;
    while (lisp::is_cons(curr)) {
        elem_count++;
        curr = curr.cons_val.cdr;
    }
    return elem_count;
}

fn int[] compile_clause_children_from_list(GrammarCompiler* gc, lisp::Value* rest, int elem_count) {
    int[] children = mem::new_array(int, (usz)elem_count);
    lisp::Value* curr = rest;
    for (int i = 0; i < elem_count && lisp::is_cons(curr); i++) {
        children[i] = compile_clause(gc, curr.cons_val.car);
        curr = curr.cons_val.cdr;
    }
    return children;
}

fn int compile_clause_string(GrammarCompiler* gc, lisp::Value* clause) {
    char[] str = clause.str_chars[:clause.str_len];
    if (str.len == 1) {
        return gc.rb.add("_tok", mk_token(str[0]));
    }
    return gc.rb.add("_str", mk_tokens(str));
}

fn int compile_clause_symbol_ref(GrammarCompiler* gc, lisp::Value* clause) {
    lisp::SymbolId sym = clause.sym_val;
    int existing = gc.find_rule(sym);
    if (existing >= 0) return existing;

    char[] sym_name = gc.interp.symbols.get_name(sym);
    int placeholder = gc.rb.add((String)sym_name, mk_fail());
    if (!gc_ensure_rule_capacity(gc)) return placeholder;
    gc.rule_names[gc.rule_count] = sym;
    gc.rule_indices[gc.rule_count] = placeholder;
    gc.rule_count++;
    return placeholder;
}

fn int compile_clause_seq_op(GrammarCompiler* gc, lisp::Value* rest) {
    int elem_count = count_clause_elements(rest);
    if (elem_count == 0) return gc.rb.add("_eps", mk_epsilon());
    int[] children = compile_clause_children_from_list(gc, rest, elem_count);
    return gc.rb.add("_seq", mk_seq(children));
}

fn int compile_clause_first_op(GrammarCompiler* gc, lisp::Value* rest) {
    int elem_count = count_clause_elements(rest);
    if (elem_count == 0) return gc.rb.add("_fail", mk_fail());
    int[] children = compile_clause_children_from_list(gc, rest, elem_count);
    return gc.rb.add("_first", mk_first(children));
}

fn int compile_clause_named_many(GrammarCompiler* gc, int child) {
    return gc.rb.add("_many", mk_many(child));
}

fn int compile_clause_named_some(GrammarCompiler* gc, int child) {
    return gc.rb.add("_some", mk_some(child));
}

fn int compile_clause_named_opt(GrammarCompiler* gc, int child) {
    int eps = gc.rb.add("_eps", mk_epsilon());
    int[] children = make_children_2(child, eps);
    return gc.rb.add("_opt", mk_first(children));
}

fn int compile_clause_named_not(GrammarCompiler* gc, int child) {
    return gc.rb.add("_not", mk_not_followed_by(child));
}

fn int compile_clause_named_and(GrammarCompiler* gc, int child) {
    return gc.rb.add("_and", mk_followed_by(child));
}

fn bool compile_clause_many_op(GrammarCompiler* gc, char[] op_name, lisp::Value* rest, int* out_idx) {
    if (!str_eq_c(op_name, "many") && !str_eq_c(op_name, "*")) return false;
    if (!lisp::is_cons(rest)) {
        *out_idx = gc.rb.add("_eps", mk_epsilon());
        return true;
    }
    int child = compile_clause(gc, rest.cons_val.car);
    *out_idx = compile_clause_named_many(gc, child);
    return true;
}

fn bool compile_clause_some_op(GrammarCompiler* gc, char[] op_name, lisp::Value* rest, int* out_idx) {
    if (!str_eq_c(op_name, "some") && !str_eq_c(op_name, "+")) return false;
    if (!lisp::is_cons(rest)) {
        *out_idx = gc.rb.add("_fail", mk_fail());
        return true;
    }
    int child = compile_clause(gc, rest.cons_val.car);
    *out_idx = compile_clause_named_some(gc, child);
    return true;
}

fn bool compile_clause_opt_op(GrammarCompiler* gc, char[] op_name, lisp::Value* rest, int* out_idx) {
    if (!str_eq_c(op_name, "opt") && !str_eq_c(op_name, "?")) return false;
    if (!lisp::is_cons(rest)) {
        *out_idx = gc.rb.add("_eps", mk_epsilon());
        return true;
    }
    int child = compile_clause(gc, rest.cons_val.car);
    *out_idx = compile_clause_named_opt(gc, child);
    return true;
}

fn bool compile_clause_scan_op(GrammarCompiler* gc, char[] op_name, lisp::Value* rest, int* out_idx) {
    if (!str_eq_c(op_name, "scan")) return false;
    if (!lisp::is_cons(rest) || !lisp::is_string(rest.cons_val.car)) {
        *out_idx = gc.rb.add("_fail", mk_fail());
        return true;
    }
    lisp::Value* pattern_val = rest.cons_val.car;
    char[] pattern = pattern_val.str_chars[:pattern_val.str_len];
    if (pattern.len > 0 && pattern[0] == '[') {
        *out_idx = make_char_class_scanner(gc, pattern);
    } else {
        *out_idx = gc.rb.add("_scan_lit", mk_tokens(pattern));
    }
    return true;
}

fn bool compile_clause_not_op(GrammarCompiler* gc, char[] op_name, lisp::Value* rest, int* out_idx) {
    if (!str_eq_c(op_name, "not") && !str_eq_c(op_name, "!")) return false;
    if (!lisp::is_cons(rest)) {
        *out_idx = gc.rb.add("_eps", mk_epsilon());
        return true;
    }
    int child = compile_clause(gc, rest.cons_val.car);
    *out_idx = compile_clause_named_not(gc, child);
    return true;
}

fn bool compile_clause_and_op(GrammarCompiler* gc, char[] op_name, lisp::Value* rest, int* out_idx) {
    if (!str_eq_c(op_name, "and") && !str_eq_c(op_name, "&")) return false;
    if (!lisp::is_cons(rest)) {
        *out_idx = gc.rb.add("_eps", mk_epsilon());
        return true;
    }
    int child = compile_clause(gc, rest.cons_val.car);
    *out_idx = compile_clause_named_and(gc, child);
    return true;
}

fn bool compile_clause_end_op(GrammarCompiler* gc, char[] op_name, int* out_idx) {
    if (!str_eq_c(op_name, "end") && !str_eq_c(op_name, "$")) return false;
    *out_idx = gc.rb.add("_end", mk_end_of_input());
    return true;
}

fn bool compile_clause_unary_op(GrammarCompiler* gc, char[] op_name, lisp::Value* rest, int* out_idx) {
    if (compile_clause_many_op(gc, op_name, rest, out_idx)) return true;
    if (compile_clause_some_op(gc, op_name, rest, out_idx)) return true;
    if (compile_clause_opt_op(gc, op_name, rest, out_idx)) return true;
    if (compile_clause_scan_op(gc, op_name, rest, out_idx)) return true;
    if (compile_clause_not_op(gc, op_name, rest, out_idx)) return true;
    if (compile_clause_and_op(gc, op_name, rest, out_idx)) return true;
    return compile_clause_end_op(gc, op_name, out_idx);
}

fn int compile_clause_list_operator(GrammarCompiler* gc, char[] op_name, lisp::Value* rest) {
    if (str_eq_c(op_name, "seq")) return compile_clause_seq_op(gc, rest);
    if (str_eq_c(op_name, "first") || str_eq_c(op_name, "or") || str_eq_c(op_name, "|")) {
        return compile_clause_first_op(gc, rest);
    }

    int unary_idx = 0;
    if (compile_clause_unary_op(gc, op_name, rest, &unary_idx)) return unary_idx;

    gc_set_error(gc, "parser/grammar-unknown-operator", "pika/grammar: unknown grammar operator");
    return gc.rb.add("_err", mk_fail());
}

// Compile a clause expression to a rule index
fn int compile_clause(GrammarCompiler* gc, lisp::Value* clause) {
    if (clause == null || lisp::is_nil(clause)) {
        return gc.rb.add("_eps", mk_epsilon());
    }

    // String literal -> TOKENS
    if (lisp::is_string(clause)) return compile_clause_string(gc, clause);

    // Symbol -> reference to another rule
    if (lisp::is_symbol(clause)) return compile_clause_symbol_ref(gc, clause);

    // List -> combinator (seq, first, many, some, etc.)
    if (lisp::is_cons(clause)) {
        lisp::Value* head = clause.cons_val.car;
        lisp::Value* rest = clause.cons_val.cdr;

        if (!lisp::is_symbol(head)) {
            gc_set_error(gc, "parser/grammar-clause-head-not-symbol", "pika/grammar: grammar clause head must be a symbol");
            return gc.rb.add("_err", mk_fail());
        }

        lisp::SymbolId op = head.sym_val;
        char[] op_name = gc.interp.symbols.get_name(op);
        return compile_clause_list_operator(gc, op_name, rest);
    }

    gc_set_error(gc, "parser/grammar-invalid-clause-type", "pika/grammar: invalid clause type");
    return gc.rb.add("_err", mk_fail());
}

fn bool str_eq_c(char[] a, char[] b) {
    if (a.len != b.len) return false;
    for (usz i = 0; i < a.len; i++) {
        if (a[i] != b[i]) return false;
    }
    return true;
}

fn bool parse_rule_definition(GrammarCompiler* gc, lisp::Value* rule_def, lisp::Interp* interp,
    lisp::SymbolId* rule_name, lisp::Value** clause_out) {
    if (!lisp::is_cons(rule_def)) {
        gc_set_error(gc, "parser/grammar-rule-not-list", "pika/grammar: rule must be a list");
        return false;
    }

    lisp::Value* head = rule_def.cons_val.car;
    if (!lisp::is_symbol(head)) {
        gc_set_error(gc, "parser/grammar-rule-head-not-symbol", "pika/grammar: rule definition must start with 'rule");
        return false;
    }

    char[] head_name = interp.symbols.get_name(head.sym_val);
    if (!str_eq_c(head_name, "rule")) {
        gc_set_error(gc, "parser/grammar-rule-head-not-rule", "pika/grammar: expected rule definition head 'rule");
        return false;
    }

    lisp::Value* rest = rule_def.cons_val.cdr;
    if (!lisp::is_cons(rest)) {
        gc_set_error(gc, "parser/grammar-rule-missing-name-clause", "pika/grammar: rule requires name and clause");
        return false;
    }

    lisp::Value* rule_name_val = rest.cons_val.car;
    if (!lisp::is_symbol(rule_name_val)) {
        gc_set_error(gc, "parser/grammar-rule-name-not-symbol", "pika/grammar: rule name must be a symbol");
        return false;
    }
    *rule_name = rule_name_val.sym_val;

    lisp::Value* clause_rest = rest.cons_val.cdr;
    if (!lisp::is_cons(clause_rest)) {
        gc_set_error(gc, "parser/grammar-rule-missing-clause", "pika/grammar: rule requires a clause");
        return false;
    }
    *clause_out = clause_rest.cons_val.car;
    return true;
}

fn void gc_register_rule(GrammarCompiler* gc, lisp::Interp* interp, lisp::SymbolId rule_name, int clause_idx) {
    int existing = gc.find_rule(rule_name);
    if (existing >= 0) {
        gc.rb.rules[existing].clause = gc.rb.rules[clause_idx].clause;
        return;
    }
    char[] name_str = interp.symbols.get_name(rule_name);
    gc.rb.rules[clause_idx].name = (String)name_str;
    if (!gc_ensure_rule_capacity(gc)) return;
    gc.rule_names[gc.rule_count] = rule_name;
    gc.rule_indices[gc.rule_count] = clause_idx;
    gc.rule_count++;
}

fn lisp::Value* finalize_named_grammar(lisp::Interp* interp, lisp::SymbolId grammar_name, GrammarCompiler* gc) {
    if (gc.rb.len == 0) {
        return parser_raise(interp, "parser/grammar-no-rules", "pika/grammar: grammar has no rules");
    }
    if (gc.rule_count == 0) {
        return parser_raise(interp, "parser/grammar-no-rules", "pika/grammar: grammar has no rules");
    }

    String[] starts = mem::new_array(String, 1);
    starts[0] = gc.rb.rules[gc.rule_indices[0]].name;
    Grammar grammar = make_grammar(starts, gc.rb.to_array());
    int start_clause = find_name_index(&grammar, starts[0]);
    if (start_clause < 0) {
        return parser_raise(interp, "parser/grammar-missing-start-rule", "pika/grammar: failed to resolve start rule");
    }

    if (!store_named_grammar(grammar_name, grammar, start_clause)) {
        return parser_raise(interp, "parser/grammar-oom", "pika/grammar: failed to grow grammar table");
    }
    return lisp::make_symbol(interp, grammar_name);
}

// (pika/grammar name rules...) -> grammar object
// Example: (pika/grammar expr
//            (rule number (scan "[0-9]+"))
//            (rule op (first "+" "-" "*" "/"))
//            (rule expr (first number (seq "(" expr op expr ")"))))
fn lisp::Value* prim_pika_grammar(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 2) {
        return parser_raise(interp, "parser/grammar-expected-name-rules", "pika/grammar: expected name and at least one rule");
    }

    // First arg is grammar name
    if (args[0].tag != lisp::ValueTag.SYMBOL) {
        return parser_raise(interp, "parser/grammar-name-not-symbol", "pika/grammar: name must be a symbol");
    }

    lisp::SymbolId grammar_name = args[0].sym_val;

    // Initialize compiler
    GrammarCompiler gc;
    gc.init(interp);
    defer mem::free(gc.rb.rules.ptr);
    defer if (gc.rule_names != null) mem::free(gc.rule_names);
    defer if (gc.rule_indices != null) mem::free(gc.rule_indices);
    if (gc.has_error) {
        return parser_raise(interp, gc.error_code, gc.error_message);
    }

    // Process each (rule name clause) definition
    for (usz i = 1; i < args.len; i++) {
        lisp::Value* rule_def = args[i];
        lisp::SymbolId rule_name = (lisp::SymbolId)0;
        lisp::Value* clause = null;
        if (!parse_rule_definition(&gc, rule_def, interp, &rule_name, &clause)) {
            return parser_raise(interp, gc.error_code, gc.error_message);
        }

        // Compile the clause
        int clause_idx = compile_clause(&gc, clause);
        if (gc.has_error) return parser_raise(interp, gc.error_code, gc.error_message);
        gc_register_rule(&gc, interp, rule_name, clause_idx);
    }
    return finalize_named_grammar(interp, grammar_name, &gc);
}
