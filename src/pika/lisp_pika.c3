module pika;

import std::core::mem;
import std::io;
import lisp;

// ============================================================
// Pika Lisp Primitives
//
// Exposes PikaParser functionality to Lisp:
//   - re-match, re-find-all, re-split, re-replace, re-fullmatch
//   - pika/grammar, pika/parse, pika/fold
// ============================================================

// ============================================================
// Regex Primitives
// ============================================================

// (re-match pattern input) -> matched-string or nil
fn lisp::Value* prim_re_match(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 2) {
        return lisp::make_nil(interp);
    }

    lisp::Value* pattern_val = args[0];
    lisp::Value* input_val = args[1];

    if (pattern_val.tag != lisp::ValueTag.STRING || input_val.tag != lisp::ValueTag.STRING) {
        return lisp::make_nil(interp);
    }

    // C3 slice [a..b] is INCLUSIVE on both ends, [:n] means first n elements
    char[] pattern = pattern_val.str_val.chars[:pattern_val.str_val.len];
    char[] input = input_val.str_val.chars[:input_val.str_val.len];

    RegexMatch m = regex_search(pattern, input);

    if (m.matched) {
        return lisp::make_string(interp, m.view);
    }

    return lisp::make_nil(interp);
}

// (re-fullmatch pattern input) -> matched-string or nil
fn lisp::Value* prim_re_fullmatch(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 2) {
        return lisp::make_nil(interp);
    }

    lisp::Value* pattern_val = args[0];
    lisp::Value* input_val = args[1];

    if (pattern_val.tag != lisp::ValueTag.STRING || input_val.tag != lisp::ValueTag.STRING) {
        return lisp::make_nil(interp);
    }

    char[] pattern = pattern_val.str_val.chars[:pattern_val.str_val.len];
    char[] input = input_val.str_val.chars[:input_val.str_val.len];

    RegexMatch m = regex_fullmatch(pattern, input);

    if (m.matched) {
        return lisp::make_string(interp, m.view);
    }

    return lisp::make_nil(interp);
}

// (re-find-all pattern input) -> list of matched strings
fn lisp::Value* prim_re_find_all(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 2) {
        return lisp::make_nil(interp);
    }

    lisp::Value* pattern_val = args[0];
    lisp::Value* input_val = args[1];

    if (pattern_val.tag != lisp::ValueTag.STRING || input_val.tag != lisp::ValueTag.STRING) {
        return lisp::make_nil(interp);
    }

    char[] pattern = pattern_val.str_val.chars[:pattern_val.str_val.len];
    char[] input = input_val.str_val.chars[:input_val.str_val.len];

    int count = 0;
    RegexMatch[64] matches = regex_find_all(pattern, input, &count);

    // Build list of matched strings
    lisp::Value* result = lisp::make_nil(interp);
    for (int i = count - 1; i >= 0; i--) {
        lisp::Value* s = lisp::make_string(interp, matches[i].view);
        result = lisp::make_cons(interp, s, result);
    }

    return result;
}

// (re-split pattern input) -> list of strings
fn lisp::Value* prim_re_split(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 2) {
        return lisp::make_nil(interp);
    }

    lisp::Value* pattern_val = args[0];
    lisp::Value* input_val = args[1];

    if (pattern_val.tag != lisp::ValueTag.STRING || input_val.tag != lisp::ValueTag.STRING) {
        return lisp::make_nil(interp);
    }

    char[] pattern = pattern_val.str_val.chars[:pattern_val.str_val.len];
    char[] input = input_val.str_val.chars[:input_val.str_val.len];

    // Find all matches
    int count = 0;
    RegexMatch[64] matches = regex_find_all(pattern, input, &count);

    // Build list of strings between matches
    lisp::Value*[65] parts;
    int part_count = 0;
    int last_end = 0;

    for (int i = 0; i < count && part_count < 64; i++) {
        if (matches[i].start > last_end) {
            char[] part = input[last_end..matches[i].start - 1];
            parts[part_count++] = lisp::make_string(interp, part);
        }
        last_end = matches[i].end + 1;
    }

    // Add final part
    if (last_end < (int)input.len && part_count < 65) {
        char[] part = input[last_end..];
        parts[part_count++] = lisp::make_string(interp, part);
    }

    // Build result list
    lisp::Value* result = lisp::make_nil(interp);
    for (int i = part_count - 1; i >= 0; i--) {
        result = lisp::make_cons(interp, parts[i], result);
    }

    return result;
}

// (re-replace pattern replacement input) -> new string
// (re-replace pattern replacement input 'global) -> replace all
fn lisp::Value* prim_re_replace(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 3) {
        return lisp::make_nil(interp);
    }

    lisp::Value* pattern_val = args[0];
    lisp::Value* replacement_val = args[1];
    lisp::Value* input_val = args[2];

    if (pattern_val.tag != lisp::ValueTag.STRING ||
        replacement_val.tag != lisp::ValueTag.STRING ||
        input_val.tag != lisp::ValueTag.STRING) {
        return lisp::make_nil(interp);
    }

    char[] pattern = pattern_val.str_val.chars[:pattern_val.str_val.len];
    char[] replacement = replacement_val.str_val.chars[:replacement_val.str_val.len];
    char[] input = input_val.str_val.chars[:input_val.str_val.len];

    bool global = false;
    if (args.len >= 4 && args[3].tag == lisp::ValueTag.SYMBOL) {
        // Check for 'global flag
        global = true;
    }

    // Build result string
    char[1024] result_buf;
    usz result_len = 0;

    if (global) {
        // Replace all occurrences
        int count = 0;
        RegexMatch[64] matches = regex_find_all(pattern, input, &count);

        int last_end = 0;
        for (int i = 0; i < count; i++) {
            // Copy text before match
            for (int j = last_end; j < matches[i].start && result_len < 1023; j++) {
                result_buf[result_len++] = input[j];
            }
            // Copy replacement
            for (usz j = 0; j < replacement.len && result_len < 1023; j++) {
                result_buf[result_len++] = replacement[j];
            }
            last_end = matches[i].end + 1;
        }
        // Copy remaining
        for (int j = last_end; j < (int)input.len && result_len < 1023; j++) {
            result_buf[result_len++] = input[j];
        }
    } else {
        // Replace first occurrence only
        RegexMatch m = regex_search(pattern, input);

        if (m.matched) {
            // Copy text before match
            for (int j = 0; j < m.start && result_len < 1023; j++) {
                result_buf[result_len++] = input[j];
            }
            // Copy replacement
            for (usz j = 0; j < replacement.len && result_len < 1023; j++) {
                result_buf[result_len++] = replacement[j];
            }
            // Copy text after match
            for (int j = m.end + 1; j < (int)input.len && result_len < 1023; j++) {
                result_buf[result_len++] = input[j];
            }
        } else {
            // No match, return original
            for (usz j = 0; j < input.len && result_len < 1023; j++) {
                result_buf[result_len++] = input[j];
            }
        }
    }

    result_buf[result_len] = 0;
    if (result_len == 0) return lisp::make_string(interp, "");
    return lisp::make_string(interp, result_buf[:result_len]);
}

// ============================================================
// Grammar Definition Primitives
// ============================================================

// Stored grammars by name
struct NamedGrammar {
    lisp::SymbolId name;
    Grammar grammar;
    bool valid;
}

NamedGrammar[16] g_named_grammars;
int g_grammar_count = 0;

// Global Lisp parser (lazy-initialized)
LispPikaParser g_lisp_parser;
bool g_lisp_parser_initialized = false;

fn void ensure_pika_parser() {
    if (!g_lisp_parser_initialized) {
        g_lisp_parser = create_lisp_parser();
        g_lisp_parser_initialized = true;
    }
}

// Find a stored grammar by name
fn NamedGrammar* find_grammar(lisp::SymbolId name) {
    for (int i = 0; i < g_grammar_count; i++) {
        if ((uint)g_named_grammars[i].name == (uint)name && g_named_grammars[i].valid) {
            return &g_named_grammars[i];
        }
    }
    return null;
}

// Grammar compiler context
struct GrammarCompiler {
    RuleBuilder rb;
    lisp::Interp* interp;
    // Map rule names to indices
    lisp::SymbolId[64] rule_names;
    int[64] rule_indices;
    int rule_count;
}

fn void GrammarCompiler.init(GrammarCompiler* self, lisp::Interp* interp) {
    self.rb = rb_new();
    self.interp = interp;
    self.rule_count = 0;
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
    if (self.rule_count < 64) {
        self.rule_names[self.rule_count] = name;
        self.rule_indices[self.rule_count] = idx;
        self.rule_count++;
    }
    return idx;
}

// Helper: make scan function for a character class pattern like "[a-z]"
fn int make_char_class_scanner(GrammarCompiler* gc, char[] pattern) {
    // Parse the pattern and create a SCAN clause
    // For now, create a simple scan function based on the pattern

    // Generate unique name for this scanner
    char[64] name;
    usz nlen = 0;
    char[] prefix = "_scan_";
    for (usz i = 0; i < prefix.len && nlen < 60; i++) name[nlen++] = prefix[i];
    // Add pattern hash
    int hash = 0;
    for (usz i = 0; i < pattern.len; i++) hash = hash * 31 + pattern[i];
    if (hash < 0) hash = -hash;
    // Convert to decimal
    char[12] digits;
    int dlen = 0;
    int h = hash;
    if (h == 0) { digits[dlen++] = '0'; }
    else {
        while (h > 0 && dlen < 10) { digits[dlen++] = (char)('0' + h % 10); h /= 10; }
    }
    for (int i = dlen - 1; i >= 0 && nlen < 63; i--) name[nlen++] = digits[i];
    name[nlen] = 0;

    // For character classes, we need a scan function
    // Since we can't create closures, we'll use a token-based approach
    // Simple patterns: just match the literal for now
    // TODO: Full character class parsing

    if (pattern.len > 0 && pattern[0] == '[') {
        // Character class - parse it
        CharClassData* cc = mem::new(CharClassData);
        cc.num_ranges = 0;
        cc.num_chars = 0;
        cc.negated = false;

        int i = 1;
        if (i < (int)pattern.len && pattern[i] == '^') {
            cc.negated = true;
            i++;
        }

        while (i < (int)pattern.len && pattern[i] != ']') {
            char c = pattern[i];
            if (i + 2 < (int)pattern.len && pattern[i + 1] == '-' && pattern[i + 2] != ']') {
                // Range a-z
                if (cc.num_ranges < 64) {
                    cc.ranges[cc.num_ranges].start = c;
                    cc.ranges[cc.num_ranges].end = pattern[i + 2];
                    cc.num_ranges++;
                }
                i += 3;
            } else {
                // Single char
                if (cc.num_chars < 64) {
                    cc.chars[cc.num_chars++] = c;
                }
                i++;
            }
        }

        // Store cc pointer in a global array for the scan function
        // This is a workaround since we can't create closures
        return store_char_class_scanner(gc, cc, (String)name[:nlen]);
    }

    // Not a character class, treat as literal string
    return gc.rb.add((String)name[:nlen], mk_tokens(pattern));
}

// Global storage for character class scan functions
CharClassData*[64] g_char_classes;
int g_char_class_count = 0;

fn int scan_char_class_0(char[] view) { return scan_char_in_class(g_char_classes[0], view); }
fn int scan_char_class_1(char[] view) { return scan_char_in_class(g_char_classes[1], view); }
fn int scan_char_class_2(char[] view) { return scan_char_in_class(g_char_classes[2], view); }
fn int scan_char_class_3(char[] view) { return scan_char_in_class(g_char_classes[3], view); }
fn int scan_char_class_4(char[] view) { return scan_char_in_class(g_char_classes[4], view); }
fn int scan_char_class_5(char[] view) { return scan_char_in_class(g_char_classes[5], view); }
fn int scan_char_class_6(char[] view) { return scan_char_in_class(g_char_classes[6], view); }
fn int scan_char_class_7(char[] view) { return scan_char_in_class(g_char_classes[7], view); }

ScanFn[8] g_scan_fns = {
    &scan_char_class_0, &scan_char_class_1, &scan_char_class_2, &scan_char_class_3,
    &scan_char_class_4, &scan_char_class_5, &scan_char_class_6, &scan_char_class_7
};

fn int store_char_class_scanner(GrammarCompiler* gc, CharClassData* cc, String name) {
    if (g_char_class_count >= 8) {
        io::printn("Error: too many character class scanners");
        return gc.rb.add(name, mk_fail());
    }
    int idx = g_char_class_count;
    g_char_classes[idx] = cc;
    g_char_class_count++;
    return gc.rb.add(name, mk_scan(g_scan_fns[idx]));
}

// Compile a clause expression to a rule index
fn int compile_clause(GrammarCompiler* gc, lisp::Value* clause) {
    if (clause == null || lisp::is_nil(clause)) {
        return gc.rb.add("_eps", mk_epsilon());
    }

    // String literal -> TOKENS
    if (lisp::is_string(clause)) {
        char[] str = clause.str_val.chars[:clause.str_val.len];
        if (str.len == 1) {
            char[16] name = "_tok_";
            name[5] = str[0];
            name[6] = 0;
            return gc.rb.add((String)name[:6], mk_token(str[0]));
        }
        char[64] name;
        usz nlen = 0;
        char[] prefix = "_str_";
        for (usz i = 0; i < prefix.len && nlen < 50; i++) name[nlen++] = prefix[i];
        for (usz i = 0; i < str.len && nlen < 60; i++) {
            char c = str[i];
            if (c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9') {
                name[nlen++] = c;
            }
        }
        name[nlen] = 0;
        return gc.rb.add((String)name[:nlen], mk_tokens(str));
    }

    // Symbol -> reference to another rule
    if (lisp::is_symbol(clause)) {
        lisp::SymbolId sym = clause.sym_val;
        int existing = gc.find_rule(sym);
        if (existing >= 0) {
            return existing;
        }
        // Forward reference - add placeholder
        char[] sym_name = gc.interp.symbols.get_name(sym);
        int placeholder = gc.rb.add((String)sym_name, mk_fail());
        if (gc.rule_count < 64) {
            gc.rule_names[gc.rule_count] = sym;
            gc.rule_indices[gc.rule_count] = placeholder;
            gc.rule_count++;
        }
        return placeholder;
    }

    // List -> combinator (seq, first, many, some, etc.)
    if (lisp::is_cons(clause)) {
        lisp::Value* head = clause.cons_val.car;
        lisp::Value* rest = clause.cons_val.cdr;

        if (!lisp::is_symbol(head)) {
            io::printn("Error: grammar clause head must be a symbol");
            return gc.rb.add("_err", mk_fail());
        }

        lisp::SymbolId op = head.sym_val;
        char[] op_name = gc.interp.symbols.get_name(op);

        // (seq a b c ...) -> sequence
        if (str_eq_c(op_name, "seq")) {
            lisp::Value*[32] elems;
            int elem_count = 0;
            lisp::Value* curr = rest;
            while (lisp::is_cons(curr) && elem_count < 32) {
                elems[elem_count++] = curr.cons_val.car;
                curr = curr.cons_val.cdr;
            }

            if (elem_count == 0) {
                return gc.rb.add("_eps", mk_epsilon());
            }

            int[] children = mem::new_array(int, (usz)elem_count);
            for (int i = 0; i < elem_count; i++) {
                children[i] = compile_clause(gc, elems[i]);
            }

            char[32] name = "_seq_";
            name[5] = (char)('0' + gc.rb.len % 10);
            name[6] = 0;
            return gc.rb.add((String)name[:6], mk_seq(children));
        }

        // (first a b c ...) -> ordered choice
        if (str_eq_c(op_name, "first") || str_eq_c(op_name, "or") || str_eq_c(op_name, "|")) {
            lisp::Value*[32] elems;
            int elem_count = 0;
            lisp::Value* curr = rest;
            while (lisp::is_cons(curr) && elem_count < 32) {
                elems[elem_count++] = curr.cons_val.car;
                curr = curr.cons_val.cdr;
            }

            if (elem_count == 0) {
                return gc.rb.add("_fail", mk_fail());
            }

            int[] children = mem::new_array(int, (usz)elem_count);
            for (int i = 0; i < elem_count; i++) {
                children[i] = compile_clause(gc, elems[i]);
            }

            char[32] name = "_first_";
            name[7] = (char)('0' + gc.rb.len % 10);
            name[8] = 0;
            return gc.rb.add((String)name[:8], mk_first(children));
        }

        // (many x) -> zero or more (*)
        if (str_eq_c(op_name, "many") || str_eq_c(op_name, "*")) {
            if (!lisp::is_cons(rest)) {
                return gc.rb.add("_eps", mk_epsilon());
            }
            int child = compile_clause(gc, rest.cons_val.car);
            char[32] name = "_many_";
            name[6] = (char)('0' + gc.rb.len % 10);
            name[7] = 0;
            return gc.rb.add((String)name[:7], mk_many(child));
        }

        // (some x) -> one or more (+)
        if (str_eq_c(op_name, "some") || str_eq_c(op_name, "+")) {
            if (!lisp::is_cons(rest)) {
                return gc.rb.add("_fail", mk_fail());
            }
            int child = compile_clause(gc, rest.cons_val.car);
            char[32] name = "_some_";
            name[6] = (char)('0' + gc.rb.len % 10);
            name[7] = 0;
            return gc.rb.add((String)name[:7], mk_some(child));
        }

        // (opt x) -> optional (?)
        if (str_eq_c(op_name, "opt") || str_eq_c(op_name, "?")) {
            if (!lisp::is_cons(rest)) {
                return gc.rb.add("_eps", mk_epsilon());
            }
            int child = compile_clause(gc, rest.cons_val.car);
            int eps = gc.rb.add("_eps", mk_epsilon());
            int[] children = make_children_2(child, eps);
            char[32] name = "_opt_";
            name[5] = (char)('0' + gc.rb.len % 10);
            name[6] = 0;
            return gc.rb.add((String)name[:6], mk_first(children));
        }

        // (scan "[a-z]+") -> character class scanner
        if (str_eq_c(op_name, "scan")) {
            if (!lisp::is_cons(rest) || !lisp::is_string(rest.cons_val.car)) {
                return gc.rb.add("_fail", mk_fail());
            }
            lisp::Value* pattern_val = rest.cons_val.car;
            char[] pattern = pattern_val.str_val.chars[:pattern_val.str_val.len];
            return make_char_class_scanner(gc, pattern);
        }

        // (not x) -> negative lookahead
        if (str_eq_c(op_name, "not") || str_eq_c(op_name, "!")) {
            if (!lisp::is_cons(rest)) {
                return gc.rb.add("_eps", mk_epsilon());
            }
            int child = compile_clause(gc, rest.cons_val.car);
            char[32] name = "_not_";
            name[5] = (char)('0' + gc.rb.len % 10);
            name[6] = 0;
            return gc.rb.add((String)name[:6], mk_not_followed_by(child));
        }

        // (and x) -> positive lookahead
        if (str_eq_c(op_name, "and") || str_eq_c(op_name, "&")) {
            if (!lisp::is_cons(rest)) {
                return gc.rb.add("_eps", mk_epsilon());
            }
            int child = compile_clause(gc, rest.cons_val.car);
            char[32] name = "_and_";
            name[5] = (char)('0' + gc.rb.len % 10);
            name[6] = 0;
            return gc.rb.add((String)name[:6], mk_followed_by(child));
        }

        // (end) -> end of input
        if (str_eq_c(op_name, "end") || str_eq_c(op_name, "$")) {
            return gc.rb.add("_end", mk_end_of_input());
        }

        io::printfn("Error: unknown grammar operator: %s", op_name);
        return gc.rb.add("_err", mk_fail());
    }

    io::printn("Error: invalid clause type");
    return gc.rb.add("_err", mk_fail());
}

fn bool str_eq_c(char[] a, char[] b) {
    if (a.len != b.len) return false;
    for (usz i = 0; i < a.len; i++) {
        if (a[i] != b[i]) return false;
    }
    return true;
}

// (pika/grammar name rules...) -> grammar object
// Example: (pika/grammar expr
//            (rule number (scan "[0-9]+"))
//            (rule op (first "+" "-" "*" "/"))
//            (rule expr (first number (seq "(" expr op expr ")"))))
fn lisp::Value* prim_pika_grammar(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 2) {
        io::printn("Error: pika/grammar requires name and at least one rule");
        return lisp::make_nil(interp);
    }

    // First arg is grammar name
    if (args[0].tag != lisp::ValueTag.SYMBOL) {
        io::printn("Error: pika/grammar name must be a symbol");
        return lisp::make_nil(interp);
    }

    lisp::SymbolId grammar_name = args[0].sym_val;

    // Initialize compiler
    GrammarCompiler gc;
    gc.init(interp);

    // Process each (rule name clause) definition
    for (usz i = 1; i < args.len; i++) {
        lisp::Value* rule_def = args[i];

        if (!lisp::is_cons(rule_def)) {
            io::printn("Error: rule must be a list");
            continue;
        }

        // Check for (rule name clause) form
        lisp::Value* head = rule_def.cons_val.car;
        if (!lisp::is_symbol(head)) {
            io::printn("Error: rule definition must start with 'rule'");
            continue;
        }

        char[] head_name = interp.symbols.get_name(head.sym_val);
        if (!str_eq_c(head_name, "rule")) {
            io::printfn("Error: expected 'rule', got '%s'", head_name);
            continue;
        }

        lisp::Value* rest = rule_def.cons_val.cdr;
        if (!lisp::is_cons(rest)) {
            io::printn("Error: rule requires name and clause");
            continue;
        }

        lisp::Value* rule_name_val = rest.cons_val.car;
        if (!lisp::is_symbol(rule_name_val)) {
            io::printn("Error: rule name must be a symbol");
            continue;
        }
        lisp::SymbolId rule_name = rule_name_val.sym_val;

        lisp::Value* clause_rest = rest.cons_val.cdr;
        if (!lisp::is_cons(clause_rest)) {
            io::printn("Error: rule requires a clause");
            continue;
        }
        lisp::Value* clause = clause_rest.cons_val.car;

        // Compile the clause
        int clause_idx = compile_clause(&gc, clause);

        // Update rule mapping
        int existing = gc.find_rule(rule_name);
        if (existing >= 0) {
            // Update placeholder with actual clause
            gc.rb.rules[existing].clause = gc.rb.rules[clause_idx].clause;
        } else {
            // Add new rule
            char[] name_str = interp.symbols.get_name(rule_name);
            gc.rb.rules[clause_idx].name = (String)name_str;
            if (gc.rule_count < 64) {
                gc.rule_names[gc.rule_count] = rule_name;
                gc.rule_indices[gc.rule_count] = clause_idx;
                gc.rule_count++;
            }
        }
    }

    // Build the grammar
    if (gc.rb.len == 0) {
        io::printn("Error: grammar has no rules");
        return lisp::make_nil(interp);
    }

    // Use first rule as start
    String[] starts = mem::new_array(String, 1);
    starts[0] = gc.rb.rules[0].name;

    Grammar grammar = make_grammar(starts, gc.rb.to_array());

    // Store grammar
    if (g_grammar_count >= 16) {
        io::printn("Error: too many grammars");
        return lisp::make_nil(interp);
    }

    g_named_grammars[g_grammar_count].name = grammar_name;
    g_named_grammars[g_grammar_count].grammar = grammar;
    g_named_grammars[g_grammar_count].valid = true;
    g_grammar_count++;

    return lisp::make_symbol(interp, grammar_name);
}

// (pika/parse grammar-name input) -> parse result as nested list
fn lisp::Value* prim_pika_parse(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 2) {
        io::printn("Error: pika/parse requires grammar-name and input");
        return lisp::make_nil(interp);
    }

    // Get grammar
    if (args[0].tag != lisp::ValueTag.SYMBOL) {
        io::printn("Error: first argument must be grammar name");
        return lisp::make_nil(interp);
    }

    NamedGrammar* ng = find_grammar(args[0].sym_val);
    if (ng == null) {
        io::printn("Error: grammar not found");
        return lisp::make_nil(interp);
    }

    // Get input string
    if (!lisp::is_string(args[1])) {
        io::printn("Error: second argument must be input string");
        return lisp::make_nil(interp);
    }

    char[] input = args[1].str_val.chars[:args[1].str_val.len];

    // Parse
    ParserState st = parse_simple(&ng.grammar, input);

    // Find top-level match
    int top_match = find_match_at(&st, ng.grammar.names[0], 0);

    if (top_match < 0) {
        return lisp::make_nil(interp);
    }

    // Convert parse tree to Lisp list
    return parse_tree_to_lisp(&st, &ng.grammar, top_match, input, interp);
}

// Convert a parse tree match to a Lisp value
fn lisp::Value* parse_tree_to_lisp(ParserState* st, Grammar* g, int match_idx, char[] input, lisp::Interp* interp) {
    if (match_idx < 0 || match_idx >= st.matches_len) {
        return lisp::make_nil(interp);
    }

    Match* m = &st.matches[match_idx];

    // Get rule name as symbol
    char[] rule_name = g.names[m.clause];
    lisp::SymbolId name_sym = interp.symbols.intern(rule_name);
    lisp::Value* name_val = lisp::make_symbol(interp, name_sym);

    // Get matched text
    char[] matched_text = "";
    if (m.last >= m.first && m.first >= 0 && m.last < (int)input.len) {
        matched_text = input[m.first..m.last];
    }
    lisp::Value* text_val = lisp::make_string(interp, matched_text);

    // Get children
    lisp::Value* children = lisp::make_nil(interp);
    if (m.submatches > 0 && m.submatches < st.submatches_len) {
        // Collect child matches in reverse order for cons
        int[32] child_indices;
        int child_count = 0;
        int sub_idx = m.submatches;
        while (sub_idx > 0 && sub_idx < st.submatches_len && child_count < 32) {
            child_indices[child_count++] = st.submatches[sub_idx];
            sub_idx++;
            if (sub_idx >= st.submatches_len || st.submatches[sub_idx] == 0) break;
        }

        // Build children list
        for (int i = child_count - 1; i >= 0; i--) {
            lisp::Value* child = parse_tree_to_lisp(st, g, child_indices[i], input, interp);
            children = lisp::make_cons(interp, child, children);
        }
    }

    // Build (name text children...)
    lisp::Value* result = lisp::make_cons(interp, children, lisp::make_nil(interp));
    result = lisp::make_cons(interp, text_val, result);
    result = lisp::make_cons(interp, name_val, result);

    return result;
}

// (pika/fold grammar-name input fold-fn) -> folded result
// fold-fn is called with (rule-name matched-text child-results...)
fn lisp::Value* prim_pika_fold(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 3) {
        io::printn("Error: pika/fold requires grammar-name, input, and fold-fn");
        return lisp::make_nil(interp);
    }

    // Get grammar
    if (args[0].tag != lisp::ValueTag.SYMBOL) {
        io::printn("Error: first argument must be grammar name");
        return lisp::make_nil(interp);
    }

    NamedGrammar* ng = find_grammar(args[0].sym_val);
    if (ng == null) {
        io::printn("Error: grammar not found");
        return lisp::make_nil(interp);
    }

    // Get input string
    if (!lisp::is_string(args[1])) {
        io::printn("Error: second argument must be input string");
        return lisp::make_nil(interp);
    }

    char[] input = args[1].str_val.chars[:args[1].str_val.len];

    // Get fold function
    lisp::Value* fold_fn = args[2];

    // Parse
    ParserState st = parse_simple(&ng.grammar, input);

    // Find top-level match
    int top_match = find_match_at(&st, ng.grammar.names[0], 0);

    if (top_match < 0) {
        return lisp::make_nil(interp);
    }

    // Fold over parse tree
    return fold_parse_tree(&st, &ng.grammar, top_match, input, fold_fn, interp);
}

// Fold over parse tree, calling fold_fn for each node
fn lisp::Value* fold_parse_tree(ParserState* st, Grammar* g, int match_idx, char[] input,
                                 lisp::Value* fold_fn, lisp::Interp* interp) {
    if (match_idx < 0 || match_idx >= st.matches_len) {
        return lisp::make_nil(interp);
    }

    Match* m = &st.matches[match_idx];

    // Get rule name as symbol
    char[] rule_name = g.names[m.clause];
    lisp::SymbolId name_sym = interp.symbols.intern(rule_name);
    lisp::Value* name_val = lisp::make_symbol(interp, name_sym);

    // Get matched text
    char[] matched_text = "";
    if (m.last >= m.first && m.first >= 0 && m.last < (int)input.len) {
        matched_text = input[m.first..m.last];
    }
    lisp::Value* text_val = lisp::make_string(interp, matched_text);

    // Recursively fold children
    lisp::Value* child_results = lisp::make_nil(interp);
    if (m.submatches > 0 && m.submatches < st.submatches_len) {
        int[32] child_indices;
        int child_count = 0;
        int sub_idx = m.submatches;
        while (sub_idx > 0 && sub_idx < st.submatches_len && child_count < 32) {
            child_indices[child_count++] = st.submatches[sub_idx];
            sub_idx++;
            if (sub_idx >= st.submatches_len || st.submatches[sub_idx] == 0) break;
        }

        // Fold children and build results list
        for (int i = child_count - 1; i >= 0; i--) {
            lisp::Value* child_result = fold_parse_tree(st, g, child_indices[i], input, fold_fn, interp);
            child_results = lisp::make_cons(interp, child_result, child_results);
        }
    }

    // Call fold_fn with (rule-name text child-results)
    // Build argument list: (fold-fn rule-name text child-results)
    lisp::Value* call_args = lisp::make_cons(interp, child_results, lisp::make_nil(interp));
    call_args = lisp::make_cons(interp, text_val, call_args);
    call_args = lisp::make_cons(interp, name_val, call_args);

    // Apply fold_fn - need to curry since our apply takes one arg at a time
    lisp::Value* r1 = lisp::jit_apply_value(fold_fn, name_val, interp);
    if (r1 != null && r1.tag == lisp::ValueTag.ERROR) return lisp::make_nil(interp);

    lisp::Value* r2 = lisp::jit_apply_value(r1, text_val, interp);
    if (r2 != null && r2.tag == lisp::ValueTag.ERROR) return lisp::make_nil(interp);

    lisp::Value* r3 = lisp::jit_apply_value(r2, child_results, interp);
    if (r3 != null && r3.tag == lisp::ValueTag.ERROR) return lisp::make_nil(interp);

    return r3;
}

// ============================================================
// Additional Regex Primitives
// ============================================================

// (re-match-pos pattern input) -> (start end) or nil
// Returns the start and end positions of the first match
fn lisp::Value* prim_re_match_pos(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 2) {
        return lisp::make_nil(interp);
    }

    lisp::Value* pattern_val = args[0];
    lisp::Value* input_val = args[1];

    if (pattern_val.tag != lisp::ValueTag.STRING || input_val.tag != lisp::ValueTag.STRING) {
        return lisp::make_nil(interp);
    }

    char[] pattern = pattern_val.str_val.chars[:pattern_val.str_val.len];
    char[] input = input_val.str_val.chars[:input_val.str_val.len];

    RegexMatch m = regex_search(pattern, input);

    if (m.matched) {
        // Return (start end) pair
        lisp::Value* start = lisp::make_int(interp, m.start);
        lisp::Value* end = lisp::make_int(interp, m.end);
        lisp::Value* end_list = lisp::make_cons(interp, end, lisp::make_nil(interp));
        return lisp::make_cons(interp, start, end_list);
    }

    return lisp::make_nil(interp);
}

// (re-find-all-pos pattern input) -> ((start1 end1) (start2 end2) ...)
// Returns list of (start end) pairs for all matches
fn lisp::Value* prim_re_find_all_pos(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 2) {
        return lisp::make_nil(interp);
    }

    lisp::Value* pattern_val = args[0];
    lisp::Value* input_val = args[1];

    if (pattern_val.tag != lisp::ValueTag.STRING || input_val.tag != lisp::ValueTag.STRING) {
        return lisp::make_nil(interp);
    }

    char[] pattern = pattern_val.str_val.chars[:pattern_val.str_val.len];
    char[] input = input_val.str_val.chars[:input_val.str_val.len];

    int count = 0;
    RegexMatch[64] matches = regex_find_all(pattern, input, &count);

    // Build list of (start end) pairs
    lisp::Value* result = lisp::make_nil(interp);
    for (int i = count - 1; i >= 0; i--) {
        lisp::Value* start = lisp::make_int(interp, matches[i].start);
        lisp::Value* end = lisp::make_int(interp, matches[i].end);
        lisp::Value* end_list = lisp::make_cons(interp, end, lisp::make_nil(interp));
        lisp::Value* pair = lisp::make_cons(interp, start, end_list);
        result = lisp::make_cons(interp, pair, result);
    }

    return result;
}

// ============================================================
// Lisp Parsing Primitives
// ============================================================

// (pika/parse-lisp input) -> parsed AST as nested list
// Uses the built-in Lisp grammar to parse Lisp code
fn lisp::Value* prim_pika_parse_lisp(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 1) {
        io::printn("Error: pika/parse-lisp requires input string");
        return lisp::make_nil(interp);
    }

    if (!lisp::is_string(args[0])) {
        io::printn("Error: pika/parse-lisp argument must be string");
        return lisp::make_nil(interp);
    }

    char[] input = args[0].str_val.chars[:args[0].str_val.len];

    // Use the global Lisp parser
    ensure_pika_parser();
    ParserState st = pika_parse_lisp(&g_lisp_parser, input);
    int top_match = pika_find_top_match(&g_lisp_parser, &st);

    if (top_match < 0) {
        return lisp::make_nil(interp);
    }

    // Convert parse tree to Lisp value representation
    return parse_tree_to_lisp_value(&st, &g_lisp_parser.grammar, top_match, input, interp);
}

// Helper: convert parse tree to Lisp value (name text children...)
fn lisp::Value* parse_tree_to_lisp_value(
    ParserState* st,
    Grammar* g,
    int match_idx,
    char[] input,
    lisp::Interp* interp
) {
    if (match_idx < 0 || match_idx >= st.matches_len) {
        return lisp::make_nil(interp);
    }

    Match* m = &st.matches[match_idx];

    // Get rule name as symbol
    char[] rule_name = g.names[m.clause];
    lisp::SymbolId name_sym = interp.symbols.intern(rule_name);
    lisp::Value* name_val = lisp::make_symbol(interp, name_sym);

    // Get matched text
    char[] matched_text = "";
    if (m.last >= m.first && m.first >= 0 && m.last < (int)input.len) {
        matched_text = input[m.first..m.last];
    }
    lisp::Value* text_val = lisp::make_string(interp, matched_text);

    // Get children
    int[] subs = get_submatches(st, match_idx);
    lisp::Value* children = lisp::make_nil(interp);

    // Build children list (reverse order for cons)
    for (int i = (int)subs.len - 1; i >= 0; i--) {
        lisp::Value* child = parse_tree_to_lisp_value(st, g, subs[i], input, interp);
        children = lisp::make_cons(interp, child, children);
    }

    // Build (name text children...)
    lisp::Value* result = lisp::make_cons(interp, children, lisp::make_nil(interp));
    result = lisp::make_cons(interp, text_val, result);
    result = lisp::make_cons(interp, name_val, result);

    return result;
}

// (pika/grammar-rules grammar-name) -> list of rule names
fn lisp::Value* prim_pika_grammar_rules(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 1) {
        io::printn("Error: pika/grammar-rules requires grammar name");
        return lisp::make_nil(interp);
    }

    if (args[0].tag != lisp::ValueTag.SYMBOL) {
        io::printn("Error: argument must be grammar name symbol");
        return lisp::make_nil(interp);
    }

    NamedGrammar* ng = find_grammar(args[0].sym_val);
    if (ng == null) {
        io::printn("Error: grammar not found");
        return lisp::make_nil(interp);
    }

    // Build list of rule names
    lisp::Value* result = lisp::make_nil(interp);
    for (int i = (int)ng.grammar.names.len - 1; i >= 0; i--) {
        char[] name = ng.grammar.names[i];
        lisp::SymbolId sym = interp.symbols.intern(name);
        lisp::Value* name_val = lisp::make_symbol(interp, sym);
        result = lisp::make_cons(interp, name_val, result);
    }

    return result;
}

// (pika/match-span grammar-name input rule-name) -> (start end) or nil
// Get the span (start/end positions) of the first match for a rule
fn lisp::Value* prim_pika_match_span(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 3) {
        io::printn("Error: pika/match-span requires grammar-name, input, and rule-name");
        return lisp::make_nil(interp);
    }

    if (args[0].tag != lisp::ValueTag.SYMBOL) {
        io::printn("Error: first argument must be grammar name");
        return lisp::make_nil(interp);
    }

    NamedGrammar* ng = find_grammar(args[0].sym_val);
    if (ng == null) {
        io::printn("Error: grammar not found");
        return lisp::make_nil(interp);
    }

    if (!lisp::is_string(args[1])) {
        io::printn("Error: second argument must be input string");
        return lisp::make_nil(interp);
    }

    if (args[2].tag != lisp::ValueTag.SYMBOL) {
        io::printn("Error: third argument must be rule name symbol");
        return lisp::make_nil(interp);
    }

    char[] input = args[1].str_val.chars[:args[1].str_val.len];
    char[] rule_name = interp.symbols.get_name(args[2].sym_val);

    // Parse
    ParserState st = parse_simple(&ng.grammar, input);

    // Find match for the specified rule at position 0
    int match_idx = find_match_at(&st, (String)rule_name, 0);

    if (match_idx < 0) {
        return lisp::make_nil(interp);
    }

    Match* m = &st.matches[match_idx];
    lisp::Value* start = lisp::make_int(interp, m.first);
    lisp::Value* end = lisp::make_int(interp, m.last);
    lisp::Value* end_list = lisp::make_cons(interp, end, lisp::make_nil(interp));
    return lisp::make_cons(interp, start, end_list);
}

// (pika/eval-lisp input) -> evaluated result
// Parse and evaluate Lisp code (using Pika parser)
fn lisp::Value* prim_pika_eval_lisp(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    // Pika-based eval-lisp is deferred — lisp_semantics.c3 needs AST adaptation.
    // Use (eval (read-string s)) instead.
    return lisp::raise_error(interp, "pika/eval-lisp not yet available — use (eval (read-string s)) instead");
}

// ============================================================
// Registration
// ============================================================

fn void register_pika_primitives(lisp::Interp* interp) {
    // Register regex primitives
    lisp::register_prim(interp, "re-match", &prim_re_match, 2);
    lisp::register_prim(interp, "re-fullmatch", &prim_re_fullmatch, 2);
    lisp::register_prim(interp, "re-find-all", &prim_re_find_all, 2);
    lisp::register_prim(interp, "re-split", &prim_re_split, 2);
    lisp::register_prim(interp, "re-replace", &prim_re_replace, -1);  // 3-4 args
    lisp::register_prim(interp, "re-match-pos", &prim_re_match_pos, 2);
    lisp::register_prim(interp, "re-find-all-pos", &prim_re_find_all_pos, 2);

    // Register grammar primitives
    lisp::register_prim(interp, "pika/grammar", &prim_pika_grammar, -1);  // variadic
    lisp::register_prim(interp, "pika/parse", &prim_pika_parse, 2);
    lisp::register_prim(interp, "pika/fold", &prim_pika_fold, 3);
    lisp::register_prim(interp, "pika/grammar-rules", &prim_pika_grammar_rules, 1);
    lisp::register_prim(interp, "pika/match-span", &prim_pika_match_span, 3);

    // Register Lisp parsing primitives
    lisp::register_prim(interp, "pika/parse-lisp", &prim_pika_parse_lisp, 1);
    lisp::register_prim(interp, "pika/eval-lisp", &prim_pika_eval_lisp, 1);
}

// ============================================================
// Tests
// ============================================================

fn void test_pika_regex(lisp::Interp* interp) {
    io::printn("=== Pika Regex Tests ===");

    // Test 1: Simple literal match
    {
        char[] pattern = "hello";
        char[] input = "say hello world";
        RegexMatch m = regex_search(pattern, input);
        if (m.matched && m.start == 4 && m.end == 8) {
            io::printn("[PASS] re-match literal: 'hello' in 'say hello world'");
        } else {
            io::printfn("[FAIL] re-match literal: matched=%d start=%d end=%d",
                m.matched, m.start, m.end);
        }
    }

    // Test 2: Character class match
    {
        char[] pattern = "[0-9]+";
        char[] input = "abc123def";
        RegexMatch m = regex_search(pattern, input);
        if (m.matched) {
            io::printn("[PASS] re-match char class: '[0-9]+' in 'abc123def'");
        } else {
            io::printn("[FAIL] re-match char class");
        }
    }

    // Test 3: Dot matches any
    {
        char[] pattern = "a.c";
        char[] input = "abc";
        RegexMatch m = regex_search(pattern, input);
        if (m.matched) {
            io::printn("[PASS] re-match dot: 'a.c' matches 'abc'");
        } else {
            io::printn("[FAIL] re-match dot");
        }
    }

    // Test 4: Fullmatch
    {
        char[] pattern = "hello";
        char[] input = "hello";
        RegexMatch m = regex_fullmatch(pattern, input);
        if (m.matched) {
            io::printn("[PASS] re-fullmatch: 'hello' == 'hello'");
        } else {
            io::printn("[FAIL] re-fullmatch");
        }
    }

    // Test 5: Fullmatch fails on partial
    {
        char[] pattern = "hello";
        char[] input = "hello world";
        RegexMatch m = regex_fullmatch(pattern, input);
        if (!m.matched) {
            io::printn("[PASS] re-fullmatch rejects partial: 'hello' != 'hello world'");
        } else {
            io::printn("[FAIL] re-fullmatch should reject partial");
        }
    }

    // Test 6: Find all
    {
        char[] pattern = "a";
        char[] input = "abracadabra";
        int count = 0;
        RegexMatch[64] matches = regex_find_all(pattern, input, &count);
        if (count == 5) {
            io::printn("[PASS] re-find-all: found 5 'a' in 'abracadabra'");
        } else {
            io::printfn("[FAIL] re-find-all: expected 5, got %d", count);
        }
    }

    // Test 7: Escaped characters
    {
        char[] pattern = "\\d";
        char[] input = "abc1def";
        RegexMatch m = regex_search(pattern, input);
        if (m.matched && m.start == 3) {
            io::printn("[PASS] re-match \\d: finds digit");
        } else {
            io::printfn("[FAIL] re-match \\d: matched=%d start=%d", m.matched, m.start);
        }
    }

    // Test 8: Word characters
    {
        char[] pattern = "\\w+";
        char[] input = "hello_world";
        RegexMatch m = regex_search(pattern, input);
        if (m.matched) {
            io::printn("[PASS] re-match \\w+: matches word chars");
        } else {
            io::printn("[FAIL] re-match \\w+");
        }
    }

    io::printn("=== Pika Regex Tests Complete ===");
    io::printn("");
}

fn void test_pika_new_primitives(lisp::Interp* interp) {
    io::printn("=== Pika New Primitives Tests ===");

    // Test re-match-pos
    {
        lisp::Value*[2] args;
        args[0] = lisp::make_string(interp, "hello");
        args[1] = lisp::make_string(interp, "say hello world");
        lisp::Value* result = prim_re_match_pos(args[..], null, interp);

        if (lisp::is_cons(result)) {
            lisp::Value* start = result.cons_val.car;
            lisp::Value* rest = result.cons_val.cdr;
            if (start.tag == lisp::ValueTag.INT && start.int_val == 4) {
                if (lisp::is_cons(rest)) {
                    lisp::Value* end = rest.cons_val.car;
                    if (end.tag == lisp::ValueTag.INT && end.int_val == 8) {
                        io::printn("[PASS] re-match-pos: returns (4 8)");
                    } else {
                        io::printn("[FAIL] re-match-pos: wrong end position");
                    }
                } else {
                    io::printn("[FAIL] re-match-pos: missing end");
                }
            } else {
                io::printn("[FAIL] re-match-pos: wrong start position");
            }
        } else {
            io::printn("[FAIL] re-match-pos: expected cons");
        }
    }

    // Test re-find-all-pos
    {
        lisp::Value*[2] args;
        args[0] = lisp::make_string(interp, "a");
        args[1] = lisp::make_string(interp, "abaca");
        lisp::Value* result = prim_re_find_all_pos(args[..], null, interp);

        int count = 0;
        lisp::Value* curr = result;
        while (lisp::is_cons(curr)) {
            count++;
            curr = curr.cons_val.cdr;
        }

        if (count == 3) {
            io::printn("[PASS] re-find-all-pos: found 3 matches");
        } else {
            io::printfn("[FAIL] re-find-all-pos: expected 3 matches, got %d", count);
        }
    }

    // Test pika/parse-lisp
    {
        lisp::Value*[1] args;
        args[0] = lisp::make_string(interp, "(+ 1 2)");
        lisp::Value* result = prim_pika_parse_lisp(args[..], null, interp);

        if (lisp::is_cons(result)) {
            lisp::Value* name = result.cons_val.car;
            if (name.tag == lisp::ValueTag.SYMBOL) {
                io::printn("[PASS] pika/parse-lisp: returns parsed structure");
            } else {
                io::printn("[FAIL] pika/parse-lisp: first element should be symbol");
            }
        } else {
            io::printn("[FAIL] pika/parse-lisp: expected cons");
        }
    }

    // Test pika/eval-lisp - use simple expression (variadic primitives don't work with curried AST)
    {
        lisp::Value*[1] args;
        args[0] = lisp::make_string(interp, "42");  // Simple literal works
        lisp::Value* result = prim_pika_eval_lisp(args[..], null, interp);

        if (result.tag == lisp::ValueTag.INT && result.int_val == 42) {
            io::printn("[PASS] pika/eval-lisp: 42 evaluates to 42");
        } else if (result.tag == lisp::ValueTag.INT) {
            io::printfn("[FAIL] pika/eval-lisp: expected 42, got %d", (int)result.int_val);
        } else {
            io::printfn("[FAIL] pika/eval-lisp: expected INT, got tag %d", (int)result.tag);
        }
    }

    io::printn("=== Pika New Primitives Tests Complete ===");
    io::printn("");
}
