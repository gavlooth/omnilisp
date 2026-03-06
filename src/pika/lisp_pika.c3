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

fn lisp::Value* regex_raise(lisp::Interp* interp, char[] code, char[] message, lisp::Value* data = null) @inline {
    return lisp::raise_error_with_payload_names(interp, "regex", code, message, data);
}

fn lisp::Value* parser_raise(lisp::Interp* interp, char[] code, char[] message, lisp::Value* data = null) @inline {
    return lisp::raise_error_with_payload_names(interp, "parser", code, message, data);
}

// (re-match pattern input) -> matched-string or nil
fn lisp::Value* prim_re_match(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 2) {
        return regex_raise(interp, "regex/re-match-expected-pattern-input", "re-match: expected (re-match pattern input)");
    }

    lisp::Value* pattern_val = args[0];
    lisp::Value* input_val = args[1];

    if (pattern_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-match-pattern-not-string", "re-match: pattern must be a string");
    if (input_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-match-input-not-string", "re-match: input must be a string");

    // C3 slice [a..b] is INCLUSIVE on both ends, [:n] means first n elements
    char[] pattern = pattern_val.str_chars[:pattern_val.str_len];
    char[] input = input_val.str_chars[:input_val.str_len];

    RegexMatch m = regex_search(pattern, input);

    if (m.matched) {
        return lisp::make_string(interp, m.view);
    }

    return lisp::make_nil(interp);
}

// (re-fullmatch pattern input) -> matched-string or nil
fn lisp::Value* prim_re_fullmatch(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 2) {
        return regex_raise(interp, "regex/re-fullmatch-expected-pattern-input", "re-fullmatch: expected (re-fullmatch pattern input)");
    }

    lisp::Value* pattern_val = args[0];
    lisp::Value* input_val = args[1];

    if (pattern_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-fullmatch-pattern-not-string", "re-fullmatch: pattern must be a string");
    if (input_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-fullmatch-input-not-string", "re-fullmatch: input must be a string");

    char[] pattern = pattern_val.str_chars[:pattern_val.str_len];
    char[] input = input_val.str_chars[:input_val.str_len];

    RegexMatch m = regex_fullmatch(pattern, input);

    if (m.matched) {
        return lisp::make_string(interp, m.view);
    }

    return lisp::make_nil(interp);
}

// (re-find-all pattern input) -> list of matched strings
fn lisp::Value* prim_re_find_all(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 2) {
        return regex_raise(interp, "regex/re-find-all-expected-pattern-input", "re-find-all: expected (re-find-all pattern input)");
    }

    lisp::Value* pattern_val = args[0];
    lisp::Value* input_val = args[1];

    if (pattern_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-find-all-pattern-not-string", "re-find-all: pattern must be a string");
    if (input_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-find-all-input-not-string", "re-find-all: input must be a string");

    char[] pattern = pattern_val.str_chars[:pattern_val.str_len];
    char[] input = input_val.str_chars[:input_val.str_len];

    int count = 0;
    RegexMatch[] matches = regex_find_all(pattern, input, &count);
    defer if (matches.len > 0) mem::free(matches.ptr);

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
        return regex_raise(interp, "regex/re-split-expected-pattern-input", "re-split: expected (re-split pattern input)");
    }

    lisp::Value* pattern_val = args[0];
    lisp::Value* input_val = args[1];

    if (pattern_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-split-pattern-not-string", "re-split: pattern must be a string");
    if (input_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-split-input-not-string", "re-split: input must be a string");

    char[] pattern = pattern_val.str_chars[:pattern_val.str_len];
    char[] input = input_val.str_chars[:input_val.str_len];

    // Find all matches
    int count = 0;
    RegexMatch[] matches = regex_find_all(pattern, input, &count);
    defer if (matches.len > 0) mem::free(matches.ptr);

    // Build result list without fixed-size part buffers.
    int right = (int)input.len;
    lisp::Value* result = lisp::make_nil(interp);
    for (int i = count - 1; i >= 0; i--) {
        int part_start = matches[i].end + 1;
        if (part_start < right) {
            char[] part = input[part_start..right - 1];
            result = lisp::make_cons(interp, lisp::make_string(interp, part), result);
        }
        right = matches[i].start;
    }
    if (right > 0) {
        char[] part = input[0..right - 1];
        result = lisp::make_cons(interp, lisp::make_string(interp, part), result);
    }

    return result;
}

// (re-replace pattern replacement input) -> new string
// (re-replace pattern replacement input 'global) -> replace all
fn bool re_replace_has_global_flag(lisp::Value*[] args) @inline {
    return args.len >= 4 && args[3].tag == lisp::ValueTag.SYMBOL;
}

struct ReReplaceBuffer {
    char* chars;
    usz len;
    usz capacity;
}

fn bool re_replace_buffer_init(ReReplaceBuffer* buf, usz initial_len) {
    if (initial_len == usz.max) return false;
    usz capacity = initial_len + 1;
    if (capacity < 32) capacity = 32;

    buf.chars = (char*)mem::malloc(capacity);
    if (buf.chars == null) {
        buf.len = 0;
        buf.capacity = 0;
        return false;
    }

    buf.len = 0;
    buf.capacity = capacity;
    buf.chars[0] = 0;
    return true;
}

fn void re_replace_buffer_free(ReReplaceBuffer* buf) @inline {
    if (buf.chars != null) mem::free(buf.chars);
    buf.chars = null;
    buf.len = 0;
    buf.capacity = 0;
}

fn bool re_replace_buffer_ensure(ReReplaceBuffer* buf, usz needed) {
    if (buf.len == usz.max) return false;
    if (needed >= usz.max - buf.len) return false;

    usz required = buf.len + needed + 1;
    if (required <= buf.capacity) return true;

    usz new_capacity = buf.capacity < 32 ? 32 : buf.capacity;
    while (new_capacity < required) {
        if (new_capacity > usz.max / 2) {
            new_capacity = required;
            break;
        }
        new_capacity *= 2;
    }

    char* next = (char*)mem::malloc(new_capacity);
    if (next == null) return false;
    for (usz i = 0; i < buf.len; i++) next[i] = buf.chars[i];
    next[buf.len] = 0;

    mem::free(buf.chars);
    buf.chars = next;
    buf.capacity = new_capacity;
    return true;
}

fn bool re_replace_append_range(ReReplaceBuffer* result, char[] input, int begin, int end_exclusive) {
    if (begin >= end_exclusive) return true;

    usz count = (usz)(end_exclusive - begin);
    if (!re_replace_buffer_ensure(result, count)) return false;

    for (int j = begin; j < end_exclusive; j++) {
        result.chars[result.len] = input[j];
        result.len++;
    }
    result.chars[result.len] = 0;
    return true;
}

fn bool re_replace_append_text(ReReplaceBuffer* result, char[] text) {
    if (!re_replace_buffer_ensure(result, text.len)) return false;
    for (usz j = 0; j < text.len; j++) {
        result.chars[result.len] = text[j];
        result.len++;
    }
    result.chars[result.len] = 0;
    return true;
}

fn bool re_replace_global(ReReplaceBuffer* result, char[] pattern, char[] replacement, char[] input) {
    int count = 0;
    RegexMatch[] matches = regex_find_all(pattern, input, &count);
    defer if (matches.len > 0) mem::free(matches.ptr);

    int last_end = 0;
    for (int i = 0; i < count; i++) {
        if (!re_replace_append_range(result, input, last_end, matches[i].start)) return false;
        if (!re_replace_append_text(result, replacement)) return false;
        last_end = matches[i].end + 1;
    }
    return re_replace_append_range(result, input, last_end, (int)input.len);
}

fn bool re_replace_first(ReReplaceBuffer* result, char[] pattern, char[] replacement, char[] input) {
    RegexMatch m = regex_search(pattern, input);
    if (m.matched) {
        if (!re_replace_append_range(result, input, 0, m.start)) return false;
        if (!re_replace_append_text(result, replacement)) return false;
        return re_replace_append_range(result, input, m.end + 1, (int)input.len);
    } else {
        return re_replace_append_text(result, input);
    }
}

fn lisp::Value* prim_re_replace(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 3) return regex_raise(interp, "regex/re-replace-expected-pattern-replacement-input", "re-replace: expected (re-replace pattern replacement input ['global])");

    lisp::Value* pattern_val = args[0];
    lisp::Value* replacement_val = args[1];
    lisp::Value* input_val = args[2];
    if (pattern_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-replace-pattern-not-string", "re-replace: pattern must be a string");
    if (replacement_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-replace-replacement-not-string", "re-replace: replacement must be a string");
    if (input_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-replace-input-not-string", "re-replace: input must be a string");

    char[] pattern = pattern_val.str_chars[:pattern_val.str_len];
    char[] replacement = replacement_val.str_chars[:replacement_val.str_len];
    char[] input = input_val.str_chars[:input_val.str_len];

    ReReplaceBuffer result = { .chars = null, .len = 0, .capacity = 0 };
    if (!re_replace_buffer_init(&result, input.len)) {
        return regex_raise(interp, "regex/re-replace-out-of-memory", "re-replace: failed to allocate result buffer");
    }

    bool ok = re_replace_has_global_flag(args)
        ? re_replace_global(&result, pattern, replacement, input)
        : re_replace_first(&result, pattern, replacement, input);
    if (!ok) {
        re_replace_buffer_free(&result);
        return regex_raise(interp, "regex/re-replace-out-of-memory", "re-replace: failed to grow result buffer");
    }

    return lisp::make_string_owned(interp, result.chars, result.len);
}

// ============================================================
// Grammar Definition Primitives
// ============================================================

// Stored grammars by name
struct NamedGrammar {
    lisp::SymbolId name;
    Grammar grammar;
    int start_clause;
    bool valid;
}

NamedGrammar* g_named_grammars = null;
int g_grammar_count = 0;
int g_grammar_capacity = 0;

fn bool ensure_named_grammar_capacity() {
    if (g_grammar_count < g_grammar_capacity) return true;
    int new_capacity = g_grammar_capacity < 16 ? 16 : g_grammar_capacity * 2;
    NamedGrammar* next = (NamedGrammar*)mem::malloc(NamedGrammar.sizeof * new_capacity);
    if (next == null) return false;
    for (int i = 0; i < g_grammar_count; i++) {
        next[i] = g_named_grammars[i];
    }
    if (g_named_grammars != null) mem::free(g_named_grammars);
    g_named_grammars = next;
    g_grammar_capacity = new_capacity;
    return true;
}

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

    if (!ensure_named_grammar_capacity()) {
        return parser_raise(interp, "parser/grammar-oom", "pika/grammar: failed to grow grammar table");
    }

    g_named_grammars[g_grammar_count].name = grammar_name;
    g_named_grammars[g_grammar_count].grammar = grammar;
    g_named_grammars[g_grammar_count].start_clause = start_clause;
    g_named_grammars[g_grammar_count].valid = true;
    g_grammar_count++;
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

// (pika/parse grammar-name input) -> parse result as nested list
fn lisp::Value* prim_pika_parse(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 2) {
        return parser_raise(interp, "parser/parse-expected-grammar-input", "pika/parse: expected grammar-name and input");
    }

    // Get grammar
    if (args[0].tag != lisp::ValueTag.SYMBOL) {
        return parser_raise(interp, "parser/parse-grammar-not-symbol", "pika/parse: first argument must be grammar name");
    }

    NamedGrammar* ng = find_grammar(args[0].sym_val);
    if (ng == null) {
        return parser_raise(interp, "parser/parse-grammar-not-found", "pika/parse: grammar not found");
    }

    // Get input string
    if (!lisp::is_string(args[1])) {
        return parser_raise(interp, "parser/parse-input-not-string", "pika/parse: second argument must be input string");
    }

    char[] input = args[1].str_chars[:args[1].str_len];

    // Parse
    ParserState st = parse_simple(&ng.grammar, input);

    // Find top-level match
    int top_match = lookup_best_match_id(0, ng.start_clause, &st);

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
    UserMatch um = get_user_view(m.clause, match_idx, st);
    if (um.submatches.len > 0) {
        // Build children list in natural order.
        for (int i = (int)um.submatches.len - 1; i >= 0; i--) {
            lisp::Value* child = parse_tree_to_lisp(st, g, um.submatches[i], input, interp);
            children = lisp::make_cons(interp, child, children);
        }
        mem::free(um.submatches.ptr);
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
        return parser_raise(interp, "parser/fold-expected-grammar-input-fn", "pika/fold: expected grammar-name, input, and fold-fn");
    }

    // Get grammar
    if (args[0].tag != lisp::ValueTag.SYMBOL) {
        return parser_raise(interp, "parser/fold-grammar-not-symbol", "pika/fold: first argument must be grammar name");
    }

    NamedGrammar* ng = find_grammar(args[0].sym_val);
    if (ng == null) {
        return parser_raise(interp, "parser/fold-grammar-not-found", "pika/fold: grammar not found");
    }

    // Get input string
    if (!lisp::is_string(args[1])) {
        return parser_raise(interp, "parser/fold-input-not-string", "pika/fold: second argument must be input string");
    }

    char[] input = args[1].str_chars[:args[1].str_len];

    // Get fold function
    lisp::Value* fold_fn = args[2];

    // Parse
    ParserState st = parse_simple(&ng.grammar, input);

    // Find top-level match
    int top_match = lookup_best_match_id(0, ng.start_clause, &st);

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
    UserMatch um = get_user_view(m.clause, match_idx, st);
    if (um.submatches.len > 0) {
        // Fold children and build results list in natural order.
        for (int i = (int)um.submatches.len - 1; i >= 0; i--) {
            lisp::Value* child_result = fold_parse_tree(st, g, um.submatches[i], input, fold_fn, interp);
            child_results = lisp::make_cons(interp, child_result, child_results);
        }
        mem::free(um.submatches.ptr);
    }

    // Call fold_fn with (rule-name text child-results)
    // Build argument list: (fold-fn rule-name text child-results)
    lisp::Value* call_args = lisp::make_cons(interp, child_results, lisp::make_nil(interp));
    call_args = lisp::make_cons(interp, text_val, call_args);
    call_args = lisp::make_cons(interp, name_val, call_args);

    // Apply fold_fn - need to curry since our apply takes one arg at a time
    lisp::Value* r1 = lisp::jit_apply_value(fold_fn, name_val, interp);
    if (r1 != null && r1.tag == lisp::ValueTag.ERROR) return r1;

    lisp::Value* r2 = lisp::jit_apply_value(r1, text_val, interp);
    if (r2 != null && r2.tag == lisp::ValueTag.ERROR) return r2;

    lisp::Value* r3 = lisp::jit_apply_value(r2, child_results, interp);
    if (r3 != null && r3.tag == lisp::ValueTag.ERROR) return r3;

    return r3;
}

// ============================================================
// Additional Regex Primitives
// ============================================================

// (re-match-pos pattern input) -> (start end) or nil
// Returns the start and end positions of the first match
fn lisp::Value* prim_re_match_pos(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 2) {
        return regex_raise(interp, "regex/re-match-pos-expected-pattern-input", "re-match-pos: expected (re-match-pos pattern input)");
    }

    lisp::Value* pattern_val = args[0];
    lisp::Value* input_val = args[1];

    if (pattern_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-match-pos-pattern-not-string", "re-match-pos: pattern must be a string");
    if (input_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-match-pos-input-not-string", "re-match-pos: input must be a string");

    char[] pattern = pattern_val.str_chars[:pattern_val.str_len];
    char[] input = input_val.str_chars[:input_val.str_len];

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
        return regex_raise(interp, "regex/re-find-all-pos-expected-pattern-input", "re-find-all-pos: expected (re-find-all-pos pattern input)");
    }

    lisp::Value* pattern_val = args[0];
    lisp::Value* input_val = args[1];

    if (pattern_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-find-all-pos-pattern-not-string", "re-find-all-pos: pattern must be a string");
    if (input_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-find-all-pos-input-not-string", "re-find-all-pos: input must be a string");

    char[] pattern = pattern_val.str_chars[:pattern_val.str_len];
    char[] input = input_val.str_chars[:input_val.str_len];

    int count = 0;
    RegexMatch[] matches = regex_find_all(pattern, input, &count);
    defer if (matches.len > 0) mem::free(matches.ptr);

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
        return parser_raise(interp, "parser/parse-lisp-expected-input", "pika/parse-lisp: expected input string");
    }

    if (!lisp::is_string(args[0])) {
        return parser_raise(interp, "parser/parse-lisp-input-not-string", "pika/parse-lisp: argument must be string");
    }

    char[] input = args[0].str_chars[:args[0].str_len];

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
        return parser_raise(interp, "parser/grammar-rules-expected-name", "pika/grammar-rules: expected grammar name");
    }

    if (args[0].tag != lisp::ValueTag.SYMBOL) {
        return parser_raise(interp, "parser/grammar-rules-name-not-symbol", "pika/grammar-rules: argument must be grammar name symbol");
    }

    NamedGrammar* ng = find_grammar(args[0].sym_val);
    if (ng == null) {
        return parser_raise(interp, "parser/grammar-rules-grammar-not-found", "pika/grammar-rules: grammar not found");
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
        return parser_raise(interp, "parser/match-span-expected-grammar-input-rule", "pika/match-span: expected grammar-name, input, and rule-name");
    }

    if (args[0].tag != lisp::ValueTag.SYMBOL) {
        return parser_raise(interp, "parser/match-span-grammar-not-symbol", "pika/match-span: first argument must be grammar name");
    }

    NamedGrammar* ng = find_grammar(args[0].sym_val);
    if (ng == null) {
        return parser_raise(interp, "parser/match-span-grammar-not-found", "pika/match-span: grammar not found");
    }

    if (!lisp::is_string(args[1])) {
        return parser_raise(interp, "parser/match-span-input-not-string", "pika/match-span: second argument must be input string");
    }

    if (args[2].tag != lisp::ValueTag.SYMBOL) {
        return parser_raise(interp, "parser/match-span-rule-not-symbol", "pika/match-span: third argument must be rule name symbol");
    }

    char[] input = args[1].str_chars[:args[1].str_len];
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
    return parser_raise(interp, "parser/eval-lisp-unavailable", "pika/eval-lisp not yet available — use (eval (read-string s)) instead");
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

fn void regex_test_literal() {
    char[] pattern = "hello";
    char[] input = "say hello world";
    RegexMatch m = regex_search(pattern, input);
    if (m.matched && m.start == 4 && m.end == 8) {
        io::printn("[PASS] re-match literal: 'hello' in 'say hello world'");
    } else {
        io::printfn("[FAIL] re-match literal: matched=%d start=%d end=%d", m.matched, m.start, m.end);
    }
}

fn void regex_test_char_class() {
    char[] pattern = "[0-9]+";
    char[] input = "abc123def";
    RegexMatch m = regex_search(pattern, input);
    if (m.matched) {
        io::printn("[PASS] re-match char class: '[0-9]+' in 'abc123def'");
    } else {
        io::printn("[FAIL] re-match char class");
    }
}

fn void regex_test_dot() {
    char[] pattern = "a.c";
    char[] input = "abc";
    RegexMatch m = regex_search(pattern, input);
    if (m.matched) {
        io::printn("[PASS] re-match dot: 'a.c' matches 'abc'");
    } else {
        io::printn("[FAIL] re-match dot");
    }
}

fn void regex_test_fullmatch() {
    char[] pattern = "hello";
    char[] input = "hello";
    RegexMatch m = regex_fullmatch(pattern, input);
    if (m.matched) {
        io::printn("[PASS] re-fullmatch: 'hello' == 'hello'");
    } else {
        io::printn("[FAIL] re-fullmatch");
    }
}

fn void regex_test_fullmatch_partial() {
    char[] pattern = "hello";
    char[] input = "hello world";
    RegexMatch m = regex_fullmatch(pattern, input);
    if (!m.matched) {
        io::printn("[PASS] re-fullmatch rejects partial: 'hello' != 'hello world'");
    } else {
        io::printn("[FAIL] re-fullmatch should reject partial");
    }
}

fn void regex_test_find_all() {
    char[] pattern = "a";
    char[] input = "abracadabra";
    int count = 0;
    RegexMatch[] matches = regex_find_all(pattern, input, &count);
    defer if (matches.len > 0) mem::free(matches.ptr);
    if (count == 5) {
        io::printn("[PASS] re-find-all: found 5 'a' in 'abracadabra'");
    } else {
        io::printfn("[FAIL] re-find-all: expected 5, got %d", count);
    }
}

fn void regex_test_digit_escape() {
    char[] pattern = "\\d";
    char[] input = "abc1def";
    RegexMatch m = regex_search(pattern, input);
    if (m.matched && m.start == 3) {
        io::printn("[PASS] re-match \\d: finds digit");
    } else {
        io::printfn("[FAIL] re-match \\d: matched=%d start=%d", m.matched, m.start);
    }
}

fn void regex_test_word_escape() {
    char[] pattern = "\\w+";
    char[] input = "hello_world";
    RegexMatch m = regex_search(pattern, input);
    if (m.matched) {
        io::printn("[PASS] re-match \\w+: matches word chars");
    } else {
        io::printn("[FAIL] re-match \\w+");
    }
}

// ============================================================
// Tests
// ============================================================

fn void test_pika_regex(lisp::Interp* interp) {
    io::printn("=== Pika Regex Tests ===");
    regex_test_literal();
    regex_test_char_class();
    regex_test_dot();
    regex_test_fullmatch();
    regex_test_fullmatch_partial();
    regex_test_find_all();
    regex_test_digit_escape();
    regex_test_word_escape();
    io::printn("=== Pika Regex Tests Complete ===");
    io::printn("");
}

fn void test_pika_primitive_re_match_pos(lisp::Interp* interp) {
    lisp::Value*[2] args;
    args[0] = lisp::make_string(interp, "hello");
    args[1] = lisp::make_string(interp, "say hello world");
    lisp::Value* result = prim_re_match_pos(args[..], null, interp);

    if (!lisp::is_cons(result)) {
        io::printn("[FAIL] re-match-pos: expected cons");
        return;
    }

    lisp::Value* start = result.cons_val.car;
    lisp::Value* rest = result.cons_val.cdr;
    if (start.tag != lisp::ValueTag.INT || start.int_val != 4) {
        io::printn("[FAIL] re-match-pos: wrong start position");
        return;
    }
    if (!lisp::is_cons(rest)) {
        io::printn("[FAIL] re-match-pos: missing end");
        return;
    }

    lisp::Value* end = rest.cons_val.car;
    if (end.tag == lisp::ValueTag.INT && end.int_val == 8) {
        io::printn("[PASS] re-match-pos: returns (4 8)");
    } else {
        io::printn("[FAIL] re-match-pos: wrong end position");
    }
}

fn void test_pika_primitive_re_find_all_pos(lisp::Interp* interp) {
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

fn void test_pika_primitive_parse_lisp(lisp::Interp* interp) {
    lisp::Value*[1] args;
    args[0] = lisp::make_string(interp, "(+ 1 2)");
    lisp::Value* result = prim_pika_parse_lisp(args[..], null, interp);

    if (!lisp::is_cons(result)) {
        io::printn("[FAIL] pika/parse-lisp: expected cons");
        return;
    }

    lisp::Value* name = result.cons_val.car;
    if (name.tag == lisp::ValueTag.SYMBOL) {
        io::printn("[PASS] pika/parse-lisp: returns parsed structure");
    } else {
        io::printn("[FAIL] pika/parse-lisp: first element should be symbol");
    }
}

fn void test_pika_primitive_eval_lisp(lisp::Interp* interp) {
    lisp::Value*[1] args;
    // Simple literal works (variadic primitives do not with curried AST yet).
    args[0] = lisp::make_string(interp, "42");
    lisp::Value* result = prim_pika_eval_lisp(args[..], null, interp);

    if (result.tag == lisp::ValueTag.INT && result.int_val == 42) {
        io::printn("[PASS] pika/eval-lisp: 42 evaluates to 42");
    } else if (result.tag == lisp::ValueTag.INT) {
        io::printfn("[FAIL] pika/eval-lisp: expected 42, got %d", (int)result.int_val);
    } else {
        io::printfn("[FAIL] pika/eval-lisp: expected INT, got tag %d", (int)result.tag);
    }
}

fn void test_pika_new_primitives(lisp::Interp* interp) {
    io::printn("=== Pika New Primitives Tests ===");
    test_pika_primitive_re_match_pos(interp);
    test_pika_primitive_re_find_all_pos(interp);
    test_pika_primitive_parse_lisp(interp);
    test_pika_primitive_eval_lisp(interp);
    io::printn("=== Pika New Primitives Tests Complete ===");
    io::printn("");
}
