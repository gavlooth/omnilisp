module lisp;

enum ParserRemovedHeadAliasKind {
    PARSER_REMOVED_HEAD_ALIAS_NONE,
    PARSER_REMOVED_HEAD_ALIAS_FN,
    PARSER_REMOVED_HEAD_ALIAS_DO,
    PARSER_REMOVED_HEAD_ALIAS_LETREC,
    PARSER_REMOVED_HEAD_ALIAS_BEGIN,
    PARSER_REMOVED_HEAD_ALIAS_RESET,
    PARSER_REMOVED_HEAD_ALIAS_SHIFT,
}

fn bool parser_head_policy_text_is(char[] text, char[] expected) @inline {
    if (text.len != expected.len) return false;
    for (usz i = 0; i < expected.len; i++) {
        if (text[i] != expected[i]) return false;
    }
    return true;
}

fn bool parser_head_policy_is_block_form(char[] text, usz start, usz end) {
    return parser_head_policy_text_is(text[start:end - start], "define")
        || parser_head_policy_text_is(text[start:end - start], "lambda")
        || parser_head_policy_text_is(text[start:end - start], "let")
        || parser_head_policy_text_is(text[start:end - start], "if")
        || parser_head_policy_text_is(text[start:end - start], "block")
        || parser_head_policy_text_is(text[start:end - start], "handle")
        || parser_head_policy_text_is(text[start:end - start], "when")
        || parser_head_policy_text_is(text[start:end - start], "unless")
        || parser_head_policy_text_is(text[start:end - start], "raise")
        || parser_head_policy_text_is(text[start:end - start], "match")
        || parser_head_policy_text_is(text[start:end - start], "checkpoint")
        || parser_head_policy_text_is(text[start:end - start], "module")
        || parser_head_policy_text_is(text[start:end - start], "import")
        || parser_head_policy_text_is(text[start:end - start], "syntax-match")
        || parser_head_policy_text_is(text[start:end - start], "template");
}

fn bool parser_head_policy_is_wide_branch_form(char[] text, usz start, usz end) {
    return parser_head_policy_text_is(text[start:end - start], "if");
}

fn bool parser_head_policy_aligns_to_second_token(char[] text, usz start, usz end) {
    return parser_head_policy_text_is(text[start:end - start], "export")
        || parser_head_policy_text_is(text[start:end - start], "export-from");
}

fn bool parser_head_policy_is_higher_order_call(char[] text, usz start, usz end) {
    return parser_head_policy_text_is(text[start:end - start], "map")
        || parser_head_policy_text_is(text[start:end - start], "foldl")
        || parser_head_policy_text_is(text[start:end - start], "foldr")
        || parser_head_policy_text_is(text[start:end - start], "filter")
        || parser_head_policy_text_is(text[start:end - start], "find")
        || parser_head_policy_text_is(text[start:end - start], "partition")
        || parser_head_policy_text_is(text[start:end - start], "sort-by")
        || parser_head_policy_text_is(text[start:end - start], "for-each");
}

fn bool parser_head_policy_is_coroutine_wrapper(char[] text, usz start, usz end) {
    return parser_head_policy_text_is(text[start:end - start], "Coroutine");
}

fn bool parser_head_policy_prefers_binding_context_alignment(char[] text, usz open_index) {
    usz head_start = open_index + 1;
    while (head_start < text.len && (text[head_start] == ' ' || text[head_start] == '\t')) head_start++;
    if (head_start >= text.len) return false;
    if (text[head_start] == ')' || text[head_start] == ']' || text[head_start] == '}' || text[head_start] == ';') return false;

    usz head_end = head_start;
    while (head_end < text.len) {
        char c = text[head_end];
        if (c == ' ' || c == '\t' || c == '(' || c == '[' || c == '{' || c == ')' || c == ']' || c == '}' || c == ';' || c == '"') {
            break;
        }
        head_end++;
    }
    if (head_end <= head_start) return false;

    return parser_head_policy_text_is(text[head_start:head_end - head_start], "lambda")
        || parser_head_policy_text_is(text[head_start:head_end - head_start], "let")
        || parser_head_policy_text_is(text[head_start:head_end - head_start], "block")
        || parser_head_policy_text_is(text[head_start:head_end - head_start], "handle")
        || parser_head_policy_text_is(text[head_start:head_end - head_start], "match")
        || parser_head_policy_text_is(text[head_start:head_end - head_start], "syntax-match")
        || parser_head_policy_text_is(text[head_start:head_end - head_start], "template");
}

fn ParserRemovedHeadAliasKind parser_head_policy_removed_alias_kind(char[] text) {
    if (parser_head_policy_text_is(text, "fn")) return PARSER_REMOVED_HEAD_ALIAS_FN;
    if (parser_head_policy_text_is(text, "do")) return PARSER_REMOVED_HEAD_ALIAS_DO;
    if (parser_head_policy_text_is(text, "letrec")) return PARSER_REMOVED_HEAD_ALIAS_LETREC;
    if (parser_head_policy_text_is(text, "begin")) return PARSER_REMOVED_HEAD_ALIAS_BEGIN;
    if (parser_head_policy_text_is(text, "reset")) return PARSER_REMOVED_HEAD_ALIAS_RESET;
    if (parser_head_policy_text_is(text, "shift")) return PARSER_REMOVED_HEAD_ALIAS_SHIFT;
    return PARSER_REMOVED_HEAD_ALIAS_NONE;
}
