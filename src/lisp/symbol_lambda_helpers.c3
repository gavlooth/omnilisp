module lisp;

fn bool symbol_text_is_lambda_spelling(char[] text) @inline {
    if (text.len == 6 &&
        text[0] == 'l' &&
        text[1] == 'a' &&
        text[2] == 'm' &&
        text[3] == 'b' &&
        text[4] == 'd' &&
        text[5] == 'a') {
        return true;
    }
    return text.len == 2 &&
        text[0] == (char)0xCE &&
        text[1] == (char)0xBB;
}

fn bool symbol_is_lambda_spelling(SymbolTable* symbols, SymbolId sym, SymbolId canonical_lambda) {
    if ((uint)sym == (uint)canonical_lambda) return true;
    if (symbols == null || sym == INVALID_SYMBOL_ID) return false;
    return symbol_text_is_lambda_spelling(symbols.get_name(sym));
}
