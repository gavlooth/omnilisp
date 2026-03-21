module lisp;

/**
 * Datum construction helpers used by parse_datum/parse_template_datum.
 */
fn Value* parser_make_nil_datum(Interp* interp) @inline {
    Value* v = interp.alloc_value_root();
    v.tag = NIL;
    return v;
}

fn Value* parser_make_int_datum(Interp* interp, long val) @inline {
    Value* v = interp.alloc_value_root();
    v.tag = INT;
    v.int_val = val;
    return v;
}

fn Value* parser_make_symbol_datum(Interp* interp, SymbolId sym) @inline {
    Value* v = interp.alloc_value_root();
    v.tag = SYMBOL;
    v.sym_val = sym;
    return v;
}

fn Value* parser_make_string_datum(Interp* interp, Lexer* lex) {
    Value* v = interp.alloc_value_root();
    v.tag = STRING;
    usz len = lex.current.text_len;
    StringVal* builder = strval_new(len + 1);
    for (usz i = 0; i < len; i++) {
        builder.chars[i] = lex.current.text[i];
    }
    builder.chars[len] = 0;
    builder.len = len;
    strval_into_value(builder, v);
    return v;
}

fn Value* parser_make_quote_datum(Interp* interp, Value* inner) {
    Value* quote_sym = parser_make_symbol_datum(interp, interp.sym_quote);
    Value* rest = interp.alloc_value_root();
    rest.tag = CONS;
    rest.cons_val.car = inner;
    rest.cons_val.cdr = parser_make_nil_datum(interp);

    Value* v = interp.alloc_value_root();
    v.tag = CONS;
    v.cons_val.car = quote_sym;
    v.cons_val.cdr = rest;
    return v;
}

fn Value* parser_make_cons_datum(Interp* interp, Value* car_v, Value* cdr_v) @inline {
    Value* v = interp.alloc_value_root();
    v.tag = CONS;
    v.cons_val.car = car_v;
    v.cons_val.cdr = cdr_v;
    return v;
}
