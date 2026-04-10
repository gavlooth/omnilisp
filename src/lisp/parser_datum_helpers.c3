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

fn Value* parser_make_string_datum(Parser* self) {
    Value* v = self.interp.alloc_value_root();
    v.tag = STRING;
    StringVal* builder = strval_new_from_slice(self.lexer.current.text[:self.lexer.current.text_len]);
    if (builder == null) {
        self.set_error("out of memory while parsing string literal");
        return null;
    }
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
