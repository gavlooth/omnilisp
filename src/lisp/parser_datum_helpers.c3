module lisp;

/**
 * Datum construction helpers used by parse_datum/parse_template_datum.
 */
fn Value* parser_make_nil_datum(Parser* self) @inline {
    Value* v = self.alloc_root_value_or_error("out of memory while parsing datum");
    if (v == null) return null;
    v.tag = NIL;
    return v;
}

fn Value* parser_make_int_datum(Parser* self, long val) @inline {
    Value* v = self.alloc_root_value_or_error("out of memory while parsing datum");
    if (v == null) return null;
    v.tag = INT;
    v.int_val = val;
    return v;
}

fn Value* parser_make_symbol_datum(Parser* self, SymbolId sym) @inline {
    Value* v = self.alloc_root_value_or_error("out of memory while parsing datum");
    if (v == null) return null;
    v.tag = SYMBOL;
    v.sym_val = sym;
    return v;
}

fn Value* parser_make_string_datum(Parser* self) {
    Value* v = self.alloc_root_value_or_error("out of memory while parsing datum");
    if (v == null) return null;
    v.tag = STRING;
    StringVal* builder = strval_new_from_slice(self.lexer.current.text[:self.lexer.current.text_len]);
    if (builder == null) {
        self.set_error("out of memory while parsing string literal");
        return null;
    }
    strval_into_value(builder, v);
    return v;
}

fn Value* parser_make_quote_datum(Parser* self, Value* inner) {
    Value* quote_sym = parser_make_symbol_datum(self, self.interp.sym_quote);
    if (quote_sym == null) return null;
    Value* rest = self.alloc_root_value_or_error("out of memory while parsing datum");
    if (rest == null) return null;
    rest.tag = CONS;
    rest.cons_val.car = inner;
    rest.cons_val.cdr = parser_make_nil_datum(self);
    if (rest.cons_val.cdr == null) return null;

    Value* v = self.alloc_root_value_or_error("out of memory while parsing datum");
    if (v == null) return null;
    v.tag = CONS;
    v.cons_val.car = quote_sym;
    v.cons_val.cdr = rest;
    return v;
}

fn Value* parser_make_cons_datum(Parser* self, Value* car_v, Value* cdr_v) @inline {
    Value* v = self.alloc_root_value_or_error("out of memory while parsing datum");
    if (v == null) return null;
    v.tag = CONS;
    v.cons_val.car = car_v;
    v.cons_val.cdr = cdr_v;
    return v;
}
