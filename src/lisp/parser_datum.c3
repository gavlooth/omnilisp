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

fn Value* Parser.parse_datum_template_only(
    Parser* self,
    bool template_mode,
    SymbolId sym
) {
    if (!template_mode) {
        self.set_error("unexpected token in datum");
        return null;
    }
    self.lexer.advance();
    return parser_make_symbol_datum(self.interp, sym);
}

fn Value* Parser.parse_datum_string(Parser* self) {
    Value* v = parser_make_string_datum(self.interp, self.lexer);
    self.lexer.advance();
    return v;
}

fn Value* Parser.parse_datum_quote(Parser* self, bool template_mode) {
    self.lexer.advance();
    Value* inner = self.parse_datum_impl(template_mode);
    if (self.has_error) return null;
    return parser_make_quote_datum(self.interp, inner);
}

fn Value* Parser.parse_datum_collection_template(Parser* self, bool is_dict) {
    Lexer* lex = self.lexer;
    TokenType close_tok = is_dict ? T_RBRACE : T_RBRACKET;
    SymbolId ctor_sym = self.interp.symbols.intern(is_dict ? "dict" : "array");

    // Consume opening token.
    lex.advance();

    Value* head = parser_make_cons_datum(
        self.interp,
        parser_make_symbol_datum(self.interp, ctor_sym),
        null
    );
    Value* tail = head;
    usz elem_count = 0;

    while (lex.current.type != close_tok && !self.has_error) {
        usz before_line = lex.current.line;
        usz before_col = lex.current.column;
        TokenType before_type = lex.current.type;

        Value* elem = self.parse_datum_impl(true);
        if (self.has_error) return null;
        if (lex.current.line == before_line && lex.current.column == before_col && lex.current.type == before_type) {
            self.set_error("template datum parser made no progress");
            return null;
        }

        Value* cell = parser_make_cons_datum(self.interp, elem, null);
        tail.cons_val.cdr = cell;
        tail = cell;
        elem_count++;
    }
    if (self.has_error) return null;
    if (lex.current.type != close_tok) {
        self.set_error(is_dict ? "expected } in dict template literal" : "expected ] in array template literal");
        return null;
    }

    if (is_dict && elem_count % 2 != 0) {
        self.set_error("dict literal requires even number of elements");
        return null;
    }

    tail.cons_val.cdr = parser_make_nil_datum(self.interp);
    lex.advance();  // consume closing token
    return head;
}

fn Value* Parser.parse_datum_list(Parser* self, bool template_mode) {
    Lexer* lex = self.lexer;
    lex.advance();
    if (lex.current.type == T_RPAREN) {
        lex.advance();
        return parser_make_nil_datum(self.interp);
    }

    usz first_line = lex.current.line;
    usz first_col = lex.current.column;
    TokenType first_type = lex.current.type;
    Value* first = self.parse_datum_impl(template_mode);
    if (self.has_error) return null;
    if (lex.current.line == first_line && lex.current.column == first_col && lex.current.type == first_type) {
        self.set_error(template_mode ? "template datum parser made no progress" : "datum parser made no progress");
        return null;
    }
    Value* head = self.interp.alloc_value_root();
    head.tag = CONS;
    head.cons_val.car = first;
    head.cons_val.cdr = null;

    Value* tail = head;
    while (lex.current.type != T_RPAREN && !self.has_error) {
        usz before_line = lex.current.line;
        usz before_col = lex.current.column;
        TokenType before_type = lex.current.type;
        Value* next = self.parse_datum_impl(template_mode);
        if (self.has_error) return null;
        if (lex.current.line == before_line && lex.current.column == before_col && lex.current.type == before_type) {
            self.set_error(template_mode ? "template datum parser made no progress" : "datum parser made no progress");
            return null;
        }
        Value* cell = self.interp.alloc_value_root();
        cell.tag = CONS;
        cell.cons_val.car = next;
        cell.cons_val.cdr = null;
        tail.cons_val.cdr = cell;
        tail = cell;
    }
    if (self.has_error) return null;

    tail.cons_val.cdr = parser_make_nil_datum(self.interp);
    lex.advance();  // consume ')'
    return head;
}

fn Value* Parser.parse_datum_impl(Parser* self, bool template_mode) {
    if (self.has_error) return null;
    Lexer* lex = self.lexer;

    switch (lex.current.type) {
        case T_DOTDOT:
            return self.parse_datum_template_only(template_mode, self.interp.sym_dotdot);
        case T_UNDERSCORE:
            return self.parse_datum_template_only(template_mode, self.interp.symbols.intern("_"));
        case T_INT: {
            long val = lex.current.int_value;
            lex.advance();
            return parser_make_int_datum(self.interp, val);
        }
        case T_SYMBOL: {
            SymbolId sym = self.get_current_symbol();
            lex.advance();
            return parser_make_symbol_datum(self.interp, sym);
        }
        case T_STRING:
            return self.parse_datum_string();
        case T_QUOTE:
            return self.parse_datum_quote(template_mode);
        case T_LPAREN:
            return self.parse_datum_list(template_mode);
        case T_LBRACE:
            if (template_mode) return self.parse_datum_collection_template(true);
            self.set_error("dict literal is not supported in datum");
            return null;
        case T_LBRACKET:
            if (template_mode) return self.parse_datum_collection_template(false);
            self.set_error("array literal is not supported in datum");
            return null;
        default:
            self.set_error(template_mode ? "unexpected token in macro template datum" : "unexpected token in datum");
            return null;
    }
}

/**
 * Parse a datum (for quote).
 */
fn Value* Parser.parse_datum(Parser* self) {
    return self.parse_datum_impl(false);
}

/**
 * Parse a template datum (for macro templates).
 * Like parse_datum() but also handles T_DOTDOT as a ".." symbol,
 * and T_UNDERSCORE as a "_" symbol.
 */
fn Value* Parser.parse_template_datum(Parser* self) {
    return self.parse_datum_impl(true);
}
