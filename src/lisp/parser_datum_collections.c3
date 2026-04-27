module lisp;

fn Value* Parser.parse_datum_collection_template(Parser* self, bool is_dict) {
    Lexer* lex = self.lexer;
    TokenType close_tok = is_dict ? T_RBRACE : T_RBRACKET;
    SymbolId ctor_sym = self.intern_or_error(is_dict ? "Dictionary" : "Array", "failed to intern template collection constructor");
    if (ctor_sym == INVALID_SYMBOL_ID) return null;

    // Consume opening token.
    lex.advance();

    Value* ctor = parser_make_symbol_datum(self, ctor_sym);
    Value* head = ctor != null ? parser_make_cons_datum(self, ctor, null) : null;
    if (head == null) {
        return null;
    }
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

        Value* cell = parser_make_cons_datum(self, elem, null);
        if (cell == null) {
            return null;
        }
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

    tail.cons_val.cdr = parser_make_nil_datum(self);
    if (tail.cons_val.cdr == null) {
        return null;
    }
    lex.advance();  // consume closing token
    return head;
}

fn Value* Parser.parse_datum_list(Parser* self, bool template_mode) {
    Lexer* lex = self.lexer;
    lex.advance();
    if (lex.current.type == T_RPAREN) {
        lex.advance();
        return parser_make_nil_datum(self);
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
    Value* head = self.alloc_root_value_or_error("out of memory while parsing datum");
    if (head == null) {
        return null;
    }
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
        Value* cell = self.alloc_root_value_or_error("out of memory while parsing datum");
        if (cell == null) {
            return null;
        }
        cell.tag = CONS;
        cell.cons_val.car = next;
        cell.cons_val.cdr = null;
        tail.cons_val.cdr = cell;
        tail = cell;
    }
    if (self.has_error) return null;

    tail.cons_val.cdr = parser_make_nil_datum(self);
    if (tail.cons_val.cdr == null) {
        return null;
    }
    lex.advance();  // consume ')'
    return head;
}
