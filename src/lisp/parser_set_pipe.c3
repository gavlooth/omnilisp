module lisp;

/**
 * Parse a set! expression (variable mutation).
 * (set! name value)
 */
fn Expr* Parser.parse_set(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();
    self.lexer.advance();  // consume 'set!'

    if (self.lexer.current.type == T_PATH) {
        // (set! obj.field value) — path mutation
        e.set_expr.is_path = true;
        e.set_expr.path_segment_count = 0;
        usz text_len = self.lexer.current.text_len;
        usz start = 0;
        for (usz i = 0; i <= text_len; i++) {
            if (i == text_len || self.lexer.current.text[i] == '.') {
                if (i > start) {
                    if (e.set_expr.path_segment_count >= MAX_PATH_SEGMENTS) {
                        self.set_error("too many path segments in set!");
                        return null;
                    }
                    usz seg_len = i - start;
                    SymbolId seg_sym = self.interp.symbols.intern(self.lexer.current.text[start:seg_len]);
                    e.set_expr.path_segments[e.set_expr.path_segment_count] = seg_sym;
                    e.set_expr.path_segment_count++;
                }
                start = i + 1;
            }
        }
        e.set_expr.name = e.set_expr.path_segments[0];
        self.lexer.advance();
    } else if (self.lexer.current.type == T_SYMBOL) {
        e.set_expr.is_path = false;
        e.set_expr.name = self.get_current_symbol();
        self.lexer.advance();
    } else {
        self.set_error("expected variable name or path after set!");
        return null;
    }

    Expr* value = self.parse_expr();
    self.expect(T_RPAREN, ")");

    e.tag = E_SET;
    e.set_expr.value = value;
    return e;
}

/**
 * Parse pipe expression: (|> val step1 step2 ...)
 * Desugars to nested application: (step2 (step1 val))
 * Each step is applied as a function to the accumulated value.
 * Steps containing _ placeholders are already desugared to lambdas by parse_application.
 */
fn Expr* Parser.parse_pipe(Parser* self) {
    if (self.has_error) return null;
    Expr* pipe_loc = self.alloc_expr_here();
    self.lexer.advance();  // consume '|>'

    // Parse initial value
    Expr* acc = self.parse_expr();
    if (acc == null || self.has_error) return null;

    // Parse steps
    Expr*[64] steps;
    usz step_count = 0;
    while (self.lexer.current.type != T_RPAREN && !self.has_error) {
        if (step_count >= 64) { self.set_error("too many pipe steps"); return null; }
        steps[step_count] = self.parse_expr();
        step_count++;
    }
    if (self.has_error) return null;
    self.expect(T_RPAREN, ")");

    if (step_count == 0) return acc;

    // Build nested calls left-to-right: (step1 acc), then (step2 result), etc.
    for (usz i = 0; i < step_count; i++) {
        Expr* step = steps[i];
        if (step.tag == E_CALL) {
            usz new_count = step.call.arg_count + 1;
            Expr** new_args = (Expr**)mem::malloc(Expr*.sizeof * new_count);
            for (usz j = 0; j < step.call.arg_count; j++) { new_args[j] = step.call.args[j]; }
            new_args[step.call.arg_count] = acc;
            mem::free(step.call.args);
            step.call.args = new_args;
            step.call.arg_count = new_count;
            acc = step;
        } else {
            Expr* call = self.interp.alloc_expr();
            call.tag = E_CALL;
            call.call = mem::malloc(ExprCall.sizeof);
            call.loc_line = pipe_loc.loc_line;
            call.loc_column = pipe_loc.loc_column;
            call.call.func = step;
            call.call.arg_count = 1;
            call.call.args = (Expr**)mem::malloc(Expr*.sizeof * 1);
            call.call.args[0] = acc;
            acc = call;
        }
    }
    return acc;
}
