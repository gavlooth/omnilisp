module lisp;

import std::collections::list;

/**
 * Parse set! forms:
 * - (set! name value) / (set! obj.field value): mutation special form
 * - (set! collection key value ...): desugared to regular call
 */
fn Expr* Parser.build_set_call_symbol(Parser* self, usz line, usz col) {
    Expr* fn_expr = self.alloc_expr_here();
    fn_expr.tag = E_VAR;
    fn_expr.loc_line = line;
    fn_expr.loc_column = col;
    fn_expr.var_expr.name = self.interp.sym_set;
    return fn_expr;
}

fn Expr* Parser.parse_generic_set_call(Parser* self, Expr* lhs, Expr* rhs, usz line, usz col) {
    List{Expr*} args;
    args.push(lhs);
    args.push(rhs);
    while (self.lexer.current.type != T_RPAREN && !self.has_error) {
        args.push(self.parse_expr());
    }
    if (self.has_error) {
        args.free();
        return null;
    }
    self.expect(T_RPAREN, ")");

    Expr* set_fn = self.build_set_call_symbol(line, col);
    Expr* call_expr = self.build_call_expr(set_fn, &args, line, col);
    args.free();
    return self.parse_postfix_index(call_expr);
}

fn Expr* Parser.parse_set(Parser* self) {
    if (self.has_error) return null;
    usz set_line = self.lexer.current.line;
    usz set_col = self.lexer.current.column;
    self.lexer.advance();  // consume 'set!'

    Expr* lhs = self.parse_expr();
    if (lhs == null || self.has_error) return null;
    Expr* rhs = self.parse_expr();
    if (rhs == null || self.has_error) return null;

    // Generic call surface: (set! collection key value ...)
    if (self.lexer.current.type != T_RPAREN) {
        return self.parse_generic_set_call(lhs, rhs, set_line, set_col);
    }

    // Mutation special form surface: (set! name value) or (set! obj.field value)
    Expr* e = self.alloc_expr_here();
    e.loc_line = set_line;
    e.loc_column = set_col;
    e.tag = E_SET;
    e.set_expr.value = rhs;
    e.set_expr.path_segments = null;
    e.set_expr.path_segment_count = 0;
    e.set_expr.is_path = false;

    if (lhs.tag == E_VAR) {
        e.set_expr.name = lhs.var_expr.name;
    } else if (lhs.tag == E_PATH) {
        if (lhs.path.segment_count == 0 || lhs.path.segments == null) {
            self.set_error("empty path in set!");
            return null;
        }
        e.set_expr.is_path = true;
        e.set_expr.path_segment_count = lhs.path.segment_count;
        e.set_expr.path_segments = lhs.path.segments;
        e.set_expr.name = lhs.path.segments[0];
    } else {
        self.set_error("expected variable name or path after set!");
        return null;
    }

    self.expect(T_RPAREN, ")");
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
    List{Expr*} steps;
    while (self.lexer.current.type != T_RPAREN && !self.has_error) {
        steps.push(self.parse_expr());
    }
    if (self.has_error) {
        steps.free();
        return null;
    }
    self.expect(T_RPAREN, ")");

    if (steps.len() == 0) {
        steps.free();
        return acc;
    }

    // Build nested calls left-to-right: (step1 acc), then (step2 result), etc.
    for (usz i = 0; i < steps.len(); i++) {
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
    steps.free();
    return acc;
}
