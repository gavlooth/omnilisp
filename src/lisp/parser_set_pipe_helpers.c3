module lisp;

import std::collections::list;

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
            Expr** new_args = (Expr**)ast_arena_alloc(&self.interp.ast_arena, Expr*.sizeof * new_count);
            for (usz j = 0; j < step.call.arg_count; j++) { new_args[j] = step.call.args[j]; }
            new_args[step.call.arg_count] = acc;
            step.call.args = new_args;
            step.call.arg_count = new_count;
            acc = step;
        } else {
            Expr* call = self.interp.alloc_expr();
            call.tag = E_CALL;
            call.call = (ExprCall*)ast_arena_alloc(&self.interp.ast_arena, ExprCall.sizeof);
            call.loc_line = pipe_loc.loc_line;
            call.loc_column = pipe_loc.loc_column;
            call.call.func = step;
            call.call.arg_count = 1;
            call.call.args = (Expr**)ast_arena_alloc(&self.interp.ast_arena, Expr*.sizeof * 1);
            call.call.args[0] = acc;
            acc = call;
        }
    }
    steps.free();
    return acc;
}
