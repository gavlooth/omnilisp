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
    if (pipe_loc == null) return null;
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
            if (step.call.arg_count == usz.max) {
                self.set_error("pipe call argument list is too large");
                steps.free();
                return null;
            }
            usz new_count = step.call.arg_count + 1;
            if (new_count > usz.max / Expr*.sizeof) {
                self.set_error("pipe call argument list is too large");
                steps.free();
                return null;
            }
            Expr** new_args = (Expr**)self.alloc_ast_bytes(Expr*.sizeof * new_count, "out of memory while growing pipe call arguments");
            if (new_args == null) { steps.free(); return null; }
            for (usz j = 0; j < step.call.arg_count; j++) { new_args[j] = step.call.args[j]; }
            new_args[step.call.arg_count] = acc;
            step.call.args = new_args;
            step.call.arg_count = new_count;
            acc = step;
        } else {
            Expr* call = self.alloc_expr();
            if (call == null) { steps.free(); return null; }
            call.tag = E_CALL;
            call.call = (ExprCall*)self.alloc_ast_bytes(ExprCall.sizeof, "out of memory while allocating pipe call");
            if (call.call == null) { steps.free(); return null; }
            call.loc_line = pipe_loc.loc_line;
            call.loc_column = pipe_loc.loc_column;
            call.call.func = step;
            call.call.arg_count = 1;
            call.call.args = (Expr**)self.alloc_ast_bytes(Expr*.sizeof, "out of memory while allocating pipe call arguments");
            if (call.call.args == null) { steps.free(); return null; }
            call.call.args[0] = acc;
            acc = call;
        }
    }
    steps.free();
    return acc;
}
