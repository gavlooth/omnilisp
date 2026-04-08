module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 4a2: MUTABLE CAPTURE DETECTION
// =============================================================================

/**
 * Check if a variable is both captured by a nested lambda AND mutated with set!.
 * This is done by scanning the expression tree for (set! name ...) where name
 * is a free variable of some nested lambda.
 *
 * We detect: is `name` set! inside any sub-expression that also contains
 * a lambda that captures `name`?
 */
fn bool Compiler.is_mutable_capture(Compiler* self, SymbolId name, Expr* body) {
    // Check if name is set! anywhere AND captured by any nested lambda
    bool has_set = self.has_set_on(name, body);
    bool has_capture = self.is_captured_by_nested_lambda(name, body);
    return has_set && has_capture;
}
