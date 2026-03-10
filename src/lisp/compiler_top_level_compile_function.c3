module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 9: TOP-LEVEL COMPILE FUNCTION
// =============================================================================

/**
 * Compile Lisp source code to C3 source code.
 * Returns the generated C3 code as a string.
 */
fn char[] compile_to_c3(char[] source, Interp* interp) {
    return compile_to_c3_ext(source, interp, false, false);
}

fn char[] compile_to_c3_ext(char[] source, Interp* interp, bool print_last, bool print_all) {
    Compiler compiler;
    compiler.init(interp);
    compiler.print_last = print_last;
    compiler.print_all = print_all;
    return compiler.compile_program(source);
}
