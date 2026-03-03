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

/**
 * Compile Lisp source code to C3 source code with print_last mode.
 * The last non-define expression is wrapped with rt_print_value + printn
 * so the compiled binary prints its result.
 */
fn char[] compile_to_c3_with_print(char[] source, Interp* interp) {
    Compiler compiler;
    compiler.init(interp);
    compiler.print_last = true;

    char[] result = compiler.compile_program(source);

    // Note: caller must copy result before compiler is freed
    return result;
}

/**
 * Compile Lisp source code to C3 source code with print_all mode.
 * Every non-define expression is wrapped with rt_print_value + printn
 * so the compiled binary prints all results (for e2e testing).
 */
fn char[] compile_to_c3_print_all(char[] source, Interp* interp) {
    Compiler compiler;
    compiler.init(interp);
    compiler.print_all = true;

    char[] result = compiler.compile_program(source);

    // Note: caller must copy result before compiler is freed
    return result;
}
