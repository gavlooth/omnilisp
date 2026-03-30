module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 9: TOP-LEVEL COMPILE FUNCTION
// =============================================================================

fn void scope_dtor_compiler_output(void* ptr) {
    if (ptr != null) mem::free(ptr);
}

/**
 * Compile Omni source code to C3 source code.
 * Returns the generated C3 code as a string.
 */
fn char[] compile_to_c3(char[] source, Interp* interp) {
    return compile_to_c3_ext(source, interp, false, false);
}

fn char[] compile_to_c3_ext(char[] source, Interp* interp, bool print_last, bool print_all) {
    Compiler compiler;
    compiler.init(interp);
    defer compiler.free();
    compiler.print_last = print_last;
    compiler.print_all = print_all;

    char[] generated = compiler.compile_program(source);
    if (generated.len == 0) return "";

    char* owned = (char*)mem::malloc(generated.len);
    if (owned == null) return "";
    mem::copy(owned, generated.ptr, generated.len);
    if (interp != null && interp.root_scope != null) {
        main::scope_register_dtor(interp.root_scope, owned, &scope_dtor_compiler_output);
    }
    return owned[:generated.len];
}
