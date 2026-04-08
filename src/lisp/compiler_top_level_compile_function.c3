module lisp;

import std::collections::list;
import main;
// =============================================================================
// SECTION 9: TOP-LEVEL COMPILE FUNCTION
// =============================================================================

fn void scope_dtor_compiler_output(void* ptr) {
    if (ptr != null) mem::free(ptr);
}

fn char[] compiler_own_result_slice(char[] slice, Interp* interp) {
    if (slice.len == 0) return "";
    if (slice.len == usz.max) return "";
    char* owned = (char*)mem::malloc(slice.len + 1);
    if (owned == null) return "";
    mem::copy(owned, slice.ptr, slice.len);
    owned[slice.len] = 0;
    if (interp != null && interp.root_scope != null) {
        main::scope_register_dtor(interp.root_scope, owned, &scope_dtor_compiler_output);
    }
    return owned[:slice.len];
}

/**
 * Compile Omni source code to C3 source code.
 * Returns the generated C3 code as a string.
 */
fn char[] compile_to_c3(char[] source, Interp* interp, char[]* error_out = null) {
    return compile_to_c3_ext(source, interp, false, false, error_out);
}

fn char[] compile_to_c3_ext(char[] source, Interp* interp, bool print_last, bool print_all, char[]* error_out = null) {
    Compiler compiler;
    compiler.init(interp);
    defer compiler.free();
    compiler.print_last = print_last;
    compiler.print_all = print_all;

    char[] generated = compiler.compile_program(source);
    if (generated.len == 0) {
        if (compiler.has_error && compiler.error_message.len > 0) {
            if (error_out != null) {
                *error_out = compiler_own_result_slice(compiler.error_message, interp);
            }
        }
        return "";
    }

    return compiler_own_result_slice(generated, interp);
}
