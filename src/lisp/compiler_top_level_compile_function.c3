module lisp;

import std::collections::list;
import main;
// =============================================================================
// SECTION 9: TOP-LEVEL COMPILE FUNCTION
// =============================================================================

int g_compiler_force_result_copy_alloc_fail_nth = 0;
bool g_compiler_force_result_dtor_register_fail = false;

fn void scope_dtor_compiler_output(void* ptr) {
    if (ptr != null) mem::free(ptr);
}

fn bool compiler_should_force_result_copy_alloc_fail() @inline {
    if (g_compiler_force_result_copy_alloc_fail_nth <= 0) return false;
    g_compiler_force_result_copy_alloc_fail_nth--;
    return g_compiler_force_result_copy_alloc_fail_nth == 0;
}

fn bool compiler_own_result_slice(char[] slice, Interp* interp, char[]* out) {
    *out = "";
    if (slice.len == 0) return true;
    if (slice.len == usz.max) return false;
    char* owned = compiler_should_force_result_copy_alloc_fail() ? null : (char*)mem::malloc(slice.len + 1);
    if (owned == null) return false;
    mem::copy(owned, slice.ptr, slice.len);
    owned[slice.len] = 0;
    if (interp != null && interp.root_scope != null) {
        if (g_compiler_force_result_dtor_register_fail ||
            !main::scope_register_dtor(interp.root_scope, owned, &scope_dtor_compiler_output)) {
            mem::free(owned);
            return false;
        }
    }
    *out = owned[:slice.len];
    return true;
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
                char[] owned_error = "";
                if (compiler_own_result_slice(compiler.error_message, interp, &owned_error)) {
                    *error_out = owned_error;
                } else {
                    *error_out = "compiler: out of memory while copying compile error message";
                }
            }
        }
        return "";
    }

    char[] owned_generated = "";
    if (!compiler_own_result_slice(generated, interp, &owned_generated)) {
        if (error_out != null) {
            *error_out = "compiler: out of memory while copying generated C3 output";
        }
        return "";
    }
    return owned_generated;
}
