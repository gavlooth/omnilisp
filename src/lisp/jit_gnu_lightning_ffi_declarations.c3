module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 1: GNU LIGHTNING FFI DECLARATIONS
// =============================================================================
//
// GNU Lightning v2 uses _jit_* prefixed functions internally.
// The C macros (jit_prolog, jit_movi, etc.) are wrappers around these.
// We declare the underlying functions directly for C3 FFI.
//
// Opaque types: jit_state_t* and jit_node_t* are void* in our declarations.
// jit_word_t = long on x86_64, jit_int32_t = int, jit_gpr_t = int.

extern fn void init_jit(void* progname);
extern fn void finish_jit();
extern fn void* jit_new_state();
extern fn void _jit_clear_state(void* state);
extern fn void _jit_destroy_state(void* state);
extern fn void* _jit_emit(void* state);
extern fn void _jit_prolog(void* state);
extern fn void _jit_ret(void* state);
extern fn void _jit_epilog(void* state);
extern fn void* _jit_arg(void* state, int code);
extern fn void _jit_getarg_l(void* state, int reg, void* arg_node);
extern fn void _jit_prepare(void* state);
extern fn void _jit_pushargr(void* state, int reg, int code);
extern fn void _jit_pushargi(void* state, long val, int code);
extern fn void* _jit_finishi(void* state, void* fn_ptr);
extern fn void _jit_retval_l(void* state, int reg);
extern fn void _jit_retr(void* state, int reg, int code);
extern fn void _jit_reti(void* state, long val, int code);
extern fn void* _jit_label(void* state);
extern fn void* _jit_forward(void* state);
extern fn void _jit_patch(void* state, void* node);
extern fn void _jit_patch_at(void* state, void* node, void* label);
extern fn void* _jit_new_node_w(void* state, int code, long u);
extern fn void* _jit_new_node_ww(void* state, int code, long u, long v);
extern fn void* _jit_new_node_www(void* state, int code, long u, long v, long w);
extern fn void* _jit_new_node_p(void* state, int code, void* u);
extern fn void* _jit_new_node_pww(void* state, int code, void* u, long v, long w);
extern fn int _jit_allocai(void* state, int size);

