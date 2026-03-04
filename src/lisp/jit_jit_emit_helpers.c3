module lisp;

/**
 * Emit a call to fn(interp), result in R0.
 * The function takes one Interp* argument.
 */
fn void emit_call_1(void* s, void* fn_ptr) {
    _jit_prepare(s);
    _jit_pushargr(s, JIT_V0, CODE_PUSHARGR_L);  // arg0 = interp
    _jit_finishi(s, fn_ptr);
    _jit_retval_l(s, JIT_R0);
}

/// Emit fn(reg0, reg1) -> R0
fn void emit_call_2(void* s, void* fn_ptr, int reg0, int reg1) {
    _jit_prepare(s);
    _jit_pushargr(s, reg0, CODE_PUSHARGR_L);
    _jit_pushargr(s, reg1, CODE_PUSHARGR_L);
    _jit_finishi(s, fn_ptr);
    _jit_retval_l(s, JIT_R0);
}

/// Emit fn(reg0, imm1) -> R0
fn void emit_call_2i(void* s, void* fn_ptr, int reg0, long imm1) {
    _jit_prepare(s);
    _jit_pushargr(s, reg0, CODE_PUSHARGR_L);
    _jit_pushargi(s, imm1, CODE_PUSHARGI_L);
    _jit_finishi(s, fn_ptr);
    _jit_retval_l(s, JIT_R0);
}

/// Emit fn(reg0, reg1, reg2) -> R0
fn void emit_call_3(void* s, void* fn_ptr, int reg0, int reg1, int reg2) {
    _jit_prepare(s);
    _jit_pushargr(s, reg0, CODE_PUSHARGR_L);
    _jit_pushargr(s, reg1, CODE_PUSHARGR_L);
    _jit_pushargr(s, reg2, CODE_PUSHARGR_L);
    _jit_finishi(s, fn_ptr);
    _jit_retval_l(s, JIT_R0);
}

/// Emit fn(reg0, imm1, reg2) -> R0
fn void emit_call_3i(void* s, void* fn_ptr, int reg0, long imm1, int reg2) {
    _jit_prepare(s);
    _jit_pushargr(s, reg0, CODE_PUSHARGR_L);
    _jit_pushargi(s, imm1, CODE_PUSHARGI_L);
    _jit_pushargr(s, reg2, CODE_PUSHARGR_L);
    _jit_finishi(s, fn_ptr);
    _jit_retval_l(s, JIT_R0);
}

/// Emit fn(reg0, reg1, imm2, reg3) -> R0
fn void emit_call_4_rrir(void* s, void* fn_ptr, int reg0, int reg1, long imm2, int reg3) {
    _jit_prepare(s);
    _jit_pushargr(s, reg0, CODE_PUSHARGR_L);
    _jit_pushargr(s, reg1, CODE_PUSHARGR_L);
    _jit_pushargi(s, imm2, CODE_PUSHARGI_L);
    _jit_pushargr(s, reg3, CODE_PUSHARGR_L);
    _jit_finishi(s, fn_ptr);
    _jit_retval_l(s, JIT_R0);
}

/// Emit fn(reg0, imm1, reg2, reg3) -> R0
fn void emit_call_4_rirr(void* s, void* fn_ptr, int reg0, long imm1, int reg2, int reg3) {
    _jit_prepare(s);
    _jit_pushargr(s, reg0, CODE_PUSHARGR_L);
    _jit_pushargi(s, imm1, CODE_PUSHARGI_L);
    _jit_pushargr(s, reg2, CODE_PUSHARGR_L);
    _jit_pushargr(s, reg3, CODE_PUSHARGR_L);
    _jit_finishi(s, fn_ptr);
    _jit_retval_l(s, JIT_R0);
}

/// Emit fn(reg0, reg1, reg2, imm3) -> R0
fn void emit_call_4_rrri(void* s, void* fn_ptr, int reg0, int reg1, int reg2, long imm3) {
    _jit_prepare(s);
    _jit_pushargr(s, reg0, CODE_PUSHARGR_L);
    _jit_pushargr(s, reg1, CODE_PUSHARGR_L);
    _jit_pushargr(s, reg2, CODE_PUSHARGR_L);
    _jit_pushargi(s, imm3, CODE_PUSHARGI_L);
    _jit_finishi(s, fn_ptr);
    _jit_retval_l(s, JIT_R0);
}

/**
 * Emit code that builds an Env* from JIT locals into V2 (callee-saved register).
 * Returns true if locals were captured (V2 holds the env), false if no locals.
 * Used by jit_compile_lambda/let_rec/fallback/set to capture let-locals.
 * Clobbers R0/R1 (scratch) and V2; V0/V1 are preserved.
 */
// Emit code to load current env into a register at runtime.
// Uses jit_get_env(interp) which returns jit_env if set, else global_env.
// Result goes to the specified register. Clobbers R0.
fn void emit_load_env(void* s, int target_reg) {
    _jit_prepare(s);
    _jit_pushargr(s, JIT_V0, CODE_PUSHARGR_L);  // arg0 = interp
    _jit_finishi(s, (void*)&jit_get_env);
    _jit_retval_l(s, target_reg);
}

fn bool emit_build_locals_env(void* s, Interp* interp, JitLocals* locals) {
    if (locals.locals.len() == 0) return false;

    // Start with current env in V2 (jit_env if in sub-expression context, else global_env)
    emit_load_env(s, JIT_V2);

    for (usz i = 0; i < locals.locals.len(); i++) {
        if (locals.locals[i].is_mutable) {
            // Mutable local: stack slot holds a shared Env* (the box).
            // Reparent it to the current chain (V2) so it links in properly,
            // then use it as the new V2.
            _jit_new_node_www(s, CODE_LDXI_L, (long)JIT_R1, (long)JIT_FP, (long)locals.locals[i].stack_offset);
            // Call jit_env_reparent(env=R1, new_parent=V2)
            emit_call_2(s, (void*)&jit_env_reparent, JIT_R1, JIT_V2);
            // After reparenting, reload the Env* from stack into V2
            // (R1 may be clobbered by finishi, so reload from stack)
            _jit_new_node_www(s, CODE_LDXI_L, (long)JIT_V2, (long)JIT_FP, (long)locals.locals[i].stack_offset);
        } else {
            // Non-mutable local: extend env with current stack value
            _jit_new_node_www(s, CODE_LDXI_L, (long)JIT_R1, (long)JIT_FP, (long)locals.locals[i].stack_offset);
            // Call jit_env_extend(interp=V0, env=V2, name, value=R1)
            _jit_prepare(s);
            _jit_pushargr(s, JIT_V0, CODE_PUSHARGR_L);   // arg0 = interp
            _jit_pushargr(s, JIT_V2, CODE_PUSHARGR_L);    // arg1 = env
            _jit_pushargi(s, (long)(uint)locals.locals[i].name, CODE_PUSHARGI_L);  // arg2 = name
            _jit_pushargr(s, JIT_R1, CODE_PUSHARGR_L);    // arg3 = value
            _jit_finishi(s, (void*)&jit_env_extend);
            _jit_retval_l(s, JIT_V2);  // V2 = updated env
        }
    }

    return true;
}
