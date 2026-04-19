module lisp;

import main;

// JIT runtime bridge surface: env extension and quasiquote support.
fn Env* jit_env_extend(Interp* interp, Env* env, SymbolId name, Value* value) {
    if (interp == null || interp.current_scope == null || env == null) return null;
    return boundary_env_extend_in_scope(interp, interp.current_scope, env, name, value);
}

// Mutable box allocation in root-scope lifetime — called from JIT-compiled code.
// Creates a root-persistent mutable-box Env node that survives child region cleanup and
// is preserved (not copied) by env_copy. This ensures closures and
// the enclosing JIT scope share the same mutable box.
<* @require interp != null : "jit_env_extend_root: null interp" *>
fn Env* jit_env_extend_root(Interp* interp, Env* env, SymbolId name, Value* value) {
    // Promote initial payload before publishing into persistent root env box.
    Value* promoted = boundary_promote_to_root_site(
        value,
        interp,
        COPY_SITE_JIT_ENV_EXTEND_ROOT
    );
    Env* box = boundary_env_extend_in_root(interp, env, name, promoted);
    return box;
}

// JIT-native quasiquote — no eval() dependency.
