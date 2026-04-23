module lisp::aot;

import std::io;
import lisp;

// =============================================================================
// Output
// =============================================================================

fn void print_value(lisp::Value* v) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) {
        io::print("[aot print_value unavailable]");
        io::print("\n");
        return;
    }
    lisp::print_value(v, &interp.symbols);
    io::print("\n");
}

// =============================================================================
// Dictionary/Array Operations
// =============================================================================

fn lisp::Value* dict_from_args(lisp::Value* arg_list) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return aot_make_error_best_effort(null, "aot dict-from-args: runtime interpreter unavailable");

    lisp::Value* dict = lisp::make_hashmap_checked(interp, 16, "aot dict-from-args: out of memory");
    if (dict == null || dict.tag != lisp::ValueTag.HASHMAP || dict.hashmap_val == null) {
        return dict == null ? aot_make_error_best_effort(interp, "aot dict-from-args: out of memory") : dict;
    }
    lisp::HashMap* map = dict.hashmap_val;
    lisp::Value* curr = arg_list;
    while (curr != null && curr.tag == lisp::ValueTag.CONS) {
        lisp::Value* head = curr.cons_val.car;

        // Pair-list form: ((k . v) ...)
        if (head != null && head.tag == lisp::ValueTag.CONS) {
            lisp::Value* key = head.cons_val.car;
            lisp::Value* val_pair = head.cons_val.cdr;
            lisp::Value* val = (val_pair != null && val_pair.tag == lisp::ValueTag.CONS)
                ? val_pair.cons_val.car
                : lisp::make_nil(interp);
            if (!lisp::hashmap_set_checked(map, key, val, interp)) {
                return aot_make_error_best_effort(interp, "aot dict-from-args: out of memory");
            }
            curr = curr.cons_val.cdr;
            continue;
        }

        // Flat arg form: (k1 v1 k2 v2 ...)
        lisp::Value* key = head;
        curr = curr.cons_val.cdr;
        lisp::Value* val = lisp::make_nil(interp);
        if (curr != null && curr.tag == lisp::ValueTag.CONS) {
            val = curr.cons_val.car;
            curr = curr.cons_val.cdr;
        }
        if (!lisp::hashmap_set_checked(map, key, val, interp)) {
            return aot_make_error_best_effort(interp, "aot dict-from-args: out of memory");
        }
    }
    return dict;
}

fn lisp::Value* index(lisp::Value* collection, lisp::Value* idx) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return aot_make_error_best_effort(null, "aot index: runtime interpreter unavailable");
    lisp::SymbolId ref_sym = (lisp::SymbolId)0;
    if (!aot_intern_checked(interp, "ref", &ref_sym)) return aot_make_error_best_effort(interp, "aot index: invalid or uninternable ref symbol");
    lisp::Value* ref_fn = interp.global_env.lookup(ref_sym);
    if (ref_fn == null) return lisp::raise_error_with_payload_names(interp, "runtime", "runtime/evaluation-error", "ref: primitive not found", null);
    lisp::Value* args = lisp::make_nil(interp);
    args = lisp::make_cons(interp, idx, args);
    args = lisp::make_cons(interp, collection, args);
    return apply_multi(ref_fn, args, 2);
}

// =============================================================================
// Effects (Bridge to JIT)
// =============================================================================

/**
 * AOT handle wrapper.
 */
fn lisp::Value* compiled_handle(lisp::Value** tags, lisp::Value** handlers, usz count, lisp::Value* body, bool strict_mode) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return aot_make_error_best_effort(null, "aot compiled-handle: runtime interpreter unavailable");
    return lisp::jit_handle_value(interp, tags, handlers, count, body, strict_mode);
}
/**
 * AOT signal wrapper.
 */
fn lisp::Value* compiled_signal(lisp::Value* tag, lisp::Value* arg) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return aot_make_error_best_effort(null, "aot compiled-signal: runtime interpreter unavailable");
    return lisp::jit_signal_value(interp, tag, arg);
}

/**
 * AOT reset wrapper.
 */
fn lisp::Value* compiled_reset(lisp::Value* body) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return aot_make_error_best_effort(null, "aot compiled-reset: runtime interpreter unavailable");
    return lisp::jit_reset_value(interp, body);
}

/**
 * AOT shift wrapper.
 */
fn lisp::Value* compiled_shift(lisp::Value* body) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return aot_make_error_best_effort(null, "aot compiled-shift: runtime interpreter unavailable");
    return lisp::jit_shift_value(interp, body);
}

/**
 * AOT resolve wrapper.
 */
fn lisp::Value* compiled_resolve(lisp::Value* k_val, lisp::Value* val) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return aot_make_error_best_effort(null, "aot compiled-resolve: runtime interpreter unavailable");
    return lisp::jit_resolve_value(interp, k_val, val);
}
