module lisp;

import std::atomic::types;

// =============================================================================
// POSIX SIGNAL HANDLING
// =============================================================================

extern fn void omni_sigint_install_handler() @extern("omni_sigint_install_handler");
extern fn void omni_sigint_bind_slot(int* slot) @extern("omni_sigint_bind_slot");
extern fn void omni_sigint_unbind_slot(int* slot) @extern("omni_sigint_unbind_slot");
extern fn int omni_sigint_slot_pending(int* slot) @extern("omni_sigint_slot_pending");
extern fn void omni_sigint_slot_clear(int* slot) @extern("omni_sigint_slot_clear");
extern fn void omni_sigint_debug_mark_pending() @extern("omni_sigint_debug_mark_pending");

fn void interp_sigint_install_handler() {
    omni_sigint_install_handler();
}

fn void interp_sigint_bind(Interp* interp) {
    if (interp == null) return;
    omni_sigint_bind_slot(&interp.sigint_requested);
}

fn void interp_sigint_unbind(Interp* interp) {
    if (interp == null) return;
    omni_sigint_unbind_slot(&interp.sigint_requested);
}

fn void interp_sigint_debug_mark_pending() {
    omni_sigint_debug_mark_pending();
}

fn bool interp_interrupt_requested(Interp* interp) @inline {
    if (interp == null) return false;
    if (omni_sigint_slot_pending(&interp.sigint_requested) != 0) return true;
    return interp.interrupt_flag != null && interp.interrupt_flag.load() != 0;
}

fn void interp_interrupt_clear(Interp* interp) @inline {
    if (interp == null) return;
    omni_sigint_slot_clear(&interp.sigint_requested);
    if (interp.interrupt_flag != null) {
        interp.interrupt_flag.store(0);
    }
}

fn Value* interp_interrupt_error(Interp* interp) @inline {
    return runtime_raise(interp, "runtime/evaluation-error", "evaluation interrupted");
}
