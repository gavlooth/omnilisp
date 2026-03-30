module lisp;

import std::atomic::types;

// =============================================================================
// POSIX SIGNAL HANDLING
// =============================================================================

const int SIGINT_VAL = 2;
alias SignalHandler = fn void(CInt);
extern fn void* signal(int signum, SignalHandler handler) @extern("signal");

// Global interrupt flag — set by SIGINT handler, checked by eval loop
bool g_interrupted = false;

fn void sigint_handler(CInt sig) {
    g_interrupted = true;
}

fn bool interp_interrupt_requested(Interp* interp) @inline {
    if (g_interrupted) return true;
    return interp != null &&
        interp.interrupt_flag != null &&
        interp.interrupt_flag.load() != 0;
}

fn void interp_interrupt_clear(Interp* interp) @inline {
    g_interrupted = false;
    if (interp != null && interp.interrupt_flag != null) {
        interp.interrupt_flag.store(0);
    }
}

fn Value* interp_interrupt_error(Interp* interp) @inline {
    return raise_error(interp, "evaluation interrupted");
}
