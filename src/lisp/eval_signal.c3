module lisp;

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
