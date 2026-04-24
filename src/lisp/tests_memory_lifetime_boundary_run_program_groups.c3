module lisp;

import std::io;
import main;

fn void run_memory_lifetime_run_program_boundary_state_test(Interp* interp, int* pass, int* fail) {
    main::ScopeRegion* saved_scope = interp.current_scope;
    main::ScopeRegion* saved_releasing = interp.releasing_scope;
    EvalResult r = run_program(
        "(define __boundary_prog_x 10)\n"
        "(define __boundary_prog_y 32)\n"
        "(+ __boundary_prog_x __boundary_prog_y)",
        interp);
    bool ok_eval = !r.error.has_error && r.value != null && r.value.tag == INT && r.value.int_val == 42;
    bool ok_state = interp.current_scope == saved_scope && interp.releasing_scope == saved_releasing;
    if (ok_eval && ok_state) {
        emit_pass_literal("[PASS] lifetime: run_program boundary state restore");
        (*pass)++;
    } else {
        io::printfn("[FAIL] lifetime: run_program boundary state restore (ok_eval=%s ok_state=%s scope_match=%s releasing_match=%s)",
            ok_eval ? "yes" : "no", ok_state ? "yes" : "no",
            interp.current_scope == saved_scope ? "yes" : "no",
            interp.releasing_scope == saved_releasing ? "yes" : "no");
        (*fail)++;
    }
}

fn void run_memory_lifetime_run_program_error_boundary_state_test(Interp* interp, int* pass, int* fail) {
    main::ScopeRegion* saved_scope = interp.current_scope;
    main::ScopeRegion* saved_releasing = interp.releasing_scope;
    EvalResult parse_err = run_program("(define __boundary_prog_err 1\n" "(+ __boundary_prog_err 2)", interp);
    bool parse_ok = parse_err.error.has_error && interp.current_scope == saved_scope && interp.releasing_scope == saved_releasing;
    EvalResult eval_err = run_program("(define __boundary_prog_eval_err 1)\n" "(car 1)", interp);
    bool eval_ok = eval_err.error.has_error && interp.current_scope == saved_scope && interp.releasing_scope == saved_releasing;
    if (parse_ok && eval_ok) {
        emit_pass_literal("[PASS] lifetime: run_program error boundary state restore");
        (*pass)++;
    } else {
        io::printfn("[FAIL] lifetime: run_program error boundary state restore (parse_ok=%s eval_ok=%s)",
            parse_ok ? "yes" : "no", eval_ok ? "yes" : "no");
        (*fail)++;
    }
}
