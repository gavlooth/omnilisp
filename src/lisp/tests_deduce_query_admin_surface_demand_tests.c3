module lisp;

import std::io;

fn bool deduce_query_test_eval_true(Interp* interp, EvalResult r) @inline {
    return !r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true;
}

fn void deduce_query_test_expect_true(
    Interp* interp,
    EvalResult r,
    char[] pass_msg,
    char[] fail_msg,
    int* pass,
    int* fail
) {
    if (deduce_query_test_eval_true(interp, r)) {
        emit_pass_literal(pass_msg);
        (*pass)++;
        return;
    }
    if (r.error.has_error) {
        io::printfn("[FAIL] %s (error: %s)", (ZString)fail_msg, (ZString)r.error.message[:256]);
    } else {
        io::printn((ZString)fail_msg);
    }
    (*fail)++;
}

fn void run_deduce_admin_surface_demand_tests(Interp* interp, int* pass, int* fail) {
    run_deduce_admin_surface_demand_core_tests(interp, pass, fail);
    run_deduce_admin_surface_demand_branch_tests(interp, pass, fail);
    run_deduce_admin_surface_demand_recursive_tests(interp, pass, fail);
    run_deduce_admin_surface_tail_tests(interp, pass, fail);
}
