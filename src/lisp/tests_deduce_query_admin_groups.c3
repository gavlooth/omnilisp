module lisp;

import std::io;

fn void run_deduce_admin_surface_tests(Interp* interp, int* pass, int* fail) {
    if (deduce_query_filter_try_run_targeted_why_result(interp, pass, fail)) return;
    run_deduce_admin_surface_metadata_and_proof_tests(interp, pass, fail);
    run_deduce_admin_surface_recursive_proof_tests(interp, pass, fail);
    run_deduce_admin_surface_query_demand_tests(interp, pass, fail);
    run_deduce_admin_surface_demand_tests(interp, pass, fail);
    run_deduce_admin_surface_demand_filter_tests(interp, pass, fail);
    run_deduce_admin_surface_demand_tail_tests(interp, pass, fail);
    run_deduce_admin_surface_metadata_and_goal_directed_tests(interp, pass, fail);
    run_deduce_admin_surface_why_result_provenance_tests(interp, pass, fail);
}
