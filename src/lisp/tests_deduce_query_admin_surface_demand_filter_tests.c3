module lisp;

fn void run_deduce_admin_surface_demand_filter_tests(Interp* interp, int* pass, int* fail) {
    run_deduce_admin_surface_demand_filter_part1_tests(interp, pass, fail);
    run_deduce_admin_surface_demand_filter_part2_tests(interp, pass, fail);
    run_deduce_admin_surface_demand_filter_part3_tests(interp, pass, fail);
}
