module lisp;

fn void run_deduce_admin_surface_tail_tests(Interp* interp, int* pass, int* fail) {
    run_deduce_admin_surface_tail_tests_part1(interp, pass, fail);
    run_deduce_admin_surface_tail_tests_part2(interp, pass, fail);
    run_deduce_admin_surface_tail_tests_part3(interp, pass, fail);
}
