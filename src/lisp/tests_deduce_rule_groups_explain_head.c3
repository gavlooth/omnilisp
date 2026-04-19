module lisp;

fn void run_deduce_rule_validation_tests_explain_head(Interp* interp, int* pass, int* fail) {
    run_deduce_rule_validation_tests_explain_head_part1(interp, pass, fail);
    run_deduce_rule_validation_tests_explain_head_part2(interp, pass, fail);
    run_deduce_rule_validation_tests_explain_head_part3(interp, pass, fail);
    run_deduce_rule_validation_tests_explain_head_part4(interp, pass, fail);
    run_deduce_rule_validation_tests_explain_head_part5(interp, pass, fail);
}
