module lisp;

import std::io;
import main;

fn void run_memory_lifetime_boundary_commit_escape_tests(Interp* interp, int* pass, int* fail) {
    run_memory_lifetime_boundary_commit_escape_primary_tests(interp, pass, fail);
    run_memory_lifetime_boundary_commit_escape_destination_fault_tests(interp, pass, fail);
    run_memory_lifetime_boundary_commit_escape_destination_commit_tests(interp, pass, fail);
    run_memory_lifetime_boundary_commit_escape_rollback_error_tests(interp, pass, fail);
    run_memory_lifetime_boundary_commit_escape_allocation_guard_tests(interp, pass, fail);
}
