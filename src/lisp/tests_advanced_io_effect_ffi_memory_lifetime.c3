module lisp;

fn void run_advanced_memory_reclamation_tests(Interp* interp, int* pass, int* fail) {
    // === MEMORY RECLAMATION TESTS ===
    // unsafe-free! on array: releases backing, accessing freed value errors
    test_error(interp, "unsafe-free! array UAF", "(let (a [1 2 3]) (block (unsafe-free! a) a))", pass, fail);
    // unsafe-free! on dict: releases backing, accessing freed value errors
    test_error(interp, "unsafe-free! dict UAF", "(let (d {'a 1 'b 2}) (block (unsafe-free! d) d))", pass, fail);
    // unsafe-free! on instance: releases owner scope, later access is trapped as use-after-free
    setup(interp, "(define [type] UnsafeFreePoint (^Integer x) (^Integer y))");
    test_error(interp, "unsafe-free! instance UAF",
        "(let (p (UnsafeFreePoint 3 4)) (block (unsafe-free! p) p.x))", pass, fail);
    // unsafe-free! on non-collection: no-op, returns Void
    test_tag(interp, "unsafe-free! int noop returns Void", "(unsafe-free! 42)", VOID, pass, fail);
    // unsafe-free! on nil: no-op, returns Void
    test_tag(interp, "unsafe-free! nil noop returns Void", "(unsafe-free! nil)", VOID, pass, fail);
}
