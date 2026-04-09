module main;

import std::core::mem;
import lisp;

fn lisp::Interp* create_plain_interp() {
    lisp::Interp* interp = (lisp::Interp*)mem::malloc(lisp::Interp.sizeof);
    if (interp == null) return null;
    interp.init();
    return interp;
}

fn void destroy_plain_interp(lisp::Interp* interp) {
    if (interp == null) return;
    interp.destroy();
    mem::free(interp);
}
