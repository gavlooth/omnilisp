module main;

import std::io;
import lisp;

fn bool bind_init_clang_or_report(lisp::LibclangHandle* clang) {
    if (lisp::libclang_init(clang)) return true;

    io::printn("Error: libclang not found.");
    io::printn("");
    io::printn("Install libclang:");
    io::printn("  Arch:   pacman -S clang");
    io::printn("  Debian: apt install libclang-dev");
    io::printn("  Fedora: dnf install clang-devel");
    return false;
}

fn lisp::ParsedFunc* alloc_bind_func_buffer(usz max_funcs) {
    lisp::ParsedFunc* funcs = (lisp::ParsedFunc*)mem::malloc(lisp::ParsedFunc.sizeof * max_funcs);
    if (funcs == null) return null;
    for (usz i = 0; i < max_funcs; i++) {
        funcs[i].params = null;
        funcs[i].param_count = 0;
        funcs[i].param_capacity = 0;
    }
    return funcs;
}
