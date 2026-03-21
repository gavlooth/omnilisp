module main;

import std::io;

fn int print_help() {
    io::printn("omni 0.1.5 — A Lisp with modern semantics");
    io::printn("");
    io::printn("Usage: omni [options] [script.omni]");
    io::printn("");
    io::printn("Running:");
    io::printn("  omni                              Start the REPL");
    io::printn("  omni <script.omni>                Run a script file");
    io::printn("  omni --repl                       Start the REPL (explicit)");
    io::printn("  omni --repl --json                Start structured JSON REPL transport");
    io::printn("  omni --eval '<expr>' [--json]     Evaluate one Omni expression");
    io::printn("  omni --test-suite <all|stack|scope|lisp>  Run one test suite (default: all)");
    io::printn("");
    io::printn("Building:");
    io::printn("  omni --check <file> [--json]      Parse/check Omni source without executing it");
    io::printn("  omni --build <file> [-o output]   AOT compile to standalone binary");
    io::printn("  omni --compile <file> <out.c3>    Compile Omni source to C3");
    io::printn("");
    io::printn("Project management:");
    io::printn("  omni --init <name>                Scaffold a new Omni project");
    io::printn("  omni --bind [project-dir]         Generate FFI bindings from omni.toml");
    io::printn("");
    io::printn("Other:");
    io::printn("  omni --language-ref, --manual    Print built-in full language reference");
    io::printn("  omni --gen-e2e                    Generate end-to-end compiler tests");
    io::printn("  omni --stack-affinity-probe       Deliberately trigger stack ownership fail-fast probe");
    io::printn("  omni --version, -v                Print version");
    io::printn("  omni --help, -h                   Print this help");
    io::printn("");
    io::printn("Exit codes:");
    io::printn("  0  Success / clean input");
    io::printn("  1  Usage error or command failure");
    io::printn("  2  Invalid test-suite name");
    return 0;
}

fn int print_version() {
    io::printn("omni 0.1.5");
    return 0;
}
