module main;

import std::io;

fn int print_compile_usage() {
    io::printn("Usage: omni --compile <file> <out.c3>");
    return 1;
}

fn void print_compile_start(char* input_file, char* output_file) {
    io::printfn("Compiling %s to %s...", (ZString)input_file, (ZString)output_file);
}

fn void print_compile_alloc_error() {
    io::printn("Error: failed to allocate compiler interpreter");
}

fn void print_compile_success(char* output_file) {
    io::printfn("Compilation successful: %s", (ZString)output_file);
}

fn void print_compile_write_error(char* output_file) {
    io::printfn("Error: cannot write output file %s", (ZString)output_file);
}

fn int print_compile_read_error(char* input_file) {
    io::printfn("Error: cannot read input file %s", (ZString)input_file);
    return 1;
}
