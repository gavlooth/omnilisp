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

fn void print_compile_write_error(char[] output_path, EntryFileWriteKind kind) {
    print_file_write_error_text("output file", output_path, kind);
}

fn void print_compile_error_message(char[] msg) {
    io::print("Error: ");
    io::print((String)msg);
    io::printn("");
}

fn int print_compile_read_error(char[] input_path, EntryFileReadKind kind) {
    print_file_read_error_text("input file", input_path, kind);
    return 1;
}
