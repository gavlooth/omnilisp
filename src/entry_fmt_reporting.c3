module main;

import std::io;

const char[] FMT_WRITE_ERROR_CODE = "io/write-failed";
const char[] FMT_READ_ERROR_CODE = "io/read-failed";

fn int print_fmt_usage() {
    io::printn("Usage: omni --fmt [--write|--check] <file>");
    io::printn("  default: print formatted source to stdout");
    io::printn("  --write: rewrite the file in place");
    io::printn("  --check: exit 0 if already formatted, 1 if changes are needed");
    return 1;
}

fn int print_fmt_read_error(char* input_file) {
    io::printfn("Error: cannot read input file %s", (ZString)input_file);
    return 1;
}

fn int print_fmt_write_error(char* output_file) {
    io::printfn("Error: cannot write formatted file %s", (ZString)output_file);
    return 1;
}
