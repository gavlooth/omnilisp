module lisp;

import std::io;

fn bool bindgen_write_text_file(char[] output_path, char[] text) {
    if (try file = io::file::open((String)output_path, "w")) {
        char[] remaining = text;
        bool write_ok = true;
        while (write_ok && remaining.len > 0) {
            usz? written_opt = file.write(remaining);
            if (catch err = written_opt) {
                write_ok = false;
                break;
            }
            if (written_opt == 0) {
                write_ok = false;
                break;
            }
            remaining = remaining[written_opt..];
        }
        if (catch err = file.close()) write_ok = false;
        return write_ok;
    }
    return false;
}

fn bool bindgen_output_file_exists(char[] output_path) {
    if (try file = io::file::open((String)output_path, "r")) {
        (void)file.close();
        return true;
    }
    return false;
}
