module main;

import std::io;

fn bool write_init_toml(char* name, usz name_len) {
    char[512] path_buf;
    usz plen = 0;
    if (!build_project_path(path_buf[..], &plen, name, name_len, "omni.toml")) {
        io::printn("Error: project path too long for omni.toml");
        return false;
    }

    char[2048] text_buf;
    char[] text;
    if (try rendered = io::bprintf(&text_buf,
            "[project]\nname = \"%s\"\nversion = \"0.2.0\"\n\n"
            "[build]\n"
            "output-dir = \"build\"\n"
            "safety = \"safe\"        # \"safe\", \"fast\", or \"none\"\n"
            "opt = \"O0\"             # \"O0\", \"O1\", \"O2\", \"O3\", \"Os\", \"Oz\"\n"
            "debug-info = \"full\"    # \"full\", \"line-tables\", \"none\"\n"
            "# sanitize = \"none\"    # \"none\", \"address\", \"memory\", \"thread\"\n"
            "# single-module = false\n\n"
            "# FFI dependencies — uncomment and edit:\n"
            "# [dependencies.ffi.math]\n"
            "# library = \"m\"\n"
            "# headers = [\"/usr/include/math.h\"]\n"
            "# functions = [\"sin\", \"cos\", \"sqrt\"]\n",
            (ZString)name)) {
        text = rendered;
    } else {
        io::printfn("Error: failed to compose omni.toml for project '%s'", (ZString)name);
        return false;
    }

    if (!write_project_text_file(path_buf[:plen], text)) {
        io::printfn("Error: cannot write %s", (ZString)&path_buf[0]);
        return false;
    }
    return true;
}

fn bool write_init_main_omni(char* name, usz name_len) {
    char[512] path_buf;
    usz plen = 0;
    if (!build_project_path(path_buf[..], &plen, name, name_len, "src/main.omni")) {
        io::printn("Error: project path too long for src/main.omni");
        return false;
    }

    char[1024] text_buf;
    char[] text;
    if (try rendered = io::bprintf(&text_buf, "(println \"Hello from %s!\")\n", (ZString)name)) {
        text = rendered;
    } else {
        io::printfn("Error: failed to compose src/main.omni for project '%s'", (ZString)name);
        return false;
    }

    if (!write_project_text_file(path_buf[:plen], text)) {
        io::printfn("Error: cannot write %s", (ZString)&path_buf[0]);
        return false;
    }
    return true;
}
