module main;

import std::io;

fn void write_init_toml(char* name, usz name_len) {
    char[512] path_buf;
    usz plen = build_project_path(path_buf[..], name, name_len, "omni.toml");
    if (try file = io::file::open((String)path_buf[:plen], "w")) {
        defer (void)file.close();
        file.write("[project]\n")!!;
        file.write("name = \"")!!;
        file.write(name[:name_len])!!;
        file.write("\"\nversion = \"0.1.5\"\n\n")!!;
        file.write("[build]\n")!!;
        file.write("output-dir = \"build\"\n")!!;
        file.write("safety = \"safe\"        # \"safe\", \"fast\", or \"none\"\n")!!;
        file.write("opt = \"O0\"             # \"O0\", \"O1\", \"O2\", \"O3\", \"Os\", \"Oz\"\n")!!;
        file.write("debug-info = \"full\"    # \"full\", \"line-tables\", \"none\"\n")!!;
        file.write("# sanitize = \"none\"    # \"none\", \"address\", \"memory\", \"thread\"\n")!!;
        file.write("# single-module = false\n\n")!!;
        file.write("# FFI dependencies — uncomment and edit:\n")!!;
        file.write("# [dependencies.ffi.math]\n")!!;
        file.write("# library = \"m\"\n")!!;
        file.write("# headers = [\"/usr/include/math.h\"]\n")!!;
        file.write("# functions = [\"sin\", \"cos\", \"sqrt\"]\n")!!;
    }
}

fn void write_init_main_omni(char* name, usz name_len) {
    char[512] path_buf;
    usz plen = build_project_path(path_buf[..], name, name_len, "src/main.omni");
    if (try file = io::file::open((String)path_buf[:plen], "w")) {
        defer (void)file.close();
        file.write("(println \"Hello from ")!!;
        file.write(name[:name_len])!!;
        file.write("!\")\n")!!;
    }
}
