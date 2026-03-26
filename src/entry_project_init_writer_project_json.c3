module main;

import std::io;

fn void write_init_project_json(char* name, usz name_len) {
    char[512] path_buf;
    usz plen = build_project_path(path_buf[..], name, name_len, "build/project.json");
    if (try file = io::file::open((String)path_buf[:plen], "w")) {
        defer (void)file.close();
        file.write("// Generated from omni.toml — do not edit\n")!!;
        file.write("{\n")!!;
        file.write("    \"langrev\": \"1\",\n")!!;
        file.write("    \"version\": \"0.2.0\",\n")!!;
        file.write("    \"sources\": [\"../src\"],\n")!!;
        file.write("    \"output\": \".\",\n")!!;
        file.write("    \"safe\": true,\n")!!;
        file.write("    \"opt\": \"O0\",\n")!!;
        file.write("    \"debug-info\": \"full\",\n")!!;
        file.write("    \"targets\": {\n")!!;
        file.write("        \"main\": {\n")!!;
        file.write("            \"type\": \"executable\"\n")!!;
        file.write("        }\n")!!;
        file.write("    }\n")!!;
        file.write("}\n")!!;
    }
}
