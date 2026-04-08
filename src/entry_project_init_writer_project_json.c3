module main;

import std::io;

fn bool write_init_project_json(char* name, usz name_len) {
    char[512] path_buf;
    usz plen = 0;
    if (!build_project_path(path_buf[..], &plen, name, name_len, "build/project.json")) {
        io::printn("Error: project path too long for build/project.json");
        return false;
    }

    char[] text =
        "// Generated from omni.toml — do not edit\n"
        "{\n"
        "    \"langrev\": \"1\",\n"
        "    \"version\": \"0.2.0\",\n"
        "    \"sources\": [\"../src\"],\n"
        "    \"output\": \".\",\n"
        "    \"safe\": true,\n"
        "    \"opt\": \"O0\",\n"
        "    \"debug-info\": \"full\",\n"
        "    \"targets\": {\n"
        "        \"main\": {\n"
        "            \"type\": \"executable\"\n"
        "        }\n"
        "    }\n"
        "}\n";

    if (!write_project_text_file(path_buf[:plen], text)) {
        io::printfn("Error: cannot write %s", (ZString)&path_buf[0]);
        return false;
    }
    return true;
}
