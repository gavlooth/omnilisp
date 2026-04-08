module main;

import std::collections::list;
import std::io;
import std::io::path;
import std::os;

const String[3] AOT_HELPER_COMMAND = { "/usr/bin/env", "bash", "./scripts/build_omni_chelpers.sh" };
const String AOT_HELPER_SCRIPT_PATH = "./scripts/build_omni_chelpers.sh";
const String AOT_ENV_BIN_PATH = "/usr/bin/env";
const String AOT_C3C_FALLBACK = "c3c";
const String[32] AOT_LINK_FLAGS = {
    "-L", "build",
    "-L", "/usr/local/lib",
    "-L", "deps/lib",
    "-l", "omni_chelpers",
    "-l", "lightning",
    "-l", "ffi",
    "-l", "dl",
    "-l", "m",
    "-l", "replxx",
    "-l", "stdc++",
    "-l", "utf8proc",
    "-l", "deflate",
    "-l", "yyjson",
    "-l", "uv",
    "-l", "bearssl",
    "-l", "lmdb"
};
const String[4] AOT_C3C_CANDIDATES = {
    "/usr/local/bin/c3c",
    "/usr/bin/c3c",
    "/bin/c3c",
    "/opt/homebrew/bin/c3c"
};

fn bool string_has_prefix(String text, String prefix) @inline {
    if (prefix.len > text.len) return false;
    for (usz i = 0; i < prefix.len; i++) {
        if (text[i] != prefix[i]) return false;
    }
    return true;
}

fn bool string_has_suffix(String text, String suffix) @inline {
    if (suffix.len > text.len) return false;
    usz start = text.len - suffix.len;
    for (usz i = 0; i < suffix.len; i++) {
        if (text[start + i] != suffix[i]) return false;
    }
    return true;
}

fn bool string_contains(String text, String needle) @inline {
    if (needle.len == 0) return true;
    if (needle.len > text.len) return false;
    for (usz start = 0; start + needle.len <= text.len; start++) {
        bool matches = true;
        for (usz i = 0; i < needle.len; i++) {
            if (text[start + i] != needle[i]) {
                matches = false;
                break;
            }
        }
        if (matches) return true;
    }
    return false;
}

fn bool string_contains_char(String text, char needle) @inline {
    for (usz i = 0; i < text.len; i++) {
        if (text[i] == needle) return true;
    }
    return false;
}

fn bool should_skip_aot_source(String directory, String relative_name) @inline {
    if (string_has_prefix(directory, "src/lisp") && string_has_prefix(relative_name, "tests")) return true;
    if (string_has_prefix(directory, "src/lisp") && string_contains(relative_name, "/tests")) return true;
    return false;
}

fn bool append_matching_sources(
    List { String }* out_sources,
    String directory,
    String prefix,
    String suffix
) {
    Path dir_path;
    if (try dir = path::temp(directory)) {
        dir_path = dir;
    } else {
        io::eprintfn("Error: invalid source directory path '%s'", (ZString)directory.zstr_tcopy());
        return false;
    }
    PathList entries;
    if (try listed = path::ls(tmem, dir_path, true, true)) {
        entries = listed;
    } else {
        io::eprintfn("Error: failed to list source directory '%s'", (ZString)directory.zstr_tcopy());
        return false;
    }

    usz before_count = out_sources.size;
    foreach (entry : entries) {
        String name = entry.str_view();
        if (!string_has_prefix(name, prefix) || !string_has_suffix(name, suffix)) continue;
        if (should_skip_aot_source(directory, name)) continue;
        if (try full_path = dir_path.append(tmem, name)) {
            out_sources.push(full_path.str_view().copy(tmem));
        } else {
            io::eprintfn(
                "Error: failed to compose source path '%s/%s'",
                (ZString)directory.zstr_tcopy(),
                (ZString)name.zstr_tcopy()
            );
            return false;
        }
    }

    if (out_sources.size == before_count) {
        io::eprintfn(
            "Error: no source files matched directory='%s' prefix='%s' suffix='%s'",
            (ZString)directory.zstr_tcopy(),
            (ZString)prefix.zstr_tcopy(),
            (ZString)suffix.zstr_tcopy()
        );
        return false;
    }
    return true;
}

fn int run_spawned_command(String[] command_line, bool search_user_path = false) {
    process::SubProcessOptions opts = {
        .inherit_stdio = true,
        .inherit_environment = true,
        .search_user_path = search_user_path
    };

    if (try proc = process::create(command_line, opts)) {
        if (try status = proc.join()) return status;
        io::eprintfn("Error: failed waiting for command '%s'", (ZString)command_line[0].zstr_tcopy());
        return 2;
    }

    io::eprintfn("Error: failed to start command '%s'", (ZString)command_line[0].zstr_tcopy());
    return 2;
}

fn bool ensure_required_file(String path_text, String label) {
    if (try p = path::temp(path_text)) {
        if (path::is_file(p)) return true;
    }
    io::eprintfn("Error: missing %s at '%s'", (ZString)label.zstr_tcopy(), (ZString)path_text.zstr_tcopy());
    return false;
}

fn bool resolve_aot_c3c_path(String* out_path) {
    char* env_c3c = c_getenv("C3C");
    if (env_c3c != null && env_c3c[0] != 0) {
        if (out_path != null) *out_path = ((ZString)env_c3c).str_view();
        return true;
    }

    foreach (candidate : AOT_C3C_CANDIDATES[..]) {
        if (try p = path::temp(candidate)) {
            if (path::is_file(p)) {
                if (out_path != null) *out_path = candidate;
                return true;
            }
        }
    }
    if (out_path != null) *out_path = AOT_C3C_FALLBACK;
    return true;
}

fn int run_aot_backend_compile(char* temp_c3_path, char* output_binary) {
    if (temp_c3_path == null || temp_c3_path[0] == 0) {
        io::eprintfn("Error: missing AOT temporary C3 path");
        return 2;
    }
    if (output_binary == null || output_binary[0] == 0) {
        io::eprintfn("Error: missing AOT output binary path");
        return 2;
    }
    if (!ensure_required_file(AOT_HELPER_SCRIPT_PATH, "AOT helper script")) return 2;
    if (!ensure_required_file(AOT_ENV_BIN_PATH, "env binary")) return 2;

    String c3c_path;
    if (!resolve_aot_c3c_path(&c3c_path)) return 2;
    if (string_contains_char(c3c_path, '/')) {
        if (!ensure_required_file(c3c_path, "AOT backend compiler")) return 2;
    }

    int helper_status = run_spawned_command(AOT_HELPER_COMMAND[..], false);
    if (helper_status != 0) return helper_status;

    List { String } sources;
    sources.tinit(128);

    if (!append_matching_sources(&sources, "src", "main", ".c3")) return 2;
    if (!append_matching_sources(&sources, "src", "scope_region", ".c3")) return 2;
    if (!append_matching_sources(&sources, "src", "stack_engine", ".c3")) return 2;
    if (!append_matching_sources(&sources, "src/lisp", "", ".c3")) return 2;
    if (!append_matching_sources(&sources, "src/pika", "", ".c3")) return 2;

    sources.push("src/ffi_bindings.c3");
    sources.push(((ZString)temp_c3_path).str_view());

    List { String } command;
    command.tinit(256);
    command.push(AOT_ENV_BIN_PATH);
    command.push("-u");
    command.push("LD_LIBRARY_PATH");
    command.push(c3c_path);
    command.push("compile");
    command.push_all(sources.array_view());
    command.push_all(AOT_LINK_FLAGS[..]);
    command.push("-o");
    command.push(((ZString)output_binary).str_view());

    String[] command_line = command.to_tarray();
    if (command_line.len == 0) {
        io::eprintfn("Error: failed to compose AOT backend command for temp source '%s' and output '%s'", (ZString)temp_c3_path, (ZString)output_binary);
        return 2;
    }

    return run_spawned_command(command_line, false);
}
