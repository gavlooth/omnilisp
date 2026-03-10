module main;

const char[] AOT_BUILD_PREFIX = "./scripts/build_omni_chelpers.sh && env -u LD_LIBRARY_PATH c3c compile "
    "src/main*.c3 src/scope_region*.c3 src/stack_engine*.c3 src/ffi_bindings.c3 "
    "src/lisp/*.c3 src/pika/*.c3 "
    "build/_aot_temp.c3 "
    "-L build -L /usr/local/lib -L deps/lib "
    "-l omni_chelpers -l lightning -l ffi -l dl -l m -l replxx -l stdc++ "
    "-l utf8proc -l deflate -l yyjson -l uv -l bearssl -l lmdb "
    "-o ";

fn int run_aot_backend_compile(char* output_binary) {
    char[2048] cmd_buf;
    usz cmd_len = 0;
    for (usz i = 0; i < AOT_BUILD_PREFIX.len && cmd_len < cmd_buf.len - 1; i++) {
        cmd_buf[cmd_len++] = AOT_BUILD_PREFIX[i];
    }
    usz out_len = cstr_len(output_binary);
    for (usz i = 0; i < out_len && cmd_len < cmd_buf.len - 1; i++) {
        cmd_buf[cmd_len++] = output_binary[i];
    }
    cmd_buf[cmd_len] = 0;
    return system(&cmd_buf[0]);
}
