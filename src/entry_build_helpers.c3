module main;

extern fn int c_getpid() @extern("getpid");
extern fn isz c_readlink(char* path, char* buf, usz size) @extern("readlink");
extern fn int c_access(char* path, int mode) @extern("access");

const int BUILD_ACCESS_EXEC = 1;
const int BUILD_ACCESS_WRITE = 2;
// Build helper lanes are split across:
// - entry_build_aot_temp.c3
// - entry_build_repo_paths.c3
// - entry_build_io_helpers.c3
