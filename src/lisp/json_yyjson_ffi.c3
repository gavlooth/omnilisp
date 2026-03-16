module lisp;

// ============================================================
// yyjson extern declarations (via C wrapper in csrc/json_helpers.c)
// ============================================================

alias YyjsonDoc = void;
alias YyjsonVal = void;
alias YyjsonMutDoc = void;
alias YyjsonMutVal = void;

// Read API
extern fn YyjsonDoc* omni_yyjson_read(char* dat, usz len, usz flags) @extern("omni_yyjson_read");
extern fn void omni_yyjson_doc_free(YyjsonDoc* doc) @extern("omni_yyjson_doc_free");
extern fn YyjsonVal* omni_yyjson_doc_get_root(YyjsonDoc* doc) @extern("omni_yyjson_doc_get_root");

// Type checking
extern fn int omni_yyjson_get_type(YyjsonVal* val) @extern("omni_yyjson_get_type");
extern fn int omni_yyjson_is_null(YyjsonVal* val) @extern("omni_yyjson_is_null");
extern fn int omni_yyjson_is_true(YyjsonVal* val) @extern("omni_yyjson_is_true");
extern fn int omni_yyjson_is_false(YyjsonVal* val) @extern("omni_yyjson_is_false");
extern fn int omni_yyjson_is_int(YyjsonVal* val) @extern("omni_yyjson_is_int");
extern fn int omni_yyjson_is_real(YyjsonVal* val) @extern("omni_yyjson_is_real");
extern fn int omni_yyjson_is_str(YyjsonVal* val) @extern("omni_yyjson_is_str");
extern fn int omni_yyjson_is_arr(YyjsonVal* val) @extern("omni_yyjson_is_arr");
extern fn int omni_yyjson_is_obj(YyjsonVal* val) @extern("omni_yyjson_is_obj");

// Value getters
extern fn long omni_yyjson_get_sint(YyjsonVal* val) @extern("omni_yyjson_get_sint");
extern fn double omni_yyjson_get_real(YyjsonVal* val) @extern("omni_yyjson_get_real");
extern fn char* omni_yyjson_get_str(YyjsonVal* val) @extern("omni_yyjson_get_str");
extern fn usz omni_yyjson_get_len(YyjsonVal* val) @extern("omni_yyjson_get_len");
extern fn int omni_yyjson_get_bool(YyjsonVal* val) @extern("omni_yyjson_get_bool");

// Array/Object
extern fn usz omni_yyjson_arr_size(YyjsonVal* arr) @extern("omni_yyjson_arr_size");
extern fn YyjsonVal* omni_yyjson_arr_get_first(YyjsonVal* arr) @extern("omni_yyjson_arr_get_first");
extern fn usz omni_yyjson_obj_size(YyjsonVal* obj) @extern("omni_yyjson_obj_size");
extern fn YyjsonVal* omni_yyjson_obj_getn(YyjsonVal* obj, char* key, usz len) @extern("omni_yyjson_obj_getn");
extern fn YyjsonVal* omni_yyjson_obj_get_first(YyjsonVal* obj) @extern("omni_yyjson_obj_get_first");
extern fn YyjsonVal* omni_yyjson_next(YyjsonVal* val) @extern("omni_yyjson_next");

// Mutable API (for json-emit)
extern fn YyjsonMutDoc* omni_yyjson_mut_doc_new() @extern("omni_yyjson_mut_doc_new");
extern fn void omni_yyjson_mut_doc_free(YyjsonMutDoc* doc) @extern("omni_yyjson_mut_doc_free");
extern fn YyjsonMutVal* omni_yyjson_mut_null(YyjsonMutDoc* doc) @extern("omni_yyjson_mut_null");
extern fn YyjsonMutVal* omni_yyjson_mut_bool(YyjsonMutDoc* doc, int val) @extern("omni_yyjson_mut_bool");
extern fn YyjsonMutVal* omni_yyjson_mut_sint(YyjsonMutDoc* doc, long val) @extern("omni_yyjson_mut_sint");
extern fn YyjsonMutVal* omni_yyjson_mut_real(YyjsonMutDoc* doc, double val) @extern("omni_yyjson_mut_real");
extern fn YyjsonMutVal* omni_yyjson_mut_strn(YyjsonMutDoc* doc, char* str, usz len) @extern("omni_yyjson_mut_strn");
extern fn YyjsonMutVal* omni_yyjson_mut_arr(YyjsonMutDoc* doc) @extern("omni_yyjson_mut_arr");
extern fn void omni_yyjson_mut_arr_append(YyjsonMutVal* arr, YyjsonMutVal* val) @extern("omni_yyjson_mut_arr_append");
extern fn YyjsonMutVal* omni_yyjson_mut_obj(YyjsonMutDoc* doc) @extern("omni_yyjson_mut_obj");
extern fn void omni_yyjson_mut_obj_add(YyjsonMutVal* obj, YyjsonMutVal* key, YyjsonMutVal* val) @extern("omni_yyjson_mut_obj_add");
extern fn void omni_yyjson_mut_doc_set_root(YyjsonMutDoc* doc, YyjsonMutVal* root) @extern("omni_yyjson_mut_doc_set_root");
extern fn char* omni_yyjson_mut_write(YyjsonMutDoc* doc, usz* len) @extern("omni_yyjson_mut_write");
extern fn char* omni_yyjson_mut_write_pretty(YyjsonMutDoc* doc, usz* len) @extern("omni_yyjson_mut_write_pretty");
extern fn char* omni_yyjson_mut_write_with_flags(YyjsonMutDoc* doc, usz flags, usz* len) @extern("omni_yyjson_mut_write_with_flags");
extern fn usz omni_yyjson_fp_to_fixed_flag(int precision) @extern("omni_yyjson_fp_to_fixed_flag");

extern fn void c_free_json(void* ptr) @extern("free");

// Parse option bits (mirrors yyjson read flag bits used by the C wrapper).
const usz JSON_PARSE_OPTION_ALLOW_COMMENTS = 1 << 3;
const usz JSON_PARSE_OPTION_ALLOW_TRAILING_COMMAS = 1 << 2;
const usz JSON_PARSE_OPTION_ALLOW_NAN_INF = 1 << 4;

const usz JSON_WRITE_OPTION_PRETTY = 1 << 0;
const usz JSON_WRITE_OPTION_ESCAPE_UNICODE = 1 << 1;
const usz JSON_WRITE_OPTION_ESCAPE_SLASHES = 1 << 2;
const usz JSON_WRITE_OPTION_ALLOW_INF_AND_NAN = 1 << 3;
const usz JSON_WRITE_OPTION_INF_AND_NAN_AS_NULL = 1 << 4;
const usz JSON_WRITE_OPTION_ALLOW_INVALID_UNICODE = 1 << 5;
const usz JSON_WRITE_OPTION_PRETTY_TWO_SPACES = 1 << 6;
const usz JSON_WRITE_OPTION_NEWLINE_AT_END = 1 << 7;
