module lisp;

extern fn void* omni_big_float_from_i64(long value) @extern("omni_big_float_from_i64");
extern fn void* omni_big_float_from_double(double value) @extern("omni_big_float_from_double");
extern fn void* omni_big_float_from_chars(char* chars, usz len) @extern("omni_big_float_from_chars");
extern fn void* omni_big_float_clone(void* value) @extern("omni_big_float_clone");
extern fn void omni_big_float_free(void* value) @extern("omni_big_float_free");
extern fn char* omni_big_float_to_string(void* value, usz* out_len) @extern("omni_big_float_to_string");
extern fn void omni_big_float_string_free(char* value) @extern("omni_big_float_string_free");
extern fn int omni_big_float_to_double(void* value, double* out) @extern("omni_big_float_to_double");
extern fn int omni_big_float_compare(void* lhs, void* rhs) @extern("omni_big_float_compare");
extern fn int omni_big_float_compare_i64(void* lhs, long rhs) @extern("omni_big_float_compare_i64");
extern fn int omni_big_float_compare_double(void* lhs, double rhs) @extern("omni_big_float_compare_double");
extern fn void* omni_big_float_binary(void* lhs, void* rhs, int op) @extern("omni_big_float_binary");
extern fn void* omni_big_float_neg(void* value) @extern("omni_big_float_neg");
extern fn void* omni_big_float_abs(void* value) @extern("omni_big_float_abs");
