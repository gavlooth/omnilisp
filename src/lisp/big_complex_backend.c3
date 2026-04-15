module lisp;

extern fn void* omni_big_complex_from_i64(long real) @extern("omni_big_complex_from_i64");
extern fn void* omni_big_complex_from_double(double real) @extern("omni_big_complex_from_double");
extern fn void* omni_big_complex_from_big_float_parts(void* real, void* imag) @extern("omni_big_complex_from_big_float_parts");
extern fn void* omni_big_complex_from_chars(char* real_chars, usz real_len, char* imag_chars, usz imag_len) @extern("omni_big_complex_from_chars");
extern fn void* omni_big_complex_clone(void* value) @extern("omni_big_complex_clone");
extern fn void omni_big_complex_free(void* value) @extern("omni_big_complex_free");
extern fn char* omni_big_complex_to_string(void* value, usz* out_len) @extern("omni_big_complex_to_string");
extern fn void omni_big_complex_string_free(char* value) @extern("omni_big_complex_string_free");
extern fn void* omni_big_complex_binary(void* lhs, void* rhs, int op) @extern("omni_big_complex_binary");
extern fn void* omni_big_complex_neg(void* value) @extern("omni_big_complex_neg");
extern fn void* omni_big_complex_abs(void* value) @extern("omni_big_complex_abs");
extern fn int omni_big_complex_equal(void* lhs, void* rhs) @extern("omni_big_complex_equal");
extern fn int omni_big_complex_is_zero(void* value) @extern("omni_big_complex_is_zero");
