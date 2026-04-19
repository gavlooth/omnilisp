module lisp;

import std::io;
import std::core::mem;
import main;

fn bool compiler_bindgen_raw_and_grouped_module_tests(ParsedFunc* funcs, usz func_count, char[] raw_output_path, char[] output_path, char[] grouped_raw_output_path, usz* pass, usz* fail) {
bool generated_ok = generate_ffi_module_pair("bindgen-canonical", "bindgen", funcs, func_count, raw_output_path, output_path);
if (!generated_ok) {
    (*fail)++;
    io::printn("[FAIL] Compiler: bindgen canonical types generate raw/facade modules");
        return false;
}

bool raw_ok = false;
if (try generated = io::file::load_temp((String)raw_output_path)) {
    bool has_raw_module = str_contains(generated, "(module ffi-bindgen-canonical-raw");
    bool has_integer = str_contains(generated, "(^Integer count)");
    bool has_const_string_param = str_contains(generated, "(^String name)");
    bool has_const_string_suffix_param = str_contains(generated, "(omni-bindgen-const-string-suffix (^String name)) ^Integer") &&
        str_contains(generated, "bindgen-policy: parameter name=name c-type=const char * role=string-input ownership=borrowed nullability=unknown teardown=none");
    bool pointer_const_char_buffer_is_mutable = str_contains(generated, "(omni-bindgen-pointer-const-buffer (^ForeignHandle buffer) (^Integer buffer_len)) ^String") &&
        str_contains(generated, "bindgen-policy: parameter name=buffer c-type=char *const role=string-buffer ownership=borrowed nullability=unknown teardown=manual-review buffer-direction=inout size-param=buffer_len");
    bool char_pointer_pointer_param_is_handle = str_contains(generated, "(omni-bindgen-string-list (^ForeignHandle items)) ^Void") &&
        str_contains(generated, "bindgen-policy: parameter name=items c-type=const char ** role=opaque-pointer ownership=borrowed nullability=unknown teardown=none");
    bool mutable_char_pointer_pointer_param_is_handle = str_contains(generated, "(omni-bindgen-mut-string-list (^ForeignHandle items)) ^Void") &&
        str_contains(generated, "bindgen-policy: parameter name=items c-type=char ** role=opaque-pointer ownership=borrowed nullability=unknown teardown=none");
    bool has_buffer_foreign_handle = str_contains(generated, "(^ForeignHandle buffer)");
    bool has_buffer_size_param = str_contains(generated, "(^Integer buffer_len)");
    bool has_string_input = str_contains(generated, "bindgen-policy: parameter name=name c-type=const char * role=string-input ownership=borrowed nullability=unknown teardown=none");
    bool has_string_buffer = str_contains(generated, "bindgen-policy: parameter name=buffer c-type=char * role=string-buffer ownership=borrowed nullability=unknown teardown=manual-review buffer-direction=inout size-param=buffer_len");
    bool named_inout_buffer_direction = str_contains(generated, "(omni-bindgen-inout-named-buffer (^ForeignHandle inout_buffer) (^Integer inout_buffer_len)) ^String") &&
        str_contains(generated, "bindgen-policy: parameter name=inout_buffer c-type=char * role=string-buffer ownership=borrowed nullability=unknown teardown=manual-review buffer-direction=inout size-param=inout_buffer_len");
    bool has_foreign_handle = str_contains(generated, "(^ForeignHandle opaque_handle)");
    bool has_foreign_handle_meta = str_contains(generated, "bindgen-policy: parameter name=opaque_handle c-type=void * role=opaque-pointer ownership=borrowed nullability=unknown teardown=none");
    bool has_callback_meta = str_contains(generated, "bindgen-policy: parameter name=callback c-type=void (*)(int) role=callback ownership=borrowed nullability=unknown teardown=callback-unregister");
    bool char_return_callback_is_handle = str_contains(generated, "(omni-bindgen-char-callback (^ForeignHandle callback)) ^Void") &&
        str_contains(generated, "bindgen-policy: parameter name=callback c-type=char (*)(int) role=callback ownership=borrowed nullability=unknown teardown=callback-unregister");
    bool has_function_shape = str_contains(generated, "bindgen-descriptor: callable type=ForeignCallable runtime=c-abi kind=function c-name=omni_bindgen_buffer name=omni-bindgen-buffer");
    bool has_param_shape = str_contains(generated, "bindgen-descriptor: parameter name=buffer c-type=char * abi-type='ptr type=ForeignHandle role=string-buffer ownership=borrowed nullability=unknown teardown=manual-review buffer-direction=inout size-param=buffer_len");
    bool has_return_shape = str_contains(generated, "bindgen-descriptor: returns c-type=const char * abi-type='string type=String role=string-return ownership=borrowed nullability=unknown teardown=manual-review");
    bool has_fallback = str_contains(generated, "(define [ffi lambda _lib] (omni-bindgen-unnamed (^Integer arg0)) ^Integer)");
    bool has_void = str_contains(generated, ") ^Void)");
    bool has_ret_foreign_handle = str_contains(generated, ") ^ForeignHandle)");
    bool has_ret_string_meta = str_contains(generated, "bindgen-policy: returns c-type=const char * role=string-return ownership=borrowed nullability=unknown teardown=manual-review");
    bool char_pointer_pointer_return_is_handle = str_contains(generated, "(omni-bindgen-ret-string-list) ^ForeignHandle") &&
        str_contains(generated, "bindgen-policy: returns c-type=const char ** role=opaque-pointer ownership=unknown nullability=unknown teardown=manual-review");
    bool unsigned_char_param_is_handle = str_contains(generated, "(omni-bindgen-bytes (^ForeignHandle bytes)) ^Void") &&
        str_contains(generated, "bindgen-policy: parameter name=bytes c-type=const unsigned char * role=opaque-pointer ownership=borrowed nullability=unknown teardown=none");
    bool unsigned_char_return_is_handle = str_contains(generated, "(omni-bindgen-ret-bytes) ^ForeignHandle") &&
        str_contains(generated, "bindgen-policy: returns c-type=const unsigned char * role=opaque-pointer ownership=unknown nullability=unknown teardown=manual-review");
    bool mutable_unsigned_char_param_is_handle = str_contains(generated, "(omni-bindgen-mut-bytes (^ForeignHandle bytes) (^Integer bytes_len)) ^Void") &&
        str_contains(generated, "bindgen-policy: parameter name=bytes c-type=unsigned char * role=opaque-pointer ownership=borrowed nullability=unknown teardown=none");
    bool signed_char_param_is_handle = str_contains(generated, "(omni-bindgen-signed-bytes (^ForeignHandle bytes)) ^Void") &&
        str_contains(generated, "bindgen-policy: parameter name=bytes c-type=const signed char * role=opaque-pointer ownership=borrowed nullability=unknown teardown=none");
    bool signed_char_return_is_handle = str_contains(generated, "(omni-bindgen-ret-signed-bytes) ^ForeignHandle") &&
        str_contains(generated, "bindgen-policy: returns c-type=const signed char * role=opaque-pointer ownership=unknown nullability=unknown teardown=manual-review");
    bool mutable_signed_char_param_is_handle = str_contains(generated, "(omni-bindgen-mut-signed-bytes (^ForeignHandle bytes) (^Integer bytes_len)) ^Void") &&
        str_contains(generated, "bindgen-policy: parameter name=bytes c-type=signed char * role=opaque-pointer ownership=borrowed nullability=unknown teardown=none");
    bool unsigned_char_not_string = !str_contains(generated, "(omni-bindgen-bytes (^String bytes))") &&
        !str_contains(generated, "(omni-bindgen-ret-bytes) ^String") &&
        !str_contains(generated, "c-type=unsigned char * role=string-buffer");
    bool signed_char_not_string = !str_contains(generated, "(omni-bindgen-signed-bytes (^String bytes))") &&
        !str_contains(generated, "(omni-bindgen-ret-signed-bytes) ^String") &&
        !str_contains(generated, "c-type=signed char * role=string-buffer");
    raw_ok = has_raw_module && has_integer && has_const_string_param && has_buffer_foreign_handle &&
        has_buffer_size_param &&
        has_string_input && has_string_buffer &&
        has_foreign_handle && has_foreign_handle_meta && has_callback_meta && has_fallback &&
        char_return_callback_is_handle && has_function_shape && has_param_shape && has_return_shape &&
        has_void && has_ret_foreign_handle && has_ret_string_meta &&
        has_const_string_suffix_param && pointer_const_char_buffer_is_mutable &&
        char_pointer_pointer_param_is_handle && mutable_char_pointer_pointer_param_is_handle &&
        char_pointer_pointer_return_is_handle && named_inout_buffer_direction;
    compiler_test_finish_bool(
        raw_ok,
        "Compiler: bindgen raw module preserves canonical foreign-handle names and metadata annotations",
        "[FAIL] Compiler: bindgen raw module preserves canonical foreign-handle names and metadata annotations",
        pass,
        fail
    );
    compiler_test_finish_bool(
        unsigned_char_param_is_handle && unsigned_char_return_is_handle &&
            mutable_unsigned_char_param_is_handle && signed_char_param_is_handle &&
            signed_char_return_is_handle && mutable_signed_char_param_is_handle &&
            unsigned_char_not_string && signed_char_not_string,
        "Compiler: bindgen keeps signed/unsigned char pointers opaque instead of string-shaped",
        "[FAIL] Compiler: bindgen keeps signed/unsigned char pointers opaque instead of string-shaped",
        pass,
        fail
    );
} else {
    (*fail)++;
    io::printn("[FAIL] Compiler: bindgen canonical types load generated raw module");
}

bool grouped_generated_ok = generate_ffi_module_pair_with_syntax(
    "bindgen-canonical",
    "bindgen",
    funcs,
    func_count,
    grouped_raw_output_path,
    output_path,
    BINDGEN_RAW_SYNTAX_GROUPED
);
if (!grouped_generated_ok) {
    (*fail)++;
    io::printn("[FAIL] Compiler: bindgen canonical types generate grouped raw module");
} else if (try generated = io::file::load_temp((String)grouped_raw_output_path)) {
    bool has_grouped_module = str_contains(generated, "(define [ffi module] _lib \"libbindgen.so\"");
    bool has_grouped_syntax_note = str_contains(generated, ";; Raw syntax: grouped");
    bool has_grouped_int = str_contains(generated, "(omni-bindgen-int (^Integer count)) ^Integer");
    bool has_grouped_buffer = str_contains(generated, "(omni-bindgen-buffer (^ForeignHandle buffer) (^Integer buffer_len)) ^String");
    bool has_grouped_fallback = str_contains(generated, "(omni-bindgen-unnamed (^Integer arg0)) ^Integer");
    bool has_grouped_meta = str_contains(generated, "bindgen-descriptor: callable type=ForeignCallable runtime=c-abi kind=function c-name=omni_bindgen_buffer name=omni-bindgen-buffer");
    bool omits_legacy_lambda = !str_contains(generated, "(define [ffi lambda _lib]");
    compiler_test_finish_bool(
        has_grouped_module && has_grouped_syntax_note && has_grouped_int && has_grouped_buffer &&
            has_grouped_fallback && has_grouped_meta && omits_legacy_lambda,
        "Compiler: bindgen can emit grouped ffi module raw bindings",
        "[FAIL] Compiler: bindgen can emit grouped ffi module raw bindings",
        pass,
        fail
    );
} else {
    (*fail)++;
    io::printn("[FAIL] Compiler: bindgen canonical types load grouped raw module");
}

    return true;
}

fn bool compiler_bindgen_strip_prefix_module_tests(ParsedFunc* funcs, usz func_count, char[] stripped_raw_output_path, char[] stripped_output_path, char[] invalid_strip_raw_output_path, char[] invalid_strip_output_path, char[] collision_strip_raw_output_path, char[] collision_strip_output_path, usz* pass, usz* fail) {
char[64][TOML_MAX_STRIP_PREFIXES] strip_prefixes;
for (usz i = 0; i < TOML_MAX_STRIP_PREFIXES; i++) {
    toml_clear_buf(&strip_prefixes[i], 64);
}
toml_set_default(&strip_prefixes[0], 64, "omni_bindgen_");
bool stripped_generated_ok = generate_ffi_module_pair_with_options(
    "bindgen-canonical-strip",
    "bindgen",
    funcs,
    func_count,
    stripped_raw_output_path,
    stripped_output_path,
    BINDGEN_RAW_SYNTAX_GROUPED,
    &strip_prefixes,
    1
);
if (!stripped_generated_ok) {
    (*fail)++;
    io::printn("[FAIL] Compiler: bindgen strip-prefixes generate grouped raw/facade modules");
} else {
    bool stripped_raw_ok = false;
    bool stripped_facade_ok = false;
    if (try generated = io::file::load_temp((String)stripped_raw_output_path)) {
        stripped_raw_ok =
            str_contains(generated, "(module ffi-bindgen-canonical-strip-raw (export omni-bindgen-int") &&
            str_contains(generated, "(omni-bindgen-int (^Integer count)) ^Integer") &&
            str_contains(generated, "c-name=omni_bindgen_buffer name=omni-bindgen-buffer") &&
            !str_contains(generated, "(module ffi-bindgen-canonical-strip-raw (export int ");
    } else {
        (*fail)++;
        io::printn("[FAIL] Compiler: bindgen strip-prefixes load grouped raw module");
    }
    if (try generated = io::file::load_temp((String)stripped_output_path)) {
        stripped_facade_ok =
            str_contains(generated, "(module ffi-bindgen-canonical-strip (export") &&
            str_contains(generated, "(omni-bindgen-int 'as raw-int)") &&
            str_contains(generated, "(omni-bindgen-const-string-suffix 'as raw-const-string-suffix)") &&
            str_contains(generated, "(omni-bindgen-pointer-const-buffer 'as raw-pointer-const-buffer)") &&
            str_contains(generated, "(define int raw-int)") &&
            str_contains(generated, "(define const-string-suffix raw-const-string-suffix)") &&
            str_contains(generated, "(define (buffer (^ForeignHandle buffer) (^Integer buffer_len))") &&
            str_contains(generated, "(define (pointer-const-buffer (^ForeignHandle buffer) (^Integer buffer_len))") &&
            str_contains(generated, "(raw-buffer buffer_input buffer_len)") &&
            str_contains(generated, "(raw-pointer-const-buffer buffer_input buffer_len)") &&
            !str_contains(generated, "(define omni-bindgen-int raw-omni-bindgen-int)");
    } else {
        (*fail)++;
        io::printn("[FAIL] Compiler: bindgen strip-prefixes load facade module");
    }
    compiler_test_finish_bool(
        stripped_raw_ok && stripped_facade_ok,
        "Compiler: bindgen strip-prefixes preserve raw C symbols and strip facade names",
        "[FAIL] Compiler: bindgen strip-prefixes preserve raw C symbols and strip facade names",
        pass,
        fail
    );
}

{
    ParsedFunc[1] invalid_strip_funcs;
    invalid_strip_funcs[0] = {};
    toml_set_default(&invalid_strip_funcs[0].name, 128, "sqlite3_3d_distance");
    toml_set_default(&invalid_strip_funcs[0].return_c_type, 64, "int");
    toml_set_default(&invalid_strip_funcs[0].return_ffi_type, 16, "'int");
    toml_set_default(&invalid_strip_funcs[0].return_omni_type, 16, "Integer");
    invalid_strip_funcs[0].params = null;
    invalid_strip_funcs[0].param_count = 0;
    invalid_strip_funcs[0].param_capacity = 0;
    invalid_strip_funcs[0].is_variadic = false;
    invalid_strip_funcs[0].return_role = BINDGEN_ROLE_VALUE;
    invalid_strip_funcs[0].return_ownership = BINDGEN_OWNERSHIP_NONE;
    invalid_strip_funcs[0].return_nullability = BINDGEN_NULLABILITY_NA;
    invalid_strip_funcs[0].return_teardown = BINDGEN_TEARDOWN_NONE;

    char[64][TOML_MAX_STRIP_PREFIXES] invalid_strip_prefixes;
    for (usz i = 0; i < TOML_MAX_STRIP_PREFIXES; i++) {
        toml_clear_buf(&invalid_strip_prefixes[i], 64);
    }
    toml_set_default(&invalid_strip_prefixes[0], 64, "sqlite3_");

    bool rejects_digit_leading_strip = !generate_ffi_module_pair_with_options(
        "bindgen-invalid-strip",
        "bindgen",
        &invalid_strip_funcs[0],
        1,
        invalid_strip_raw_output_path,
        invalid_strip_output_path,
        BINDGEN_RAW_SYNTAX_GROUPED,
        &invalid_strip_prefixes,
        1
    );
    bool invalid_raw_not_written = !bindgen_output_file_exists(invalid_strip_raw_output_path);
    bool invalid_facade_not_written = !bindgen_output_file_exists(invalid_strip_output_path);
    compiler_test_finish_bool(
        rejects_digit_leading_strip && invalid_raw_not_written && invalid_facade_not_written,
        "Compiler: bindgen strip-prefixes reject digit-leading emitted names before writing",
        "[FAIL] Compiler: bindgen strip-prefixes reject digit-leading emitted names before writing",
        pass,
        fail
    );
}

{
    ParsedFunc[2] collision_strip_funcs;
    collision_strip_funcs[0] = {};
    collision_strip_funcs[1] = {};
    toml_set_default(&collision_strip_funcs[0].name, 128, "sqlite3_open");
    toml_set_default(&collision_strip_funcs[1].name, 128, "open");
    for (usz i = 0; i < 2; i++) {
        toml_set_default(&collision_strip_funcs[i].return_c_type, 64, "int");
        toml_set_default(&collision_strip_funcs[i].return_ffi_type, 16, "'int");
        toml_set_default(&collision_strip_funcs[i].return_omni_type, 16, "Integer");
        collision_strip_funcs[i].params = null;
        collision_strip_funcs[i].param_count = 0;
        collision_strip_funcs[i].param_capacity = 0;
        collision_strip_funcs[i].is_variadic = false;
        collision_strip_funcs[i].return_role = BINDGEN_ROLE_VALUE;
        collision_strip_funcs[i].return_ownership = BINDGEN_OWNERSHIP_NONE;
        collision_strip_funcs[i].return_nullability = BINDGEN_NULLABILITY_NA;
        collision_strip_funcs[i].return_teardown = BINDGEN_TEARDOWN_NONE;
    }

    char[64][TOML_MAX_STRIP_PREFIXES] collision_strip_prefixes;
    for (usz i = 0; i < TOML_MAX_STRIP_PREFIXES; i++) {
        toml_clear_buf(&collision_strip_prefixes[i], 64);
    }
    toml_set_default(&collision_strip_prefixes[0], 64, "sqlite3_");

    bool rejects_emitted_name_collision = !generate_ffi_module_pair_with_options(
        "bindgen-collision-strip",
        "bindgen",
        &collision_strip_funcs[0],
        2,
        collision_strip_raw_output_path,
        collision_strip_output_path,
        BINDGEN_RAW_SYNTAX_GROUPED,
        &collision_strip_prefixes,
        1
    );
    bool collision_raw_not_written = !bindgen_output_file_exists(collision_strip_raw_output_path);
    bool collision_facade_not_written = !bindgen_output_file_exists(collision_strip_output_path);
    compiler_test_finish_bool(
        rejects_emitted_name_collision && collision_raw_not_written && collision_facade_not_written,
        "Compiler: bindgen strip-prefixes reject emitted name collisions before writing",
        "[FAIL] Compiler: bindgen strip-prefixes reject emitted name collisions before writing",
        pass,
        fail
    );
}

    return true;
}

fn bool compiler_bindgen_facade_failure_tests(ParsedFunc* funcs, usz func_count, int pid, usz seq, usz* pass, usz* fail) {
{
    char[512] facade_fail_raw_path_buf;
    char[512] facade_fail_path_buf;
    char[] facade_fail_raw_path;
    char[] facade_fail_path;
    if (try rendered = io::bprintf(
        &facade_fail_raw_path_buf,
        "/tmp/omni_bindgen_facade_fail-%d-%d_raw.omni",
        pid,
        (int)seq
    )) {
        facade_fail_raw_path = rendered;
    } else {
        (*fail)++;
        io::printn("[FAIL] Compiler: bindgen compose facade-failure raw output path");
        return false;
    }
    if (try rendered = io::bprintf(
        &facade_fail_path_buf,
        "/tmp/omni_bindgen_missing_facade_dir-%d-%d/facade.omni",
        pid,
        (int)seq
    )) {
        facade_fail_path = rendered;
    } else {
        (*fail)++;
        io::printn("[FAIL] Compiler: bindgen compose facade-failure output path");
        return false;
    }
    defer (void)io::file::delete((String)facade_fail_raw_path);
    defer (void)io::file::delete((String)facade_fail_path);

    bool rejects_facade_write_failure = !generate_ffi_module_pair(
        "bindgen-facade-fail",
        "bindgen",
        funcs,
        func_count,
        facade_fail_raw_path,
        facade_fail_path
    );
    compiler_test_finish_bool(
        rejects_facade_write_failure &&
            !bindgen_output_file_exists(facade_fail_raw_path) &&
            !bindgen_output_file_exists(facade_fail_path),
        "Compiler: bindgen cleans new raw output when facade write fails",
        "[FAIL] Compiler: bindgen cleans new raw output when facade write fails",
        pass,
        fail
    );
}

{
    char[512] facade_fail_rerun_raw_path_buf;
    char[512] facade_fail_rerun_path_buf;
    char[] facade_fail_rerun_raw_path;
    char[] facade_fail_rerun_path;
    if (try rendered = io::bprintf(
        &facade_fail_rerun_raw_path_buf,
        "/tmp/omni_bindgen_facade_fail_rerun-%d-%d_raw.omni",
        pid,
        (int)seq
    )) {
        facade_fail_rerun_raw_path = rendered;
    } else {
        (*fail)++;
        io::printn("[FAIL] Compiler: bindgen compose facade-fail rerun raw output path");
        return false;
    }
    if (try rendered = io::bprintf(
        &facade_fail_rerun_path_buf,
        "/tmp/omni_bindgen_missing_facade_dir_rerun-%d-%d/facade.omni",
        pid,
        (int)seq
    )) {
        facade_fail_rerun_path = rendered;
    } else {
        (*fail)++;
        io::printn("[FAIL] Compiler: bindgen compose facade-fail rerun facade output path");
        return false;
    }
    defer (void)io::file::delete((String)facade_fail_rerun_raw_path);
    defer (void)io::file::delete((String)facade_fail_rerun_path);
    if (!compiler_write_text_file(facade_fail_rerun_raw_path, ";; existing raw artifact\n")) {
        (*fail)++;
        io::printn("[FAIL] Compiler: bindgen seed rerun facade-failure raw artifact");
        return false;
    }

    bool rejects_facade_write_failure_on_existing_raw = !generate_ffi_module_pair(
        "bindgen-facade-fail-rerun",
        "bindgen",
        funcs,
        func_count,
        facade_fail_rerun_raw_path,
        facade_fail_rerun_path
    );
    compiler_test_finish_bool(
        rejects_facade_write_failure_on_existing_raw &&
            bindgen_output_file_exists(facade_fail_rerun_raw_path) &&
            !bindgen_output_file_exists(facade_fail_rerun_path),
        "Compiler: bindgen preserves existing raw output on rerun facade-write failure",
        "[FAIL] Compiler: bindgen preserves existing raw output on rerun facade-write failure",
        pass,
        fail
    );
}

    return true;
}

fn bool compiler_bindgen_facade_content_tests(ParsedFunc* funcs, usz func_count, char[] output_path, char[] raw_output_path, usz* pass, usz* fail) {
if (try generated = io::file::load_temp((String)output_path)) {
    bool has_facade_module = str_contains(generated, "(module ffi-bindgen-canonical (export");
    bool imports_raw_file = str_contains(generated, "(import \"bindgen-canonical_raw.omni\")");
    bool aliases_raw = str_contains(generated, "(import ffi-bindgen-canonical-raw (");
    bool defines_wrapper = str_contains(generated, "(define omni-bindgen-int raw-omni-bindgen-int)");
    bool defines_const_string_alias = str_contains(generated, "(define omni-bindgen-const-string raw-omni-bindgen-const-string)");
    bool skips_const_string_wrapper = !str_contains(generated, "REVIEW(bindgen): review omni-bindgen-const-string");
    bool has_buffer_size_validator = str_contains(generated, "(define (bindgen-validate-string-buffer-size buffer-size direction)");
    bool has_buffer_helper = str_contains(generated, "(define (bindgen-prepare-string-buffer buffer buffer-size role ownership nullability teardown direction)");
    bool has_buffer_manual_helper = str_contains(generated, "(define (bindgen-prepare-string-buffer-manual-review buffer buffer-size role ownership nullability teardown direction)");
    bool has_buffer_nil_guard = str_contains(generated, "(if (nil? buffer)") &&
        str_contains(generated, "(raise \"bindgen/string-buffer-nil\")");
    bool has_buffer_unsupported_teardown_guard = str_contains(generated, "(raise \"bindgen/string-buffer-unsupported-teardown\")");
    bool has_buffer_role_guard = str_contains(generated, "(raise \"bindgen/string-buffer-role-mismatch\")");
    bool has_buffer_ownership_guard = str_contains(generated, "(raise \"bindgen/string-buffer-ownership-mismatch\")");
    bool has_buffer_manual_review_guard = str_contains(generated, "(raise \"bindgen/string-buffer-manual-review\")");
    bool has_buffer_size_required_guard = str_contains(generated, "(raise \"bindgen/string-buffer-size-required\")");
    bool has_buffer_size_type_guard = str_contains(generated, "(raise \"bindgen/string-buffer-size-type-mismatch\")");
    bool has_buffer_size_positive_guard = str_contains(generated, "(raise \"bindgen/string-buffer-size-nonpositive\")");
    bool has_buffer_direction_guard = str_contains(generated, "(raise \"bindgen/string-buffer-direction-mismatch\")");
    bool has_callback_helper = str_contains(generated, "(define (bindgen-register-callback callback role ownership nullability teardown)");
    bool has_callback_cleanup_helper = str_contains(generated, "(define (bindgen-unregister-callback callback-handle role ownership nullability teardown)");
    bool has_callback_nil_guard = str_contains(generated, "(raise \"bindgen/callback-nil\")");
    bool has_callback_role_guard = str_contains(generated, "(raise \"bindgen/callback-role-mismatch\")");
    bool has_callback_ownership_guard = str_contains(generated, "(raise \"bindgen/callback-ownership-mismatch\")");
    bool has_callback_unsupported_teardown_guard = str_contains(generated, "(raise \"bindgen/callback-unsupported-teardown\")");
    bool registration_fails_closed_for_generic_callback = str_contains(generated, "(raise \"bindgen/callback-registration-requires-subsystem-shim\")");
    bool unregister_fails_closed_for_generic_callback = str_contains(generated, "(raise \"bindgen/callback-unregister-requires-subsystem-shim\")");
    bool unregister_nil_cleanup_is_idempotent = str_contains(
        generated,
        "(if (nil? callback-handle)\n        nil\n        (if (== role \"callback\")\n            (if (== ownership \"borrowed\")\n                (if (== teardown \"callback-unregister\")\n                    (raise \"bindgen/callback-unregister-requires-subsystem-shim\")"
    );
    bool does_not_register_callback_via_uv_shim = !str_contains(generated, "(__raw-uv-timer-callback-handle callback)");
    bool does_not_unregister_callback_via_uv_shim = !str_contains(generated, "(__raw-uv-timer-callback-unhandle callback-handle)");
    bool has_opaque_arg_helper = str_contains(generated, "(define (bindgen-wrap-opaque-arg handle role ownership nullability teardown)");
    bool has_string_return_borrowed_helper = str_contains(generated, "(define (bindgen-wrap-string-return-borrowed text role ownership nullability teardown)");
    bool has_string_return_unknown_helper = str_contains(generated, "(define (bindgen-wrap-string-return-unknown text role ownership nullability teardown)");
    bool has_opaque_return_manual_helper = str_contains(generated, "(define (bindgen-wrap-opaque-return-manual-review handle role ownership nullability teardown)");
    bool has_opaque_return_none_helper = str_contains(generated, "(define (bindgen-wrap-opaque-return-none handle role ownership nullability teardown)");
    bool flags_buffer_review = str_contains(generated, ";; REVIEW(bindgen): review omni-bindgen-buffer — string return ownership/nullability is not automatic; mutable char* buffer direction/size must be wrapped explicitly.");
    bool flags_foreign_handle_review = str_contains(generated, ";; REVIEW(bindgen): review omni-bindgen-void-ptr — opaque pointer ownership/lifetime must be reviewed.");
    bool flags_callback_review = str_contains(generated, ";; REVIEW(bindgen): review omni-bindgen-callback — callback registration and teardown must be wrapped explicitly.");
    bool wraps_buffer = str_contains(generated, "(define (omni-bindgen-buffer (^ForeignHandle buffer) (^Integer buffer_len))");
    bool wraps_foreign_handle = str_contains(generated, "(define (omni-bindgen-void-ptr (^ForeignHandle opaque_handle))");
    bool wraps_callback = str_contains(generated, "(define (omni-bindgen-callback (^ForeignHandle callback))");
    bool wraps_string_return = str_contains(generated, "(define (omni-bindgen-ret-string)");
    bool wraps_foreign_handle_return = str_contains(generated, "(define (omni-bindgen-ret-ptr)");
    bool wraps_buffer_result = str_contains(
        generated,
        "(let (buffer_input (bindgen-prepare-string-buffer buffer buffer_len \"string-buffer\" \"borrowed\" \"unknown\" \"manual-review\" \"inout\")"
    ) && str_contains(
        generated,
        "result (raw-omni-bindgen-buffer buffer_input buffer_len)"
    ) && str_contains(
        generated,
        "result_text result"
    );
    bool wraps_return_result = str_contains(generated, "(let (result (raw-omni-bindgen-ret-string)") &&
        str_contains(generated, "result_text result");
    bool wraps_foreign_handle_result = str_contains(generated, "(let (result (raw-omni-bindgen-ret-ptr)") &&
        str_contains(generated, "result_handle result");
    bool binds_callback_handle = str_contains(generated, "(let (callback_handle (bindgen-register-callback callback \"callback\" \"borrowed\" \"unknown\" \"callback-unregister\"))\n");
    bool binds_foreign_handle_handle = str_contains(generated, "(let (opaque_handle_handle (bindgen-wrap-opaque-arg opaque_handle \"opaque-pointer\" \"borrowed\" \"unknown\" \"none\"))\n");
    bool has_buffer_body_note = str_contains(generated, "REVIEW: mutable char* buffers need explicit allocation, size, and output semantics.");
    bool has_callback_body_note = str_contains(generated, "REVIEW: confirm callback-handle registration/unregister policy matches the target subsystem contract.");
    bool has_foreign_handle_return_note = str_contains(generated, "REVIEW: wrap opaque returns in a handle/finalizer policy before exposing them broadly.");
    bool forwards_callback = str_contains(generated, "(raw-omni-bindgen-callback callback_handle)");
    bool wraps_borrowed_string_return_via_helper = str_contains(
        generated,
        "(bindgen-wrap-string-return-borrowed result_text \"string-return\" \"borrowed\" \"unknown\" \"manual-review\")"
    );
    bool wraps_unknown_string_return_via_helper = str_contains(
        generated,
        "(bindgen-wrap-string-return-unknown result_text \"string-return\" \"unknown\" \"unknown\" \"manual-review\")"
    );
    bool wraps_opaque_return_via_helper = str_contains(
        generated,
        "(bindgen-wrap-opaque-return-manual-review result_handle \"opaque-pointer\" \"unknown\" \"unknown\" \"manual-review\")"
    );
    bool routes_buffer_through_helper = str_contains(generated, "(bindgen-prepare-string-buffer buffer buffer_len \"string-buffer\" \"borrowed\" \"unknown\" \"manual-review\" \"inout\")");
    bool routes_callback_through_helper = str_contains(generated, "(bindgen-register-callback callback \"callback\" \"borrowed\" \"unknown\" \"callback-unregister\")");
    bool routes_opaque_through_helper = str_contains(generated, "(bindgen-wrap-opaque-arg opaque_handle \"opaque-pointer\" \"borrowed\" \"unknown\" \"none\")");
    bool emits_callback_cleanup = str_contains(generated, "(bindgen-unregister-callback callback_handle \"callback\" \"borrowed\" \"unknown\" \"callback-unregister\")");
    compiler_test_finish_bool(
        has_facade_module && imports_raw_file && aliases_raw && defines_wrapper &&
            defines_const_string_alias && skips_const_string_wrapper &&
            has_buffer_size_validator &&
            has_buffer_helper && has_buffer_manual_helper &&
            has_buffer_nil_guard && has_buffer_unsupported_teardown_guard &&
            has_buffer_role_guard && has_buffer_ownership_guard && has_buffer_manual_review_guard &&
            has_buffer_size_required_guard && has_buffer_size_type_guard &&
            has_buffer_size_positive_guard && has_buffer_direction_guard &&
            has_callback_helper && has_callback_cleanup_helper && has_opaque_arg_helper &&
            has_callback_nil_guard && has_callback_role_guard &&
            has_callback_ownership_guard && has_callback_unsupported_teardown_guard &&
            registration_fails_closed_for_generic_callback &&
            unregister_fails_closed_for_generic_callback &&
            unregister_nil_cleanup_is_idempotent &&
            does_not_register_callback_via_uv_shim &&
            does_not_unregister_callback_via_uv_shim &&
            has_string_return_borrowed_helper && has_string_return_unknown_helper &&
            has_opaque_return_manual_helper && has_opaque_return_none_helper &&
            flags_buffer_review && flags_foreign_handle_review && flags_callback_review && wraps_buffer && wraps_foreign_handle &&
            wraps_callback && wraps_string_return && has_buffer_body_note &&
            has_callback_body_note && forwards_callback &&
            wraps_buffer_result && wraps_return_result &&
            wraps_foreign_handle_return && wraps_foreign_handle_result && has_foreign_handle_return_note &&
            binds_callback_handle && binds_foreign_handle_handle &&
            routes_buffer_through_helper && routes_callback_through_helper && routes_opaque_through_helper &&
            wraps_borrowed_string_return_via_helper &&
            wraps_unknown_string_return_via_helper &&
            wraps_opaque_return_via_helper &&
            emits_callback_cleanup,
        "Compiler: bindgen facade module imports raw, scaffolds foreign-handle wrappers, and flags manual review",
        "[FAIL] Compiler: bindgen facade module imports raw, scaffolds foreign-handle wrappers, and flags manual review",
        pass,
        fail
    );
} else {
    (*fail)++;
    io::printn("[FAIL] Compiler: bindgen canonical types load generated facade module");
}

if (!compiler_write_text_file(output_path, ";; user facade\n")) {
    (*fail)++;
    io::printn("[FAIL] Compiler: bindgen facade preservation write sentinel");
} else {
    bool preserved = generate_ffi_module_pair("bindgen-canonical", "bindgen", funcs, func_count, raw_output_path, output_path);
    if (try generated = io::file::load_temp((String)output_path)) {
        compiler_test_finish_bool(
            preserved && str_contains(generated, ";; user facade"),
            "Compiler: bindgen preserves existing facade file",
            "[FAIL] Compiler: bindgen preserves existing facade file",
            pass,
            fail
        );
    } else {
        (*fail)++;
        io::printn("[FAIL] Compiler: bindgen preserve existing facade load");
    }
}

    return true;
}
