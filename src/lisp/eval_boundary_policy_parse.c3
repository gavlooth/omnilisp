module lisp;

import std::io;

enum BoundaryPolicyParseStatus : char {
    BOUNDARY_POLICY_PARSE_MISSING,
    BOUNDARY_POLICY_PARSE_OK,
    BOUNDARY_POLICY_PARSE_INVALID,
}

fn bool boundary_ascii_is_space(char c) @inline {
    return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

fn char boundary_ascii_lower(char c) @inline {
    if (c >= 'A' && c <= 'Z') return c + ('a' - 'A');
    return c;
}

fn usz boundary_trimmed_span(char* raw, char** start_out) @inline {
    if (raw == null) {
        if (start_out != null) *start_out = null;
        return 0;
    }

    char* start = raw;
    while (*start != 0 && boundary_ascii_is_space(*start)) start++;
    if (*start == 0) {
        if (start_out != null) *start_out = start;
        return 0;
    }

    char* end = start;
    while (*end != 0) end++;
    while (end > start && boundary_ascii_is_space(*(end - 1))) end--;

    if (start_out != null) *start_out = start;
    return (usz)(end - start);
}

fn bool boundary_token_equals_ci(char* start, usz len, char* token) @inline {
    if (start == null || token == null) return false;

    usz i = 0;
    while (i < len && token[i] != 0) {
        if (boundary_ascii_lower(start[i]) != boundary_ascii_lower(token[i])) return false;
        i++;
    }
    if (i != len) return false;
    return token[i] == 0;
}

fn BoundaryPolicyParseStatus boundary_policy_parse_bool_token(char* raw, bool* out_value) @inline {
    if (out_value != null) *out_value = false;

    char* start = null;
    usz len = boundary_trimmed_span(raw, &start);
    if (len == 0) return BOUNDARY_POLICY_PARSE_MISSING;

    if (len == 1) {
        char c0 = boundary_ascii_lower(start[0]);
        if (c0 == '1') {
            if (out_value != null) *out_value = true;
            return BOUNDARY_POLICY_PARSE_OK;
        }
        if (c0 == '0') {
            if (out_value != null) *out_value = false;
            return BOUNDARY_POLICY_PARSE_OK;
        }
        return BOUNDARY_POLICY_PARSE_INVALID;
    }

    if (boundary_token_equals_ci(start, len, (char*)"true") ||
        boundary_token_equals_ci(start, len, (char*)"yes") ||
        boundary_token_equals_ci(start, len, (char*)"on")) {
        if (out_value != null) *out_value = true;
        return BOUNDARY_POLICY_PARSE_OK;
    }
    if (boundary_token_equals_ci(start, len, (char*)"false") ||
        boundary_token_equals_ci(start, len, (char*)"no") ||
        boundary_token_equals_ci(start, len, (char*)"off")) {
        if (out_value != null) *out_value = false;
        return BOUNDARY_POLICY_PARSE_OK;
    }
    return BOUNDARY_POLICY_PARSE_INVALID;
}

fn BoundaryPolicyParseStatus boundary_policy_parse_usize_token(char* raw, usz* out_value) @inline {
    if (out_value != null) *out_value = 0;

    char* start = null;
    usz len = boundary_trimmed_span(raw, &start);
    if (len == 0) return BOUNDARY_POLICY_PARSE_MISSING;

    usz result = 0;
    for (usz i = 0; i < len; i++) {
        char ch = start[i];
        if (ch < '0' || ch > '9') return BOUNDARY_POLICY_PARSE_INVALID;
        result = result * 10 + (usz)(ch - '0');
    }
    if (out_value != null) *out_value = result;
    return BOUNDARY_POLICY_PARSE_OK;
}

fn bool boundary_policy_strict_mode_enabled() @inline {
    bool strict_env = false;
    BoundaryPolicyParseStatus strict_status =
        boundary_policy_parse_bool_token(boundary_c_getenv("OMNI_BOUNDARY_POLICY_STRICT"), &strict_env);
    if (strict_status == BOUNDARY_POLICY_PARSE_OK) return strict_env;

    if (boundary_c_getenv("OMNI_TEST_SUMMARY") != null) return true;
    if (boundary_c_getenv("OMNI_BOUNDARY_ASSERT_SUMMARY") != null) return true;
    return false;
}

fn void boundary_policy_report_invalid_bool(char* name, char* raw, bool default_value, bool strict_mode) @noinline {
    char* raw_value = raw != null ? raw : (char*)"(null)";
    io::eprintfn(
        "[boundary][policy] invalid boolean env %s='%s' (expected: 0/1/true/false/on/off/yes/no, default=%d)",
        (ZString)name,
        (ZString)raw_value,
        default_value ? 1 : 0
    );
    if (strict_mode) {
        unreachable("boundary policy: invalid boolean env");
    }
    if (!g_boundary_policy_invalid_warned) {
        io::eprintfn("[boundary][policy] using defaults after invalid env values (strict mode off)");
        g_boundary_policy_invalid_warned = true;
    }
}

fn void boundary_policy_report_invalid_usize(char* name, char* raw, usz default_value, bool strict_mode) @noinline {
    char* raw_value = raw != null ? raw : (char*)"(null)";
    io::eprintfn(
        "[boundary][policy] invalid usize env %s='%s' (expected: unsigned integer, default=%d)",
        (ZString)name,
        (ZString)raw_value,
        (long)default_value
    );
    if (strict_mode) {
        unreachable("boundary policy: invalid usize env");
    }
    if (!g_boundary_policy_invalid_warned) {
        io::eprintfn("[boundary][policy] using defaults after invalid env values (strict mode off)");
        g_boundary_policy_invalid_warned = true;
    }
}

fn bool boundary_policy_read_bool(char* name, bool default_value, bool strict_mode) @inline {
    char* raw = boundary_c_getenv(name);
    bool parsed = false;
    BoundaryPolicyParseStatus status = boundary_policy_parse_bool_token(raw, &parsed);
    if (status == BOUNDARY_POLICY_PARSE_OK) return parsed;
    if (status == BOUNDARY_POLICY_PARSE_INVALID) {
        boundary_policy_report_invalid_bool(name, raw, default_value, strict_mode);
    }
    return default_value;
}

fn usz boundary_policy_read_usize(char* name, usz default_value, bool strict_mode) @inline {
    char* raw = boundary_c_getenv(name);
    usz parsed = 0;
    BoundaryPolicyParseStatus status = boundary_policy_parse_usize_token(raw, &parsed);
    if (status == BOUNDARY_POLICY_PARSE_OK) return parsed;
    if (status == BOUNDARY_POLICY_PARSE_INVALID) {
        boundary_policy_report_invalid_usize(name, raw, default_value, strict_mode);
    }
    return default_value;
}

fn void boundary_policy_load_from_env() @noinline {
    bool strict_mode = boundary_policy_strict_mode_enabled();
    g_boundary_runtime_policy = {
        .initialized = true,
        .strict_mode = strict_mode,
        .scope_chain_scan_bypass = boundary_policy_read_bool(
            "OMNI_BOUNDARY_SCOPE_CHAIN_SCAN_BYPASS",
            false,
            strict_mode
        ),
        .scope_chain_scan_budget = boundary_policy_read_usize(
            "OMNI_BOUNDARY_SCOPE_CHAIN_SCAN_BUDGET",
            0,
            strict_mode
        ),
        .graph_audit = boundary_policy_read_bool(
            "OMNI_BOUNDARY_GRAPH_AUDIT",
            false,
            strict_mode
        ),
        .graph_audit_rate = boundary_policy_read_usize(
            "OMNI_BOUNDARY_GRAPH_AUDIT_RATE",
            1,
            strict_mode
        ),
        .graph_audit_max_roots = boundary_policy_read_usize(
            "OMNI_BOUNDARY_GRAPH_AUDIT_MAX_ROOTS",
            0,
            strict_mode
        ),
        .verbose_telemetry = boundary_policy_read_bool(
            "OMNI_BOUNDARY_VERBOSE_TELEMETRY",
            false,
            strict_mode
        ),
        .trace_runtime = boundary_policy_read_bool(
            "OMNI_BOUNDARY_TRACE",
            false,
            strict_mode
        ),
        .benchmark_trace_runtime = boundary_policy_read_bool(
            "OMNI_BOUNDARY_BENCHMARK_TRACE",
            false,
            strict_mode
        ),
    };
}
