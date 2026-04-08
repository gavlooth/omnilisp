module lisp;

import std::core::mem;

struct DeducePersistedRuleSignatureRecordV1 {
    char     version;
    char     reserved0;
    char     reserved1;
    char     reserved2;
    char     reserved3;
    char     reserved4;
    char     reserved5;
    char     reserved6;
    SymbolId head_predicate;
    ulong    body_count;
    ulong    head_term_count;
    ulong    body_term_total_count;
    uint     max_var_id;
    uint     reserved7;
    ulong    head_literal_blob_len;
    ulong    body_literal_blob_len;
}

const char DEDUCE_PERSISTED_RULE_SIGNATURE_VERSION = 1;

fn void deduce_write_u64_le(char* out, ulong value) @inline {
    if (out == null) return;
    for (usz i = 0; i < ulong.sizeof; i++) {
        out[i] = (char)((value >> (i * 8)) & 0xFF);
    }
}

fn bool deduce_rule_signature_literals_supported(DeduceRuleSignature* sig) {
    if (sig == null) return false;
    for (usz i = 0; i < sig.head_term_count; i++) {
        if (sig.head_term_literals == null) break;
        if (!deduce_value_supported_by_tuple_codec(sig.head_term_literals[i])) return false;
    }
    for (usz i = 0; i < sig.body_term_total_count; i++) {
        if (sig.body_term_literals == null) break;
        if (!deduce_value_supported_by_tuple_codec(sig.body_term_literals[i])) return false;
    }
    return true;
}

fn usz? deduce_tuple_encoded_size(Value*[] vals) {
    usz total = 0;
    for (usz i = 0; i < vals.len; i++) {
        Value* v = vals[i];
        if (v == null || v.tag == NIL) {
            total += 1;
        } else if (v.tag == INT || v.tag == DOUBLE) {
            total += 9;
        } else if (v.tag == STRING) {
            usz slen = v.str_len;
            if (slen > 0xFFFF) slen = 0xFFFF;
            total += 3 + slen;
        } else if (v.tag == SYMBOL) {
            total += 3;
        } else if (v.tag == TIME_POINT) {
            total += 34;
        } else {
            return WRITE_FAILED~;
        }
    }
    return total;
}

fn bool deduce_encode_rule_catalog_key(ulong ordinal, char[ulong.sizeof]* out) {
    if (out == null) return false;
    deduce_write_u64_le(&(*out)[0], ordinal);
    return true;
}

fn usz deduce_rule_signature_record_size(
    DeduceRuleSignature* sig,
    usz head_literal_blob_len,
    usz body_literal_blob_len
) {
    if (sig == null) return 0;
    return DeducePersistedRuleSignatureRecordV1.sizeof +
        (SymbolId.sizeof * sig.body_count) +
        (bool.sizeof * sig.body_count) +
        (ulong.sizeof * sig.body_count) +
        (int.sizeof * sig.body_count) +
        (bool.sizeof * sig.body_count) +
        (ulong.sizeof * sig.body_count) +
        (DeduceRuleTermKind.sizeof * sig.head_term_count) +
        (DeduceAggregateOpKind.sizeof * sig.head_term_count) +
        (bool.sizeof * sig.head_term_count) +
        (uint.sizeof * sig.head_term_count) +
        head_literal_blob_len +
        (usz.sizeof * sig.body_count) +
        (usz.sizeof * sig.body_count) +
        (bool.sizeof * sig.body_term_total_count) +
        (uint.sizeof * sig.body_term_total_count) +
        body_literal_blob_len;
}

fn bool deduce_copy_bytes_into_record(char[] out, usz* cursor, void* src, usz len) {
    if (out.ptr == null || cursor == null) return false;
    if (*cursor + len > out.len) return false;
    if (len > 0 && src == null) return false;
    if (len > 0) mem::copy(out.ptr + *cursor, src, len);
    *cursor += len;
    return true;
}

fn bool deduce_encode_rule_signature_record(
    DeduceRuleSignature* sig,
    Interp* interp,
    char[] out
) {
    if (sig == null || interp == null) return false;
    if (!deduce_rule_signature_literals_supported(sig)) return false;

    usz? head_literal_blob_len_opt = deduce_tuple_encoded_size(sig.head_term_literals[0:sig.head_term_count]);
    if (catch err = head_literal_blob_len_opt) return false;
    usz? body_literal_blob_len_opt = deduce_tuple_encoded_size(sig.body_term_literals[0:sig.body_term_total_count]);
    if (catch err = body_literal_blob_len_opt) return false;
    usz head_literal_blob_len = head_literal_blob_len_opt;
    usz body_literal_blob_len = body_literal_blob_len_opt;
    usz needed = deduce_rule_signature_record_size(sig, head_literal_blob_len, body_literal_blob_len);
    if (needed == 0 || out.len < needed) return false;

    DeducePersistedRuleSignatureRecordV1 header = {
        .version = DEDUCE_PERSISTED_RULE_SIGNATURE_VERSION,
        .reserved0 = 0,
        .reserved1 = 0,
        .reserved2 = 0,
        .reserved3 = 0,
        .reserved4 = 0,
        .reserved5 = 0,
        .reserved6 = 0,
        .head_predicate = sig.head_predicate,
        .body_count = sig.body_count,
        .head_term_count = sig.head_term_count,
        .body_term_total_count = sig.body_term_total_count,
        .max_var_id = sig.max_var_id,
        .reserved7 = 0,
        .head_literal_blob_len = head_literal_blob_len,
        .body_literal_blob_len = body_literal_blob_len,
    };

    usz cursor = 0;
    if (!deduce_copy_bytes_into_record(out, &cursor, &header, DeducePersistedRuleSignatureRecordV1.sizeof)) return false;
    if (!deduce_copy_bytes_into_record(out, &cursor, sig.body_predicates, SymbolId.sizeof * sig.body_count)) return false;
    if (!deduce_copy_bytes_into_record(out, &cursor, sig.body_negated, bool.sizeof * sig.body_count)) return false;
    if (!deduce_copy_bytes_into_record(out, &cursor, sig.body_bound_masks, ulong.sizeof * sig.body_count)) return false;
    if (!deduce_copy_bytes_into_record(out, &cursor, sig.body_selected_indexes, int.sizeof * sig.body_count)) return false;
    if (!deduce_copy_bytes_into_record(out, &cursor, sig.body_filter_pushdown, bool.sizeof * sig.body_count)) return false;
    if (!deduce_copy_bytes_into_record(out, &cursor, sig.body_projection_masks, ulong.sizeof * sig.body_count)) return false;
    if (!deduce_copy_bytes_into_record(out, &cursor, sig.head_term_kinds, DeduceRuleTermKind.sizeof * sig.head_term_count)) return false;
    if (!deduce_copy_bytes_into_record(out, &cursor, sig.head_term_aggregate_ops, DeduceAggregateOpKind.sizeof * sig.head_term_count)) return false;
    if (!deduce_copy_bytes_into_record(out, &cursor, sig.head_term_is_variable, bool.sizeof * sig.head_term_count)) return false;
    if (!deduce_copy_bytes_into_record(out, &cursor, sig.head_term_var_ids, uint.sizeof * sig.head_term_count)) return false;

    if (head_literal_blob_len > 0) {
        usz? encoded_head_literals = encode_tuple(sig.head_term_literals[0:sig.head_term_count], out.ptr + cursor, head_literal_blob_len);
        if (catch err = encoded_head_literals) return false;
        if (encoded_head_literals != head_literal_blob_len) return false;
    }
    cursor += head_literal_blob_len;

    if (!deduce_copy_bytes_into_record(out, &cursor, sig.body_term_offsets, usz.sizeof * sig.body_count)) return false;
    if (!deduce_copy_bytes_into_record(out, &cursor, sig.body_term_counts, usz.sizeof * sig.body_count)) return false;
    if (!deduce_copy_bytes_into_record(out, &cursor, sig.body_term_is_variable, bool.sizeof * sig.body_term_total_count)) return false;
    if (!deduce_copy_bytes_into_record(out, &cursor, sig.body_term_var_ids, uint.sizeof * sig.body_term_total_count)) return false;

    if (body_literal_blob_len > 0) {
        usz? encoded_body_literals = encode_tuple(sig.body_term_literals[0:sig.body_term_total_count], out.ptr + cursor, body_literal_blob_len);
        if (catch err = encoded_body_literals) return false;
        if (encoded_body_literals != body_literal_blob_len) return false;
    }
    cursor += body_literal_blob_len;
    return cursor == needed;
}

fn bool deduce_record_slice(char[] data, usz* cursor, usz len, char[]* out_slice) {
    if (cursor == null || out_slice == null) return false;
    if (*cursor + len > data.len) return false;
    if (len == 0) {
        *out_slice = {};
        return true;
    }
    *out_slice = (((char*)data.ptr) + *cursor)[:len];
    *cursor += len;
    return true;
}
