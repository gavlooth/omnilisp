module lisp;

import std::core::mem;

fn usz deduce_format_usz_hex(char[] out, usz value) {
    if (out.len == 0) return 0;
    if (value == 0) {
        out[0] = '0';
        return 1;
    }

    char[2 * usz.sizeof] rev;
    usz count = 0;
    usz cur = value;
    while (cur != 0 && count < rev.len) {
        usz nibble = cur & 0xF;
        rev[count] = DEDUCE_HEX_DIGITS[nibble];
        count++;
        cur >>= 4;
    }

    for (usz i = 0; i < count; i++) {
        out[i] = rev[count - 1 - i];
    }
    return count;
}

fn bool deduce_append_slice(char[256]* out, usz* idx, char[] src) {
    if (*idx + src.len >= out.len) return false;
    for (usz i = 0; i < src.len; i++) {
        (*out)[*idx + i] = src[i];
    }
    *idx += src.len;
    return true;
}

fn bool deduce_append_hex(char[256]* out, usz* idx, usz value) {
    char[2 * usz.sizeof] hex_buf;
    usz hlen = deduce_format_usz_hex(&hex_buf, value);
    if (*idx + hlen >= out.len) return false;
    for (usz i = 0; i < hlen; i++) {
        (*out)[*idx + i] = hex_buf[i];
    }
    *idx += hlen;
    return true;
}

fn bool deduce_build_memory_path(char[256]* out, DeduceDb* db) {
    if (out == null || db == null) return false;
    usz idx = 0;
    if (!deduce_append_slice(out, &idx, "/tmp/deduce-")) return false;
    if (!deduce_append_hex(out, &idx, (usz)c_getpid())) return false;
    if (idx + 1 >= out.len) return false;
    (*out)[idx++] = '-';
    g_deduce_memory_open_seq++;
    if (!deduce_append_hex(out, &idx, g_deduce_memory_open_seq)) return false;
    if (idx + 1 >= out.len) return false;
    (*out)[idx++] = '-';
    if (!deduce_append_hex(out, &idx, (usz)db)) return false;
    if (idx >= out.len) return false;
    (*out)[idx] = 0;
    return true;
}

fn char* deduce_copy_c_string(char[] src) {
    usz total_len = 0;
    if (!deduce_checked_add_usz(src.len, 1, &total_len)) return null;
    char* out = (char*)mem::malloc(total_len);
    if (out == null) return null;
    for (usz i = 0; i < src.len; i++) out[i] = src[i];
    out[src.len] = 0;
    return out;
}

fn char* deduce_copy_extensional_dbi_name(char[] base_name) {
    usz total_len = 0;
    usz alloc_len = 0;
    if (!deduce_checked_add_usz(base_name.len, DEDUCE_EXTENSIONAL_DBI_SUFFIX.len, &total_len)) return null;
    if (!deduce_checked_add_usz(total_len, 1, &alloc_len)) return null;
    char* out = (char*)mem::malloc(alloc_len);
    if (out == null) return null;
    for (usz i = 0; i < base_name.len; i++) out[i] = base_name[i];
    for (usz i = 0; i < DEDUCE_EXTENSIONAL_DBI_SUFFIX.len; i++) {
        out[base_name.len + i] = DEDUCE_EXTENSIONAL_DBI_SUFFIX[i];
    }
    out[total_len] = 0;
    return out;
}

fn bool deduce_open_extensional_dbi_for_relation(
    Relation* rel,
    MdbTxn* txn,
    bool create,
    MdbDbi* dbi_out,
    Interp* interp
) {
    if (rel == null || rel.db == null || !rel.db.open || txn == null || dbi_out == null || interp == null) return false;
    char[] name_str = interp.symbols.get_name(rel.name);
    char* dbi_name = deduce_copy_extensional_dbi_name(name_str);
    if (dbi_name == null) return false;
    defer mem::free(dbi_name);

    MdbDbi dbi = 0;
    int rc = mdb_dbi_open(txn, dbi_name, create ? MDB_CREATE : 0, &dbi);
    if (rc != MDB_SUCCESS) return false;
    *dbi_out = dbi;
    return true;
}

fn bool deduce_open_extensional_dbi_for_predicate(
    DeduceDb* db,
    SymbolId predicate,
    MdbTxn* txn,
    bool create,
    MdbDbi* dbi_out,
    Interp* interp
) {
    if (db == null || !db.open || predicate == (SymbolId)0 || txn == null || dbi_out == null || interp == null) return false;
    char[] name_str = interp.symbols.get_name(predicate);
    char* dbi_name = deduce_copy_extensional_dbi_name(name_str);
    if (dbi_name == null) return false;
    defer mem::free(dbi_name);

    MdbDbi dbi = 0;
    int rc = mdb_dbi_open(txn, dbi_name, create ? MDB_CREATE : 0, &dbi);
    if (rc != MDB_SUCCESS) return false;
    *dbi_out = dbi;
    return true;
}

fn bool deduce_open_relation_meta_dbi(
    DeduceDb* db,
    MdbTxn* txn,
    bool create,
    MdbDbi* dbi_out
) {
    return deduce_open_relation_meta_dbi_rc(db, txn, create, dbi_out) == MDB_SUCCESS;
}

fn int deduce_open_relation_meta_dbi_rc(
    DeduceDb* db,
    MdbTxn* txn,
    bool create,
    MdbDbi* dbi_out
) {
    if (db == null || !db.open || txn == null || dbi_out == null) return -1;
    char* dbi_name = deduce_copy_c_string(DEDUCE_RELATION_META_DBI_NAME);
    if (dbi_name == null) return -1;
    defer mem::free(dbi_name);

    MdbDbi dbi = 0;
    int rc = mdb_dbi_open(txn, dbi_name, create ? MDB_CREATE : 0, &dbi);
    if (rc != MDB_SUCCESS) return rc;
    *dbi_out = dbi;
    return MDB_SUCCESS;
}

fn bool deduce_open_rule_catalog_dbi(
    DeduceDb* db,
    MdbTxn* txn,
    bool create,
    MdbDbi* dbi_out
) {
    return deduce_open_rule_catalog_dbi_rc(db, txn, create, dbi_out) == MDB_SUCCESS;
}

fn int deduce_open_rule_catalog_dbi_rc(
    DeduceDb* db,
    MdbTxn* txn,
    bool create,
    MdbDbi* dbi_out
) {
    if (db == null || !db.open || txn == null || dbi_out == null) return -1;
    char* dbi_name = deduce_copy_c_string(DEDUCE_RULE_CATALOG_DBI_NAME);
    if (dbi_name == null) return -1;
    defer mem::free(dbi_name);

    MdbDbi dbi = 0;
    int rc = mdb_dbi_open(txn, dbi_name, create ? MDB_CREATE : 0, &dbi);
    if (rc != MDB_SUCCESS) return rc;
    *dbi_out = dbi;
    return MDB_SUCCESS;
}

fn bool deduce_open_rule_signature_dbi(
    DeduceDb* db,
    MdbTxn* txn,
    bool create,
    MdbDbi* dbi_out
) {
    return deduce_open_rule_signature_dbi_rc(db, txn, create, dbi_out) == MDB_SUCCESS;
}

fn int deduce_open_rule_signature_dbi_rc(
    DeduceDb* db,
    MdbTxn* txn,
    bool create,
    MdbDbi* dbi_out
) {
    if (db == null || !db.open || txn == null || dbi_out == null) return -1;
    char* dbi_name = deduce_copy_c_string(DEDUCE_RULE_SIGNATURE_DBI_NAME);
    if (dbi_name == null) return -1;
    defer mem::free(dbi_name);

    MdbDbi dbi = 0;
    int rc = mdb_dbi_open(txn, dbi_name, create ? MDB_CREATE : 0, &dbi);
    if (rc != MDB_SUCCESS) return rc;
    *dbi_out = dbi;
    return MDB_SUCCESS;
}
