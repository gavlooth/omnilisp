module lisp;

import std::core::mem;

struct DeducePersistedRuleCatalogRecordV1 {
    char  version;
    char  reserved0;
    char  reserved1;
    char  reserved2;
    char  reserved3;
    char  reserved4;
    char  reserved5;
    char  reserved6;
    ulong body_count;
    ulong head_name_len;
}

struct DeducePersistedRuleCatalogBodyAtomV1 {
    char  negated;
    char  reserved0;
    char  reserved1;
    char  reserved2;
    char  reserved3;
    char  reserved4;
    char  reserved5;
    char  reserved6;
    ulong name_len;
}

const char DEDUCE_PERSISTED_RULE_CATALOG_VERSION = 1;

fn void deduce_persisted_rule_catalog_entry_release(DeducePersistedRuleCatalogEntry* entry) {
    if (entry == null) return;
    if (entry.body_predicates != null) {
        mem::free(entry.body_predicates);
        entry.body_predicates = null;
    }
    if (entry.body_negated != null) {
        mem::free(entry.body_negated);
        entry.body_negated = null;
    }
    entry.head_predicate = (SymbolId)0;
    entry.body_count = 0;
}

fn void deduce_db_clear_persisted_rule_catalog(DeduceDb* db) {
    if (db == null || db.persisted_rule_catalog == null) {
        if (db != null) {
            db.persisted_rule_catalog_count = 0;
            db.persisted_rule_catalog_capacity = 0;
        }
        return;
    }
    for (usz i = 0; i < db.persisted_rule_catalog_count; i++) {
        deduce_persisted_rule_catalog_entry_release(&db.persisted_rule_catalog[i]);
    }
    mem::free(db.persisted_rule_catalog);
    db.persisted_rule_catalog = null;
    db.persisted_rule_catalog_count = 0;
    db.persisted_rule_catalog_capacity = 0;
}

fn bool deduce_db_ensure_persisted_rule_catalog_capacity(DeduceDb* db, usz needed) {
    if (db == null) return false;
    if (needed <= db.persisted_rule_catalog_capacity) return true;

    usz new_cap = db.persisted_rule_catalog_capacity > 0 ? db.persisted_rule_catalog_capacity : 8;
    while (new_cap < needed) {
        if (new_cap > usz.max / 2) {
            new_cap = needed;
            break;
        }
        new_cap *= 2;
    }
    if (new_cap > usz.max / DeducePersistedRuleCatalogEntry.sizeof) return false;

    DeducePersistedRuleCatalogEntry* next =
        (DeducePersistedRuleCatalogEntry*)mem::realloc(
            db.persisted_rule_catalog,
            DeducePersistedRuleCatalogEntry.sizeof * new_cap
        );
    if (next == null) return false;
    db.persisted_rule_catalog = next;
    db.persisted_rule_catalog_capacity = new_cap;
    return true;
}

fn bool deduce_db_append_persisted_rule_catalog_entry(
    DeduceDb* db,
    SymbolId head_predicate,
    SymbolId[] body_predicates,
    bool[] body_negated
) {
    if (db == null || body_predicates.len != body_negated.len) return false;
    if (db.persisted_rule_catalog_count == usz.max) return false;
    usz needed = db.persisted_rule_catalog_count + 1;
    if (!deduce_db_ensure_persisted_rule_catalog_capacity(db, needed)) return false;

    DeducePersistedRuleCatalogEntry* entry = &db.persisted_rule_catalog[db.persisted_rule_catalog_count];
    entry.head_predicate = head_predicate;
    entry.body_predicates = null;
    entry.body_negated = null;
    entry.body_count = body_predicates.len;

    if (body_predicates.len > 0) {
        usz body_predicates_bytes = 0;
        usz body_negated_bytes = 0;
        if (!deduce_checked_mul_usz(SymbolId.sizeof, body_predicates.len, &body_predicates_bytes) ||
            !deduce_checked_mul_usz(bool.sizeof, body_negated.len, &body_negated_bytes)) {
            deduce_persisted_rule_catalog_entry_release(entry);
            return false;
        }
        entry.body_predicates = (SymbolId*)mem::malloc(body_predicates_bytes);
        if (entry.body_predicates == null) {
            deduce_persisted_rule_catalog_entry_release(entry);
            return false;
        }
        entry.body_negated = (bool*)mem::malloc(body_negated_bytes);
        if (entry.body_negated == null) {
            deduce_persisted_rule_catalog_entry_release(entry);
            return false;
        }
        for (usz i = 0; i < body_predicates.len; i++) {
            entry.body_predicates[i] = body_predicates[i];
            entry.body_negated[i] = body_negated[i];
        }
    }

    db.persisted_rule_catalog_count++;
    return true;
}

fn bool deduce_rule_catalog_record_size(DeduceRuleSignature* sig, Interp* interp, usz* out_size) {
    if (sig == null || interp == null || out_size == null || sig.head_predicate == (SymbolId)0) return false;
    char[] head_name = interp.symbols.get_name(sig.head_predicate);
    usz total = 0;
    if (!deduce_checked_add_usz(DeducePersistedRuleCatalogRecordV1.sizeof, head_name.len, &total)) return false;
    for (usz i = 0; i < sig.body_count; i++) {
        if (sig.body_predicates == null) return false;
        char[] body_name = interp.symbols.get_name(sig.body_predicates[i]);
        usz body_atom_size = 0;
        if (!deduce_checked_add_usz(DeducePersistedRuleCatalogBodyAtomV1.sizeof, body_name.len, &body_atom_size)) return false;
        if (!deduce_checked_add_inplace(&total, body_atom_size)) return false;
    }
    *out_size = total;
    return true;
}

fn bool deduce_encode_rule_catalog_record(
    DeduceRuleSignature* sig,
    Interp* interp,
    char[] out
) {
    if (sig == null || interp == null) return false;
    usz needed = 0;
    if (!deduce_rule_catalog_record_size(sig, interp, &needed)) return false;
    if (needed == 0 || out.len < needed) return false;

    char[] head_name = interp.symbols.get_name(sig.head_predicate);
    DeducePersistedRuleCatalogRecordV1 header = {
        .version = DEDUCE_PERSISTED_RULE_CATALOG_VERSION,
        .reserved0 = 0,
        .reserved1 = 0,
        .reserved2 = 0,
        .reserved3 = 0,
        .reserved4 = 0,
        .reserved5 = 0,
        .reserved6 = 0,
        .body_count = sig.body_count,
        .head_name_len = head_name.len
    };
    usz cursor = 0;
    if (!deduce_copy_bytes_into_record(out, &cursor, &header, DeducePersistedRuleCatalogRecordV1.sizeof)) return false;
    if (!deduce_copy_bytes_into_record(out, &cursor, head_name.ptr, head_name.len)) return false;

    for (usz i = 0; i < sig.body_count; i++) {
        char[] body_name = interp.symbols.get_name(sig.body_predicates[i]);
        DeducePersistedRuleCatalogBodyAtomV1 atom = {
            .negated = sig.body_negated != null && sig.body_negated[i] ? (char)1 : (char)0,
            .reserved0 = 0,
            .reserved1 = 0,
            .reserved2 = 0,
            .reserved3 = 0,
            .reserved4 = 0,
            .reserved5 = 0,
            .reserved6 = 0,
            .name_len = body_name.len
        };
        if (!deduce_copy_bytes_into_record(out, &cursor, &atom, DeducePersistedRuleCatalogBodyAtomV1.sizeof)) return false;
        if (!deduce_copy_bytes_into_record(out, &cursor, body_name.ptr, body_name.len)) return false;
    }
    return cursor == needed;
}
