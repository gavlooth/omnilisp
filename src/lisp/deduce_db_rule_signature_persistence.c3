module lisp;

import std::core::mem;

fn Value* deduce_db_persist_rule_signatures(DeduceDb* db, Interp* interp) {
    if (db == null || !db.open || interp == null) {
        return deduce_raise(interp, "deduce/rule-signature-invalid-state",
            "deduce: invalid persisted rule signature state");
    }

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, 0, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/rule-signature-txn-open-failed",
            "deduce: persisted rule signature txn open failed");
    }

    MdbDbi rule_signature_dbi = 0;
    if (!deduce_open_rule_signature_dbi(db, txn, true, &rule_signature_dbi)) {
        mdb_txn_abort(txn);
        return deduce_raise(interp, "deduce/rule-signature-open-failed",
            "deduce: persisted rule signature dbi open failed");
    }

    rc = mdb_drop(txn, rule_signature_dbi, 0);
    if (rc != MDB_SUCCESS) {
        mdb_txn_abort(txn);
        return deduce_raise(interp, "deduce/rule-signature-clear-failed",
            "deduce: persisted rule signature clear failed");
    }

    bool can_persist_executable_signatures = true;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        if (!deduce_rule_signature_literals_supported(&db.rule_signatures[i])) {
            can_persist_executable_signatures = false;
            break;
        }
    }
    if (!can_persist_executable_signatures) {
        rc = mdb_txn_commit(txn);
        if (rc != MDB_SUCCESS) {
            return deduce_raise(interp, "deduce/rule-signature-commit-failed",
                "deduce: persisted rule signature commit failed");
        }
        return null;
    }

    for (usz i = 0; i < db.rule_signature_count; i++) {
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        usz? head_literal_blob_len_opt = deduce_tuple_encoded_size(sig.head_term_literals[0:sig.head_term_count]);
        if (catch err = head_literal_blob_len_opt) {
            mdb_txn_abort(txn);
            return deduce_raise(interp, "deduce/rule-signature-encode-failed",
                "deduce: persisted rule signature encode failed");
        }
        usz? body_literal_blob_len_opt = deduce_tuple_encoded_size(sig.body_term_literals[0:sig.body_term_total_count]);
        if (catch err = body_literal_blob_len_opt) {
            mdb_txn_abort(txn);
            return deduce_raise(interp, "deduce/rule-signature-encode-failed",
                "deduce: persisted rule signature encode failed");
        }
        usz? record_size_opt = deduce_rule_signature_record_size(sig, head_literal_blob_len_opt, body_literal_blob_len_opt);
        if (catch err = record_size_opt) {
            mdb_txn_abort(txn);
            return deduce_raise(interp, "deduce/rule-signature-encode-failed",
                "deduce: persisted rule signature encode failed");
        }
        usz record_size = record_size_opt;
        if (record_size == 0) {
            mdb_txn_abort(txn);
            return deduce_raise(interp, "deduce/rule-signature-encode-failed",
                "deduce: persisted rule signature encode failed");
        }
        char* record_buf = (char*)mem::malloc(record_size);
        if (record_buf == null) {
            mdb_txn_abort(txn);
            return deduce_raise(interp, "deduce/rule-signature-out-of-memory",
                "deduce: persisted rule signature out of memory");
        }
        defer mem::free(record_buf);

        if (!deduce_encode_rule_signature_record(sig, interp, record_buf[:record_size])) {
            mdb_txn_abort(txn);
            return deduce_raise(interp, "deduce/rule-signature-encode-failed",
                "deduce: persisted rule signature encode failed");
        }

        char[ulong.sizeof] key_buf = {};
        deduce_encode_rule_catalog_key(i, &key_buf);
        MdbVal key = {
            .mv_size = key_buf.len,
            .mv_data = &key_buf[0]
        };
        MdbVal data = {
            .mv_size = record_size,
            .mv_data = record_buf
        };
        rc = mdb_put(txn, rule_signature_dbi, &key, &data, 0);
        if (rc != MDB_SUCCESS) {
            mdb_txn_abort(txn);
            return deduce_raise(interp, "deduce/rule-signature-write-failed",
                "deduce: persisted rule signature write failed");
        }
    }

    rc = mdb_txn_commit(txn);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(interp, "deduce/rule-signature-commit-failed",
            "deduce: persisted rule signature commit failed");
    }
    return null;
}
