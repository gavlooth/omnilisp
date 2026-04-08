module lisp;

enum DeduceRelationOpenStatus {
    REL_OPEN_OK,
    REL_OPEN_OUT_OF_MEMORY,
    REL_OPEN_TXN_BEGIN_FAILED,
    REL_OPEN_DBI_FAILED,
    REL_OPEN_TXN_COMMIT_FAILED,
}

bool g_deduce_force_relation_open_commit_fail = false;

fn DeduceRelationOpenStatus deduce_relation_open_dbi(Relation* rel, Interp* interp) {
    // Open LMDB named database for this relation.
    char[] name_str = interp.symbols.get_name(rel.name);
    return deduce_relation_open_dbi_with_name(rel, name_str);
}

fn DeduceRelationOpenStatus deduce_relation_open_dbi_with_name(Relation* rel, char[] name_str) {
    char* dbi_name = deduce_copy_c_string(name_str);
    if (dbi_name == null) return REL_OPEN_OUT_OF_MEMORY;
    defer mem::free(dbi_name);

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(rel.db.env, null, 0, &txn);
    if (rc != MDB_SUCCESS) return REL_OPEN_TXN_BEGIN_FAILED;

    rc = mdb_dbi_open(txn, dbi_name, MDB_CREATE, &rel.dbi);
    if (rc != MDB_SUCCESS) {
        mdb_txn_abort(txn);
        return REL_OPEN_DBI_FAILED;
    }

    if (g_deduce_force_relation_open_commit_fail) {
        mdb_txn_abort(txn);
        return REL_OPEN_TXN_COMMIT_FAILED;
    }

    rc = mdb_txn_commit(txn);
    if (rc != MDB_SUCCESS) return REL_OPEN_TXN_COMMIT_FAILED;
    return REL_OPEN_OK;
}
