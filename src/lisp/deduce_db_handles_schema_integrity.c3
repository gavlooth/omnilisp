module lisp;

fn void deduce_integrity_violation_history_append(
    DeduceIntegrityViolationEntry[DEDUCE_INTEGRITY_HISTORY_LIMIT]* history,
    usz* count,
    DeduceIntegrityViolationEntry entry
) {
    if (history == null || count == null) return;
    if (*count < DEDUCE_INTEGRITY_HISTORY_LIMIT) {
        (*history)[*count] = entry;
        (*count)++;
        return;
    }
    for (usz i = 1; i < DEDUCE_INTEGRITY_HISTORY_LIMIT; i++) {
        (*history)[i - 1] = (*history)[i];
    }
    (*history)[DEDUCE_INTEGRITY_HISTORY_LIMIT - 1] = entry;
}

fn void deduce_relation_schema_note_integrity_violation(
    DeduceRelationSchema* schema,
    DeduceIntegrityViolationClass violation_class,
    SymbolId relation,
    SymbolId code,
    ulong sequence
) {
    if (schema == null) return;
    if (schema.integrity_violation_count < ulong.max) schema.integrity_violation_count++;
    schema.last_integrity_violation_code = code;
    DeduceIntegrityViolationEntry entry = {
        .sequence = sequence,
        .relation = relation,
        .code = code,
        .violation_class = violation_class
    };
    deduce_integrity_violation_history_append(&schema.integrity_violation_history, &schema.integrity_violation_history_count, entry);
    if (violation_class == DEDUCE_INTEGRITY_VIOLATION_KEY) {
        if (schema.key_integrity_violation_count < ulong.max) schema.key_integrity_violation_count++;
        return;
    }
    if (violation_class == DEDUCE_INTEGRITY_VIOLATION_UNIQUE) {
        if (schema.unique_integrity_violation_count < ulong.max) schema.unique_integrity_violation_count++;
        return;
    }
    if (violation_class == DEDUCE_INTEGRITY_VIOLATION_REFERENCE &&
        schema.reference_integrity_violation_count < ulong.max) {
        schema.reference_integrity_violation_count++;
        return;
    }
    if (violation_class == DEDUCE_INTEGRITY_VIOLATION_CHECK &&
        schema.check_integrity_violation_count < ulong.max) {
        schema.check_integrity_violation_count++;
    }
}

fn void deduce_db_note_integrity_violation(
    Relation* rel,
    DeduceIntegrityViolationClass violation_class,
    char[] code_name,
    Interp* interp
) {
    if (rel == null || rel.db == null || interp == null) return;

    DeduceDb* db = rel.db;
    SymbolId code = interp.symbols.intern(code_name);

    if (db.integrity_violation_count < ulong.max) db.integrity_violation_count++;
    if (db.integrity_violation_sequence < ulong.max) db.integrity_violation_sequence++;
    ulong sequence = db.integrity_violation_sequence;
    db.last_integrity_violation_code = code;
    db.last_integrity_violation_relation = rel.name;
    DeduceIntegrityViolationEntry entry = {
        .sequence = sequence,
        .relation = rel.name,
        .code = code,
        .violation_class = violation_class
    };
    deduce_integrity_violation_history_append(&db.integrity_violation_history, &db.integrity_violation_history_count, entry);

    if (violation_class == DEDUCE_INTEGRITY_VIOLATION_KEY) {
        if (db.key_integrity_violation_count < ulong.max) db.key_integrity_violation_count++;
    } else if (violation_class == DEDUCE_INTEGRITY_VIOLATION_UNIQUE) {
        if (db.unique_integrity_violation_count < ulong.max) db.unique_integrity_violation_count++;
    } else if (violation_class == DEDUCE_INTEGRITY_VIOLATION_REFERENCE &&
               db.reference_integrity_violation_count < ulong.max) {
        db.reference_integrity_violation_count++;
    } else if (violation_class == DEDUCE_INTEGRITY_VIOLATION_CHECK &&
               db.check_integrity_violation_count < ulong.max) {
        db.check_integrity_violation_count++;
    }

    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    deduce_relation_schema_note_integrity_violation(schema, violation_class, rel.name, code, sequence);
}
