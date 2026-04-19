module lisp;

import std::core::mem;

fn bool deduce_index_columns_equal(DeduceIndexDescriptor* desc, usz[] positions) {
    if (desc == null) return false;
    if (desc.column_count != positions.len) return false;
    for (usz i = 0; i < positions.len; i++) {
        if (desc.column_positions == null || desc.column_positions[i] != positions[i]) return false;
    }
    return true;
}

fn bool deduce_schema_add_index(
    DeduceIndexDescriptor* indexes,
    usz max_indexes,
    usz* index_count,
    usz[] positions,
    bool unique
) {
    if (positions.len == 0) return true;
    if (*index_count >= max_indexes) return false;

    for (usz i = 0; i < *index_count; i++) {
        if (!deduce_index_columns_equal(&indexes[i], positions)) continue;
        if (unique && !indexes[i].unique) indexes[i].unique = true;
        return true;
    }

    if (positions.len > usz.max / usz.sizeof) return false;
    usz* cols = (usz*)mem::malloc(usz.sizeof * positions.len);
    if (cols == null) return false;
    for (usz i = 0; i < positions.len; i++) cols[i] = positions[i];

    indexes[*index_count].column_positions = cols;
    indexes[*index_count].column_count = positions.len;
    indexes[*index_count].unique = unique;
    (*index_count)++;
    return true;
}

fn bool deduce_find_column_position(SymbolId* columns, usz arity, SymbolId name, usz* pos_out) {
    if (pos_out == null || name == (SymbolId)0) return false;
    if (columns == null) return false;
    for (usz i = 0; i < arity; i++) {
        if (columns[i] != name) continue;
        *pos_out = i;
        return true;
    }
    return false;
}

fn void deduce_release_index_descriptors(DeduceIndexDescriptor* indexes, usz count) {
    if (indexes == null) return;
    for (usz i = 0; i < count; i++) {
        if (indexes[i].column_positions != null) {
            mem::free(indexes[i].column_positions);
            indexes[i].column_positions = null;
        }
        indexes[i].column_count = 0;
        indexes[i].unique = false;
    }
}

fn bool deduce_db_ensure_relation_schema_capacity(DeduceDb* db, usz needed) {
    if (db == null) return false;
    if (needed <= db.relation_schema_capacity) return true;

    usz new_cap = db.relation_schema_capacity > 0 ? db.relation_schema_capacity : 8;
    while (new_cap < needed) {
        if (new_cap > usz.max / 2) {
            new_cap = needed;
            break;
        }
        new_cap *= 2;
    }
    if (new_cap > usz.max / DeduceRelationSchema.sizeof) return false;

    DeduceRelationSchema* next =
        (DeduceRelationSchema*)mem::realloc(db.relation_schemas, DeduceRelationSchema.sizeof * new_cap);
    if (next == null) return false;
    db.relation_schemas = next;
    db.relation_schema_capacity = new_cap;
    return true;
}
