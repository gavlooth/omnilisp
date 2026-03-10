module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 1b: PRIMITIVE VARIABLE HASH TABLE
// =============================================================================

const usz PRIM_HASH_SIZE = 256;  // Power of 2, > 2*87
struct PrimHashEntry {
    SymbolId key;
    char[]   emit;
}

// Module-level hash table for primitive variable → C3 code mapping
PrimHashEntry[PRIM_HASH_SIZE] g_prim_hash;
bool g_prim_hash_initialized = false;

fn void prim_hash_insert(SymbolId key, char[] emit) {
    usz idx = ((usz)key) & (PRIM_HASH_SIZE - 1);
    for (usz i = 0; i < PRIM_HASH_SIZE; i++) {
        usz slot = (idx + i) & (PRIM_HASH_SIZE - 1);
        if ((uint)g_prim_hash[slot].key == 0xFFFFFFFF) {
            g_prim_hash[slot].key = key;
            g_prim_hash[slot].emit = emit;
            return;
        }
    }
}

fn char[] prim_hash_lookup(SymbolId key) {
    usz idx = ((usz)key) & (PRIM_HASH_SIZE - 1);
    for (usz i = 0; i < PRIM_HASH_SIZE; i++) {
        usz slot = (idx + i) & (PRIM_HASH_SIZE - 1);
        if ((uint)g_prim_hash[slot].key == 0xFFFFFFFF) return "";
        if ((uint)g_prim_hash[slot].key == (uint)key) return g_prim_hash[slot].emit;
    }
    return "";
}

fn void init_prim_hash(Interp* interp) {
    if (g_prim_hash_initialized) return;
    g_prim_hash_initialized = true;

    // Clear
    for (usz i = 0; i < PRIM_HASH_SIZE; i++) {
        g_prim_hash[i].key = (SymbolId)0xFFFFFFFF;
    }

    SymbolTable* st = &interp.symbols;
    init_prim_hash_arithmetic_and_comparison(st);
    init_prim_hash_core_and_strings(st);
    init_prim_hash_files_and_misc(st);
    init_prim_hash_collections_math_bitwise(st);
}
