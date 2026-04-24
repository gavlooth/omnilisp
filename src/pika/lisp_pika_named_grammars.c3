module pika;

import std::core::mem;
import std::thread;
import lisp;

struct NamedGrammar {
    lisp::SymbolId name;
    Grammar grammar;
    int start_clause;
    bool valid;
}

NamedGrammar* g_named_grammars = null;
int g_grammar_count = 0;
int g_grammar_capacity = 0;
thread::Mutex g_named_grammar_mu;
thread::OnceFlag g_named_grammar_mu_once;

fn void named_grammar_mu_init_once() {
    if (catch err = g_named_grammar_mu.init()) {
        unreachable("named grammar mutex init failed");
    }
}

fn void named_grammar_lock() @inline {
    g_named_grammar_mu_once.call(&named_grammar_mu_init_once);
    g_named_grammar_mu.lock();
}

fn void named_grammar_unlock() @inline {
    g_named_grammar_mu.unlock();
}

fn bool ensure_named_grammar_capacity() {
    if (g_grammar_count < g_grammar_capacity) return true;
    if (g_grammar_count < 0) return false;
    if (g_grammar_capacity < 0) return false;

    usz new_capacity = 16;
    if (g_grammar_capacity >= 16) {
        if (g_grammar_capacity > int.max / 2) return false;
        new_capacity = (usz)g_grammar_capacity * 2;
    }
    if (NamedGrammar.sizeof > 0 && new_capacity > usz.max / NamedGrammar.sizeof) return false;

    NamedGrammar* next = (NamedGrammar*)mem::malloc(NamedGrammar.sizeof * new_capacity);
    if (next == null) return false;
    for (int i = 0; i < g_grammar_count; i++) {
        next[i] = g_named_grammars[i];
    }
    if (g_named_grammars != null) mem::free(g_named_grammars);
    g_named_grammars = next;
    g_grammar_capacity = (int)new_capacity;
    return true;
}

fn int find_grammar_index_locked(lisp::SymbolId name) {
    for (int i = 0; i < g_grammar_count; i++) {
        if ((uint)g_named_grammars[i].name == (uint)name && g_named_grammars[i].valid) {
            return i;
        }
    }
    return -1;
}

fn bool lookup_named_grammar(lisp::SymbolId name, Grammar* out_grammar, int* out_start_clause) {
    named_grammar_lock();
    defer named_grammar_unlock();

    int idx = find_grammar_index_locked(name);
    if (idx < 0) return false;
    *out_grammar = g_named_grammars[idx].grammar;
    *out_start_clause = g_named_grammars[idx].start_clause;
    return true;
}

fn bool named_grammar_exists(lisp::SymbolId name) {
    named_grammar_lock();
    defer named_grammar_unlock();

    return find_grammar_index_locked(name) >= 0;
}

fn bool store_named_grammar(lisp::SymbolId grammar_name, Grammar grammar, int start_clause, bool* duplicate_name) {
    named_grammar_lock();
    defer named_grammar_unlock();

    *duplicate_name = false;
    if (find_grammar_index_locked(grammar_name) >= 0) {
        *duplicate_name = true;
        return false;
    }
    if (!ensure_named_grammar_capacity()) return false;

    g_named_grammars[g_grammar_count].name = grammar_name;
    g_named_grammars[g_grammar_count].grammar = grammar;
    g_named_grammars[g_grammar_count].start_clause = start_clause;
    g_named_grammars[g_grammar_count].valid = true;
    g_grammar_count++;
    return true;
}
