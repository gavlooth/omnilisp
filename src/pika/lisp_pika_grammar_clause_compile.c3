module pika;

import lisp;

// Clause compile helper lanes are split across:
// - lisp_pika_grammar_clause_scan.c3
// - lisp_pika_grammar_clause_nodes.c3
// - lisp_pika_grammar_clause_ops.c3

fn int compile_clause(GrammarCompiler* gc, lisp::Value* clause) {
    if (clause == null || lisp::is_nil(clause)) {
        return gc.rb.add("_eps", mk_epsilon());
    }

    // String literal -> TOKENS
    if (lisp::is_string(clause)) return compile_clause_string(gc, clause);

    // Symbol -> reference to another rule
    if (lisp::is_symbol(clause)) return compile_clause_symbol_ref(gc, clause);

    // List -> combinator (seq, first, many, some, etc.)
    if (lisp::is_cons(clause)) {
        lisp::Value* head = clause.cons_val.car;
        lisp::Value* rest = clause.cons_val.cdr;

        if (!lisp::is_symbol(head)) {
            gc_set_error(gc, "parser/grammar-clause-head-not-symbol", "pika/grammar: grammar clause head must be a symbol");
            return gc.rb.add("_err", mk_fail());
        }

        lisp::SymbolId op = head.sym_val;
        char[] op_name = gc.interp.symbols.get_name(op);
        return compile_clause_list_operator(gc, op_name, rest);
    }

    gc_set_error(gc, "parser/grammar-invalid-clause-type", "pika/grammar: invalid clause type");
    return gc.rb.add("_err", mk_fail());
}
