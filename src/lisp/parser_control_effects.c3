module lisp;

import std::collections::list;

fn Expr* Parser.parse_quote(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();  // Capture 'quote' location
    self.lexer.advance();  // consume 'quote'

    Value* datum = self.parse_datum();

    self.expect(T_RPAREN, ")");

    e.tag = E_QUOTE;
    e.quote.datum = datum;
    return e;
}

fn Expr* Parser.parse_reset(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();  // Capture 'reset' location
    self.lexer.advance();  // consume 'reset'

    Expr* body = self.parse_expr();

    self.expect(T_RPAREN, ")");

    e.tag = E_RESET;
    e.reset.body = body;
    return e;
}

fn Expr* Parser.parse_shift(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();  // Capture 'shift' location
    self.lexer.advance();  // consume 'shift'

    if (self.lexer.current.type != T_SYMBOL) { self.set_error("expected continuation name"); return null; }
    SymbolId k_name = self.get_current_symbol();
    self.lexer.advance();

    Expr* body = self.parse_expr();

    self.expect(T_RPAREN, ")");

    e.tag = E_SHIFT;
    e.shift.k_name = k_name;
    e.shift.body = body;
    return e;
}

fn Expr* Parser.parse_perform(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();  // Capture 'perform' location
    self.lexer.advance();  // consume 'perform'

    if (self.lexer.current.type != T_SYMBOL) { self.set_error("expected effect tag"); return null; }
    SymbolId etag = self.get_current_symbol();
    self.lexer.advance();

    Expr* arg = self.parse_expr();

    self.expect(T_RPAREN, ")");

    e.tag = E_PERFORM;
    e.perform.tag = etag;
    e.perform.arg = arg;
    return e;
}

fn Expr* Parser.parse_resolve(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();
    self.lexer.advance();  // consume 'resolve'

    Expr* val = self.parse_expr();

    self.expect(T_RPAREN, ")");

    e.tag = E_RESOLVE;
    e.resolve.value = val;
    return e;
}

// (with-continuation k body...) → (let ((k __k)) body...)
fn Expr* Parser.parse_with_continuation(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();
    self.lexer.advance();  // consume 'with-continuation'

    if (self.lexer.current.type != T_SYMBOL) { self.set_error("expected continuation name"); return null; }
    SymbolId k_name = self.get_current_symbol();
    self.lexer.advance();

    // Parse body expressions (implicit begin)
    Expr* body = self.parse_implicit_begin();
    self.expect(T_RPAREN, ")");

    // Desugar to (let ((k __k)) body)
    e.tag = E_LET;
    e.let_expr.name = k_name;
    Expr* var_ref = self.interp.alloc_expr();
    var_ref.tag = E_VAR;
    var_ref.var_expr.name = self.interp.sym__k;
    e.let_expr.init = var_ref;
    e.let_expr.body = body;
    e.let_expr.is_recursive = false;
    return e;
}

fn bool Parser.consume_handle_strict_annotation(Parser* self) {
    if (self.lexer.current.type != T_SYMBOL) return false;
    if (self.lexer.current.text_len != 7) return false;
    if (self.lexer.current.text[0] != '^') return false;
    if (self.lexer.current.text[1] != 's') return false;
    if (self.lexer.current.text[2] != 't') return false;
    if (self.lexer.current.text[3] != 'r') return false;
    if (self.lexer.current.text[4] != 'i') return false;
    if (self.lexer.current.text[5] != 'c') return false;
    if (self.lexer.current.text[6] != 't') return false;
    self.lexer.advance();  // consume ^strict
    return true;
}

fn bool Parser.parse_handle_old_clause(Parser* self, EffectClause* clause) {
    self.lexer.advance();  // consume inner (
    if (self.lexer.current.type != T_SYMBOL) { self.set_error("expected effect tag"); return false; }
    clause.effect_tag = self.get_current_symbol();
    self.lexer.advance();

    if (self.lexer.current.type != T_SYMBOL) { self.set_error("expected continuation name"); return false; }
    clause.k_name = self.get_current_symbol();
    self.lexer.advance();

    if (self.lexer.current.type != T_SYMBOL) { self.set_error("expected arg name"); return false; }
    clause.arg_name = self.get_current_symbol();
    self.lexer.advance();

    self.expect(T_RPAREN, ")");
    if (self.has_error) return false;
    clause.handler_body = self.parse_expr();
    return !self.has_error;
}

fn bool Parser.parse_handle_new_clause(Parser* self, EffectClause* clause) {
    if (self.lexer.current.type != T_SYMBOL) { self.set_error("expected effect tag"); return false; }
    clause.effect_tag = self.get_current_symbol();
    self.lexer.advance();

    if (self.lexer.current.type != T_SYMBOL) { self.set_error("expected arg name"); return false; }
    clause.arg_name = self.get_current_symbol();
    self.lexer.advance();

    clause.k_name = self.interp.sym__k;
    clause.handler_body = self.parse_implicit_begin();
    return !self.has_error;
}

fn bool Parser.parse_handle_clause(Parser* self, EffectClause* clause) {
    if (self.lexer.current.type == T_LPAREN) {
        return self.parse_handle_old_clause(clause);
    }
    if (self.lexer.current.type == T_SYMBOL) {
        return self.parse_handle_new_clause(clause);
    }
    self.set_error("expected effect tag or '('");
    return false;
}

fn Expr* Parser.parse_handle(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();  // Capture 'handle' location
    self.lexer.advance();  // consume 'handle'

    bool strict = self.consume_handle_strict_annotation();

    Expr* body = self.parse_expr();

    e.tag = E_HANDLE;
    e.handle = (ExprHandle*)mem::malloc(ExprHandle.sizeof);
    e.handle.body = body;
    e.handle.strict_mode = strict;

    List{EffectClause} clause_list;
    while (self.lexer.current.type == T_LPAREN && !self.has_error) {
        self.lexer.advance();  // consume outer (
        EffectClause clause;
        if (!self.parse_handle_clause(&clause)) return null;

        self.expect(T_RPAREN, ")");
        if (self.has_error) return null;

        clause_list.push(clause);
    }

    // Finalize clauses
    e.handle.clause_count = clause_list.len();
    e.handle.clauses = (EffectClause*)mem::malloc(EffectClause.sizeof * clause_list.len());
    for (usz i = 0; i < clause_list.len(); i++) { e.handle.clauses[i] = clause_list[i]; }
    clause_list.free();

    self.expect(T_RPAREN, ")");

    return e;
}

/**
 * Parse an 'and' expression (short-circuit boolean and).
 * (and left right) - returns left if falsy, otherwise right
 */
fn Expr* Parser.parse_and(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();  // Capture 'and' location
    self.lexer.advance();  // consume 'and'

    Expr* left = self.parse_expr();
    Expr* right = self.parse_expr();

    self.expect(T_RPAREN, ")");

    e.tag = E_AND;
    e.and_expr.left = left;
    e.and_expr.right = right;
    return e;
}

/**
 * Parse an 'or' expression (short-circuit boolean or).
 * (or left right) - returns left if truthy, otherwise right
 */
fn Expr* Parser.parse_or(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();  // Capture 'or' location
    self.lexer.advance();  // consume 'or'

    Expr* left = self.parse_expr();
    Expr* right = self.parse_expr();

    self.expect(T_RPAREN, ")");

    e.tag = E_OR;
    e.or_expr.left = left;
    e.or_expr.right = right;
    return e;
}

/**
 * Parse a begin expression (sequencing).
 * (begin e1 e2 ... en) - evaluates all, returns last
 */
fn Expr* Parser.parse_begin(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();  // Capture 'begin' location
    self.lexer.advance();  // consume 'begin'

    List{Expr*} items;
    while (self.lexer.current.type != T_RPAREN && !self.has_error) {
        items.push(self.parse_expr());
    }
    if (self.has_error) { items.free(); return null; }
    self.lexer.advance();  // consume ')'

    // (begin) with no expressions evaluates to nil
    if (items.len() == 0) {
        items.free();
        e.tag = E_LIT;
        e.lit.value = self.interp.alloc_value_root();
        e.lit.value.tag = NIL;
    } else {
        e.tag = E_BEGIN;
        e.begin = mem::malloc(ExprBegin.sizeof);
        e.begin.expr_count = items.len();
        e.begin.exprs = (Expr**)mem::malloc(Expr*.sizeof * items.len());
        for (usz i = 0; i < items.len(); i++) { e.begin.exprs[i] = items[i]; }
        items.free();
    }
    return e;
}
