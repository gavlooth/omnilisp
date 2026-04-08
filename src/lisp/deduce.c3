module lisp;

// ============================================================
// Deduce — Relational Datalog for Omni
//
// LMDB-backed named relations, Datalog rules, bottom-up query.
// ============================================================

// ============================================================
// Namespaced command aliases (deduce/<command> ...)
// ============================================================
fn Value* prim_deduce_ns_open(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_open(args, env, interp);
}

fn Value* prim_deduce_ns_open_named(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_open_named(args, env, interp);
}

fn Value* prim_deduce_ns_block(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_block(args, env, interp);
}

fn Value* prim_deduce_ns_commit(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_commit(args, env, interp);
}

fn Value* prim_deduce_ns_abort(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_abort(args, env, interp);
}

fn Value* prim_deduce_ns_scan(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_scan(args, env, interp);
}

fn Value* prim_deduce_ns_scan_range(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_scan_range(args, env, interp);
}

fn Value* prim_deduce_ns_query(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_query(args, env, interp);
}

fn Value* prim_deduce_ns_schema(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_schema(args, env, interp);
}

fn Value* prim_deduce_ns_indexes(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_indexes(args, env, interp);
}

fn Value* prim_deduce_ns_stats(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_stats(args, env, interp);
}

fn Value* prim_deduce_ns_count(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_count(args, env, interp);
}

fn Value* prim_deduce_ns_match(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_match(args, env, interp);
}

fn Value* prim_deduce_ns_fact(Value*[] args, Env* env, Interp* interp) {
    return prim_assert_fact(args, env, interp);
}

fn Value* prim_deduce_ns_retract(Value*[] args, Env* env, Interp* interp) {
    return prim_retract_fact(args, env, interp);
}

fn Value* prim_deduce_ns_clear(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_clear(args, env, interp);
}

fn Value* prim_deduce_ns_drop(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_drop(args, env, interp);
}

fn Value* prim_deduce_ns_rule(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_rule(args, env, interp);
}

fn Value* prim_deduce_ns_explain(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_explain(args, env, interp);
}

fn Value* prim_deduce_ns_why_result(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_why_result(args, env, interp);
}

fn Value* prim_deduce_ns_analyze(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_analyze(args, env, interp);
}

fn Value* prim_deduce_ns_materialize(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_materialize(args, env, interp);
}

fn Value* prim_deduce_ns_dematerialize(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_dematerialize(args, env, interp);
}

fn Value* prim_deduce_ns_refresh(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_refresh(args, env, interp);
}

// ============================================================
// Dispatch helpers
// ============================================================
fn bool deduce_cmd_matches(char[] name, char[] cmd) {
    return deduce_name_equals(name, cmd);
}

fn Value* prim_deduce_dispatch(char[] name, Value*[] rest, Env* env, Interp* interp) {
    if (deduce_cmd_matches(name, "open")) return prim_deduce_open(rest, env, interp);
    if (deduce_cmd_matches(name, "open-named")) return prim_deduce_open_named(rest, env, interp);
    if (deduce_cmd_matches(name, "block")) return prim_deduce_block(rest, env, interp);
    if (deduce_cmd_matches(name, "commit")) return prim_deduce_commit(rest, env, interp);
    if (deduce_cmd_matches(name, "abort")) return prim_deduce_abort(rest, env, interp);
    if (deduce_cmd_matches(name, "scan")) return prim_deduce_scan(rest, env, interp);
    if (deduce_cmd_matches(name, "scan-range")) return prim_deduce_scan_range(rest, env, interp);
    if (deduce_cmd_matches(name, "query")) return prim_deduce_query(rest, env, interp);
    if (deduce_cmd_matches(name, "schema")) return prim_deduce_schema(rest, env, interp);
    if (deduce_cmd_matches(name, "indexes")) return prim_deduce_indexes(rest, env, interp);
    if (deduce_cmd_matches(name, "stats")) return prim_deduce_stats(rest, env, interp);
    if (deduce_cmd_matches(name, "materialize!")) return prim_deduce_materialize(rest, env, interp);
    if (deduce_cmd_matches(name, "dematerialize!")) return prim_deduce_dematerialize(rest, env, interp);
    if (deduce_cmd_matches(name, "refresh!")) return prim_deduce_refresh(rest, env, interp);
    if (deduce_cmd_matches(name, "count")) return prim_deduce_count(rest, env, interp);
    if (deduce_cmd_matches(name, "match")) return prim_deduce_match(rest, env, interp);
    if (deduce_cmd_matches(name, "fact!")) return prim_assert_fact(rest, env, interp);
    if (deduce_cmd_matches(name, "retract!")) return prim_retract_fact(rest, env, interp);
    if (deduce_cmd_matches(name, "clear!")) return prim_deduce_clear(rest, env, interp);
    if (deduce_cmd_matches(name, "drop!")) return prim_deduce_drop(rest, env, interp);
    if (deduce_cmd_matches(name, "rule!")) return prim_deduce_rule(rest, env, interp);
    if (deduce_cmd_matches(name, "explain")) return prim_deduce_explain(rest, env, interp);
    if (deduce_cmd_matches(name, "why-result")) return prim_deduce_why_result(rest, env, interp);
    if (deduce_cmd_matches(name, "analyze")) return prim_deduce_analyze(rest, env, interp);
    return null;
}

// ============================================================
// (deduce 'cmd args...) — unified dispatch
//
// (deduce 'open 'memory)           → database handle
// (deduce 'open "/path")           → database handle
// (deduce 'open-named db rel-name db-name col...) → relation handle
// (deduce 'block db ['read|'write|'write-deferred]) → transaction handle
// (deduce 'commit txn)             → Void
// (deduce 'abort txn)              → Void
// (deduce 'scan relation)          → list of dicts
// (deduce 'scan-range relation lower-bound upper-bound) → list of dicts
// (deduce 'query relation fn)      → filtered list of dicts
// (deduce 'schema relation)        → relation schema payload
// (deduce 'indexes relation)       → registered logical indexes
// (deduce 'stats relation)         → relation statistics payload
// (deduce 'materialize! relation)  → Void
// (deduce 'dematerialize! relation) → Void
// (deduce 'refresh! db)            → refresh payload
// (deduce 'count relation)         → int
// (deduce 'match relation pattern) → list of dicts
// (deduce 'fact! relation vals...) → Void
// (deduce 'retract! relation vals...)  → Void
// (deduce 'retract! txn relation vals...) → Void
// (deduce 'clear! relation)        → Void
// (deduce 'drop! relation)         → Void
// (deduce 'rule! db head-atom body-atom...) → Void
// (deduce 'explain db [head-symbol|rule-index]) → deterministic plan payload
// (deduce 'why-result relation vals...) → provenance payload for one stored row
// (deduce 'analyze db)             → deterministic evaluator diagnostics payload
// ============================================================
fn Value* prim_deduce(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return deduce_raise(interp, "deduce/dispatch-expected-command", "deduce: expected (deduce 'command args...)");

    Value* cmd = args[0];
    if (cmd == null || cmd.tag != SYMBOL) {
        return deduce_raise(interp, "deduce/dispatch-command-not-symbol", "deduce: first argument must be a quoted symbol");
    }

    char[] name = interp.symbols.get_name(cmd.sym_val);

    Value* out = prim_deduce_dispatch(name, args[1..], env, interp);
    if (out != null) return out;

    return deduce_raise(interp, "deduce/dispatch-unknown-command", "deduce: unknown command (expected 'open 'open-named 'block 'commit 'abort 'scan 'scan-range 'query 'schema 'indexes 'stats 'materialize! 'dematerialize! 'refresh! 'count 'match 'fact! 'retract! 'clear! 'drop! 'rule! 'explain 'why-result 'analyze)");
}
