module lisp;


import std::io;

fn bool deduce_test_write_restart_script(char[] script_path, char[] db_path, bool writer_phase) {
    char[2048] script_buf;
    char[] script;
    if (writer_phase) {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (let (db (deduce 'open \"%s\") "
            "        rel (deduce 'open-named db 'person \"person_v1\" 'name 'age)) "
            "    (deduce 'fact! rel \"Alice\" 30) "
            "    (if (= (deduce 'count rel) 1) true (deduce 'count 42))))",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    } else {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (let (db (deduce 'open \"%s\") "
            "        rel (deduce 'open-named db 'person \"person_v1\" 'name 'age)) "
            "    (if (= (deduce 'count rel) 1) true (deduce 'count 42))))",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    }
    return write_text_file(script_path, script);
}

fn bool deduce_test_write_materialized_restart_script(char[] script_path, char[] db_path, bool writer_phase) {
    char[4096] script_buf;
    char[] script;
    if (writer_phase) {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define [relation db] edge (src) (dst)) "
            "  (define [relation db] ancestor (src) (dst)) "
            "  (deduce 'fact! edge 1 2) "
            "  (deduce/rule! db '(ancestor ?x ?y) '(edge ?x ?y)) "
            "  (deduce/materialize! ancestor) "
            "  true)",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    } else {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define ancestor (deduce 'open-named db 'ancestor \"ancestor\" 'src 'dst)) "
            "  (define schema0 (deduce/schema ancestor)) "
            "  (define analyze (deduce/analyze db)) "
            "  (define refreshed (deduce/refresh! ancestor)) "
            "  (define schema1 (deduce/schema ancestor)) "
            "  (define db-refresh (deduce/refresh! db)) "
            "  (and "
            "    (= (ref schema0 'materialized-view) true) "
            "    (= (ref schema0 'materialized-derived-ready) true) "
            "    (= (ref schema0 'materialized-refresh-policy) 'manual) "
            "    (= (ref schema0 'materialized-refresh-count) 0) "
            "    (= (ref schema0 'materialized-last-stale-reason) 'never-refreshed) "
            "    (= (ref schema0 'materialized-stale) true) "
            "    (= (ref analyze 'materialized-view-count) 1) "
            "    (= (ref analyze 'rule-count) 1) "
            "    (= (ref analyze 'incremental-dependency-edges) 1) "
            "    (= (ref analyze 'ready-materialized-view-count) 1) "
            "    (= (ref analyze 'unready-materialized-view-count) 0) "
            "    (= (ref refreshed 'kind) 'deduce-refresh) "
            "    (= (ref refreshed 'refresh-scope) 'relation) "
            "    (= (ref refreshed 'refresh-execution-path) 'targeted) "
            "    (= (ref refreshed 'refresh-fallback-reason) nil) "
            "    (= (ref refreshed 'requested-refresh-relation) 'ancestor) "
            "    (= (ref refreshed 'refreshed-materialized-view-count) 1) "
            "    (= (car (ref refreshed 'refreshed-materialized-relations)) 'ancestor) "
            "    (= (ref refreshed 'remaining-stale-materialized-relations) nil) "
            "    (= (ref schema1 'materialized-derived-ready) true) "
            "    (= (ref schema1 'materialized-refresh-count) 1) "
            "    (= (ref schema1 'materialized-last-stale-reason) nil) "
            "    (= (ref schema1 'materialized-stale) false) "
            "    (= (ref db-refresh 'kind) 'deduce-refresh) "
            "    (= (ref db-refresh 'refresh-scope) 'database) "
            "    (= (ref db-refresh 'refresh-execution-path) 'db-wide) "
            "    (= (ref db-refresh 'refresh-fallback-reason) nil) "
            "    (= (ref db-refresh 'refreshed-materialized-view-count) 0) "
            "    (= (ref db-refresh 'stale-materialized-view-count) 0) "
            "    (= (ref db-refresh 'refreshed-materialized-relations) nil) "
            "    (= (ref db-refresh 'remaining-stale-materialized-relations) nil)))",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    }
    return write_text_file(script_path, script);
}

fn bool deduce_test_write_materialized_refresh_restart_script(char[] script_path, char[] db_path, bool writer_phase) {
    char[4096] script_buf;
    char[] script;
    if (writer_phase) {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define [relation db] edge (src) (dst)) "
            "  (define [relation db] ancestor (src) (dst)) "
            "  (deduce 'fact! edge 1 2) "
            "  (deduce/rule! db '(ancestor ?x ?y) '(edge ?x ?y)) "
            "  (deduce/materialize! ancestor) "
            "  (deduce/refresh! ancestor) "
            "  true)",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    } else {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define ancestor (deduce 'open-named db 'ancestor \"ancestor\" 'src 'dst)) "
            "  (define schema (deduce/schema ancestor)) "
            "  (define analyze (deduce/analyze db)) "
            "  (and "
            "    (= (deduce 'count ancestor) 1) "
            "    (= (ref schema 'materialized-view) true) "
            "    (= (ref schema 'materialized-derived-ready) true) "
            "    (= (ref schema 'materialized-refresh-policy) 'manual) "
            "    (= (ref schema 'materialized-refresh-count) 1) "
            "    (> (ref schema 'materialized-last-refresh-mutation-epoch) 0) "
            "    (= (ref schema 'materialized-last-stale-reason) nil) "
            "    (= (ref schema 'materialized-stale) false) "
            "    (= (ref analyze 'materialized-view-count) 1) "
            "    (= (ref analyze 'rule-count) 1) "
            "    (= (ref analyze 'incremental-dependency-edges) 1) "
            "    (= (ref analyze 'ready-materialized-view-count) 1) "
            "    (= (ref analyze 'unready-materialized-view-count) 0)))",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    }
    return write_text_file(script_path, script);
}

fn bool deduce_test_write_materialized_stale_restart_script(char[] script_path, char[] db_path, bool writer_phase) {
    char[4096] script_buf;
    char[] script;
    if (writer_phase) {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define [relation db] edge (src) (dst)) "
            "  (define [relation db] ancestor (src) (dst)) "
            "  (deduce 'fact! edge 1 2) "
            "  (deduce/rule! db '(ancestor ?x ?y) '(edge ?x ?y)) "
            "  (deduce/materialize! ancestor) "
            "  (deduce/refresh! ancestor) "
            "  (deduce 'fact! edge 2 3) "
            "  true)",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    } else {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define ancestor (deduce 'open-named db 'ancestor \"ancestor\" 'src 'dst)) "
            "  (define schema (deduce/schema ancestor)) "
            "  (define analyze (deduce/analyze db)) "
            "  (and "
            "    (= (deduce 'count ancestor) 1) "
            "    (= (ref schema 'materialized-view) true) "
            "    (= (ref schema 'materialized-derived-ready) true) "
            "    (= (ref schema 'materialized-refresh-policy) 'manual) "
            "    (= (ref schema 'materialized-refresh-count) 1) "
            "    (> (ref schema 'materialized-last-refresh-mutation-epoch) 0) "
            "    (= (ref schema 'materialized-last-stale-reason) 'dependency-dirty) "
            "    (> (ref schema 'materialized-last-stale-mutation-epoch) "
            "       (ref schema 'materialized-last-refresh-mutation-epoch)) "
            "    (= (ref schema 'materialized-stale) true) "
            "    (= (ref analyze 'materialized-view-count) 1) "
            "    (= (ref analyze 'rule-count) 1) "
            "    (= (ref analyze 'incremental-dependency-edges) 1) "
            "    (= (ref analyze 'ready-materialized-view-count) 1) "
            "    (= (ref analyze 'unready-materialized-view-count) 0) "
            "    (= (ref analyze 'stale-materialized-view-count) 1)))",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    }
    return write_text_file(script_path, script);
}

fn bool deduce_test_write_materialized_on_read_refresh_restart_script(char[] script_path, char[] db_path, bool writer_phase) {
    char[4096] script_buf;
    char[] script;
    if (writer_phase) {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define [relation db] edge (src) (dst)) "
            "  (define [relation db materialized on-read] ancestor (src) (dst)) "
            "  (deduce 'fact! edge 1 2) "
            "  (deduce/rule! db '(ancestor ?x ?y) '(edge ?x ?y)) "
            "  (= (deduce 'count ancestor) 1))",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    } else {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define ancestor (deduce 'open-named db 'ancestor \"ancestor\" 'src 'dst)) "
            "  (define schema (deduce/schema ancestor)) "
            "  (define analyze (deduce/analyze db)) "
            "  (and "
            "    (= (deduce 'count ancestor) 1) "
            "    (= (ref schema 'materialized-view) true) "
            "    (= (ref schema 'materialized-derived-ready) true) "
            "    (= (ref schema 'materialized-refresh-policy) 'on-read) "
            "    (= (ref schema 'materialized-refresh-count) 1) "
            "    (= (ref schema 'materialized-last-stale-reason) nil) "
            "    (= (ref schema 'materialized-stale) false) "
            "    (= (ref analyze 'rule-count) 1) "
            "    (= (ref analyze 'incremental-dependency-edges) 1) "
            "    (= (ref analyze 'ready-materialized-view-count) 1) "
            "    (= (ref analyze 'unready-materialized-view-count) 0)))",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    }
    return write_text_file(script_path, script);
}

fn bool deduce_test_write_materialized_on_read_stale_restart_script(char[] script_path, char[] db_path, bool writer_phase) {
    char[4096] script_buf;
    char[] script;
    if (writer_phase) {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define [relation db] edge (src) (dst)) "
            "  (define [relation db materialized on-read] ancestor (src) (dst)) "
            "  (deduce 'fact! edge 1 2) "
            "  (deduce/rule! db '(ancestor ?x ?y) '(edge ?x ?y)) "
            "  (= (deduce 'count ancestor) 1) "
            "  (deduce 'fact! edge 2 3) "
            "  true)",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    } else {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define ancestor (deduce 'open-named db 'ancestor \"ancestor\" 'src 'dst)) "
            "  (define schema0 (deduce/schema ancestor)) "
            "  (define count1 (deduce 'count ancestor)) "
            "  (define schema1 (deduce/schema ancestor)) "
            "  (define analyze (deduce/analyze db)) "
            "  (and "
            "    (= (ref schema0 'materialized-view) true) "
            "    (= (ref schema0 'materialized-derived-ready) true) "
            "    (= (ref schema0 'materialized-refresh-policy) 'on-read) "
            "    (= (ref schema0 'materialized-refresh-count) 1) "
            "    (= (ref schema0 'materialized-last-stale-reason) 'dependency-dirty) "
            "    (= (ref schema0 'materialized-stale) true) "
            "    (= count1 2) "
            "    (= (ref schema1 'materialized-derived-ready) true) "
            "    (= (ref schema1 'materialized-refresh-count) 2) "
            "    (= (ref schema1 'materialized-last-stale-reason) nil) "
            "    (= (ref schema1 'materialized-stale) false) "
            "    (= (ref analyze 'rule-count) 1) "
            "    (= (ref analyze 'incremental-dependency-edges) 1) "
            "    (= (ref analyze 'ready-materialized-view-count) 1) "
            "    (= (ref analyze 'unready-materialized-view-count) 0) "
            "    (= (ref analyze 'stale-materialized-view-count) 0)))",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    }
    return write_text_file(script_path, script);
}

fn bool deduce_test_write_materialized_drop_restart_script(char[] script_path, char[] db_path, bool writer_phase) {
    char[4096] script_buf;
    char[] script;
    if (writer_phase) {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define [relation db materialized] doomed (src) (dst)) "
            "  (deduce 'drop! doomed) "
            "  true)",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    } else {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define doomed (deduce 'open-named db 'doomed \"doomed\" 'src 'dst)) "
            "  (define schema (deduce/schema doomed)) "
            "  (define analyze (deduce/analyze db)) "
            "  (and "
            "    (= (ref schema 'materialized-view) false) "
            "    (= (ref schema 'materialized-derived-ready) false) "
            "    (= (ref analyze 'materialized-view-count) 0) "
            "    (= (ref analyze 'ready-materialized-view-count) 0) "
            "    (= (ref analyze 'unready-materialized-view-count) 0)))",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    }
    return write_text_file(script_path, script);
}

fn bool deduce_test_write_materialized_dematerialize_restart_script(char[] script_path, char[] db_path, bool writer_phase) {
    char[4096] script_buf;
    char[] script;
    if (writer_phase) {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define [relation db] edge (src) (dst)) "
            "  (define [relation db] ancestor (src) (dst)) "
            "  (deduce 'fact! edge 1 2) "
            "  (deduce/rule! db '(ancestor ?x ?y) '(edge ?x ?y)) "
            "  (deduce/materialize! ancestor) "
            "  (deduce/refresh! ancestor) "
            "  (deduce/dematerialize! ancestor) "
            "  true)",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    } else {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define ancestor (deduce 'open-named db 'ancestor \"ancestor\" 'src 'dst)) "
            "  (define schema (deduce/schema ancestor)) "
            "  (define analyze (deduce/analyze db)) "
            "  (define payload (handle (deduce/refresh! ancestor) (raise msg msg))) "
            "  (and "
            "    (= (ref schema 'materialized-view) false) "
            "    (= (ref schema 'materialized-derived-ready) false) "
            "    (= (ref schema 'materialized-refresh-policy) nil) "
            "    (= (ref schema 'materialized-refresh-count) 0) "
            "    (= (ref schema 'materialized-last-stale-reason) nil) "
            "    (= (ref schema 'materialized-stale) false) "
            "    (= (ref analyze 'materialized-view-count) 0) "
            "    (= (ref analyze 'ready-materialized-view-count) 0) "
            "    (= (ref analyze 'unready-materialized-view-count) 0) "
            "    (= (dict? payload) true) "
            "    (= (ref payload 'code) 'deduce/refresh-relation-not-materialized)))",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    }
    return write_text_file(script_path, script);
}

fn bool deduce_test_run_main_script(char[] script_path) {
    char[1024] cmd_buf;
    char[] cmd;
    if (try char[] rendered = io::bprintf(
        &cmd_buf,
        "ASAN_OPTIONS=detect_leaks=0:halt_on_error=1:abort_on_error=1 "
        "LD_LIBRARY_PATH=/usr/local/lib ./build/main %s",
        (ZString)script_path
    )) {
        cmd = rendered;
    } else {
        return false;
    }
    cmd_buf[cmd.len] = 0;
    return deduce_test_system(&cmd_buf[0]) == 0;
}

fn bool deduce_test_write_materialized_selective_rule_install_restart_script(char[] script_path, char[] db_path, bool writer_phase) {
    char[4096] script_buf;
    char[] script;
    if (writer_phase) {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define [relation db materialized] pending (src) (dst)) "
            "  (define [relation db] edge (src) (dst)) "
            "  (define [relation db] ancestor (src) (dst)) "
            "  (deduce 'fact! edge 1 2) "
            "  (deduce/rule! db '(ancestor ?x ?y) '(edge ?x ?y)) "
            "  (deduce/materialize! ancestor) "
            "  (deduce/refresh! ancestor) "
            "  (deduce/rule! db '(ancestor ?x ?z) '(edge ?x ?y) '(ancestor ?y ?z)) "
            "  true)",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    } else {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define pending (deduce 'open-named db 'pending \"pending\" 'src 'dst)) "
            "  (define ancestor (deduce 'open-named db 'ancestor \"ancestor\" 'src 'dst)) "
            "  (define pending-schema (deduce/schema pending)) "
            "  (define ancestor-schema (deduce/schema ancestor)) "
            "  (define analyze (deduce/analyze db)) "
            "  (and "
            "    (= (ref pending-schema 'materialized-view) true) "
            "    (= (ref pending-schema 'materialized-derived-ready) false) "
            "    (= (ref pending-schema 'materialized-last-stale-reason) 'never-refreshed) "
            "    (= (ref pending-schema 'materialized-stale) true) "
            "    (= (ref ancestor-schema 'materialized-view) true) "
            "    (= (ref ancestor-schema 'materialized-derived-ready) true) "
            "    (= (ref ancestor-schema 'materialized-last-stale-reason) 'rule-set-change) "
            "    (= (ref ancestor-schema 'materialized-stale) true) "
            "    (= (ref analyze 'ready-materialized-view-count) 1) "
            "    (= (ref analyze 'unready-materialized-view-count) 1) "
            "    (= (ref analyze 'stale-materialized-view-count) 2)))",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    }
    return write_text_file(script_path, script);
}
