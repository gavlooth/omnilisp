module pika;

import lisp;

fn bool register_pika_primitives(lisp::Interp* interp) {
    // Register regex primitives
    if (!lisp::register_prim(interp, "re-match", &prim_re_match, 2)) return false;
    if (!lisp::register_prim(interp, "re-fullmatch", &prim_re_fullmatch, 2)) return false;
    if (!lisp::register_prim(interp, "re-find-all", &prim_re_find_all, 2)) return false;
    if (!lisp::register_prim(interp, "re-split", &prim_re_split, 2)) return false;
    if (!lisp::register_prim(interp, "re-replace", &prim_re_replace, -1)) return false;  // 3-4 args
    if (!lisp::register_prim(interp, "re-match-pos", &prim_re_match_pos, 2)) return false;
    if (!lisp::register_prim(interp, "re-find-all-pos", &prim_re_find_all_pos, 2)) return false;

    // Register grammar primitives
    if (!lisp::register_prim(interp, "pika/grammar", &prim_pika_grammar, -1)) return false;  // variadic
    if (!lisp::register_prim(interp, "pika/parse", &prim_pika_parse, 2)) return false;
    if (!lisp::register_prim(interp, "pika/fold", &prim_pika_fold, 3)) return false;
    if (!lisp::register_prim(interp, "pika/grammar-rules", &prim_pika_grammar_rules, 1)) return false;
    if (!lisp::register_prim(interp, "pika/match-span", &prim_pika_match_span, 3)) return false;

    // Register Lisp parsing primitives
    if (!lisp::register_prim(interp, "pika/parse-lisp", &prim_pika_parse_lisp, 1)) return false;
    return lisp::register_prim(interp, "pika/eval-lisp", &prim_pika_eval_lisp, 1);
}
