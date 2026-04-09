module pika;

fn int compile_regex_atom(RegexCompiler* rc) {
    int saved_pos = rc.tok.pos;
    RegexToken tok = rc.tok.next_token();

    switch (tok.type) {
        case RE_LITERAL:
            return rc.rb.add(regex_gen_name(rc, "lit"), mk_token(tok.ch));

        case RE_DOT:
            return rc.rb.add(regex_gen_name(rc, "dot"), mk_scan(&regex_scan_dot));

        case RE_CHAR_CLASS:
            return rc.rb.add(regex_gen_name(rc, "cls"), mk_scan_ctx(&regex_scan_char_class_ctx, (void*)tok.cclass));

        case RE_LPAREN:
            int inner = compile_regex_alternation(rc);
            RegexToken close = rc.tok.next_token();
            if (close.type != RE_RPAREN) {
                rc.tok.set_error("Unclosed group");
                return rc.rb.add("_err", mk_fail());
            }
            return inner;

        case RE_NONCAP:
            int inner2 = compile_regex_alternation(rc);
            RegexToken close2 = rc.tok.next_token();
            if (close2.type != RE_RPAREN) {
                rc.tok.set_error("Unclosed group");
                return rc.rb.add("_err", mk_fail());
            }
            return inner2;

        case RE_LOOKAHEAD_POS:
            int inner3 = compile_regex_alternation(rc);
            RegexToken close3 = rc.tok.next_token();
            if (close3.type != RE_RPAREN) {
                rc.tok.set_error("Unclosed group");
                return rc.rb.add("_err", mk_fail());
            }
            return rc.rb.add(regex_gen_name(rc, "la"), mk_followed_by(inner3));

        case RE_LOOKAHEAD_NEG:
            int inner4 = compile_regex_alternation(rc);
            RegexToken close4 = rc.tok.next_token();
            if (close4.type != RE_RPAREN) {
                rc.tok.set_error("Unclosed group");
                return rc.rb.add("_err", mk_fail());
            }
            return rc.rb.add(regex_gen_name(rc, "nla"), mk_not_followed_by(inner4));

        case RE_ANCHOR_END:
            return rc.rb.add("_end", mk_end_of_input());

        case RE_ERROR:
            return rc.rb.add(regex_gen_name(rc, "err"), mk_fail());

        default:
            rc.tok.pos = saved_pos;
            return -1;
    }
}
