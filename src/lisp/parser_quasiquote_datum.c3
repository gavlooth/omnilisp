module lisp;

import std::collections::list;

fn Expr* Parser.parse_qq_template(Parser* self) {
    if (self.has_error) return null;
    switch (self.lexer.current.type) {
        case T_BACKTICK:
            return self.parse_qq_backtick();
        case T_COMMA:
            return self.parse_qq_unquote();
        case T_COMMA_AT:
            return self.parse_qq_unquote_splicing();
        case T_QUOTE:
            return self.parse_qq_quote_datum();
        case T_INT:
            return self.parse_qq_int_literal();
        case T_STRING:
            return self.parse_qq_string_literal();
        case T_SYMBOL:
            return self.parse_qq_var_symbol(self.get_current_symbol());
        case T_LPAREN:
            return self.parse_qq_list_form();
        case T_UNDERSCORE:
            return self.parse_qq_var_symbol(self.intern_or_error("_", "parser: failed to intern quasiquote underscore symbol"));
        default:
            self.set_error("unexpected token in quasiquote template");
            return null;
    }
}
