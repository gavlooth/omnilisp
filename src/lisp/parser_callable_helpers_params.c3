module lisp;

import std::io;
import std::collections::list;
import main;

fn bool Parser.collect_callable_params(Parser* self, List{SymbolId}* params, List{TypeAnnotation}* param_anns, bool* has_any_typed, List{Pattern*}* destr_patterns, List{SymbolId}* destr_param_names) {
    while (self.lexer.current.type == T_SYMBOL || self.lexer.current.type == T_LPAREN || self.lexer.current.type == T_LBRACE || self.lexer.current.type == T_LBRACKET) {
        if (self.lexer.current.type == T_LBRACE || self.lexer.current.type == T_LBRACKET) {
            Pattern* dp = self.parse_pattern();
            char[32] gensym_buf;
            if (try char[] gensym_text = io::bprintf(&gensym_buf, "__d%d", (int)(*destr_patterns).len())) {
                SymbolId gensym = self.intern_or_error(gensym_text, "failed to intern destructuring parameter name");
                if (gensym == INVALID_SYMBOL_ID) return false;
                params.push(gensym);
                TypeAnnotation empty_ann;
                empty_ann.has_annotation = false;
                param_anns.push(empty_ann);
                destr_patterns.push(dp);
                destr_param_names.push(gensym);
            } else {
                self.set_error("failed to format destructuring parameter name");
                return false;
            }
        } else if (self.lexer.current.type == T_LPAREN) {
            self.lexer.advance();  // consume '('
            TypeAnnotation ann = self.parse_type_annotation();
            if (!ann.has_annotation) {
                self.set_error("expected type annotation in typed parameter");
                return false;
            }
            if (self.lexer.current.type != T_SYMBOL) {
                self.set_error("expected parameter name after type annotation");
                return false;
            }
            params.push(self.get_current_symbol());
            param_anns.push(ann);
            *has_any_typed = true;
            self.lexer.advance();
            self.expect(T_RPAREN, ")");
            if (self.has_error) return false;
        } else {
            params.push(self.get_current_symbol());
            TypeAnnotation empty_ann;
            empty_ann.has_annotation = false;
            param_anns.push(empty_ann);
            self.lexer.advance();
        }

        if (self.lexer.current.type == T_DOTDOT || self.lexer.current.type == T_RPAREN) {
            break;
        }
    }
    return !self.has_error;
}
