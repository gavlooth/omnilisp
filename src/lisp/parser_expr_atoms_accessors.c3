module lisp;

import std::collections::list;

fn Expr* Parser.parse_path_expr(Parser* self) {
    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
    e.tag = E_PATH;
    e.path.segment_count = 0;
    e.path.segments = null;

    List{SymbolId} segments;
    usz text_len = self.lexer.current.text_len;
    usz start = 0;
    for (usz i = 0; i <= text_len; i++) {
        if (i == text_len || self.lexer.current.text[i] == '.') {
            if (i > start) {
                usz seg_len = i - start;
                SymbolId seg_sym = self.interp.symbols.intern(self.lexer.current.text[start:seg_len]);
                segments.push(seg_sym);
            }
            start = i + 1;
        }
    }
    if (segments.len() == 0) {
        segments.free();
        self.set_error("empty path expression");
        return null;
    }
    e.path.segment_count = segments.len();
    e.path.segments = (SymbolId*)self.alloc_ast_bytes(SymbolId.sizeof * segments.len(), "out of memory while parsing path expression");
    if (e.path.segments == null) {
        segments.free();
        return null;
    }
    for (usz i = 0; i < segments.len(); i++) {
        e.path.segments[i] = segments[i];
    }
    segments.free();
    self.lexer.advance();
    return self.parse_postfix_index(e);
}
