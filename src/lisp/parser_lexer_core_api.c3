module lisp;

import std::io;

fn bool Lexer.ensure_text_capacity(Lexer* self, usz needed_cap) {
    if (needed_cap <= self.current.text_cap) return true;

    usz new_cap = self.current.text_cap > 0 ? self.current.text_cap : LEXER_TEXT_INITIAL_CAP;
    while (new_cap < needed_cap) {
        if (new_cap > usz.max / 2) {
            new_cap = needed_cap;
            break;
        }
        new_cap *= 2;
    }

    char* new_text = (char*)mem::realloc(self.current.text, new_cap);
    if (new_text == null) return false;
    self.current.text = new_text;
    self.current.text_cap = new_cap;
    return true;
}

fn bool Lexer.push_current_text_char(Lexer* self, char c) @inline {
    usz next_len = self.current.text_len + 1;
    if (!self.ensure_text_capacity(next_len + 1)) return false;
    self.current.text[self.current.text_len] = c;
    self.current.text_len = next_len;
    self.current.text[self.current.text_len] = 0;
    return true;
}

fn bool Lexer.set_error_text(Lexer* self, char[] msg) {
    self.current.type = T_ERROR;
    if (!self.ensure_text_capacity(msg.len + 1)) {
        self.current.text_len = 0;
        return false;
    }
    for (usz i = 0; i < msg.len; i++) self.current.text[i] = msg[i];
    self.current.text[msg.len] = 0;
    self.current.text_len = msg.len;
    return true;
}

fn void Lexer.init(Lexer* self, char[] source) {
    self.source = source;
    self.pos = 0;
    self.len = source.len;
    self.line = 1;
    self.column = 1;
    self.current.text = (char*)mem::malloc(LEXER_TEXT_INITIAL_CAP);
    if (self.current.text == null) {
        self.current.text_cap = 0;
        self.current.text_len = 0;
        self.current.type = T_ERROR;
        return;
    }
    self.current.text_cap = LEXER_TEXT_INITIAL_CAP;
    self.current.text_len = 0;
    self.current.text[0] = 0;
    self.advance();  // Prime the first token
}

fn void Lexer.destroy(Lexer* self) {
    if (self.current.text != null) {
        mem::free(self.current.text);
        self.current.text = null;
    }
    self.current.text_cap = 0;
    self.current.text_len = 0;
}

fn char Lexer.peek(Lexer* self) {
    if (self.pos >= self.len) return 0;
    return self.source[self.pos];
}

fn char Lexer.next_char(Lexer* self) {
    if (self.pos >= self.len) return 0;
    char c = self.source[self.pos++];
    if (c == '\n') {
        self.line++;
        self.column = 1;
    } else {
        self.column++;
    }
    return c;
}

fn void Lexer.set_error_token(Lexer* self) @inline {
    self.current.type = T_ERROR;
    self.current.text_len = 0;
    if (self.current.text != null && self.current.text_cap > 0) self.current.text[0] = 0;
    self.pos++;
}

fn bool Lexer.at_end(Lexer* self) {
    return self.current.type == T_EOF;
}

fn bool Lexer.match(Lexer* self, TokenType t) {
    if (self.current.type == t) {
        self.advance();
        return true;
    }
    return false;
}

fn void Lexer.expect(Lexer* self, TokenType t, char[] msg) {
    if (!self.match(t)) {
        io::printfn("Parse error at line %d, col %d: expected '%s'", self.line, self.column, msg);
        unreachable("parse error: expected token not found");
    }
}
