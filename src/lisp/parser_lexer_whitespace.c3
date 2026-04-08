module lisp;

fn void Lexer.skip_nested_block_comment(Lexer* self) @inline {
    // Block comment: #| ... |# (nestable)
    self.next_char();  // consume #
    self.next_char();  // consume |
    int depth = 1;
    while (self.pos < self.len && depth > 0) {
        if (self.source[self.pos] == '#' && self.pos + 1 < self.len && self.source[self.pos + 1] == '|') {
            depth++;
            self.next_char();
            self.next_char();
            continue;
        }
        if (self.source[self.pos] == '|' && self.pos + 1 < self.len && self.source[self.pos + 1] == '#') {
            depth--;
            self.next_char();
            self.next_char();
            continue;
        }
        self.next_char();
    }
}

fn void Lexer.skip_whitespace(Lexer* self) {
    while (self.pos < self.len) {
        char c = self.source[self.pos];
        if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
            self.next_char();  // Track line/column
            continue;
        }
        if (c == ';') {
            // Line comment: skip to end of line.
            while (self.pos < self.len && self.source[self.pos] != '\n') {
                self.next_char();
            }
            continue;
        }
        if (c == '#' && self.pos + 1 < self.len && self.source[self.pos + 1] == '|') {
            self.skip_nested_block_comment();
            continue;
        }
        break;
    }
}
