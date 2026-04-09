module pika;

// ============================================================
// Lisp Grammar for PikaParser
// Matches current syntax: parentheses for lists, ^Type annotations,
// [x y z] for arrays, {:key val} for dicts, 'x for quote
// ============================================================

// ============================================================
// SCAN Functions for Token Matching
// Each returns 0 if no match, else the number of characters matched
// ============================================================

// Skip whitespace and comments - returns number of characters consumed
// Note: returns 0 if no whitespace, which the parser treats as "no match"
fn int scan_ws(char[] view) {
    int i = 0;
    while (i < (int)view.len) {
        char c = view[i];
        if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
            i++;
        } else if (c == ';') {
            // Skip comment to end of line
            while (i < (int)view.len && view[i] != '\n') {
                i++;
            }
        } else {
            break;
        }
    }
    return i;
}

// Scan exactly one whitespace character (for MANY wrapper)
fn int scan_ws_char(char[] view) {
    if (view.len == 0) return 0;
    char c = view[0];
    if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
        return 1;
    }
    if (c == ';') {
        // Skip entire comment as one "character"
        int i = 1;
        while (i < (int)view.len && view[i] != '\n') {
            i++;
        }
        return i;
    }
    return 0;
}

// Match integer: optional minus followed by digits
fn int scan_int(char[] view) {
    int i = 0;

    // Optional minus
    if (i < (int)view.len && view[i] == '-') {
        if (i + 1 >= (int)view.len || view[i + 1] < '0' || view[i + 1] > '9') {
            return 0; // Just '-' is not an integer
        }
        i++;
    }

    // Require at least one digit
    if (i >= (int)view.len || view[i] < '0' || view[i] > '9') {
        return 0;
    }

    while (i < (int)view.len && view[i] >= '0' && view[i] <= '9') {
        i++;
    }

    // Check not followed by decimal point with digit (that's a float)
    if (i < (int)view.len && view[i] == '.') {
        if (i + 1 < (int)view.len && view[i + 1] >= '0' && view[i + 1] <= '9') {
            return 0; // This is a float, not an int
        }
    }

    // Check not followed by 'e' or 'E' (that's a float)
    if (i < (int)view.len && (view[i] == 'e' || view[i] == 'E')) {
        return 0;
    }

    return i;
}

// Match float: int part, decimal point, fractional part, optional exponent
fn int scan_float(char[] view) {
    int i = 0;

    // Optional minus
    if (i < (int)view.len && view[i] == '-') {
        i++;
    }

    // Integer part (required)
    if (i >= (int)view.len || view[i] < '0' || view[i] > '9') {
        return 0;
    }
    while (i < (int)view.len && view[i] >= '0' && view[i] <= '9') {
        i++;
    }

    bool has_decimal = false;
    bool has_exp = false;

    // Decimal point and fractional part
    if (i < (int)view.len && view[i] == '.') {
        if (i + 1 < (int)view.len && view[i + 1] >= '0' && view[i + 1] <= '9') {
            has_decimal = true;
            i++; // consume '.'
            while (i < (int)view.len && view[i] >= '0' && view[i] <= '9') {
                i++;
            }
        }
    }

    // Exponent part (e.g., e-5, E10)
    if (i < (int)view.len && (view[i] == 'e' || view[i] == 'E')) {
        int exp_start = i;
        i++;
        if (i < (int)view.len && (view[i] == '+' || view[i] == '-')) {
            i++;
        }
        if (i < (int)view.len && view[i] >= '0' && view[i] <= '9') {
            has_exp = true;
            while (i < (int)view.len && view[i] >= '0' && view[i] <= '9') {
                i++;
            }
        } else {
            // Invalid exponent, rollback
            i = exp_start;
        }
    }

    // Must have either decimal or exponent to be a float
    if (!has_decimal && !has_exp) {
        return 0;
    }

    return i;
}

// Match string: "..." with escape sequences
fn int scan_string(char[] view) {
    if (view.len == 0 || view[0] != '"') return 0;

    int i = 1;
    while (i < (int)view.len) {
        char c = view[i];
        if (c == '"') {
            return i + 1; // Include closing quote
        }
        if (c == '\\' && i + 1 < (int)view.len) {
            i += 2; // Skip escape sequence
        } else {
            i++;
        }
    }

    return 0; // Unterminated string
}

// Helper: is this a symbol character?
fn bool is_sym_char(char c) {
    if (c >= 'a' && c <= 'z') return true;
    if (c >= 'A' && c <= 'Z') return true;
    if (c >= '0' && c <= '9') return true;
    if (c == '_' || c == '-' || c == '+' || c == '*' || c == '/' ||
        c == '=' || c == '<' || c == '>' || c == '!' || c == '?' ||
        c == ':' || c == '.' || c == '@' || c == '#' || c == '$' ||
        c == '%' || c == '&' || c == '|' || c == '~') {
        return true;
    }
    return false;
}

// Match symbol (not containing dots except as part of path)
fn int scan_symbol(char[] view) {
    if (view.len == 0) return 0;

    char c = view[0];
    // Symbol can't start with a digit, or with just '.'
    if (c >= '0' && c <= '9') return 0;
    if (!is_sym_char(c)) return 0;

    int i = 0;
    bool has_dot = false;
    while (i < (int)view.len && is_sym_char(view[i])) {
        if (view[i] == '.' &&
            i + 1 < (int)view.len &&
            view[i + 1] == '[') {
            break;
        }
        if (view[i] == '.' &&
            i + 1 < (int)view.len &&
            view[i + 1] == '.') {
            break;
        }
        if (view[i] == '.') has_dot = true;
        i++;
    }

    // If it has a dot, reject (that's a path)
    // Unless it's just ".." which is handled separately
    if (has_dot && i > 2) return 0;
    if (has_dot && i <= 2) {
        // Could be "." or ".." - let dotdot handle ".."
        if (i == 2 && view[0] == '.' && view[1] == '.') return 0;
    }

    return i;
}

// Match path: symbol.symbol.symbol
fn int scan_path(char[] view) {
    if (view.len == 0) return 0;

    char c = view[0];
    if (c >= '0' && c <= '9') return 0;
    if (!is_sym_char(c)) return 0;
    if (c == '.') return 0; // Path can't start with dot

    int i = 0;
    bool has_internal_dot = false;
    int last_dot = -1;

    while (i < (int)view.len && is_sym_char(view[i])) {
        if (view[i] == '.' &&
            i + 1 < (int)view.len &&
            view[i + 1] == '[') {
            break;
        }
        if (view[i] == '.' &&
            i + 1 < (int)view.len &&
            view[i + 1] == '.') {
            break;
        }
        if (view[i] == '.') {
            has_internal_dot = true;
            last_dot = i;
        }
        i++;
    }

    // Must have internal dot, not at start or end
    if (!has_internal_dot) return 0;
    if (last_dot == i - 1) return 0; // Ends with dot

    return i;
}

// Match dotdot: .. followed by whitespace/delimiter or EOF
fn int scan_dotdot(char[] view) {
    if (view.len < 2) return 0;
    if (view[0] != '.' || view[1] != '.') return 0;

    // Check what follows
    if (view.len == 2) return 2;

    char next = view[2];
    if (next == ' ' || next == '\t' || next == '\n' || next == '\r' ||
        next == ')' || next == ']' || next == '}') {
        return 2;
    }

    return 0;
}
