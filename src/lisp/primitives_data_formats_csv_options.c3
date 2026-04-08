module lisp;

import std::core::mem;
import std::io;
import main;

struct CsvFieldBuf {
    char* data;
    usz   len;
    usz   cap;
}

struct CsvParseOptions {
    char delimiter;
    bool strict;
}

const int CSV_QUOTE_MINIMAL = 0;
const int CSV_QUOTE_ALWAYS = 1;
const int CSV_QUOTE_NONE = 2;

struct CsvEmitOptions {
    char delimiter;
    char[] line_ending;
    char quote_char;
    int quote_style;
    char[] nil_as;
    bool strict;
}

fn bool csv_is_symbol_name(Value* v, char[] name, Interp* interp) {
    if (v == null || v.tag != SYMBOL) return false;
    char[] sym_name = interp.symbols.get_name(v.sym_val);
    if (sym_name.len != name.len) return false;
    for (usz i = 0; i < name.len; i++) {
        if (sym_name[i] != name[i]) return false;
    }
    return true;
}

fn Value* parse_csv_char_option(
    Value* value,
    char* out_char,
    char[] option_name,
    Interp* interp,
    char[] context
) {
    if (value == null || value.tag != STRING || value.str_len != 1) {
        if (string_equals(context, "csv-parse") && string_equals(option_name, "delimiter")) {
            return raise_error(interp, "csv-parse: delimiter must be a one-character string");
        }
        if (string_equals(context, "csv-emit") && string_equals(option_name, "delimiter")) {
            return raise_error(interp, "csv-emit: delimiter must be a one-character string");
        }
        if (string_equals(context, "csv-emit") && string_equals(option_name, "quote-char")) {
            return raise_error(interp, "csv-emit: quote-char must be a one-character string");
        }

        return raise_error(interp, "option must be a one-character string");
    }

    *out_char = value.str_chars[0];
    return null;
}

fn Value* parse_csv_bool_option(
    Value* value,
    bool* out_value,
    char[] option_name,
    Interp* interp,
    char[] context
) {
    if (is_nil(value)) {
        *out_value = false;
        return null;
    }
    if (is_symbol(value) && value.sym_val == interp.sym_true) {
        *out_value = true;
        return null;
    }
    if (is_symbol(value) && value.sym_val == interp.sym_false) {
        *out_value = false;
        return null;
    }

    char[200] msg_buf;
    char[] msg;
    if (try char[] formatted = io::bprintf(&msg_buf, "%s: %s must be a boolean (true/false)", context, option_name)) {
        msg = formatted;
    } else {
        msg = "csv-emit: option must be a boolean (true/false)";
    }
    return raise_error(interp, msg);
}

fn Value* parse_csv_line_ending(Value* value, char[]* out_line_ending, Interp* interp) {
    if (value == null || value.tag != STRING) {
        return raise_error(interp, "csv-emit: line-ending must be a string");
    }

    if (value.str_len == 1 && (value.str_chars[0] == '\n' || value.str_chars[0] == '\r')) {
        *out_line_ending = value.str_chars[:value.str_len];
        return null;
    }

    if (value.str_len == 2 && value.str_chars[0] == '\r' && value.str_chars[1] == '\n') {
        *out_line_ending = value.str_chars[:value.str_len];
        return null;
    }

    return raise_error(interp, "csv-emit: line-ending must be \"\\n\", \"\\r\", or \"\\r\\n\"");
}

fn Value* parse_csv_quote_style_option(Value* value, int* out_style, Interp* interp) {
    if (value == null || value.tag != SYMBOL) {
        if (is_cons(value)) {
            Value* quote_sym = car(value);
            if (!is_nil(quote_sym) && is_symbol(quote_sym) && csv_is_symbol_name(quote_sym, "quote", interp)) {
                Value* quote_arg = cdr(value);
                if (!is_cons(quote_arg) || !is_nil(cdr(quote_arg))) {
                    return raise_error(interp, "csv-emit: quote-style must be one of 'minimal, 'always, or 'none");
                }
                value = car(quote_arg);
            }
        }
    }
    if (value == null || value.tag != SYMBOL) {
        return raise_error(interp, "csv-emit: quote-style must be one of 'minimal, 'always, or 'none");
    }

    if (csv_is_symbol_name(value, "minimal", interp)) {
        *out_style = CSV_QUOTE_MINIMAL;
        return null;
    }
    if (csv_is_symbol_name(value, "always", interp)) {
        *out_style = CSV_QUOTE_ALWAYS;
        return null;
    }
    if (csv_is_symbol_name(value, "none", interp)) {
        *out_style = CSV_QUOTE_NONE;
        return null;
    }

    return raise_error(interp, "csv-emit: quote-style must be one of 'minimal, 'always, or 'none");
}

fn Value* parse_csv_parse_option_entry(Value* key, Value* value, CsvParseOptions* opts, Interp* interp) {
    if (csv_is_symbol_name(key, "delimiter", interp)) {
        return parse_csv_char_option(value, &opts.delimiter, "delimiter", interp, "csv-parse");
    }
    if (csv_is_symbol_name(key, "strict", interp)) {
        return parse_csv_bool_option(value, &opts.strict, "strict", interp, "csv-parse");
    }
    return raise_error(interp, "csv-parse: unknown option key");
}

fn Value* parse_csv_emit_option_entry(Value* key, Value* value, CsvEmitOptions* opts, Interp* interp) {
    if (csv_is_symbol_name(key, "delimiter", interp)) {
        return parse_csv_char_option(value, &opts.delimiter, "delimiter", interp, "csv-emit");
    }
    if (csv_is_symbol_name(key, "line-ending", interp)) {
        return parse_csv_line_ending(value, &opts.line_ending, interp);
    }
    if (csv_is_symbol_name(key, "quote-char", interp)) {
        return parse_csv_char_option(value, &opts.quote_char, "quote-char", interp, "csv-emit");
    }
    if (csv_is_symbol_name(key, "quote-style", interp)) {
        return parse_csv_quote_style_option(value, &opts.quote_style, interp);
    }
    if (csv_is_symbol_name(key, "nil-as", interp)) {
        if (value == null || value.tag == NIL) {
            opts.nil_as = "";
        } else if (value.tag == STRING) {
            opts.nil_as = value.str_chars[:value.str_len];
        } else if (is_symbol(value)) {
            opts.nil_as = interp.symbols.get_name(value.sym_val);
        } else {
            return raise_error(interp, "csv-emit: nil-as must be a string or symbol");
        }
        return null;
    }
    if (csv_is_symbol_name(key, "strict", interp)) {
        return parse_csv_bool_option(value, &opts.strict, "strict", interp, "csv-emit");
    }
    return raise_error(interp, "csv-emit: unknown option key");
}

fn bool csv_char_is_line_ending(char ch) {
    return ch == '\n' || ch == '\r';
}
