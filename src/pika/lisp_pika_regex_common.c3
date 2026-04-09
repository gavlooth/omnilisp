module pika;

import lisp;

fn lisp::Value* regex_raise(lisp::Interp* interp, char[] code, char[] message, lisp::Value* data = null) @inline {
    return lisp::raise_error_with_payload_names(interp, "regex", code, message, data);
}

fn lisp::Value* parser_raise(lisp::Interp* interp, char[] code, char[] message, lisp::Value* data = null) @inline {
    return lisp::raise_error_with_payload_names(interp, "parser", code, message, data);
}

fn lisp::Value* regex_raise_invalid_pattern_if_any(lisp::Interp* interp, char[] code, char[] message) {
    char[] detail;
    if (!regex_take_last_compile_error(&detail)) return null;
    lisp::Value* data = detail.len > 0 ? lisp::make_string(interp, detail) : null;
    return regex_raise(interp, code, message, data);
}
