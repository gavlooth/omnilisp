module lisp;

import std::io;

const usz ASYNC_IO_TCP_READ_DEFAULT_MAX_BYTES = 4096;
const usz ASYNC_IO_TCP_READ_HARD_MAX_BYTES = 65536;
const usz ASYNC_IO_HTTP_GET_REQUEST_BUFFER_BYTES = 4096;
const usz ASYNC_IO_HTTP_REQUEST_BUFFER_BYTES = 8192;
const usz ASYNC_IO_HTTP_RESPONSE_BUFFER_BYTES = 65536;
const usz ASYNC_IO_HTTP_RESPONSE_READ_MAX_BYTES = 65000;

fn Value* async_io_raise_fiber_required(Interp* interp, char[] op_name, char[] code, char[] fallback_message) {
    char[96] ebuf;
    if (try char[] msg = io::bprintf(&ebuf, "%s: requires running fiber context", (ZString)op_name)) {
        return io_raise(interp, code, msg);
    }
    return io_raise(interp, code, fallback_message);
}

fn Value* async_io_raise_loop_unavailable(Interp* interp, char[] op_name, char[] code, char[] fallback_message) {
    char[96] ebuf;
    if (try char[] msg = io::bprintf(&ebuf, "%s: libuv loop unavailable", (ZString)op_name)) {
        return io_raise(interp, code, msg);
    }
    return io_raise(interp, code, fallback_message);
}
