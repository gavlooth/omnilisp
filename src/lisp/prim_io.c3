module lisp;

fn Value* prim_print(Value*[] args, Env* env, Interp* interp) {
    if (args.len >= 1) {
        return io_console_emit(args[0], false, false, interp);
    }
    return make_void(interp);
}

fn Value* prim_println(Value*[] args, Env* env, Interp* interp) {
    if (args.len >= 1) {
        return io_console_emit(args[0], false, true, interp);
    }
    return make_void(interp);
}

fn Value* prim_newline(Value*[] args, Env* env, Interp* interp) {
    return io_console_emit(null, false, true, interp);
}

fn Value* prim_display(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "display: expected argument");
    return io_console_emit(args[0], true, false, interp);
}

fn Value* prim_read_line(Value*[] args, Env* env, Interp* interp) {
    if (args.len > 1 || (args.len == 1 && !is_nil(args[0]))) {
        return io_raise(interp, "io/read-line-expected-no-args", "read-line: expected (read-line)");
    }

    char[IO_READ_LINE_BUFFER_MAX] line_buf = {};
    IoInputReadResult read = io_runtime_read_line(line_buf[..], interp);
    switch (read.status) {
        case IO_INPUT_READ_OK:
            return make_string(interp, line_buf[:read.len]);
        case IO_INPUT_READ_EOF:
            return make_nil(interp);
        case IO_INPUT_READ_TRUNCATED:
            return io_raise(interp, "io/read-line-too-long", "read-line: input line exceeds supported length");
        case IO_INPUT_READ_INTERRUPTED:
            return interp_interrupt_error(interp);
        default:
            return io_raise(interp, "io/read-line-failed", "read-line: failed to read input");
    }
}
