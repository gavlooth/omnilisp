module lisp;

import std::core::mem;
import std::io;
import main;

fn void repl_server_capture_reset(Interp* interp) {
    interp.console_capture_len = 0;
    if (interp.console_capture_buf != null) interp.console_capture_buf[0] = 0;
}

fn char[] repl_server_capture_text(Interp* interp) {
    if (interp.console_capture_buf == null || interp.console_capture_len == 0) return "";
    return interp.console_capture_buf[:interp.console_capture_len];
}

fn bool repl_server_eval_result_interrupted(EvalResult result) @inline {
    return result.error.has_error && json_repl_str_eq(json_repl_error_message(result), "evaluation interrupted");
}

fn bool repl_server_send_eval_result(ReplServerOutput* output, char[] id, char[] session, EvalResult result, Interp* interp) {
    char[] captured = repl_server_capture_text(interp);
    if (captured.len > 0 && !repl_server_send_out_event(output, id, session, captured)) return false;

    if (repl_server_eval_result_interrupted(result)) {
        return repl_server_send_interrupted_event(output, id, session);
    }

    if (result.error.has_error) {
        return repl_server_send_event_error(
            output,
            id,
            session,
            "runtime/eval-error",
            json_repl_error_message(result),
            true,
            json_repl_zero_based(result.error.line),
            json_repl_zero_based(result.error.column)
        );
    }

    char* rendered = null;
    usz rendered_len = 0;
    if (!json_repl_render_value(result.value, &interp.symbols, &rendered, &rendered_len)) {
        return repl_server_send_event_error(
            output,
            id,
            session,
            "runtime/render-failed",
            "failed to render evaluation result"
        );
    }
    defer mem::free(rendered);

    if (!repl_server_send_value_event(output, id, session, rendered[:rendered_len])) return false;
    return repl_server_send_done(output, id, session);
}

fn bool repl_server_preload_project_session(ReplServerOutput* output, ReplServerSession* session, char[] id, char* project_dir) {
    char[1280] main_path_buf = {};
    usz main_path_len = 0;
    if (!main::repl_resolve_project_main_path(project_dir, main_path_buf[..], &main_path_len)) {
        return repl_server_send_event_error(
            output,
            id,
            session.id_ptr[:session.id_len],
            "runtime/project-preload-failed",
            "failed to resolve Omni project entry"
        );
    }

    char[] path = main_path_buf[:main_path_len];
    if (try source = io::file::load_temp((String)path)) {
        Interp* interp = session.interp;
        if (!push_source_dir(path, interp)) {
            return repl_server_send_event_error(
                output,
                id,
                session.id_ptr[:session.id_len],
                "runtime/source-dir-allocation-failed",
                "project preload: failed to allocate source directory context"
            );
        }
        defer pop_source_dir(interp);

        interp.console_capture_active = true;
        repl_server_capture_reset(interp);
        EvalResult result = run_program(source, interp);
        interp.console_capture_active = false;

        char[] captured = repl_server_capture_text(interp);
        if (captured.len > 0 && !repl_server_send_out_event(output, id, session.id_ptr[:session.id_len], captured)) {
            return false;
        }
        if (repl_server_eval_result_interrupted(result)) {
            return repl_server_send_interrupted_event(output, id, session.id_ptr[:session.id_len]);
        }
        if (result.error.has_error) {
            return repl_server_send_event_error(
                output,
                id,
                session.id_ptr[:session.id_len],
                "runtime/project-preload-failed",
                json_repl_error_message(result),
                true,
                json_repl_zero_based(result.error.line),
                json_repl_zero_based(result.error.column)
            );
        }
        return true;
    }

    main::EntryFileReadKind read_kind;
    if (!main::classify_file_read_path(path, &read_kind)) {
        char[128] msg_buf = {};
        char[] message = "project preload: read failed";
        if (try rendered = io::bprintf(
            &msg_buf,
            "project preload: %s",
            (ZString)main::file_read_kind_reason(read_kind)
        )) {
            message = rendered;
        }
        return repl_server_send_event_error(
            output,
            id,
            session.id_ptr[:session.id_len],
            main::file_read_kind_code(read_kind),
            message
        );
    }

    return repl_server_send_event_error(
        output,
        id,
        session.id_ptr[:session.id_len],
        "io/read-failed",
        "project preload: read failed"
    );
}

fn bool repl_server_eval_session(ReplServerOutput* output, ReplServerSession* session, char[] id, char[] code, bool program_mode) {
    Interp* interp = session.interp;
    interp.console_capture_active = true;
    repl_server_capture_reset(interp);

    EvalResult result = program_mode
        ? repl_eval_program(code, interp)
        : repl_eval_line(code, interp);

    interp.console_capture_active = false;
    return repl_server_send_eval_result(output, id, session.id_ptr[:session.id_len], result, interp);
}

fn bool repl_server_load_file_session(ReplServerOutput* output, ReplServerSession* session, char[] id, char[] path) {
    if (try source = io::file::load_temp((String)path)) {
        Interp* interp = session.interp;
        if (!push_source_dir(path, interp)) {
            return repl_server_send_event_error(
                output,
                id,
                session.id_ptr[:session.id_len],
                "runtime/source-dir-allocation-failed",
                "load-file: failed to allocate source directory context"
            );
        }
        defer pop_source_dir(interp);

        interp.console_capture_active = true;
        repl_server_capture_reset(interp);
        EvalResult result = run_program(source, interp);
        interp.console_capture_active = false;

        char[] captured = repl_server_capture_text(interp);
        if (captured.len > 0 && !repl_server_send_out_event(output, id, session.id_ptr[:session.id_len], captured)) return false;
        if (repl_server_eval_result_interrupted(result)) {
            return repl_server_send_interrupted_event(output, id, session.id_ptr[:session.id_len]);
        }
        if (result.error.has_error) {
            return repl_server_send_event_error(
                output,
                id,
                session.id_ptr[:session.id_len],
                "runtime/eval-error",
                json_repl_error_message(result),
                true,
                json_repl_zero_based(result.error.line),
                json_repl_zero_based(result.error.column)
            );
        }

        char* rendered = null;
        usz rendered_len = 0;
        if (!json_repl_render_value(result.value, &interp.symbols, &rendered, &rendered_len)) {
            return repl_server_send_event_error(
                output,
                id,
                session.id_ptr[:session.id_len],
                "runtime/render-failed",
                "failed to render evaluation result"
            );
        }
        defer mem::free(rendered);

        if (!repl_server_send_value_event(output, id, session.id_ptr[:session.id_len], rendered[:rendered_len])) return false;
        if (!repl_server_send_loaded_event(output, id, session.id_ptr[:session.id_len], path)) return false;
        return repl_server_send_done(output, id, session.id_ptr[:session.id_len]);
    }

    main::EntryFileReadKind read_kind;
    if (!main::classify_file_read_path(path, &read_kind)) {
        char[128] msg_buf = {};
        char[] message = "load-file: read failed";
        if (try rendered = io::bprintf(
            &msg_buf,
            "load-file: %s",
            (ZString)main::file_read_kind_reason(read_kind)
        )) {
            message = rendered;
        }
        return repl_server_send_event_error(
            output,
            id,
            session.id_ptr[:session.id_len],
            main::file_read_kind_code(read_kind),
            message
        );
    }

    return repl_server_send_event_error(
        output,
        id,
        session.id_ptr[:session.id_len],
        "io/read-failed",
        "load-file: read failed"
    );
}

fn bool repl_server_complete_session(ReplServerOutput* output, ReplServerSession* session, char[] id, char[] prefix) {
    char[] session_id = session.id_ptr[:session.id_len];
    if (!repl_server_send_complete_event(output, id, session_id, session.interp.global_env, &session.interp.symbols, prefix)) {
        return false;
    }
    return repl_server_send_done(output, id, session_id);
}
