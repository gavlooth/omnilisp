module lisp;

import std::core::mem;
import std::io;

fn Value* ui_ftxui_collect_children(
    UiFtxuiRunState* state,
    Value* seq,
    OmniFtxuiComponent*** out_children,
    usz* out_count,
    Interp* interp
) {
    *out_children = null;
    *out_count = 0;
    if (seq == null || is_nil(seq)) return null;

    usz count = 0;
    if (seq.tag == ARRAY && seq.array_val != null) {
        count = seq.array_val.length;
    } else if (is_cons(seq)) {
        Value* cur = seq;
        while (is_cons(cur)) {
            count++;
            cur = cdr(cur);
        }
        if (!is_nil(cur)) return raise_error(interp, "ui-ftxui: children must be a proper list or array");
    } else {
        return raise_error(interp, "ui-ftxui: children must be a proper list or array");
    }

    if (count == 0) return null;

    OmniFtxuiComponent** children =
        (OmniFtxuiComponent**)mem::malloc(OmniFtxuiComponent*.sizeof * count);
    if (children == null) return raise_error(interp, "ui-ftxui: out of memory allocating child component array");
    for (usz i = 0; i < count; i++) children[i] = null;

    if (seq.tag == ARRAY && seq.array_val != null) {
        for (usz i = 0; i < count; i++) {
            Value* child_err = ui_ftxui_lower_node(state, seq.array_val.items[i], &children[i], interp);
            if (child_err != null) {
                mem::free(children);
                return child_err;
            }
        }
    } else {
        Value* cur = seq;
        usz index = 0;
        while (is_cons(cur)) {
            Value* child_err = ui_ftxui_lower_node(state, car(cur), &children[index], interp);
            if (child_err != null) {
                mem::free(children);
                return child_err;
            }
            index++;
            cur = cdr(cur);
        }
    }

    *out_children = children;
    *out_count = count;
    return null;
}

fn Value* prim_ui_ftxui_run(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "__ui-ftxui-run: expected ui root node");
    if (omni_ftxui_backend_available() == 0) {
        return raise_error(interp, "__ui-ftxui-run: FTXUI backend unavailable");
    }

    UiFtxuiRunState state = {};
    defer ui_ftxui_destroy_state(&state);

    OmniFtxuiStatus status = omni_ftxui_context_create(&state.context);
    if (status != OMNI_FTXUI_STATUS_OK) return ui_ftxui_raise_status(&state, "context_create", status, interp);

    OmniFtxuiScreenCreateOptions screen_options = {};
    screen_options.size = OmniFtxuiScreenCreateOptions.sizeof;
    screen_options.abi_version = OMNI_FTXUI_ABI_VERSION;
    screen_options.mode = OMNI_FTXUI_SCREEN_FIT_COMPONENT;
    screen_options.track_mouse = false;
    screen_options.handle_piped_input = false;
    screen_options.width = { .size = OmniFtxuiDimension.sizeof, .abi_version = OMNI_FTXUI_ABI_VERSION, .kind = OMNI_FTXUI_DIMENSION_FIT, .value = 0 };
    screen_options.height = { .size = OmniFtxuiDimension.sizeof, .abi_version = OMNI_FTXUI_ABI_VERSION, .kind = OMNI_FTXUI_DIMENSION_FIT, .value = 0 };
    status = omni_ftxui_screen_create(state.context, &screen_options, &state.screen);
    if (status != OMNI_FTXUI_STATUS_OK) return ui_ftxui_raise_status(&state, "screen_create", status, interp);

    OmniFtxuiComponent* root = null;
    Value* lower_err = ui_ftxui_lower_node(&state, args[0], &root, interp);
    if (lower_err != null) return lower_err;

    OmniFtxuiComponent* quit_wrapped = null;
    status = omni_ftxui_component_wrap_quit_keys(state.context, state.screen, root, &quit_wrapped);
    if (status != OMNI_FTXUI_STATUS_OK) return ui_ftxui_raise_status(&state, "component_wrap_quit_keys", status, interp);
    Value* track_err = ui_ftxui_track_component_or_oom(&state, quit_wrapped, interp);
    if (track_err != null) return track_err;

    status = omni_ftxui_screen_loop(state.context, state.screen, quit_wrapped);
    if (status != OMNI_FTXUI_STATUS_OK) return ui_ftxui_raise_status(&state, "screen_loop", status, interp);

    return make_symbol(interp, interp.sym_true);
}
