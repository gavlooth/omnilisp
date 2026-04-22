module lisp;

import std::core::mem;
import std::io;

struct UiFtxuiSessionOwner {
    UiFtxuiRunState state;
    OmniFtxuiSession* session;
}

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
            if (count == usz.max) return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui: children list too large");
            count++;
            cur = cdr(cur);
        }
        if (!is_nil(cur)) return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui: children must be a proper list or array");
    } else {
        return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui: children must be a proper list or array");
    }

    if (count == 0) return null;
    if (count > usz.max / OmniFtxuiComponent*.sizeof) {
        return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui: child component array too large");
    }

    OmniFtxuiComponent** children =
        (OmniFtxuiComponent**)mem::malloc(OmniFtxuiComponent*.sizeof * count);
    if (children == null) return ftxui_raise(interp, "ui/out-of-memory", "ui-ftxui: out of memory allocating child component array");
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

fn Value* ui_ftxui_effect_attrs(Value* node, Interp* interp) {
    Value* attrs = ui_ftxui_dict_get(node, "attrs", interp);
    if (attrs == null || !is_dict(attrs)) return null;
    return attrs;
}

fn char[] ui_ftxui_effect_tag_name(Value* node, Interp* interp) {
    if (node == null || !is_dict(node)) return "";
    return ui_ftxui_kind_name(ui_ftxui_dict_get(node, "tag", interp), interp);
}

fn bool ui_ftxui_effect_tag_is(Value* node, Interp* interp, char[] expected) {
    return ui_ftxui_str_eq(ui_ftxui_effect_tag_name(node, interp), expected);
}

fn Value* ui_ftxui_require_effect_attrs(Value* node, Interp* interp, char[] message) {
    Value* attrs = ui_ftxui_effect_attrs(node, interp);
    if (attrs == null) return ftxui_raise(interp, "ui/invalid-argument", message);
    return attrs;
}

fn bool ui_ftxui_effect_backend_is_ftxui(Value* attrs, Interp* interp) {
    Value* backend = ui_ftxui_dict_get(attrs, "backend", interp);
    if (backend == null || is_nil(backend)) return false;
    char[] backend_name = ui_ftxui_kind_name(backend, interp);
    return ui_ftxui_str_eq(backend_name, "ftxui");
}

fn bool ui_ftxui_event_kind_from_value(Value* kind_value, Interp* interp, int* out_kind) {
    if (kind_value == null || is_nil(kind_value)) {
        *out_kind = OMNI_FTXUI_EVENT_CUSTOM;
        return true;
    }
    char[] name = ui_ftxui_kind_name(kind_value, interp);
    if (ui_ftxui_str_eq(name, "custom")) {
        *out_kind = OMNI_FTXUI_EVENT_CUSTOM;
    } else if (ui_ftxui_str_eq(name, "return")) {
        *out_kind = OMNI_FTXUI_EVENT_RETURN;
    } else if (ui_ftxui_str_eq(name, "escape")) {
        *out_kind = OMNI_FTXUI_EVENT_ESCAPE;
    } else if (ui_ftxui_str_eq(name, "tab")) {
        *out_kind = OMNI_FTXUI_EVENT_TAB;
    } else if (ui_ftxui_str_eq(name, "backspace")) {
        *out_kind = OMNI_FTXUI_EVENT_BACKSPACE;
    } else if (ui_ftxui_str_eq(name, "delete")) {
        *out_kind = OMNI_FTXUI_EVENT_DELETE;
    } else if (ui_ftxui_str_eq(name, "arrow-left")) {
        *out_kind = OMNI_FTXUI_EVENT_ARROW_LEFT;
    } else if (ui_ftxui_str_eq(name, "arrow-right")) {
        *out_kind = OMNI_FTXUI_EVENT_ARROW_RIGHT;
    } else if (ui_ftxui_str_eq(name, "arrow-up")) {
        *out_kind = OMNI_FTXUI_EVENT_ARROW_UP;
    } else if (ui_ftxui_str_eq(name, "arrow-down")) {
        *out_kind = OMNI_FTXUI_EVENT_ARROW_DOWN;
    } else if (ui_ftxui_str_eq(name, "character")) {
        *out_kind = OMNI_FTXUI_EVENT_CHARACTER;
    } else {
        return false;
    }
    return true;
}

fn Value* ui_ftxui_dispatch_post_event(UiFtxuiRunState* state, Value* attrs, Interp* interp) {
    int kind = OMNI_FTXUI_EVENT_CUSTOM;
    if (!ui_ftxui_event_kind_from_value(ui_ftxui_dict_get(attrs, "kind", interp), interp, &kind)) {
        return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui.dispatch: unsupported post_event kind");
    }

    char* text = null;
    if (kind == OMNI_FTXUI_EVENT_CHARACTER) {
        Value* text_value = ui_ftxui_dict_get(attrs, "text", interp);
        Value* text_err = ui_ftxui_require_stringish(
            text_value,
            &text,
            interp,
            "ui-ftxui.dispatch: character post_event requires string-like 'text'"
        );
        if (text_err != null) return text_err;
        defer if (text != null) mem::free(text);
    }

    OmniFtxuiEventSpec spec = {};
    spec.size = OmniFtxuiEventSpec.sizeof;
    spec.abi_version = OMNI_FTXUI_ABI_VERSION;
    spec.kind = kind;
    spec.text = text;

    OmniFtxuiEventHandle* event = null;
    OmniFtxuiStatus status = omni_ftxui_event_create(state.context, &spec, &event);
    if (status != OMNI_FTXUI_STATUS_OK) return ui_ftxui_raise_status(state, "event_create", status, interp);
    defer if (event != null) omni_ftxui_event_destroy(event);

    status = omni_ftxui_screen_post_event(state.context, state.screen, event);
    if (status != OMNI_FTXUI_STATUS_OK) return ui_ftxui_raise_status(state, "screen_post_event", status, interp);
    return null;
}

fn Value* ui_ftxui_dispatch_effect_node(UiFtxuiRunState* state, Value* node, Interp* interp, bool* saw_render) {
    if (node == null || !is_dict(node)) return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui.dispatch: expected effect node dictionary");
    Value* attrs = ui_ftxui_require_effect_attrs(node, interp, "ui-ftxui.dispatch: effect attrs must be dictionary");
    if (attrs != null && attrs.tag == ERROR) return attrs;

    if (ui_ftxui_effect_tag_is(node, interp, "render")) {
        Value* view = ui_ftxui_dict_get(attrs, "node", interp);
        if (view == null) return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui.dispatch: render effect requires 'node'");
        OmniFtxuiComponent* rendered = null;
        Value* lower_err = ui_ftxui_lower_node(state, view, &rendered, interp);
        if (lower_err != null) return lower_err;
        *saw_render = true;
        return null;
    }

    if (ui_ftxui_effect_tag_is(node, interp, "invalidate")) {
        return null;
    }

    if (ui_ftxui_effect_tag_is(node, interp, "post_event")) {
        return null;
    }

    if (ui_ftxui_effect_tag_is(node, interp, "close")) {
        return null;
    }

    if (ui_ftxui_effect_tag_is(node, interp, "read_event")) {
        return ftxui_raise(interp, "ui/unsupported", "ui-ftxui.dispatch: read_event is not supported by the non-interactive backend dispatcher");
    }

    return ftxui_raise(interp, "ui/unsupported", "ui-ftxui.dispatch: unsupported effect tag");
}

fn Value* ui_ftxui_dispatch_effect_children(UiFtxuiRunState* state, Value* children, Interp* interp, bool* saw_render) {
    if (children == null || is_nil(children)) return null;
    if (children.tag == ARRAY && children.array_val != null) {
        for (usz i = 0; i < children.array_val.length; i++) {
            Value* err = ui_ftxui_dispatch_effect_node(state, children.array_val.items[i], interp, saw_render);
            if (err != null) return err;
        }
        return null;
    }
    if (is_cons(children)) {
        Value* cur = children;
        while (is_cons(cur)) {
            Value* err = ui_ftxui_dispatch_effect_node(state, car(cur), interp, saw_render);
            if (err != null) return err;
            cur = cdr(cur);
        }
        if (!is_nil(cur)) return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui.dispatch: effect children must be a proper list or array");
        return null;
    }
    return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui.dispatch: effect children must be a proper list or array");
}

fn Value* ui_ftxui_loop_effect_node(
    UiFtxuiRunState* state,
    Value* node,
    Interp* interp,
    bool* saw_render,
    OmniFtxuiComponent** out_root
) {
    if (node == null || !is_dict(node)) return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui.loop: expected effect node dictionary");
    Value* attrs = ui_ftxui_require_effect_attrs(node, interp, "ui-ftxui.loop: effect attrs must be dictionary");
    if (attrs != null && attrs.tag == ERROR) return attrs;

    if (ui_ftxui_effect_tag_is(node, interp, "render")) {
        if (*saw_render) return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui.loop: open tree requires exactly one render child");
        Value* view = ui_ftxui_dict_get(attrs, "node", interp);
        if (view == null) return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui.loop: render effect requires 'node'");
        Value* lower_err = ui_ftxui_lower_node(state, view, out_root, interp);
        if (lower_err != null) return lower_err;
        *saw_render = true;
        return null;
    }

    if (ui_ftxui_effect_tag_is(node, interp, "invalidate")) {
        OmniFtxuiStatus status = omni_ftxui_screen_request_animation_frame(state.context, state.screen);
        if (status != OMNI_FTXUI_STATUS_OK) return ui_ftxui_raise_status(state, "screen_request_animation_frame", status, interp);
        return null;
    }

    if (ui_ftxui_effect_tag_is(node, interp, "post_event")) {
        return ui_ftxui_dispatch_post_event(state, attrs, interp);
    }

    if (ui_ftxui_effect_tag_is(node, interp, "close")) {
        OmniFtxuiStatus status = omni_ftxui_screen_exit(state.context, state.screen);
        if (status != OMNI_FTXUI_STATUS_OK) return ui_ftxui_raise_status(state, "screen_exit", status, interp);
        return null;
    }

    if (ui_ftxui_effect_tag_is(node, interp, "read_event")) {
        return ftxui_raise(interp, "ui/unsupported", "ui-ftxui.loop: read_event is not supported by the blocking FTXUI loop");
    }

    return ftxui_raise(interp, "ui/unsupported", "ui-ftxui.loop: unsupported effect tag");
}

fn Value* ui_ftxui_loop_runtime_effect_node(
    UiFtxuiRunState* state,
    Value* node,
    Interp* interp,
    bool* close_requested
) {
    if (node == null || !is_dict(node)) return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui.loop: expected effect node dictionary");
    Value* attrs = ui_ftxui_require_effect_attrs(node, interp, "ui-ftxui.loop: effect attrs must be dictionary");
    if (attrs != null && attrs.tag == ERROR) return attrs;

    if (ui_ftxui_effect_tag_is(node, interp, "render")) return null;

    if (ui_ftxui_effect_tag_is(node, interp, "invalidate")) {
        OmniFtxuiStatus status = omni_ftxui_screen_request_animation_frame(state.context, state.screen);
        if (status != OMNI_FTXUI_STATUS_OK) return ui_ftxui_raise_status(state, "screen_request_animation_frame", status, interp);
        return null;
    }

    if (ui_ftxui_effect_tag_is(node, interp, "post_event")) {
        return ui_ftxui_dispatch_post_event(state, attrs, interp);
    }

    if (ui_ftxui_effect_tag_is(node, interp, "close")) {
        OmniFtxuiStatus status = omni_ftxui_screen_exit(state.context, state.screen);
        if (status != OMNI_FTXUI_STATUS_OK) return ui_ftxui_raise_status(state, "screen_exit", status, interp);
        *close_requested = true;
        return null;
    }

    if (ui_ftxui_effect_tag_is(node, interp, "read_event")) {
        return ftxui_raise(interp, "ui/unsupported", "ui-ftxui.loop: read_event is not supported by the blocking FTXUI loop");
    }

    return ftxui_raise(interp, "ui/unsupported", "ui-ftxui.loop: unsupported effect tag");
}

fn Value* ui_ftxui_loop_effect_children(
    UiFtxuiRunState* state,
    Value* children,
    Interp* interp,
    bool* saw_render,
    OmniFtxuiComponent** out_root
) {
    if (children == null || is_nil(children)) return null;
    if (children.tag == ARRAY && children.array_val != null) {
        for (usz i = 0; i < children.array_val.length; i++) {
            Value* err = ui_ftxui_loop_effect_node(state, children.array_val.items[i], interp, saw_render, out_root);
            if (err != null) return err;
        }
        return null;
    }
    if (is_cons(children)) {
        Value* cur = children;
        while (is_cons(cur)) {
            Value* err = ui_ftxui_loop_effect_node(state, car(cur), interp, saw_render, out_root);
            if (err != null) return err;
            cur = cdr(cur);
        }
        if (!is_nil(cur)) return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui.loop: effect children must be a proper list or array");
        return null;
    }
    return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui.loop: effect children must be a proper list or array");
}

fn Value* ui_ftxui_loop_runtime_effect_children(
    UiFtxuiRunState* state,
    Value* children,
    Interp* interp,
    bool* close_requested
) {
    if (children == null || is_nil(children)) return null;
    if (children.tag == ARRAY && children.array_val != null) {
        for (usz i = 0; i < children.array_val.length; i++) {
            Value* err = ui_ftxui_loop_runtime_effect_node(state, children.array_val.items[i], interp, close_requested);
            if (err != null) return err;
        }
        return null;
    }
    if (is_cons(children)) {
        Value* cur = children;
        while (is_cons(cur)) {
            Value* err = ui_ftxui_loop_runtime_effect_node(state, car(cur), interp, close_requested);
            if (err != null) return err;
            cur = cdr(cur);
        }
        if (!is_nil(cur)) return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui.loop: effect children must be a proper list or array");
        return null;
    }
    return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui.loop: effect children must be a proper list or array");
}

fn void ui_ftxui_session_finalizer(void* handle) {
    UiFtxuiSessionOwner* owner = (UiFtxuiSessionOwner*)handle;
    if (owner == null) return;
    if (owner.session != null) {
        omni_ftxui_session_destroy(owner.session);
        owner.session = null;
    }
    ui_ftxui_destroy_state(&owner.state);
    mem::free(owner);
}

fn Value* ui_ftxui_session_require(Value* value, Interp* interp, UiFtxuiSessionOwner** out_owner) {
    *out_owner = null;
    if (!ffi_handle_name_equals(value, "ui-ftxui-session")) {
        return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui.session: expected ui-ftxui-session handle");
    }
    if (value.ffi_val.lib_handle == null) {
        return ftxui_raise(interp, "ui/invalid-state", "ui-ftxui.session: session handle is closed");
    }
    *out_owner = (UiFtxuiSessionOwner*)value.ffi_val.lib_handle;
    return null;
}

fn Value* ui_ftxui_session_require_open_tree(Value* root, Interp* interp, char[] op_name) {
    if (root == null || !is_dict(root)) {
        char[128] msg_buf;
        char[] msg = "ui-ftxui.session: expected effect tree dictionary";
        if (try char[] formatted = io::bprintf(&msg_buf, "ui-ftxui.%s: expected effect tree dictionary", (ZString)op_name)) {
            msg = formatted;
        }
        return ftxui_raise(interp, "ui/invalid-argument", msg);
    }
    if (!ui_ftxui_effect_tag_is(root, interp, "open")) {
        char[128] msg_buf;
        char[] msg = "ui-ftxui.session: root effect must be open";
        if (try char[] formatted = io::bprintf(&msg_buf, "ui-ftxui.%s: root effect must be open", (ZString)op_name)) {
            msg = formatted;
        }
        return ftxui_raise(interp, "ui/invalid-argument", msg);
    }
    Value* attrs = ui_ftxui_require_effect_attrs(root, interp, "ui-ftxui.session: open attrs must be dictionary");
    if (attrs != null && attrs.tag == ERROR) return attrs;
    if (!ui_ftxui_effect_backend_is_ftxui(attrs, interp)) {
        char[128] msg_buf;
        char[] msg = "ui-ftxui.session: open attrs require backend 'ftxui'";
        if (try char[] formatted = io::bprintf(&msg_buf, "ui-ftxui.%s: open attrs require backend 'ftxui'", (ZString)op_name)) {
            msg = formatted;
        }
        return ftxui_raise(interp, "ui/invalid-argument", msg);
    }
    return null;
}

fn Value* ui_ftxui_session_status_bool(UiFtxuiSessionOwner* owner, Interp* interp) {
    bool quitted = false;
    OmniFtxuiStatus status = omni_ftxui_session_has_quitted(owner.state.context, owner.session, &quitted);
    if (status != OMNI_FTXUI_STATUS_OK) return ui_ftxui_raise_status(&owner.state, "session_has_quitted", status, interp);
    return quitted ? make_nil(interp) : make_symbol(interp, interp.sym_true);
}

fn char[] ui_ftxui_session_event_kind_symbol(int kind) {
    switch (kind) {
        case OMNI_FTXUI_EVENT_CUSTOM:
            return "custom";
        case OMNI_FTXUI_EVENT_CHARACTER:
            return "character";
        case OMNI_FTXUI_EVENT_SPECIAL:
            return "special";
        case OMNI_FTXUI_EVENT_ARROW_LEFT:
            return "arrow-left";
        case OMNI_FTXUI_EVENT_ARROW_RIGHT:
            return "arrow-right";
        case OMNI_FTXUI_EVENT_ARROW_UP:
            return "arrow-up";
        case OMNI_FTXUI_EVENT_ARROW_DOWN:
            return "arrow-down";
        case OMNI_FTXUI_EVENT_RETURN:
            return "return";
        case OMNI_FTXUI_EVENT_ESCAPE:
            return "escape";
        case OMNI_FTXUI_EVENT_TAB:
            return "tab";
        case OMNI_FTXUI_EVENT_TAB_REVERSE:
            return "tab-reverse";
        case OMNI_FTXUI_EVENT_BACKSPACE:
            return "backspace";
        case OMNI_FTXUI_EVENT_DELETE:
            return "delete";
        case OMNI_FTXUI_EVENT_HOME:
            return "home";
        case OMNI_FTXUI_EVENT_END:
            return "end";
        case OMNI_FTXUI_EVENT_PAGE_UP:
            return "page-up";
        case OMNI_FTXUI_EVENT_PAGE_DOWN:
            return "page-down";
        case OMNI_FTXUI_EVENT_F1:
            return "f1";
        case OMNI_FTXUI_EVENT_F2:
            return "f2";
        case OMNI_FTXUI_EVENT_F3:
            return "f3";
        case OMNI_FTXUI_EVENT_F4:
            return "f4";
        case OMNI_FTXUI_EVENT_F5:
            return "f5";
        case OMNI_FTXUI_EVENT_F6:
            return "f6";
        case OMNI_FTXUI_EVENT_F7:
            return "f7";
        case OMNI_FTXUI_EVENT_F8:
            return "f8";
        case OMNI_FTXUI_EVENT_F9:
            return "f9";
        case OMNI_FTXUI_EVENT_F10:
            return "f10";
        case OMNI_FTXUI_EVENT_F11:
            return "f11";
        case OMNI_FTXUI_EVENT_F12:
            return "f12";
        default:
            return "unknown";
    }
}

fn Value* ui_ftxui_session_event_dict_set(Value* dict, Interp* interp, char[] key, Value* value) {
    if (dict == null || dict.tag != HASHMAP || dict.hashmap_val == null) {
        return ftxui_raise(interp, "ui/out-of-memory", "ui-ftxui.session-read-event: out of memory");
    }
    if (value == null || value.tag == ERROR) return value;
    SymbolId key_sym = interp.symbols.intern(key);
    if (key_sym == INVALID_SYMBOL_ID) return ftxui_raise(interp, "ui/out-of-memory", "ui-ftxui.session-read-event: out of memory");
    if (!hashmap_set_symbol_checked(dict.hashmap_val, key_sym, value, interp)) {
        return ftxui_raise(interp, "ui/out-of-memory", "ui-ftxui.session-read-event: out of memory");
    }
    return null;
}

fn Value* ui_ftxui_session_event_to_value(OmniFtxuiEventReadResult* event, Interp* interp) {
    if (event == null || !event.has_event) return make_nil(interp);
    Value* out = make_hashmap_checked(interp, 4, "ui-ftxui.session-read-event: out of memory");
    if (out == null || out.tag == ERROR) return out;
    char[] kind_name = ui_ftxui_session_event_kind_symbol(event.kind);
    SymbolId kind_sym = interp.symbols.intern(kind_name);
    if (kind_sym == INVALID_SYMBOL_ID) return ftxui_raise(interp, "ui/out-of-memory", "ui-ftxui.session-read-event: out of memory");
    if (Value* err = ui_ftxui_session_event_dict_set(out, interp, "kind", make_symbol(interp, kind_sym))) return err;

    usz text_len = 0;
    while (text_len < event.text.len && event.text[text_len] != 0) text_len++;
    if (text_len > 0) {
        if (Value* err = ui_ftxui_session_event_dict_set(out, interp, "text", make_string(interp, event.text[:text_len]))) return err;
    }
    return out;
}

fn Value* ui_ftxui_session_runtime_effect_node(
    UiFtxuiSessionOwner* owner,
    Value* node,
    Interp* interp
) {
    char[] tag = ui_ftxui_effect_tag_name(node, interp);
    if (ui_ftxui_str_eq(tag, "render")) return null;
    if (ui_ftxui_str_eq(tag, "invalidate")) {
        OmniFtxuiStatus status = omni_ftxui_screen_request_animation_frame(owner.state.context, owner.state.screen);
        if (status != OMNI_FTXUI_STATUS_OK) return ui_ftxui_raise_status(&owner.state, "screen_request_animation_frame", status, interp);
        return null;
    }
    if (ui_ftxui_str_eq(tag, "post_event")) {
        return ui_ftxui_dispatch_post_event(&owner.state, ui_ftxui_effect_attrs(node, interp), interp);
    }
    if (ui_ftxui_str_eq(tag, "close")) {
        OmniFtxuiStatus status = omni_ftxui_screen_exit(owner.state.context, owner.state.screen);
        if (status != OMNI_FTXUI_STATUS_OK) return ui_ftxui_raise_status(&owner.state, "screen_exit", status, interp);
        return null;
    }
    if (ui_ftxui_str_eq(tag, "read_event")) {
        return ftxui_raise(interp, "ui/unsupported", "ui-ftxui.session: read_event payloads are not supported by the current FTXUI backend session API");
    }
    if (tag.len == 0) return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui.session: expected effect node dictionary");
    return ftxui_raise(interp, "ui/unsupported", "ui-ftxui.session: unsupported effect tag in session update");
}

fn Value* ui_ftxui_session_runtime_effect_children(
    UiFtxuiSessionOwner* owner,
    Value* children,
    Interp* interp
) {
    if (children == null || is_nil(children)) return null;
    if (children.tag == ARRAY && children.array_val != null) {
        for (usz i = 0; i < children.array_val.length; i++) {
            Value* err = ui_ftxui_session_runtime_effect_node(owner, children.array_val.items[i], interp);
            if (err != null) return err;
        }
        return null;
    }
    if (is_cons(children)) {
        Value* cur = children;
        while (is_cons(cur)) {
            Value* err = ui_ftxui_session_runtime_effect_node(owner, car(cur), interp);
            if (err != null) return err;
            cur = cdr(cur);
        }
        if (!is_nil(cur)) return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui.session: effect children must be a proper list or array");
        return null;
    }
    return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui.session: effect children must be a proper list or array");
}

fn Value* prim_ui_ftxui_session_open(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return ftxui_raise(interp, "ui/arity", "__ui-ftxui-session-open: expected ui effect tree");
    if (omni_ftxui_backend_available() == 0) {
        return ftxui_raise(interp, "ui/unavailable", "__ui-ftxui-session-open: FTXUI backend unavailable");
    }

    Value* root = args[0];
    Value* tree_err = ui_ftxui_session_require_open_tree(root, interp, "session-open");
    if (tree_err != null) return tree_err;

    UiFtxuiSessionOwner* owner = (UiFtxuiSessionOwner*)mem::malloc(UiFtxuiSessionOwner.sizeof);
    if (owner == null) return ftxui_raise(interp, "ui/out-of-memory", "ui-ftxui.session-open: out of memory allocating session owner");
    *owner = {};

    OmniFtxuiStatus status = omni_ftxui_context_create(&owner.state.context);
    if (status != OMNI_FTXUI_STATUS_OK) {
        Value* err = ui_ftxui_raise_status(&owner.state, "context_create", status, interp);
        ui_ftxui_session_finalizer(owner);
        return err;
    }

    OmniFtxuiScreenCreateOptions screen_options = {};
    screen_options.size = OmniFtxuiScreenCreateOptions.sizeof;
    screen_options.abi_version = OMNI_FTXUI_ABI_VERSION;
    screen_options.mode = OMNI_FTXUI_SCREEN_FIT_COMPONENT;
    screen_options.track_mouse = false;
    screen_options.handle_piped_input = false;
    screen_options.width = { .size = OmniFtxuiDimension.sizeof, .abi_version = OMNI_FTXUI_ABI_VERSION, .kind = OMNI_FTXUI_DIMENSION_FIT, .value = 0 };
    screen_options.height = { .size = OmniFtxuiDimension.sizeof, .abi_version = OMNI_FTXUI_ABI_VERSION, .kind = OMNI_FTXUI_DIMENSION_FIT, .value = 0 };
    status = omni_ftxui_screen_create(owner.state.context, &screen_options, &owner.state.screen);
    if (status != OMNI_FTXUI_STATUS_OK) {
        Value* err = ui_ftxui_raise_status(&owner.state, "screen_create", status, interp);
        ui_ftxui_session_finalizer(owner);
        return err;
    }

    bool saw_render = false;
    OmniFtxuiComponent* root_component = null;
    Value* loop_err = ui_ftxui_loop_effect_children(
        &owner.state,
        ui_ftxui_dict_get(root, "children", interp),
        interp,
        &saw_render,
        &root_component
    );
    if (loop_err != null) {
        ui_ftxui_session_finalizer(owner);
        return loop_err;
    }
    if (!saw_render || root_component == null) {
        ui_ftxui_session_finalizer(owner);
        return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui.session-open: open tree requires exactly one render child");
    }

    OmniFtxuiComponent* quit_wrapped = null;
    status = omni_ftxui_component_wrap_quit_keys(owner.state.context, owner.state.screen, root_component, &quit_wrapped);
    if (status != OMNI_FTXUI_STATUS_OK) {
        Value* err = ui_ftxui_raise_status(&owner.state, "component_wrap_quit_keys", status, interp);
        ui_ftxui_session_finalizer(owner);
        return err;
    }
    Value* track_err = ui_ftxui_track_component_or_oom(&owner.state, quit_wrapped, interp);
    if (track_err != null) {
        ui_ftxui_session_finalizer(owner);
        return track_err;
    }

    status = omni_ftxui_session_create(owner.state.context, owner.state.screen, quit_wrapped, &owner.session);
    if (status != OMNI_FTXUI_STATUS_OK) {
        Value* err = ui_ftxui_raise_status(&owner.state, "session_create", status, interp);
        ui_ftxui_session_finalizer(owner);
        return err;
    }

    Value* handle = make_ffi_handle_ex_with_descriptor(
        interp,
        (void*)owner,
        "ui-ftxui-session",
        &ui_ftxui_session_finalizer,
        false,
        FOREIGN_RUNTIME_NATIVE,
        FOREIGN_HANDLE_OBJECT,
        FOREIGN_CAP_RELEASE | FOREIGN_CAP_REFLECT,
        false,
        FFI_HANDLE_OWNED);
    if (handle == null) {
        ui_ftxui_session_finalizer(owner);
        return ftxui_raise(interp, "ui/out-of-memory", "ui-ftxui.session-open: out of memory wrapping session handle");
    }
    return handle;
}

fn Value* prim_ui_ftxui_session_render(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return ftxui_raise(interp, "ui/arity", "__ui-ftxui-session-render: expected session handle");
    UiFtxuiSessionOwner* owner = null;
    if (Value* err = ui_ftxui_session_require(args[0], interp, &owner)) return err;
    OmniFtxuiStatus status = omni_ftxui_session_run_once(owner.state.context, owner.session);
    if (status != OMNI_FTXUI_STATUS_OK) return ui_ftxui_raise_status(&owner.state, "session_run_once", status, interp);
    return ui_ftxui_session_status_bool(owner, interp);
}

fn Value* prim_ui_ftxui_session_read_event(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return ftxui_raise(interp, "ui/arity", "__ui-ftxui-session-read-event: expected session handle");
    UiFtxuiSessionOwner* owner = null;
    if (Value* err = ui_ftxui_session_require(args[0], interp, &owner)) return err;
    OmniFtxuiStatus status = omni_ftxui_session_run_once_blocking(owner.state.context, owner.session);
    if (status != OMNI_FTXUI_STATUS_OK) return ui_ftxui_raise_status(&owner.state, "session_run_once_blocking", status, interp);
    OmniFtxuiEventReadResult event = {};
    event.size = OmniFtxuiEventReadResult.sizeof;
    event.abi_version = OMNI_FTXUI_ABI_VERSION;
    status = omni_ftxui_session_take_event(owner.state.context, owner.session, &event);
    if (status != OMNI_FTXUI_STATUS_OK) return ui_ftxui_raise_status(&owner.state, "session_take_event", status, interp);
    return ui_ftxui_session_event_to_value(&event, interp);
}

fn Value* prim_ui_ftxui_session_update(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return ftxui_raise(interp, "ui/arity", "__ui-ftxui-session-update: expected session handle and ui effect tree");
    UiFtxuiSessionOwner* owner = null;
    if (Value* err = ui_ftxui_session_require(args[0], interp, &owner)) return err;
    Value* tree_err = ui_ftxui_session_require_open_tree(args[1], interp, "session-update");
    if (tree_err != null) return tree_err;
    Value* update_err = ui_ftxui_session_runtime_effect_children(
        owner,
        ui_ftxui_dict_get(args[1], "children", interp),
        interp
    );
    if (update_err != null) return update_err;
    return args[0];
}

fn Value* prim_ui_ftxui_session_invalidate(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return ftxui_raise(interp, "ui/arity", "__ui-ftxui-session-invalidate: expected session handle");
    UiFtxuiSessionOwner* owner = null;
    if (Value* err = ui_ftxui_session_require(args[0], interp, &owner)) return err;
    OmniFtxuiStatus status = omni_ftxui_screen_request_animation_frame(owner.state.context, owner.state.screen);
    if (status != OMNI_FTXUI_STATUS_OK) return ui_ftxui_raise_status(&owner.state, "screen_request_animation_frame", status, interp);
    return args[0];
}

fn Value* prim_ui_ftxui_session_post_event(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return ftxui_raise(interp, "ui/arity", "__ui-ftxui-session-post-event: expected session handle and event attrs");
    UiFtxuiSessionOwner* owner = null;
    if (Value* err = ui_ftxui_session_require(args[0], interp, &owner)) return err;
    Value* post_err = ui_ftxui_dispatch_post_event(&owner.state, args[1], interp);
    if (post_err != null) return post_err;
    return args[0];
}

fn Value* prim_ui_ftxui_session_close(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return ftxui_raise(interp, "ui/arity", "__ui-ftxui-session-close: expected session handle");
    Value* value = args[0];
    if (!ffi_handle_name_equals(value, "ui-ftxui-session")) {
        return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui.session-close: expected ui-ftxui-session handle");
    }
    if (value.ffi_val.lib_handle == null) return make_symbol(interp, interp.sym_true);
    UiFtxuiSessionOwner* owner = (UiFtxuiSessionOwner*)value.ffi_val.lib_handle;
    OmniFtxuiStatus status = omni_ftxui_screen_exit(owner.state.context, owner.state.screen);
    if (status != OMNI_FTXUI_STATUS_OK) return ui_ftxui_raise_status(&owner.state, "screen_exit", status, interp);
    if (!ffi_handle_release_payload(value.ffi_val)) {
        return ftxui_raise(interp, "ui/invalid-state", "ui-ftxui.session-close: failed to release session handle");
    }
    return make_symbol(interp, interp.sym_true);
}

fn Value* prim_ui_ftxui_dispatch_effect_tree(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return ftxui_raise(interp, "ui/arity", "__ui-ftxui-dispatch: expected ui effect tree");
    if (omni_ftxui_backend_available() == 0) {
        return ftxui_raise(interp, "ui/unavailable", "__ui-ftxui-dispatch: FTXUI backend unavailable");
    }

    Value* root = args[0];
    if (root == null || !is_dict(root)) return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui.dispatch: expected effect tree dictionary");
    if (!ui_ftxui_effect_tag_is(root, interp, "open")) {
        return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui.dispatch: root effect must be open");
    }

    Value* attrs = ui_ftxui_require_effect_attrs(root, interp, "ui-ftxui.dispatch: open attrs must be dictionary");
    if (attrs != null && attrs.tag == ERROR) return attrs;
    if (!ui_ftxui_effect_backend_is_ftxui(attrs, interp)) {
        return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui.dispatch: open attrs require backend 'ftxui'");
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

    bool saw_render = false;
    Value* dispatch_err = ui_ftxui_dispatch_effect_children(&state, ui_ftxui_dict_get(root, "children", interp), interp, &saw_render);
    if (dispatch_err != null) return dispatch_err;
    if (!saw_render) return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui.dispatch: open tree requires at least one render child");

    return make_symbol(interp, interp.sym_true);
}

fn Value* prim_ui_ftxui_loop_effect_tree(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return ftxui_raise(interp, "ui/arity", "__ui-ftxui-loop: expected ui effect tree");
    if (omni_ftxui_backend_available() == 0) {
        return ftxui_raise(interp, "ui/unavailable", "__ui-ftxui-loop: FTXUI backend unavailable");
    }

    Value* root = args[0];
    if (root == null || !is_dict(root)) return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui.loop: expected effect tree dictionary");
    if (!ui_ftxui_effect_tag_is(root, interp, "open")) {
        return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui.loop: root effect must be open");
    }

    Value* attrs = ui_ftxui_require_effect_attrs(root, interp, "ui-ftxui.loop: open attrs must be dictionary");
    if (attrs != null && attrs.tag == ERROR) return attrs;
    if (!ui_ftxui_effect_backend_is_ftxui(attrs, interp)) {
        return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui.loop: open attrs require backend 'ftxui'");
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

    bool saw_render = false;
    OmniFtxuiComponent* root_component = null;
    Value* loop_err = ui_ftxui_loop_effect_children(
        &state,
        ui_ftxui_dict_get(root, "children", interp),
        interp,
        &saw_render,
        &root_component
    );
    if (loop_err != null) return loop_err;
    if (!saw_render || root_component == null) {
        return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui.loop: open tree requires exactly one render child");
    }

    OmniFtxuiComponent* quit_wrapped = null;
    status = omni_ftxui_component_wrap_quit_keys(state.context, state.screen, root_component, &quit_wrapped);
    if (status != OMNI_FTXUI_STATUS_OK) return ui_ftxui_raise_status(&state, "component_wrap_quit_keys", status, interp);
    Value* track_err = ui_ftxui_track_component_or_oom(&state, quit_wrapped, interp);
    if (track_err != null) return track_err;

    OmniFtxuiApp* app = null;
    status = omni_ftxui_app_create(state.context, state.screen, quit_wrapped, &app);
    if (status != OMNI_FTXUI_STATUS_OK) return ui_ftxui_raise_status(&state, "app_create", status, interp);
    defer if (app != null) omni_ftxui_app_destroy(app);

    bool close_requested = false;
    Value* runtime_err = ui_ftxui_loop_runtime_effect_children(
        &state,
        ui_ftxui_dict_get(root, "children", interp),
        interp,
        &close_requested
    );
    if (runtime_err != null) return runtime_err;
    if (close_requested) return make_symbol(interp, interp.sym_true);

    status = omni_ftxui_app_run(state.context, app);
    if (status != OMNI_FTXUI_STATUS_OK) return ui_ftxui_raise_status(&state, "app_run", status, interp);

    return make_symbol(interp, interp.sym_true);
}

fn Value* prim_ui_ftxui_run(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return ftxui_raise(interp, "ui/arity", "__ui-ftxui-run: expected ui root node");
    if (omni_ftxui_backend_available() == 0) {
        return ftxui_raise(interp, "ui/unavailable", "__ui-ftxui-run: FTXUI backend unavailable");
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
