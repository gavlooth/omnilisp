module lisp;

import std::core::mem;
import std::io;

struct UiFtxuiRunState {
    OmniFtxuiContext* context;
    OmniFtxuiScreen* screen;
    OmniFtxuiComponent** components;
    usz component_count;
    usz component_capacity;
    void** heap_ptrs;
    usz heap_ptr_count;
    usz heap_ptr_capacity;
}

fn bool ui_ftxui_str_eq(char[] left, char[] right) @inline {
    if (left.len != right.len) return false;
    for (usz i = 0; i < left.len; i++) {
        if (left[i] != right[i]) return false;
    }
    return true;
}

fn bool ui_ftxui_push_heap_ptr(UiFtxuiRunState* state, void* ptr) {
    if (ptr == null) return true;
    if (state.heap_ptr_count == state.heap_ptr_capacity) {
        usz new_cap = state.heap_ptr_capacity == 0 ? 8 : state.heap_ptr_capacity * 2;
        void** grown = (void**)mem::realloc(state.heap_ptrs, void*.sizeof * new_cap);
        if (grown == null) return false;
        state.heap_ptrs = grown;
        state.heap_ptr_capacity = new_cap;
    }
    state.heap_ptrs[state.heap_ptr_count++] = ptr;
    return true;
}

fn bool ui_ftxui_push_component(UiFtxuiRunState* state, OmniFtxuiComponent* component) {
    if (component == null) return true;
    if (state.component_count == state.component_capacity) {
        usz new_cap = state.component_capacity == 0 ? 8 : state.component_capacity * 2;
        OmniFtxuiComponent** grown =
            (OmniFtxuiComponent**)mem::realloc(state.components, OmniFtxuiComponent*.sizeof * new_cap);
        if (grown == null) return false;
        state.components = grown;
        state.component_capacity = new_cap;
    }
    state.components[state.component_count++] = component;
    return true;
}

fn void ui_ftxui_destroy_state(UiFtxuiRunState* state) {
    if (state == null) return;
    for (usz i = state.component_count; i > 0; i--) {
        OmniFtxuiComponent* component = state.components[i - 1];
        if (component != null) omni_ftxui_component_destroy(component);
    }
    if (state.components != null) mem::free(state.components);
    for (usz i = 0; i < state.heap_ptr_count; i++) {
        if (state.heap_ptrs[i] != null) mem::free(state.heap_ptrs[i]);
    }
    if (state.heap_ptrs != null) mem::free(state.heap_ptrs);
    if (state.screen != null) omni_ftxui_screen_destroy(state.screen);
    if (state.context != null) omni_ftxui_context_destroy(state.context);
    *state = {};
}

fn Value* ui_ftxui_raise_status(
    UiFtxuiRunState* state,
    char[] op_name,
    OmniFtxuiStatus status,
    Interp* interp
) {
    char* status_name = omni_ftxui_status_name(status);
    char* detail = state.context != null ? omni_ftxui_context_last_error_message(state.context) : null;
    ZString status_text = status_name != null ? (ZString)status_name : "<unknown>";
    ZString detail_text = detail != null ? (ZString)detail : "";
    ZString separator = detail != null && detail[0] != 0 ? ": " : "";
    char[512] msg_buf;
    char[] msg = io::bprintf(
        &msg_buf,
        "ui-ftxui %s failed (%s)%s%s",
        (ZString)op_name,
        status_text,
        separator,
        detail_text
    )!!;
    return raise_error(interp, msg);
}

fn Value* ui_ftxui_track_component_or_oom(
    UiFtxuiRunState* state,
    OmniFtxuiComponent* component,
    Interp* interp
) {
    if (ui_ftxui_push_component(state, component)) return null;
    if (component != null) omni_ftxui_component_destroy(component);
    return raise_error(interp, "ui-ftxui: out of memory while tracking component");
}

fn Value* ui_ftxui_track_heap_or_oom(UiFtxuiRunState* state, void* ptr, Interp* interp) {
    if (ui_ftxui_push_heap_ptr(state, ptr)) return null;
    if (ptr != null) mem::free(ptr);
    return raise_error(interp, "ui-ftxui: out of memory while tracking heap state");
}

fn Value* ui_ftxui_dict_get(Value* dict, char[] key, Interp* interp) {
    if (!is_dict(dict)) return null;
    return hashmap_get(dict.hashmap_val, make_symbol(interp, interp.symbols.intern(key)));
}

fn char[] ui_ftxui_kind_name(Value* kind_value, Interp* interp) {
    if (kind_value == null) return "";
    if (is_symbol(kind_value)) return interp.symbols.get_name(kind_value.sym_val);
    if (is_string(kind_value)) return kind_value.str_chars[:kind_value.str_len];
    return "";
}

fn Value* ui_ftxui_require_stringish(Value* value, char** out_text, Interp* interp, char[] message) {
    char* text = copy_value_to_heap_c_string(value, interp);
    if (text == null) return raise_error(interp, message);
    *out_text = text;
    return null;
}

fn Value* ui_ftxui_collect_number_sequence(
    Value* seq,
    double** out_values,
    usz* out_count,
    Interp* interp,
    char[] message
) {
    *out_values = null;
    *out_count = 0;
    if (seq == null || is_nil(seq)) return raise_error(interp, message);

    usz count = 0;
    if (seq.tag == ARRAY && seq.array_val != null) {
        count = seq.array_val.length;
    } else if (is_cons(seq)) {
        Value* cur = seq;
        while (is_cons(cur)) {
            count++;
            cur = cdr(cur);
        }
        if (!is_nil(cur)) return raise_error(interp, message);
    } else {
        return raise_error(interp, message);
    }

    if (count == 0) return raise_error(interp, "ui-ftxui: graph requires at least one numeric sample");

    double* values = (double*)mem::malloc(double.sizeof * count);
    if (values == null) return raise_error(interp, "ui-ftxui: out of memory allocating graph series");

    if (seq.tag == ARRAY && seq.array_val != null) {
        for (usz i = 0; i < count; i++) {
            Value* item = seq.array_val.items[i];
            if (!is_number(item)) {
                mem::free(values);
                return raise_error(interp, message);
            }
            values[i] = to_double(item);
        }
    } else {
        Value* cur = seq;
        usz index = 0;
        while (is_cons(cur) && index < count) {
            Value* item = car(cur);
            if (!is_number(item)) {
                mem::free(values);
                return raise_error(interp, message);
            }
            values[index++] = to_double(item);
            cur = cdr(cur);
        }
    }

    *out_values = values;
    *out_count = count;
    return null;
}

fn Value* ui_ftxui_wrap_element_as_component(
    UiFtxuiRunState* state,
    OmniFtxuiElementCreateOptions* options,
    OmniFtxuiElement** children,
    usz child_count,
    OmniFtxuiComponent** out_component,
    Interp* interp
) {
    OmniFtxuiElement* element = null;
    OmniFtxuiStatus status = omni_ftxui_element_create(state.context, options, children, child_count, &element);
    if (status != OMNI_FTXUI_STATUS_OK) return ui_ftxui_raise_status(state, "element_create", status, interp);
    defer if (element != null) omni_ftxui_element_destroy(element);

    OmniFtxuiComponent* component = null;
    status = omni_ftxui_component_from_element(state.context, element, &component);
    if (status != OMNI_FTXUI_STATUS_OK) return ui_ftxui_raise_status(state, "component_from_element", status, interp);

    Value* track_err = ui_ftxui_track_component_or_oom(state, component, interp);
    if (track_err != null) return track_err;
    *out_component = component;
    return null;
}

fn Value* ui_ftxui_create_component(
    UiFtxuiRunState* state,
    OmniFtxuiComponentCreateOptions* options,
    OmniFtxuiComponent** children,
    usz child_count,
    OmniFtxuiComponent** out_component,
    Interp* interp
) {
    OmniFtxuiComponent* component = null;
    OmniFtxuiStatus status = omni_ftxui_component_create(state.context, options, children, child_count, &component);
    if (status != OMNI_FTXUI_STATUS_OK) return ui_ftxui_raise_status(state, "component_create", status, interp);

    Value* track_err = ui_ftxui_track_component_or_oom(state, component, interp);
    if (track_err != null) return track_err;
    *out_component = component;
    return null;
}

fn Value* ui_ftxui_lower_node(
    UiFtxuiRunState* state,
    Value* node,
    OmniFtxuiComponent** out_component,
    Interp* interp
) {
    if (node == null || !is_dict(node)) {
        return raise_error(interp, "ui-ftxui: expected ui node dictionary");
    }

    Value* kind_value = ui_ftxui_dict_get(node, "kind", interp);
    Value* props = ui_ftxui_dict_get(node, "props", interp);
    Value* children_value = ui_ftxui_dict_get(node, "children", interp);
    char[] kind_name = ui_ftxui_kind_name(kind_value, interp);

    if (kind_name.len == 0) return raise_error(interp, "ui-ftxui: node kind must be string or symbol");
    if (props == null || !is_dict(props)) return raise_error(interp, "ui-ftxui: node props must be dictionary");

    if (ui_ftxui_str_eq(kind_name, "text") || ui_ftxui_str_eq(kind_name, "paragraph")) {
        char* text = null;
        Value* text_value = ui_ftxui_dict_get(props, "text", interp);
        Value* text_err = ui_ftxui_require_stringish(text_value, &text, interp, "ui-ftxui: text node requires string-like 'text'");
        if (text_err != null) return text_err;
        Value* track_err = ui_ftxui_track_heap_or_oom(state, text, interp);
        if (track_err != null) return track_err;

        OmniFtxuiElementCreateOptions options = {};
        options.size = OmniFtxuiElementCreateOptions.sizeof;
        options.abi_version = OMNI_FTXUI_ABI_VERSION;
        options.kind = ui_ftxui_str_eq(kind_name, "paragraph")
            ? OMNI_FTXUI_ELEMENT_PARAGRAPH
            : OMNI_FTXUI_ELEMENT_TEXT;
        options.text = text;
        return ui_ftxui_wrap_element_as_component(state, &options, null, 0, out_component, interp);
    }

    if (ui_ftxui_str_eq(kind_name, "button")) {
        char* label = null;
        Value* label_value = ui_ftxui_dict_get(props, "label", interp);
        Value* label_err = ui_ftxui_require_stringish(label_value, &label, interp, "ui-ftxui: button requires string-like 'label'");
        if (label_err != null) return label_err;
        Value* track_err = ui_ftxui_track_heap_or_oom(state, label, interp);
        if (track_err != null) return track_err;

        OmniFtxuiComponentCreateOptions options = {};
        options.size = OmniFtxuiComponentCreateOptions.sizeof;
        options.abi_version = OMNI_FTXUI_ABI_VERSION;
        options.kind = OMNI_FTXUI_COMPONENT_BUTTON;
        options.label = label;
        return ui_ftxui_create_component(state, &options, null, 0, out_component, interp);
    }

    if (ui_ftxui_str_eq(kind_name, "input")) {
        char* placeholder = null;
        Value* placeholder_value = ui_ftxui_dict_get(props, "placeholder", interp);
        Value* placeholder_err = ui_ftxui_require_stringish(
            placeholder_value,
            &placeholder,
            interp,
            "ui-ftxui: input requires string-like 'placeholder'"
        );
        if (placeholder_err != null) return placeholder_err;
        Value* track_err = ui_ftxui_track_heap_or_oom(state, placeholder, interp);
        if (track_err != null) return track_err;

        char* text_value = copy_to_heap_c_string("");
        if (text_value == null) return raise_error(interp, "ui-ftxui: out of memory allocating input state");
        track_err = ui_ftxui_track_heap_or_oom(state, text_value, interp);
        if (track_err != null) return track_err;

        OmniFtxuiComponentCreateOptions options = {};
        options.size = OmniFtxuiComponentCreateOptions.sizeof;
        options.abi_version = OMNI_FTXUI_ABI_VERSION;
        options.kind = OMNI_FTXUI_COMPONENT_INPUT;
        options.label = placeholder;
        options.text_value = text_value;
        return ui_ftxui_create_component(state, &options, null, 0, out_component, interp);
    }

    if (ui_ftxui_str_eq(kind_name, "checkbox")) {
        char* label = null;
        Value* label_value = ui_ftxui_dict_get(props, "label", interp);
        Value* label_err = ui_ftxui_require_stringish(label_value, &label, interp, "ui-ftxui: checkbox requires string-like 'label'");
        if (label_err != null) return label_err;
        Value* track_err = ui_ftxui_track_heap_or_oom(state, label, interp);
        if (track_err != null) return track_err;

        bool* checked = (bool*)mem::malloc(bool.sizeof);
        if (checked == null) return raise_error(interp, "ui-ftxui: out of memory allocating checkbox state");
        *checked = !is_falsy(ui_ftxui_dict_get(props, "checked", interp), interp);
        track_err = ui_ftxui_track_heap_or_oom(state, checked, interp);
        if (track_err != null) return track_err;

        OmniFtxuiComponentCreateOptions options = {};
        options.size = OmniFtxuiComponentCreateOptions.sizeof;
        options.abi_version = OMNI_FTXUI_ABI_VERSION;
        options.kind = OMNI_FTXUI_COMPONENT_CHECKBOX;
        options.label = label;
        options.checked_value = checked;
        return ui_ftxui_create_component(state, &options, null, 0, out_component, interp);
    }

    if (ui_ftxui_str_eq(kind_name, "menu")) {
        Value* items_value = ui_ftxui_dict_get(props, "items", interp);
        char** items = null;
        usz item_count = 0;
        Value* items_err = collect_string_sequence(
            items_value,
            &items,
            &item_count,
            interp,
            "ui-ftxui/menu-items-invalid",
            "ui-ftxui: menu 'items' must be a proper list or array of strings/symbols"
        );
        if (items_err != null) return items_err;
        if (items == null || item_count == 0) return raise_error(interp, "ui-ftxui: menu requires at least one item");
        defer free_heap_c_string_items(items, item_count);

        int* selected_index = (int*)mem::malloc(int.sizeof);
        if (selected_index == null) return raise_error(interp, "ui-ftxui: out of memory allocating menu state");
        *selected_index = 0;
        Value* track_err = ui_ftxui_track_heap_or_oom(state, selected_index, interp);
        if (track_err != null) return track_err;

        Value* selected_key = ui_ftxui_dict_get(props, "selected-key", interp);
        if (selected_key != null && !is_nil(selected_key)) {
            if (items_value.tag == ARRAY && items_value.array_val != null) {
                for (usz i = 0; i < items_value.array_val.length; i++) {
                    if (values_equal(items_value.array_val.items[i], selected_key)) {
                        *selected_index = (int)i;
                        break;
                    }
                }
            } else if (is_cons(items_value)) {
                Value* cur = items_value;
                int idx = 0;
                while (is_cons(cur)) {
                    if (values_equal(car(cur), selected_key)) {
                        *selected_index = idx;
                        break;
                    }
                    idx++;
                    cur = cdr(cur);
                }
            }
        }

        OmniFtxuiComponentCreateOptions options = {};
        options.size = OmniFtxuiComponentCreateOptions.sizeof;
        options.abi_version = OMNI_FTXUI_ABI_VERSION;
        options.kind = OMNI_FTXUI_COMPONENT_MENU;
        options.items = items;
        options.item_count = item_count;
        options.selected_index = selected_index;
        return ui_ftxui_create_component(state, &options, null, 0, out_component, interp);
    }

    if (ui_ftxui_str_eq(kind_name, "hbox") || ui_ftxui_str_eq(kind_name, "vbox")) {
        OmniFtxuiComponent** children = null;
        usz child_count = 0;
        Value* child_err = ui_ftxui_collect_children(state, children_value, &children, &child_count, interp);
        if (child_err != null) return child_err;
        defer if (children != null) mem::free(children);

        OmniFtxuiComponentCreateOptions options = {};
        options.size = OmniFtxuiComponentCreateOptions.sizeof;
        options.abi_version = OMNI_FTXUI_ABI_VERSION;
        options.kind = ui_ftxui_str_eq(kind_name, "hbox")
            ? OMNI_FTXUI_COMPONENT_CONTAINER_HORIZONTAL
            : OMNI_FTXUI_COMPONENT_CONTAINER_VERTICAL;
        return ui_ftxui_create_component(state, &options, children, child_count, out_component, interp);
    }

    if (ui_ftxui_str_eq(kind_name, "window")) {
        char* title = null;
        Value* title_value = ui_ftxui_dict_get(props, "title", interp);
        Value* title_err = ui_ftxui_require_stringish(title_value, &title, interp, "ui-ftxui: window requires string-like 'title'");
        if (title_err != null) return title_err;
        Value* track_err = ui_ftxui_track_heap_or_oom(state, title, interp);
        if (track_err != null) return track_err;

        OmniFtxuiComponent** children = null;
        usz child_count = 0;
        Value* child_err = ui_ftxui_collect_children(state, children_value, &children, &child_count, interp);
        if (child_err != null) return child_err;
        defer if (children != null) mem::free(children);

        OmniFtxuiComponentCreateOptions options = {};
        options.size = OmniFtxuiComponentCreateOptions.sizeof;
        options.abi_version = OMNI_FTXUI_ABI_VERSION;
        options.kind = OMNI_FTXUI_COMPONENT_WINDOW;
        options.title = title;
        return ui_ftxui_create_component(state, &options, children, child_count, out_component, interp);
    }

    if (ui_ftxui_str_eq(kind_name, "graph")) {
        Value* series_value = ui_ftxui_dict_get(props, "series", interp);
        double* values = null;
        usz value_count = 0;
        Value* series_err = ui_ftxui_collect_number_sequence(
            series_value,
            &values,
            &value_count,
            interp,
            "ui-ftxui: graph 'series' must be a proper list or array of numbers"
        );
        if (series_err != null) return series_err;

        Value* track_err = ui_ftxui_track_heap_or_oom(state, values, interp);
        if (track_err != null) return track_err;

        OmniFtxuiElement* element = null;
        OmniFtxuiStatus status =
            omni_ftxui_element_graph_from_series(state.context, values, value_count, &element);
        if (status != OMNI_FTXUI_STATUS_OK) {
            return ui_ftxui_raise_status(state, "element_graph_from_series", status, interp);
        }
        defer if (element != null) omni_ftxui_element_destroy(element);

        OmniFtxuiComponent* component = null;
        status = omni_ftxui_component_from_element(state.context, element, &component);
        if (status != OMNI_FTXUI_STATUS_OK) {
            return ui_ftxui_raise_status(state, "component_from_element", status, interp);
        }

        track_err = ui_ftxui_track_component_or_oom(state, component, interp);
        if (track_err != null) return track_err;
        *out_component = component;
        return null;
    }

    char[256] msg_buf;
    char[] msg = io::bprintf(&msg_buf, "ui-ftxui: unsupported node kind '%s'", (ZString)kind_name)!!;
    return raise_error(interp, msg);
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
