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
        if (state.heap_ptr_count == usz.max) return false;
        usz needed = state.heap_ptr_count + 1;
        usz new_cap = state.heap_ptr_capacity == 0 ? 8 : state.heap_ptr_capacity;
        while (new_cap < needed) {
            if (new_cap > usz.max / 2) {
                new_cap = needed;
                break;
            }
            new_cap *= 2;
        }
        if (new_cap > usz.max / void*.sizeof) return false;
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
        if (state.component_count == usz.max) return false;
        usz needed = state.component_count + 1;
        usz new_cap = state.component_capacity == 0 ? 8 : state.component_capacity;
        while (new_cap < needed) {
            if (new_cap > usz.max / 2) {
                new_cap = needed;
                break;
            }
            new_cap *= 2;
        }
        if (new_cap > usz.max / OmniFtxuiComponent*.sizeof) return false;
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
    char[] msg = "ui-ftxui operation failed";
    if (try char[] formatted = io::bprintf(
        &msg_buf,
        "ui-ftxui %s failed (%s)%s%s",
        (ZString)op_name,
        status_text,
        separator,
        detail_text
    )) {
        msg = formatted;
    }
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
