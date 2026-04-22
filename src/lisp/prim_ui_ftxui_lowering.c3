module lisp;

import std::core::mem;
import std::io;

fn Value* ui_ftxui_lower_node(
    UiFtxuiRunState* state,
    Value* node,
    OmniFtxuiComponent** out_component,
    Interp* interp
) {
    if (node == null || !is_dict(node)) {
        return ftxui_raise(interp, "ui/arg-mismatch", "ui-ftxui: expected ui node dictionary");
    }

    Value* kind_value = ui_ftxui_dict_get(node, "kind", interp);
    Value* props = ui_ftxui_dict_get(node, "props", interp);
    Value* children_value = ui_ftxui_dict_get(node, "children", interp);
    char[] kind_name = ui_ftxui_kind_name(kind_value, interp);

    if (kind_name.len == 0) return ftxui_raise(interp, "ui/arg-mismatch", "ui-ftxui: node kind must be string or symbol");
    if (props == null || !is_dict(props)) return ftxui_raise(interp, "ui/arg-mismatch", "ui-ftxui: node props must be dictionary");

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
        if (text_value == null) return ftxui_raise(interp, "ui/out-of-memory", "ui-ftxui: out of memory allocating input state");
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
        if (checked == null) return ftxui_raise(interp, "ui/out-of-memory", "ui-ftxui: out of memory allocating checkbox state");
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
        if (items == null || item_count == 0) return ftxui_raise(interp, "ui/arg-mismatch", "ui-ftxui: menu requires at least one item");
        if (item_count > (usz)int.max) return ftxui_raise(interp, "ui/range-error", "ui-ftxui: menu has too many items");
        defer free_heap_c_string_items(items, item_count);

        int* selected_index = (int*)mem::malloc(int.sizeof);
        if (selected_index == null) return ftxui_raise(interp, "ui/out-of-memory", "ui-ftxui: out of memory allocating menu state");
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
                usz idx = 0;
                while (is_cons(cur)) {
                    if (values_equal(car(cur), selected_key)) {
                        *selected_index = (int)idx;
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
    char[] msg;
    if (try char[] formatted = io::bprintf(&msg_buf, "ui-ftxui: unsupported node kind '%s'", (ZString)kind_name)) {
        msg = formatted;
    } else {
        msg = "ui-ftxui: unsupported node kind";
    }
    return runtime_raise(interp, "runtime/evaluation-error", msg);
}
