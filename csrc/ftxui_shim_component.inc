omni_ftxui_status omni_ftxui_component_create(
    omni_ftxui_context_t* context,
    const omni_ftxui_component_create_options* options,
    omni_ftxui_component_t* const* children,
    size_t child_count,
    omni_ftxui_component_t** out_component
) {
    if (options == nullptr || out_component == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_create: null argument");
    }
    if (!validate_sized_abi(options->size, options->abi_version, sizeof(omni_ftxui_component_create_options))) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_create: invalid option ABI");
    }
    if (child_count > 0 && children == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_create: nonzero child_count with null children");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    (void)children;
    (void)child_count;
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "component_create: FTXUI backend unavailable");
#else
    return status_try(context, "component_create: out of memory", "component_create: exception", [&]() -> omni_ftxui_status {
    ftxui::Component component;
    switch (options->kind) {
        case OMNI_FTXUI_COMPONENT_CONTAINER_HORIZONTAL:
            component = ftxui::Container::Horizontal(copy_components(children, child_count));
            break;
        case OMNI_FTXUI_COMPONENT_CONTAINER_VERTICAL:
            component = ftxui::Container::Vertical(copy_components(children, child_count));
            break;
        case OMNI_FTXUI_COMPONENT_CONTAINER_TAB:
            if (options->selected_index == nullptr) {
                return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_create: tab requires selected_index pointer");
            }
            component = ftxui::Container::Tab(copy_components(children, child_count), options->selected_index);
            break;
        case OMNI_FTXUI_COMPONENT_CONTAINER_STACKED:
            component = ftxui::Container::Stacked(copy_components(children, child_count));
            break;
        case OMNI_FTXUI_COMPONENT_BUTTON:
            component = ftxui::Button(options->label ? options->label : "", [] {});
            break;
        case OMNI_FTXUI_COMPONENT_CHECKBOX:
            if (options->checked_value == nullptr) {
                return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_create: checkbox requires checked_value pointer");
            }
            component = ftxui::Checkbox(options->label ? options->label : "", options->checked_value);
            break;
        case OMNI_FTXUI_COMPONENT_INPUT: {
            auto text = std::make_shared<std::string>(options->text_value ? options->text_value : "");
            component = ftxui::Input(text.get(), options->label ? options->label : "");
            auto* out = make_component(component);
            if (out == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "component_create: allocation failed");
            component_retain_children_keep_alive(out, children, child_count);
            out->keep_alive.push_back(text);
            *out_component = out;
            ctx_clear(context);
            return OMNI_FTXUI_STATUS_OK;
        }
        case OMNI_FTXUI_COMPONENT_MENU:
        case OMNI_FTXUI_COMPONENT_MENU_ENTRY:
        case OMNI_FTXUI_COMPONENT_RADIOBOX:
        case OMNI_FTXUI_COMPONENT_DROPDOWN:
        case OMNI_FTXUI_COMPONENT_TOGGLE: {
            if (options->items == nullptr || options->selected_index == nullptr) {
                return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_create: list component requires items + selected_index");
            }
            auto owned = copy_items(options->items, options->item_count);
            if (options->kind == OMNI_FTXUI_COMPONENT_MENU || options->kind == OMNI_FTXUI_COMPONENT_MENU_ENTRY) {
                component = ftxui::Menu(owned.get(), options->selected_index);
            } else if (options->kind == OMNI_FTXUI_COMPONENT_RADIOBOX) {
                component = ftxui::Radiobox(owned.get(), options->selected_index);
            } else if (options->kind == OMNI_FTXUI_COMPONENT_DROPDOWN) {
                component = ftxui::Dropdown(owned.get(), options->selected_index);
            } else {
                component = ftxui::Toggle(owned.get(), options->selected_index);
            }
            auto* out = make_component(component);
            if (out == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "component_create: allocation failed");
            component_retain_children_keep_alive(out, children, child_count);
            out->keep_alive.push_back(owned);
            *out_component = out;
            ctx_clear(context);
            return OMNI_FTXUI_STATUS_OK;
        }
        case OMNI_FTXUI_COMPONENT_SLIDER_INT:
            if (options->int_value == nullptr) {
                return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_create: int slider requires int_value pointer");
            }
            component = ftxui::Slider(
                options->label ? options->label : "",
                options->int_value,
                options->int_min,
                options->int_max,
                options->int_step <= 0 ? 1 : options->int_step
            );
            break;
        case OMNI_FTXUI_COMPONENT_SLIDER_FLOAT:
            if (options->float_value == nullptr) {
                return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_create: float slider requires float_value pointer");
            }
            component = ftxui::Slider(
                options->label ? options->label : "",
                options->float_value,
                options->float_min,
                options->float_max,
                options->float_step <= 0.0f ? 0.01f : options->float_step
            );
            break;
        case OMNI_FTXUI_COMPONENT_RENDERER:
            if (child_count < 1 || children == nullptr || children[0] == nullptr) {
                return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_create: renderer requires one base child");
            }
            component = ftxui::Renderer(children[0]->component, [base = children[0]->component] { return base->Render(); });
            break;
        case OMNI_FTXUI_COMPONENT_WINDOW: {
            struct WindowState {
                std::shared_ptr<std::string> title;
                std::shared_ptr<int> default_left;
                std::shared_ptr<int> default_top;
                std::shared_ptr<int> default_width;
                std::shared_ptr<int> default_height;
                std::shared_ptr<bool> default_resize_left;
                std::shared_ptr<bool> default_resize_right;
                std::shared_ptr<bool> default_resize_top;
                std::shared_ptr<bool> default_resize_down;
            };

            auto state = std::make_shared<WindowState>();
            state->title = std::make_shared<std::string>(options->title ? options->title : (options->label ? options->label : ""));
            state->default_left = std::make_shared<int>(0);
            state->default_top = std::make_shared<int>(0);
            state->default_width = std::make_shared<int>(20);
            state->default_height = std::make_shared<int>(10);
            state->default_resize_left = std::make_shared<bool>(true);
            state->default_resize_right = std::make_shared<bool>(true);
            state->default_resize_top = std::make_shared<bool>(true);
            state->default_resize_down = std::make_shared<bool>(true);

            ftxui::WindowOptions window_options;
            if (child_count >= 1 && children != nullptr && children[0] != nullptr) {
                window_options.inner = children[0]->component;
            } else {
                window_options.inner = ftxui::Renderer([] { return ftxui::emptyElement(); });
            }
            window_options.title = state->title->c_str();
            window_options.left = options->left ? options->left : state->default_left.get();
            window_options.top = options->top ? options->top : state->default_top.get();
            window_options.width = options->width ? options->width : state->default_width.get();
            window_options.height = options->height ? options->height : state->default_height.get();
            window_options.resize_left = options->resize_left ? options->resize_left : state->default_resize_left.get();
            window_options.resize_right = options->resize_right ? options->resize_right : state->default_resize_right.get();
            window_options.resize_top = options->resize_top ? options->resize_top : state->default_resize_top.get();
            window_options.resize_down = options->resize_down ? options->resize_down : state->default_resize_down.get();

            component = ftxui::Window(window_options);
            auto* out = make_component(component);
            if (out == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "component_create: allocation failed");
            component_retain_children_keep_alive(out, children, child_count);
            out->keep_alive.push_back(state);
            *out_component = out;
            ctx_clear(context);
            return OMNI_FTXUI_STATUS_OK;
        }
        case OMNI_FTXUI_COMPONENT_COLLAPSIBLE: {
            if (child_count < 1 || children == nullptr || children[0] == nullptr) {
                return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_create: collapsible requires one child");
            }
            auto default_visible = std::make_shared<bool>(false);
            bool* visible = options->visible_ptr ? options->visible_ptr : default_visible.get();
            component = ftxui::Collapsible(
                options->label ? options->label : (options->title ? options->title : ""),
                children[0]->component,
                visible
            );
            auto* out = make_component(component);
            if (out == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "component_create: allocation failed");
            component_retain_children_keep_alive(out, children, child_count);
            if (!options->visible_ptr) out->keep_alive.push_back(default_visible);
            *out_component = out;
            ctx_clear(context);
            return OMNI_FTXUI_STATUS_OK;
        }
        case OMNI_FTXUI_COMPONENT_MODAL: {
            if (child_count < 2 || children == nullptr || children[0] == nullptr || children[1] == nullptr) {
                return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_create: modal requires main and modal children");
            }
            auto default_visible = std::make_shared<bool>(false);
            bool* visible = options->visible_ptr ? options->visible_ptr : default_visible.get();
            component = ftxui::Modal(children[0]->component, children[1]->component, visible);
            auto* out = make_component(component);
            if (out == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "component_create: allocation failed");
            component_retain_children_keep_alive(out, children, child_count);
            if (!options->visible_ptr) out->keep_alive.push_back(default_visible);
            *out_component = out;
            ctx_clear(context);
            return OMNI_FTXUI_STATUS_OK;
        }
        case OMNI_FTXUI_COMPONENT_HOVERABLE: {
            if (child_count < 1 || children == nullptr || children[0] == nullptr) {
                return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_create: hoverable requires one child");
            }
            auto default_hover = std::make_shared<bool>(false);
            bool* hover = options->hover_value ? options->hover_value : default_hover.get();
            component = ftxui::Hoverable(children[0]->component, hover);
            auto* out = make_component(component);
            if (out == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "component_create: allocation failed");
            component_retain_children_keep_alive(out, children, child_count);
            if (!options->hover_value) out->keep_alive.push_back(default_hover);
            *out_component = out;
            ctx_clear(context);
            return OMNI_FTXUI_STATUS_OK;
        }
        case OMNI_FTXUI_COMPONENT_RESIZABLE_SPLIT: {
            if (child_count < 2 || children == nullptr || children[0] == nullptr || children[1] == nullptr) {
                return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_create: resizable split requires main and back children");
            }
            ftxui::Direction direction = map_direction((omni_ftxui_direction)options->split_direction);
            auto default_main_size = std::make_shared<int>(
                (direction == ftxui::Direction::Left || direction == ftxui::Direction::Right) ? 20 : 10
            );

            ftxui::ResizableSplitOption split;
            split.main = children[0]->component;
            split.back = children[1]->component;
            split.direction = direction;
            split.main_size = options->main_size ? options->main_size : default_main_size.get();

            component = ftxui::ResizableSplit(split);
            auto* out = make_component(component);
            if (out == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "component_create: allocation failed");
            component_retain_children_keep_alive(out, children, child_count);
            if (!options->main_size) out->keep_alive.push_back(default_main_size);
            *out_component = out;
            ctx_clear(context);
            return OMNI_FTXUI_STATUS_OK;
        }
        default:
            return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_create: unknown component kind");
    }
    auto* out = make_component(component);
    if (out == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "component_create: allocation failed");
    component_retain_children_keep_alive(out, children, child_count);
    *out_component = out;
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

void omni_ftxui_component_destroy(omni_ftxui_component_t* component) {
    delete component;
}

omni_ftxui_status omni_ftxui_component_add_child(
    omni_ftxui_context_t* context,
    omni_ftxui_component_t* parent,
    omni_ftxui_component_t* child
) {
    if (parent == nullptr || child == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_add_child: null argument");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "component_add_child: FTXUI backend unavailable");
#else
    return status_try(context, "component_add_child: out of memory", "component_add_child: exception", [&]() -> omni_ftxui_status {
    parent->component->Add(child->component);
    component_retain_keep_alive(parent, child);
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

omni_ftxui_status omni_ftxui_component_take_focus(
    omni_ftxui_context_t* context,
    omni_ftxui_component_t* component
) {
    if (component == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_take_focus: null component");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "component_take_focus: FTXUI backend unavailable");
#else
    return status_try(context, "component_take_focus: out of memory", "component_take_focus: exception", [&]() -> omni_ftxui_status {
    component->component->TakeFocus();
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

omni_ftxui_status omni_ftxui_component_handle_event(
    omni_ftxui_context_t* context,
    omni_ftxui_component_t* component,
    const omni_ftxui_event_t* event,
    bool* out_handled
) {
    if (component == nullptr || event == nullptr || out_handled == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_handle_event: null argument");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "component_handle_event: FTXUI backend unavailable");
#else
    return status_try(context, "component_handle_event: out of memory", "component_handle_event: exception", [&]() -> omni_ftxui_status {
    ctx_clear(context);
    *out_handled = component->component->OnEvent(event->event);
    if (context != nullptr && context->last_error_code != OMNI_FTXUI_STATUS_OK) {
        return context->last_error_code;
    }
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

omni_ftxui_status omni_ftxui_component_wrap_renderer(
    omni_ftxui_context_t* context,
    omni_ftxui_component_t* base,
    const omni_ftxui_callback_options* callbacks,
    omni_ftxui_component_t** out_component
) {
    if (base == nullptr || callbacks == nullptr || out_component == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_wrap_renderer: null argument");
    }
    if (!validate_sized_abi(callbacks->size, callbacks->abi_version, sizeof(omni_ftxui_callback_options))) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_wrap_renderer: invalid callback option ABI");
    }
    if (callbacks->render_cb == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_wrap_renderer: render callback required");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "component_wrap_renderer: FTXUI backend unavailable");
#else
    return status_try(context, "component_wrap_renderer: out of memory", "component_wrap_renderer: exception", [&]() -> omni_ftxui_status {
    struct RenderState {
        omni_ftxui_context_t* context;
        omni_ftxui_render_callback_fn callback;
        void* user_data;
    };
    auto state = std::make_shared<RenderState>();
    state->context = context;
    state->callback = callbacks->render_cb;
    state->user_data = callbacks->user_data;

    auto wrapped = ftxui::Renderer(base->component, [state] {
        omni_ftxui_element_t* out = nullptr;
        omni_ftxui_status r = OMNI_FTXUI_STATUS_OK;
        try {
            r = state->callback(state->user_data, state->context, &out);
            std::unique_ptr<omni_ftxui_element_t, decltype(&omni_ftxui_element_destroy)> owned(
                out,
                omni_ftxui_element_destroy
            );
            if (r != OMNI_FTXUI_STATUS_OK || owned == nullptr) return ftxui::text("");
            return owned->element;
        } catch (const std::bad_alloc&) {
            ctx_fail(state->context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "render callback: out of memory");
            return ftxui::text("");
        } catch (const std::exception&) {
            ctx_fail(state->context, OMNI_FTXUI_STATUS_INTERNAL_ERROR, "render callback: exception");
            return ftxui::text("");
        } catch (...) {
            ctx_fail(state->context, OMNI_FTXUI_STATUS_INTERNAL_ERROR, "render callback: unknown exception");
            return ftxui::text("");
        }
    });
    auto* out = make_component(std::move(wrapped));
    if (out == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "component_wrap_renderer: allocation failed");
    component_retain_keep_alive(out, base);
    out->keep_alive.push_back(state);
    *out_component = out;
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

omni_ftxui_status omni_ftxui_component_wrap_event_handler(
    omni_ftxui_context_t* context,
    omni_ftxui_component_t* base,
    const omni_ftxui_callback_options* callbacks,
    omni_ftxui_component_t** out_component
) {
    if (base == nullptr || callbacks == nullptr || out_component == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_wrap_event_handler: null argument");
    }
    if (!validate_sized_abi(callbacks->size, callbacks->abi_version, sizeof(omni_ftxui_callback_options))) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_wrap_event_handler: invalid callback option ABI");
    }
    if (callbacks->event_cb == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_wrap_event_handler: event callback required");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "component_wrap_event_handler: FTXUI backend unavailable");
#else
    return status_try(context, "component_wrap_event_handler: out of memory", "component_wrap_event_handler: exception", [&]() -> omni_ftxui_status {
    struct EventState {
        omni_ftxui_context_t* context;
        omni_ftxui_event_callback_fn callback;
        void* user_data;
    };
    auto state = std::make_shared<EventState>();
    state->context = context;
    state->callback = callbacks->event_cb;
    state->user_data = callbacks->user_data;

    auto wrapped = ftxui::CatchEvent(base->component, [state](ftxui::Event e) {
        try {
            omni_ftxui_event_t event(std::move(e));
            int handled = state->callback(state->user_data, state->context, &event);
            return handled != 0;
        } catch (const std::bad_alloc&) {
            ctx_fail(state->context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "event callback: out of memory");
            return false;
        } catch (const std::exception&) {
            ctx_fail(state->context, OMNI_FTXUI_STATUS_INTERNAL_ERROR, "event callback: exception");
            return false;
        } catch (...) {
            ctx_fail(state->context, OMNI_FTXUI_STATUS_INTERNAL_ERROR, "event callback: unknown exception");
            return false;
        }
    });
    auto* out = make_component(std::move(wrapped));
    if (out == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "component_wrap_event_handler: allocation failed");
    }
    component_retain_keep_alive(out, base);
    out->keep_alive.push_back(state);
    *out_component = out;
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

omni_ftxui_status omni_ftxui_component_wrap_action(
    omni_ftxui_context_t* context,
    omni_ftxui_component_t* base,
    const omni_ftxui_callback_options* callbacks,
    omni_ftxui_component_t** out_component
) {
    if (base == nullptr || callbacks == nullptr || out_component == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_wrap_action: null argument");
    }
    if (!validate_sized_abi(callbacks->size, callbacks->abi_version, sizeof(omni_ftxui_callback_options))) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_wrap_action: invalid callback option ABI");
    }
    if (callbacks->action_cb == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_wrap_action: action callback required");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "component_wrap_action: FTXUI backend unavailable");
#else
    return status_try(context, "component_wrap_action: out of memory", "component_wrap_action: exception", [&]() -> omni_ftxui_status {
    struct ActionState {
        omni_ftxui_context_t* context;
        omni_ftxui_action_callback_fn callback;
        void* user_data;
    };
    auto state = std::make_shared<ActionState>();
    state->context = context;
    state->callback = callbacks->action_cb;
    state->user_data = callbacks->user_data;

    auto wrapped = ftxui::CatchEvent(base->component, [state](ftxui::Event e) {
        bool should_fire = e == ftxui::Event::Return;
        if (!should_fire && e.is_character()) {
            should_fire = e.character() == " ";
        }
        if (!should_fire) return false;
        try {
            state->callback(state->user_data);
            return true;
        } catch (const std::bad_alloc&) {
            ctx_fail(state->context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "action callback: out of memory");
            return false;
        } catch (const std::exception&) {
            ctx_fail(state->context, OMNI_FTXUI_STATUS_INTERNAL_ERROR, "action callback: exception");
            return false;
        } catch (...) {
            ctx_fail(state->context, OMNI_FTXUI_STATUS_INTERNAL_ERROR, "action callback: unknown exception");
            return false;
        }
    });
    auto* out = make_component(std::move(wrapped));
    if (out == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "component_wrap_action: allocation failed");
    }
    component_retain_keep_alive(out, base);
    out->keep_alive.push_back(state);
    *out_component = out;
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

omni_ftxui_status omni_ftxui_component_wrap_message_action(
    omni_ftxui_context_t* context,
    omni_ftxui_screen_t* screen,
    omni_ftxui_component_t* base,
    const char* message,
    omni_ftxui_component_t** out_component
) {
    if (screen == nullptr || base == nullptr || message == nullptr || out_component == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_wrap_message_action: null argument");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "component_wrap_message_action: FTXUI backend unavailable");
#else
    return status_try(context, "component_wrap_message_action: out of memory", "component_wrap_message_action: exception", [&]() -> omni_ftxui_status {
    struct MessageActionState {
        omni_ftxui_context_t* context;
        std::shared_ptr<ftxui::ScreenInteractive> screen;
        std::string message;
    };
    auto state = std::make_shared<MessageActionState>();
    state->context = context;
    state->screen = screen->screen;
    state->message = message;

    auto wrapped = ftxui::CatchEvent(base->component, [state](ftxui::Event e) {
        bool should_fire = e == ftxui::Event::Return;
        if (!should_fire && e.is_character()) {
            should_fire = e.character() == " ";
        }
        if (!should_fire) return false;
        try {
            state->screen->PostEvent(ftxui::Event::Special(state->message));
            return true;
        } catch (const std::bad_alloc&) {
            ctx_fail(state->context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "message action: out of memory");
            return false;
        } catch (const std::exception&) {
            ctx_fail(state->context, OMNI_FTXUI_STATUS_INTERNAL_ERROR, "message action: exception");
            return false;
        } catch (...) {
            ctx_fail(state->context, OMNI_FTXUI_STATUS_INTERNAL_ERROR, "message action: unknown exception");
            return false;
        }
    });
    auto* out = make_component(std::move(wrapped));
    if (out == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "component_wrap_message_action: allocation failed");
    }
    component_retain_keep_alive(out, base);
    out->keep_alive.push_back(state);
    *out_component = out;
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

omni_ftxui_status omni_ftxui_component_from_element(
    omni_ftxui_context_t* context,
    omni_ftxui_element_t* element,
    omni_ftxui_component_t** out_component
) {
    if (element == nullptr || out_component == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_from_element: null argument");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "component_from_element: FTXUI backend unavailable");
#else
    return status_try(context, "component_from_element: out of memory", "component_from_element: exception", [&]() -> omni_ftxui_status {
    auto rendered = element->element;
    auto component = ftxui::Renderer([rendered] { return rendered; });
    auto* out = make_component(std::move(component));
    if (out == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "component_from_element: allocation failed");
    *out_component = out;
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

omni_ftxui_status omni_ftxui_component_wrap_quit_keys(
    omni_ftxui_context_t* context,
    omni_ftxui_screen_t* screen,
    omni_ftxui_component_t* base,
    omni_ftxui_component_t** out_component
) {
    if (screen == nullptr || base == nullptr || out_component == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_wrap_quit_keys: null argument");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "component_wrap_quit_keys: FTXUI backend unavailable");
#else
    return status_try(context, "component_wrap_quit_keys: out of memory", "component_wrap_quit_keys: exception", [&]() -> omni_ftxui_status {
    auto screen_ref = screen->screen;
    if (!screen_ref) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_wrap_quit_keys: invalid screen");
    }
    auto wrapped = ftxui::CatchEvent(base->component, [screen_ref, context](ftxui::Event e) {
        try {
            if (e == ftxui::Event::Escape) {
                screen_ref->ExitLoopClosure()();
                return true;
            }
            if (e.is_character()) {
                std::string ch = e.character();
                if (ch == "q" || ch == "Q") {
                    screen_ref->ExitLoopClosure()();
                    return true;
                }
            }
            return false;
        } catch (const std::bad_alloc&) {
            ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "quit-key wrapper: out of memory");
            return false;
        } catch (const std::exception&) {
            ctx_fail(context, OMNI_FTXUI_STATUS_INTERNAL_ERROR, "quit-key wrapper: exception");
            return false;
        } catch (...) {
            ctx_fail(context, OMNI_FTXUI_STATUS_INTERNAL_ERROR, "quit-key wrapper: unknown exception");
            return false;
        }
    });
    auto* out = make_component(std::move(wrapped));
    if (out == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "component_wrap_quit_keys: allocation failed");
    component_retain_keep_alive(out, base);
    out->keep_alive.push_back(screen_ref);
    *out_component = out;
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

omni_ftxui_status omni_ftxui_component_wrap_maybe(
    omni_ftxui_context_t* context,
    omni_ftxui_component_t* base,
    bool* visible_ptr,
    omni_ftxui_component_t** out_component
) {
    if (base == nullptr || visible_ptr == nullptr || out_component == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "component_wrap_maybe: null argument");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "component_wrap_maybe: FTXUI backend unavailable");
#else
    return status_try(context, "component_wrap_maybe: out of memory", "component_wrap_maybe: exception", [&]() -> omni_ftxui_status {
    auto wrapped = ftxui::Maybe(base->component, visible_ptr);
    auto* out = make_component(std::move(wrapped));
    if (out == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "component_wrap_maybe: allocation failed");
    component_retain_keep_alive(out, base);
    *out_component = out;
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}
