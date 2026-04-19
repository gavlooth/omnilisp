int omni_ftxui_backend_available(void) {
    return OMNI_FTXUI_HAS_BACKEND ? 1 : 0;
}

const char* omni_ftxui_status_name(omni_ftxui_status status) {
    switch (status) {
        case OMNI_FTXUI_STATUS_OK: return "ok";
        case OMNI_FTXUI_STATUS_INVALID_ARGUMENT: return "invalid_argument";
        case OMNI_FTXUI_STATUS_OUT_OF_MEMORY: return "out_of_memory";
        case OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE: return "backend_unavailable";
        case OMNI_FTXUI_STATUS_NOT_SUPPORTED: return "not_supported";
        case OMNI_FTXUI_STATUS_INTERNAL_ERROR:
        default: return "internal_error";
    }
}

omni_ftxui_status omni_ftxui_context_create(omni_ftxui_context_t** out_context) {
    if (out_context == nullptr) return OMNI_FTXUI_STATUS_INVALID_ARGUMENT;
    try {
        auto* ctx = new (std::nothrow) omni_ftxui_context_t();
        if (ctx == nullptr) return OMNI_FTXUI_STATUS_OUT_OF_MEMORY;
        ctx->last_error_code = OMNI_FTXUI_STATUS_OK;
        *out_context = ctx;
        return OMNI_FTXUI_STATUS_OK;
    } catch (const std::bad_alloc&) {
        return OMNI_FTXUI_STATUS_OUT_OF_MEMORY;
    } catch (...) {
        return OMNI_FTXUI_STATUS_INTERNAL_ERROR;
    }
}

void omni_ftxui_context_destroy(omni_ftxui_context_t* context) {
    delete context;
}

omni_ftxui_status omni_ftxui_context_last_error_code(const omni_ftxui_context_t* context) {
    if (context == nullptr) return OMNI_FTXUI_STATUS_INVALID_ARGUMENT;
    return context->last_error_code;
}

const char* omni_ftxui_context_last_error_message(const omni_ftxui_context_t* context) {
    if (context == nullptr) return "null context";
    return context->last_error_message.c_str();
}

void omni_ftxui_context_clear_error(omni_ftxui_context_t* context) {
    ctx_clear(context);
}

omni_ftxui_status omni_ftxui_event_create(
    omni_ftxui_context_t* context,
    const omni_ftxui_event_spec* spec,
    omni_ftxui_event_t** out_event
) {
    if (spec == nullptr || out_event == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "event_create: null argument");
    }
    if (!validate_sized_abi(spec->size, spec->abi_version, sizeof(omni_ftxui_event_spec))) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "event_create: invalid option ABI");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    (void)spec;
    (void)out_event;
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "event_create: FTXUI backend unavailable");
#else
    return status_try(context, "event_create: out of memory", "event_create: exception", [&]() -> omni_ftxui_status {
    ftxui::Event event = ftxui::Event::Custom;
    switch (spec->kind) {
        case OMNI_FTXUI_EVENT_CUSTOM: event = ftxui::Event::Custom; break;
        case OMNI_FTXUI_EVENT_CHARACTER: event = ftxui::Event::Character(spec->text ? spec->text : ""); break;
        case OMNI_FTXUI_EVENT_SPECIAL: event = ftxui::Event::Special(spec->text ? spec->text : ""); break;
        case OMNI_FTXUI_EVENT_ARROW_LEFT: event = ftxui::Event::ArrowLeft; break;
        case OMNI_FTXUI_EVENT_ARROW_RIGHT: event = ftxui::Event::ArrowRight; break;
        case OMNI_FTXUI_EVENT_ARROW_UP: event = ftxui::Event::ArrowUp; break;
        case OMNI_FTXUI_EVENT_ARROW_DOWN: event = ftxui::Event::ArrowDown; break;
        case OMNI_FTXUI_EVENT_RETURN: event = ftxui::Event::Return; break;
        case OMNI_FTXUI_EVENT_ESCAPE: event = ftxui::Event::Escape; break;
        case OMNI_FTXUI_EVENT_TAB: event = ftxui::Event::Tab; break;
        case OMNI_FTXUI_EVENT_TAB_REVERSE: event = ftxui::Event::TabReverse; break;
        case OMNI_FTXUI_EVENT_BACKSPACE: event = ftxui::Event::Backspace; break;
        case OMNI_FTXUI_EVENT_DELETE: event = ftxui::Event::Delete; break;
        case OMNI_FTXUI_EVENT_HOME: event = ftxui::Event::Home; break;
        case OMNI_FTXUI_EVENT_END: event = ftxui::Event::End; break;
        case OMNI_FTXUI_EVENT_PAGE_UP: event = ftxui::Event::PageUp; break;
        case OMNI_FTXUI_EVENT_PAGE_DOWN: event = ftxui::Event::PageDown; break;
        case OMNI_FTXUI_EVENT_F1: event = ftxui::Event::F1; break;
        case OMNI_FTXUI_EVENT_F2: event = ftxui::Event::F2; break;
        case OMNI_FTXUI_EVENT_F3: event = ftxui::Event::F3; break;
        case OMNI_FTXUI_EVENT_F4: event = ftxui::Event::F4; break;
        case OMNI_FTXUI_EVENT_F5: event = ftxui::Event::F5; break;
        case OMNI_FTXUI_EVENT_F6: event = ftxui::Event::F6; break;
        case OMNI_FTXUI_EVENT_F7: event = ftxui::Event::F7; break;
        case OMNI_FTXUI_EVENT_F8: event = ftxui::Event::F8; break;
        case OMNI_FTXUI_EVENT_F9: event = ftxui::Event::F9; break;
        case OMNI_FTXUI_EVENT_F10: event = ftxui::Event::F10; break;
        case OMNI_FTXUI_EVENT_F11: event = ftxui::Event::F11; break;
        case OMNI_FTXUI_EVENT_F12: event = ftxui::Event::F12; break;
        default:
            return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "event_create: unknown event kind");
    }
    auto* evt = new (std::nothrow) omni_ftxui_event_t(std::move(event));
    if (evt == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "event_create: allocation failed");
    *out_event = evt;
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

void omni_ftxui_event_destroy(omni_ftxui_event_t* event) {
    delete event;
}

omni_ftxui_status omni_ftxui_screen_create(
    omni_ftxui_context_t* context,
    const omni_ftxui_screen_create_options* options,
    omni_ftxui_screen_t** out_screen
) {
    if (options == nullptr || out_screen == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "screen_create: null argument");
    }
    if (!validate_sized_abi(options->size, options->abi_version, sizeof(omni_ftxui_screen_create_options))) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "screen_create: invalid option ABI");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "screen_create: FTXUI backend unavailable");
#else
    return status_try(context, "screen_create: out of memory", "screen_create: exception", [&]() -> omni_ftxui_status {
    if (options->handle_piped_input) {
        return ctx_fail(
            context,
            OMNI_FTXUI_STATUS_NOT_SUPPORTED,
            "screen_create: handle_piped_input is not supported by the FTXUI backend"
        );
    }

    std::shared_ptr<ftxui::ScreenInteractive> screen;
    switch (options->mode) {
        case OMNI_FTXUI_SCREEN_FULLSCREEN:
            screen.reset(new ftxui::ScreenInteractive(ftxui::ScreenInteractive::Fullscreen()));
            break;
        case OMNI_FTXUI_SCREEN_FULLSCREEN_PRIMARY:
            screen.reset(new ftxui::ScreenInteractive(ftxui::ScreenInteractive::FullscreenPrimaryScreen()));
            break;
        case OMNI_FTXUI_SCREEN_FULLSCREEN_ALTERNATE:
            screen.reset(new ftxui::ScreenInteractive(ftxui::ScreenInteractive::FullscreenAlternateScreen()));
            break;
        case OMNI_FTXUI_SCREEN_FIT_COMPONENT:
            screen.reset(new ftxui::ScreenInteractive(ftxui::ScreenInteractive::FitComponent()));
            break;
        case OMNI_FTXUI_SCREEN_TERMINAL_OUTPUT:
            screen.reset(new ftxui::ScreenInteractive(ftxui::ScreenInteractive::TerminalOutput()));
            break;
        default:
            return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "screen_create: unknown mode");
    }

    screen->TrackMouse(options->track_mouse);

    auto* out = new (std::nothrow) omni_ftxui_screen_t();
    if (out == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "screen_create: allocation failed");
    out->screen = std::move(screen);
    *out_screen = out;
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

void omni_ftxui_screen_destroy(omni_ftxui_screen_t* screen) {
    delete screen;
}

omni_ftxui_status omni_ftxui_screen_loop(
    omni_ftxui_context_t* context,
    omni_ftxui_screen_t* screen,
    omni_ftxui_component_t* root
) {
    if (screen == nullptr || root == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "screen_loop: null argument");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "screen_loop: FTXUI backend unavailable");
#else
    return status_try(context, "screen_loop: out of memory", "screen_loop: exception", [&]() -> omni_ftxui_status {
    screen->screen->Loop(root->component);
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

omni_ftxui_status omni_ftxui_screen_post_event(
    omni_ftxui_context_t* context,
    omni_ftxui_screen_t* screen,
    const omni_ftxui_event_t* event
) {
    if (screen == nullptr || event == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "screen_post_event: null argument");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "screen_post_event: FTXUI backend unavailable");
#else
    return status_try(context, "screen_post_event: out of memory", "screen_post_event: exception", [&]() -> omni_ftxui_status {
    screen->screen->PostEvent(event->event);
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

omni_ftxui_status omni_ftxui_screen_exit(omni_ftxui_context_t* context, omni_ftxui_screen_t* screen) {
    if (screen == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "screen_exit: null screen");
#if !OMNI_FTXUI_HAS_BACKEND
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "screen_exit: FTXUI backend unavailable");
#else
    return status_try(context, "screen_exit: out of memory", "screen_exit: exception", [&]() -> omni_ftxui_status {
    screen->screen->Exit();
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

omni_ftxui_status omni_ftxui_screen_request_animation_frame(
    omni_ftxui_context_t* context,
    omni_ftxui_screen_t* screen
) {
    if (screen == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "screen_request_animation_frame: null screen");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "screen_request_animation_frame: FTXUI backend unavailable");
#else
    return status_try(context, "screen_request_animation_frame: out of memory", "screen_request_animation_frame: exception", [&]() -> omni_ftxui_status {
    screen->screen->RequestAnimationFrame();
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

omni_ftxui_status omni_ftxui_screen_set_track_mouse(
    omni_ftxui_context_t* context,
    omni_ftxui_screen_t* screen,
    bool enabled
) {
    if (screen == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "screen_set_track_mouse: null screen");
#if !OMNI_FTXUI_HAS_BACKEND
    (void)enabled;
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "screen_set_track_mouse: FTXUI backend unavailable");
#else
    return status_try(context, "screen_set_track_mouse: out of memory", "screen_set_track_mouse: exception", [&]() -> omni_ftxui_status {
    screen->screen->TrackMouse(enabled);
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

omni_ftxui_status omni_ftxui_screen_set_handle_piped_input(
    omni_ftxui_context_t* context,
    omni_ftxui_screen_t* screen,
    bool enabled
) {
    if (screen == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "screen_set_handle_piped_input: null screen");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    (void)enabled;
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "screen_set_handle_piped_input: FTXUI backend unavailable");
#else
    return status_try(context, "screen_set_handle_piped_input: out of memory", "screen_set_handle_piped_input: exception", [&]() -> omni_ftxui_status {
    if (enabled) {
        return ctx_fail(
            context,
            OMNI_FTXUI_STATUS_NOT_SUPPORTED,
            "screen_set_handle_piped_input: handle_piped_input is not supported by the FTXUI backend"
        );
    }
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

omni_ftxui_status omni_ftxui_app_create(
    omni_ftxui_context_t* context,
    omni_ftxui_screen_t* screen,
    omni_ftxui_component_t* root,
    omni_ftxui_app_t** out_app
) {
    if (screen == nullptr || root == nullptr || out_app == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "app_create: null argument");
    }
    return status_try(context, "app_create: out of memory", "app_create: exception", [&]() -> omni_ftxui_status {
    auto* app = new (std::nothrow) omni_ftxui_app_t{screen, root};
    if (app == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "app_create: allocation failed");
    *out_app = app;
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
}

void omni_ftxui_app_destroy(omni_ftxui_app_t* app) {
    delete app;
}

omni_ftxui_status omni_ftxui_app_run(omni_ftxui_context_t* context, omni_ftxui_app_t* app) {
    if (app == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "app_run: null app");
    return status_try(context, "app_run: out of memory", "app_run: exception", [&]() -> omni_ftxui_status {
    return omni_ftxui_screen_loop(context, app->screen, app->root);
    });
}

omni_ftxui_status omni_ftxui_app_exit(omni_ftxui_context_t* context, omni_ftxui_app_t* app) {
    if (app == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "app_exit: null app");
    return status_try(context, "app_exit: out of memory", "app_exit: exception", [&]() -> omni_ftxui_status {
    return omni_ftxui_screen_exit(context, app->screen);
    });
}

omni_ftxui_status omni_ftxui_app_post_event(
    omni_ftxui_context_t* context,
    omni_ftxui_app_t* app,
    const omni_ftxui_event_t* event
) {
    if (app == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "app_post_event: null app");
    return status_try(context, "app_post_event: out of memory", "app_post_event: exception", [&]() -> omni_ftxui_status {
    return omni_ftxui_screen_post_event(context, app->screen, event);
    });
}
