#include "ftxui_shim.h"

#include <algorithm>
#include <cmath>
#include <memory>
#include <new>
#include <string>
#include <utility>
#include <vector>

#if __has_include(<ftxui/component/component.hpp>) && \
    __has_include(<ftxui/component/screen_interactive.hpp>) && \
    __has_include(<ftxui/dom/elements.hpp>) && \
    __has_include(<ftxui/dom/table.hpp>) && \
    __has_include(<ftxui/dom/canvas.hpp>) && \
    __has_include(<ftxui/screen/color.hpp>)
#define OMNI_FTXUI_HAS_BACKEND 1
#include <ftxui/component/component.hpp>
#include <ftxui/component/component_options.hpp>
#include <ftxui/component/screen_interactive.hpp>
#include <ftxui/dom/canvas.hpp>
#include <ftxui/dom/elements.hpp>
#include <ftxui/dom/table.hpp>
#include <ftxui/screen/color.hpp>
#else
#define OMNI_FTXUI_HAS_BACKEND 0
#endif

struct omni_ftxui_context_t {
    omni_ftxui_status last_error_code;
    std::string last_error_message;
};

#if OMNI_FTXUI_HAS_BACKEND
struct omni_ftxui_event_t {
    explicit omni_ftxui_event_t(ftxui::Event e) : event(std::move(e)) {}
    ftxui::Event event;
};

struct omni_ftxui_element_t {
    explicit omni_ftxui_element_t(ftxui::Element e) : element(std::move(e)) {}
    ftxui::Element element;
};

struct omni_ftxui_component_t {
    explicit omni_ftxui_component_t(ftxui::Component c) : component(std::move(c)) {}
    ftxui::Component component;
    std::vector<std::shared_ptr<void>> keep_alive;
};

struct omni_ftxui_screen_t {
    std::unique_ptr<ftxui::ScreenInteractive> screen;
};

struct omni_ftxui_table_t {
    std::vector<std::vector<std::string>> data;
    std::unique_ptr<ftxui::Table> table;
};

struct omni_ftxui_canvas_t {
    explicit omni_ftxui_canvas_t(int w, int h) : canvas(std::make_unique<ftxui::Canvas>(w, h)) {}
    std::unique_ptr<ftxui::Canvas> canvas;
};
#else
struct omni_ftxui_event_t {};
struct omni_ftxui_element_t {};
struct omni_ftxui_component_t {};
struct omni_ftxui_screen_t {};
struct omni_ftxui_table_t {};
struct omni_ftxui_canvas_t {};
#endif

struct omni_ftxui_app_t {
    omni_ftxui_screen_t* screen;
    omni_ftxui_component_t* root;
};

namespace {

constexpr uint32_t kAbiVersion = OMNI_FTXUI_SHIM_ABI_VERSION;

inline bool validate_sized_abi(uint32_t size, uint32_t abi, uint32_t expected) {
    return abi == kAbiVersion && size >= expected;
}

inline omni_ftxui_status ctx_fail(omni_ftxui_context_t* ctx, omni_ftxui_status code, const char* message) {
    if (ctx != nullptr) {
        ctx->last_error_code = code;
        ctx->last_error_message = message ? message : "";
    }
    return code;
}

inline void ctx_clear(omni_ftxui_context_t* ctx) {
    if (ctx == nullptr) return;
    ctx->last_error_code = OMNI_FTXUI_STATUS_OK;
    ctx->last_error_message.clear();
}

#if OMNI_FTXUI_HAS_BACKEND
inline void component_retain_keep_alive(
    omni_ftxui_component_t* target,
    const omni_ftxui_component_t* source
) {
    if (target == nullptr || source == nullptr) return;
    target->keep_alive.insert(
        target->keep_alive.end(),
        source->keep_alive.begin(),
        source->keep_alive.end()
    );
}

inline void component_retain_children_keep_alive(
    omni_ftxui_component_t* target,
    omni_ftxui_component_t* const* children,
    size_t child_count
) {
    if (target == nullptr || children == nullptr) return;
    for (size_t i = 0; i < child_count; ++i) {
        component_retain_keep_alive(target, children[i]);
    }
}

inline int clamp_u8(int v) {
    return std::max(0, std::min(255, v));
}

inline ftxui::Color map_color(const omni_ftxui_color& c) {
    switch (c.kind) {
        case OMNI_FTXUI_COLOR_INDEXED_16:
        case OMNI_FTXUI_COLOR_INDEXED_256:
            return ftxui::Color::Palette256(static_cast<uint8_t>(clamp_u8(c.a)));
        case OMNI_FTXUI_COLOR_RGB:
            return ftxui::Color::RGB(static_cast<uint8_t>(clamp_u8(c.a)),
                                     static_cast<uint8_t>(clamp_u8(c.b)),
                                     static_cast<uint8_t>(clamp_u8(c.c)));
        case OMNI_FTXUI_COLOR_HSV:
            return ftxui::Color::HSV(static_cast<uint8_t>(clamp_u8(c.a)),
                                     static_cast<uint8_t>(clamp_u8(c.b)),
                                     static_cast<uint8_t>(clamp_u8(c.c)));
        case OMNI_FTXUI_COLOR_DEFAULT:
        default:
            return ftxui::Color::Default;
    }
}

inline ftxui::Direction map_direction(omni_ftxui_direction d) {
    switch (d) {
        case OMNI_FTXUI_DIRECTION_LEFT: return ftxui::Direction::Left;
        case OMNI_FTXUI_DIRECTION_RIGHT: return ftxui::Direction::Right;
        case OMNI_FTXUI_DIRECTION_UP: return ftxui::Direction::Up;
        case OMNI_FTXUI_DIRECTION_DOWN:
        default: return ftxui::Direction::Down;
    }
}

inline ftxui::WidthOrHeight map_width_or_height(omni_ftxui_width_or_height v) {
    return v == OMNI_FTXUI_HEIGHT ? ftxui::HEIGHT : ftxui::WIDTH;
}

inline ftxui::Constraint map_constraint(omni_ftxui_constraint c) {
    switch (c) {
        case OMNI_FTXUI_LESS_THAN: return ftxui::LESS_THAN;
        case OMNI_FTXUI_GREATER_THAN: return ftxui::GREATER_THAN;
        case OMNI_FTXUI_EQUAL:
        default: return ftxui::EQUAL;
    }
}

inline ftxui::BorderStyle map_border_style(omni_ftxui_border_style b) {
    switch (b) {
        case OMNI_FTXUI_BORDER_DASHED: return ftxui::DASHED;
        case OMNI_FTXUI_BORDER_HEAVY: return ftxui::HEAVY;
        case OMNI_FTXUI_BORDER_ROUNDED: return ftxui::ROUNDED;
        case OMNI_FTXUI_BORDER_DOUBLE: return ftxui::DOUBLE;
        case OMNI_FTXUI_BORDER_EMPTY: return ftxui::EMPTY;
        case OMNI_FTXUI_BORDER_LIGHT:
        default: return ftxui::LIGHT;
    }
}

inline omni_ftxui_element_t* make_element(ftxui::Element e) {
    return new (std::nothrow) omni_ftxui_element_t(std::move(e));
}

inline omni_ftxui_component_t* make_component(ftxui::Component c) {
    return new (std::nothrow) omni_ftxui_component_t(std::move(c));
}

inline ftxui::Elements copy_children(omni_ftxui_element_t* const* children, size_t child_count) {
    ftxui::Elements out;
    out.reserve(child_count);
    for (size_t i = 0; i < child_count; ++i) {
        if (children[i] != nullptr) out.push_back(children[i]->element);
    }
    return out;
}

inline ftxui::Components copy_components(omni_ftxui_component_t* const* children, size_t child_count) {
    ftxui::Components out;
    out.reserve(child_count);
    for (size_t i = 0; i < child_count; ++i) {
        if (children[i] != nullptr) out.push_back(children[i]->component);
    }
    return out;
}

inline std::shared_ptr<std::vector<std::string>> copy_items(const char** items, size_t item_count) {
    auto owned = std::make_shared<std::vector<std::string>>();
    owned->reserve(item_count);
    for (size_t i = 0; i < item_count; ++i) owned->push_back(items[i] ? items[i] : "");
    return owned;
}

inline std::vector<int> call_graph_callback(
    omni_ftxui_context_t* context,
    void* user_data,
    omni_ftxui_graph_callback_fn graph_cb,
    int width,
    int height
) {
    if (width <= 0) return {};

    std::vector<int> values(static_cast<size_t>(width), 0);
    if (graph_cb == nullptr) return values;

    omni_ftxui_status status =
        graph_cb(user_data, context, static_cast<int32_t>(width), static_cast<int32_t>(height), values.data());
    if (status != OMNI_FTXUI_STATUS_OK && context != nullptr && context->last_error_code == OMNI_FTXUI_STATUS_OK) {
        ctx_fail(context, status, "graph callback failed");
    }
    return values;
}

inline double graph_sample_series(
    const std::vector<double>& series,
    size_t sample_index,
    int width
) {
    if (series.empty()) return 0.0;
    if (series.size() == 1 || width <= 1) return series.front();

    const double span = static_cast<double>(series.size() - 1);
    const double pos =
        (static_cast<double>(sample_index) * span) / static_cast<double>(width - 1);
    const size_t left = static_cast<size_t>(pos);
    const size_t right = std::min(left + 1, series.size() - 1);
    const double frac = pos - static_cast<double>(left);
    return series[left] + (series[right] - series[left]) * frac;
}

inline std::vector<int> graph_values_from_series(
    const std::shared_ptr<std::vector<double>>& series,
    int width,
    int height
) {
    if (width <= 0 || series == nullptr || series->empty()) return {};

    std::vector<int> values(static_cast<size_t>(width), 0);
    if (height <= 1) return values;

    const auto [min_it, max_it] = std::minmax_element(series->begin(), series->end());
    const double min_value = *min_it;
    const double max_value = *max_it;
    const int graph_height = height - 1;

    if (std::abs(max_value - min_value) < 1e-9) {
        const int center = graph_height / 2;
        std::fill(values.begin(), values.end(), center);
        return values;
    }

    for (size_t i = 0; i < values.size(); ++i) {
        const double sample = graph_sample_series(*series, i, width);
        const double normalized = (sample - min_value) / (max_value - min_value);
        const double y = (1.0 - normalized) * static_cast<double>(graph_height);
        values[i] = static_cast<int>(std::lround(y));
    }
    return values;
}
#endif

}  // namespace

extern "C" {

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
    auto* ctx = new (std::nothrow) omni_ftxui_context_t();
    if (ctx == nullptr) return OMNI_FTXUI_STATUS_OUT_OF_MEMORY;
    ctx->last_error_code = OMNI_FTXUI_STATUS_OK;
    *out_context = ctx;
    return OMNI_FTXUI_STATUS_OK;
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
    if (options->handle_piped_input) {
        return ctx_fail(
            context,
            OMNI_FTXUI_STATUS_NOT_SUPPORTED,
            "screen_create: handle_piped_input is not supported by the FTXUI backend"
        );
    }

    std::unique_ptr<ftxui::ScreenInteractive> screen;
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
    screen->screen->Loop(root->component);
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
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
    screen->screen->PostEvent(event->event);
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
#endif
}

omni_ftxui_status omni_ftxui_screen_exit(omni_ftxui_context_t* context, omni_ftxui_screen_t* screen) {
    if (screen == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "screen_exit: null screen");
#if !OMNI_FTXUI_HAS_BACKEND
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "screen_exit: FTXUI backend unavailable");
#else
    screen->screen->Exit();
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
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
    screen->screen->RequestAnimationFrame();
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
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
    screen->screen->TrackMouse(enabled);
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
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
    if (enabled) {
        return ctx_fail(
            context,
            OMNI_FTXUI_STATUS_NOT_SUPPORTED,
            "screen_set_handle_piped_input: handle_piped_input is not supported by the FTXUI backend"
        );
    }
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
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
    auto* app = new (std::nothrow) omni_ftxui_app_t{screen, root};
    if (app == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "app_create: allocation failed");
    *out_app = app;
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
}

void omni_ftxui_app_destroy(omni_ftxui_app_t* app) {
    delete app;
}

omni_ftxui_status omni_ftxui_app_run(omni_ftxui_context_t* context, omni_ftxui_app_t* app) {
    if (app == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "app_run: null app");
    return omni_ftxui_screen_loop(context, app->screen, app->root);
}

omni_ftxui_status omni_ftxui_app_exit(omni_ftxui_context_t* context, omni_ftxui_app_t* app) {
    if (app == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "app_exit: null app");
    return omni_ftxui_screen_exit(context, app->screen);
}

omni_ftxui_status omni_ftxui_app_post_event(
    omni_ftxui_context_t* context,
    omni_ftxui_app_t* app,
    const omni_ftxui_event_t* event
) {
    if (app == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "app_post_event: null app");
    return omni_ftxui_screen_post_event(context, app->screen, event);
}

omni_ftxui_status omni_ftxui_element_create(
    omni_ftxui_context_t* context,
    const omni_ftxui_element_create_options* options,
    omni_ftxui_element_t* const* children,
    size_t child_count,
    omni_ftxui_element_t** out_element
) {
    if (options == nullptr || out_element == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "element_create: null argument");
    }
    if (!validate_sized_abi(options->size, options->abi_version, sizeof(omni_ftxui_element_create_options))) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "element_create: invalid option ABI");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    (void)children;
    (void)child_count;
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "element_create: FTXUI backend unavailable");
#else
    ftxui::Element result;
    const char* text = options->text ? options->text : "";
    const ftxui::Elements elems = copy_children(children, child_count);
    switch (options->kind) {
        case OMNI_FTXUI_ELEMENT_EMPTY: result = ftxui::emptyElement(); break;
        case OMNI_FTXUI_ELEMENT_TEXT: result = ftxui::text(text); break;
        case OMNI_FTXUI_ELEMENT_VTEXT: result = ftxui::vtext(text); break;
        case OMNI_FTXUI_ELEMENT_PARAGRAPH: result = ftxui::paragraph(text); break;
        case OMNI_FTXUI_ELEMENT_PARAGRAPH_LEFT: result = ftxui::paragraphAlignLeft(text); break;
        case OMNI_FTXUI_ELEMENT_PARAGRAPH_CENTER: result = ftxui::paragraphAlignCenter(text); break;
        case OMNI_FTXUI_ELEMENT_PARAGRAPH_RIGHT: result = ftxui::paragraphAlignRight(text); break;
        case OMNI_FTXUI_ELEMENT_PARAGRAPH_JUSTIFY: result = ftxui::paragraphAlignJustify(text); break;
        case OMNI_FTXUI_ELEMENT_SEPARATOR: result = ftxui::separator(); break;
        case OMNI_FTXUI_ELEMENT_SEPARATOR_LIGHT: result = ftxui::separatorLight(); break;
        case OMNI_FTXUI_ELEMENT_SEPARATOR_HEAVY: result = ftxui::separatorHeavy(); break;
        case OMNI_FTXUI_ELEMENT_SEPARATOR_DOUBLE: result = ftxui::separatorDouble(); break;
        case OMNI_FTXUI_ELEMENT_GAUGE: result = ftxui::gauge(options->gauge_progress); break;
        case OMNI_FTXUI_ELEMENT_GAUGE_LEFT: result = ftxui::gaugeLeft(options->gauge_progress); break;
        case OMNI_FTXUI_ELEMENT_GAUGE_RIGHT: result = ftxui::gaugeRight(options->gauge_progress); break;
        case OMNI_FTXUI_ELEMENT_GAUGE_UP: result = ftxui::gaugeUp(options->gauge_progress); break;
        case OMNI_FTXUI_ELEMENT_GAUGE_DOWN: result = ftxui::gaugeDown(options->gauge_progress); break;
        case OMNI_FTXUI_ELEMENT_HBOX: result = ftxui::hbox(elems); break;
        case OMNI_FTXUI_ELEMENT_VBOX: result = ftxui::vbox(elems); break;
        case OMNI_FTXUI_ELEMENT_DBOX: result = ftxui::dbox(elems); break;
        case OMNI_FTXUI_ELEMENT_HFLOW: result = ftxui::hflow(elems); break;
        case OMNI_FTXUI_ELEMENT_VFLOW: result = ftxui::vflow(elems); break;
        case OMNI_FTXUI_ELEMENT_FLEXBOX: result = ftxui::flexbox(elems); break;
        case OMNI_FTXUI_ELEMENT_GRIDBOX: {
            std::vector<ftxui::Elements> lines;
            lines.push_back(elems);
            result = ftxui::gridbox(std::move(lines));
            break;
        }
        case OMNI_FTXUI_ELEMENT_WINDOW:
            if (elems.size() < 2) {
                return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "element_create: window requires title and body children");
            }
            result = ftxui::window(elems[0], elems[1]);
            break;
        case OMNI_FTXUI_ELEMENT_FILLER: result = ftxui::filler(); break;
        case OMNI_FTXUI_ELEMENT_SPINNER:
            result = ftxui::spinner(options->spinner_charset, options->spinner_frame);
            break;
        case OMNI_FTXUI_ELEMENT_CANVAS: {
            if (options->canvas_width <= 0 || options->canvas_height <= 0) {
                return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "element_create: canvas requires positive width/height");
            }
            ftxui::Canvas canvas(options->canvas_width, options->canvas_height);
            result = ftxui::canvas(canvas);
            break;
        }
        case OMNI_FTXUI_ELEMENT_TABLE: {
            if (options->table_rows == 0 || options->table_cols == 0) {
                return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "element_create: table requires positive rows/cols");
            }
            if (options->table_rows * options->table_cols != elems.size()) {
                return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "element_create: table rows*cols must match child_count");
            }
            std::vector<std::vector<ftxui::Element>> rows;
            rows.reserve(options->table_rows);
            size_t idx = 0;
            for (size_t r = 0; r < options->table_rows; ++r) {
                std::vector<ftxui::Element> row;
                row.reserve(options->table_cols);
                for (size_t c = 0; c < options->table_cols; ++c) row.push_back(elems[idx++]);
                rows.push_back(std::move(row));
            }
            ftxui::Table table(std::move(rows));
            result = table.Render();
            break;
        }
        case OMNI_FTXUI_ELEMENT_GRAPH:
            if (options->graph_cb == nullptr) {
                return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "element_create: graph requires graph_cb");
            }
            result = ftxui::graph([context, user_data = options->graph_user_data, graph_cb = options->graph_cb](int width, int height) {
                return call_graph_callback(context, user_data, graph_cb, width, height);
            });
            break;
        default:
            return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "element_create: unknown element kind");
    }
    auto* out = make_element(std::move(result));
    if (out == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "element_create: allocation failed");
    *out_element = out;
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
#endif
}

omni_ftxui_status omni_ftxui_element_graph_from_series(
    omni_ftxui_context_t* context,
    const double* values,
    size_t value_count,
    omni_ftxui_element_t** out_element
) {
    if (values == nullptr || value_count == 0 || out_element == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "element_graph_from_series: invalid argument");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    (void)values;
    (void)value_count;
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "element_graph_from_series: FTXUI backend unavailable");
#else
    auto series = std::make_shared<std::vector<double>>(values, values + value_count);
    auto* out = make_element(ftxui::graph([series](int width, int height) {
        return graph_values_from_series(series, width, height);
    }));
    if (out == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "element_graph_from_series: allocation failed");
    }
    *out_element = out;
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
#endif
}

void omni_ftxui_element_destroy(omni_ftxui_element_t* element) {
    delete element;
}

omni_ftxui_status omni_ftxui_element_apply_decorator(
    omni_ftxui_context_t* context,
    omni_ftxui_element_t* element,
    const omni_ftxui_decorator_options* options
) {
    if (element == nullptr || options == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "element_apply_decorator: null argument");
    }
    if (!validate_sized_abi(options->size, options->abi_version, sizeof(omni_ftxui_decorator_options))) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "element_apply_decorator: invalid option ABI");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    (void)options;
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "element_apply_decorator: FTXUI backend unavailable");
#else
    switch (options->kind) {
        case OMNI_FTXUI_DECORATOR_BORDER: element->element |= ftxui::border; break;
        case OMNI_FTXUI_DECORATOR_BORDER_LIGHT: element->element |= ftxui::borderLight; break;
        case OMNI_FTXUI_DECORATOR_BORDER_HEAVY: element->element |= ftxui::borderHeavy; break;
        case OMNI_FTXUI_DECORATOR_BORDER_DOUBLE: element->element |= ftxui::borderDouble; break;
        case OMNI_FTXUI_DECORATOR_BORDER_ROUNDED: element->element |= ftxui::borderRounded; break;
        case OMNI_FTXUI_DECORATOR_BORDER_EMPTY: element->element |= ftxui::borderEmpty; break;
        case OMNI_FTXUI_DECORATOR_FRAME: element->element |= ftxui::frame; break;
        case OMNI_FTXUI_DECORATOR_XFRAME: element->element |= ftxui::xframe; break;
        case OMNI_FTXUI_DECORATOR_YFRAME: element->element |= ftxui::yframe; break;
        case OMNI_FTXUI_DECORATOR_CENTER: element->element |= ftxui::center; break;
        case OMNI_FTXUI_DECORATOR_HCENTER: element->element |= ftxui::hcenter; break;
        case OMNI_FTXUI_DECORATOR_VCENTER: element->element |= ftxui::vcenter; break;
        case OMNI_FTXUI_DECORATOR_ALIGN_RIGHT: element->element |= ftxui::align_right; break;
        case OMNI_FTXUI_DECORATOR_BOLD: element->element |= ftxui::bold; break;
        case OMNI_FTXUI_DECORATOR_ITALIC: element->element |= ftxui::italic; break;
        case OMNI_FTXUI_DECORATOR_DIM: element->element |= ftxui::dim; break;
        case OMNI_FTXUI_DECORATOR_INVERTED: element->element |= ftxui::inverted; break;
        case OMNI_FTXUI_DECORATOR_UNDERLINED: element->element |= ftxui::underlined; break;
        case OMNI_FTXUI_DECORATOR_UNDERLINED_DOUBLE: element->element |= ftxui::underlinedDouble; break;
        case OMNI_FTXUI_DECORATOR_STRIKETHROUGH: element->element |= ftxui::strikethrough; break;
        case OMNI_FTXUI_DECORATOR_BLINK: element->element |= ftxui::blink; break;
        case OMNI_FTXUI_DECORATOR_COLOR: element->element |= ftxui::color(map_color(options->color)); break;
        case OMNI_FTXUI_DECORATOR_BGCOLOR: element->element |= ftxui::bgcolor(map_color(options->color)); break;
        case OMNI_FTXUI_DECORATOR_FLEX: element->element |= ftxui::flex; break;
        case OMNI_FTXUI_DECORATOR_FLEX_GROW: element->element |= ftxui::flex_grow; break;
        case OMNI_FTXUI_DECORATOR_FLEX_SHRINK: element->element |= ftxui::flex_shrink; break;
        case OMNI_FTXUI_DECORATOR_XFLEX: element->element |= ftxui::xflex; break;
        case OMNI_FTXUI_DECORATOR_XFLEX_GROW: element->element |= ftxui::xflex_grow; break;
        case OMNI_FTXUI_DECORATOR_XFLEX_SHRINK: element->element |= ftxui::xflex_shrink; break;
        case OMNI_FTXUI_DECORATOR_YFLEX: element->element |= ftxui::yflex; break;
        case OMNI_FTXUI_DECORATOR_YFLEX_GROW: element->element |= ftxui::yflex_grow; break;
        case OMNI_FTXUI_DECORATOR_YFLEX_SHRINK: element->element |= ftxui::yflex_shrink; break;
        case OMNI_FTXUI_DECORATOR_NOTFLEX: element->element |= ftxui::notflex; break;
        case OMNI_FTXUI_DECORATOR_FOCUS: element->element |= ftxui::focus; break;
        case OMNI_FTXUI_DECORATOR_SELECT: element->element |= ftxui::select; break;
        case OMNI_FTXUI_DECORATOR_AUTOMERGE: element->element |= ftxui::automerge; break;
        case OMNI_FTXUI_DECORATOR_VSCROLL_INDICATOR: element->element |= ftxui::vscroll_indicator; break;
        case OMNI_FTXUI_DECORATOR_HSCROLL_INDICATOR: element->element |= ftxui::hscroll_indicator; break;
        case OMNI_FTXUI_DECORATOR_CLEAR_UNDER: element->element |= ftxui::clear_under; break;
        case OMNI_FTXUI_DECORATOR_SIZE:
            element->element |= ftxui::size(
                map_width_or_height(options->width_or_height),
                map_constraint(options->constraint),
                options->size_value
            );
            break;
        case OMNI_FTXUI_DECORATOR_HYPERLINK:
            if (options->link == nullptr) {
                return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "element_apply_decorator: hyperlink requires link");
            }
            element->element |= ftxui::hyperlink(options->link);
            break;
        case OMNI_FTXUI_DECORATOR_SELECTION_STYLE_RESET:
            element->element = ftxui::selectionStyleReset(element->element);
            break;
        case OMNI_FTXUI_DECORATOR_SELECTION_COLOR:
            element->element |= ftxui::selectionColor(map_color(options->color));
            break;
        case OMNI_FTXUI_DECORATOR_SELECTION_BACKGROUND_COLOR:
            element->element |= ftxui::selectionBackgroundColor(map_color(options->color));
            break;
        case OMNI_FTXUI_DECORATOR_SELECTION_FOREGROUND_COLOR:
            element->element |= ftxui::selectionForegroundColor(map_color(options->color));
            break;
        default:
            return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "element_apply_decorator: unknown decorator");
    }
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
#endif
}

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
#if !OMNI_FTXUI_HAS_BACKEND
    (void)children;
    (void)child_count;
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "component_create: FTXUI backend unavailable");
#else
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
    parent->component->Add(child->component);
    component_retain_keep_alive(parent, child);
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
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
    component->component->TakeFocus();
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
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
        omni_ftxui_status r = state->callback(state->user_data, state->context, &out);
        if (r != OMNI_FTXUI_STATUS_OK || out == nullptr) return ftxui::text("");
        return out->element;
    });
    auto* out = make_component(std::move(wrapped));
    if (out == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "component_wrap_renderer: allocation failed");
    component_retain_keep_alive(out, base);
    out->keep_alive.push_back(state);
    *out_component = out;
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
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
        omni_ftxui_event_t event(std::move(e));
        int handled = state->callback(state->user_data, state->context, &event);
        return handled != 0;
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
    auto rendered = element->element;
    auto component = ftxui::Renderer([rendered] { return rendered; });
    auto* out = make_component(std::move(component));
    if (out == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "component_from_element: allocation failed");
    *out_component = out;
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
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
    auto wrapped = ftxui::CatchEvent(base->component, [screen](ftxui::Event e) {
        if (e == ftxui::Event::Escape) {
            screen->screen->ExitLoopClosure()();
            return true;
        }
        if (e.is_character()) {
            std::string ch = e.character();
            if (ch == "q" || ch == "Q") {
                screen->screen->ExitLoopClosure()();
                return true;
            }
        }
        return false;
    });
    auto* out = make_component(std::move(wrapped));
    if (out == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "component_wrap_quit_keys: allocation failed");
    component_retain_keep_alive(out, base);
    *out_component = out;
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
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
    auto wrapped = ftxui::Maybe(base->component, visible_ptr);
    auto* out = make_component(std::move(wrapped));
    if (out == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "component_wrap_maybe: allocation failed");
    component_retain_keep_alive(out, base);
    *out_component = out;
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
#endif
}

omni_ftxui_status omni_ftxui_table_create(
    omni_ftxui_context_t* context,
    const omni_ftxui_table_create_options* options,
    omni_ftxui_table_t** out_table
) {
    if (options == nullptr || out_table == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "table_create: null argument");
    }
    if (!validate_sized_abi(options->size, options->abi_version, sizeof(omni_ftxui_table_create_options))) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "table_create: invalid option ABI");
    }
    if (options->rows == 0 || options->cols == 0 || options->cells == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "table_create: invalid table dimensions");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "table_create: FTXUI backend unavailable");
#else
    auto* table = new (std::nothrow) omni_ftxui_table_t();
    if (table == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "table_create: allocation failed");

    table->data.resize(options->rows);
    size_t idx = 0;
    for (size_t r = 0; r < options->rows; ++r) {
        table->data[r].reserve(options->cols);
        for (size_t c = 0; c < options->cols; ++c) {
            table->data[r].push_back(options->cells[idx] ? options->cells[idx] : "");
            ++idx;
        }
    }
    table->table = std::make_unique<ftxui::Table>(table->data);
    *out_table = table;
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
#endif
}

void omni_ftxui_table_destroy(omni_ftxui_table_t* table) {
    delete table;
}

omni_ftxui_status omni_ftxui_table_select_all(
    omni_ftxui_context_t* context,
    omni_ftxui_table_t* table,
    omni_ftxui_border_style border
) {
    if (table == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "table_select_all: null table");
#if !OMNI_FTXUI_HAS_BACKEND
    (void)border;
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "table_select_all: FTXUI backend unavailable");
#else
    table->table->SelectAll().Border(map_border_style(border));
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
#endif
}

omni_ftxui_status omni_ftxui_table_select_row(
    omni_ftxui_context_t* context,
    omni_ftxui_table_t* table,
    size_t row,
    omni_ftxui_border_style border
) {
    if (table == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "table_select_row: null table");
#if !OMNI_FTXUI_HAS_BACKEND
    (void)row;
    (void)border;
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "table_select_row: FTXUI backend unavailable");
#else
    table->table->SelectRow(static_cast<int>(row)).Border(map_border_style(border));
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
#endif
}

omni_ftxui_status omni_ftxui_table_select_column(
    omni_ftxui_context_t* context,
    omni_ftxui_table_t* table,
    size_t column,
    omni_ftxui_border_style border
) {
    if (table == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "table_select_column: null table");
#if !OMNI_FTXUI_HAS_BACKEND
    (void)column;
    (void)border;
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "table_select_column: FTXUI backend unavailable");
#else
    table->table->SelectColumn(static_cast<int>(column)).Border(map_border_style(border));
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
#endif
}

omni_ftxui_status omni_ftxui_table_select_cell(
    omni_ftxui_context_t* context,
    omni_ftxui_table_t* table,
    size_t row,
    size_t column,
    omni_ftxui_border_style border
) {
    if (table == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "table_select_cell: null table");
#if !OMNI_FTXUI_HAS_BACKEND
    (void)row;
    (void)column;
    (void)border;
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "table_select_cell: FTXUI backend unavailable");
#else
    table->table->SelectCell(static_cast<int>(row), static_cast<int>(column)).Border(map_border_style(border));
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
#endif
}

omni_ftxui_status omni_ftxui_table_render(
    omni_ftxui_context_t* context,
    omni_ftxui_table_t* table,
    omni_ftxui_element_t** out_element
) {
    if (table == nullptr || out_element == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "table_render: null argument");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "table_render: FTXUI backend unavailable");
#else
    auto* out = make_element(table->table->Render());
    if (out == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "table_render: allocation failed");
    *out_element = out;
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
#endif
}

omni_ftxui_status omni_ftxui_canvas_create(
    omni_ftxui_context_t* context,
    const omni_ftxui_canvas_create_options* options,
    omni_ftxui_canvas_t** out_canvas
) {
    if (options == nullptr || out_canvas == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "canvas_create: null argument");
    }
    if (!validate_sized_abi(options->size, options->abi_version, sizeof(omni_ftxui_canvas_create_options))) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "canvas_create: invalid option ABI");
    }
    if (options->width <= 0 || options->height <= 0) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "canvas_create: invalid dimensions");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "canvas_create: FTXUI backend unavailable");
#else
    auto* out = new (std::nothrow) omni_ftxui_canvas_t(options->width, options->height);
    if (out == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "canvas_create: allocation failed");
    *out_canvas = out;
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
#endif
}

void omni_ftxui_canvas_destroy(omni_ftxui_canvas_t* canvas) {
    delete canvas;
}

omni_ftxui_status omni_ftxui_canvas_draw_text(
    omni_ftxui_context_t* context,
    omni_ftxui_canvas_t* canvas,
    int32_t x,
    int32_t y,
    const char* text
) {
    if (canvas == nullptr || text == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "canvas_draw_text: null argument");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    (void)x;
    (void)y;
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "canvas_draw_text: FTXUI backend unavailable");
#else
    canvas->canvas->DrawText(x, y, text);
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
#endif
}

omni_ftxui_status omni_ftxui_canvas_draw_point_on(
    omni_ftxui_context_t* context,
    omni_ftxui_canvas_t* canvas,
    int32_t x,
    int32_t y
) {
    if (canvas == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "canvas_draw_point_on: null canvas");
#if !OMNI_FTXUI_HAS_BACKEND
    (void)x;
    (void)y;
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "canvas_draw_point_on: FTXUI backend unavailable");
#else
    canvas->canvas->DrawPointOn(x, y);
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
#endif
}

omni_ftxui_status omni_ftxui_canvas_draw_point_off(
    omni_ftxui_context_t* context,
    omni_ftxui_canvas_t* canvas,
    int32_t x,
    int32_t y
) {
    if (canvas == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "canvas_draw_point_off: null canvas");
#if !OMNI_FTXUI_HAS_BACKEND
    (void)x;
    (void)y;
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "canvas_draw_point_off: FTXUI backend unavailable");
#else
    canvas->canvas->DrawPointOff(x, y);
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
#endif
}

omni_ftxui_status omni_ftxui_canvas_draw_line(
    omni_ftxui_context_t* context,
    omni_ftxui_canvas_t* canvas,
    int32_t x1,
    int32_t y1,
    int32_t x2,
    int32_t y2
) {
    if (canvas == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "canvas_draw_line: null canvas");
#if !OMNI_FTXUI_HAS_BACKEND
    (void)x1;
    (void)y1;
    (void)x2;
    (void)y2;
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "canvas_draw_line: FTXUI backend unavailable");
#else
    canvas->canvas->DrawPointLine(x1, y1, x2, y2);
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
#endif
}

omni_ftxui_status omni_ftxui_canvas_render(
    omni_ftxui_context_t* context,
    omni_ftxui_canvas_t* canvas,
    omni_ftxui_element_t** out_element
) {
    if (canvas == nullptr || out_element == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "canvas_render: null argument");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "canvas_render: FTXUI backend unavailable");
#else
    auto* out = make_element(ftxui::canvas(*canvas->canvas));
    if (out == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "canvas_render: allocation failed");
    *out_element = out;
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
#endif
}

}  // extern "C"
