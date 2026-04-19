#include "ftxui_shim.h"

#include <algorithm>
#include <cmath>
#include <exception>
#include <limits>
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
    explicit omni_ftxui_component_t(ftxui::Component c) : keep_alive(), component(std::move(c)) {}
    std::vector<std::shared_ptr<void>> keep_alive;
    ftxui::Component component;
};

struct omni_ftxui_screen_t {
    std::shared_ptr<ftxui::ScreenInteractive> screen;
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
        try {
            ctx->last_error_message = message ? message : "";
        } catch (...) {
            ctx->last_error_message.clear();
        }
    }
    return code;
}

inline void ctx_clear(omni_ftxui_context_t* ctx) {
    if (ctx == nullptr) return;
    ctx->last_error_code = OMNI_FTXUI_STATUS_OK;
    ctx->last_error_message.clear();
}

template <typename Fn>
inline omni_ftxui_status status_try(
    omni_ftxui_context_t* ctx,
    const char* out_of_memory_message,
    const char* exception_message,
    Fn&& fn
) {
    try {
        return fn();
    } catch (const std::bad_alloc&) {
        return ctx_fail(ctx, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, out_of_memory_message);
    } catch (const std::exception&) {
        return ctx_fail(ctx, OMNI_FTXUI_STATUS_INTERNAL_ERROR, exception_message);
    } catch (...) {
        return ctx_fail(ctx, OMNI_FTXUI_STATUS_INTERNAL_ERROR, exception_message);
    }
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

    omni_ftxui_status status = OMNI_FTXUI_STATUS_OK;
    try {
        status = graph_cb(user_data, context, static_cast<int32_t>(width), static_cast<int32_t>(height), values.data());
    } catch (const std::bad_alloc&) {
        ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "graph callback: out of memory");
        return values;
    } catch (const std::exception&) {
        ctx_fail(context, OMNI_FTXUI_STATUS_INTERNAL_ERROR, "graph callback: exception");
        return values;
    } catch (...) {
        ctx_fail(context, OMNI_FTXUI_STATUS_INTERNAL_ERROR, "graph callback: unknown exception");
        return values;
    }
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

#include "ftxui_shim_runtime.inc"
#include "ftxui_shim_element.inc"
#include "ftxui_shim_component.inc"
#include "ftxui_shim_table_canvas.inc"

}  // extern "C"
