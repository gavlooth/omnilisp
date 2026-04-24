#include "ftxui_shim.h"

#include <algorithm>
#include <cmath>
#include <deque>
#include <exception>
#include <limits>
#include <memory>
#include <new>
#include <string>
#include <utility>
#include <vector>

#if __has_include(<ftxui/component/component.hpp>) && \
    __has_include(<ftxui/component/loop.hpp>) && \
    __has_include(<ftxui/component/screen_interactive.hpp>) && \
    __has_include(<ftxui/dom/elements.hpp>) && \
    __has_include(<ftxui/dom/table.hpp>) && \
    __has_include(<ftxui/dom/canvas.hpp>) && \
    __has_include(<ftxui/screen/color.hpp>)
#define OMNI_FTXUI_HAS_BACKEND 1
#include <ftxui/component/component.hpp>
#include <ftxui/component/component_options.hpp>
#include <ftxui/component/loop.hpp>
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

struct omni_ftxui_captured_event_t {
    omni_ftxui_event_kind kind = OMNI_FTXUI_EVENT_CUSTOM;
    std::string text;
};

struct omni_ftxui_session_t {
    omni_ftxui_screen_t* screen;
    omni_ftxui_component_t* root;
    std::deque<omni_ftxui_captured_event_t> events;
    size_t dropped_events = 0;
    std::unique_ptr<ftxui::Loop> loop;
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
struct omni_ftxui_session_t {};
struct omni_ftxui_table_t {};
struct omni_ftxui_canvas_t {};
#endif

struct omni_ftxui_app_t {
    omni_ftxui_screen_t* screen;
    omni_ftxui_component_t* root;
};

namespace {

constexpr uint32_t kAbiVersion = OMNI_FTXUI_SHIM_ABI_VERSION;
constexpr uint32_t kEventReadResultMagic = 0x46545845u;
constexpr size_t kSessionEventQueueLimit = 1024;

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

inline ftxui::Color plot_color(size_t index) {
    switch (index % 6) {
        case 0: return ftxui::Color::Cyan;
        case 1: return ftxui::Color::Yellow;
        case 2: return ftxui::Color::Green;
        case 3: return ftxui::Color::Magenta;
        case 4: return ftxui::Color::Blue;
        default: return ftxui::Color::Red;
    }
}

inline bool finite_values(const double* values, size_t count) {
    if (values == nullptr || count == 0) return false;
    for (size_t i = 0; i < count; ++i) {
        if (!std::isfinite(values[i])) return false;
    }
    return true;
}

inline int plot_scale_x(double value, double min_value, double max_value, int width) {
    if (width <= 1 || std::abs(max_value - min_value) < 1e-12) return width / 2;
    const double norm = (value - min_value) / (max_value - min_value);
    return std::max(0, std::min(width - 1, static_cast<int>(std::lround(norm * static_cast<double>(width - 1)))));
}

inline int plot_scale_y(double value, double min_value, double max_value, int height) {
    if (height <= 1 || std::abs(max_value - min_value) < 1e-12) return height / 2;
    const double norm = (value - min_value) / (max_value - min_value);
    return std::max(0, std::min(height - 1, static_cast<int>(std::lround((1.0 - norm) * static_cast<double>(height - 1)))));
}

inline void plot_range(
    const omni_ftxui_plot_options* options,
    double* out_x_min,
    double* out_x_max,
    double* out_y_min,
    double* out_y_max
) {
    *out_y_min = options->values[0];
    *out_y_max = options->values[0];
    for (size_t i = 1; i < options->value_count; ++i) {
        *out_y_min = std::min(*out_y_min, options->values[i]);
        *out_y_max = std::max(*out_y_max, options->values[i]);
    }
    if (options->kind == OMNI_FTXUI_PLOT_BAR || options->kind == OMNI_FTXUI_PLOT_HISTOGRAM) {
        *out_y_min = std::min(*out_y_min, 0.0);
        *out_y_max = std::max(*out_y_max, 0.0);
    }
    if (options->has_y_min) {
        *out_y_min = options->y_min;
        if (!options->has_y_max && *out_y_max < *out_y_min) *out_y_max = *out_y_min;
    }
    if (options->has_y_max) {
        *out_y_max = options->y_max;
        if (!options->has_y_min && *out_y_min > *out_y_max) *out_y_min = *out_y_max;
    }

    if (options->x_values != nullptr && options->x_count > 0) {
        *out_x_min = options->x_values[0];
        *out_x_max = options->x_values[0];
        for (size_t i = 1; i < options->x_count; ++i) {
            *out_x_min = std::min(*out_x_min, options->x_values[i]);
            *out_x_max = std::max(*out_x_max, options->x_values[i]);
        }
    } else {
        *out_x_min = 0.0;
        *out_x_max = options->value_count > 1 ? static_cast<double>(options->value_count - 1) : 1.0;
    }
}

inline void draw_marker(ftxui::Canvas& canvas, int x, int y, omni_ftxui_plot_marker marker, ftxui::Color color) {
    if (marker == OMNI_FTXUI_PLOT_MARKER_CROSS) {
        canvas.DrawPointLine(x - 1, y - 1, x + 1, y + 1, color);
        canvas.DrawPointLine(x - 1, y + 1, x + 1, y - 1, color);
        return;
    }
    if (marker == OMNI_FTXUI_PLOT_MARKER_PLUS) {
        canvas.DrawPointLine(x - 1, y, x + 1, y, color);
        canvas.DrawPointLine(x, y - 1, x, y + 1, color);
        return;
    }
    canvas.DrawPointLine(x, y, x, y, color);
}

inline void draw_series_plot(ftxui::Canvas& canvas, const omni_ftxui_plot_options* options) {
    double x_min = 0.0;
    double x_max = 0.0;
    double y_min = 0.0;
    double y_max = 0.0;
    plot_range(options, &x_min, &x_max, &y_min, &y_max);
    const int width = std::max(1, options->width);
    const int height = std::max(1, options->height);

    size_t offset = 0;
    const size_t series_count = options->series_count == 0 ? 1 : options->series_count;
    for (size_t series = 0; series < series_count; ++series) {
        const size_t count = options->series_count == 0 ? options->value_count : options->series_lengths[series];
        if (count == 0) continue;
        const ftxui::Color color = plot_color(series);
        int prev_x = 0;
        int prev_y = 0;
        for (size_t i = 0; i < count; ++i) {
            const size_t idx = offset + i;
            const double x_value = options->x_values != nullptr && idx < options->x_count
                ? options->x_values[idx]
                : static_cast<double>(i);
            const int x = plot_scale_x(x_value, x_min, x_max, width);
            const int y = plot_scale_y(options->values[idx], y_min, y_max, height);
            if (options->kind == OMNI_FTXUI_PLOT_SCATTER) {
                draw_marker(canvas, x, y, options->marker, color);
            } else {
                if (i > 0) canvas.DrawPointLine(prev_x, prev_y, x, y, color);
                else canvas.DrawPointLine(x, y, x, y, color);
            }
            prev_x = x;
            prev_y = y;
        }
        offset += count;
    }
}

inline void draw_bar_plot(ftxui::Canvas& canvas, const omni_ftxui_plot_options* options) {
    double x_min = 0.0;
    double x_max = 0.0;
    double y_min = 0.0;
    double y_max = 0.0;
    plot_range(options, &x_min, &x_max, &y_min, &y_max);
    const int width = std::max(1, options->width);
    const int height = std::max(1, options->height);
    const int baseline = plot_scale_y(0.0, y_min, y_max, height);
    for (size_t i = 0; i < options->value_count; ++i) {
        const int x0 = static_cast<int>((i * static_cast<size_t>(width)) / options->value_count);
        const int x1 = static_cast<int>(((i + 1) * static_cast<size_t>(width)) / options->value_count);
        const int y = plot_scale_y(options->values[i], y_min, y_max, height);
        const int left = std::max(0, x0);
        const int right = std::max(left, std::min(width - 1, x1 - 1));
        for (int x = left; x <= right; ++x) {
            canvas.DrawPointLine(x, baseline, x, y, plot_color(i));
        }
    }
}

inline ftxui::Color heatmap_color(double value, double min_value, double max_value) {
    double norm = 0.5;
    if (std::abs(max_value - min_value) >= 1e-12) norm = (value - min_value) / (max_value - min_value);
    norm = std::max(0.0, std::min(1.0, norm));
    const int red = static_cast<int>(std::lround(255.0 * norm));
    const int green = static_cast<int>(std::lround(120.0 * (1.0 - std::abs(norm - 0.5) * 2.0)));
    const int blue = static_cast<int>(std::lround(255.0 * (1.0 - norm)));
    return ftxui::Color::RGB(static_cast<uint8_t>(clamp_u8(red)),
                             static_cast<uint8_t>(clamp_u8(green)),
                             static_cast<uint8_t>(clamp_u8(blue)));
}

inline void draw_heatmap(ftxui::Canvas& canvas, const omni_ftxui_plot_options* options) {
    const int width = std::max(1, options->width);
    const int height = std::max(1, options->height);
    const auto [min_it, max_it] = std::minmax_element(options->values, options->values + options->value_count);
    const double min_value = *min_it;
    const double max_value = *max_it;
    for (size_t row = 0; row < options->heatmap_rows; ++row) {
        const int y0 = static_cast<int>((row * static_cast<size_t>(height)) / options->heatmap_rows);
        const int y1 = static_cast<int>(((row + 1) * static_cast<size_t>(height)) / options->heatmap_rows);
        for (size_t col = 0; col < options->heatmap_cols; ++col) {
            const int x0 = static_cast<int>((col * static_cast<size_t>(width)) / options->heatmap_cols);
            const int x1 = static_cast<int>(((col + 1) * static_cast<size_t>(width)) / options->heatmap_cols);
            const double value = options->values[row * options->heatmap_cols + col];
            const ftxui::Color color = heatmap_color(value, min_value, max_value);
            for (int y = y0; y < std::max(y0 + 1, y1); ++y) {
                for (int x = x0; x < std::max(x0 + 1, x1); ++x) {
                    canvas.DrawBlock(x, y, true, color);
                }
            }
        }
    }
}
#endif

}  // namespace

extern "C" {

#include "ftxui_shim_runtime.inc"
#include "ftxui_shim_element.inc"
#include "ftxui_shim_component.inc"
#include "ftxui_shim_table_canvas.inc"

}  // extern "C"
