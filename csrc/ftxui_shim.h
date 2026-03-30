#ifndef OMNI_FTXUI_SHIM_H
#define OMNI_FTXUI_SHIM_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define OMNI_FTXUI_SHIM_ABI_VERSION 3u

typedef struct omni_ftxui_context_t omni_ftxui_context_t;
typedef struct omni_ftxui_app_t omni_ftxui_app_t;
typedef struct omni_ftxui_screen_t omni_ftxui_screen_t;
typedef struct omni_ftxui_event_t omni_ftxui_event_t;
typedef struct omni_ftxui_element_t omni_ftxui_element_t;
typedef struct omni_ftxui_component_t omni_ftxui_component_t;
typedef struct omni_ftxui_table_t omni_ftxui_table_t;
typedef struct omni_ftxui_canvas_t omni_ftxui_canvas_t;

typedef enum omni_ftxui_status {
    OMNI_FTXUI_STATUS_OK = 0,
    OMNI_FTXUI_STATUS_INVALID_ARGUMENT = 1,
    OMNI_FTXUI_STATUS_OUT_OF_MEMORY = 2,
    OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE = 3,
    OMNI_FTXUI_STATUS_NOT_SUPPORTED = 4,
    OMNI_FTXUI_STATUS_INTERNAL_ERROR = 5,
} omni_ftxui_status;

typedef enum omni_ftxui_color_kind {
    OMNI_FTXUI_COLOR_DEFAULT = 0,
    OMNI_FTXUI_COLOR_INDEXED_16 = 1,
    OMNI_FTXUI_COLOR_INDEXED_256 = 2,
    OMNI_FTXUI_COLOR_RGB = 3,
    OMNI_FTXUI_COLOR_HSV = 4,
} omni_ftxui_color_kind;

typedef struct omni_ftxui_color {
    uint32_t size;
    uint32_t abi_version;
    omni_ftxui_color_kind kind;
    int32_t a;
    int32_t b;
    int32_t c;
} omni_ftxui_color;

typedef enum omni_ftxui_dimension_kind {
    OMNI_FTXUI_DIMENSION_FULL = 0,
    OMNI_FTXUI_DIMENSION_FIT = 1,
    OMNI_FTXUI_DIMENSION_FIXED = 2,
} omni_ftxui_dimension_kind;

typedef struct omni_ftxui_dimension {
    uint32_t size;
    uint32_t abi_version;
    omni_ftxui_dimension_kind kind;
    int32_t value;
} omni_ftxui_dimension;

typedef enum omni_ftxui_direction {
    OMNI_FTXUI_DIRECTION_LEFT = 0,
    OMNI_FTXUI_DIRECTION_RIGHT = 1,
    OMNI_FTXUI_DIRECTION_UP = 2,
    OMNI_FTXUI_DIRECTION_DOWN = 3,
} omni_ftxui_direction;

typedef enum omni_ftxui_width_or_height {
    OMNI_FTXUI_WIDTH = 0,
    OMNI_FTXUI_HEIGHT = 1,
} omni_ftxui_width_or_height;

typedef enum omni_ftxui_constraint {
    OMNI_FTXUI_LESS_THAN = 0,
    OMNI_FTXUI_EQUAL = 1,
    OMNI_FTXUI_GREATER_THAN = 2,
} omni_ftxui_constraint;

typedef enum omni_ftxui_border_style {
    OMNI_FTXUI_BORDER_LIGHT = 0,
    OMNI_FTXUI_BORDER_DASHED = 1,
    OMNI_FTXUI_BORDER_HEAVY = 2,
    OMNI_FTXUI_BORDER_ROUNDED = 3,
    OMNI_FTXUI_BORDER_DOUBLE = 4,
    OMNI_FTXUI_BORDER_EMPTY = 5,
} omni_ftxui_border_style;

typedef enum omni_ftxui_screen_mode {
    OMNI_FTXUI_SCREEN_FULLSCREEN = 0,
    OMNI_FTXUI_SCREEN_FULLSCREEN_PRIMARY = 1,
    OMNI_FTXUI_SCREEN_FULLSCREEN_ALTERNATE = 2,
    OMNI_FTXUI_SCREEN_FIT_COMPONENT = 3,
    OMNI_FTXUI_SCREEN_TERMINAL_OUTPUT = 4,
} omni_ftxui_screen_mode;

typedef enum omni_ftxui_event_kind {
    OMNI_FTXUI_EVENT_CUSTOM = 0,
    OMNI_FTXUI_EVENT_CHARACTER = 1,
    OMNI_FTXUI_EVENT_SPECIAL = 2,
    OMNI_FTXUI_EVENT_ARROW_LEFT = 3,
    OMNI_FTXUI_EVENT_ARROW_RIGHT = 4,
    OMNI_FTXUI_EVENT_ARROW_UP = 5,
    OMNI_FTXUI_EVENT_ARROW_DOWN = 6,
    OMNI_FTXUI_EVENT_RETURN = 7,
    OMNI_FTXUI_EVENT_ESCAPE = 8,
    OMNI_FTXUI_EVENT_TAB = 9,
    OMNI_FTXUI_EVENT_TAB_REVERSE = 10,
    OMNI_FTXUI_EVENT_BACKSPACE = 11,
    OMNI_FTXUI_EVENT_DELETE = 12,
    OMNI_FTXUI_EVENT_HOME = 13,
    OMNI_FTXUI_EVENT_END = 14,
    OMNI_FTXUI_EVENT_PAGE_UP = 15,
    OMNI_FTXUI_EVENT_PAGE_DOWN = 16,
    OMNI_FTXUI_EVENT_F1 = 17,
    OMNI_FTXUI_EVENT_F2 = 18,
    OMNI_FTXUI_EVENT_F3 = 19,
    OMNI_FTXUI_EVENT_F4 = 20,
    OMNI_FTXUI_EVENT_F5 = 21,
    OMNI_FTXUI_EVENT_F6 = 22,
    OMNI_FTXUI_EVENT_F7 = 23,
    OMNI_FTXUI_EVENT_F8 = 24,
    OMNI_FTXUI_EVENT_F9 = 25,
    OMNI_FTXUI_EVENT_F10 = 26,
    OMNI_FTXUI_EVENT_F11 = 27,
    OMNI_FTXUI_EVENT_F12 = 28,
} omni_ftxui_event_kind;

typedef struct omni_ftxui_event_spec {
    uint32_t size;
    uint32_t abi_version;
    omni_ftxui_event_kind kind;
    const char* text;
} omni_ftxui_event_spec;

typedef omni_ftxui_status (*omni_ftxui_graph_callback_fn)(
    void* user_data,
    omni_ftxui_context_t* context,
    int32_t width,
    int32_t height,
    int32_t* out_values
);

typedef enum omni_ftxui_element_kind {
    OMNI_FTXUI_ELEMENT_EMPTY = 0,
    OMNI_FTXUI_ELEMENT_TEXT = 1,
    OMNI_FTXUI_ELEMENT_VTEXT = 2,
    OMNI_FTXUI_ELEMENT_PARAGRAPH = 3,
    OMNI_FTXUI_ELEMENT_PARAGRAPH_LEFT = 4,
    OMNI_FTXUI_ELEMENT_PARAGRAPH_CENTER = 5,
    OMNI_FTXUI_ELEMENT_PARAGRAPH_RIGHT = 6,
    OMNI_FTXUI_ELEMENT_PARAGRAPH_JUSTIFY = 7,
    OMNI_FTXUI_ELEMENT_SEPARATOR = 8,
    OMNI_FTXUI_ELEMENT_SEPARATOR_LIGHT = 9,
    OMNI_FTXUI_ELEMENT_SEPARATOR_HEAVY = 10,
    OMNI_FTXUI_ELEMENT_SEPARATOR_DOUBLE = 11,
    OMNI_FTXUI_ELEMENT_SPINNER = 12,
    OMNI_FTXUI_ELEMENT_GAUGE = 13,
    OMNI_FTXUI_ELEMENT_GAUGE_LEFT = 14,
    OMNI_FTXUI_ELEMENT_GAUGE_RIGHT = 15,
    OMNI_FTXUI_ELEMENT_GAUGE_UP = 16,
    OMNI_FTXUI_ELEMENT_GAUGE_DOWN = 17,
    OMNI_FTXUI_ELEMENT_HBOX = 18,
    OMNI_FTXUI_ELEMENT_VBOX = 19,
    OMNI_FTXUI_ELEMENT_DBOX = 20,
    OMNI_FTXUI_ELEMENT_HFLOW = 21,
    OMNI_FTXUI_ELEMENT_VFLOW = 22,
    OMNI_FTXUI_ELEMENT_FLEXBOX = 23,
    OMNI_FTXUI_ELEMENT_GRIDBOX = 24,
    OMNI_FTXUI_ELEMENT_WINDOW = 25,
    OMNI_FTXUI_ELEMENT_FILLER = 26,
    OMNI_FTXUI_ELEMENT_CANVAS = 27,
    OMNI_FTXUI_ELEMENT_TABLE = 28,
    OMNI_FTXUI_ELEMENT_GRAPH = 29,
} omni_ftxui_element_kind;

typedef struct omni_ftxui_element_create_options {
    uint32_t size;
    uint32_t abi_version;
    omni_ftxui_element_kind kind;
    const char* text;
    float gauge_progress;
    omni_ftxui_direction direction;
    int32_t spinner_charset;
    size_t spinner_frame;
    int32_t canvas_width;
    int32_t canvas_height;
    size_t table_rows;
    size_t table_cols;
    void* graph_user_data;
    omni_ftxui_graph_callback_fn graph_cb;
} omni_ftxui_element_create_options;

typedef enum omni_ftxui_decorator_kind {
    OMNI_FTXUI_DECORATOR_BORDER = 0,
    OMNI_FTXUI_DECORATOR_BORDER_LIGHT = 1,
    OMNI_FTXUI_DECORATOR_BORDER_HEAVY = 2,
    OMNI_FTXUI_DECORATOR_BORDER_DOUBLE = 3,
    OMNI_FTXUI_DECORATOR_BORDER_ROUNDED = 4,
    OMNI_FTXUI_DECORATOR_BORDER_EMPTY = 5,
    OMNI_FTXUI_DECORATOR_FRAME = 6,
    OMNI_FTXUI_DECORATOR_XFRAME = 7,
    OMNI_FTXUI_DECORATOR_YFRAME = 8,
    OMNI_FTXUI_DECORATOR_CENTER = 9,
    OMNI_FTXUI_DECORATOR_HCENTER = 10,
    OMNI_FTXUI_DECORATOR_VCENTER = 11,
    OMNI_FTXUI_DECORATOR_ALIGN_RIGHT = 12,
    OMNI_FTXUI_DECORATOR_BOLD = 13,
    OMNI_FTXUI_DECORATOR_ITALIC = 14,
    OMNI_FTXUI_DECORATOR_DIM = 15,
    OMNI_FTXUI_DECORATOR_INVERTED = 16,
    OMNI_FTXUI_DECORATOR_UNDERLINED = 17,
    OMNI_FTXUI_DECORATOR_UNDERLINED_DOUBLE = 18,
    OMNI_FTXUI_DECORATOR_STRIKETHROUGH = 19,
    OMNI_FTXUI_DECORATOR_BLINK = 20,
    OMNI_FTXUI_DECORATOR_COLOR = 21,
    OMNI_FTXUI_DECORATOR_BGCOLOR = 22,
    OMNI_FTXUI_DECORATOR_FLEX = 23,
    OMNI_FTXUI_DECORATOR_FLEX_GROW = 24,
    OMNI_FTXUI_DECORATOR_FLEX_SHRINK = 25,
    OMNI_FTXUI_DECORATOR_XFLEX = 26,
    OMNI_FTXUI_DECORATOR_XFLEX_GROW = 27,
    OMNI_FTXUI_DECORATOR_XFLEX_SHRINK = 28,
    OMNI_FTXUI_DECORATOR_YFLEX = 29,
    OMNI_FTXUI_DECORATOR_YFLEX_GROW = 30,
    OMNI_FTXUI_DECORATOR_YFLEX_SHRINK = 31,
    OMNI_FTXUI_DECORATOR_NOTFLEX = 32,
    OMNI_FTXUI_DECORATOR_FOCUS = 33,
    OMNI_FTXUI_DECORATOR_SELECT = 34,
    OMNI_FTXUI_DECORATOR_AUTOMERGE = 35,
    OMNI_FTXUI_DECORATOR_VSCROLL_INDICATOR = 36,
    OMNI_FTXUI_DECORATOR_HSCROLL_INDICATOR = 37,
    OMNI_FTXUI_DECORATOR_CLEAR_UNDER = 38,
    OMNI_FTXUI_DECORATOR_SIZE = 39,
    OMNI_FTXUI_DECORATOR_HYPERLINK = 40,
    OMNI_FTXUI_DECORATOR_SELECTION_STYLE_RESET = 41,
    OMNI_FTXUI_DECORATOR_SELECTION_COLOR = 42,
    OMNI_FTXUI_DECORATOR_SELECTION_BACKGROUND_COLOR = 43,
    OMNI_FTXUI_DECORATOR_SELECTION_FOREGROUND_COLOR = 44,
} omni_ftxui_decorator_kind;

typedef struct omni_ftxui_decorator_options {
    uint32_t size;
    uint32_t abi_version;
    omni_ftxui_decorator_kind kind;
    omni_ftxui_color color;
    omni_ftxui_width_or_height width_or_height;
    omni_ftxui_constraint constraint;
    int32_t size_value;
    const char* link;
} omni_ftxui_decorator_options;

typedef enum omni_ftxui_component_kind {
    OMNI_FTXUI_COMPONENT_CONTAINER_HORIZONTAL = 0,
    OMNI_FTXUI_COMPONENT_CONTAINER_VERTICAL = 1,
    OMNI_FTXUI_COMPONENT_CONTAINER_TAB = 2,
    OMNI_FTXUI_COMPONENT_CONTAINER_STACKED = 3,
    OMNI_FTXUI_COMPONENT_BUTTON = 4,
    OMNI_FTXUI_COMPONENT_CHECKBOX = 5,
    OMNI_FTXUI_COMPONENT_INPUT = 6,
    OMNI_FTXUI_COMPONENT_MENU = 7,
    OMNI_FTXUI_COMPONENT_MENU_ENTRY = 8,
    OMNI_FTXUI_COMPONENT_RADIOBOX = 9,
    OMNI_FTXUI_COMPONENT_DROPDOWN = 10,
    OMNI_FTXUI_COMPONENT_TOGGLE = 11,
    OMNI_FTXUI_COMPONENT_SLIDER_INT = 12,
    OMNI_FTXUI_COMPONENT_SLIDER_FLOAT = 13,
    OMNI_FTXUI_COMPONENT_WINDOW = 14,
    OMNI_FTXUI_COMPONENT_COLLAPSIBLE = 15,
    OMNI_FTXUI_COMPONENT_MODAL = 16,
    OMNI_FTXUI_COMPONENT_RENDERER = 17,
    OMNI_FTXUI_COMPONENT_HOVERABLE = 18,
    OMNI_FTXUI_COMPONENT_RESIZABLE_SPLIT = 19,
} omni_ftxui_component_kind;

typedef struct omni_ftxui_component_create_options {
    uint32_t size;
    uint32_t abi_version;
    omni_ftxui_component_kind kind;
    const char* label;
    const char* title;
    const char* text_value;
    const char** items;
    size_t item_count;
    int* selected_index;
    bool* checked_value;
    int* int_value;
    int int_min;
    int int_max;
    int int_step;
    float* float_value;
    float float_min;
    float float_max;
    float float_step;
    bool* visible_ptr;
    int* left;
    int* top;
    int* width;
    int* height;
    bool* resize_left;
    bool* resize_right;
    bool* resize_top;
    bool* resize_down;
    bool* hover_value;
    int split_direction;
    int* main_size;
} omni_ftxui_component_create_options;

typedef struct omni_ftxui_table_create_options {
    uint32_t size;
    uint32_t abi_version;
    size_t rows;
    size_t cols;
    const char** cells;
} omni_ftxui_table_create_options;

typedef struct omni_ftxui_canvas_create_options {
    uint32_t size;
    uint32_t abi_version;
    int32_t width;
    int32_t height;
} omni_ftxui_canvas_create_options;

typedef struct omni_ftxui_screen_create_options {
    uint32_t size;
    uint32_t abi_version;
    omni_ftxui_screen_mode mode;
    bool track_mouse;
    bool handle_piped_input;
    omni_ftxui_dimension width;
    omni_ftxui_dimension height;
} omni_ftxui_screen_create_options;

typedef omni_ftxui_status (*omni_ftxui_render_callback_fn)(
    void* user_data,
    omni_ftxui_context_t* context,
    omni_ftxui_element_t** out_element
);

typedef int (*omni_ftxui_event_callback_fn)(
    void* user_data,
    omni_ftxui_context_t* context,
    const omni_ftxui_event_t* event
);

typedef void (*omni_ftxui_action_callback_fn)(void* user_data);

typedef struct omni_ftxui_callback_options {
    uint32_t size;
    uint32_t abi_version;
    void* user_data;
    omni_ftxui_render_callback_fn render_cb;
    omni_ftxui_event_callback_fn event_cb;
    omni_ftxui_action_callback_fn action_cb;
} omni_ftxui_callback_options;

/* Returns 1 when the binary is linked with an available FTXUI backend. */
int omni_ftxui_backend_available(void);
const char* omni_ftxui_status_name(omni_ftxui_status status);

omni_ftxui_status omni_ftxui_context_create(omni_ftxui_context_t** out_context);
void omni_ftxui_context_destroy(omni_ftxui_context_t* context);
omni_ftxui_status omni_ftxui_context_last_error_code(const omni_ftxui_context_t* context);
const char* omni_ftxui_context_last_error_message(const omni_ftxui_context_t* context);
void omni_ftxui_context_clear_error(omni_ftxui_context_t* context);

omni_ftxui_status omni_ftxui_event_create(
    omni_ftxui_context_t* context,
    const omni_ftxui_event_spec* spec,
    omni_ftxui_event_t** out_event
);
void omni_ftxui_event_destroy(omni_ftxui_event_t* event);

omni_ftxui_status omni_ftxui_screen_create(
    omni_ftxui_context_t* context,
    const omni_ftxui_screen_create_options* options,
    omni_ftxui_screen_t** out_screen
);
void omni_ftxui_screen_destroy(omni_ftxui_screen_t* screen);
omni_ftxui_status omni_ftxui_screen_loop(
    omni_ftxui_context_t* context,
    omni_ftxui_screen_t* screen,
    omni_ftxui_component_t* root
);
omni_ftxui_status omni_ftxui_screen_post_event(
    omni_ftxui_context_t* context,
    omni_ftxui_screen_t* screen,
    const omni_ftxui_event_t* event
);
omni_ftxui_status omni_ftxui_screen_exit(
    omni_ftxui_context_t* context,
    omni_ftxui_screen_t* screen
);
omni_ftxui_status omni_ftxui_screen_request_animation_frame(
    omni_ftxui_context_t* context,
    omni_ftxui_screen_t* screen
);
omni_ftxui_status omni_ftxui_screen_set_track_mouse(
    omni_ftxui_context_t* context,
    omni_ftxui_screen_t* screen,
    bool enabled
);
omni_ftxui_status omni_ftxui_screen_set_handle_piped_input(
    omni_ftxui_context_t* context,
    omni_ftxui_screen_t* screen,
    bool enabled
);

/* App is a lightweight wrapper around screen + root component. It does not own either handle. */
omni_ftxui_status omni_ftxui_app_create(
    omni_ftxui_context_t* context,
    omni_ftxui_screen_t* screen,
    omni_ftxui_component_t* root,
    omni_ftxui_app_t** out_app
);
void omni_ftxui_app_destroy(omni_ftxui_app_t* app);
omni_ftxui_status omni_ftxui_app_run(omni_ftxui_context_t* context, omni_ftxui_app_t* app);
omni_ftxui_status omni_ftxui_app_exit(omni_ftxui_context_t* context, omni_ftxui_app_t* app);
omni_ftxui_status omni_ftxui_app_post_event(
    omni_ftxui_context_t* context,
    omni_ftxui_app_t* app,
    const omni_ftxui_event_t* event
);

omni_ftxui_status omni_ftxui_element_create(
    omni_ftxui_context_t* context,
    const omni_ftxui_element_create_options* options,
    omni_ftxui_element_t* const* children,
    size_t child_count,
    omni_ftxui_element_t** out_element
);
omni_ftxui_status omni_ftxui_element_graph_from_series(
    omni_ftxui_context_t* context,
    const double* values,
    size_t value_count,
    omni_ftxui_element_t** out_element
);
void omni_ftxui_element_destroy(omni_ftxui_element_t* element);
omni_ftxui_status omni_ftxui_element_apply_decorator(
    omni_ftxui_context_t* context,
    omni_ftxui_element_t* element,
    const omni_ftxui_decorator_options* options
);

omni_ftxui_status omni_ftxui_component_create(
    omni_ftxui_context_t* context,
    const omni_ftxui_component_create_options* options,
    omni_ftxui_component_t* const* children,
    size_t child_count,
    omni_ftxui_component_t** out_component
);
void omni_ftxui_component_destroy(omni_ftxui_component_t* component);
omni_ftxui_status omni_ftxui_component_add_child(
    omni_ftxui_context_t* context,
    omni_ftxui_component_t* parent,
    omni_ftxui_component_t* child
);
omni_ftxui_status omni_ftxui_component_take_focus(
    omni_ftxui_context_t* context,
    omni_ftxui_component_t* component
);

/*
 * Generic callback wrappers:
 * - renderer: wraps base component with a custom render callback.
 * - event handler: wraps base component with a custom event callback.
 * - action: attaches a generic click action to a button-like wrapper.
 */
omni_ftxui_status omni_ftxui_component_wrap_renderer(
    omni_ftxui_context_t* context,
    omni_ftxui_component_t* base,
    const omni_ftxui_callback_options* callbacks,
    omni_ftxui_component_t** out_component
);
omni_ftxui_status omni_ftxui_component_wrap_event_handler(
    omni_ftxui_context_t* context,
    omni_ftxui_component_t* base,
    const omni_ftxui_callback_options* callbacks,
    omni_ftxui_component_t** out_component
);
omni_ftxui_status omni_ftxui_component_from_element(
    omni_ftxui_context_t* context,
    omni_ftxui_element_t* element,
    omni_ftxui_component_t** out_component
);
omni_ftxui_status omni_ftxui_component_wrap_quit_keys(
    omni_ftxui_context_t* context,
    omni_ftxui_screen_t* screen,
    omni_ftxui_component_t* base,
    omni_ftxui_component_t** out_component
);
omni_ftxui_status omni_ftxui_component_wrap_maybe(
    omni_ftxui_context_t* context,
    omni_ftxui_component_t* base,
    bool* visible_ptr,
    omni_ftxui_component_t** out_component
);

omni_ftxui_status omni_ftxui_table_create(
    omni_ftxui_context_t* context,
    const omni_ftxui_table_create_options* options,
    omni_ftxui_table_t** out_table
);
void omni_ftxui_table_destroy(omni_ftxui_table_t* table);
omni_ftxui_status omni_ftxui_table_select_all(
    omni_ftxui_context_t* context,
    omni_ftxui_table_t* table,
    omni_ftxui_border_style border
);
omni_ftxui_status omni_ftxui_table_select_row(
    omni_ftxui_context_t* context,
    omni_ftxui_table_t* table,
    size_t row,
    omni_ftxui_border_style border
);
omni_ftxui_status omni_ftxui_table_select_column(
    omni_ftxui_context_t* context,
    omni_ftxui_table_t* table,
    size_t column,
    omni_ftxui_border_style border
);
omni_ftxui_status omni_ftxui_table_select_cell(
    omni_ftxui_context_t* context,
    omni_ftxui_table_t* table,
    size_t row,
    size_t column,
    omni_ftxui_border_style border
);
omni_ftxui_status omni_ftxui_table_render(
    omni_ftxui_context_t* context,
    omni_ftxui_table_t* table,
    omni_ftxui_element_t** out_element
);

omni_ftxui_status omni_ftxui_canvas_create(
    omni_ftxui_context_t* context,
    const omni_ftxui_canvas_create_options* options,
    omni_ftxui_canvas_t** out_canvas
);
void omni_ftxui_canvas_destroy(omni_ftxui_canvas_t* canvas);
omni_ftxui_status omni_ftxui_canvas_draw_text(
    omni_ftxui_context_t* context,
    omni_ftxui_canvas_t* canvas,
    int32_t x,
    int32_t y,
    const char* text
);
omni_ftxui_status omni_ftxui_canvas_draw_point_on(
    omni_ftxui_context_t* context,
    omni_ftxui_canvas_t* canvas,
    int32_t x,
    int32_t y
);
omni_ftxui_status omni_ftxui_canvas_draw_point_off(
    omni_ftxui_context_t* context,
    omni_ftxui_canvas_t* canvas,
    int32_t x,
    int32_t y
);
omni_ftxui_status omni_ftxui_canvas_draw_line(
    omni_ftxui_context_t* context,
    omni_ftxui_canvas_t* canvas,
    int32_t x1,
    int32_t y1,
    int32_t x2,
    int32_t y2
);
omni_ftxui_status omni_ftxui_canvas_render(
    omni_ftxui_context_t* context,
    omni_ftxui_canvas_t* canvas,
    omni_ftxui_element_t** out_element
);

#ifdef __cplusplus
}
#endif

#endif
