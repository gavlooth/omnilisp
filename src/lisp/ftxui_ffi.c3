module lisp;

// Mirror of csrc/ftxui_shim.h. Keep this file aligned with the C header.

const uint OMNI_FTXUI_ABI_VERSION = 3;

alias OmniFtxuiStatus = int;
alias OmniFtxuiContext = void;
alias OmniFtxuiApp = void;
alias OmniFtxuiScreen = void;
alias OmniFtxuiEventHandle = void;
alias OmniFtxuiElement = void;
alias OmniFtxuiComponent = void;
alias OmniFtxuiTable = void;
alias OmniFtxuiCanvas = void;

const OmniFtxuiStatus OMNI_FTXUI_STATUS_OK = 0;
const OmniFtxuiStatus OMNI_FTXUI_STATUS_INVALID_ARGUMENT = 1;
const OmniFtxuiStatus OMNI_FTXUI_STATUS_OUT_OF_MEMORY = 2;
const OmniFtxuiStatus OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE = 3;
const OmniFtxuiStatus OMNI_FTXUI_STATUS_NOT_SUPPORTED = 4;
const OmniFtxuiStatus OMNI_FTXUI_STATUS_INTERNAL_ERROR = 5;

const int OMNI_FTXUI_COLOR_DEFAULT = 0;
const int OMNI_FTXUI_COLOR_INDEXED_16 = 1;
const int OMNI_FTXUI_COLOR_INDEXED_256 = 2;
const int OMNI_FTXUI_COLOR_RGB = 3;
const int OMNI_FTXUI_COLOR_HSV = 4;

const int OMNI_FTXUI_DIMENSION_FULL = 0;
const int OMNI_FTXUI_DIMENSION_FIT = 1;
const int OMNI_FTXUI_DIMENSION_FIXED = 2;

const int OMNI_FTXUI_DIRECTION_LEFT = 0;
const int OMNI_FTXUI_DIRECTION_RIGHT = 1;
const int OMNI_FTXUI_DIRECTION_UP = 2;
const int OMNI_FTXUI_DIRECTION_DOWN = 3;

const int OMNI_FTXUI_WIDTH = 0;
const int OMNI_FTXUI_HEIGHT = 1;

const int OMNI_FTXUI_LESS_THAN = 0;
const int OMNI_FTXUI_EQUAL = 1;
const int OMNI_FTXUI_GREATER_THAN = 2;

const int OMNI_FTXUI_BORDER_LIGHT = 0;
const int OMNI_FTXUI_BORDER_DASHED = 1;
const int OMNI_FTXUI_BORDER_HEAVY = 2;
const int OMNI_FTXUI_BORDER_ROUNDED = 3;
const int OMNI_FTXUI_BORDER_DOUBLE = 4;
const int OMNI_FTXUI_BORDER_EMPTY = 5;

const int OMNI_FTXUI_SCREEN_FULLSCREEN = 0;
const int OMNI_FTXUI_SCREEN_FULLSCREEN_PRIMARY = 1;
const int OMNI_FTXUI_SCREEN_FULLSCREEN_ALTERNATE = 2;
const int OMNI_FTXUI_SCREEN_FIT_COMPONENT = 3;
const int OMNI_FTXUI_SCREEN_TERMINAL_OUTPUT = 4;

const int OMNI_FTXUI_EVENT_CUSTOM = 0;
const int OMNI_FTXUI_EVENT_CHARACTER = 1;
const int OMNI_FTXUI_EVENT_SPECIAL = 2;
const int OMNI_FTXUI_EVENT_ARROW_LEFT = 3;
const int OMNI_FTXUI_EVENT_ARROW_RIGHT = 4;
const int OMNI_FTXUI_EVENT_ARROW_UP = 5;
const int OMNI_FTXUI_EVENT_ARROW_DOWN = 6;
const int OMNI_FTXUI_EVENT_RETURN = 7;
const int OMNI_FTXUI_EVENT_ESCAPE = 8;
const int OMNI_FTXUI_EVENT_TAB = 9;
const int OMNI_FTXUI_EVENT_TAB_REVERSE = 10;
const int OMNI_FTXUI_EVENT_BACKSPACE = 11;
const int OMNI_FTXUI_EVENT_DELETE = 12;
const int OMNI_FTXUI_EVENT_HOME = 13;
const int OMNI_FTXUI_EVENT_END = 14;
const int OMNI_FTXUI_EVENT_PAGE_UP = 15;
const int OMNI_FTXUI_EVENT_PAGE_DOWN = 16;
const int OMNI_FTXUI_EVENT_F1 = 17;
const int OMNI_FTXUI_EVENT_F2 = 18;
const int OMNI_FTXUI_EVENT_F3 = 19;
const int OMNI_FTXUI_EVENT_F4 = 20;
const int OMNI_FTXUI_EVENT_F5 = 21;
const int OMNI_FTXUI_EVENT_F6 = 22;
const int OMNI_FTXUI_EVENT_F7 = 23;
const int OMNI_FTXUI_EVENT_F8 = 24;
const int OMNI_FTXUI_EVENT_F9 = 25;
const int OMNI_FTXUI_EVENT_F10 = 26;
const int OMNI_FTXUI_EVENT_F11 = 27;
const int OMNI_FTXUI_EVENT_F12 = 28;

const int OMNI_FTXUI_ELEMENT_EMPTY = 0;
const int OMNI_FTXUI_ELEMENT_TEXT = 1;
const int OMNI_FTXUI_ELEMENT_VTEXT = 2;
const int OMNI_FTXUI_ELEMENT_PARAGRAPH = 3;
const int OMNI_FTXUI_ELEMENT_PARAGRAPH_LEFT = 4;
const int OMNI_FTXUI_ELEMENT_PARAGRAPH_CENTER = 5;
const int OMNI_FTXUI_ELEMENT_PARAGRAPH_RIGHT = 6;
const int OMNI_FTXUI_ELEMENT_PARAGRAPH_JUSTIFY = 7;
const int OMNI_FTXUI_ELEMENT_SEPARATOR = 8;
const int OMNI_FTXUI_ELEMENT_SEPARATOR_LIGHT = 9;
const int OMNI_FTXUI_ELEMENT_SEPARATOR_HEAVY = 10;
const int OMNI_FTXUI_ELEMENT_SEPARATOR_DOUBLE = 11;
const int OMNI_FTXUI_ELEMENT_SPINNER = 12;
const int OMNI_FTXUI_ELEMENT_GAUGE = 13;
const int OMNI_FTXUI_ELEMENT_GAUGE_LEFT = 14;
const int OMNI_FTXUI_ELEMENT_GAUGE_RIGHT = 15;
const int OMNI_FTXUI_ELEMENT_GAUGE_UP = 16;
const int OMNI_FTXUI_ELEMENT_GAUGE_DOWN = 17;
const int OMNI_FTXUI_ELEMENT_HBOX = 18;
const int OMNI_FTXUI_ELEMENT_VBOX = 19;
const int OMNI_FTXUI_ELEMENT_DBOX = 20;
const int OMNI_FTXUI_ELEMENT_HFLOW = 21;
const int OMNI_FTXUI_ELEMENT_VFLOW = 22;
const int OMNI_FTXUI_ELEMENT_FLEXBOX = 23;
const int OMNI_FTXUI_ELEMENT_GRIDBOX = 24;
const int OMNI_FTXUI_ELEMENT_WINDOW = 25;
const int OMNI_FTXUI_ELEMENT_FILLER = 26;
const int OMNI_FTXUI_ELEMENT_CANVAS = 27;
const int OMNI_FTXUI_ELEMENT_TABLE = 28;
const int OMNI_FTXUI_ELEMENT_GRAPH = 29;

const int OMNI_FTXUI_DECORATOR_BORDER = 0;
const int OMNI_FTXUI_DECORATOR_BORDER_LIGHT = 1;
const int OMNI_FTXUI_DECORATOR_BORDER_HEAVY = 2;
const int OMNI_FTXUI_DECORATOR_BORDER_DOUBLE = 3;
const int OMNI_FTXUI_DECORATOR_BORDER_ROUNDED = 4;
const int OMNI_FTXUI_DECORATOR_BORDER_EMPTY = 5;
const int OMNI_FTXUI_DECORATOR_FRAME = 6;
const int OMNI_FTXUI_DECORATOR_XFRAME = 7;
const int OMNI_FTXUI_DECORATOR_YFRAME = 8;
const int OMNI_FTXUI_DECORATOR_CENTER = 9;
const int OMNI_FTXUI_DECORATOR_HCENTER = 10;
const int OMNI_FTXUI_DECORATOR_VCENTER = 11;
const int OMNI_FTXUI_DECORATOR_ALIGN_RIGHT = 12;
const int OMNI_FTXUI_DECORATOR_BOLD = 13;
const int OMNI_FTXUI_DECORATOR_ITALIC = 14;
const int OMNI_FTXUI_DECORATOR_DIM = 15;
const int OMNI_FTXUI_DECORATOR_INVERTED = 16;
const int OMNI_FTXUI_DECORATOR_UNDERLINED = 17;
const int OMNI_FTXUI_DECORATOR_UNDERLINED_DOUBLE = 18;
const int OMNI_FTXUI_DECORATOR_STRIKETHROUGH = 19;
const int OMNI_FTXUI_DECORATOR_BLINK = 20;
const int OMNI_FTXUI_DECORATOR_COLOR = 21;
const int OMNI_FTXUI_DECORATOR_BGCOLOR = 22;
const int OMNI_FTXUI_DECORATOR_FLEX = 23;
const int OMNI_FTXUI_DECORATOR_FLEX_GROW = 24;
const int OMNI_FTXUI_DECORATOR_FLEX_SHRINK = 25;
const int OMNI_FTXUI_DECORATOR_XFLEX = 26;
const int OMNI_FTXUI_DECORATOR_XFLEX_GROW = 27;
const int OMNI_FTXUI_DECORATOR_XFLEX_SHRINK = 28;
const int OMNI_FTXUI_DECORATOR_YFLEX = 29;
const int OMNI_FTXUI_DECORATOR_YFLEX_GROW = 30;
const int OMNI_FTXUI_DECORATOR_YFLEX_SHRINK = 31;
const int OMNI_FTXUI_DECORATOR_NOTFLEX = 32;
const int OMNI_FTXUI_DECORATOR_FOCUS = 33;
const int OMNI_FTXUI_DECORATOR_SELECT = 34;
const int OMNI_FTXUI_DECORATOR_AUTOMERGE = 35;
const int OMNI_FTXUI_DECORATOR_VSCROLL_INDICATOR = 36;
const int OMNI_FTXUI_DECORATOR_HSCROLL_INDICATOR = 37;
const int OMNI_FTXUI_DECORATOR_CLEAR_UNDER = 38;
const int OMNI_FTXUI_DECORATOR_SIZE = 39;
const int OMNI_FTXUI_DECORATOR_HYPERLINK = 40;
const int OMNI_FTXUI_DECORATOR_SELECTION_STYLE_RESET = 41;
const int OMNI_FTXUI_DECORATOR_SELECTION_COLOR = 42;
const int OMNI_FTXUI_DECORATOR_SELECTION_BACKGROUND_COLOR = 43;
const int OMNI_FTXUI_DECORATOR_SELECTION_FOREGROUND_COLOR = 44;

const int OMNI_FTXUI_COMPONENT_CONTAINER_HORIZONTAL = 0;
const int OMNI_FTXUI_COMPONENT_CONTAINER_VERTICAL = 1;
const int OMNI_FTXUI_COMPONENT_CONTAINER_TAB = 2;
const int OMNI_FTXUI_COMPONENT_CONTAINER_STACKED = 3;
const int OMNI_FTXUI_COMPONENT_BUTTON = 4;
const int OMNI_FTXUI_COMPONENT_CHECKBOX = 5;
const int OMNI_FTXUI_COMPONENT_INPUT = 6;
const int OMNI_FTXUI_COMPONENT_MENU = 7;
const int OMNI_FTXUI_COMPONENT_MENU_ENTRY = 8;
const int OMNI_FTXUI_COMPONENT_RADIOBOX = 9;
const int OMNI_FTXUI_COMPONENT_DROPDOWN = 10;
const int OMNI_FTXUI_COMPONENT_TOGGLE = 11;
const int OMNI_FTXUI_COMPONENT_SLIDER_INT = 12;
const int OMNI_FTXUI_COMPONENT_SLIDER_FLOAT = 13;
const int OMNI_FTXUI_COMPONENT_WINDOW = 14;
const int OMNI_FTXUI_COMPONENT_COLLAPSIBLE = 15;
const int OMNI_FTXUI_COMPONENT_MODAL = 16;
const int OMNI_FTXUI_COMPONENT_RENDERER = 17;
const int OMNI_FTXUI_COMPONENT_HOVERABLE = 18;
const int OMNI_FTXUI_COMPONENT_RESIZABLE_SPLIT = 19;

struct OmniFtxuiColor {
    uint size;
    uint abi_version;
    int kind;
    int a;
    int b;
    int c;
}

struct OmniFtxuiDimension {
    uint size;
    uint abi_version;
    int kind;
    int value;
}

struct OmniFtxuiEventSpec {
    uint size;
    uint abi_version;
    int kind;
    char* text;
}

alias OmniFtxuiGraphCallbackFn = fn OmniFtxuiStatus(
    void* user_data,
    OmniFtxuiContext* context,
    int width,
    int height,
    int* out_values
);

struct OmniFtxuiElementCreateOptions {
    uint size;
    uint abi_version;
    int kind;
    char* text;
    float gauge_progress;
    int direction;
    int spinner_charset;
    usz spinner_frame;
    int canvas_width;
    int canvas_height;
    usz table_rows;
    usz table_cols;
    void* graph_user_data;
    OmniFtxuiGraphCallbackFn* graph_cb;
}

struct OmniFtxuiDecoratorOptions {
    uint size;
    uint abi_version;
    int kind;
    OmniFtxuiColor color;
    int width_or_height;
    int constraint;
    int size_value;
    char* link;
}

struct OmniFtxuiComponentCreateOptions {
    uint size;
    uint abi_version;
    int kind;
    char* label;
    char* title;
    char* text_value;
    char** items;
    usz item_count;
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
}

struct OmniFtxuiTableCreateOptions {
    uint size;
    uint abi_version;
    usz rows;
    usz cols;
    char** cells;
}

struct OmniFtxuiCanvasCreateOptions {
    uint size;
    uint abi_version;
    int width;
    int height;
}

struct OmniFtxuiScreenCreateOptions {
    uint size;
    uint abi_version;
    int mode;
    bool track_mouse;
    // The backend currently reports NOT_SUPPORTED when this is enabled.
    bool handle_piped_input;
    OmniFtxuiDimension width;
    OmniFtxuiDimension height;
}

alias OmniFtxuiRenderCallbackFn = fn OmniFtxuiStatus(void* user_data, OmniFtxuiContext* context, OmniFtxuiElement** out_element);
alias OmniFtxuiEventCallbackFn = fn int(void* user_data, OmniFtxuiContext* context, OmniFtxuiEventHandle* event);
alias OmniFtxuiActionCallbackFn = fn void(void* user_data);

struct OmniFtxuiCallbackOptions {
    uint size;
    uint abi_version;
    void* user_data;
    OmniFtxuiRenderCallbackFn* render_cb;
    OmniFtxuiEventCallbackFn* event_cb;
    OmniFtxuiActionCallbackFn* action_cb;
}

extern fn int omni_ftxui_backend_available() @extern("omni_ftxui_backend_available");
extern fn char* omni_ftxui_status_name(OmniFtxuiStatus status) @extern("omni_ftxui_status_name");

extern fn OmniFtxuiStatus omni_ftxui_context_create(OmniFtxuiContext** out_context) @extern("omni_ftxui_context_create");
extern fn void omni_ftxui_context_destroy(OmniFtxuiContext* context) @extern("omni_ftxui_context_destroy");
extern fn OmniFtxuiStatus omni_ftxui_context_last_error_code(OmniFtxuiContext* context) @extern("omni_ftxui_context_last_error_code");
extern fn char* omni_ftxui_context_last_error_message(OmniFtxuiContext* context) @extern("omni_ftxui_context_last_error_message");
extern fn void omni_ftxui_context_clear_error(OmniFtxuiContext* context) @extern("omni_ftxui_context_clear_error");

extern fn OmniFtxuiStatus omni_ftxui_event_create(OmniFtxuiContext* context, OmniFtxuiEventSpec* spec, OmniFtxuiEventHandle** out_event) @extern("omni_ftxui_event_create");
extern fn void omni_ftxui_event_destroy(OmniFtxuiEventHandle* event) @extern("omni_ftxui_event_destroy");

extern fn OmniFtxuiStatus omni_ftxui_screen_create(OmniFtxuiContext* context, OmniFtxuiScreenCreateOptions* options, OmniFtxuiScreen** out_screen) @extern("omni_ftxui_screen_create");
extern fn void omni_ftxui_screen_destroy(OmniFtxuiScreen* screen) @extern("omni_ftxui_screen_destroy");
extern fn OmniFtxuiStatus omni_ftxui_screen_loop(OmniFtxuiContext* context, OmniFtxuiScreen* screen, OmniFtxuiComponent* root) @extern("omni_ftxui_screen_loop");
extern fn OmniFtxuiStatus omni_ftxui_screen_post_event(OmniFtxuiContext* context, OmniFtxuiScreen* screen, OmniFtxuiEventHandle* event) @extern("omni_ftxui_screen_post_event");
extern fn OmniFtxuiStatus omni_ftxui_screen_exit(OmniFtxuiContext* context, OmniFtxuiScreen* screen) @extern("omni_ftxui_screen_exit");
extern fn OmniFtxuiStatus omni_ftxui_screen_request_animation_frame(OmniFtxuiContext* context, OmniFtxuiScreen* screen) @extern("omni_ftxui_screen_request_animation_frame");
extern fn OmniFtxuiStatus omni_ftxui_screen_set_track_mouse(OmniFtxuiContext* context, OmniFtxuiScreen* screen, bool enabled) @extern("omni_ftxui_screen_set_track_mouse");
extern fn OmniFtxuiStatus omni_ftxui_screen_set_handle_piped_input(OmniFtxuiContext* context, OmniFtxuiScreen* screen, bool enabled) @extern("omni_ftxui_screen_set_handle_piped_input");

extern fn OmniFtxuiStatus omni_ftxui_app_create(OmniFtxuiContext* context, OmniFtxuiScreen* screen, OmniFtxuiComponent* root, OmniFtxuiApp** out_app) @extern("omni_ftxui_app_create");
extern fn void omni_ftxui_app_destroy(OmniFtxuiApp* app) @extern("omni_ftxui_app_destroy");
extern fn OmniFtxuiStatus omni_ftxui_app_run(OmniFtxuiContext* context, OmniFtxuiApp* app) @extern("omni_ftxui_app_run");
extern fn OmniFtxuiStatus omni_ftxui_app_exit(OmniFtxuiContext* context, OmniFtxuiApp* app) @extern("omni_ftxui_app_exit");
extern fn OmniFtxuiStatus omni_ftxui_app_post_event(OmniFtxuiContext* context, OmniFtxuiApp* app, OmniFtxuiEventHandle* event) @extern("omni_ftxui_app_post_event");

extern fn OmniFtxuiStatus omni_ftxui_element_create(
    OmniFtxuiContext* context,
    OmniFtxuiElementCreateOptions* options,
    OmniFtxuiElement** children,
    usz child_count,
    OmniFtxuiElement** out_element
) @extern("omni_ftxui_element_create");
extern fn OmniFtxuiStatus omni_ftxui_element_graph_from_series(
    OmniFtxuiContext* context,
    double* values,
    usz value_count,
    OmniFtxuiElement** out_element
) @extern("omni_ftxui_element_graph_from_series");
extern fn void omni_ftxui_element_destroy(OmniFtxuiElement* element) @extern("omni_ftxui_element_destroy");
extern fn OmniFtxuiStatus omni_ftxui_element_apply_decorator(
    OmniFtxuiContext* context,
    OmniFtxuiElement* element,
    OmniFtxuiDecoratorOptions* options
) @extern("omni_ftxui_element_apply_decorator");

extern fn OmniFtxuiStatus omni_ftxui_component_create(
    OmniFtxuiContext* context,
    OmniFtxuiComponentCreateOptions* options,
    OmniFtxuiComponent** children,
    usz child_count,
    OmniFtxuiComponent** out_component
) @extern("omni_ftxui_component_create");
extern fn void omni_ftxui_component_destroy(OmniFtxuiComponent* component) @extern("omni_ftxui_component_destroy");
extern fn OmniFtxuiStatus omni_ftxui_component_add_child(OmniFtxuiContext* context, OmniFtxuiComponent* parent, OmniFtxuiComponent* child) @extern("omni_ftxui_component_add_child");
extern fn OmniFtxuiStatus omni_ftxui_component_take_focus(OmniFtxuiContext* context, OmniFtxuiComponent* component) @extern("omni_ftxui_component_take_focus");
extern fn OmniFtxuiStatus omni_ftxui_component_wrap_renderer(
    OmniFtxuiContext* context,
    OmniFtxuiComponent* base,
    OmniFtxuiCallbackOptions* callbacks,
    OmniFtxuiComponent** out_component
) @extern("omni_ftxui_component_wrap_renderer");
extern fn OmniFtxuiStatus omni_ftxui_component_wrap_event_handler(
    OmniFtxuiContext* context,
    OmniFtxuiComponent* base,
    OmniFtxuiCallbackOptions* callbacks,
    OmniFtxuiComponent** out_component
) @extern("omni_ftxui_component_wrap_event_handler");
extern fn OmniFtxuiStatus omni_ftxui_component_from_element(
    OmniFtxuiContext* context,
    OmniFtxuiElement* element,
    OmniFtxuiComponent** out_component
) @extern("omni_ftxui_component_from_element");
extern fn OmniFtxuiStatus omni_ftxui_component_wrap_quit_keys(
    OmniFtxuiContext* context,
    OmniFtxuiScreen* screen,
    OmniFtxuiComponent* base,
    OmniFtxuiComponent** out_component
) @extern("omni_ftxui_component_wrap_quit_keys");
extern fn OmniFtxuiStatus omni_ftxui_component_wrap_maybe(
    OmniFtxuiContext* context,
    OmniFtxuiComponent* base,
    bool* visible_ptr,
    OmniFtxuiComponent** out_component
) @extern("omni_ftxui_component_wrap_maybe");

extern fn OmniFtxuiStatus omni_ftxui_table_create(OmniFtxuiContext* context, OmniFtxuiTableCreateOptions* options, OmniFtxuiTable** out_table) @extern("omni_ftxui_table_create");
extern fn void omni_ftxui_table_destroy(OmniFtxuiTable* table) @extern("omni_ftxui_table_destroy");
extern fn OmniFtxuiStatus omni_ftxui_table_select_all(OmniFtxuiContext* context, OmniFtxuiTable* table, int border) @extern("omni_ftxui_table_select_all");
extern fn OmniFtxuiStatus omni_ftxui_table_select_row(OmniFtxuiContext* context, OmniFtxuiTable* table, usz row, int border) @extern("omni_ftxui_table_select_row");
extern fn OmniFtxuiStatus omni_ftxui_table_select_column(OmniFtxuiContext* context, OmniFtxuiTable* table, usz column, int border) @extern("omni_ftxui_table_select_column");
extern fn OmniFtxuiStatus omni_ftxui_table_select_cell(OmniFtxuiContext* context, OmniFtxuiTable* table, usz row, usz column, int border) @extern("omni_ftxui_table_select_cell");
extern fn OmniFtxuiStatus omni_ftxui_table_render(OmniFtxuiContext* context, OmniFtxuiTable* table, OmniFtxuiElement** out_element) @extern("omni_ftxui_table_render");

extern fn OmniFtxuiStatus omni_ftxui_canvas_create(OmniFtxuiContext* context, OmniFtxuiCanvasCreateOptions* options, OmniFtxuiCanvas** out_canvas) @extern("omni_ftxui_canvas_create");
extern fn void omni_ftxui_canvas_destroy(OmniFtxuiCanvas* canvas) @extern("omni_ftxui_canvas_destroy");
extern fn OmniFtxuiStatus omni_ftxui_canvas_draw_text(OmniFtxuiContext* context, OmniFtxuiCanvas* canvas, int x, int y, char* text) @extern("omni_ftxui_canvas_draw_text");
extern fn OmniFtxuiStatus omni_ftxui_canvas_draw_point_on(OmniFtxuiContext* context, OmniFtxuiCanvas* canvas, int x, int y) @extern("omni_ftxui_canvas_draw_point_on");
extern fn OmniFtxuiStatus omni_ftxui_canvas_draw_point_off(OmniFtxuiContext* context, OmniFtxuiCanvas* canvas, int x, int y) @extern("omni_ftxui_canvas_draw_point_off");
extern fn OmniFtxuiStatus omni_ftxui_canvas_draw_line(OmniFtxuiContext* context, OmniFtxuiCanvas* canvas, int x1, int y1, int x2, int y2) @extern("omni_ftxui_canvas_draw_line");
extern fn OmniFtxuiStatus omni_ftxui_canvas_render(OmniFtxuiContext* context, OmniFtxuiCanvas* canvas, OmniFtxuiElement** out_element) @extern("omni_ftxui_canvas_render");
