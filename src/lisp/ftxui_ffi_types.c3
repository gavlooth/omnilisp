module lisp;

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

struct OmniFtxuiEventReadResult {
    uint size;
    uint abi_version;
    uint initialized;
    bool has_event;
    int kind;
    char* text;
    usz text_len;
    usz dropped_events;
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
    OmniFtxuiGraphCallbackFn graph_cb;
}

struct OmniFtxuiPlotOptions {
    uint size;
    uint abi_version;
    int kind;
    int width;
    int height;
    double* values;
    usz value_count;
    usz* series_lengths;
    usz series_count;
    double* x_values;
    usz x_count;
    usz heatmap_rows;
    usz heatmap_cols;
    int marker;
    int has_y_min;
    int has_y_max;
    double y_min;
    double y_max;
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
    OmniFtxuiRenderCallbackFn render_cb;
    OmniFtxuiEventCallbackFn event_cb;
    OmniFtxuiActionCallbackFn action_cb;
}
