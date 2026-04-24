module lisp;

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

extern fn OmniFtxuiStatus omni_ftxui_session_create(OmniFtxuiContext* context, OmniFtxuiScreen* screen, OmniFtxuiComponent* root, OmniFtxuiSession** out_session) @extern("omni_ftxui_session_create");
extern fn void omni_ftxui_session_destroy(OmniFtxuiSession* session) @extern("omni_ftxui_session_destroy");
extern fn OmniFtxuiStatus omni_ftxui_session_run_once(OmniFtxuiContext* context, OmniFtxuiSession* session) @extern("omni_ftxui_session_run_once");
extern fn OmniFtxuiStatus omni_ftxui_session_run_once_blocking(OmniFtxuiContext* context, OmniFtxuiSession* session) @extern("omni_ftxui_session_run_once_blocking");
extern fn OmniFtxuiStatus omni_ftxui_session_has_quitted(OmniFtxuiContext* context, OmniFtxuiSession* session, bool* out_quitted) @extern("omni_ftxui_session_has_quitted");
extern fn OmniFtxuiStatus omni_ftxui_session_take_event(OmniFtxuiContext* context, OmniFtxuiSession* session, OmniFtxuiEventReadResult* out_event) @extern("omni_ftxui_session_take_event");
extern fn void omni_ftxui_event_read_result_clear(OmniFtxuiEventReadResult* event) @extern("omni_ftxui_event_read_result_clear");

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
extern fn OmniFtxuiStatus omni_ftxui_element_plot(
    OmniFtxuiContext* context,
    OmniFtxuiPlotOptions* options,
    OmniFtxuiElement** out_element
) @extern("omni_ftxui_element_plot");
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
extern fn OmniFtxuiStatus omni_ftxui_component_handle_event(OmniFtxuiContext* context, OmniFtxuiComponent* component, OmniFtxuiEventHandle* event, bool* out_handled) @extern("omni_ftxui_component_handle_event");
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
extern fn OmniFtxuiStatus omni_ftxui_component_wrap_action(
    OmniFtxuiContext* context,
    OmniFtxuiComponent* base,
    OmniFtxuiCallbackOptions* callbacks,
    OmniFtxuiComponent** out_component
) @extern("omni_ftxui_component_wrap_action");
extern fn OmniFtxuiStatus omni_ftxui_component_wrap_message_action(
    OmniFtxuiContext* context,
    OmniFtxuiScreen* screen,
    OmniFtxuiComponent* base,
    char* message,
    OmniFtxuiComponent** out_component
) @extern("omni_ftxui_component_wrap_message_action");
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
