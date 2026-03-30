local M = {}

local state = {
  job_id = nil,
  channel_kind = nil,
  protocol = "legacy_json",
  session_id = nil,
  bufnr = nil,
  winid = nil,
  eval_annot_ns = vim.api.nvim_create_namespace("omni.nvim.eval_annotations"),
  stdout_partial = "",
  stderr_partial = "",
  request_seq = 0,
  pending = {},
  deferred_requests = {},
  auto_scroll = true,
}

local notify = function(message, level)
  vim.notify(message, level or vim.log.levels.INFO, { title = "omni.nvim" })
end

local matching = {
  ["("] = ")",
  ["["] = "]",
  ["{"] = "}",
}

local reverse_matching = {
  [")"] = "(",
  ["]"] = "[",
  ["}"] = "{",
}

local ts_form_types = {
  accessor = true,
  array = true,
  dict = true,
  float = true,
  form_comment = true,
  index_expression = true,
  integer = true,
  list = true,
  path = true,
  placeholder = true,
  quasiquote = true,
  quote = true,
  regex_literal = true,
  string = true,
  symbol = true,
  unquote = true,
  unquote_splicing = true,
}

local ts_compound_form_types = {
  accessor = true,
  array = true,
  dict = true,
  form_comment = true,
  index_expression = true,
  list = true,
  path = true,
  quasiquote = true,
  quote = true,
  unquote = true,
  unquote_splicing = true,
}

local ts_eval_capture_groups = {
  call = { "call.outer" },
  block = { "block.outer" },
  declaration = { "function.outer", "assignment.outer", "class.outer" },
}

local function trim(text)
  return (text:gsub("^%s+", ""):gsub("%s+$", ""))
end

local function ends_with_newline(text)
  return text:sub(-1) == "\n"
end

local function strip_ansi(text)
  return text:gsub("\27%[[0-9;?]*[ -/]*[@-~]", "")
end

local function truncate_text(text, max_length)
  local ok, normalized = pcall(tostring, text)
  if not ok then
    normalized = ""
  end
  normalized = normalized:gsub("[\r\n]+", " ")
  normalized = normalized:gsub("^%s+", ""):gsub("%s+$", "")
  if max_length and max_length > 0 and #normalized > max_length then
    local limit = max_length - 3
    if limit <= 0 then
      return "..."
    end
    return normalized:sub(1, limit) .. "..."
  end
  return normalized
end

local function stringify_eval_value(value)
  local ok, inspected = pcall(vim.inspect, value)
  if ok then
    return inspected
  end
  return tostring(value)
end

local function pretty_values_enabled(config)
  return not (config and config.output and config.output.pretty_values == false)
end

local function pretty_width(config)
  local width = config and config.output and config.output.pretty_width or 72
  if type(width) ~= "number" or width < 20 then
    return 72
  end
  return math.floor(width)
end

local function pretty_indent(config)
  local indent = config and config.output and config.output.pretty_indent or 2
  if type(indent) ~= "number" or indent < 1 then
    return 2
  end
  return math.floor(indent)
end

local function skip_pretty_ws(text, index)
  while index <= #text do
    local ch = text:sub(index, index)
    if not ch:match("%s") then
      break
    end
    index = index + 1
  end
  return index
end

local function parse_pretty_string(text, index)
  local start = index
  index = index + 1
  while index <= #text do
    local ch = text:sub(index, index)
    if ch == "\\" then
      index = index + 2
    elseif ch == "\"" then
      return { kind = "atom", text = text:sub(start, index) }, index + 1
    else
      index = index + 1
    end
  end
  return nil, nil
end

local function parse_pretty_atom(text, index)
  local start = index
  while index <= #text do
    local ch = text:sub(index, index)
    if ch:match("%s") or matching[ch] or reverse_matching[ch] then
      break
    end
    index = index + 1
  end
  if index == start then
    return nil, nil
  end
  return { kind = "atom", text = text:sub(start, index - 1) }, index
end

local function parse_pretty_node(text, index)
  index = skip_pretty_ws(text, index)
  if index > #text then
    return nil, index
  end

  local ch = text:sub(index, index)
  local close = matching[ch]
  if close then
    local children = {}
    index = index + 1
    while index <= #text do
      index = skip_pretty_ws(text, index)
      if index > #text then
        return nil, nil
      end
      if text:sub(index, index) == close then
        return {
          kind = "container",
          open = ch,
          close = close,
          children = children,
        }, index + 1
      end
      local child
      child, index = parse_pretty_node(text, index)
      if not child then
        return nil, nil
      end
      table.insert(children, child)
    end
    return nil, nil
  end

  if ch == "\"" then
    return parse_pretty_string(text, index)
  end

  return parse_pretty_atom(text, index)
end

local function parse_pretty_root(text)
  local node, index = parse_pretty_node(text, 1)
  if not node then
    return nil
  end
  index = skip_pretty_ws(text, index)
  if index <= #text then
    return nil
  end
  return node
end

local function render_pretty_inline(node)
  if not node then
    return nil
  end
  if node.kind == "atom" then
    return node.text
  end

  local parts = {}
  for _, child in ipairs(node.children or {}) do
    local rendered = render_pretty_inline(child)
    if not rendered then
      return nil
    end
    table.insert(parts, rendered)
  end
  return node.open .. table.concat(parts, " ") .. node.close
end

local function extend_block_lines(lines, block)
  for _, line in ipairs(vim.split(block, "\n", { plain = true })) do
    table.insert(lines, line)
  end
end

local function render_pretty_node(node, config, depth)
  depth = depth or 0
  if not node then
    return ""
  end
  if node.kind == "atom" then
    return node.text
  end

  local indent_size = pretty_indent(config)
  local width = pretty_width(config)
  local indent = string.rep(" ", depth * indent_size)
  local child_indent = string.rep(" ", (depth + 1) * indent_size)
  local inline = render_pretty_inline(node)
  if inline and (#indent + #inline) <= width then
    return inline
  end

  if #node.children == 0 then
    return node.open .. node.close
  end

  if node.open == "{" then
    local lines = { indent .. node.open }
    local i = 1
    while i <= #node.children do
      local key = node.children[i]
      local value = node.children[i + 1]
      if not value then
        local rendered = render_pretty_node(key, config, depth + 1)
        if rendered:find("\n", 1, true) then
          extend_block_lines(lines, rendered)
        else
          table.insert(lines, child_indent .. rendered)
        end
        i = i + 1
      else
        local key_inline = render_pretty_inline(key) or render_pretty_node(key, config, depth + 1)
        local value_inline = render_pretty_inline(value)
        if key_inline and value_inline and (#child_indent + #key_inline + 1 + #value_inline) <= width then
          table.insert(lines, child_indent .. key_inline .. " " .. value_inline)
        else
          table.insert(lines, child_indent .. key_inline)
          local rendered_value = render_pretty_node(value, config, depth + 2)
          if rendered_value:find("\n", 1, true) then
            extend_block_lines(lines, rendered_value)
          else
            table.insert(lines, string.rep(" ", (depth + 2) * indent_size) .. rendered_value)
          end
        end
        i = i + 2
      end
    end
    table.insert(lines, indent .. node.close)
    return table.concat(lines, "\n")
  end

  local lines = { indent .. node.open }
  for _, child in ipairs(node.children) do
    local rendered = render_pretty_node(child, config, depth + 1)
    if rendered:find("\n", 1, true) then
      extend_block_lines(lines, rendered)
    else
      table.insert(lines, child_indent .. rendered)
    end
  end
  table.insert(lines, indent .. node.close)
  return table.concat(lines, "\n")
end

local function format_eval_value(config, value)
  local display = value
  if display == nil then
    return "nil"
  end
  if type(display) ~= "string" then
    display = stringify_eval_value(display)
  end
  if not pretty_values_enabled(config) then
    return display
  end
  local parsed = parse_pretty_root(display)
  if not parsed then
    return display
  end
  return render_pretty_node(parsed, config, 0)
end

local function annotation_enabled(config, label, selection)
  if not config or type(config.eval) ~= "table" then
    return false, nil, nil
  end
  local annotations = config.eval.annotations
  if type(annotations) ~= "table" or annotations.enabled ~= true then
    return false, nil, nil
  end
  if not selection then
    return false, nil, nil
  end
  if annotations.labels and annotations.labels[label] ~= true then
    return false, nil, nil
  end
  return true,
    (annotations.hl and annotations.hl.ok) or "DiagnosticOk",
    (annotations.hl and annotations.hl.error) or "DiagnosticError"
end

local function format_annotation_message(result, max_length)
  if type(result) ~= "table" then
    return "!! invalid eval result"
  end
  if result.ok then
    local display = result.value
    if display == nil then
      display = "nil"
    elseif type(display) ~= "string" then
      display = stringify_eval_value(display)
    end
    return truncate_text(string.format("=> %s", display), max_length)
  end
  local error_info = result.error or {}
  local message = error_info.message or "unknown evaluation error"
  return truncate_text(string.format("!! %s", message), max_length)
end

local function place_eval_annotation(config, ctx, label, result)
  if not ctx then
    return
  end
  local enabled, hl_ok, hl_error = annotation_enabled(config, label, ctx.selection)
  if not enabled then
    return
  end
  local bufnr = ctx.bufnr
  if not bufnr or not vim.api.nvim_buf_is_valid(bufnr) then
    return
  end
  local selection = ctx.selection
  local row = selection.end_row
  local col = selection.end_col or 0
  if type(row) ~= "number" or type(col) ~= "number" then
    return
  end
  local line_count = vim.api.nvim_buf_line_count(bufnr)
  if row < 0 or row >= line_count then
    return
  end
  local line = vim.api.nvim_buf_get_lines(bufnr, row, row + 1, false)[1] or ""
  if col < 0 or col > #line then
    col = #line
  end
  vim.api.nvim_buf_clear_namespace(bufnr, state.eval_annot_ns, row, row + 1)
  local annotations = config.eval.annotations
  local max_length = annotations and annotations.max_length or 0
  local text = format_annotation_message(result, max_length)
  local hl = result.ok and hl_ok or hl_error
  if text == "" then
    return
  end
  vim.api.nvim_buf_set_extmark(bufnr, state.eval_annot_ns, row, col, {
    virt_text = { { text, hl } },
    virt_text_pos = "eol",
    hl_mode = "combine",
  })
end

local function append_lines(lines)
  if not state.bufnr or not vim.api.nvim_buf_is_valid(state.bufnr) then
    return
  end

  local normalized = {}
  for _, line in ipairs(lines) do
    local text = tostring(line or "")
    local parts = vim.split(text, "\n", { plain = true })
    vim.list_extend(normalized, parts)
  end

  vim.api.nvim_buf_set_lines(state.bufnr, -1, -1, false, normalized)
  if state.auto_scroll and state.winid and vim.api.nvim_win_is_valid(state.winid)
    and vim.api.nvim_win_get_buf(state.winid) == state.bufnr then
    local last = vim.api.nvim_buf_line_count(state.bufnr)
    vim.api.nvim_win_set_cursor(state.winid, { last, 0 })
  end
end

local function ensure_output_buffer(config)
  if state.bufnr and vim.api.nvim_buf_is_valid(state.bufnr) then
    return state.bufnr
  end

  local bufnr = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_name(bufnr, config.output.name)
  vim.bo[bufnr].buftype = "nofile"
  vim.bo[bufnr].bufhidden = "hide"
  vim.bo[bufnr].swapfile = false
  vim.bo[bufnr].filetype = "omni-repl"
  state.bufnr = bufnr
  return bufnr
end

function M.open_output(config)
  local bufnr = ensure_output_buffer(config)
  state.auto_scroll = config.output.auto_scroll ~= false

  if state.winid and vim.api.nvim_win_is_valid(state.winid) then
    if vim.api.nvim_win_get_buf(state.winid) ~= bufnr then
      vim.api.nvim_win_set_buf(state.winid, bufnr)
    end
    vim.api.nvim_set_current_win(state.winid)
    return bufnr
  end

  vim.cmd(config.output.split)
  state.winid = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(state.winid, bufnr)
  vim.bo[bufnr].modifiable = true
  return bufnr
end

function M.clear_output()
  if not state.bufnr or not vim.api.nvim_buf_is_valid(state.bufnr) then
    return
  end
  vim.api.nvim_buf_set_lines(state.bufnr, 0, -1, false, { "" })
  if state.winid and vim.api.nvim_win_is_valid(state.winid)
    and vim.api.nvim_win_get_buf(state.winid) == state.bufnr then
    vim.api.nvim_win_set_cursor(state.winid, { 1, 0 })
  end
end

local function flush_partial(stream_name, prefix)
  local key = stream_name .. "_partial"
  local partial = state[key]
  if partial == "" then
    return
  end
  state[key] = ""
  if prefix and partial ~= "" then
    partial = prefix .. partial
  end
  append_lines({ strip_ansi(partial) })
end

local function handle_stream(stream_name, prefix, data)
  if not data or vim.tbl_isempty(data) then
    return
  end

  local key = stream_name .. "_partial"
  local partial = state[key]
  local lines = {}

  for index, chunk in ipairs(data) do
    local text = chunk or ""
    if index == 1 then
      text = partial .. text
    end

    local is_last = index == #data
    if is_last and chunk ~= "" then
      state[key] = text
    else
      state[key] = ""
      if prefix and text ~= "" then
        text = prefix .. text
      end
      table.insert(lines, strip_ansi(text))
    end
  end

  if #lines > 0 then
    append_lines(lines)
  end
end

local function shell_command(config)
  if type(config.cmd) == "string" then
    return vim.split(config.cmd, " ", { trimempty = true })
  end
  return config.cmd
end

local function eval_command(config)
  if type(config.eval.cmd) == "string" then
    return vim.split(config.eval.cmd, " ", { trimempty = true })
  end
  return config.eval.cmd
end

local function repl_mode(config)
  if config.repl and config.repl.mode then
    return config.repl.mode
  end
  return "text"
end

local function discovery_config(config)
  local repl_config = config and config.repl or {}
  local discovery = repl_config.discovery
  if type(discovery) ~= "table" then
    discovery = {}
  end
  return {
    enabled = discovery.enabled ~= false,
    host = type(discovery.host) == "string" and discovery.host ~= "" and discovery.host or "127.0.0.1",
    port_file = type(discovery.port_file) == "string" and discovery.port_file ~= "" and discovery.port_file or ".omni-repl-port",
  }
end

local function root_markers(config)
  local lsp_config = config and config.lsp or {}
  if type(lsp_config.root_markers) == "table" and not vim.tbl_isempty(lsp_config.root_markers) then
    return lsp_config.root_markers
  end
  return { "omni.toml", "project.json", ".git" }
end

local function current_buffer_dir()
  local path = vim.api.nvim_buf_get_name(0)
  if type(path) == "string" and path ~= "" then
    local normalized = vim.fs.normalize(path)
    if vim.fn.isdirectory(normalized) == 1 then
      return normalized
    end
    return vim.fs.dirname(normalized)
  end
  return vim.fn.getcwd()
end

local function project_root(config)
  local start = current_buffer_dir()
  local match = vim.fs.find(root_markers(config), {
    path = start,
    upward = true,
    stop = vim.loop.os_homedir(),
  })[1]
  if match then
    return vim.fs.dirname(match)
  end
  return start
end

local function discover_tcp_endpoint(config)
  local discovery = discovery_config(config)
  if not discovery.enabled then
    return nil
  end

  local root = project_root(config)
  local candidates = {}
  if root and root ~= "" then
    table.insert(candidates, vim.fs.normalize(root .. "/" .. discovery.port_file))
  end

  local start = current_buffer_dir()
  local found = vim.fs.find(discovery.port_file, {
    path = start,
    upward = true,
    stop = vim.loop.os_homedir(),
  })[1]
  if found then
    found = vim.fs.normalize(found)
    local seen = false
    for _, candidate in ipairs(candidates) do
      if candidate == found then
        seen = true
        break
      end
    end
    if not seen then
      table.insert(candidates, found)
    end
  end

  for _, path in ipairs(candidates) do
    if vim.fn.filereadable(path) == 1 then
      local lines = vim.fn.readfile(path, "", 1)
      local raw_port = trim(lines[1] or "")
      local port = tonumber(raw_port)
      if port and port > 0 and port <= 65535 then
        return {
          host = discovery.host,
          port = math.floor(port),
          path = path,
        }
      end
    end
  end

  return nil
end

local append_eval_result

function M.set_notifier(fn)
  notify = fn
end

local function reset_request_state()
  state.stdout_partial = ""
  state.stderr_partial = ""
  state.request_seq = 0
  state.pending = {}
  state.deferred_requests = {}
  state.session_id = nil
end

local function next_request_id()
  state.request_seq = state.request_seq + 1
  return tostring(state.request_seq)
end

local function append_prefixed_chunks(prefix, text)
  if type(text) ~= "string" or text == "" then
    return
  end
  local lines = {}
  for _, line in ipairs(vim.split(text, "\n", { plain = true })) do
    table.insert(lines, prefix .. line)
  end
  append_lines(lines)
end

local function flush_server_deferred_requests(config)
  if state.protocol ~= "server_json" or not state.session_id then
    return
  end
  local queued = state.deferred_requests
  state.deferred_requests = {}
  for _, item in ipairs(queued) do
    local request_id = next_request_id()
    state.pending[request_id] = {
      kind = "server_eval",
      label = item.label,
      payload = item.payload,
      annotation_ctx = item.annotation_ctx,
      output = {},
    }
    local request = vim.json.encode({
      id = request_id,
      op = "eval",
      session = state.session_id,
      code = item.payload,
      mode = item.mode,
    })
    local sent = vim.fn.chansend(state.job_id, request .. "\n")
    if sent == 0 then
      state.pending[request_id] = nil
      append_lines({
        string.format(">> %s", item.label),
        item.payload,
        "!! failed to send request to omni repl server",
      })
    end
  end
end

local function handle_server_message(decoded)
  local event = decoded.event
  local request_id = decoded.id
  local pending = request_id and state.pending[request_id] or nil

  if event == "session" and pending and pending.kind == "clone" then
    pending.session_id = decoded.session
    return
  end

  if event == "out" or event == "err" then
    if pending and pending.kind == "server_eval" then
      local prefix = event == "err" and "stderr| " or "stdout| "
      local text = tostring(decoded.text or "")
      if text ~= "" then
        for _, line in ipairs(vim.split(text, "\n", { plain = true })) do
          table.insert(pending.output, prefix .. strip_ansi(line))
        end
      end
      return
    end
    append_prefixed_chunks(event == "err" and "stderr| " or "stdout| ", strip_ansi(tostring(decoded.text or "")))
    return
  end

  if event == "value" and pending and pending.kind == "server_eval" then
    pending.result = { ok = true, value = decoded.value }
    return
  end

  if event == "interrupted" and pending and pending.kind == "server_eval" then
    state.pending[request_id] = nil
    local result = {
      ok = false,
      error = {
        message = "evaluation interrupted",
      },
    }
    if #pending.output > 0 then
      append_lines(pending.output)
    end
    append_eval_result((pending.annotation_ctx and pending.annotation_ctx.config) or nil,
      pending.label,
      pending.payload,
      result,
      pending.annotation_ctx)
    return
  end

  if event == "error" and pending and pending.kind == "server_eval" then
    state.pending[request_id] = nil
    local error_info = decoded.error or {}
    local result = {
      ok = false,
      error = {
        message = error_info.message or "unknown evaluation error",
        range = error_info.range,
      },
    }
    if #pending.output > 0 then
      append_lines(pending.output)
    end
    append_eval_result((pending.annotation_ctx and pending.annotation_ctx.config) or nil,
      pending.label,
      pending.payload,
      result,
      pending.annotation_ctx)
    return
  end

  if event == "done" and pending and pending.kind == "clone" then
    state.pending[request_id] = nil
    state.session_id = pending.session_id
    if state.session_id then
      append_lines({ string.format("[omni repl] connected to tcp server session %s", state.session_id) })
      flush_server_deferred_requests(pending.config)
    else
      append_lines({ "!! omni repl server did not return a session id" })
    end
    return
  end

  if event == "done" and pending and pending.kind == "server_eval" then
    state.pending[request_id] = nil
    local result = pending.result or { ok = true, value = "#<void>" }
    if #pending.output > 0 then
      append_lines(pending.output)
    end
    append_eval_result((pending.annotation_ctx and pending.annotation_ctx.config) or nil,
      pending.label,
      pending.payload,
      result,
      pending.annotation_ctx)
    return
  end

  if event == "error" then
    local error_info = decoded.error or {}
    local message = error_info.message or "unknown repl server error"
    append_lines({ string.format("!! %s", message) })
    if request_id and pending then
      state.pending[request_id] = nil
    end
    return
  end
end

local function handle_legacy_json_message(decoded, line)
  local request_id = decoded.id
  local pending = request_id and state.pending[request_id] or nil
  if pending then
    state.pending[request_id] = nil
    append_eval_result((pending.annotation_ctx and pending.annotation_ctx.config) or nil,
      pending.label,
      pending.payload,
      decoded,
      pending.annotation_ctx)
    return
  end

  local error_info = decoded.error or {}
  local message = error_info.message or "unknown repl error"
  append_lines({ string.format("!! %s", message) })
end

local function handle_json_message(line)
  local ok, decoded = pcall(vim.json.decode, line)
  if not ok or type(decoded) ~= "table" then
    append_lines({ strip_ansi(line) })
    return
  end

  if state.protocol == "server_json" then
    handle_server_message(decoded)
    return
  end

  flush_partial("stderr", "stderr| ")
  handle_legacy_json_message(decoded, line)
end

local function handle_json_stream(data)
  if not data or vim.tbl_isempty(data) then
    return
  end

  local partial = state.stdout_partial
  for index, chunk in ipairs(data) do
    local text = chunk or ""
    if index == 1 then
      text = partial .. text
    end

    local is_last = index == #data
    if is_last and chunk ~= "" then
      partial = text
    else
      partial = ""
      text = strip_ansi(text)
      if text ~= "" then
        handle_json_message(text)
      end
    end
  end

  state.stdout_partial = partial
end

function M.start(config, opts)
  opts = opts or {}
  local restore_win = opts.focus == false and vim.api.nvim_get_current_win() or nil

  if state.job_id and (state.channel_kind ~= "job" or vim.fn.jobwait({ state.job_id }, 0)[1] == -1) then
    M.open_output(config)
    if restore_win and vim.api.nvim_win_is_valid(restore_win) then
      vim.api.nvim_set_current_win(restore_win)
    end
    return state.job_id
  end

  M.open_output(config)
  if restore_win and vim.api.nvim_win_is_valid(restore_win) then
    vim.api.nvim_set_current_win(restore_win)
  end
  append_lines({
    repl_mode(config) == "json"
      and "[omni repl] starting structured session..."
      or "[omni repl] starting...",
  })

  reset_request_state()

  local endpoint = repl_mode(config) == "json" and discover_tcp_endpoint(config) or nil
  if endpoint then
    local channel_id = vim.fn.sockconnect("tcp", string.format("%s:%d", endpoint.host, endpoint.port), {
      rpc = false,
      on_data = function(_, data)
        vim.schedule(function()
          if data and #data == 1 and data[1] == "" then
            if state.stdout_partial ~= "" then
              handle_json_message(state.stdout_partial)
              state.stdout_partial = ""
            end
            append_lines({ string.format("[omni repl] tcp server disconnected (%s:%d)", endpoint.host, endpoint.port) })
            state.job_id = nil
            state.channel_kind = nil
            state.protocol = "legacy_json"
            reset_request_state()
            return
          end
          handle_json_stream(data)
        end)
      end,
    })
    if channel_id > 0 then
      state.job_id = channel_id
      state.channel_kind = "socket"
      state.protocol = "server_json"
      append_lines({ string.format("[omni repl] connecting to tcp server via %s", endpoint.path) })
      local request_id = next_request_id()
      state.pending[request_id] = {
        kind = "clone",
        config = config,
      }
      local request = vim.json.encode({
        id = request_id,
        op = "clone",
      })
      local sent = vim.fn.chansend(state.job_id, request .. "\n")
      if sent == 0 then
        state.pending[request_id] = nil
        vim.fn.chanclose(state.job_id)
        state.job_id = nil
        state.channel_kind = nil
        state.protocol = "legacy_json"
        append_lines({ "[omni repl] failed to initialize tcp server session; falling back to local process" })
      else
        return channel_id
      end
    else
      append_lines({ string.format("[omni repl] failed to connect to discovered tcp server %s:%d; falling back to local process", endpoint.host, endpoint.port) })
    end
  end

  local json_mode = repl_mode(config) == "json"
  local job_id = vim.fn.jobstart(shell_command(config), {
    pty = not json_mode,
    on_stdout = function(_, data)
      vim.schedule(function()
        if json_mode then
          handle_json_stream(data)
        else
          handle_stream("stdout", nil, data)
        end
      end)
    end,
    on_stderr = function(_, data)
      vim.schedule(function()
        handle_stream("stderr", "stderr| ", data)
      end)
    end,
    on_exit = function(_, code)
      vim.schedule(function()
        if json_mode then
          if state.stdout_partial ~= "" then
            handle_json_message(state.stdout_partial)
            state.stdout_partial = ""
          end
        else
          flush_partial("stdout")
        end
        flush_partial("stderr", "stderr| ")
        if json_mode then
          for _, pending in pairs(state.pending) do
            if pending.label and pending.payload then
              append_lines({
                string.format(">> %s", pending.label),
                pending.payload,
                string.format("!! omni structured repl exited before replying (code %d)", code),
              })
            end
          end
        end
        append_lines({ string.format("[omni repl] exited with code %d", code) })
        state.job_id = nil
        state.channel_kind = nil
        state.protocol = "legacy_json"
        reset_request_state()
      end)
    end,
  })

  if job_id <= 0 then
    notify("failed to start omni REPL job", vim.log.levels.ERROR)
    return nil
  end

  state.job_id = job_id
  state.channel_kind = "job"
  state.protocol = "legacy_json"
  return job_id
end

function M.stop()
  if not state.job_id then
    return
  end
  if state.channel_kind == "socket" then
    vim.fn.chanclose(state.job_id)
  else
    vim.fn.jobstop(state.job_id)
  end
  state.job_id = nil
  state.channel_kind = nil
  state.protocol = "legacy_json"
  reset_request_state()
end

function M.restart(config)
  M.stop()
  return M.start(config)
end

append_eval_result = function(config, label, payload, result, annotation_ctx)
  local lines = {
    string.format(">> %s", label),
    payload,
  }

  if result.ok then
    local display = format_eval_value(config, result.value)
    local formatted_lines = vim.split(display, "\n", { plain = true })
    if #formatted_lines <= 1 then
      table.insert(lines, string.format("=> %s", display))
    else
      table.insert(lines, "=>")
      for _, line in ipairs(formatted_lines) do
        table.insert(lines, line)
      end
    end
  else
    local error_info = result.error or {}
    local message = error_info.message or "unknown evaluation error"
    local range = error_info.range or {}
    local start = range.start or {}
    if type(start.line) == "number" and type(start.character) == "number" then
      message = string.format("%s (%d:%d)", message, start.line + 1, start.character + 1)
    end
    table.insert(lines, string.format("!! %s", message))
  end

  local config = annotation_ctx and annotation_ctx.config
  if config then
    place_eval_annotation(config, annotation_ctx, label, result)
  end
  append_lines(lines)
end

local function eval_once(config, text, label, annotation_ctx)
  local payload = trim(text or "")
  if payload == "" then
    notify("nothing to evaluate", vim.log.levels.WARN)
    return false
  end

  M.open_output(config)

  local result = vim.system(vim.list_extend(eval_command(config), { payload }), { text = true }):wait()
  if result.code == 0 or result.code == 1 then
    local ok, decoded = pcall(vim.json.decode, result.stdout)
    if ok and type(decoded) == "table" then
      local stderr = trim(result.stderr or "")
      if stderr ~= "" then
        for _, line in ipairs(vim.split(stderr, "\n", { plain = true, trimempty = true })) do
          append_lines({ string.format("stderr| %s", strip_ansi(line)) })
        end
      end
      append_eval_result(config, label, payload, decoded, annotation_ctx)
      return decoded.ok == true
    end
  end

  local stderr = trim(result.stderr or "")
  local stdout = trim(result.stdout or "")
  local message = stderr ~= "" and stderr or stdout
  if message == "" then
    message = string.format("omni --eval failed with exit code %d", result.code)
  end
  append_lines({
    string.format(">> %s", label),
    payload,
    string.format("!! %s", message),
  })
  return false
end

local function send_text(config, text, label, mode_override, annotation_ctx)
  local payload = trim(text or "")
  if payload == "" then
    notify("nothing to send", vim.log.levels.WARN)
    return
  end

  if not state.job_id then
    if not config.auto_start then
      notify("Omni REPL is not running", vim.log.levels.WARN)
      return
    end
    if not M.start(config) then
      return
    end
  end

  if state.protocol == "server_json" then
    if not state.session_id then
      table.insert(state.deferred_requests, {
        label = label,
        payload = payload,
        annotation_ctx = annotation_ctx,
        mode = mode_override or (label == "buffer" and "program" or "expr"),
      })
      append_lines({ "[omni repl] queued request until tcp server session is ready" })
      return
    end

    local request_id = next_request_id()
    state.pending[request_id] = {
      kind = "server_eval",
      label = label,
      payload = payload,
      annotation_ctx = annotation_ctx,
      output = {},
    }
    local mode = mode_override or (label == "buffer" and "program" or "expr")
    local request = vim.json.encode({
      id = request_id,
      op = "eval",
      session = state.session_id,
      code = payload,
      mode = mode,
    })
    local sent = vim.fn.chansend(state.job_id, request .. "\n")
    if sent == 0 then
      state.pending[request_id] = nil
      append_lines({
        string.format(">> %s", label),
        payload,
        "!! failed to send request to omni repl server",
      })
    end
    return
  end

  if repl_mode(config) == "json" then
    local request_id = next_request_id()
    state.pending[request_id] = {
      kind = "legacy_eval",
      label = label,
      payload = payload,
      annotation_ctx = annotation_ctx,
    }

    local mode = mode_override or (label == "buffer" and "program" or "expr")
    local request = vim.json.encode({
      id = request_id,
      input = payload,
      mode = mode,
    })
    local sent = vim.fn.chansend(state.job_id, request .. "\n")
    if sent == 0 then
      state.pending[request_id] = nil
      append_lines({
        string.format(">> %s", label),
        payload,
        "!! failed to send request to omni structured repl",
      })
    end
    return
  end

  append_lines({ string.format(">> %s", label), payload })
  vim.fn.chansend(state.job_id, payload)
  if not ends_with_newline(payload) then
    vim.fn.chansend(state.job_id, "\n")
  end
end

local function get_buffer_text(bufnr)
  return table.concat(vim.api.nvim_buf_get_lines(bufnr, 0, -1, false), "\n")
end

local function text_from_ts_range(bufnr, start_row, start_col, end_row, end_col)
  local lines = vim.api.nvim_buf_get_text(bufnr, start_row, start_col, end_row, end_col, {})
  return table.concat(lines, "\n")
end

local function current_line_length(bufnr, row)
  local line = vim.api.nvim_buf_get_lines(bufnr, row, row + 1, false)[1] or ""
  return #line
end

local function normalize_selection_bounds(bufnr, start_row, start_col, end_row, end_col, mode)
  if end_row < start_row or (end_row == start_row and end_col < start_col) then
    start_row, end_row = end_row, start_row
    start_col, end_col = end_col, start_col
  end

  if mode == "V" or mode == "\22" or mode == "line" or mode == "block" then
    start_col = 0
    end_col = current_line_length(bufnr, end_row)
  else
    end_col = math.min(end_col + 1, current_line_length(bufnr, end_row))
  end

  if end_row == start_row and end_col <= start_col then
    end_col = math.min(start_col + 1, current_line_length(bufnr, end_row))
  end

  return start_row, start_col, end_row, end_col
end

local function selected_text(bufnr, opts)
  local mode = opts and opts.mode or vim.fn.visualmode()
  local start_row
  local start_col
  local end_row
  local end_col

  if opts and opts.start_row ~= nil then
    start_row = opts.start_row
    start_col = opts.start_col or 0
    end_row = opts.end_row or opts.start_row
    end_col = opts.end_col or current_line_length(bufnr, end_row)
  else
    local start_pos = vim.fn.getpos("'<")
    local end_pos = vim.fn.getpos("'>")
    if start_pos[2] == 0 or end_pos[2] == 0 then
      return nil
    end
    start_row = start_pos[2] - 1
    start_col = math.max(start_pos[3] - 1, 0)
    end_row = end_pos[2] - 1
    end_col = math.max(end_pos[3] - 1, 0)
  end

  start_row, start_col, end_row, end_col =
    normalize_selection_bounds(bufnr, start_row, start_col, end_row, end_col, mode)

  if start_row < 0 or end_row < start_row then
    return nil
  end
  return text_from_ts_range(bufnr, start_row, start_col, end_row, end_col)
end

local function find_tree_sitter_form_node(bufnr)
  local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "omni")
  if not ok or not parser then
    return nil
  end

  local trees = parser:parse()
  local tree = trees and trees[1] or nil
  if not tree then
    return nil
  end

  local root = tree:root()
  if not root then
    return nil
  end

  local cursor = vim.api.nvim_win_get_cursor(0)
  local row = cursor[1] - 1
  local col = cursor[2]
  local line_length = current_line_length(bufnr, row)
  if line_length == 0 then
    col = 0
  elseif col >= line_length then
    col = line_length - 1
  end

  local node = root:named_descendant_for_range(row, col, row, math.min(col + 1, line_length))
  if not node then
    return nil
  end

  local smallest_form = nil
  while node do
    local node_type = node:type()
    if ts_form_types[node_type] then
      smallest_form = smallest_form or node
      if ts_compound_form_types[node_type] then
        return node
      end
    end
    node = node:parent()
  end

  return smallest_form
end

local function find_tree_sitter_form_text(bufnr)
  local node = find_tree_sitter_form_node(bufnr)
  if not node then
    return nil
  end

  local start_row, start_col, end_row, end_col = node:range()
  if start_row == end_row and start_col == end_col then
    return nil
  end
  return {
    start_row = start_row,
    start_col = start_col,
    end_row = end_row,
    end_col = end_col,
  }
end

local function find_tree_sitter_root_form_text(bufnr)
  local node = find_tree_sitter_form_node(bufnr)
  if not node then
    return nil
  end

  local root_form = node
  local parent = root_form:parent()
  while parent do
    local parent_type = parent:type()
    if parent_type == "source_file" then
      break
    end
    if ts_form_types[parent_type] then
      root_form = parent
    end
    parent = parent:parent()
  end

  local start_row, start_col, end_row, end_col = root_form:range()
  if start_row == end_row and start_col == end_col then
    return nil
  end
  return {
    start_row = start_row,
    start_col = start_col,
    end_row = end_row,
    end_col = end_col,
  }
end

local function range_contains_cursor(start_row, start_col, end_row, end_col, row, col)
  if row < start_row or row > end_row then
    return false
  end
  if row == start_row and col < start_col then
    return false
  end
  if row == end_row and col >= end_col then
    return false
  end
  return true
end

local function range_size(start_row, start_col, end_row, end_col)
  return ((end_row - start_row) * 1048576) + (end_col - start_col)
end

local function find_tree_sitter_capture_range(bufnr, capture_names)
  local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "omni")
  if not ok or not parser then
    return nil
  end

  local ok_query, query = pcall(vim.treesitter.query.get, "omni", "textobjects")
  if not ok_query or not query then
    return nil
  end

  local trees = parser:parse()
  local tree = trees and trees[1] or nil
  if not tree then
    return nil
  end

  local root = tree:root()
  if not root then
    return nil
  end

  local capture_lookup = {}
  for _, name in ipairs(capture_names) do
    capture_lookup[name] = true
  end

  local cursor = vim.api.nvim_win_get_cursor(0)
  local row = cursor[1] - 1
  local col = cursor[2]
  local line_length = current_line_length(bufnr, row)
  if line_length == 0 then
    col = 0
  elseif col >= line_length then
    col = line_length - 1
  end

  local best = nil
  local best_size = nil
  for id, node in query:iter_captures(root, bufnr, 0, -1) do
    local capture_name = query.captures[id]
    if capture_lookup[capture_name] then
      local start_row, start_col, end_row, end_col = node:range()
      if range_contains_cursor(start_row, start_col, end_row, end_col, row, col) then
        local size = range_size(start_row, start_col, end_row, end_col)
        if not best or size < best_size then
          best = {
            start_row = start_row,
            start_col = start_col,
            end_row = end_row,
            end_col = end_col,
          }
          best_size = size
        end
      end
    end
  end

  if not best then
    return nil
  end
  return best
end

local function line_offsets(lines)
  local offsets = {}
  local acc = 1
  for i, line in ipairs(lines) do
    offsets[i] = acc
    acc = acc + #line + 1
  end
  return offsets
end

local function cursor_index(lines, row, col)
  local offsets = line_offsets(lines)
  return offsets[row] + col
end

local function build_text(lines)
  return table.concat(lines, "\n")
end

local function scan_until(text, limit)
  local stack = {}
  local in_string = false
  local in_comment = false
  local escape_next = false

  for i = 1, math.min(limit, #text) do
    local ch = text:sub(i, i)

    if in_comment then
      if ch == "\n" then
        in_comment = false
      end
    elseif in_string then
      if escape_next then
        escape_next = false
      elseif ch == "\\" then
        escape_next = true
      elseif ch == '"' then
        in_string = false
      end
    else
      if ch == ";" then
        in_comment = true
      elseif ch == '"' then
        in_string = true
      elseif matching[ch] then
        table.insert(stack, { ch = ch, pos = i })
      elseif reverse_matching[ch] then
        local top = stack[#stack]
        if top and top.ch == reverse_matching[ch] then
          table.remove(stack)
        end
      end
    end
  end

  return stack
end

local function find_form_range(bufnr)
  local cursor = vim.api.nvim_win_get_cursor(0)
  local row = cursor[1]
  local col = cursor[2]
  local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
  local text = build_text(lines)
  local idx = cursor_index(lines, row, col)
  local pre_stack = scan_until(text, math.max(idx - 1, 0))
  local post_stack = scan_until(text, idx)
  local opener = post_stack[#post_stack] or pre_stack[#pre_stack]

  if not opener then
    return nil
  end

  local stack = { opener }
  local in_string = false
  local in_comment = false
  local escape_next = false

  for i = opener.pos + 1, #text do
    local ch = text:sub(i, i)
    if in_comment then
      if ch == "\n" then
        in_comment = false
      end
    elseif in_string then
      if escape_next then
        escape_next = false
      elseif ch == "\\" then
        escape_next = true
      elseif ch == '"' then
        in_string = false
      end
    else
      if ch == ";" then
        in_comment = true
      elseif ch == '"' then
        in_string = true
      elseif matching[ch] then
        table.insert(stack, { ch = ch, pos = i })
      elseif reverse_matching[ch] then
        local top = stack[#stack]
        if top and top.ch == reverse_matching[ch] then
          table.remove(stack)
          if #stack == 0 then
            return opener.pos, i
          end
        end
      end
    end
  end

  return nil
end

local function find_root_form_range(bufnr)
  local cursor = vim.api.nvim_win_get_cursor(0)
  local row = cursor[1]
  local col = cursor[2]
  local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
  local text = build_text(lines)
  local idx = cursor_index(lines, row, col)

  local stack = {}
  local in_string = false
  local in_comment = false
  local escape_next = false
  local root_start = nil

  for i = 1, #text do
    local ch = text:sub(i, i)

    if in_comment then
      if ch == "\n" then
        in_comment = false
      end
    elseif in_string then
      if escape_next then
        escape_next = false
      elseif ch == "\\" then
        escape_next = true
      elseif ch == '"' then
        in_string = false
      end
    else
      if ch == ";" then
        in_comment = true
      elseif ch == '"' then
        in_string = true
      elseif matching[ch] then
        if #stack == 0 then
          root_start = i
        end
        table.insert(stack, { ch = ch, pos = i })
      elseif reverse_matching[ch] then
        local top = stack[#stack]
        if top and top.ch == reverse_matching[ch] then
          table.remove(stack)
          if #stack == 0 and root_start and idx >= root_start and idx <= i then
            return root_start, i
          end
        end
      end
    end
  end

  return nil
end

local function text_from_range(bufnr, start_idx, end_idx)
  local text = get_buffer_text(bufnr)
  return text:sub(start_idx, end_idx)
end

local function text_from_selection_range(bufnr, selection)
  if not selection then
    return nil
  end
  return text_from_ts_range(
    bufnr,
    selection.start_row,
    selection.start_col,
    selection.end_row,
    selection.end_col
  )
end

local function absolute_index(lines, row, col)
  local offsets = line_offsets(lines)
  return offsets[row + 1] + col
end

local function position_from_index(lines, idx)
  local remaining = math.max(idx - 1, 0)
  for row, line in ipairs(lines) do
    local width = #line
    if remaining <= width then
      return row - 1, remaining
    end
    remaining = remaining - width - 1
  end

  local last_row = math.max(#lines - 1, 0)
  return last_row, current_line_length(vim.api.nvim_get_current_buf(), last_row)
end

local function selection_to_absolute_range(bufnr, selection)
  local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
  if vim.tbl_isempty(lines) then
    return nil, nil, nil
  end
  return absolute_index(lines, selection.start_row, selection.start_col), absolute_index(lines, selection.end_row, selection.end_col), lines
end

local function selection_from_absolute_range(bufnr, start_idx, end_idx)
  local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
  if vim.tbl_isempty(lines) or start_idx >= end_idx then
    return nil
  end

  local start_row, start_col = position_from_index(lines, start_idx)
  local end_row, end_col = position_from_index(lines, end_idx)
  return {
    start_row = start_row,
    start_col = start_col,
    end_row = end_row,
    end_col = end_col,
  }
end

local function selection_from_index_range(bufnr, start_idx, end_idx)
  return selection_from_absolute_range(bufnr, start_idx, end_idx + 1)
end

local function trim_wrapper_selection(bufnr, selection)
  local text = text_from_selection_range(bufnr, selection)
  if not text or text == "" then
    return nil
  end

  local start_idx, end_idx = selection_to_absolute_range(bufnr, selection)
  if not start_idx or not end_idx or start_idx >= end_idx then
    return nil
  end

  local first_two = text:sub(1, 2)
  local first = text:sub(1, 1)
  local last = text:sub(-1)
  local inner_start = start_idx
  local inner_end = end_idx

  if matching[first] and matching[first] == last then
    inner_start = inner_start + 1
    inner_end = inner_end - 1
  elseif first_two == ",@" then
    inner_start = inner_start + 2
  elseif first == "'" or first == "`" or first == "," or first == "." then
    inner_start = inner_start + 1
  else
    return selection
  end

  if inner_start >= inner_end then
    return selection
  end
  return selection_from_absolute_range(bufnr, inner_start, inner_end) or selection
end

local function visual_end_from_range(bufnr, selection)
  local end_row = selection.end_row
  local end_col = selection.end_col

  if end_col > 0 then
    return end_row, end_col - 1
  end
  if end_row == 0 then
    return 0, 0
  end

  end_row = end_row - 1
  local previous_length = current_line_length(bufnr, end_row)
  return end_row, math.max(previous_length - 1, 0)
end

local function select_range(bufnr, selection)
  if not selection then
    return false
  end

  local start_row = selection.start_row
  local start_col = selection.start_col
  local end_row, end_col = visual_end_from_range(bufnr, selection)

  vim.fn.setpos("'<", { 0, start_row + 1, start_col + 1, 0 })
  vim.fn.setpos("'>", { 0, end_row + 1, end_col + 1, 0 })
  vim.api.nvim_win_set_cursor(0, { start_row + 1, start_col })
  vim.cmd("normal! gv")
  return true
end

local function eval_text(config, text, label, mode_override, annotation_ctx)
  if config.eval and config.eval.mode == "json" then
    local ok = eval_once(config, text, label, annotation_ctx)
    if ok or not config.eval.fallback_to_repl then
      return
    end
  end
  send_text(config, text, label, mode_override, annotation_ctx)
end

function M.send_current_form(config)
  local bufnr = vim.api.nvim_get_current_buf()
  local selection = find_tree_sitter_form_text(bufnr)
  local text = text_from_selection_range(bufnr, selection)
  if not text then
    local start_idx, end_idx = find_form_range(bufnr)
    if not start_idx or not end_idx then
      return M.send_current_line(config)
    end
    text = text_from_range(bufnr, start_idx, end_idx)
    selection = selection_from_index_range(bufnr, start_idx, end_idx)
  end
  eval_text(config, text, "form", nil, { config = config, bufnr = bufnr, selection = selection })
end

function M.send_root_form(config)
  local bufnr = vim.api.nvim_get_current_buf()
  local selection = find_tree_sitter_root_form_text(bufnr)
  local text = text_from_selection_range(bufnr, selection)
  if not text then
    local start_idx, end_idx = find_root_form_range(bufnr)
    if not start_idx or not end_idx then
      return M.send_current_form(config)
    end
    text = text_from_range(bufnr, start_idx, end_idx)
    selection = selection_from_index_range(bufnr, start_idx, end_idx)
  end

  eval_text(config, text, "root", nil, { config = config, bufnr = bufnr, selection = selection })
end

function M.send_current_line(config)
  local bufnr = vim.api.nvim_get_current_buf()
  local line = vim.api.nvim_win_get_cursor(0)[1] - 1
  local line_text = vim.api.nvim_get_current_line()
  local selection = {
    start_row = line,
    start_col = 0,
    end_row = line,
    end_col = #line_text,
  }
  eval_text(config, line_text, "line", nil, { config = config, bufnr = bufnr, selection = selection })
end

local function send_capture_group(config, capture_group_name, label, fallback)
  local bufnr = vim.api.nvim_get_current_buf()
  local capture_names = ts_eval_capture_groups[capture_group_name]
  local selection = capture_names and find_tree_sitter_capture_range(bufnr, capture_names) or nil
  local text = text_from_selection_range(bufnr, selection)
  if not text then
    return fallback(config)
  end
  eval_text(config, text, label, nil, { config = config, bufnr = bufnr, selection = selection })
end

function M.send_current_call(config)
  send_capture_group(config, "call", "call", M.send_current_form)
end

function M.send_current_block(config)
  send_capture_group(config, "block", "block", M.send_root_form)
end

function M.send_current_declaration(config)
  send_capture_group(config, "declaration", "declaration", M.send_root_form)
end

function M.select_current_form()
  local bufnr = vim.api.nvim_get_current_buf()
  local selection = find_tree_sitter_form_text(bufnr)
  if not selection then
    local start_idx, end_idx = find_form_range(bufnr)
    if not start_idx or not end_idx then
      notify("no enclosing form found", vim.log.levels.WARN)
      return
    end
    selection = selection_from_index_range(bufnr, start_idx, end_idx)
  end
  select_range(bufnr, selection)
end

function M.select_current_form_inner()
  local bufnr = vim.api.nvim_get_current_buf()
  local selection = find_tree_sitter_form_text(bufnr)
  if not selection then
    local start_idx, end_idx = find_form_range(bufnr)
    if not start_idx or not end_idx then
      notify("no enclosing form found", vim.log.levels.WARN)
      return
    end
    selection = selection_from_index_range(bufnr, start_idx, end_idx)
  end
  select_range(bufnr, trim_wrapper_selection(bufnr, selection) or selection)
end

function M.select_root_form()
  local bufnr = vim.api.nvim_get_current_buf()
  local selection = find_tree_sitter_root_form_text(bufnr)
  if not selection then
    local start_idx, end_idx = find_root_form_range(bufnr)
    if not start_idx or not end_idx then
      notify("no enclosing root form found", vim.log.levels.WARN)
      return
    end
    selection = selection_from_index_range(bufnr, start_idx, end_idx)
  end
  select_range(bufnr, selection)
end

function M.select_root_form_inner()
  local bufnr = vim.api.nvim_get_current_buf()
  local selection = find_tree_sitter_root_form_text(bufnr)
  if not selection then
    local start_idx, end_idx = find_root_form_range(bufnr)
    if not start_idx or not end_idx then
      notify("no enclosing root form found", vim.log.levels.WARN)
      return
    end
    selection = selection_from_index_range(bufnr, start_idx, end_idx)
  end
  select_range(bufnr, trim_wrapper_selection(bufnr, selection) or selection)
end

local function select_capture_group(capture_group_name, fallback, inner)
  local bufnr = vim.api.nvim_get_current_buf()
  local capture_names = ts_eval_capture_groups[capture_group_name]
  local selection = capture_names and find_tree_sitter_capture_range(bufnr, capture_names) or nil
  if selection then
    if inner then
      selection = trim_wrapper_selection(bufnr, selection) or selection
    end
    select_range(bufnr, selection)
    return
  end
  fallback()
end

function M.select_current_call()
  select_capture_group("call", M.select_current_form, false)
end

function M.select_current_block()
  select_capture_group("block", M.select_root_form, false)
end

function M.select_current_declaration()
  select_capture_group("declaration", M.select_root_form, false)
end

function M.select_current_call_inner()
  select_capture_group("call", M.select_current_form_inner, true)
end

function M.select_current_block_inner()
  select_capture_group("block", M.select_root_form_inner, true)
end

function M.select_current_declaration_inner()
  select_capture_group("declaration", M.select_root_form_inner, true)
end

function M.send_buffer(config)
  send_text(config, get_buffer_text(vim.api.nvim_get_current_buf()), "buffer")
end

function M.send_selection(config, opts)
  local bufnr = vim.api.nvim_get_current_buf()
  local text = selected_text(bufnr, opts)
  if not text or trim(text) == "" then
    notify("nothing selected to evaluate", vim.log.levels.WARN)
    return
  end
  send_text(config, text, "selection", "program")
end

function M.send_operator(config, motion_type)
  local start_pos = vim.fn.getpos("'[")
  local end_pos = vim.fn.getpos("']")
  if start_pos[2] == 0 or end_pos[2] == 0 then
    notify("operator range is not available", vim.log.levels.WARN)
    return
  end

  local mode = motion_type or "char"
  if mode == "v" then
    mode = "char"
  elseif mode == "V" then
    mode = "line"
  elseif mode == "\22" then
    mode = "block"
  end

  M.send_selection(config, {
    mode = mode,
    start_row = start_pos[2] - 1,
    start_col = math.max(start_pos[3] - 1, 0),
    end_row = end_pos[2] - 1,
    end_col = math.max(end_pos[3] - 1, 0),
  })
end

return M
