local utils = require("omni.repl.utils")

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

local Module = {}

function Module.new(state)
  local strip_ansi = utils.strip_ansi
  local truncate_text = utils.truncate_text
  local stringify_eval_value = utils.stringify_eval_value

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

  local function open_output(config)
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

  local function clear_output()
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


  local api = {
    append_lines = append_lines,
    clear_output = clear_output,
    flush_partial = flush_partial,
    format_eval_value = format_eval_value,
    handle_stream = handle_stream,
    open_output = open_output,
    place_eval_annotation = place_eval_annotation,
  }
  return api
end

return Module
