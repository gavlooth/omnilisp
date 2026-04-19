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

local Module = {}

function Module.new()
  local trim = utils.trim
  local api = {}

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


  api.get_buffer_text = get_buffer_text
  api.text_from_range = text_from_range
  api.selected_text = selected_text
  api.text_from_selection_range = text_from_selection_range
  api.find_tree_sitter_form_text = find_tree_sitter_form_text
  api.find_tree_sitter_root_form_text = find_tree_sitter_root_form_text
  api.find_tree_sitter_capture_range = find_tree_sitter_capture_range
  api.find_form_range = find_form_range
  api.find_root_form_range = find_root_form_range
  api.selection_from_index_range = selection_from_index_range
  api.trim_wrapper_selection = trim_wrapper_selection
  api.select_range = select_range
  api.ts_eval_capture_groups = ts_eval_capture_groups
  return api
end

return Module
