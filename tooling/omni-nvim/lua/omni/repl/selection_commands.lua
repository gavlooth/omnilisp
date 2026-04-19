local utils = require("omni.repl.utils")

local Module = {}

function Module.new(selectors, send_text, eval_once, notify)
  local trim = utils.trim
  local get_buffer_text = selectors.get_buffer_text
  local text_from_range = selectors.text_from_range
  local selected_text = selectors.selected_text
  local text_from_selection_range = selectors.text_from_selection_range
  local find_tree_sitter_form_text = selectors.find_tree_sitter_form_text
  local find_tree_sitter_root_form_text = selectors.find_tree_sitter_root_form_text
  local find_tree_sitter_capture_range = selectors.find_tree_sitter_capture_range
  local find_form_range = selectors.find_form_range
  local find_root_form_range = selectors.find_root_form_range
  local selection_from_index_range = selectors.selection_from_index_range
  local trim_wrapper_selection = selectors.trim_wrapper_selection
  local select_range = selectors.select_range
  local ts_eval_capture_groups = selectors.ts_eval_capture_groups
  local api = {}

  local function eval_text(config, text, label, mode_override, annotation_ctx)
    if config.eval and config.eval.mode == "json" then
      local ok = eval_once(config, text, label, annotation_ctx)
      if ok or not config.eval.fallback_to_repl then
        return
      end
    end
    send_text(config, text, label, mode_override, annotation_ctx)
  end

  function api.send_current_form(config)
    local bufnr = vim.api.nvim_get_current_buf()
    local selection = find_tree_sitter_form_text(bufnr)
    local text = text_from_selection_range(bufnr, selection)
    if not text then
      local start_idx, end_idx = find_form_range(bufnr)
      if not start_idx or not end_idx then
        return api.send_current_line(config)
      end
      text = text_from_range(bufnr, start_idx, end_idx)
      selection = selection_from_index_range(bufnr, start_idx, end_idx)
    end
    eval_text(config, text, "form", nil, { config = config, bufnr = bufnr, selection = selection })
  end

  function api.send_root_form(config)
    local bufnr = vim.api.nvim_get_current_buf()
    local selection = find_tree_sitter_root_form_text(bufnr)
    local text = text_from_selection_range(bufnr, selection)
    if not text then
      local start_idx, end_idx = find_root_form_range(bufnr)
      if not start_idx or not end_idx then
        return api.send_current_form(config)
      end
      text = text_from_range(bufnr, start_idx, end_idx)
      selection = selection_from_index_range(bufnr, start_idx, end_idx)
    end

    eval_text(config, text, "root", nil, { config = config, bufnr = bufnr, selection = selection })
  end

  function api.send_current_line(config)
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

  function api.send_current_call(config)
    send_capture_group(config, "call", "call", api.send_current_form)
  end

  function api.send_current_block(config)
    send_capture_group(config, "block", "block", api.send_root_form)
  end

  function api.send_current_declaration(config)
    send_capture_group(config, "declaration", "declaration", api.send_root_form)
  end

  function api.select_current_form()
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

  function api.select_current_form_inner()
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

  function api.select_root_form()
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

  function api.select_root_form_inner()
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

  function api.select_current_call()
    select_capture_group("call", api.select_current_form, false)
  end

  function api.select_current_block()
    select_capture_group("block", api.select_root_form, false)
  end

  function api.select_current_declaration()
    select_capture_group("declaration", api.select_root_form, false)
  end

  function api.select_current_call_inner()
    select_capture_group("call", api.select_current_form_inner, true)
  end

  function api.select_current_block_inner()
    select_capture_group("block", api.select_root_form_inner, true)
  end

  function api.select_current_declaration_inner()
    select_capture_group("declaration", api.select_root_form_inner, true)
  end

  function api.send_buffer(config)
    send_text(config, get_buffer_text(vim.api.nvim_get_current_buf()), "buffer")
  end

  function api.send_selection(config, opts)
    local bufnr = vim.api.nvim_get_current_buf()
    local text = selected_text(bufnr, opts)
    if not text or trim(text) == "" then
      notify("nothing selected to evaluate", vim.log.levels.WARN)
      return
    end
    send_text(config, text, "selection", "program")
  end

  function api.send_operator(config, motion_type)
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

    api.send_selection(config, {
      mode = mode,
      start_row = start_pos[2] - 1,
      start_col = math.max(start_pos[3] - 1, 0),
      end_row = end_pos[2] - 1,
      end_col = math.max(end_pos[3] - 1, 0),
    })
  end


  return api
end

return Module
