local M = {}

function M.new(ctx)
  local current_bufnr = ctx.current_bufnr
  local location_to_qf_item = ctx.location_to_qf_item
  local notify = ctx.notify
  local request_sync = ctx.request_sync
  local text_document_params = ctx.text_document_params
  local cursor_position = ctx.cursor_position
  local collect_call_hierarchy_items = ctx.collect_call_hierarchy_items
  local set_quickfix = ctx.set_quickfix
  local api = {}

  local function position_in_range(position, range)
    local start = range.start or {}
    local finish = range["end"] or {}
    local line = position.line or 0
    local character = position.character or 0
    local start_line = start.line or 0
    local start_character = start.character or 0
    local end_line = finish.line or 0
    local end_character = finish.character or 0

    if line < start_line or line > end_line then
      return false
    end
    if line == start_line and character < start_character then
      return false
    end
    if line == end_line and character > end_character then
      return false
    end
    return true
  end

  local function compare_link_position(left, right)
    local left_start = (left.range or {}).start or {}
    local right_start = (right.range or {}).start or {}
    if (left_start.line or 0) == (right_start.line or 0) then
      return (left_start.character or 0) < (right_start.character or 0)
    end
    return (left_start.line or 0) < (right_start.line or 0)
  end

  local function current_document_links(bufnr)
    local responses = request_sync(bufnr, "textDocument/documentLink", text_document_params(bufnr))
    if not responses then
      return nil
    end

    local links = {}
    for _, response in pairs(responses) do
      for _, link in ipairs(response.result or {}) do
        if type(link.range) == "table" then
          table.insert(links, link)
        end
      end
    end
    table.sort(links, compare_link_position)
    return links
  end

  local function jump_to_link(direction)
    local bufnr = current_bufnr()
    local links = current_document_links(bufnr)
    if not links then
      return false
    end
    if vim.tbl_isempty(links) then
      notify("Omni document link: no links in buffer", vim.log.levels.INFO)
      return false
    end

    local cursor = cursor_position()
    local current_index = nil
    for index, link in ipairs(links) do
      if position_in_range(cursor, link.range) then
        current_index = index
        break
      end
    end

    local target_index = nil
    if direction > 0 then
      for index, link in ipairs(links) do
        local start = (link.range or {}).start or {}
        if (start.line or 0) > cursor.line or (
          (start.line or 0) == cursor.line and (start.character or 0) > cursor.character
        ) then
          target_index = index
          break
        end
      end
      if target_index == nil then
        target_index = 1
      end
    else
      for index = #links, 1, -1 do
        local link = links[index]
        local start = (link.range or {}).start or {}
        if (start.line or 0) < cursor.line or (
          (start.line or 0) == cursor.line and (start.character or 0) < cursor.character
        ) then
          target_index = index
          break
        end
      end
      if target_index == nil then
        target_index = #links
      end
    end

    if current_index ~= nil and #links == 1 then
      target_index = current_index
    end

    local target = links[target_index]
    local start = (target.range or {}).start or {}
    vim.api.nvim_win_set_cursor(0, { (start.line or 0) + 1, start.character or 0 })
    return true
  end

  function api.open_link()
    local bufnr = current_bufnr()
    local links = current_document_links(bufnr)
    if not links then
      return false
    end

    local position = cursor_position()

    local selected = nil
    for _, link in ipairs(links) do
      if position_in_range(position, link.range) then
        selected = link
        break
      end
    end
    if not selected then
      notify("Omni document link: no target at cursor", vim.log.levels.INFO)
      return false
    end

    if selected.target:match("^file://") then
      vim.cmd("edit " .. vim.fn.fnameescape(vim.uri_to_fname(selected.target)))
      return true
    end
    notify("Omni document link: unsupported target URI", vim.log.levels.WARN)
    return false
  end

  function api.next_link()
    return jump_to_link(1)
  end

  function api.prev_link()
    return jump_to_link(-1)
  end

  function api.document_links()
    local bufnr = current_bufnr()
    local links = current_document_links(bufnr)
    if not links then
      return false
    end

    local items = {}
    local source_name = vim.api.nvim_buf_get_name(bufnr)
    for _, link in ipairs(links) do
      if type(link.target) ~= "string" or not link.target:match("^file://") then
        goto continue
      end
      local start = (link.range or {}).start or {}
      table.insert(items, {
        filename = source_name,
        lnum = (start.line or 0) + 1,
        col = (start.character or 0) + 1,
        text = vim.fn.fnamemodify(vim.uri_to_fname(link.target), ":t"),
      })
      ::continue::
    end

    set_quickfix(items, "Omni document links")
    return true
  end

  function api.references(include_declaration)
    local bufnr = current_bufnr()
    local responses = request_sync(bufnr, "textDocument/references", {
      textDocument = {
        uri = vim.uri_from_bufnr(bufnr),
      },
      position = cursor_position(),
      context = {
        includeDeclaration = include_declaration ~= false,
      },
    })
    if not responses then
      return false
    end

    local items = {}
    for _, response in pairs(responses) do
      for _, location in ipairs(response.result or {}) do
        local item = location_to_qf_item(location, nil)
        if item then
          table.insert(items, item)
        end
      end
    end
    set_quickfix(items, "Omni references")
    return true
  end

  local function call_hierarchy_prepare()
    local bufnr = current_bufnr()
    local responses = request_sync(bufnr, "textDocument/prepareCallHierarchy", {
      textDocument = {
        uri = vim.uri_from_bufnr(bufnr),
      },
      position = cursor_position(),
    })
    if not responses then
      return nil
    end

    local items = collect_call_hierarchy_items(responses)
    if vim.tbl_isempty(items) then
      notify("Omni call hierarchy: no symbol at cursor", vim.log.levels.INFO)
      return nil
    end
    return items
  end

  local function call_hierarchy_request(method, title, item_to_qf)
    local prepared_items = call_hierarchy_prepare()
    if not prepared_items then
      return false
    end

    local qf_items = {}
    for _, prepared_item in ipairs(prepared_items) do
      local responses = request_sync(current_bufnr(), method, { item = prepared_item })
      if responses then
        for _, response in pairs(responses) do
          for _, entry in ipairs(response.result or {}) do
            local item = item_to_qf(entry, prepared_item)
            if item then
              table.insert(qf_items, item)
            end
          end
        end
      end
    end

    set_quickfix(qf_items, title)
    return true
  end

  local function call_hierarchy_callsite_qf(source, target, from_ranges)
    local location = nil
    if type(from_ranges[1]) == "table" and type(source.uri) == "string" then
      location = {
        uri = source.uri,
        range = from_ranges[1],
      }
    else
      location = target
    end

    local label = string.format(
      "%s -> %s",
      source.name or "(anonymous)",
      target.name or "(anonymous)"
    )
    return location_to_qf_item(location, label)
  end

  local function location_request(method, title)
    local bufnr = current_bufnr()
    local responses = request_sync(bufnr, method, {
      textDocument = {
        uri = vim.uri_from_bufnr(bufnr),
      },
      position = cursor_position(),
    })
    if not responses then
      return false
    end

    local items = {}
    for _, response in pairs(responses) do
      for _, location in ipairs(response.result or {}) do
        local item = location_to_qf_item(location, nil)
        if item then
          table.insert(items, item)
        end
      end
    end
    set_quickfix(items, title)
    return true
  end

  function api.definitions()
    return location_request("textDocument/definition", "Omni definitions")
  end

  function api.declarations()
    return location_request("textDocument/declaration", "Omni declarations")
  end

  function api.implementations()
    return location_request("textDocument/implementation", "Omni implementations")
  end

  function api.type_definitions()
    return location_request("textDocument/typeDefinition", "Omni type definitions")
  end

  function api.incoming_calls()
    return call_hierarchy_request(
      "callHierarchy/incomingCalls",
      "Omni incoming calls",
      function(entry, prepared_item)
        return call_hierarchy_callsite_qf(entry.from or {}, prepared_item or {}, entry.fromRanges or {})
      end
    )
  end

  function api.outgoing_calls()
    return call_hierarchy_request(
      "callHierarchy/outgoingCalls",
      "Omni outgoing calls",
      function(entry, prepared_item)
        return call_hierarchy_callsite_qf(prepared_item or {}, entry.to or {}, entry.fromRanges or {})
      end
    )
  end

  function api.apply_folds()
    local bufnr = current_bufnr()
    local responses = request_sync(bufnr, "textDocument/foldingRange", text_document_params(bufnr))
    if not responses then
      return false
    end

    local ranges = {}
    local seen = {}
    for _, response in pairs(responses) do
      for _, item in ipairs(response.result or {}) do
        local start_line = tonumber(item.startLine)
        local end_line = tonumber(item.endLine)
        if start_line and end_line and end_line > start_line then
          local key = string.format("%d:%d", start_line, end_line)
          if not seen[key] then
            seen[key] = true
            table.insert(ranges, {
              start_line = start_line,
              end_line = end_line,
            })
          end
        end
      end
    end

    table.sort(ranges, function(left, right)
      if left.start_line == right.start_line then
        return left.end_line > right.end_line
      end
      return left.start_line < right.start_line
    end)

    vim.api.nvim_buf_call(bufnr, function()
      local view = vim.fn.winsaveview()
      vim.wo.foldmethod = "manual"
      vim.cmd("silent! normal! zE")
      for _, range in ipairs(ranges) do
        vim.cmd(string.format("silent keepjumps %d,%dfold", range.start_line + 1, range.end_line + 1))
      end
      vim.fn.winrestview(view)
    end)

    notify(string.format("Omni LSP folds refreshed: %d", #ranges), vim.log.levels.INFO)
    return true
  end

  local function flatten_selection_ranges(node, ranges)
    local current = node
    while type(current) == "table" and type(current.range) == "table" do
      table.insert(ranges, current.range)
      current = current.parent
    end
  end

  local function range_key(range)
    local start = range.start or {}
    local finish = range["end"] or {}
    return string.format(
      "%d:%d:%d:%d",
      tonumber(start.line or 0),
      tonumber(start.character or 0),
      tonumber(finish.line or 0),
      tonumber(finish.character or 0)
    )
  end

  local function current_selection_key()
    local start_pos = vim.fn.getpos("'<")
    local end_pos = vim.fn.getpos("'>")
    if start_pos[2] == 0 or end_pos[2] == 0 then
      return nil
    end

    local start_line = start_pos[2] - 1
    local start_col = math.max(0, start_pos[3] - 1)
    local end_line = end_pos[2] - 1
    local end_col = math.max(0, end_pos[3])
    return string.format("%d:%d:%d:%d", start_line, start_col, end_line, end_col)
  end

  local function visual_end_position(bufnr, range)
    local finish = range["end"] or {}
    local end_line = tonumber(finish.line or 0)
    local end_character = tonumber(finish.character or 0)
    if end_character > 0 then
      return end_line + 1, end_character
    end

    if end_line > 0 then
      local previous_line = vim.api.nvim_buf_get_lines(bufnr, end_line - 1, end_line, false)[1] or ""
      return end_line, math.max(1, #previous_line)
    end
    return 1, 1
  end

  local function apply_selection_range(bufnr, range)
    local start = range.start or {}
    local start_line = tonumber(start.line or 0) + 1
    local start_col = tonumber(start.character or 0) + 1
    local end_line, end_col = visual_end_position(bufnr, range)

    vim.fn.setpos("'<", { 0, start_line, start_col, 0 })
    vim.fn.setpos("'>", { 0, end_line, end_col, 0 })
    vim.api.nvim_win_set_cursor(0, { end_line, math.max(0, end_col - 1) })
    if #vim.api.nvim_list_uis() > 0 then
      vim.cmd("normal! gv")
    end
  end

  function api.expand_selection()
    local bufnr = current_bufnr()
    local cursor = vim.api.nvim_win_get_cursor(0)
    local responses = request_sync(bufnr, "textDocument/selectionRange", {
      textDocument = {
        uri = vim.uri_from_bufnr(bufnr),
      },
      positions = {
        {
          line = cursor[1] - 1,
          character = cursor[2],
        },
      },
    })
    if not responses then
      return false
    end

    local ranges = {}
    for _, response in pairs(responses) do
      local result = response.result or {}
      if type(result[1]) == "table" then
        flatten_selection_ranges(result[1], ranges)
        break
      end
    end
    if vim.tbl_isempty(ranges) then
      notify("Omni LSP selection range: no results", vim.log.levels.INFO)
      return false
    end

    local current_key = current_selection_key()
    local next_index = 1
    if current_key then
      for index, range in ipairs(ranges) do
        if range_key(range) == current_key then
          next_index = math.min(index + 1, #ranges)
          break
        end
      end
    end

    apply_selection_range(bufnr, ranges[next_index])
    return true
  end

  return api
end

return M
