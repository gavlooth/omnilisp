local M = {}

local state = {
  notifier = function(message, level)
    vim.notify(message, level or vim.log.levels.INFO, { title = "omni.nvim" })
  end,
  commands_registered = false,
  pull_diagnostics = {
    document = {},
    workspace = {},
  },
}

local function path_exists(path)
  return type(path) == "string" and path ~= "" and vim.fn.filereadable(path) == 1
end

local function directory_exists(path)
  return type(path) == "string" and path ~= "" and vim.fn.isdirectory(path) == 1
end

local function module_root()
  local source = debug.getinfo(1, "S").source:sub(2)
  local omni_lua_dir = vim.fs.dirname(source)
  local plugin_root = vim.fs.dirname(vim.fs.dirname(omni_lua_dir))
  return plugin_root
end

local function configured_repo_root(config)
  if config.lsp and directory_exists(config.lsp.repo_root) then
    return config.lsp.repo_root
  end
  if directory_exists(config.repo_root) then
    return config.repo_root
  end

  local plugin_root = module_root()
  local default_root = vim.fs.normalize(plugin_root .. "/../..")
  if directory_exists(default_root) then
    return default_root
  end
  return nil
end

local function server_path(config)
  if config.lsp and path_exists(config.lsp.server_path) then
    return config.lsp.server_path
  end

  local repo = configured_repo_root(config)
  if not repo then
    return nil
  end

  local default_path = vim.fs.normalize(repo .. "/tooling/omni-lsp/omni_lsp.py")
  if path_exists(default_path) then
    return default_path
  end
  return nil
end

local function root_markers(config)
  if config.lsp and type(config.lsp.root_markers) == "table" and not vim.tbl_isempty(config.lsp.root_markers) then
    return config.lsp.root_markers
  end
  return { "omni.toml", "project.json", ".git" }
end

local function server_cmd(config)
  if config.lsp and type(config.lsp.cmd) == "table" and not vim.tbl_isempty(config.lsp.cmd) then
    return config.lsp.cmd
  end

  local path = server_path(config)
  if not path then
    return nil
  end
  return { "python3", path }
end

function M.spec(config)
  local cmd = server_cmd(config)
  if not cmd then
    return nil
  end

  return {
    cmd = cmd,
    filetypes = { "omni" },
    root_markers = root_markers(config),
  }
end

local function setup_builtin(config, spec)
  if not (vim.lsp and type(vim.lsp.enable) == "function") then
    return false
  end

  local configured = false
  if vim.lsp.config ~= nil then
    configured = pcall(vim.lsp.config, "omni_lsp", spec)
  end
  if not configured and type(vim.lsp.config) == "table" then
    vim.lsp.config.omni_lsp = spec
    configured = true
  end
  if not configured then
    return false
  end

  if not (config.lsp and config.lsp.enable == false) then
    vim.lsp.enable("omni_lsp")
  end
  return true
end

local function setup_lspconfig(config, spec)
  local ok_configs, configs = pcall(require, "lspconfig.configs")
  local ok_lspconfig, lspconfig = pcall(require, "lspconfig")
  if not ok_configs or not ok_lspconfig then
    return false
  end

  local root_pattern = nil
  local ok_util, util = pcall(require, "lspconfig.util")
  if ok_util then
    root_pattern = util.root_pattern(unpack(root_markers(config)))
  end

  configs.omni_lsp = {
    default_config = {
      cmd = spec.cmd,
      filetypes = spec.filetypes,
      root_dir = root_pattern,
      single_file_support = true,
    },
  }

  if not (config.lsp and config.lsp.enable == false) then
    lspconfig.omni_lsp.setup({})
  end
  return true
end

function M.setup(config)
  local spec = M.spec(config)
  if not spec then
    return false, "Omni LSP server path not found"
  end

  if setup_builtin(config, spec) then
    return true, "builtin"
  end
  if setup_lspconfig(config, spec) then
    return true, "lspconfig"
  end
  return false, "No supported Neovim LSP registration API found"
end

function M.set_notifier(fn)
  if type(fn) == "function" then
    state.notifier = fn
  end
end

local function notify(message, level)
  state.notifier(message, level)
end

local function current_bufnr()
  return vim.api.nvim_get_current_buf()
end

local function text_document_params(bufnr)
  local uri = vim.uri_from_bufnr(bufnr)
  return { textDocument = { uri = uri } }
end

local function cursor_position()
  local cursor = vim.api.nvim_win_get_cursor(0)
  return {
    line = cursor[1] - 1,
    character = cursor[2],
  }
end

local function request_sync(bufnr, method, params, timeout_ms)
  if not (vim.lsp and type(vim.lsp.buf_request_sync) == "function") then
    notify("Neovim LSP request API not available", vim.log.levels.WARN)
    return nil
  end
  return vim.lsp.buf_request_sync(bufnr, method, params, timeout_ms or 1000)
end

local function workspace_cache_key(bufnr, client_id)
  if client_id ~= nil then
    return tostring(client_id)
  end
  if vim.lsp and type(vim.lsp.get_clients) == "function" then
    local ok, clients = pcall(vim.lsp.get_clients, { bufnr = bufnr })
    if ok then
      for _, client in ipairs(clients or {}) do
        if client.name == "omni_lsp" and client.id ~= nil then
          return tostring(client.id)
        end
      end
    end
  end
  return "__default__"
end

local function location_to_qf_item(location, fallback_name)
  local uri = location.uri or ((location.targetUri ~= nil) and location.targetUri)
  local range = location.range or location.targetSelectionRange or location.targetRange
  if type(uri) ~= "string" or type(range) ~= "table" then
    return nil
  end

  local start = range.start or {}
  return {
    filename = vim.uri_to_fname(uri),
    lnum = (start.line or 0) + 1,
    col = (start.character or 0) + 1,
    text = fallback_name or vim.fn.fnamemodify(vim.uri_to_fname(uri), ":t"),
  }
end

local function diagnostic_to_qf_item(uri, diagnostic)
  if type(uri) ~= "string" or type(diagnostic) ~= "table" then
    return nil
  end
  local item_range = diagnostic.range or {}
  local start = item_range.start or {}
  local severity = tonumber(diagnostic.severity or 1) or 1
  local severity_label = ({
    [1] = "error",
    [2] = "warning",
    [3] = "info",
    [4] = "hint",
  })[severity] or "diagnostic"
  local source = diagnostic.source or "omni"
  local message = tostring(diagnostic.message or "")
  return {
    filename = vim.uri_to_fname(uri),
    lnum = (start.line or 0) + 1,
    col = (start.character or 0) + 1,
    text = string.format("%s [%s] %s", source, severity_label, message),
  }
end

local function symbol_item_to_qf(symbol, parent_name)
  local name = symbol.name or "(anonymous)"
  local label = parent_name and (parent_name .. " / " .. name) or name
  if symbol.location then
    return location_to_qf_item(symbol.location, label)
  end

  local selection = symbol.selectionRange or symbol.range
  if type(selection) ~= "table" then
    return nil
  end
  local start = selection.start or {}
  return {
    filename = vim.api.nvim_buf_get_name(current_bufnr()),
    lnum = (start.line or 0) + 1,
    col = (start.character or 0) + 1,
    text = label,
  }
end

local function collect_document_symbol_items(symbols, items, parent_name)
  for _, symbol in ipairs(symbols or {}) do
    local item = symbol_item_to_qf(symbol, parent_name)
    if item then
      table.insert(items, item)
    end
    if type(symbol.children) == "table" then
      collect_document_symbol_items(symbol.children, items, symbol.name or parent_name)
    end
  end
end

local function set_quickfix(items, title, opts)
  local should_open = not (type(opts) == "table" and opts.open == false)
  local should_notify_empty = not (type(opts) == "table" and opts.notify_empty == false)
  vim.fn.setqflist({}, " ", {
    title = title,
    items = items,
  })
  if #items == 0 then
    if should_notify_empty then
      notify(title .. ": no results", vim.log.levels.INFO)
    end
    return
  end
  if should_open then
    vim.cmd("copen")
  end
end

local function register_lsp_commands()
  if state.commands_registered then
    return
  end
  state.commands_registered = true

  if not (vim.lsp and type(vim.lsp) == "table") then
    return
  end

  vim.lsp.commands = vim.lsp.commands or {}
  vim.lsp.commands["omni.showReferences"] = function(command, _)
    local arguments = command.arguments or {}
    local uri = arguments[1]
    local locations = arguments[3]
    local fallback = nil
    if type(uri) == "string" then
      fallback = vim.fn.fnamemodify(vim.uri_to_fname(uri), ":t")
    end
    local items = {}
    for _, location in ipairs(locations or {}) do
      local item = location_to_qf_item(location, fallback)
      if item then
        table.insert(items, item)
      end
    end
    set_quickfix(items, "Omni codelens references")
  end
end

function M.register_commands()
  register_lsp_commands()
end

local function collect_call_hierarchy_items(responses)
  local items = {}
  for _, response in pairs(responses or {}) do
    for _, item in ipairs(response.result or {}) do
      table.insert(items, item)
    end
  end
  return items
end

function M.document_symbols()
  local bufnr = current_bufnr()
  local responses = request_sync(bufnr, "textDocument/documentSymbol", text_document_params(bufnr))
  if not responses then
    return false
  end

  local items = {}
  for _, response in pairs(responses) do
    if type(response.result) == "table" then
      collect_document_symbol_items(response.result, items, nil)
    end
  end
  set_quickfix(items, "Omni document symbols")
  return true
end

function M.workspace_symbols(query)
  local symbol_query = query
  if not symbol_query or symbol_query == "" then
    symbol_query = vim.fn.input("Omni workspace symbol query: ")
  end
  if not symbol_query or symbol_query == "" then
    return false
  end

  local bufnr = current_bufnr()
  local responses = request_sync(bufnr, "workspace/symbol", { query = symbol_query })
  if not responses then
    return false
  end

  local items = {}
  for _, response in pairs(responses) do
    for _, symbol in ipairs(response.result or {}) do
      local item = symbol_item_to_qf(symbol, nil)
      if item then
        table.insert(items, item)
      end
    end
  end
  set_quickfix(items, "Omni workspace symbols: " .. symbol_query)
  return true
end

local function fetch_document_diagnostics_items()
  local bufnr = current_bufnr()
  local uri = vim.uri_from_bufnr(bufnr)
  local cached = state.pull_diagnostics.document[uri]
  local responses = request_sync(bufnr, "textDocument/diagnostic", {
    textDocument = { uri = uri },
    previousResultId = cached and cached.result_id or nil,
  })
  if not responses then
    return nil
  end

  local diagnostics = cached and cached.items or {}
  for _, response in pairs(responses) do
    local result = response.result or {}
    if result.kind == "full" then
      diagnostics = result.items or {}
      state.pull_diagnostics.document[uri] = {
        result_id = result.resultId,
        items = diagnostics,
      }
    elseif result.kind ~= "unchanged" then
      diagnostics = {}
      state.pull_diagnostics.document[uri] = nil
    end
  end

  local items = {}
  for _, diagnostic in ipairs(diagnostics or {}) do
    local item = diagnostic_to_qf_item(uri, diagnostic)
    if item then
      table.insert(items, item)
    end
  end
  return items
end

function M.document_diagnostics(opts)
  local items = fetch_document_diagnostics_items()
  if not items then
    return false
  end
  set_quickfix(items, "Omni pull diagnostics", opts)
  return true
end

function M.reset_document_diagnostics(uri)
  local target_uri = uri
  if type(target_uri) ~= "string" or target_uri == "" then
    target_uri = vim.uri_from_bufnr(current_bufnr())
  end
  state.pull_diagnostics.document[target_uri] = nil
  return true
end

local function fetch_workspace_diagnostics_items()
  local bufnr = current_bufnr()
  local workspace_key = workspace_cache_key(bufnr)
  local workspace_cache = state.pull_diagnostics.workspace[workspace_key] or {}
  local previous = {}
  for uri, entry in pairs(workspace_cache) do
    if type(entry) == "table" and type(entry.result_id) == "string" and entry.result_id ~= "" then
      table.insert(previous, { uri = uri, value = entry.result_id })
    end
  end
  table.sort(previous, function(left, right)
    return left.uri < right.uri
  end)
  local responses = request_sync(bufnr, "workspace/diagnostic", {
    previousResultIds = previous,
  })
  if not responses then
    return nil
  end

  local current = vim.deepcopy(workspace_cache)
  local ordered_items = {}
  for _, response in pairs(responses) do
    local result = response.result or {}
    for _, report in ipairs(result.items or {}) do
      local uri = report.uri
      if type(uri) ~= "string" then
        goto continue
      end
      if report.kind == "full" then
        current[uri] = {
          result_id = report.resultId,
          items = report.items or {},
        }
      elseif report.kind ~= "unchanged" then
        current[uri] = nil
      end
      if current[uri] then
        ordered_items[uri] = current[uri].items or {}
      end
      ::continue::
    end
  end
  state.pull_diagnostics.workspace[workspace_key] = current

  local items = {}
  local uris = vim.tbl_keys(ordered_items)
  table.sort(uris)
  for _, uri in ipairs(uris) do
    for _, diagnostic in ipairs(ordered_items[uri] or {}) do
      local item = diagnostic_to_qf_item(uri, diagnostic)
      if item then
        table.insert(items, item)
      end
    end
  end
  return items
end

function M.workspace_diagnostics(opts)
  local items = fetch_workspace_diagnostics_items()
  if not items then
    return false
  end
  set_quickfix(items, "Omni workspace diagnostics", opts)
  return true
end

function M.reset_workspace_diagnostics(bufnr, client_id)
  state.pull_diagnostics.workspace[workspace_cache_key(bufnr or current_bufnr(), client_id)] = nil
  return true
end

function M.reset_all_diagnostics(uri)
  M.reset_document_diagnostics(uri)
  M.reset_workspace_diagnostics()
  return true
end

local function append_unique_items(target, seen, items)
  for _, item in ipairs(items or {}) do
    local key = table.concat({
      item.filename or "",
      tostring(item.lnum or 0),
      tostring(item.col or 0),
      item.text or "",
    }, "\x1f")
    if not seen[key] then
      seen[key] = true
      table.insert(target, item)
    end
  end
end

function M.all_diagnostics(opts)
  local document_items = fetch_document_diagnostics_items()
  if not document_items then
    return false
  end
  local workspace_items = fetch_workspace_diagnostics_items()
  if not workspace_items then
    return false
  end

  local items = {}
  local seen = {}
  append_unique_items(items, seen, document_items)
  append_unique_items(items, seen, workspace_items)
  set_quickfix(items, "Omni all pull diagnostics", opts)
  return true
end

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

function M.open_link()
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

function M.next_link()
  return jump_to_link(1)
end

function M.prev_link()
  return jump_to_link(-1)
end

function M.document_links()
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

function M.references(include_declaration)
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

function M.definitions()
  return location_request("textDocument/definition", "Omni definitions")
end

function M.declarations()
  return location_request("textDocument/declaration", "Omni declarations")
end

function M.implementations()
  return location_request("textDocument/implementation", "Omni implementations")
end

function M.type_definitions()
  return location_request("textDocument/typeDefinition", "Omni type definitions")
end

function M.incoming_calls()
  return call_hierarchy_request(
    "callHierarchy/incomingCalls",
    "Omni incoming calls",
    function(entry, prepared_item)
      return call_hierarchy_callsite_qf(entry.from or {}, prepared_item or {}, entry.fromRanges or {})
    end
  )
end

function M.outgoing_calls()
  return call_hierarchy_request(
    "callHierarchy/outgoingCalls",
    "Omni outgoing calls",
    function(entry, prepared_item)
      return call_hierarchy_callsite_qf(prepared_item or {}, entry.to or {}, entry.fromRanges or {})
    end
  )
end

function M.apply_folds()
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

function M.expand_selection()
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

function M.info(config)
  return {
    server_path = server_path(config),
    cmd = server_cmd(config),
    root_markers = root_markers(config),
  }
end

return M
