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

  local navigation = require("omni.lsp_navigation").new({
    current_bufnr = current_bufnr,
    location_to_qf_item = location_to_qf_item,
    notify = notify,
    request_sync = request_sync,
    text_document_params = text_document_params,
    cursor_position = cursor_position,
    collect_call_hierarchy_items = collect_call_hierarchy_items,
    set_quickfix = set_quickfix,
  })

M.open_link = navigation.open_link
M.next_link = navigation.next_link
M.prev_link = navigation.prev_link
M.document_links = navigation.document_links
M.references = navigation.references
M.definitions = navigation.definitions
M.declarations = navigation.declarations
M.implementations = navigation.implementations
M.type_definitions = navigation.type_definitions
M.incoming_calls = navigation.incoming_calls
M.outgoing_calls = navigation.outgoing_calls
M.apply_folds = navigation.apply_folds
M.expand_selection = navigation.expand_selection

function M.info(config)
  return {
    server_path = server_path(config),
    cmd = server_cmd(config),
    root_markers = root_markers(config),
  }
end

return M
