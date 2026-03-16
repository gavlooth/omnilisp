local M = {}

local state = {
  registered = false,
  queries_loaded = false,
  runtimepath_added = false,
}

local function path_exists(path)
  return type(path) == "string" and path ~= "" and vim.fn.isdirectory(path) == 1
end

local function module_root()
  local source = debug.getinfo(1, "S").source:sub(2)
  local omni_lua_dir = vim.fs.dirname(source)
  local plugin_root = vim.fs.dirname(vim.fs.dirname(omni_lua_dir))
  return plugin_root
end

local function grammar_dir(config)
  if config.treesitter and path_exists(config.treesitter.grammar_dir) then
    return config.treesitter.grammar_dir
  end

  local plugin_root = module_root()
  local default_dir = vim.fs.normalize(plugin_root .. "/../tree-sitter-omni")
  if path_exists(default_dir) then
    return default_dir
  end
  return nil
end

local function repo_root(config)
  if config.treesitter and path_exists(config.treesitter.repo_root) then
    return config.treesitter.repo_root
  end

  local plugin_root = module_root()
  local default_root = vim.fs.normalize(plugin_root .. "/../..")
  if path_exists(default_root) then
    return default_root
  end
  return nil
end

local function append_runtimepath(path)
  if not path or state.runtimepath_added then
    return
  end

  local current = vim.opt.runtimepath:get()
  for _, entry in ipairs(current) do
    if vim.fs.normalize(entry) == vim.fs.normalize(path) then
      state.runtimepath_added = true
      return
    end
  end

  vim.opt.runtimepath:append(path)
  state.runtimepath_added = true
end

local function read_query_file(path)
  if vim.fn.filereadable(path) ~= 1 then
    return nil
  end
  local lines = vim.fn.readfile(path)
  if not lines or vim.tbl_isempty(lines) then
    return ""
  end
  return table.concat(lines, "\n")
end

function M.ensure_queries(config)
  if state.queries_loaded then
    return true
  end

  local grammar = grammar_dir(config)
  if not grammar then
    return false
  end

  if not (config.treesitter and config.treesitter.append_runtimepath == false) then
    append_runtimepath(grammar)
  end

  local query_dir = vim.fs.normalize(grammar .. "/queries")
  if vim.fn.isdirectory(query_dir) ~= 1 then
    return false
  end

  local query_names = { "highlights", "injections", "locals", "textobjects", "folds" }
  for _, name in ipairs(query_names) do
    local ok_existing, existing = pcall(vim.treesitter.query.get, "omni", name)
    if not ok_existing or not existing then
      local query_text = read_query_file(query_dir .. "/" .. name .. ".scm")
      if query_text ~= nil then
        vim.treesitter.query.set("omni", name, query_text)
      end
    end
  end

  state.queries_loaded = true
  return true
end

function M.register(config)
  if state.registered then
    return true
  end

  local ok, parsers = pcall(require, "nvim-treesitter.parsers")
  if not ok then
    return false
  end

  local grammar = grammar_dir(config)
  local repo = repo_root(config)
  if not grammar or not repo then
    return false
  end

  if not (config.treesitter and config.treesitter.append_runtimepath == false) then
    append_runtimepath(grammar)
  end
  M.ensure_queries(config)

  local configs = parsers.get_parser_configs()
  configs.omni = {
    install_info = {
      url = repo,
      files = { "src/parser.c" },
      location = "tooling/tree-sitter-omni",
      generate_requires_npm = false,
      requires_generate_from_grammar = false,
    },
    filetype = "omni",
  }

  state.registered = true
  return true
end

function M.info(config)
  return {
    grammar_dir = grammar_dir(config),
    repo_root = repo_root(config),
    registered = state.registered,
    queries_loaded = state.queries_loaded,
    runtimepath_added = state.runtimepath_added,
  }
end

return M
