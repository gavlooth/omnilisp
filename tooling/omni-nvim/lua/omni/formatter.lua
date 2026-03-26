local M = {}

local function module_root()
  local source = debug.getinfo(1, "S").source:sub(2)
  local omni_lua_dir = vim.fs.dirname(source)
  return vim.fs.dirname(vim.fs.dirname(omni_lua_dir))
end

local function repo_root(config)
  local treesitter = config and config.treesitter
  if treesitter and type(treesitter.repo_root) == "string" and treesitter.repo_root ~= "" then
    return treesitter.repo_root
  end

  local plugin_root = module_root()
  return vim.fs.normalize(plugin_root .. "/../..")
end

local function formatter_command(config)
  local formatter = config and config.formatter
  if formatter and type(formatter.cmd) == "table" and #formatter.cmd > 0 then
    return vim.deepcopy(formatter.cmd)
  end
  return { "omni", "--fmt", "--write", "$FILENAME" }
end

function M.conform_formatter(config)
  local command = formatter_command(config)
  local executable = table.remove(command, 1)
  return {
    inherit = false,
    command = executable,
    args = command,
    stdin = false,
    cwd = repo_root(config),
  }
end

function M.conform_formatters_by_ft(config)
  local formatter = config and config.formatter
  local formatter_name = formatter and formatter.name or "omni_fmt"
  return {
    omni = { formatter_name },
  }
end

function M.conform_setup_spec(config)
  local formatter = config and config.formatter
  local formatter_name = formatter and formatter.name or "omni_fmt"
  return {
    formatters = {
      [formatter_name] = M.conform_formatter(config),
    },
    formatters_by_ft = M.conform_formatters_by_ft(config),
  }
end

return M
