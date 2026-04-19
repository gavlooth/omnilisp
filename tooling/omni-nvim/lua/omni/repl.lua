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

local output = require("omni.repl.output").new(state)
local session = require("omni.repl.session").new(state, function(message, level)
  notify(message, level)
end, output)
local selectors = require("omni.repl.selection").new()
local selection = require("omni.repl.selection_commands").new(
  selectors,
  session.send_text,
  session.eval_once,
  function(message, level)
    notify(message, level)
  end
)

function M.set_notifier(fn)
  notify = fn
end

function M.open_output(config)
  return output.open_output(config)
end

function M.clear_output()
  return output.clear_output()
end

function M.start(config, opts)
  return session.start(config, opts)
end

function M.stop()
  return session.stop()
end

function M.restart(config)
  return session.restart(config)
end

M.send_current_form = selection.send_current_form
M.send_root_form = selection.send_root_form
M.send_current_line = selection.send_current_line
M.send_current_call = selection.send_current_call
M.send_current_block = selection.send_current_block
M.send_current_declaration = selection.send_current_declaration
M.select_current_form = selection.select_current_form
M.select_current_form_inner = selection.select_current_form_inner
M.select_root_form = selection.select_root_form
M.select_root_form_inner = selection.select_root_form_inner
M.select_current_call = selection.select_current_call
M.select_current_block = selection.select_current_block
M.select_current_declaration = selection.select_current_declaration
M.select_current_call_inner = selection.select_current_call_inner
M.select_current_block_inner = selection.select_current_block_inner
M.select_current_declaration_inner = selection.select_current_declaration_inner
M.send_buffer = selection.send_buffer
M.send_selection = selection.send_selection
M.send_operator = selection.send_operator

return M
