local repl = require("omni.repl")
local lsp = require("omni.lsp")
local treesitter = require("omni.treesitter")
local formatter = require("omni.formatter")

local M = {}

local defaults = {
  cmd = { "omni", "--repl", "--json" },
  repl = {
    mode = "json",
  },
  eval = {
    mode = "json",
    cmd = { "omni", "--eval", "--json" },
    fallback_to_repl = true,
    annotations = {
      enabled = false,
      max_length = 160,
      labels = {
        form = true,
        root = true,
        call = true,
        block = true,
        declaration = true,
        line = true,
      },
      hl = {
        ok = "DiagnosticOk",
        error = "DiagnosticError",
      },
    },
  },
  output = {
    name = "Omni REPL",
    split = "botright 12split",
    auto_scroll = true,
  },
  auto_start = true,
  lsp = {
    auto_setup = false,
    enable = true,
    cmd = nil,
    server_path = nil,
    root_markers = { "project.json", ".git" },
    highlights = {
      auto_refresh = false,
      refresh_events = { "CursorHold", "CursorHoldI" },
      clear_events = { "CursorMoved", "InsertEnter", "BufLeave" },
    },
    codelens = {
      auto_refresh = false,
      events = { "BufEnter", "InsertLeave" },
    },
    pull_diagnostics = {
      auto_refresh = false,
      events = { "BufEnter", "InsertLeave" },
      auto_open = false,
      auto_notify_empty = false,
      workspace_auto_refresh = false,
      workspace_events = { "BufWritePost" },
      workspace_auto_open = false,
      workspace_auto_notify_empty = false,
      all_auto_refresh = false,
      all_events = { "BufEnter", "BufWritePost" },
      all_auto_open = false,
      all_auto_notify_empty = false,
    },
    mappings = true,
    keys = {
      hover = "K",
      definition = "gd",
      definition_list = "<localleader>lg",
      declaration = "gD",
      declaration_list = "<localleader>lG",
      implementation = "gi",
      implementation_list = "<localleader>lI",
      type_definition = "<localleader>lt",
      type_definition_list = "<localleader>lT",
      incoming_calls = "<localleader>lC",
      outgoing_calls = "<localleader>lc",
      references = "grr",
      references_list = "<localleader>lR",
      rename = "<localleader>lr",
      code_action = "<localleader>la",
      format = "<localleader>lf",
      signature_help = "<localleader>ls",
      document_symbols = "<localleader>lq",
      workspace_symbols = "<localleader>lQ",
      open_link = "<localleader>lo",
      document_links = "<localleader>lO",
      next_link = "]o",
      prev_link = "[o",
      highlight = "<localleader>lh",
      clear_highlights = "<localleader>lH",
      diagnostics = "<localleader>ld",
      document_diagnostics = "<localleader>lp",
      workspace_diagnostics = "<localleader>lP",
      all_diagnostics = "<localleader>lA",
      diagnostic_next = "]d",
      diagnostic_prev = "[d",
      code_lens_refresh = "<localleader>ll",
      code_lens_run = "<localleader>lL",
      inlay_hints = "<localleader>li",
      folds = "<localleader>lz",
      expand_selection = "<localleader>lv",
    },
  },
  formatter = {
    name = "omni_fmt",
    cmd = { "omni", "--fmt", "--write", "$FILENAME" },
  },
  treesitter = {
    register = true,
    append_runtimepath = true,
    grammar_dir = nil,
    repo_root = nil,
  },
  textobjects = {
    enabled = true,
    keys = {
      form_outer = "af",
      form_inner = "if",
      root_outer = "ar",
      root_inner = "ir",
      declaration_outer = "ad",
      declaration_inner = "id",
      call_outer = "ac",
      call_inner = "ic",
      block_outer = "aB",
      block_inner = "iB",
    },
  },
  mappings = true,
  keys = {
    start = "<localleader>rs",
    restart = "<localleader>rr",
    open = "<localleader>ro",
    clear = "<localleader>rc",
    select_decl = "<localleader>sd",
    select_call = "<localleader>sc",
    select_block = "<localleader>sb",
    select_form = "<localleader>sf",
    select_root = "<localleader>sr",
    eval_decl = "<localleader>ed",
    eval_call = "<localleader>ec",
    eval_block = "<localleader>ep",
    eval_operator = "<localleader>eo",
    eval_form = "<localleader>ef",
    eval_root = "<localleader>er",
    eval_line = "<localleader>el",
    eval_selection = "<localleader>es",
    eval_buffer = "<localleader>eb",
  },
}

local config = vim.deepcopy(defaults)
local commands_created = false
local operatorfunc_installed = false
local codelens_augroup = nil
local highlight_augroup = nil
local pull_diagnostics_augroup = nil
local workspace_pull_diagnostics_augroup = nil
local all_pull_diagnostics_augroup = nil
local pull_diagnostics_cache_augroup = nil

local function notify(message, level)
  vim.notify(message, level or vim.log.levels.INFO, { title = "omni.nvim" })
end

local function install_operatorfunc()
  if operatorfunc_installed then
    return
  end
  operatorfunc_installed = true
  _G.__omni_nvim_eval_operator = function(motion_type)
    repl.send_operator(config, motion_type)
  end
end

local function start_eval_operator()
  install_operatorfunc()
  vim.go.operatorfunc = "v:lua.__omni_nvim_eval_operator"
  return "g@"
end

local function call_lsp(method, opts)
  if not (vim.lsp and vim.lsp.buf and type(vim.lsp.buf[method]) == "function") then
    notify("Neovim LSP buffer API not available", vim.log.levels.WARN)
    return
  end
  vim.lsp.buf[method](opts)
end

local function visual_lsp_range()
  local start_pos = vim.fn.getpos("'<")
  local end_pos = vim.fn.getpos("'>")
  if start_pos[2] == 0 or end_pos[2] == 0 then
    return nil
  end

  return {
    ["start"] = {
      line = start_pos[2] - 1,
      character = math.max(0, start_pos[3] - 1),
    },
    ["end"] = {
      line = end_pos[2] - 1,
      character = math.max(0, end_pos[3]),
    },
  }
end

local function command_lsp_range(opts)
  if type(opts) ~= "table" then
    return nil
  end

  if not opts.range or opts.range == 0 or not opts.line1 or not opts.line2 then
    return nil
  end

  local end_line_text = vim.api.nvim_buf_get_lines(0, opts.line2 - 1, opts.line2, false)[1] or ""
  return {
    ["start"] = {
      line = math.max(0, opts.line1 - 1),
      character = 0,
    },
    ["end"] = {
      line = math.max(0, opts.line2 - 1),
      character = #end_line_text,
    },
  }
end

local function format_buffer(range)
  local opts = { async = true }
  if range then
    opts.range = range
  end
  call_lsp("format", opts)
end

local function code_action(range)
  local opts = nil
  if range then
    opts = { range = range }
  end
  call_lsp("code_action", opts)
end

local function diagnostics_supported()
  return vim.diagnostic and type(vim.diagnostic.setloclist) == "function"
end

local function open_diagnostics()
  if not diagnostics_supported() then
    notify("Neovim diagnostics API not available", vim.log.levels.WARN)
    return
  end
  vim.diagnostic.setloclist({ open = true })
end

local function goto_next_diagnostic()
  if not (vim.diagnostic and type(vim.diagnostic.goto_next) == "function") then
    notify("Neovim diagnostics API not available", vim.log.levels.WARN)
    return
  end
  vim.diagnostic.goto_next()
end

local function goto_prev_diagnostic()
  if not (vim.diagnostic and type(vim.diagnostic.goto_prev) == "function") then
    notify("Neovim diagnostics API not available", vim.log.levels.WARN)
    return
  end
  vim.diagnostic.goto_prev()
end

local function inlay_hints_supported()
  return vim.lsp and vim.lsp.inlay_hint and type(vim.lsp.inlay_hint.enable) == "function"
end

local function inlay_hints_enabled(bufnr)
  if not (vim.lsp and vim.lsp.inlay_hint and type(vim.lsp.inlay_hint.is_enabled) == "function") then
    return false
  end
  local ok, enabled = pcall(vim.lsp.inlay_hint.is_enabled, { bufnr = bufnr })
  if ok then
    return enabled == true
  end
  ok, enabled = pcall(vim.lsp.inlay_hint.is_enabled, bufnr)
  if ok then
    return enabled == true
  end
  return false
end

local function set_inlay_hints_enabled(enabled)
  if not inlay_hints_supported() then
    notify("Neovim inlay hints API not available", vim.log.levels.WARN)
    return
  end

  local bufnr = vim.api.nvim_get_current_buf()
  local ok = pcall(vim.lsp.inlay_hint.enable, enabled, { bufnr = bufnr })
  if not ok then
    ok = pcall(vim.lsp.inlay_hint.enable, bufnr, enabled)
  end
  if not ok then
    notify("Failed to update inlay hints", vim.log.levels.WARN)
    return
  end

  notify(enabled and "Omni inlay hints enabled" or "Omni inlay hints disabled", vim.log.levels.INFO)
end

local function toggle_inlay_hints()
  set_inlay_hints_enabled(not inlay_hints_enabled(vim.api.nvim_get_current_buf()))
end

local function code_lens_supported()
  return vim.lsp and vim.lsp.codelens and type(vim.lsp.codelens.refresh) == "function"
end

local function document_highlight_supported()
  return vim.lsp and vim.lsp.buf and type(vim.lsp.buf.document_highlight) == "function"
end

local function clear_references_supported()
  return vim.lsp and vim.lsp.buf and type(vim.lsp.buf.clear_references) == "function"
end

local function refresh_document_highlight()
  if not document_highlight_supported() then
    notify("Neovim document highlight API not available", vim.log.levels.WARN)
    return
  end
  vim.lsp.buf.document_highlight()
end

local function clear_document_highlight()
  if not clear_references_supported() then
    notify("Neovim clear references API not available", vim.log.levels.WARN)
    return
  end
  vim.lsp.buf.clear_references()
end

local function refresh_code_lens()
  if not code_lens_supported() then
    notify("Neovim codelens API not available", vim.log.levels.WARN)
    return
  end
  vim.lsp.codelens.refresh()
end

local function run_code_lens()
  if not (vim.lsp and vim.lsp.codelens and type(vim.lsp.codelens.run) == "function") then
    notify("Neovim codelens API not available", vim.log.levels.WARN)
    return
  end
  vim.lsp.codelens.run()
end

local function codelens_auto_refresh_enabled()
  local lsp_config = config.lsp or {}
  local codelens_config = lsp_config.codelens or {}
  return codelens_config.auto_refresh == true
end

local function highlight_auto_refresh_enabled()
  local lsp_config = config.lsp or {}
  local highlights_config = lsp_config.highlights or {}
  return highlights_config.auto_refresh == true
end

local function pull_diagnostics_auto_refresh_enabled()
  local lsp_config = config.lsp or {}
  local pull_config = lsp_config.pull_diagnostics or {}
  return pull_config.auto_refresh == true
end

local function workspace_pull_diagnostics_auto_refresh_enabled()
  local lsp_config = config.lsp or {}
  local pull_config = lsp_config.pull_diagnostics or {}
  return pull_config.workspace_auto_refresh == true
end

local function all_pull_diagnostics_auto_refresh_enabled()
  local lsp_config = config.lsp or {}
  local pull_config = lsp_config.pull_diagnostics or {}
  return pull_config.all_auto_refresh == true
end

local function codelens_refresh_events()
  local lsp_config = config.lsp or {}
  local codelens_config = lsp_config.codelens or {}
  local events = codelens_config.events
  if type(events) == "table" and not vim.tbl_isempty(events) then
    return events
  end
  return { "BufEnter", "InsertLeave" }
end

local function highlight_refresh_events()
  local lsp_config = config.lsp or {}
  local highlights_config = lsp_config.highlights or {}
  local events = highlights_config.refresh_events
  if type(events) == "table" and not vim.tbl_isempty(events) then
    return events
  end
  return { "CursorHold", "CursorHoldI" }
end

local function highlight_clear_events()
  local lsp_config = config.lsp or {}
  local highlights_config = lsp_config.highlights or {}
  local events = highlights_config.clear_events
  if type(events) == "table" and not vim.tbl_isempty(events) then
    return events
  end
  return { "CursorMoved", "InsertEnter", "BufLeave" }
end

local function pull_diagnostics_refresh_events()
  local lsp_config = config.lsp or {}
  local pull_config = lsp_config.pull_diagnostics or {}
  local events = pull_config.events
  if type(events) == "table" and not vim.tbl_isempty(events) then
    return events
  end
  return { "BufEnter", "InsertLeave" }
end

local function workspace_pull_diagnostics_refresh_events()
  local lsp_config = config.lsp or {}
  local pull_config = lsp_config.pull_diagnostics or {}
  local events = pull_config.workspace_events
  if type(events) == "table" and not vim.tbl_isempty(events) then
    return events
  end
  return { "BufWritePost" }
end

local function all_pull_diagnostics_refresh_events()
  local lsp_config = config.lsp or {}
  local pull_config = lsp_config.pull_diagnostics or {}
  local events = pull_config.all_events
  if type(events) == "table" and not vim.tbl_isempty(events) then
    return events
  end
  return { "BufEnter", "BufWritePost" }
end

local function pull_diagnostics_auto_refresh_opts()
  local lsp_config = config.lsp or {}
  local pull_config = lsp_config.pull_diagnostics or {}
  return {
    open = pull_config.auto_open == true,
    notify_empty = pull_config.auto_notify_empty == true,
  }
end

local function workspace_pull_diagnostics_auto_refresh_opts()
  local lsp_config = config.lsp or {}
  local pull_config = lsp_config.pull_diagnostics or {}
  return {
    open = pull_config.workspace_auto_open == true,
    notify_empty = pull_config.workspace_auto_notify_empty == true,
  }
end

local function all_pull_diagnostics_auto_refresh_opts()
  local lsp_config = config.lsp or {}
  local pull_config = lsp_config.pull_diagnostics or {}
  return {
    open = pull_config.all_auto_open == true,
    notify_empty = pull_config.all_auto_notify_empty == true,
  }
end

local function ensure_codelens_autocmds(bufnr)
  if not codelens_auto_refresh_enabled() then
    return
  end
  if vim.b[bufnr].omni_nvim_codelens_autocmd then
    return
  end
  vim.b[bufnr].omni_nvim_codelens_autocmd = true
  codelens_augroup = codelens_augroup or vim.api.nvim_create_augroup("OmniNvimCodeLens", { clear = false })
  vim.api.nvim_create_autocmd(codelens_refresh_events(), {
    group = codelens_augroup,
    buffer = bufnr,
    callback = function()
      refresh_code_lens()
    end,
    desc = "Omni codelens auto refresh",
  })
end

local function ensure_highlight_autocmds(bufnr)
  if not highlight_auto_refresh_enabled() then
    return
  end
  if vim.b[bufnr].omni_nvim_highlight_autocmd then
    return
  end
  vim.b[bufnr].omni_nvim_highlight_autocmd = true
  highlight_augroup = highlight_augroup or vim.api.nvim_create_augroup("OmniNvimDocumentHighlight", { clear = false })
  vim.api.nvim_create_autocmd(highlight_refresh_events(), {
    group = highlight_augroup,
    buffer = bufnr,
    callback = function()
      refresh_document_highlight()
    end,
    desc = "Omni document highlight refresh",
  })
  vim.api.nvim_create_autocmd(highlight_clear_events(), {
    group = highlight_augroup,
    buffer = bufnr,
    callback = function()
      clear_document_highlight()
    end,
    desc = "Omni document highlight clear",
  })
end

local function ensure_pull_diagnostics_autocmds(bufnr)
  if not pull_diagnostics_auto_refresh_enabled() then
    return
  end
  if vim.b[bufnr].omni_nvim_pull_diagnostics_autocmd then
    return
  end
  vim.b[bufnr].omni_nvim_pull_diagnostics_autocmd = true
  pull_diagnostics_augroup = pull_diagnostics_augroup or vim.api.nvim_create_augroup("OmniNvimPullDiagnostics", { clear = false })
  vim.api.nvim_create_autocmd(pull_diagnostics_refresh_events(), {
    group = pull_diagnostics_augroup,
    buffer = bufnr,
    callback = function()
      lsp.document_diagnostics(pull_diagnostics_auto_refresh_opts())
    end,
    desc = "Omni pull diagnostics auto refresh",
  })
end

local function ensure_workspace_pull_diagnostics_autocmds(bufnr)
  if not workspace_pull_diagnostics_auto_refresh_enabled() then
    return
  end
  if vim.b[bufnr].omni_nvim_workspace_pull_diagnostics_autocmd then
    return
  end
  vim.b[bufnr].omni_nvim_workspace_pull_diagnostics_autocmd = true
  workspace_pull_diagnostics_augroup = workspace_pull_diagnostics_augroup or vim.api.nvim_create_augroup("OmniNvimWorkspacePullDiagnostics", { clear = false })
  vim.api.nvim_create_autocmd(workspace_pull_diagnostics_refresh_events(), {
    group = workspace_pull_diagnostics_augroup,
    buffer = bufnr,
    callback = function()
      lsp.workspace_diagnostics(workspace_pull_diagnostics_auto_refresh_opts())
    end,
    desc = "Omni workspace pull diagnostics auto refresh",
  })
end

local function ensure_all_pull_diagnostics_autocmds(bufnr)
  if not all_pull_diagnostics_auto_refresh_enabled() then
    return
  end
  if vim.b[bufnr].omni_nvim_all_pull_diagnostics_autocmd then
    return
  end
  vim.b[bufnr].omni_nvim_all_pull_diagnostics_autocmd = true
  all_pull_diagnostics_augroup = all_pull_diagnostics_augroup
    or vim.api.nvim_create_augroup("OmniNvimAllPullDiagnostics", { clear = false })
  vim.api.nvim_create_autocmd(all_pull_diagnostics_refresh_events(), {
    group = all_pull_diagnostics_augroup,
    buffer = bufnr,
    callback = function()
      lsp.all_diagnostics(all_pull_diagnostics_auto_refresh_opts())
    end,
    desc = "Omni all pull diagnostics auto refresh",
  })
end

local function ensure_pull_diagnostics_cache_autocmds(bufnr)
  if vim.b[bufnr].omni_nvim_pull_diagnostics_cache_autocmd then
    return
  end
  vim.b[bufnr].omni_nvim_pull_diagnostics_cache_autocmd = true
  pull_diagnostics_cache_augroup = pull_diagnostics_cache_augroup
    or vim.api.nvim_create_augroup("OmniNvimPullDiagnosticsCache", { clear = false })
  vim.api.nvim_create_autocmd({ "BufDelete", "BufWipeout" }, {
    group = pull_diagnostics_cache_augroup,
    buffer = bufnr,
    callback = function(args)
      lsp.reset_document_diagnostics(vim.uri_from_bufnr(args.buf))
    end,
    desc = "Omni pull diagnostics cache cleanup",
  })
  vim.api.nvim_create_autocmd("LspDetach", {
    group = pull_diagnostics_cache_augroup,
    buffer = bufnr,
    callback = function(args)
      local client = nil
      local client_id = args.data and args.data.client_id or nil
      if client_id and vim.lsp and type(vim.lsp.get_client_by_id) == "function" then
        client = vim.lsp.get_client_by_id(client_id)
      end
      if not client or client.name ~= "omni_lsp" then
        return
      end
      lsp.reset_document_diagnostics(vim.uri_from_bufnr(args.buf))
      lsp.reset_workspace_diagnostics()
    end,
    desc = "Omni pull diagnostics cache reset on detach",
  })
end

local function create_commands()
  if commands_created then
    return
  end
  commands_created = true

  vim.api.nvim_create_user_command("OmniReplStart", function()
    repl.start(config)
  end, {})

  vim.api.nvim_create_user_command("OmniReplStop", function()
    repl.stop()
  end, {})

  vim.api.nvim_create_user_command("OmniReplRestart", function()
    repl.restart(config)
  end, {})

  vim.api.nvim_create_user_command("OmniReplOpen", function()
    repl.open_output(config)
  end, {})

  vim.api.nvim_create_user_command("OmniReplClear", function()
    repl.clear_output()
  end, {})

  vim.api.nvim_create_user_command("OmniTreesitterRegister", function()
    if treesitter.register(config) then
      notify("registered Omni Tree-sitter parser config", vim.log.levels.INFO)
    else
      notify("nvim-treesitter not available or Omni grammar path missing", vim.log.levels.WARN)
    end
  end, {})

  vim.api.nvim_create_user_command("OmniLspSetup", function()
    local ok, backend = lsp.setup(config)
    if ok then
      notify("registered Omni LSP via " .. backend, vim.log.levels.INFO)
    else
      notify(backend, vim.log.levels.WARN)
    end
  end, {})

  vim.api.nvim_create_user_command("OmniConformSetupSpec", function()
    print(vim.inspect(formatter.conform_setup_spec(config)))
  end, {})

  vim.api.nvim_create_user_command("OmniLspHover", function()
    call_lsp("hover")
  end, {})

  vim.api.nvim_create_user_command("OmniLspDefinition", function()
    call_lsp("definition")
  end, {})

  vim.api.nvim_create_user_command("OmniLspDefinitionsList", function()
    lsp.definitions()
  end, {})

  vim.api.nvim_create_user_command("OmniLspDeclaration", function()
    call_lsp("declaration")
  end, {})

  vim.api.nvim_create_user_command("OmniLspDeclarationsList", function()
    lsp.declarations()
  end, {})

  vim.api.nvim_create_user_command("OmniLspImplementation", function()
    call_lsp("implementation")
  end, {})

  vim.api.nvim_create_user_command("OmniLspImplementationsList", function()
    lsp.implementations()
  end, {})

  vim.api.nvim_create_user_command("OmniLspTypeDefinition", function()
    call_lsp("type_definition")
  end, {})

  vim.api.nvim_create_user_command("OmniLspTypeDefinitionsList", function()
    lsp.type_definitions()
  end, {})

  vim.api.nvim_create_user_command("OmniLspIncomingCallsList", function()
    lsp.incoming_calls()
  end, {})

  vim.api.nvim_create_user_command("OmniLspOutgoingCallsList", function()
    lsp.outgoing_calls()
  end, {})

  vim.api.nvim_create_user_command("OmniLspReferences", function()
    call_lsp("references")
  end, {})

  vim.api.nvim_create_user_command("OmniLspReferencesList", function()
    lsp.references(true)
  end, {})

  vim.api.nvim_create_user_command("OmniLspRename", function()
    call_lsp("rename")
  end, {})

  vim.api.nvim_create_user_command("OmniLspCodeAction", function(opts)
    code_action(command_lsp_range(opts))
  end, { range = true })

  vim.api.nvim_create_user_command("OmniLspFormat", function(opts)
    format_buffer(command_lsp_range(opts))
  end, { range = true })

  vim.api.nvim_create_user_command("OmniLspSignatureHelp", function()
    call_lsp("signature_help")
  end, {})

  vim.api.nvim_create_user_command("OmniLspDocumentSymbols", function()
    lsp.document_symbols()
  end, {})

  vim.api.nvim_create_user_command("OmniLspWorkspaceSymbols", function(opts)
    lsp.workspace_symbols(opts.args)
  end, { nargs = "?" })

  vim.api.nvim_create_user_command("OmniLspOpenLink", function()
    lsp.open_link()
  end, {})

  vim.api.nvim_create_user_command("OmniLspDocumentLinks", function()
    lsp.document_links()
  end, {})

  vim.api.nvim_create_user_command("OmniLspNextLink", function()
    lsp.next_link()
  end, {})

  vim.api.nvim_create_user_command("OmniLspPrevLink", function()
    lsp.prev_link()
  end, {})

  vim.api.nvim_create_user_command("OmniLspDocumentHighlight", function()
    refresh_document_highlight()
  end, {})

  vim.api.nvim_create_user_command("OmniLspClearReferences", function()
    clear_document_highlight()
  end, {})

  vim.api.nvim_create_user_command("OmniLspDiagnostics", function()
    open_diagnostics()
  end, {})

  vim.api.nvim_create_user_command("OmniLspDocumentDiagnostics", function()
    lsp.document_diagnostics()
  end, {})

  vim.api.nvim_create_user_command("OmniLspWorkspaceDiagnostics", function()
    lsp.workspace_diagnostics()
  end, {})

  vim.api.nvim_create_user_command("OmniLspAllDiagnostics", function()
    lsp.all_diagnostics()
  end, {})

  vim.api.nvim_create_user_command("OmniLspAllDiagnosticsReset", function()
    lsp.reset_all_diagnostics()
    notify("reset Omni pull diagnostics caches for current buffer and workspace", vim.log.levels.INFO)
  end, {})

  vim.api.nvim_create_user_command("OmniLspDocumentDiagnosticsReset", function()
    lsp.reset_document_diagnostics()
    notify("reset Omni pull diagnostics cache for current buffer", vim.log.levels.INFO)
  end, {})

  vim.api.nvim_create_user_command("OmniLspWorkspaceDiagnosticsReset", function()
    lsp.reset_workspace_diagnostics()
    notify("reset Omni workspace pull diagnostics cache", vim.log.levels.INFO)
  end, {})

  vim.api.nvim_create_user_command("OmniLspNextDiagnostic", function()
    goto_next_diagnostic()
  end, {})

  vim.api.nvim_create_user_command("OmniLspPrevDiagnostic", function()
    goto_prev_diagnostic()
  end, {})

  vim.api.nvim_create_user_command("OmniLspCodeLensRefresh", function()
    refresh_code_lens()
  end, {})

  vim.api.nvim_create_user_command("OmniLspCodeLensRun", function()
    run_code_lens()
  end, {})

  vim.api.nvim_create_user_command("OmniLspInlayHintsEnable", function()
    set_inlay_hints_enabled(true)
  end, {})

  vim.api.nvim_create_user_command("OmniLspInlayHintsDisable", function()
    set_inlay_hints_enabled(false)
  end, {})

  vim.api.nvim_create_user_command("OmniLspInlayHintsToggle", function()
    toggle_inlay_hints()
  end, {})

  vim.api.nvim_create_user_command("OmniLspRefreshFolds", function()
    lsp.apply_folds()
  end, {})

  vim.api.nvim_create_user_command("OmniLspExpandSelection", function()
    lsp.expand_selection()
  end, {})

  vim.api.nvim_create_user_command("OmniEvalForm", function()
    repl.send_current_form(config)
  end, {})

  vim.api.nvim_create_user_command("OmniSelectForm", function()
    repl.select_current_form()
  end, {})

  vim.api.nvim_create_user_command("OmniSelectRootForm", function()
    repl.select_root_form()
  end, {})

  vim.api.nvim_create_user_command("OmniSelectDecl", function()
    repl.select_current_declaration()
  end, {})

  vim.api.nvim_create_user_command("OmniSelectCall", function()
    repl.select_current_call()
  end, {})

  vim.api.nvim_create_user_command("OmniSelectBlock", function()
    repl.select_current_block()
  end, {})

  vim.api.nvim_create_user_command("OmniEvalDecl", function()
    repl.send_current_declaration(config)
  end, {})

  vim.api.nvim_create_user_command("OmniEvalCall", function()
    repl.send_current_call(config)
  end, {})

  vim.api.nvim_create_user_command("OmniEvalBlock", function()
    repl.send_current_block(config)
  end, {})

  vim.api.nvim_create_user_command("OmniEvalRootForm", function()
    repl.send_root_form(config)
  end, {})

  vim.api.nvim_create_user_command("OmniEvalLine", function()
    repl.send_current_line(config)
  end, {})

  vim.api.nvim_create_user_command("OmniEvalSelection", function()
    repl.send_selection(config)
  end, { range = true })

  vim.api.nvim_create_user_command("OmniEvalBuffer", function()
    repl.send_buffer(config)
  end, {})

  vim.api.nvim_create_user_command("OmniEvalOperator", function()
    notify("Omni eval operator active: enter a motion", vim.log.levels.INFO)
    install_operatorfunc()
    vim.go.operatorfunc = "v:lua.__omni_nvim_eval_operator"
    vim.api.nvim_feedkeys("g@", "n", false)
  end, {})
end

local function set_buffer_maps(bufnr)
  if not config.mappings then
    return
  end
  if vim.b[bufnr].omni_nvim_mapped then
    return
  end

  vim.b[bufnr].omni_nvim_mapped = true

  local map = function(lhs, rhs, desc)
    vim.keymap.set("n", lhs, rhs, {
      buffer = bufnr,
      silent = true,
      desc = desc,
    })
  end

  map(config.keys.start, function() repl.start(config) end, "Omni REPL start")
  map(config.keys.restart, function() repl.restart(config) end, "Omni REPL restart")
  map(config.keys.open, function() repl.open_output(config) end, "Omni REPL open transcript")
  map(config.keys.clear, function() repl.clear_output() end, "Omni REPL clear transcript")
  map(config.keys.select_decl, function() repl.select_current_declaration() end, "Omni select declaration")
  map(config.keys.select_call, function() repl.select_current_call() end, "Omni select call")
  map(config.keys.select_block, function() repl.select_current_block() end, "Omni select block")
  map(config.keys.select_form, function() repl.select_current_form() end, "Omni select form")
  map(config.keys.select_root, function() repl.select_root_form() end, "Omni select root form")
  map(config.keys.eval_decl, function() repl.send_current_declaration(config) end, "Omni eval declaration")
  map(config.keys.eval_call, function() repl.send_current_call(config) end, "Omni eval call")
  map(config.keys.eval_block, function() repl.send_current_block(config) end, "Omni eval block")
  map(config.keys.eval_form, function() repl.send_current_form(config) end, "Omni eval form")
  map(config.keys.eval_root, function() repl.send_root_form(config) end, "Omni eval root form")
  map(config.keys.eval_line, function() repl.send_current_line(config) end, "Omni eval line")
  map(config.keys.eval_buffer, function() repl.send_buffer(config) end, "Omni eval buffer")
  vim.keymap.set("n", config.keys.eval_operator, start_eval_operator, {
    buffer = bufnr,
    silent = true,
    expr = true,
    desc = "Omni eval operator",
  })

  vim.keymap.set("x", config.keys.eval_selection, function()
    repl.send_selection(config)
  end, {
    buffer = bufnr,
    silent = true,
    desc = "Omni eval selection",
  })

  if config.lsp and config.lsp.mappings ~= false then
    local lsp_keys = config.lsp.keys or {}
    map(lsp_keys.hover, function() call_lsp("hover") end, "Omni LSP hover")
    map(lsp_keys.definition, function() call_lsp("definition") end, "Omni LSP definition")
    map(lsp_keys.definition_list, function() lsp.definitions() end, "Omni LSP definitions list")
    map(lsp_keys.declaration, function() call_lsp("declaration") end, "Omni LSP declaration")
    map(lsp_keys.declaration_list, function() lsp.declarations() end, "Omni LSP declarations list")
    map(lsp_keys.implementation, function() call_lsp("implementation") end, "Omni LSP implementation")
    map(lsp_keys.implementation_list, function() lsp.implementations() end, "Omni LSP implementations list")
    map(lsp_keys.type_definition, function() call_lsp("type_definition") end, "Omni LSP type definition")
    map(lsp_keys.type_definition_list, function() lsp.type_definitions() end, "Omni LSP type definitions list")
    map(lsp_keys.incoming_calls, function() lsp.incoming_calls() end, "Omni LSP incoming calls list")
    map(lsp_keys.outgoing_calls, function() lsp.outgoing_calls() end, "Omni LSP outgoing calls list")
    map(lsp_keys.references, function() call_lsp("references") end, "Omni LSP references")
    map(lsp_keys.references_list, function() lsp.references(true) end, "Omni LSP references list")
    map(lsp_keys.rename, function() call_lsp("rename") end, "Omni LSP rename")
    map(lsp_keys.code_action, function() code_action(nil) end, "Omni LSP code action")
    map(lsp_keys.format, format_buffer, "Omni LSP format")
    map(lsp_keys.signature_help, function() call_lsp("signature_help") end, "Omni LSP signature help")
    map(lsp_keys.document_symbols, function() lsp.document_symbols() end, "Omni LSP document symbols")
    map(lsp_keys.workspace_symbols, function() lsp.workspace_symbols() end, "Omni LSP workspace symbols")
    map(lsp_keys.open_link, function() lsp.open_link() end, "Omni LSP open link")
    map(lsp_keys.document_links, function() lsp.document_links() end, "Omni LSP document links")
    map(lsp_keys.next_link, function() lsp.next_link() end, "Omni LSP next link")
    map(lsp_keys.prev_link, function() lsp.prev_link() end, "Omni LSP previous link")
    map(lsp_keys.highlight, refresh_document_highlight, "Omni LSP document highlight")
    map(lsp_keys.clear_highlights, clear_document_highlight, "Omni LSP clear references")
    map(lsp_keys.diagnostics, open_diagnostics, "Omni LSP diagnostics")
    map(lsp_keys.document_diagnostics, function() lsp.document_diagnostics() end, "Omni LSP document diagnostics")
    map(lsp_keys.workspace_diagnostics, function() lsp.workspace_diagnostics() end, "Omni LSP workspace diagnostics")
    map(lsp_keys.all_diagnostics, function() lsp.all_diagnostics() end, "Omni LSP all pull diagnostics")
    map(lsp_keys.diagnostic_next, goto_next_diagnostic, "Omni LSP next diagnostic")
    map(lsp_keys.diagnostic_prev, goto_prev_diagnostic, "Omni LSP previous diagnostic")
    map(lsp_keys.code_lens_refresh, refresh_code_lens, "Omni LSP codelens refresh")
    map(lsp_keys.code_lens_run, run_code_lens, "Omni LSP codelens run")
    map(lsp_keys.inlay_hints, toggle_inlay_hints, "Omni LSP inlay hints toggle")
    map(lsp_keys.folds, function() lsp.apply_folds() end, "Omni LSP refresh folds")
    vim.keymap.set({ "n", "x" }, lsp_keys.expand_selection, function()
      lsp.expand_selection()
    end, {
      buffer = bufnr,
      silent = true,
      desc = "Omni LSP expand selection",
    })
    vim.keymap.set("x", lsp_keys.code_action, function()
      code_action(visual_lsp_range())
    end, {
      buffer = bufnr,
      silent = true,
      desc = "Omni LSP code action",
    })
    vim.keymap.set("x", lsp_keys.format, function()
      format_buffer(visual_lsp_range())
    end, {
      buffer = bufnr,
      silent = true,
      desc = "Omni LSP format",
    })
  end

  if config.textobjects and config.textobjects.enabled ~= false then
    local textobject_map = function(lhs, rhs, desc)
      vim.keymap.set({ "o", "x" }, lhs, rhs, {
        buffer = bufnr,
        silent = true,
        desc = desc,
      })
    end

    local textobject_keys = config.textobjects.keys or {}
    textobject_map(textobject_keys.form_outer, repl.select_current_form, "Omni textobject form")
    textobject_map(textobject_keys.form_inner, repl.select_current_form_inner, "Omni textobject form inner")
    textobject_map(textobject_keys.root_outer, repl.select_root_form, "Omni textobject root form")
    textobject_map(textobject_keys.root_inner, repl.select_root_form_inner, "Omni textobject root form inner")
    textobject_map(textobject_keys.declaration_outer, repl.select_current_declaration, "Omni textobject declaration")
    textobject_map(textobject_keys.declaration_inner, repl.select_current_declaration_inner, "Omni textobject declaration inner")
    textobject_map(textobject_keys.call_outer, repl.select_current_call, "Omni textobject call")
    textobject_map(textobject_keys.call_inner, repl.select_current_call_inner, "Omni textobject call inner")
    textobject_map(textobject_keys.block_outer, repl.select_current_block, "Omni textobject block")
    textobject_map(textobject_keys.block_inner, repl.select_current_block_inner, "Omni textobject block inner")
  end
end

function M.setup(opts)
  config = vim.tbl_deep_extend("force", vim.deepcopy(defaults), opts or {})
  install_operatorfunc()
  create_commands()
  repl.set_notifier(notify)
  lsp.set_notifier(notify)
  lsp.register_commands()
  treesitter.ensure_queries(config)
  if config.treesitter and config.treesitter.register ~= false then
    treesitter.register(config)
  end
  if config.lsp and config.lsp.auto_setup then
    local ok = lsp.setup(config)
    if not ok then
      notify("Omni LSP auto-setup skipped", vim.log.levels.WARN)
    end
  end
  return config
end

function M.apply_buffer()
  create_commands()
  repl.set_notifier(notify)
  lsp.set_notifier(notify)
  lsp.register_commands()
  treesitter.ensure_queries(config)
  local bufnr = vim.api.nvim_get_current_buf()
  set_buffer_maps(bufnr)
  ensure_codelens_autocmds(bufnr)
  ensure_highlight_autocmds(bufnr)
  ensure_pull_diagnostics_autocmds(bufnr)
  ensure_workspace_pull_diagnostics_autocmds(bufnr)
  ensure_all_pull_diagnostics_autocmds(bufnr)
  ensure_pull_diagnostics_cache_autocmds(bufnr)
end

function M.config()
  return config
end

function M.conform_formatter()
  return formatter.conform_formatter(config)
end

function M.conform_formatters_by_ft()
  return formatter.conform_formatters_by_ft(config)
end

function M.conform_setup_spec()
  return formatter.conform_setup_spec(config)
end

return M
