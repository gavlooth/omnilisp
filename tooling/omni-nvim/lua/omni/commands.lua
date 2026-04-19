local M = {}

local commands_created = false

function M.create(ctx)
  if commands_created then
    return
  end
  commands_created = true

  local config = ctx.config
  local repl = ctx.repl
  local lsp = ctx.lsp
  local treesitter = ctx.treesitter
  local formatter = ctx.formatter
  local notify = ctx.notify
  local call_lsp = ctx.call_lsp
  local command_lsp_range = ctx.command_lsp_range
  local code_action = ctx.code_action
  local format_buffer = ctx.format_buffer
  local refresh_document_highlight = ctx.refresh_document_highlight
  local clear_document_highlight = ctx.clear_document_highlight
  local open_diagnostics = ctx.open_diagnostics
  local goto_next_diagnostic = ctx.goto_next_diagnostic
  local goto_prev_diagnostic = ctx.goto_prev_diagnostic
  local refresh_code_lens = ctx.refresh_code_lens
  local run_code_lens = ctx.run_code_lens
  local set_inlay_hints_enabled = ctx.set_inlay_hints_enabled
  local toggle_inlay_hints = ctx.toggle_inlay_hints
  local install_operatorfunc = ctx.install_operatorfunc

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

return M
