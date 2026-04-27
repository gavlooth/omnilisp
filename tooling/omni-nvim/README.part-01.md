# omni-nvim

`omni-nvim` is a minimal Neovim plugin for working against the Omni REPL.
It is intentionally small: prefer a discovered project-local TCP REPL server
when `.omni-repl-port` is present, otherwise start a live structured
`omni --repl --json` session, send code from the current buffer, and keep the
transcript in a scratch window.

This is a first-party scaffold for a more Conjure-like workflow, not a full
Conjure replacement yet.

## Features

- Attaches to a discovered TCP REPL server when `.omni-repl-port` is present in
  the project tree.
- Falls back to a local Neovim job using the structured `--repl --json`
  transport when no TCP discovery file is available.
- Sends the current form, root form, visual selection, current line, or whole buffer.
- Sends the enclosing declaration, call, or block using Omni Tree-sitter captures when available.
- Selects the enclosing form, root form, declaration, call, or block directly in the buffer.
- Supports an operator-style eval motion over the structured selection transport.
- Adds buffer-local Omni textobjects in operator-pending and visual mode.
- Uses `omni --eval --json` for current-form and current-line eval by default.
- Uses the persistent structured REPL for fallback eval and whole-buffer sends.
- Uses Tree-sitter form selection for current-form eval when the Omni parser is
  available, with delimiter scanning as fallback.
- Can register the first-party `omni-lsp` server from the current repo checkout.
- Exposes buffer-local LSP actions for hover, definition, references, rename,
  code action, formatting, and signature help.
- Exposes buffer-local LSP declaration jumps.
- Exposes buffer-local LSP implementation jumps.
- Exposes buffer-local LSP type-definition jumps.
- Exposes quickfix-backed multi-target lists for definitions, declarations,
  implementations, and type definitions.
- Exposes quickfix-backed incoming and outgoing call lists from Omni LSP call
  hierarchy results.
- Makes `OmniLspCodeAction` and `OmniLspFormat` range-aware for Ex ranges and
  visual selections.
- Exposes quickfix-backed LSP symbol commands for current-buffer and workspace
  symbol lists.
- Exposes a buffer-local document-link opener for import/export targets at the
  cursor.
- Exposes a quickfix-backed list of all document links in the current Omni
  buffer.
- Exposes next/previous navigation across document-link ranges in the current
  Omni buffer.
- Exposes a quickfix-backed current-symbol references list, including workspace
  fallback results when the active Omni buffer does not own the declaration.
- Exposes buffer-local codelens refresh/run commands for Omni LSP declaration
  lenses.
- Can auto-refresh Omni LSP codelenses on selected buffer events when enabled.
- Exposes buffer-local diagnostics commands for opening the location list and
  moving to the next or previous diagnostic.
- Exposes buffer-local inlay-hint toggles for Omni LSP parameter hints.
- Exposes a buffer-local fold refresh command that applies Omni LSP folding
  ranges as manual Neovim folds.
- Exposes structural selection expansion backed by Omni LSP `selectionRange`.
- Exposes first-party `conform.nvim` formatter helper specs for the shipped
  `omni --fmt` CLI.
- Streams the transcript into a scratch buffer.
- Detects `.omni` files and applies buffer-local mappings.
- Auto-starts the REPL on first Omni buffer open by default; set
  `auto_start = false` for a manual `:OmniReplStart` workflow.
- Pretty-prints nested Omni values in the transcript by default, with
  configurable width and indent settings.

## Requirements

- Neovim 0.10+
- `omni` available on `$PATH`

If Omni is installed in the default repo location, this command should work:

```bash
omni --version
```

## Installation

`plugin/omni.lua` now performs bootstrap only: it registers the Omni command
surface and filetype wiring with default settings, but it does not run the full
`setup()` side effects on startup. Call `require("omni").setup(...)` from your
plugin manager config to opt into custom REPL/eval settings, Tree-sitter
registration, and optional LSP auto-setup.

Example with `lazy.nvim`:

```lua
{
  dir = "/path/to/Omni/tooling/omni-nvim",
  init = function()
    vim.filetype.add({
      extension = {
        omni = "omni",
      },
    })
  end,
  ft = { "omni" },
  config = function()
    require("omni").setup({
      cmd = { "omni", "--repl", "--json" },
      repl = {
        mode = "json",
        discovery = {
          enabled = true,
          host = "127.0.0.1",
          port_file = ".omni-repl-port",
        },
      },
      eval = {
        cmd = { "omni", "--eval", "--json" },
      },
    })
  end,
}
```

If your plugin manager already exposes the plugin's `ftdetect/` files before the
lazy-load point, you can omit the `init` block. Keep it when `.omni`
filetype detection is otherwise missing.

REPL discovery note:

- When `repl.discovery.enabled = true` (the default), omni.nvim looks for
  `.omni-repl-port` from the current Omni buffer upward through the project
  root markers (`omni.toml`, `project.json`, `.git`).
- If the file is present and contains a valid port, omni.nvim connects to
  `repl.discovery.host:port` using the newer REPL-server protocol.
- If discovery is missing or the TCP attach fails, omni.nvim falls back to the
  configured local `cmd`.

Tree-sitter note:

- `require("omni").setup()` registers the Omni parser config and query files.
- You still need to install the parser once with `:TSInstall omni` before
  Tree-sitter highlighting becomes available.

If you also use `nvim-treesitter`, `omni-nvim` will register the Omni parser
config when `require("omni").setup()` runs. Then install the
parser with:

```vim
:TSInstall omni
```

If you want to trigger registration manually, use:

```vim
:OmniTreesitterRegister
```

If you also want `omni-nvim` to register the first-party language server, use:

```vim
:OmniLspSetup
```

Or enable that automatically:

```lua
require("omni").setup({
  lsp = {
    auto_setup = true,
  },
})
```

To auto-refresh codelenses in Omni buffers:

```lua
require("omni").setup({
  lsp = {
    codelens = {
      auto_refresh = true,
      events = { "BufEnter", "InsertLeave" },
    },
  },
})
```

To auto-refresh document highlights in Omni buffers:

```lua
require("omni").setup({
  lsp = {
    highlights = {
      auto_refresh = true,
      refresh_events = { "CursorHold", "CursorHoldI" },
      clear_events = { "CursorMoved", "InsertEnter", "BufLeave" },
    },
  },
})
```

To auto-refresh pull document diagnostics in Omni buffers:

```lua
require("omni").setup({
  lsp = {
    pull_diagnostics = {
      auto_refresh = true,
      events = { "BufEnter", "InsertLeave" },
      auto_open = false,
      auto_notify_empty = false,
    },
  },
})
```

To auto-refresh workspace pull diagnostics in Omni buffers:

```lua
require("omni").setup({
  lsp = {
    pull_diagnostics = {
      workspace_auto_refresh = true,
      workspace_events = { "BufWritePost" },
      workspace_auto_open = false,
      workspace_auto_notify_empty = false,
    },
  },
})
```

To auto-refresh the combined pull-diagnostics snapshot in Omni buffers:

```lua
require("omni").setup({
  lsp = {
    pull_diagnostics = {
      all_auto_refresh = true,
      all_events = { "BufEnter", "BufWritePost" },
      all_auto_open = false,
      all_auto_notify_empty = false,
    },
  },
})
```

By default this uses:

- `python3 /.../tooling/omni-lsp/omni_lsp.py`
- root markers: `omni.toml`, `project.json`, `.git`

Override the LSP command or server path with:

```lua
require("omni").setup({
  repo_root = "/path/to/Omni",
  lsp = {
    cmd = { "python3", "/custom/path/omni_lsp.py" },
    repo_root = "/path/to/Omni",
    root_markers = { "omni.toml", "project.json", ".git" },
  },
})
```

`treesitter.repo_root` is only for the Tree-sitter grammar checkout. It no
longer controls LSP server discovery or formatter working directory.

To wire the shipped `omni --fmt` CLI into `conform.nvim`:

```lua
local omni = require("omni")

require("conform").setup(vim.tbl_deep_extend("force", {
  formatters = {
    omni_fmt = omni.conform_formatter(),
  },
  formatters_by_ft = omni.conform_formatters_by_ft(),
}, {
  format_on_save = {
    timeout_ms = 1000,
    lsp_format = "fallback",
  },
}))
```

Or use the combined helper spec directly:

```lua
local omni = require("omni")
local spec = omni.conform_setup_spec()

require("conform").setup(vim.tbl_deep_extend("force", spec, {
  format_on_save = {
    timeout_ms = 1000,
    lsp_format = "fallback",
  },
}))
```

By default the helper targets `omni --fmt --write $FILENAME` with `stdin = false`.
That matches the current formatter CLI contract and Conform's temp-file path
for non-stdin formatters. Override the formatter working directory with:

```lua
require("omni").setup({
  formatter = {
    cwd = "/path/to/project",
  },
})
```
