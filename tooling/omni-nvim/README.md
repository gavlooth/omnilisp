# omni-nvim

`omni-nvim` is a minimal Neovim plugin for working against the Omni REPL.
It is intentionally small: start a live structured `omni --repl --json`
session, send code from the current buffer, and keep the transcript in a
scratch window.

This is a first-party scaffold for a more Conjure-like workflow, not a full
Conjure replacement yet.

## Features

- Starts an Omni REPL in a Neovim job using the structured `--repl --json`
  transport by default.
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

## Requirements

- Neovim 0.10+
- `omni` available on `$PATH`

If Omni is installed in the default repo location, this command should work:

```bash
omni --version
```

## Installation

Example with `lazy.nvim`:

```lua
{
  dir = "/home/heefoo/Documents/code/Omni/tooling/omni-nvim",
  ft = { "omni" },
  config = function()
    require("omni").setup({
      cmd = { "omni", "--repl", "--json" },
      repl = {
        mode = "json",
      },
      eval = {
        cmd = { "omni", "--eval", "--json" },
      },
    })
  end,
}
```

If you also use `nvim-treesitter`, `omni-nvim` will register the Omni parser
config automatically when `require("omni").setup()` runs. Then install the
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
- root markers: `project.json`, `.git`

Override the LSP command or server path with:

```lua
require("omni").setup({
  lsp = {
    cmd = { "python3", "/custom/path/omni_lsp.py" },
    root_markers = { "project.json", ".git" },
  },
})
```

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
for non-stdin formatters.

## Commands

- `:OmniReplStart`
- `:OmniReplStop`
- `:OmniReplRestart`
- `:OmniReplOpen`
- `:OmniReplClear`
- `:OmniTreesitterRegister`
- `:OmniLspSetup`
- `:OmniConformSetupSpec`
- `:OmniLspHover`
- `:OmniLspDefinition`
- `:OmniLspDefinitionsList`
- `:OmniLspDeclaration`
- `:OmniLspDeclarationsList`
- `:OmniLspImplementation`
- `:OmniLspImplementationsList`
- `:OmniLspTypeDefinition`
- `:OmniLspTypeDefinitionsList`
- `:OmniLspIncomingCallsList`
- `:OmniLspOutgoingCallsList`
- `:OmniLspReferences`
- `:OmniLspReferencesList`
- `:OmniLspRename`
- `:OmniLspCodeAction`
- `:OmniLspFormat`
- `:OmniLspSignatureHelp`
- `:OmniLspDocumentSymbols`
- `:OmniLspWorkspaceSymbols [query]`
- `:OmniLspOpenLink`
- `:OmniLspDocumentLinks`
- `:OmniLspNextLink`
- `:OmniLspPrevLink`
- `:OmniLspDocumentHighlight`
- `:OmniLspClearReferences`
- `:OmniLspDiagnostics`
- `:OmniLspDocumentDiagnostics`
- `:OmniLspWorkspaceDiagnostics`
- `:OmniLspAllDiagnostics`
- `:OmniLspAllDiagnosticsReset`
- `:OmniLspDocumentDiagnosticsReset`
- `:OmniLspWorkspaceDiagnosticsReset`
- `:OmniLspNextDiagnostic`
- `:OmniLspPrevDiagnostic`
- `:OmniLspCodeLensRefresh`
- `:OmniLspCodeLensRun`
- `:OmniLspInlayHintsEnable`
- `:OmniLspInlayHintsDisable`
- `:OmniLspInlayHintsToggle`
- `:OmniLspRefreshFolds`
- `:OmniLspExpandSelection`
- `:OmniSelectForm`
- `:OmniSelectRootForm`
- `:OmniSelectDecl`
- `:OmniSelectCall`
- `:OmniSelectBlock`
- `:OmniEvalForm`
- `:OmniEvalDecl`
- `:OmniEvalCall`
- `:OmniEvalBlock`
- `:OmniEvalRootForm`
- `:OmniEvalLine`
- `:OmniEvalOperator`
- `:OmniEvalSelection`
- `:OmniEvalBuffer`

## Default Mappings

These are buffer-local in Omni buffers:

- `<localleader>rs` start REPL
- `<localleader>rr` restart REPL
- `<localleader>ro` open transcript window
- `<localleader>rc` clear transcript window
- `<localleader>sd` select enclosing declaration
- `<localleader>sc` select enclosing call
- `<localleader>sb` select enclosing block
- `<localleader>sf` select current form
- `<localleader>sr` select root form
- `<localleader>ed` send enclosing declaration
- `<localleader>ec` send enclosing call
- `<localleader>ep` send enclosing block
- `<localleader>eo` start eval operator
- `<localleader>ef` send current form
- `<localleader>er` send root form
- `<localleader>el` send current line
- `<localleader>es` send visual selection
- `<localleader>eb` send whole buffer
- `K` LSP hover
- `gd` LSP definition
- `<localleader>lg` LSP definitions quickfix list
- `gD` LSP declaration
- `<localleader>lG` LSP declarations quickfix list
- `gi` LSP implementation
- `<localleader>lI` LSP implementations quickfix list
- `<localleader>lt` LSP type definition
- `<localleader>lT` LSP type definitions quickfix list
- `<localleader>lC` LSP incoming calls quickfix list
- `<localleader>lc` LSP outgoing calls quickfix list
- `grr` LSP references
- `<localleader>lR` LSP references quickfix list
- `<localleader>lr` LSP rename
- `<localleader>la` LSP code action
- `<localleader>lf` LSP format
- `<localleader>ls` LSP signature help
- `<localleader>lq` LSP document symbols
- `<localleader>lQ` LSP workspace symbols
- `<localleader>lo` LSP open document link
- `<localleader>lO` LSP document links list
- `]o` next document link
- `[o` previous document link
- `<localleader>lh` LSP document highlight
- `<localleader>lH` LSP clear references
- `<localleader>ld` LSP diagnostics location list
- `<localleader>lp` LSP pull document diagnostics
- `<localleader>lP` LSP pull workspace diagnostics
- `<localleader>lA` LSP combined pull diagnostics
- `]d` next diagnostic
- `[d` previous diagnostic
- `<localleader>ll` refresh LSP codelens
- `<localleader>lL` run LSP codelens
- `<localleader>li` toggle LSP inlay hints
- `<localleader>lz` refresh LSP folds
- `<localleader>lv` expand LSP selection

Default textobjects in Omni buffers:

- `af` / `if` current form
- `ar` / `ir` root form
- `ad` / `id` declaration
- `ac` / `ic` call
- `aB` / `iB` block

Disable mappings with:

```lua
require("omni").setup({
  mappings = false,
})
```

Disable the textobject bindings separately with:

```lua
require("omni").setup({
  textobjects = {
    enabled = false,
  },
})
```

Or override them:

```lua
require("omni").setup({
  textobjects = {
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
  keys = {
    start = "<localleader>os",
    restart = "<localleader>or",
    open = "<localleader>oo",
    clear = "<localleader>oc",
    select_decl = "<localleader>sd",
    select_call = "<localleader>sc",
    select_block = "<localleader>sb",
    select_form = "<localleader>sf",
    select_root = "<localleader>sr",
    eval_decl = "<localleader>ed",
    eval_call = "<localleader>ec",
    eval_block = "<localleader>ep",
    eval_operator = "<localleader>eo",
    eval_form = "<localleader>ee",
    eval_root = "<localleader>er",
    eval_line = "<localleader>el",
    eval_selection = "<localleader>es",
    eval_buffer = "<localleader>eb",
  },
})
```

Disable automatic `nvim-treesitter` registration with:

```lua
require("omni").setup({
  treesitter = {
    register = false,
  },
})
```

Disable the buffer-local LSP mappings separately with:

```lua
require("omni").setup({
  lsp = {
    mappings = false,
  },
})
```

Enable inline eval annotations for single-form evals (default: off):

```lua
require("omni").setup({
  eval = {
    annotations = {
      enabled = true,
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
})
```

Operator eval usage:

```vim
<localleader>eoiw
<localleader>eoap
```

That enters an operator-pending eval and sends the motion-selected region
through the same structured REPL path as visual selection eval.

## Notes

- The transcript buffer strips ANSI escape codes from Omni's REPL output.
- Transcript split direction is controlled by `output.split`, and automatic
  cursor-following can be disabled with `output.auto_scroll = false`.
- Result annotations are optional and off by default. Enable them with
  `eval.annotations.enabled = true` to show single-form `--eval` results as
  virtual text in the current buffer.
- When `nvim-treesitter` is installed, `omni-nvim` registers the Omni parser
  from this repo using the current checkout as the install source and appends
  `tooling/tree-sitter-omni` to `runtimepath` so the bundled queries are found.
- `:OmniLspSetup` registers the first-party `omni-lsp` server from the same
  repo checkout. It prefers Neovim's built-in `vim.lsp.config` / `vim.lsp.enable`
  path when available and falls back to `lspconfig` registration on older setups.
- The `OmniLsp*` commands are thin wrappers around `vim.lsp.buf.*`, so they use
  whatever Omni LSP client is attached to the current buffer, not only the
  repo-local registration path.
- `OmniLspCodeAction` and `OmniLspFormat` pass explicit line/selection ranges
  through to the LSP client when you invoke them with an Ex range or the
  default visual-mode mappings, so selection-based refactors and formatting use
  the actual Omni region.
- `OmniLspDocumentSymbols` and `OmniLspWorkspaceSymbols` use synchronous LSP
  requests and open the quickfix list with the returned symbol locations. The
  workspace-symbol command accepts an optional query argument and prompts when
  none is provided.
- `OmniLspOpenLink` uses a synchronous `textDocument/documentLink` request,
  finds the link range under the cursor, and opens file targets directly in the
  current Neovim session.
- `OmniLspDocumentLinks` uses the same `textDocument/documentLink` surface and
  opens a quickfix list of the source link locations in the current buffer,
  labelled by their target file names.
- `OmniLspNextLink` and `OmniLspPrevLink` use the same document-link surface and
  move the cursor through link ranges in the current buffer, wrapping when they
  reach the end or beginning.
- `OmniLspDocumentHighlight` and `OmniLspClearReferences` delegate to
  Neovim's built-in document-highlight API for the current buffer, which makes
  it easy to trigger or clear Omni symbol highlights explicitly even when you
  do not want them on editor timers.
- `OmniLspDefinitionsList`, `OmniLspDeclarationsList`,
  `OmniLspImplementationsList`, and `OmniLspTypeDefinitionsList` use
  synchronous location requests and open the quickfix list, which is useful
  when Omni returns more than one navigation target for an overloaded name.
- `OmniLspReferencesList` uses a synchronous `textDocument/references` request
  and opens the quickfix list for the symbol under the cursor, including the
  declaration by default. When `omni-lsp` falls back to workspace references,
  that list can include matches from other open Omni files and unopened
  workspace files too.
- `OmniLspCodeLensRefresh` and `OmniLspCodeLensRun` delegate to Neovim's
  built-in codelens API, which lets Omni LSP declaration lenses render and run
  through the attached client when the editor supports it. `omni-nvim`
  registers the first-party `omni.showReferences` LSP command and opens the
  supplied codelens locations in quickfix when that lens is run.
- If `lsp.codelens.auto_refresh = true`, `omni-nvim` also installs buffer-local
  autocmds for the configured events and refreshes Omni codelenses
  automatically in Omni buffers.
- If `lsp.highlights.auto_refresh = true`, `omni-nvim` installs buffer-local
  autocmds for the configured refresh and clear events, so Omni document
  highlights follow cursor movement without requiring a separate plugin.
- `OmniLspRename` delegates to the attached Omni LSP client. When `omni-lsp`
  falls back to workspace rename, the resulting workspace edit can touch other
  open Omni files and unopened workspace files in the same discovered root.
- `OmniLspDiagnostics` opens the location list for the current buffer through
  `vim.diagnostic.setloclist`, while `OmniLspNextDiagnostic` and
  `OmniLspPrevDiagnostic` delegate to Neovim's diagnostic jump helpers.
- `OmniLspDocumentDiagnostics` and `OmniLspWorkspaceDiagnostics` use the Omni
  LSP pull-diagnostics requests and open quickfix with the returned diagnostic
  items, which is useful when you want an explicit server snapshot instead of
  only the editor's push-diagnostics view. `omni-nvim` also caches pull
  diagnostic `resultId` values and reuses them on later requests, so unchanged
  replies still rebuild quickfix from the previous snapshot instead of going
  blank.
- `OmniLspAllDiagnostics` builds one quickfix snapshot from both current-buffer
  and workspace pull diagnostics, deduplicating identical entries while still
  reusing the same cached pull-diagnostics result ids under the hood.
- `OmniLspAllDiagnosticsReset` clears both the current-buffer and workspace
  pull-diagnostics caches before the next combined snapshot.
- `OmniLspDocumentDiagnosticsReset` and `OmniLspWorkspaceDiagnosticsReset`
  clear the cached pull-diagnostics `resultId` state for the current buffer or
  workspace snapshot, which is useful after an LSP restart or when you want to
  force the next pull request to behave like a fresh full query.
- `omni-nvim` also clears current-buffer pull-diagnostics cache entries when an
  Omni buffer is deleted or wiped, and clears both current-buffer and workspace
  pull-diagnostics caches when the `omni_lsp` client detaches from that buffer,
  so stale `previousResultId` values do not survive old editor sessions.
- If `lsp.pull_diagnostics.auto_refresh = true`, `omni-nvim` installs
  buffer-local autocmds for the configured events and refreshes current-buffer
  pull diagnostics automatically into quickfix. By default this auto-refresh
  path uses `auto_open = false` and `auto_notify_empty = false`, so it keeps
  the quickfix snapshot current without reopening the quickfix window on every
  refresh event and keeps clean refreshes silent.
- If `lsp.pull_diagnostics.workspace_auto_refresh = true`, `omni-nvim`
  installs a separate buffer-local autocmd set for the configured workspace
  events and refreshes workspace pull diagnostics into quickfix. By default
  this workspace auto-refresh path uses `workspace_auto_open = false` and
  `workspace_auto_notify_empty = false`, so it does not force the quickfix
  window open on every refresh and keeps clean workspace refreshes silent.
- If `lsp.pull_diagnostics.all_auto_refresh = true`, `omni-nvim` installs a
  separate buffer-local autocmd set for the configured combined-snapshot
  events and refreshes `OmniLspAllDiagnostics` automatically. By default this
  uses `all_auto_open = false` and `all_auto_notify_empty = false`, so the
  merged quickfix snapshot stays current without reopening quickfix on every
  event and clean combined refreshes stay silent.
- `OmniLspInlayHintsEnable`, `OmniLspInlayHintsDisable`, and
  `OmniLspInlayHintsToggle` delegate to Neovim's inlay-hint API for the current
  buffer and are compatible with both newer and older calling conventions.
- `OmniLspRefreshFolds` requests `textDocument/foldingRange` from the attached
  LSP client and applies the results as manual folds in the current window.
  This is explicit and buffer-local: it does not try to replace Neovim's global
  folding setup automatically.
- `OmniLspExpandSelection` requests `textDocument/selectionRange` and walks up
  the returned parent chain one step at a time, so repeated use expands the
  current Omni selection from the innermost symbol or form outward.
- When Neovim does not already have Omni queries loaded, `omni-nvim` also loads
  the bundled first-party query files directly from `tooling/tree-sitter-omni`
  so capture-based features can work before a separate parser plugin package
  exists.
- Current-form extraction prefers Tree-sitter Omni nodes when the parser is
  installed, which lets path, quote, accessor, and index forms be selected as
  standalone eval targets.
- Enclosing declaration/call/block eval uses the bundled Omni textobject queries
  when the parser is installed, then falls back to the existing form/root
  selection behavior if those captures are unavailable.
- The `OmniSelect*` commands use the same capture-aware range lookup and keep
  the delimiter-based fallback for form/root selection when the parser is not
  available.
- The default Omni textobjects are buffer-local `x`/`o` mappings layered on top
  of those same selection helpers. Inner variants now trim the outer wrapper
  for bracketed and prefixed forms when possible, while outer variants keep the
  full structural region.
- Root-form eval selects the highest enclosing Omni form under the cursor
  before the file boundary, which is useful for top-level definitions and
  module wrappers.
- If the Tree-sitter parser is not available, current-form extraction falls
  back to the delimiter-aware scanner for `()`, `[]`, and `{}`. Root-form eval
  uses the same fallback strategy for delimiter-backed top-level forms.
- Current-form and current-line eval default to the structured `omni --eval --json`
  path; fallback eval and whole-buffer sends use the persistent structured
  `omni --repl --json` session.
- Visual selection eval uses the persistent structured REPL in `program` mode so
  multi-form regions can be sent without squeezing them through the single-form
  CLI eval path.
- Operator eval uses Vim motions to define the region and then sends that text
  through the same `program`-mode structured REPL path as visual selection eval.
- This scaffold talks to the real Omni CLI instead of a second evaluation path.
