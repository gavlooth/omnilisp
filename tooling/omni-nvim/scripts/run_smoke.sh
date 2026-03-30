#!/usr/bin/env sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname "$0")" && pwd)
PLUGIN_DIR=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)
TMP_OMNI_FILE=$(mktemp "${TMPDIR:-/tmp}/omni-nvim-smoke-XXXXXX.omni")
cleanup() {
  rm -f "$TMP_OMNI_FILE"
}
trap cleanup EXIT INT TERM

nvim --headless --clean -u NONE -i NONE \
  --cmd "set runtimepath^=$PLUGIN_DIR" \
  --cmd "filetype plugin on" \
  --cmd "runtime plugin/omni.lua" \
  +"lua assert(vim.fn.exists(':OmniReplStart') == 2, 'OmniReplStart command missing after bootstrap')" \
  +"edit $TMP_OMNI_FILE" \
  +"lua assert(vim.bo.filetype == 'omni', 'Expected .omni filetype detection')" \
  +"lua assert(vim.b.omni_nvim_mapped == true, 'Expected omni buffer mappings to be applied')" \
  +"lua assert(vim.fn.exists(':OmniLspSetup') == 2, 'OmniLspSetup command missing after bootstrap')" \
  +qa

printf '%s\n' "omni-nvim smoke: ok"
