if vim.g.loaded_omni_nvim == 1 then
  return
end

vim.g.loaded_omni_nvim = 1

require("omni").setup()
