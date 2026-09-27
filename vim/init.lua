vim.loader.enable()
require("configs")
require("plugins")
require("plugin_configs")
require("lsp")
require("keybindings")
require("colours")

-- Per-machine settings that don't belong in the repo
local local_config = vim.fn.expand("~/.config/local/nvim.lua")
if vim.uv.fs_stat(local_config) then dofile(local_config) end
