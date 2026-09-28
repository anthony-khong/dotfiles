-- Leader
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Makes copy and pasting work
vim.opt.clipboard = "unnamed"
vim.opt.clipboard = vim.opt.clipboard + "unnamedplus"

-- On a remote box (SSH, mosh, or headless Linux), yanks go to the laptop's clipboard as
-- OSC 52, through tmux and mosh. `p` pastes Neovim's own last yank: mosh can't carry the
-- terminal's reply to an OSC 52 paste request, so asking would hang. To paste the laptop's
-- clipboard, use the terminal's paste (Cmd-V).
local remote = vim.env.SSH_CONNECTION or vim.env.SSH_TTY
  or (vim.fn.has("linux") == 1 and not vim.env.DISPLAY and not vim.env.WAYLAND_DISPLAY)
if remote then
  local osc52 = require("vim.ui.clipboard.osc52")
  local function last_yank()
    return { vim.fn.split(vim.fn.getreg(""), "\n"), vim.fn.getregtype("") }
  end
  vim.g.clipboard = {
    name = "OSC 52 copy, local paste",
    copy = { ["+"] = osc52.copy("+"), ["*"] = osc52.copy("*") },
    paste = { ["+"] = last_yank, ["*"] = last_yank },
  }
end

-- Search case insensitive when all characters are lower case
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Better scrolling experience
vim.opt.scrolloff = 8
vim.opt.scrolljump = 1

-- No backup files. Swap files stay on (Nvim's default), in ~/.local/state/nvim/swap/: after a
-- crash, a power cut or `tmux kill-server`, reopening the file offers to (R)ecover unsaved edits.
-- A second Nvim opening the same file only gets a warning, not that prompt.
vim.opt.backup = false
vim.opt.writebackup = false

-- Keep undo history across sessions (saved to ~/.local/state/nvim/undo/ on :w)
vim.opt.undofile = true

-- Expand tab to spaces
vim.opt.expandtab = true

-- Sign column shows up all the time
vim.opt.signcolumn = "yes"

-- Set relative number
vim.opt.number = true
vim.opt.relativenumber = true

-- Spell check: a word passes if it's right in any of these. install.sh fetches the files.
-- On in markdown and git commits (ftplugin/), <Space>ss elsewhere; where treesitter
-- highlights, only comments and strings get checked
vim.opt.spelllang = { "en", "id", "es" }

-- No folding
vim.opt.foldenable = false

-- Tab management
vim.opt.splitright = true
vim.opt.splitbelow = true

-- Statusline
vim.opt.laststatus = 0
vim.opt.cmdheight = 1

-- Better Completion
vim.opt.completeopt = {'menuone', 'noselect', 'noinsert', 'preview'}
vim.opt.shortmess = vim.opt.shortmess + { c = true }

-- Bordered Window
vim.o.winborder = 'rounded'
