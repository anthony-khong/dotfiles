-- Leader
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Makes copy and pasting work
vim.opt.clipboard = "unnamed"
vim.opt.clipboard = vim.opt.clipboard + "unnamedplus"

-- Search case insensitive when all characters are lower case
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Better scrolling experience
vim.opt.scrolloff = 8
vim.opt.scrolljump = 1

-- Disable backup and swap files
vim.opt.backup = false
vim.opt.writebackup = false
vim.opt.swapfile = false

-- Expand tab to spaces
vim.opt.expandtab = true

-- Sign column shows up all the time
vim.opt.signcolumn = "yes"

-- Set relative number
vim.opt.number = true
vim.opt.relativenumber = true

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
