local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.uv.fs_stat(lazypath) then
  vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable",
                  "https://github.com/folke/lazy.nvim.git", lazypath })
end
vim.opt.rtp:prepend(lazypath)

local lisps = { "clojure", "fennel", "janet", "hy", "racket", "scheme", "lisp" }

require("lazy").setup({
  spec = {
    -- Loaded at startup
    "nvim-lua/plenary.nvim",
    { "shatur/neovim-ayu", priority = 1000 },
    "lewis6991/gitsigns.nvim",
    "lukas-reineke/indent-blankline.nvim",
    "junegunn/vim-easy-align",
    "tpope/vim-repeat",
    "tpope/vim-surround",
    "tpope/vim-endwise",
    "AndrewRadev/splitjoin.vim",
    "christoomey/vim-tmux-navigator",
    "jpalardy/vim-slime",
    "neovim/nvim-lspconfig",
    "lukas-reineke/lsp-format.nvim",
    { "nvim-treesitter/nvim-treesitter", branch = "main", build = ":TSUpdate" },
    { "nvim-treesitter/nvim-treesitter-textobjects", branch = "main" },
    "j-hui/fidget.nvim",
    "tpope/vim-projectionist",
    "nvim-tree/nvim-tree.lua",
    "nvim-telescope/telescope.nvim",
    { "saghen/blink.cmp", version = "1.*" },
    "hrsh7th/vim-vsnip",
    "rafamadriz/friendly-snippets",
    { "elixir-tools/elixir-tools.nvim", tag = "stable" },
    "mrcjkb/rustaceanvim",
    "jidn/vim-dbml",

    -- Loaded only when needed
    { "tpope/vim-fugitive", cmd = { "G", "Git", "Gdiff", "Gvdiffsplit", "Gread", "Gwrite", "Gedit" } },
    { "mattn/emmet-vim", ft = { "html", "heex", "eelixir", "css", "javascriptreact", "typescriptreact" } },
    { "rust-lang/rust.vim", ft = "rust" },
    { "plasticboy/vim-markdown", ft = "markdown" },
    { "Olical/conjure", ft = lisps },
    { "eraserhd/parinfer-rust", ft = lisps, build = "cargo build --release" },
    { "hylang/vim-hy", ft = "hy" },
    { "guns/vim-sexp", ft = lisps },
    { "tpope/vim-sexp-mappings-for-regular-people", ft = lisps },
  },
  install = { colorscheme = { "ayu-dark" } },
  rocks = { enabled = false },
  change_detection = { notify = false },
})
