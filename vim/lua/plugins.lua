-- Source: https://raw.githubusercontent.com/arusahni/dotfiles/fd6bf6435763f6d29e95ec3c42cdc33aa2cf6952/nvim/lua/plugins.lua

local fn = vim.fn
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap =
    fn.system({"git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path})
end

vim.cmd [[packadd packer.nvim]]

return require("packer").startup {
  function(use)
    use "wbthomason/packer.nvim"

    -- Utilities
    use 'nvim-lua/plenary.nvim'

    -- Appearance
    use 'lewis6991/gitsigns.nvim'
    use 'shatur/neovim-ayu'
    use "lukas-reineke/indent-blankline.nvim"

    -- Editing
    use 'junegunn/vim-easy-align'
    use 'tpope/vim-repeat'
    use 'tpope/vim-surround'
    use 'mattn/emmet-vim'

    -- Tmux
    use 'christoomey/vim-tmux-navigator'
    use 'jpalardy/vim-slime'

    -- Git
    use 'tpope/vim-fugitive'

    -- IDE
    use 'neovim/nvim-lspconfig'
    use "lukas-reineke/lsp-format.nvim"
    use { "nvim-treesitter/nvim-treesitter", branch = "main", }
    use 'j-hui/fidget.nvim'
    use 'tpope/vim-projectionist'

    use 'nvim-tree/nvim-tree.lua'
    use {
      'nvim-telescope/telescope.nvim',
      requires = { {'nvim-lua/plenary.nvim'} }
    }

    -- Completion
    use 'hrsh7th/cmp-buffer'
    use 'hrsh7th/cmp-cmdline'
    use 'hrsh7th/cmp-nvim-lsp'
    use 'hrsh7th/cmp-path'
    use 'hrsh7th/nvim-cmp'

    -- Snippets
    use 'hrsh7th/vim-vsnip'
    use 'hrsh7th/cmp-vsnip'
    use "rafamadriz/friendly-snippets"

    -- Elixir
    use({ "elixir-tools/elixir-tools.nvim", tag = "stable", requires = { "nvim-lua/plenary.nvim" }})
    use 'hrsh7th/cmp-nvim-lsp-signature-help'

    -- Rust
    use 'rust-lang/rust.vim'
    use 'mrcjkb/rustaceanvim'

    -- Lisp
    use 'Olical/conjure'
    use {
      'eraserhd/parinfer-rust',
      run = 'cargo build --release',
      -- For M1 Macs:
      -- $ cargo build --target=aarch64-apple-darwin --release
      -- $ rm -rf target/release && mv target/aarch64-apple-darwin/release target/release
    }
    use 'hylang/vim-hy'
    use 'guns/vim-sexp'
    use 'tpope/vim-sexp-mappings-for-regular-people'

    -- Others
    use 'plasticboy/vim-markdown'
    use 'jidn/vim-dbml'

    if packer_bootstrap then
      require("packer").sync()
    end
  end
}
