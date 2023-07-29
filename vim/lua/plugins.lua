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
		use 'airblade/vim-gitgutter'
		use 'ayu-theme/ayu-vim'
    use 'projekt0n/github-nvim-theme'
    use "lukas-reineke/indent-blankline.nvim"

    -- Editing
    use 'junegunn/vim-easy-align'
    use 'scrooloose/nerdcommenter'
    use 'tpope/vim-repeat'
    use 'tpope/vim-surround'
    use 'mattn/emmet-vim'
    use {
      'laytan/tailwind-sorter.nvim',
      requires = {'nvim-treesitter/nvim-treesitter', 'nvim-lua/plenary.nvim'},
      run = 'cd formatter && npm i && npm run build',
    }

    -- Tmux
    use 'christoomey/vim-tmux-navigator'
    use 'jpalardy/vim-slime'

    -- Git
    use 'tpope/vim-fugitive'

    -- IDE
    use 'neovim/nvim-lspconfig'
    use "lukas-reineke/lsp-format.nvim"
    use 'sheerun/vim-polyglot'
    use 'nvim-treesitter/nvim-treesitter'
    use { 'j-hui/fidget.nvim', tag = 'legacy' }

    use 'nvim-tree/nvim-tree.lua'
    use {
      'nvim-telescope/telescope.nvim', tag = '0.1.1',
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
    use 'hrsh7th/vim-vsnip-integ'
    use 'hrsh7th/cmp-vsnip'
    use "rafamadriz/friendly-snippets"

    -- Elixir
    use {
      'elixir-tools/elixir-tools.nvim',
      requires = { 'nvim-lua/plenary.nvim' }
    }

    -- Rust
    use 'rust-lang/rust.vim'
    use 'simrat39/rust-tools.nvim'

    -- JavaScript and TypeScript
    use 'leafgarland/typescript-vim'
    use 'maxmellon/vim-jsx-pretty'
    use 'pangloss/vim-javascript'

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
    use 'ekalinin/Dockerfile.vim'
    use 'plasticboy/vim-markdown'
    use 'kevinoid/vim-jsonc'
    use 'jidn/vim-dbml'

    if packer_bootstrap then
      require("packer").sync()
    end
  end
}
