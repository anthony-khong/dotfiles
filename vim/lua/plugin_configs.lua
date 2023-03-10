-- Nvim Tree
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
require("nvim-tree").setup({
  renderer = {
    icons = {
      glyphs = {
        default = "-",
        symlink = "L",
        modified = "~",
        folder = {
          arrow_closed = "▸",
          arrow_open = "▾",
          default = "",
          open = "",
        },
        git = {
          unstaged = "✗",
          staged = "✓",
          unmerged = " ",
          renamed = "➜",
          untracked = "★",
          deleted = " ",
          ignored = " ",
        }
      }
    }
  },
  sort_by = "case_sensitive",
  view = { width = 30, },
})

vim.cmd([[
  nnoremap <localleader>tt :NvimTreeToggle<CR>
  nnoremap <localleader>tf :NvimTreeFindFile<CR>
  nnoremap <localleader>tr :NvimTreeRefresh<CR>
]])

-- Indent Line
require("indent_blankline").setup {
  char = "·",
  show_current_context = true
}

-- Slime
vim.cmd([[
  let g:slime_target = "tmux"
  silent! let g:slime_default_config = {
      \ "socket_name": split($TMUX, ",")[0],
      \ "target_pane": ":0.1"
      \ }
  let g:slime_dont_ask_default = 1
  let g:slime_python_ipython = 1
]])

-- Easy Align
vim.cmd([[
  xmap ga <Plug>(EasyAlign)
  nmap ga <Plug>(EasyAlign)
]])

-- Fugitive
vim.cmd([[
  nnoremap <localleader>gd :Gdiff<CR>
  nnoremap <localleader>gb :Git blame<CR>
]])


-- Nerd Commenter
vim.cmd([[
  let g:NERDCreateDefaultMappings = 0
  nmap gcc <Plug>NERDCommenterToggle
  vmap gcc <Plug>NERDCommenterToggle
]])

-- Treesitter
require 'nvim-treesitter.configs'.setup {
  ensure_installed = {
    "eex",
    "elixir",
    "erlang",
    "heex",
    "html",
    "surface",
  },
  sync_install = false,
  ignore_install = {},
  highlight = {
    enable = true,
    disable = {},
  },
}

-- Telescope
local builtin = require('telescope.builtin')
vim.keymap.set('n', '<leader>ff', builtin.find_files, {})
vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
vim.keymap.set('n', '<leader>fb', builtin.buffers, {})
vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})

-- VSnip
vim.cmd([[
  imap <expr> <Tab> vsnip#expandable()  ? '<Plug>(vsnip-expand)' : '<Tab>'
  smap <expr> <Tab> vsnip#expandable()  ? '<Plug>(vsnip-expand)' : '<Tab>'

  let g:vsnip_filetypes = {}
  let g:vsnip_filetypes.elixir = ['elixir', 'html']
]])
