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
  nnoremap <localleader>tc :NvimTreeCollapse<CR>

  nnoremap <localleader>tT :tabe<CR>:NvimTreeToggle<CR>
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

  function! SlimeOverride_EscapeText_elixir(text)
    if a:text =~ "|>" && len(split(a:text,"\n")) > 1
      return ["(\n", a:text, ")\n"]
    end
    return [a:text]
  endfunction
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

  let g:NERDSpaceDelims = 1
  let g:NERDCompactSexyComs = 1
]])

-- Treesitter
require 'nvim-treesitter.configs'.setup {
  ensure_installed = {
    "css",
    "elixir",
    "erlang",
    "heex",
    "html",
    "html",
    "json",
    "lua",
    "surface",
    "toml",
    "tsx",
    "yaml",
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
  imap <expr> <Tab> vsnip#expandable() ? '<Plug>(vsnip-expand)'    : '<Tab>'
  smap <expr> <Tab> vsnip#expandable() ? '<Plug>(vsnip-expand)'    : '<Tab>'
  imap <expr> <C-n> vsnip#jumpable(1)  ? '<Plug>(vsnip-jump-next)' : '<C-n>'
  smap <expr> <C-n> vsnip#jumpable(1)  ? '<Plug>(vsnip-jump-next)' : '<C-n>'
  imap <expr> <C-p> vsnip#jumpable(-1) ? '<Plug>(vsnip-jump-prev)' : '<C-p>'
  smap <expr> <C-p> vsnip#jumpable(-1) ? '<Plug>(vsnip-jump-prev)' : '<C-p>'

  let g:vsnip_filetypes = {}
  let g:vsnip_filetypes.elixir = ['elixir', 'eelixir', 'html']
  let g:vsnip_filetypes.eelixir = ['elixir', 'eelixir', 'html']
]])

-- Emmet
vim.cmd([[
  let g:user_emmet_leader_key='<C-E>'
]])
