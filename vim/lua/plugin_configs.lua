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

-- Nerd Commenter
vim.cmd([[
  let g:NERDCreateDefaultMappings = 0
  nmap gcc <Plug>NERDCommenterToggle
  vmap gcc <Plug>NERDCommenterToggle
]])

-- Treesitter
require'nvim-treesitter.configs'.setup {
  ensure_installed = {
    "eex",
    "elixir",
    "erlang",
    "heex",
    "html",
    "surface",
  },
  sync_install = false,
  ignore_install = { },
  highlight = {
    enable = true,
    disable = { },
  },
}

-- UltiSnips
vim.cmd([[
  augroup load_ultisnips
    autocmd!
    autocmd InsertEnter * call plug#load('ultisnips') | autocmd! load_ultisnips
  augroup END
  let g:UltiSnipsExpandTrigger = "<Tab>"
  let g:UltiSnipsSnippetDirectories = ["UltiSnips", "custom_snippets"]
  nnoremap <localleader>ku :au! UltiSnips_AutoTrigger<CR>
]])

-- Telescope
local builtin = require('telescope.builtin')
vim.keymap.set('n', '<leader>ff', builtin.find_files, {})
vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
vim.keymap.set('n', '<leader>fb', builtin.buffers, {})
vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})

-- FZF
vim.cmd([[
  let g:fzf_layout = { 'window': { 'width': 0.9, 'height': 0.9, 'relative': v:true, 'border': 'rounded' } }
  let g:fzf_action = {'enter': 'tabedit'}
  nnoremap <localleader>be :call fzf#vim#gitfiles('', fzf#vim#with_preview('down:72%'))<CR>
  nnoremap <localleader>bh :call fzf#vim#gitfiles('~', fzf#vim#with_preview('down:72%'))<CR>
  nnoremap <localleader>bE :call fzf#vim#files('', fzf#vim#with_preview('down:72%'))<CR>
  nnoremap <localleader>bH :call fzf#vim#files('~', fzf#vim#with_preview('down:72%'))<CR>
  let g:fzf_colors = {
    \ 'fg':      ['fg', 'Normal'],
    \ 'bg':      ['bg', 'Normal'],
    \ 'hl':      ['fg', 'Comment'],
    \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
    \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
    \ 'hl+':     ['fg', 'Statement'],
    \ 'info':    ['fg', 'PreProc'],
    \ 'border':  ['fg', 'Ignore'],
    \ 'prompt':  ['fg', 'Conditional'],
    \ 'pointer': ['fg', 'Exception'],
    \ 'marker':  ['fg', 'Keyword'],
    \ 'spinner': ['fg', 'Label'],
    \ 'header':  ['fg', 'Comment'] }
]])
