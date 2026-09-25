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
require("ibl").setup()

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


-- Treesitter
local treesitter = require("nvim-treesitter")
treesitter.setup()
local ts_languages = {
  "bash",
  "css",
  "eex",
  "elixir",
  "erlang",
  "heex",
  "html",
  "json",
  "lua",
  "python",
  "rust",
  "surface",
  "toml",
  "tsx",
  "yaml",
}
treesitter.install(ts_languages)

-- Highlight with treesitter wherever one of these parsers applies;
-- indent with it only for the Elixir family
vim.api.nvim_create_autocmd("FileType", {
  callback = function(ev)
    local lang = vim.treesitter.language.get_lang(ev.match)
    if not vim.tbl_contains(ts_languages, lang) or not pcall(vim.treesitter.start, ev.buf, lang) then
      return
    end
    if vim.tbl_contains({ "elixir", "heex", "eex" }, lang) then
      vim.bo[ev.buf].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
    end
  end,
})

-- Treesitter text objects: function (af/if), module or class (ac/ic),
-- parameter in a definition (aa/ia). Jumps ahead if the cursor isn't in one.
require("nvim-treesitter-textobjects").setup({ select = { lookahead = true } })
local select_textobject = require("nvim-treesitter-textobjects.select").select_textobject
for lhs, capture in pairs({
  af = "@function.outer", ["if"] = "@function.inner",
  ac = "@class.outer", ic = "@class.inner",
  aa = "@parameter.outer", ia = "@parameter.inner",
}) do
  vim.keymap.set({ "x", "o" }, lhs, function() select_textobject(capture, "textobjects") end)
end

-- Built-in optional plugins: clear search highlighting on insert or when idle; :Undotree
vim.cmd.packadd("nohlsearch")
vim.cmd.packadd("nvim.undotree")


-- Telescope
local builtin = require('telescope.builtin')
vim.keymap.set('n', '<leader>ff', builtin.find_files, {})
vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
vim.keymap.set('n', '<leader>fb', builtin.buffers, {})
vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})
vim.keymap.set('n', '<leader>fs', builtin.lsp_dynamic_workspace_symbols, {})

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

-- Conjure
vim.cmd([[
  let g:conjure#log#wrap = 1
  let g:conjure#filetypes = ["clojure", "fennel", "janet", "hy", "racket", "scheme", "lisp"]

  augroup conjure_mappings
    autocmd!
    autocmd FileType clojure,fennel,janet,hy,racket,scheme,lisp
      \ nnoremap <buffer> <leader>cc vip:ConjureEval<CR>
    autocmd FileType clojure,fennel,janet,hy,racket,scheme,lisp
      \ nmap <buffer> <Space>cl <Space>lv<C-W><C-H>:exe "vertical resize " . (winwidth(0) * 5/4)<CR>
  augroup END
]])

-- Parinfer
vim.cmd([[
  let g:parinfer_mode = 'smart'
  let g:parinfer_enabled = 1
  let g:parinfer_force_balance = 0
]])

-- Git Signs
require('gitsigns').setup({
  signs = {
    add = { text = '+' }, change = { text = '~' }, delete = { text = '_' },
    topdelete = { text = '‾' }, changedelete = { text = '~' },
  },
})
