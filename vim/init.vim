"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                          Plugin Settings                          "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set runtimepath^=~/.config/nvim

call plug#begin()

" Appearance
Plug 'airblade/vim-gitgutter'
Plug 'Mangeshrex/uwu.vim'
Plug 'projekt0n/github-nvim-theme', { 'branch': 'main' }
Plug 'tanvirtin/monokai.nvim'

" Completions
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" Editing
Plug 'SirVer/ultisnips', { 'on': [] }
Plug 'ervandew/supertab'
Plug 'junegunn/vim-easy-align'
Plug 'scrooloose/nerdcommenter'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'

" Utilities
Plug 'jpalardy/vim-slime', { 'branch': 'main' }
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-fugitive'

" Tmux
Plug 'christoomey/vim-tmux-navigator'

" JavaScript
Plug 'leafgarland/typescript-vim'
Plug 'maxmellon/vim-jsx-pretty'
Plug 'pangloss/vim-javascript'
Plug 'rescript-lang/vim-rescript', {'tag': 'v2.1.0'}

" Clojure
Plug 'Olical/conjure', {'tag': 'v4.25.0'}
Plug 'eraserhd/parinfer-rust', {'do': 'cargo build --release'}
Plug 'guns/vim-sexp'
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'tpope/vim-sexp-mappings-for-regular-people'

" Rust
Plug 'rust-lang/rust.vim'

" Dockerfile
Plug 'ekalinin/Dockerfile.vim'

" Markdown
Plug 'plasticboy/vim-markdown'

" JSON
Plug 'kevinoid/vim-jsonc'

call plug#end()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                         Plugin Settings                        "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Set the leader to comma
let mapleader = "\<space>"
let maplocalleader = "\<space>"

" CoC
let g:coc_global_extensions = [
			\ 'coc-conjure',
			\ 'coc-css',
      \ 'coc-diagnostic',
			\ 'coc-eslint',
			\ 'coc-json',
			\ 'coc-prettier',
			\ 'coc-pyright',
      \ 'coc-rust-analyzer',
			\ 'coc-tsserver'
			\ ]
nmap <silent> <localleader>cd <Plug>(coc-definition)
nmap <silent> <localleader>ci <Plug>(coc-implementation)
nmap <silent> <localleader>cf <Plug>(coc-references)
nmap <silent> <localleader>ct <Plug>(coc-type-definition)
nmap <silent> <localleader>cp <Plug>(coc-diagnostic-prev)
nmap <silent> <localleader>cn <Plug>(coc-diagnostic-next)
nmap <localleader>cr <Plug>(coc-rename)
nmap <localleader>cd <Plug>(coc-codeaction)

nnoremap <nowait><expr> <localleader>cc coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
nnoremap <nowait><expr> <localleader>cC coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"

"" Use K to show documentation in preview window.
function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction
nnoremap <silent> K :call <SID>show_documentation()<CR>

"" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" SuperTab
let g:SuperTabDefaultCompletionType = "<c-n>"

" NERDCommenter
let g:NERDCreateDefaultMappings = 0
nmap gcc <Plug>NERDCommenterToggle
vmap gcc <Plug>NERDCommenterToggle

" Easy Align
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" Slime
let g:slime_target = "tmux"
silent! let g:slime_default_config = {
    \ "socket_name": split($TMUX, ",")[0],
    \ "target_pane": ":0.1"
    \ }
let g:slime_dont_ask_default = 1
let g:slime_python_ipython = 1

" FZF
let g:fzf_layout = { 'window': { 'width': 0.9,
                               \ 'height': 0.9,
                               \ 'border': 'rounded' } }
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

" NERDTree
let g:NERDTreeWinSize=50
nnoremap <localleader>bj :NERDTreeToggle<CR>
nnoremap <localleader>bf :NERDTreeFind<CR>

" Fugitive
nnoremap <localleader>gd :Gdiff<CR>
nnoremap <localleader>gb :Git blame<CR>

" UltiSnips
augroup load_ultisnips
  autocmd!
  autocmd InsertEnter * call plug#load('ultisnips') | autocmd! load_ultisnips
augroup END
let g:UltiSnipsExpandTrigger = "<Tab>"
let g:UltiSnipsSnippetDirectories = ["UltiSnips", "custom_snippets"]
nnoremap <localleader>ku :au! UltiSnips_AutoTrigger<CR>

" JSONC
nnoremap <localleader>jc :set ft=jsonc<CR>

" vim-sexp
let g:sexp_mappings = {
  \ 'sexp_move_to_prev_bracket': '',
  \ 'sexp_move_to_next_bracket': ''
  \ }

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                         Vim Settings                           "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Makes copy and pasting work
set clipboard=unnamed
set clipboard+=unnamedplus

" Search case insensitive when all characters are lower case
set ignorecase
set smartcase

" Enable syntax highlighting
syntax on
filetype plugin indent on

" Better scrolling experience
set scrolloff=8
set scrolljump=1

" Disable backup and swap files
set nobackup
set nowritebackup
set noswapfile

" Expand tab to spaces
set expandtab

" Ensure that CoC nicely
set hidden
set cmdheight=3
set updatetime=200

" Sign column shows up all the time
set signcolumn=yes

" Set relative number
set number
set relativenumber

" No folding
set nofoldenable

" Tab management
set splitright
set splitbelow

" Colorscheme
runtime! plugin/default.vim
if (has("termguicolors"))
    set termguicolors
endif
let dark_background = ($DARK_VIM == "1")
if dark_background
  "silent! colorscheme monokai_soda
  silent! colorscheme uwu
else
  silent! colorscheme github_light
  let $BAT_THEME='GitHub'
endif

hi Normal     guibg=NONE ctermbg=NONE
hi LineNr     guibg=NONE ctermbg=NONE
hi NonText    guibg=NONE ctermbg=NONE
hi SignColumn guibg=NONE ctermbg=NONE
hi StatusLine guibg=NONE ctermbg=NONE

" Statusline
set laststatus=0
set cmdheight=1

" Auto-delete trailing white space
autocmd FileType python,clojure autocmd BufWritePre <buffer> %s/\s\+$//e

" Does not release visual mode during block indentation
vnoremap < <gv
vnoremap > >gv

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                         Vim Remaps                             "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Jumping between tabs
nnoremap ( :tabprevious<CR>
nnoremap ) :tabnext<CR>

" Turning off highlights (after searching)
nnoremap <Space>nh :nohlsearch<CR>

" Remove trailing whitespace
nnoremap <Space>rw :keeppatterns %s/\s\+$//<CR>

" Insert [X] in front of word
nnoremap <Space>x ^xi* [X]<Esc>

" Panes
nnoremap <Space>sh :split<CR>
nnoremap <Space>sv :vsplit<CR>

" Writing and quiting
nnoremap <C-S> :w<CR>
nnoremap <C-X> :q<CR>

" Set completeopt to have a better completion experience
" :help completeopt
" menuone: popup even when there's only one match
" noinsert: Do not insert text until a selection is made
" noselect: Do not select, force user to select one from the menu
" Source: https://sharksforarms.dev/posts/neovim-rust/
set completeopt=menuone,noinsert,noselect
" Avoid showing extra messages when using completion
set shortmess+=c
