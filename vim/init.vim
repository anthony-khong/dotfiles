"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                          Plugin Settings                          "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set runtimepath^=~/.config/nvim

call plug#begin()

" Appearance
Plug 'airblade/vim-gitgutter'
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

" Clojure
Plug 'Olical/conjure', {'tag': 'v4.25.0'}
Plug 'eraserhd/parinfer-rust', {'do': 'cargo build --release'}
Plug 'guns/vim-sexp'
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'tpope/vim-sexp-mappings-for-regular-people'

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
			\ 'coc-eslint',
			\ 'coc-json',
			\ 'coc-prettier',
			\ 'coc-pyright',
			\ 'coc-tsserver',
			\ ]
nmap <silent> <localleader>cd <Plug>(coc-definition)
nmap <silent> <localleader>ci <Plug>(coc-implementation)
nmap <silent> <localleader>cf <Plug>(coc-references)
nmap <silent> <localleader>ct <Plug>(coc-type-definition)
nmap <silent> <localleader>cp <Plug>(coc-diagnostic-prev)
nmap <silent> <localleader>cn <Plug>(coc-diagnostic-next)
nmap <localleader>cr <Plug>(coc-rename)
nmap <localleader>cd <Plug>(coc-codeaction)

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

" NERDTree
nnoremap <localleader>bj :NERDTreeToggle<CR>

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

" JSONC
nnoremap <localleader>jc :set ft=jsonc<CR>

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
silent! colorscheme monokai_soda
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
