let dark_background = 1

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                          Plugin Settings                          "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set runtimepath^=~/.config/nvim

call plug#begin()

Plug 'liuchengxu/vim-better-default'

" Editing
Plug 'SirVer/ultisnips', { 'on': [] }
Plug 'ervandew/supertab'
Plug 'honza/vim-snippets'
Plug 'junegunn/vim-easy-align'
Plug 'scrooloose/nerdcommenter'
Plug 'tpope/vim-surround'

" Motions
Plug 'easymotion/vim-easymotion'
Plug 'ggVGc/vim-fuzzysearch'
Plug 'vim-scripts/ctags.vim'

" Completion + Linting
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'w0rp/ale'

" Visualisation
Plug 'airblade/vim-gitgutter'
Plug 'chuling/equinusocio-material.vim'
Plug 'itchyny/lightline.vim'
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'ncm2/float-preview.nvim'
Plug 'patstockwell/vim-monokai-tasty'
Plug 'plasticboy/vim-markdown'
Plug 'romgrk/github-light.vim'

" Utilities
Plug 'christoomey/vim-tmux-navigator'
Plug 'jpalardy/vim-slime'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'scrooloose/nerdtree'
Plug 'tmux-plugins/vim-tmux-focus-events'
Plug 'tpope/vim-fugitive'

" Lisp
Plug 'Olical/conjure', {'tag': 'v4.1.0'}
Plug 'bakpakin/fennel.vim'
Plug 'eraserhd/parinfer-rust'
Plug 'guns/vim-clojure-highlight'
Plug 'guns/vim-clojure-static'
Plug 'guns/vim-sexp'
Plug 'tpope/vim-sexp-mappings-for-regular-people'

" Python
Plug 'davidhalter/jedi-vim'
Plug 'sbdchd/neoformat'
Plug 'zchee/deoplete-jedi'

" Docker
Plug 'ekalinin/Dockerfile.vim'

call plug#end()


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                         Vim Settings                           "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Set the leader to comma
let mapleader = ","
let maplocalleader="\<space>"

" Makes copy and pasting work
set clipboard=unnamed
set clipboard+=unnamedplus

" One tab equals to four spaces
set tabstop=4
set softtabstop=4
set shiftwidth=4

" Use multiple of shiftwidth when indenting with '<' and '>'
set shiftround

" Expand tab to spaces
set expandtab

" Search case insensitive when all characters are lower case
set ignorecase
set smartcase

" No more weird characters when pressing <Esc>
set ttimeout
set ttimeoutlen=0

" Does not release visual mode during block indentation
vnoremap < <gv
vnoremap > >gv

" Enable syntax highlighting
syntax on
filetype plugin indent on

" More line buffer
set so=3

" Disable stupid backup and swap files - they trigger too many events
" for file system watchers
set nobackup
set nowritebackup
set noswapfile

" Tab management
set splitright
set splitbelow

" Sign column shows up all the time
set signcolumn=yes

" Colorscheme
runtime! plugin/default.vim
if $USER != "ubuntu"
    if (has("termguicolors"))
        set termguicolors
    endif
endif
if dark_background
    set background=dark
    let g:equinusocio_material_style = 'pure'
    let g:equinusocio_material_hide_vertsplit = 1
    let g:equinusocio_material_bracket_improved = 1
    set fillchars+=vert:│
    colorscheme equinusocio_material
    "silent! colorscheme vim-monokai-tasty
else
    set background=light
    "silent! colorscheme akk_light
    silent! colorscheme github-light
endif
hi Normal  guibg=NONE ctermbg=NONE
hi LineNr  guibg=NONE ctermbg=NONE
hi NonText guibg=NONE ctermbg=NONE

" Floating window
let g:float_preview#docked = 0
let g:float_preview#max_width = 80
let g:float_preview#max_height = 40

" Word wrapping
set wrap

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                           Vim Bindings                           "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Jump to escape mode
inoremap hh <Esc>
inoremap thhh th<Esc>
inoremap jj <Esc>
tnoremap <Esc> <C-\><C-n>

" Moving up and down physical lines
nnoremap j gj
nnoremap k gk

" Page up and down, then centers the cursor
noremap <C-u> <S-Up>zz
noremap <C-d> <S-Down>zz

" Jump to next method
nmap }} ]m
nmap {{ [m

" Centers screen whenever a search is found
nnoremap G Gzz
nnoremap n nzz
nnoremap N Nzz

" Since <C-a> is the Tmux prefix, we replace <C-a> with <C-o>
inoremap <C-o> <C-a>

" Repeat dot on multiple lines
vnoremap . :normal .<CR>

" Adds numbered jumps to jump list
nnoremap <expr> k (v:count > 1 ? "m'" . v:count : '') . 'k'
nnoremap <expr> j (v:count > 1 ? "m'" . v:count : '') . 'j'

" Make Ctrl-e jump to the end of the current line in the insert mode. This is
" handy when you are in the middle of a line and would like to go to its end
" without switching to the normal mode.
inoremap <C-e> <C-o>$

" Jumping between tabs
nnoremap ( :tabprevious<CR>
nnoremap ) :tabnext<CR>

" Jumping between Vim panes
nnoremap <Space>j <C-W><C-J>
nnoremap <Space>k <C-W><C-K>
nnoremap <Space>l <C-W><C-L>
nnoremap <Space>h <C-W><C-H>

" Navigating Jumps
nnoremap <Space>o <C-O>
nnoremap <Space>i <C-I>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                          Plugin Settings                          "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Ale
let g:ale_linters = {
      \ 'python': ['pyflakes', 'mypy'],
      \ 'clojure': ['clj-kondo']
      \}
let g:ale_python_mypy_options = "–ignore-missing-imports"
let g:ale_linters_explicit = 1
let g:ale_completion_enabled = 1
let g:ale_sign_error = '>>'
let g:ale_sign_warning = '!!'
hi ALEErrorSign   ctermbg=234         ctermfg=9  guibg=234 guifg=9
hi ALEWarningSign ctermbg=234         ctermfg=11 guibg=234 guifg=11
hi ALEWarning     ctermbg=DarkMagenta guibg=DarkMagenta
let g:airline#extensions#ale#enabled = 1

" Deoplete
let g:deoplete#enable_at_startup = 1
call deoplete#custom#option('keyword_patterns', {'clojure': '[\w!$%&*+/:<=>?@\^_~\-\.#]*'})
set completeopt-=preview

" Neoformat
" Enable alignment
let g:neoformat_basic_format_align = 1
" Enable tab to spaces conversion
let g:neoformat_basic_format_retab = 1
" Enable trimmming of trailing whitespace
let g:neoformat_basic_format_trim = 1

" Lazy loading for UltiSnips
augroup load_ultisnips
  autocmd!
  autocmd InsertEnter * call plug#load('ultisnips') | autocmd! load_ultisnips
augroup END

" Lightline
"let g:lightline = {
      "\ 'colorscheme': 'one',
      "\ }
let g:lightline = {
  \ 'colorscheme': 'equinusocio_material',
  \ }

" Tmux Navigator
if exists('$TMUX')
    function! TmuxOrSplitSwitch(wincmd, tmuxdir)
    let previous_winnr = winnr()
    silent! execute "wincmd " . a:wincmd
    if previous_winnr == winnr()
        call system("tmux select-pane -" . a:tmuxdir)
        redraw!
    endif
    endfunction

    let previous_title = substitute(system("tmux display-message -p '#{pane_title}'"), '\n', '', '')
    let &t_ti = "\<Esc>]2;vim\<Esc>\\" . &t_ti
    let &t_te = "\<Esc>]2;". previous_title . "\<Esc>\\" . &t_te

    nnoremap <silent> <C-h> :call TmuxOrSplitSwitch('h', 'L')<cr>
    nnoremap <silent> <C-j> :call TmuxOrSplitSwitch('j', 'D')<cr>
    nnoremap <silent> <C-k> :call TmuxOrSplitSwitch('k', 'U')<cr>
    nnoremap <silent> <C-l> :call TmuxOrSplitSwitch('l', 'R')<cr>
    tnoremap <silent> <C-h> <C-\><C-n>:call TmuxOrSplitSwitch('h', 'L')<cr>
    tnoremap <silent> <C-j> <C-\><C-n>:call TmuxOrSplitSwitch('j', 'D')<cr>
    tnoremap <silent> <C-k> <C-\><C-n>:call TmuxOrSplitSwitch('k', 'U')<cr>
    tnoremap <silent> <C-l> <C-\><C-n>:call TmuxOrSplitSwitch('l', 'R')<cr>
else
    map <C-h> <C-w>h
    map <C-j> <C-w>j
    map <C-k> <C-w>k
    map <C-l> <C-w>l
endif

" NERDCommenter
let g:NERDCreateDefaultMappings = 0
nmap gcc <Plug>NERDCommenterToggle
vmap gcc <Plug>NERDCommenterToggle

" SuperTab
let g:SuperTabDefaultCompletionType = "<c-n>"

" EasyMotion
let g:EasyMotion_smartcase = 1
let g:EasyMotion_keys = "aoeui'l;z,rqv.cjwyfxbdhtns"

" FuzzySearch
let g:fzf_action = {
            \'enter': 'tabedit',
            \'ctrl-v': 'vsplit',
            \'ctrl-t': 'tabedit'}

" UltiSnips
let g:UltiSnipsExpandTrigger = "<Tab>"
let g:UltiSnipsJumpForwardTrigger = "<C-b>"
let g:UltiSnipsJumpBackwardTrigger = "<C-z>"
let g:UltiSnipsSnippetDirectories = ["UltiSnips", "custom_snippets"]

" Easy Align
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" vim-sexp
let g:sexp_enable_insert_mode_mappings = 0

" Parinfer
let g:parinfer_mode = 'smart'
let g:parinfer_enabled = 1
let g:parinfer_force_balance = 0

" Rainbow Parantheses
augroup rainbow_lisp
  autocmd!
  autocmd FileType lisp,clojure,scheme RainbowParentheses
augroup END
let g:rainbow#blacklist = [228, 231, 238, 245]

" Slime
let g:slime_target = "tmux"
silent! let g:slime_default_config = {
    \ "socket_name": split($TMUX, ",")[0],
    \ "target_pane": ":0.1"
    \ }
let g:slime_dont_ask_default = 1
let g:slime_python_ipython = 1

" Jedi
" disable autocompletion, cause we use deoplete for completion
let g:jedi#completions_enabled = 0
" open the go-to function in split, not another buffer
let g:jedi#use_splits_not_buffers = "right"

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                     Leader + Space Remaps                        "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Reloading
nnoremap <Space>sv :source $INIT_VIM<CR>:echo "init.vim reloaded!"<CR>

" Turning off highlights (after searching)
nnoremap <Space>nh :nohlsearch<CR>

" Close current tab
nnoremap <Space>eq :q<CR>

" Tabs (misnamed as buffers)
nnoremap <Space>be :FZF<CR>
nnoremap <Space>bh :FZF ~<CR>
nnoremap <Space>bj :NERDTreeToggle<CR>
nnoremap <Space>bn :tabnext<CR>
nnoremap <Space>bp :tabprevious<CR>
nnoremap <Space>bs :update<CR>
nnoremap <Space>bx :q<CR>
if !exists('g:lasttab')
  let g:lasttab = 1
endif
nnoremap <Space>bb :exe "tabn ".g:lasttab<CR>
au TabLeave * let g:lasttab = tabpagenr()

" Remove trailing whitespace
nnoremap <silent> <Space>rw :keeppatterns %s/\s\+$//<CR>
autocmd FileType python,clojure autocmd BufWritePre <buffer> %s/\s\+$//e

" Shortuct to reload vimrc
nmap <Space>rv :so ~/.config/nvim/init.vim<CR>:echo '.nvimrc reloaded!'<CR>

" Panes
nnoremap <Space>ah :split<CR>
nnoremap <Space>av :vsplit<CR>
nnoremap <Space>ax :q<CR>
nnoremap <Space>th :split \| term<CR>
nnoremap <Space>tv :vsplit \| term<CR>

" Copy the entire script to the clipboard, to top and to bottom
nnoremap <Space>ya mwggVG"+y'wzz
nnoremap <Space>yk mwggV'w"+y'wzz
nnoremap <Space>yj mwGV'w"+y'wzz

" Map sort function to a key
vnoremap <Space>s :sort<CR>

" Switch back and forth between relative and absolute number
nnoremap <Space>rn :set relativenumber!<CR>

" Git
nnoremap <Space>gg :GitGutter<CR>
nnoremap <Space>gd :Gdiff<CR>
nnoremap <Space>gb :Gblame<CR>
highlight SignColumn guibg=NONE ctermbg=NONE

" Jumps
" This is to get rid of awful <Leader><Leader> default
map <Space><Space>em <Plug>(easymotion-prefix)
nmap <Space><Space> <Plug>(easymotion-s)
vmap <Space><Space> <Plug>(easymotion-s)

" Slurps and Barfs
let g:sexp_filetypes = ''

function! s:vim_sexp_mappings()
    nmap <silent><buffer> [[        <Plug>(sexp_move_to_prev_top_element)
    nmap <silent><buffer> ]]        <Plug>(sexp_move_to_next_top_element)
    nmap <silent><buffer> <Space>ih <Plug>(sexp_insert_at_list_head)
    nmap <silent><buffer> <Space>it <Plug>(sexp_insert_at_list_tail)
    nmap <silent><buffer> <Space>w( <Plug>(sexp_round_head_wrap_element)
    nmap <silent><buffer> <Space>w) <Plug>(sexp_round_tail_wrap_element)
    nmap <silent><buffer> <Space>w{ <Plug>(sexp_curly_head_wrap_element)
    nmap <silent><buffer> <Space>w} <Plug>(sexp_curly_tail_wrap_element)
    nmap <silent><buffer> <Space>w[ <Plug>(sexp_square_head_wrap_element)
    nmap <silent><buffer> <Space>w] <Plug>(sexp_square_tail_wrap_element)
    nmap <silent><buffer> <Space>rl <Plug>(sexp_raise_list)
    nmap <silent><buffer> <Space>re <Plug>(sexp_raise_element)

    nmap <buffer> {{ <Plug>(sexp_move_to_prev_element_head)
    nmap <buffer> }} <Plug>(sexp_move_to_next_element_head)
    nmap <buffer> }{ <Plug>(sexp_swap_element_backward)
    nmap <buffer> {} <Plug>(sexp_swap_element_forward)

    nmap <buffer> >< <Plug>(sexp_emit_tail_element)
    nmap <buffer> <> <Plug>(sexp_capture_next_element)
endfunction

augroup VIM_SEXP_MAPPING
    autocmd!
    autocmd FileType clojure,scheme,lisp,timl call s:vim_sexp_mappings()
augroup END


function! s:dim_closing_parens()
    if &background ==# 'dark'
        hi ClosingParens ctermfg=237 guifg=grey23
    else
        hi ClosingParens ctermfg=251 guifg=grey78
    endif
    call matchadd('ClosingParens', ')')
    call matchadd('ClosingParens', ']')
    call matchadd('ClosingParens', '}')
endfunction

autocmd FileType clojure,scheme,lisp,timl call s:dim_closing_parens()
