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

" Dockerfile
Plug 'ekalinin/Dockerfile.vim'

" Markdown
Plug 'plasticboy/vim-markdown'

call plug#end()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                         Plugin Settings                        "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" CoC
let g:coc_global_extensions = ['coc-tsserver']

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
nnoremap <Space>be :call fzf#vim#files('', fzf#vim#with_preview('down:72%'))<CR>
nnoremap <Space>bh :call fzf#vim#files('~', fzf#vim#with_preview('down:72%'))<CR>

" NERDTree
nnoremap <Space>bj :NERDTreeToggle<CR>

" Fugitive
nnoremap <Space>gd :Gdiff<CR>
nnoremap <Space>gb :Gblame<CR>

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

" Sign column shows up all the time
set signcolumn=yes

" Set relative number
set number
set relativenumber

" No folding
set foldlevelstart=1

" One status line
set laststatus=0

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
