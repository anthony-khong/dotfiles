"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                          Plugin Settings                          "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set runtimepath^=~/.config/nvim

call plug#begin()

" Appearance
Plug 'itchyny/lightline.vim'
Plug 'tanvirtin/monokai.nvim'
Plug 'vimpostor/vim-tpipeline'

" Editing
Plug 'junegunn/vim-easy-align'
Plug 'scrooloose/nerdcommenter'

" Tmux
Plug 'christoomey/vim-tmux-navigator'

" JavaScript
Plug 'leafgarland/typescript-vim'
Plug 'maxmellon/vim-jsx-pretty'
Plug 'pangloss/vim-javascript'

" Completions
Plug 'neoclide/coc.nvim', {'branch': 'release'}

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

" Lightline
let g:tpipeline_cursormoved = 1
set guicursor=
source ~/.config/nvim/lightline.vim
let g:lightline = {
    \ 'colorscheme': 'custom_black',
    \ }
let g:lightline.active = {
    \ 'left': [ [ 'left-edge' ],
    \           [ 'readonly', 'space', 'filename', 'space', 'modified' ],
    \           [ 'right-edge', 'space', 'left-edge' ],
    \           [ 'mode' ],
    \           [ 'right-edge', 'space'  ],
    \           [ 'percent', 'lineinfo' ] ],
    \ 'right': [ ]
    \ }
let g:lightline.tabline = {
    \ 'left': [ [ 'tabs' ] ],
    \ 'right': [ ] }
let g:lightline.tabline_separator = {
    \ 'left': "\uE0B4",
    \ 'right': "" }
let g:lightline.component = {
	\ 'left-edge': "\uE0B6",
	\ 'right-edge': "\uE0B4",
    \ 'space': ' ',
	\ 'mode': '%{lightline#mode()}',
    \ }
let g:lightline.component_type = {
    \ 'left-edge': 'raw',
    \ 'mode': 'raw',
    \ 'filename': 'raw',
    \ 'readonly': 'raw',
    \ 'modified': 'raw',
    \ 'space': 'raw',
    \ 'right-edge': 'raw'
    \ }
let g:lightline.separator = { 'left': '', 'right': '' }
let g:lightline.subseparator = { 'left': '', 'right': '' }
let g:lightline.mode_map = {
    \ 'n' : 'N',
    \ 'i' : 'I',
    \ 'R' : 'R',
    \ 'v' : 'V',
    \ 'V' : 'VL',
    \ "\<C-v>": 'VB',
    \ 'c' : 'C',
    \ 's' : 'S',
    \ 'S' : 'SL',
    \ "\<C-s>": 'SB',
    \ 't': 'T',
    \ }

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                         Vim Settings                           "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Makes copy and pasting work
set clipboard=unnamed
set clipboard+=unnamedplus

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
