set nocompatible hidden laststatus=2

set runtimepath^=~/.config/nvim

call plug#begin()

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

call plug#end()

let g:fzf_layout = { 'window': { 'width': 0.9, 
                               \ 'height': 0.67,
                               \ 'border': 'rounded' } }
let g:fzf_action = {
            \'enter': 'tabedit',
            \'ctrl-v': 'vsplit',
            \'ctrl-t': 'tabedit'}

nnoremap <Space>be :call fzf#vim#gitfiles('', fzf#vim#with_preview('down'))<CR>
