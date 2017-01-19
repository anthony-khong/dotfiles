" Python shortcuts
nnoremap <C-t> opass # TODO<C-c>
nnoremap <Leader>m Oimport matplotlib.pyplot as plt<C-c>
nnoremap db Oimport pdb;pdb.set_trace() <C-c>

" Get a line of #'s
nnoremap <Leader>3 i#<esc>78.b
nnoremap <Leader>4 i#<esc>74.b
nnoremap <Leader>5 i#<esc>70.b

" Set character limiter
" Taken from: https://github.com/thoughtstream/Damian-Conway-s-Vim-Setup/blob/master/.vimrc
highlight ColorColumn ctermbg=red
highlight ColorColumn guibg=red
function! MarkMargin (on)
    if exists('b:MarkMargin')
        try
            call matchdelete(b:MarkMargin)
        catch /./
        endtry
        unlet b:MarkMargin
    endif
    if a:on
        let b:MarkMargin = matchadd('ColorColumn', '\%91v', 100)
    endif
endfunction

augroup MarkMargin
    autocmd!
    autocmd  BufEnter  *.py    :call MarkMargin(1)
    autocmd  BufEnter  *.vp*   :call MarkMargin(0)
augroup END

" Foldings
setlocal foldmethod=indent
setlocal foldlevel=99
setlocal nowrap

" Simpyl Fold
let g:SimpylFold_docstring_preview = 1

" Jedi-Vim
let g:jedi#force_py_version=3
let g:jedi#auto_initialization = 1
let g:jedi#popup_on_dot = 0
let g:jedi#popup_select_first = 0
let g:jedi#smart_auto_mappings = 0
let g:jedi#show_call_signatures = "1"
let g:jedi#completions_enabled = 1
let g:jedi#use_tabs_not_buffers = 1
