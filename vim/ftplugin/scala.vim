set tabstop=2
set shiftwidth=2
set expandtab

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
        let b:MarkMargin = matchadd('ColorColumn', '\%101v', 100)
    endif
endfunction

augroup MarkMargin
    autocmd!
    autocmd  BufEnter  *.scala    :call MarkMargin(1)
    autocmd  BufEnter  *.sc    :call MarkMargin(1)
    autocmd  BufEnter  *.vp*   :call MarkMargin(0)
augroup END
