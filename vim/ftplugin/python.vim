" Run current Python script
nnoremap <Leader><Space> :!ipython -i %<CR>

" Python shortcuts
map <Leader>t Oimport pdb; pdb.set_trace() # BREAKPOINT<C-c>
map <Leader>m Oimport matplotlib.pyplot as plt<C-c>
map <C-t> opass # TODO<C-c>

" Wrap a block/line with a try except block
vnoremap <Space>te >gvdOtry:<C-c>mqo<BS>except Exception as msg:<C-c>oprint msg<CR>import pdb; pdb.set_trace() # BREAKPOINT<C-C>kkP'q
nnoremap <Space>te V>gvdOtry:<C-c>mqo<BS>except Exception as msg:<C-c>oprint msg<CR>import pdb; pdb.set_trace() # BREAKPOINT<C-C>kkP'q

" Get a line of #'s
nnoremap <Leader>3 i#<esc>78.b
nnoremap <Leader>4 i#<esc>74.b
nnoremap <Leader>5 i#<esc>70.b

" Set character limiter
set colorcolumn=80
highlight ColorColumn ctermbg=234

" Jedi-Vim
" let g:jedi#usages_command = "<Space>;"
let g:jedi#popup_on_dot = 0
let g:jedi#popup_select_first = 0
" let g:jedi#completions_command = "<Space>c"
let g:jedi#smart_auto_mappings = 0

