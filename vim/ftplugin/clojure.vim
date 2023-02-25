" Conjure
nnoremap <Space>cb :ConjureConnect 4444<CR>

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
call s:vim_sexp_mappings()

" Parantheses
let g:rainbow#blacklist = [228, 231, 238, 245]
function! s:dim_closing_parens()
    hi link ClosingParens Comment
    call matchadd('ClosingParens', ')')
    call matchadd('ClosingParens', ']')
    call matchadd('ClosingParens', '}')
endfunction
call s:dim_closing_parens()
