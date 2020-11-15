" Conjure
let g:conjure#mapping#def_word = "d"
let g:conjure#log#hud#enabled = v:true
let g:conjure#log#hud#height = 0.95

nnoremap <Space>cb :ConjureConnect 4444<CR>
nnoremap <Space>cc mwvip:ConjureEval<CR>'wzz
nmap <Space>cl <Space>lv<C-W><C-H>:exe "vertical resize " . (winwidth(0) * 5/4)<CR>
