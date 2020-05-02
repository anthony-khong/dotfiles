" Conjure
let g:conjure_config = {
    \ "mappings.def-word": "d",
    \ "log.hud.enabled?": v:true,
    \ "log.hud.height": "0.95"
    \ }

nnoremap <Space>cc mwvip:ConjureEval<CR>'wzz
nmap <Space>cl <Space>lv<C-W><C-H>:exe "vertical resize " . (winwidth(0) * 5/4)<CR>
