" Conjure
let g:conjure_log_auto_close=v:false
let g:conjure_quick_doc_normal_mode=v:false
let g:conjure_log_size_small=38

nnoremap <Space>tm :split \| term lein midje :autotest<CR>
