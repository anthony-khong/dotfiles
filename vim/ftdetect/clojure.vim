" Find documentation under cursor
nnoremap <Space>dd viw"xy:exe 'Doc ' . @x<CR>
vnoremap <Space>dd "xy:exe 'Doc ' . @x<CR>

" Vim-Iced
let g:iced#buffer#stdout#mods="vertical"
let g:iced_enable_auto_indent = v:false
let g:iced_enable_popup_document="any"

nnoremap <Space>cj :IcedJackIn<CR>
nnoremap <Space>cc :IcedConnect<CR>:IcedStdoutBufferOpen<CR>
nnoremap <Space>cl :IcedRequire<CR>
nnoremap <Space>cn :IcedEvalNs<CR>
nnoremap <Space>rl :IcedStdoutBufferClear<CR>
nnoremap <Space>cet :IcedEvalOuterTopList<CR>
vnoremap <Space>cet :IcedEvalReplVisual<CR>
nnoremap <Space>cit vi(:IcedEvalReplVisual<CR>
nnoremap <Space>> :IcedSlurp<CR>
nnoremap <Space>< :IcedBarf<CR>
