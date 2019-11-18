" Vim-Fireplace
nnoremap <Space>fr :Require<CR>

" Vim-Iced
let g:iced#buffer#stdout#mods="vertical"
let g:iced_enable_auto_indent=v:false
let g:iced_enable_auto_document="normal"
let g:iced_enable_popup_document="full"

"" Cider connect and init
nnoremap <Space>cj :IcedJackIn<CR>
nnoremap <Space>cc :IcedConnect<CR>:IcedStdoutBufferOpen<CR>:exec 'vertical resize '. string(&columns * 0.65)<CR>
nnoremap <Space>cr :IcedRequire<CR>
nnoremap <Space>cR :IcedRequireAll<CR>
nnoremap <Space>cn :IcedEvalNs<CR>
nnoremap <Space>cf :IcedCommandPalette<CR>

"" Cider eval
nnoremap <Space>cet :IcedEvalOuterTopList<CR>
vnoremap <Space>cet :IcedEvalVisual<CR>
nnoremap <Space>cit va(:IcedEvalVisual<CR>
nnoremap <Space>ci :IcedInterrupt<CR>
nnoremap <Space>cw :IcedStdoutBufferClear<CR>

"" Cider edits
nnoremap <> :IcedSlurp<CR>
nnoremap >< :IcedBarf<CR>
nnoremap <Space>cl :IcedMoveToLet<CR>

"" Cider show source and docs
nnoremap <Space>css :IcedPopupSourceShow<CR>
nnoremap <Space>csd :IcedPopupDocumentOpen<CR>

"" Cider toggle test
nnoremap <Space>tt :IcedToggleSrcAndTest<CR>
