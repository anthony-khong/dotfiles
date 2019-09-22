nnoremap <silent> <Space>hi :Hindent<CR>

" Background process and window management
nnoremap <silent> <Space>is :InteroStart<CR>
nnoremap <silent> <Space>ik :InteroKill<CR>
nnoremap <silent> <Space>ir :InteroRestart<CR><C-W>H:vertical resize 90<CR>

" Open intero/GHCi split horizontally
nnoremap <silent> <Space>io :InteroOpen<CR>
nnoremap <silent> <Space>iov :InteroOpen<CR><C-W>H:vertical resize 90<CR>
nnoremap <silent> <Space>ih :InteroHide<CR>

" Send whatever is under the cursor
nnoremap <silent> <Space>ss :InteroSend <C-r><C-w><CR>
nnoremap <silent> <Space>go :InteroSend go<CR>

" Reloading (pick one)
" Automatically reload on save
au BufWritePost *.hs InteroReload

" Manually save and reload
nnoremap <silent> <Space>wr :w \| :InteroReload<CR>

" Load individual modules
nnoremap <silent> <Space>il :InteroLoadCurrentModule<CR>
nnoremap <silent> <Space>if :InteroLoadCurrentFile<CR>

" Type-related information
" Heads up! These next two differ from the rest.
map <silent> <Space>t <Plug>InteroGenericType
map <silent> <Space>T <Plug>InteroType
nnoremap <silent> <Space>it :InteroTypeInsert<CR>

" Navigation
" nnoremap <silent> <Space>jd :InteroGoToDef<CR>

" Managing targets
" Prompts you to enter targets (no silent):
nnoremap <Space>ist :InteroSetTargets<SPACE>
