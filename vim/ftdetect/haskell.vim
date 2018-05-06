
au FileType haskell nnoremap <silent> <Space>hi :Hindent<CR>

" Background process and window management
au FileType haskell nnoremap <silent> <Space>is :InteroStart<CR>
au FileType haskell nnoremap <silent> <Space>ik :InteroKill<CR>
au FileType haskell nnoremap <silent> <Space>ir :InteroRestart<CR><C-W>H:vertical resize 90<CR>

" Open intero/GHCi split horizontally
au FileType haskell nnoremap <silent> <Space>io :InteroOpen<CR>
au FileType haskell nnoremap <silent> <Space>iov :InteroOpen<CR><C-W>H:vertical resize 90<CR>
au FileType haskell nnoremap <silent> <Space>ih :InteroHide<CR>

" Reloading (pick one)
" Automatically reload on save
au BufWritePost *.hs InteroReload

" Manually save and reload
au FileType haskell nnoremap <silent> <Space>wr :w \| :InteroReload<CR>

" Load individual modules
au FileType haskell nnoremap <silent> <Space>il :InteroLoadCurrentModule<CR>
au FileType haskell nnoremap <silent> <Space>if :InteroLoadCurrentFile<CR>

" Type-related information
" Heads up! These next two differ from the rest.
au FileType haskell map <silent> <Space>t <Plug>InteroGenericType
au FileType haskell map <silent> <Space>T <Plug>InteroType
au FileType haskell nnoremap <silent> <Space>it :InteroTypeInsert<CR>

" Navigation
au FileType haskell nnoremap <silent> <Space>jd :InteroGoToDef<CR>

" Managing targets
" Prompts you to enter targets (no silent):
au FileType haskell nnoremap <Space>ist :InteroSetTargets<SPACE>
