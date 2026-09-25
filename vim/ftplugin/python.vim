setlocal nowrap
setlocal colorcolumn=96

nnoremap <buffer> <Leader>m Oimport matplotlib.pyplot as plt<Esc>
nnoremap <buffer> <Leader>3 i#<Esc>78.b

" Send the `# %%` cell under the cursor to the slime pane (IPython)
let b:slime_cell_delimiter = "# %%"
nmap <buffer> <leader>cc <Plug>SlimeSendCell
