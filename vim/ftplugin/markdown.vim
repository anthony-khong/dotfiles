" Source: https://www.piware.de/2014/07/vim-config-for-markdownlatex-pandoc-editing/

setlocal autoindent
setlocal colorcolumn=0
setlocal linebreak
setlocal nonumber
setlocal shiftwidth=4
setlocal tabstop=4
setlocal wrap
setlocal textwidth=0
"setlocal spell

" Moving up and down physical lines
nnoremap <buffer> <expr> j v:count ? 'j' : 'gj'
nnoremap <buffer> <expr> k v:count ? 'k' : 'gk'
