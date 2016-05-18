" Source: https://www.piware.de/2014/07/vim-config-for-markdownlatex-pandoc-editing/

setlocal autoindent
setlocal colorcolumn=0
setlocal linebreak
setlocal nonumber
setlocal shiftwidth=4
setlocal tabstop=4
setlocal wrap
setlocal textwidth=0
setlocal spell

syntax region Statement oneline matchgroup=Delimiter start="\$" end="\$"
syntax region Statement matchgroup=Delimiter start="\\begin{.*}" end="\\end{.*}" contains=Statement
syntax region Statement matchgroup=Delimiter start="{" end="}" contains=Statement

:syn match markdownIgnore "\$.*_.*\$"

" This gets rid of the nasty _ italic bug in tpope's vim-markdown
" block $$...$$
syn region math start=/\$\$/ end=/\$\$/
" inline math
syn match math '\$[^$].\{-}\$'

" actually highlight the region we defined as "math"
hi link math Statement

" Moving up and down physical lines
nnoremap j gj
nnoremap k gk
