" Source: https://www.piware.de/2014/07/vim-config-for-markdownlatex-pandoc-editing/

set textwidth=0
set colorcolumn=0
set noexpandtab
set wrap

syntax region Statement oneline matchgroup=Delimiter start="\$" end="\$"
syntax region Statement matchgroup=Delimiter start="\\begin{.*}" end="\\end{.*}" contains=Statement
syntax region Statement matchgroup=Delimiter start="{" end="}" contains=Statement

:syn match markdownIgnore "\$.*_.*\$"
