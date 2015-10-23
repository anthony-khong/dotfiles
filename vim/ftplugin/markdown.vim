" Source: https://www.piware.de/2014/07/vim-config-for-markdownlatex-pandoc-editing/

set textwidth=0
set colorcolumn=0
set noexpandtab
set wrap

function s:md_settings()
    inoremap <buffer> <Leader>n \note[item]{}<Esc>i
    noremap <buffer> <Leader>b :! pandoc -t beamer % -o %<.pdf<CR><CR>
    noremap <buffer> <Leader>l :! pandoc -t latex % -o %<.pdf<CR>
    noremap <buffer> <Leader>v :! evince %<.pdf 2>&1 >/dev/null &<CR><CR>

    adjust syntax highlighting for LaTeX parts
      inline formulas:
    syntax region Statement oneline matchgroup=Delimiter start="\$" end="\$"
      environments:
    syntax region Statement matchgroup=Delimiter start="\\begin{.*}" end="\\end{.*}" contains=Statement
      commands:
    syntax region Statement matchgroup=Delimiter start="{" end="}" contains=Statement
    nnoremap <buffer> <Leader>p :!pandoc -V geometry:margin=2cm -o "%:r.pdf" %<CR><CR>
endfunction

:call <SID>md_settings()

:syn match markdownIgnore "\$.*_.*\$"

