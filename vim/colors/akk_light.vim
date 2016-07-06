" Inspired by vim-monochrome

set background=light

hi clear
if exists('syntax_on')
   syntax reset
endif

let g:colors_name = 'monochrome'

" These commands are generated, see bin/generate.rb.
hi Normal                   ctermfg=232  ctermbg=255 gui=NONE      cterm=NONE      term=NONE
hi Cursor                   ctermfg=255  ctermbg=232 gui=NONE      cterm=NONE      term=NONE
hi CursorLine               ctermfg=232  ctermbg=234 gui=NONE      cterm=NONE      term=NONE
hi CursorLineNr             ctermfg=232  ctermbg=255 gui=bold      cterm=bold      term=bold
hi ColorColumn              ctermfg=232  ctermbg=234 gui=NONE      cterm=NONE      term=NONE
hi FoldColumn               ctermfg=248  ctermbg=255 gui=NONE      cterm=NONE      term=NONE
hi Folded                   ctermfg=232  ctermbg=255 gui=NONE      cterm=NONE      term=NONE
hi LineNr                   ctermfg=248  ctermbg=255 gui=NONE      cterm=NONE      term=NONE
hi Statement                ctermfg=232  ctermbg=255 gui=bold      cterm=bold      term=bold
hi PreProc                  ctermfg=232  ctermbg=255 gui=bold      cterm=bold      term=bold
hi String                   ctermfg=27   ctermbg=255 gui=NONE      cterm=NONE      term=NONE
hi Comment                  ctermfg=243  ctermbg=255 gui=NONE      cterm=NONE      term=NONE
hi Constant                 ctermfg=232  ctermbg=255 gui=NONE      cterm=NONE      term=NONE
hi Type                     ctermfg=232  ctermbg=255 gui=bold      cterm=bold      term=bold
hi Function                 ctermfg=232  ctermbg=255 gui=NONE      cterm=NONE      term=NONE
hi Identifier               ctermfg=232  ctermbg=255 gui=NONE      cterm=NONE      term=NONE
hi Special                  ctermfg=232  ctermbg=255 gui=NONE      cterm=NONE      term=NONE
hi MatchParen               ctermfg=255  ctermbg=232 gui=NONE      cterm=NONE      term=NONE
hi pythonEscape             ctermfg=67   ctermbg=255 gui=NONE      cterm=NONE      term=NONE
hi Search                   ctermfg=232  ctermbg=67  gui=NONE      cterm=NONE      term=NONE
hi Visual                   ctermfg=232  ctermbg=67  gui=NONE      cterm=NONE      term=NONE
hi NonText                  ctermfg=248  ctermbg=255 gui=NONE      cterm=NONE      term=NONE
hi Directory                ctermfg=232  ctermbg=255 gui=bold      cterm=bold      term=bold
hi Title                    ctermfg=232  ctermbg=255 gui=bold      cterm=bold      term=bold
hi markdownHeadingDelimiter ctermfg=232  ctermbg=255 gui=bold      cterm=bold      term=bold
hi markdownHeadingRule      ctermfg=232  ctermbg=255 gui=bold      cterm=bold      term=bold
hi markdownLinkText         ctermfg=67   ctermbg=255 gui=underline cterm=underline term=underline
hi Todo                     ctermfg=255  ctermbg=226 gui=bold      cterm=bold      term=bold
hi Pmenu                    ctermfg=232  ctermbg=67  gui=NONE      cterm=NONE      term=NONE
hi PmenuSel                 ctermfg=67   ctermbg=232 gui=NONE      cterm=NONE      term=NONE
hi helpSpecial              ctermfg=232  ctermbg=255 gui=NONE      cterm=NONE      term=NONE
hi helpHyperTextJump        ctermfg=67   ctermbg=255 gui=underline cterm=underline term=underline
hi helpNote                 ctermfg=232  ctermbg=255 gui=NONE      cterm=NONE      term=NONE
hi vimOption                ctermfg=232  ctermbg=255 gui=NONE      cterm=NONE      term=NONE
hi vimGroup                 ctermfg=232  ctermbg=255 gui=NONE      cterm=NONE      term=NONE
hi vimHiClear               ctermfg=232  ctermbg=255 gui=NONE      cterm=NONE      term=NONE
hi vimHiGroup               ctermfg=232  ctermbg=255 gui=NONE      cterm=NONE      term=NONE
hi vimHiAttrib              ctermfg=232  ctermbg=255 gui=NONE      cterm=NONE      term=NONE
hi vimHiGui                 ctermfg=232  ctermbg=255 gui=NONE      cterm=NONE      term=NONE
hi vimHiGuiFgBg             ctermfg=232  ctermbg=255 gui=NONE      cterm=NONE      term=NONE
hi vimHiCTerm               ctermfg=232  ctermbg=255 gui=NONE      cterm=NONE      term=NONE
hi vimHiCTermFgBg           ctermfg=232  ctermbg=255 gui=NONE      cterm=NONE      term=NONE
hi vimSynType               ctermfg=232  ctermbg=255 gui=NONE      cterm=NONE      term=NONE
hi vimCommentTitle          ctermfg=243  ctermbg=255 gui=NONE      cterm=NONE      term=NONE
