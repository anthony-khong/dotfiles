" -----------------------------------------------------------------------------
" A color scheme using the Ubuntu palette.
"
" Note: Color codes got with http://www.vim.org/scripts/script.php?script_id=1349
" Author: @viniciusban
" -----------------------------------------------------------------------------

hi clear

if exists("syntax_on")
  syntax reset
endif

set t_Co=256

" Color definitions
let s:WHITE     = {"gui": "#FFFFFF", "cterm": "231"}
let s:BLACK     = {"gui": "#000000", "cterm": "black"}
let s:GRAY      = {"gui": "#C6C6C6", "cterm": "251"}
let s:DGRAY     = {"gui": "#626262", "cterm": "darkgray"}
let s:ORANGE    = {"gui": "#FF875F", "cterm": "209"}
let s:DORANGE   = {"gui": "#FF5F50", "cterm": "202"}
let s:LORANGE   = {"gui": "#FED7DB", "cterm": "224"}
let s:DMAGENTA  = {"gui": "#871879", "cterm": "89"}
let s:MAGENTA   = {"gui": "#AF005F", "cterm": "darkmagenta"}
let s:GREEN     = {"gui": "#00FF00", "cterm": "lightgreen"}
let s:DGREEN    = {"gui": "#00AF00", "cterm": "darkgreen"}
let s:YELLOW    = {"gui": "#FFFFAF", "cterm": "220"}
let s:RED       = {"gui": "#D70000", "cterm": "red"}
let s:DRED      = {"gui": "#840002", "cterm": "darkred"}
let s:I         = {"gui": "italic",  "cterm": "none"}
let s:NONE      = {"gui": "none", "cterm": "none"}

if (has('gui_running'))
    let s:M = "gui"
else
    let s:M = "cterm"
endif

if &background == "dark"
    let s:BG = s:NONE
    let s:REVERSED = s:WHITE
else
    let s:BG = s:WHITE
    let s:REVERSED = s:BLACK
endif

" Highlight groups

exe "hi Normal       " .s:M. "fg=" .s:DGRAY[s:M].     " " .s:M. "bg=" .s:BG[s:M].    " " .s:M. "=none"

exe "hi Comment      " .s:M. "fg=" .s:GRAY[s:M].                                        " " .s:M. "=" .s:I[s:M]
exe "hi String       " .s:M. "fg=" .s:ORANGE[s:M].                                      " " .s:M. "=none"
exe "hi Number       " .s:M. "fg=" .s:ORANGE[s:M].                                      " " .s:M. "=none"
exe "hi Boolean      " .s:M. "fg=" .s:DMAGENTA[s:M].                                    " " .s:M. "=none"
exe "hi Constant     " .s:M. "fg=" .s:ORANGE[s:M].                                      " " .s:M. "=none"
exe "hi Identifier   " .s:M. "fg=" .s:REVERSED[s:M].                                    " " .s:M. "=none"
exe "hi Function     " .s:M. "fg=" .s:MAGENTA[s:M].                                     " " .s:M. "=none"
exe "hi Statement    " .s:M. "fg=" .s:DMAGENTA[s:M].                                    " " .s:M. "=none"
exe "hi Operator     " .s:M. "fg=" .s:DMAGENTA[s:M].                                    " " .s:M. "=none"
exe "hi PreProc      " .s:M. "fg=" .s:REVERSED[s:M].                                    " " .s:M. "=none"
exe "hi Include      " .s:M. "fg=" .s:DMAGENTA[s:M].                                    " " .s:M. "=none"
exe "hi Type         " .s:M. "fg=" .s:DORANGE[s:M].                                     " " .s:M. "=none"
exe "hi Typedef      " .s:M. "fg=" .s:DMAGENTA[s:M].                                    " " .s:M. "=none"
exe "hi Error        " .s:M. "fg=" .s:WHITE[s:M].     " " .s:M. "bg=" .s:DRED[s:M].     " " .s:M. "=none"
exe "hi Todo         " .s:M. "fg=" .s:DGREEN[s:M].    " " .s:M. "bg=" .s:BLACK[s:M].    " " .s:M. "=bold"
exe "hi Special      " .s:M. "fg=" .s:DGREEN[s:M].                                      " " .s:M. "=none"
exe "hi Underlined   "                                                                  " " .s:M. "=underline"

exe "hi SpecialKey   " .s:M. "fg=" .s:DORANGE[s:M].   " " .s:M. "bg=" .s:DMAGENTA[s:M]. " " .s:M. "=none"
exe "hi NonText      " .s:M. "fg=" .s:WHITE[s:M].     " " .s:M. "bg=" .s:MAGENTA[s:M].  " " .s:M. "=none"
exe "hi EndOfBuffer  " .s:M. "fg=" .s:MAGENTA[s:M].   " " .s:M. "bg=" .s:BG[s:M].       " " .s:M. "=none"
exe "hi Ignore       " .s:M. "fg=" .s:DGRAY[s:M].                                       " " .s:M. "=none"
exe "hi Directory    " .s:M. "fg=" .s:MAGENTA[s:M].                                     " " .s:M. "=none"

exe "hi Title        " .s:M. "fg=" .s:ORANGE[s:M].    " " .s:M. "bg=" .s:DMAGENTA[s:M]. " " .s:M. "=none"
exe "hi ModeMsg      " .s:M. "fg=" .s:BLACK[s:M].     " " .s:M. "bg=" .s:DORANGE[s:M].  " " .s:M. "=none"
exe "hi MoreMsg      " .s:M. "fg=" .s:BLACK[s:M].     " " .s:M. "bg=" .s:DORANGE[s:M].  " " .s:M. "=none"
exe "hi Question     " .s:M. "fg=" .s:DORANGE[s:M].   " " .s:M. "bg=" .s:BG[s:M].       " " .s:M. "=none"
exe "hi WarningMsg   " .s:M. "fg=" .s:RED[s:M].                                         " " .s:M. "=none"

exe "hi Visual       " .s:M. "fg=" .s:WHITE[s:M].     " " .s:M. "bg=" .s:ORANGE[s:M].   " " .s:M. "=none"
exe "hi VisualNOS    " .s:M. "fg=" .s:WHITE[s:M].     " " .s:M. "bg=" .s:ORANGE[s:M].   " " .s:M. "=none"

exe "hi Search       " .s:M. "fg=" .s:YELLOW[s:M].    " " .s:M. "bg=" .s:DMAGENTA[s:M]. " " .s:M. "=bold"
exe "hi IncSearch    " .s:M. "fg=" .s:WHITE[s:M].     " " .s:M. "bg=" .s:DMAGENTA[s:M]. " " .s:M. "=bold"

exe "hi Cursor       " .s:M. "fg=" .s:BLACK[s:M].     " " .s:M. "bg=" .s:GREEN[s:M].    " " .s:M. "=none"
exe "hi CursorLine   " .s:M. "fg=" .s:NONE[s:M].      " " .s:M. "bg=" .s:BLACK[s:M].  " " .s:M. "=none"
exe "hi CursorColumn " .s:M. "fg=" .s:NONE[s:M].      " " .s:M. "bg=" .s:BLACK[s:M].  " " .s:M. "=none"
exe "hi ColorColumn  " .s:M. "fg=" .s:DORANGE[s:M].   " " .s:M. "bg=" .s:DMAGENTA[s:M]. " " .s:M. "=none"

exe "hi LineNr       " .s:M. "fg=" .s:DMAGENTA[s:M].                                    " " .s:M. "=none"
exe "hi CursorLineNr " .s:M. "fg=" .s:DORANGE[s:M].                                     " " .s:M. "=none"

exe "hi Folded       " .s:M. "fg=" .s:LORANGE[s:M].   " " .s:M. "bg=" .s:BG[s:M].       " " .s:M. "=none"
exe "hi FoldColumn   " .s:M. "fg=" .s:DMAGENTA[s:M].  " " .s:M. "bg=" .s:BG[s:M].       " " .s:M. "=none"
exe "hi SignColumn   " .s:M. "fg=" .s:DMAGENTA[s:M].  " " .s:M. "bg=" .s:BG[s:M].       " " .s:M. "=none"

exe "hi TabLine      " .s:M. "fg=" .s:DMAGENTA[s:M].  " " .s:M. "bg=" .s:BG[s:M].       " " .s:M. "=underline"
exe "hi TabLineFill  " .s:M. "fg=" .s:DMAGENTA[s:M].  " " .s:M. "bg=" .s:BG[s:M].       " " .s:M. "=underline"
exe "hi TabLineSel   " .s:M. "fg=" .s:DORANGE[s:M].   " " .s:M. "bg=" .s:DMAGENTA[s:M]. " " .s:M. "=underline"

exe "hi StatusLine   " .s:M. "fg=" .s:DORANGE[s:M].   " " .s:M. "bg=" .s:BG[s:M].       " " .s:M. "=underline,bold"
exe "hi StatusLineNC " .s:M. "fg=" .s:DMAGENTA[s:M].  " " .s:M. "bg=" .s:BG[s:M].       " " .s:M. "=underline"
exe "hi VertSplit    " .s:M. "fg=" .s:DMAGENTA[s:M].  " " .s:M. "bg=" .s:BG[s:M].       " " .s:M. "=none"

exe "hi WildMenu     " .s:M. "fg=" .s:WHITE[s:M].     " " .s:M. "bg=" .s:DMAGENTA[s:M]. " " .s:M. "=none"
exe "hi Menu         " .s:M. "fg=" .s:WHITE[s:M].     " " .s:M. "bg=" .s:DMAGENTA[s:M]. " " .s:M. "=none"

exe "hi DiffAdd      " .s:M. "fg=" .s:WHITE[s:M].    " " .s:M. "bg=" .s:DGREEN[s:M].    " " .s:M. "=none"
exe "hi DiffDelete   " .s:M. "fg=" .s:WHITE[s:M].    " " .s:M. "bg=" .s:DRED[s:M].      " " .s:M. "=none"
exe "hi DiffChange   " .s:M. "fg=" .s:BLACK[s:M].    " " .s:M. "bg=" .s:YELLOW[s:M].    " " .s:M. "=none"
exe "hi DiffText     " .s:M. "fg=" .s:BLACK[s:M].    " " .s:M. "bg=" .s:WHITE[s:M].     " " .s:M. "=underline"

" Diff specific (only for files, not for lines)
exe "hi diffNewFile  " .s:M. "fg=" .s:DGREEN[s:M].   " " .s:M. "bg=" .s:BG[s:M].        " " .s:M. "=none"
exe "hi diffOldFile  " .s:M. "fg=" .s:DRED[s:M].     " " .s:M. "bg=" .s:BG[s:M].        " " .s:M. "=none"

let g:colors_name = "ubuntu"
