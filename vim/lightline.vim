" Reference:
" https://github.com/itchyny/lightline.vim/blob/master/autoload/lightline/colorscheme/ayu_dark.vim

let s:base_bg = '#000000'
let s:base_fg = '#ffffff'
let s:text_bg = '#0087ff'
let s:text_fg = '#ffffff'
let s:unselected_bg = '#808080'

let s:p = {
    \ 'normal': {}, 
    \ 'inactive': {}, 
    \ 'insert': {}, 
    \ 'replace': {}, 
    \ 'visual': {}, 
    \ 'tabline': {}
    \ }
let s:p.normal.left = [ 
    \ [ s:text_bg, s:base_bg ], 
    \ [ s:text_fg, s:text_bg ], 
    \ [ s:text_bg, s:base_bg ], 
    \ [ s:text_fg, s:text_bg ], 
    \ [ s:text_bg, s:base_bg ], 
    \ [ s:base_fg, s:base_bg ], 
    \ [ s:text_bg, s:base_bg ]
    \ ]
let s:p.normal.middle = [ 
    \ [ s:base_fg, s:base_bg ], 
    \ ]
let s:p.normal.right = [ 
    \ [ s:base_fg, s:base_bg ], 
    \ ]
let s:p.tabline.left = [ 
    \ [ s:base_fg, s:unselected_bg ], 
    \ ]
let s:p.tabline.tabsel = [ 
    \ [ s:text_fg, s:text_bg ], 
    \ ]

let g:lightline#colorscheme#custom_black#palette = lightline#colorscheme#fill(s:p)

