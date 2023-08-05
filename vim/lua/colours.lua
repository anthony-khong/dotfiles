vim.opt.termguicolors = true

vim.cmd([[
  " colorscheme github_light
  let ayucolor="dark"
  silent! colorscheme ayu
  hi Pmenu guibg=#334351 guifg=#E6E1CF

  hi Normal     guibg=NONE ctermbg=NONE
  hi LineNr     guibg=NONE ctermbg=NONE
  hi NonText    guibg=NONE ctermbg=NONE
  hi SignColumn guibg=NONE ctermbg=NONE
  hi StatusLine guibg=NONE ctermbg=NONE
]])
