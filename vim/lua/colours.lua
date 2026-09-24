vim.opt.termguicolors = true

local colors = require('ayu.colors')
colors.generate()
require('ayu').setup({
  overrides = {
    ['@module'] = { fg = colors.entity },
    ['@string.special.symbol'] = { fg = colors.constant },
    ['@punctuation.bracket'] = { fg = colors.fg },
  },
})

vim.cmd([[
  colorscheme ayu-dark
  hi Pmenu guibg=#334351 guifg=#E6E1CF

  hi Normal     guibg=NONE ctermbg=NONE
  hi LineNr     guibg=NONE ctermbg=NONE
  hi NonText    guibg=NONE ctermbg=NONE
  hi SignColumn guibg=NONE ctermbg=NONE
  hi StatusLine guibg=NONE ctermbg=NONE
]])
