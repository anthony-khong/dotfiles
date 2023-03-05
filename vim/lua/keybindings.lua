-- Functional wrapper for mapping custom keybindings
function map(mode, lhs, rhs, opts)
    local options = { noremap = true }
    if opts then
        options = vim.tbl_extend("force", options, opts)
    end
    vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

-- Does not release visual mode during block indentation
map("v", "<", "<gv")
map("v", ">", ">gv")

-- Jumping between tabs
map("n", "(", ":tabprevious<CR>")
map("n", ")", ":tabnext<CR>")

-- Turning off highlights (after searching)
map("n", "<Space>nh", ":nohlsearch<CR>")

-- Remove trailing whitespace
map("n", "<Space>rw", ":keeppatterns %s/\\s\\+$//<CR>")

-- Insert [X] in front of word
map("n", "<Space>x", "^xi* [X]<Esc>")

-- Panes
map("n", "<Space>sh", ":split<CR>")
map("n", "<Space>sv", ":vsplit<CR>")

-- Writing and quiting
map("n", "<C-S>", ":w<CR>")
map("n", "<C-X>", ":q<CR>")
