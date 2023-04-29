-- LSP Status
require"fidget".setup{}

-- LSP Config
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
local opts = { noremap=true, silent=true }
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, opts)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, opts)

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  require("lsp-format").on_attach(client)

  -- Enable completion triggered by <c-x><c-o>
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  local bufopts = { noremap=true, silent=true, buffer=bufnr }
  vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
  vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
  vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
  vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
  vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
  vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, bufopts)
  vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, bufopts)
  vim.keymap.set('n', '<space>wl', function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, bufopts)
  vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, bufopts)
  vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, bufopts)
  vim.keymap.set('n', '<space>ca', vim.lsp.buf.code_action, bufopts)
  vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
  vim.keymap.set('n', '<space>f', function() vim.lsp.buf.format { async = true } end, bufopts)
end

-- Completion
local cmp = require'cmp'
cmp.setup({
  snippet = {
    expand = function(args)
      vim.fn["vsnip#anonymous"](args.body)
    end,
  },
  mapping = {
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<C-n>'] = cmp.mapping.select_next_item(),
    ['<S-Tab>'] = cmp.mapping.scroll_docs(-8),
    ['<Tab>'] = cmp.mapping.scroll_docs(8),
    ['<C-e>'] = cmp.mapping.abort(),
    ['<C-y>'] = cmp.mapping.confirm({ select = true }),
    ['<CR>'] = cmp.mapping.confirm({ select = true })
  },
  window = {
    completion = cmp.config.window.bordered(),
    documentation = cmp.config.window.bordered(),
  },
  formatting = {
    fields = {'menu', 'abbr', 'kind'},
    format = function(entry, item)
      local menu_icon ={
        nvim_lsp = 'λ',
        vsnip = '⋗',
        buffer = 'b',
        path = 'p'
      }
      item.menu = menu_icon[entry.source.name]
      return item
    end,
  },
  sources = cmp.config.sources({
    { name = 'nvim_lsp', keyword_length = 3 },
    { name = 'nvim_lsp_signature_help', keyword_length = 3 },
    { name = 'nvim_lua', keyword_length = 3 },
    { name = 'buffer', keyword_length = 3 },
    { name = 'vsnip', keyword_length = 3 },
  })
})

-- Elixir
local elixir = require("elixir")
local elixirls = require("elixir.elixirls")
local capabilities = require('cmp_nvim_lsp').default_capabilities()

elixir.setup {
  elixirls = {
    cmd = vim.fn.expand("~/.elixir-ls/release/language_server.sh"),
    autostart = true,
    on_attach = function(client, bufnr)
      on_attach(client, bufnr)
      local map_opts = { buffer = true, noremap = true}

      vim.keymap.set("n", "<space>fp", ":ElixirFromPipe<cr>", map_opts)
      vim.keymap.set("n", "<space>tp", ":ElixirToPipe<cr>", map_opts)
      vim.keymap.set("v", "<space>em", ":ElixirExpandMacro<cr>", map_opts)
    end
  },
  capabilities = capabilities
}

vim.cmd([[
  au BufRead,BufNewFile *.eex,*.heex,*.leex,*.sface,*.lexs set filetype=eelixir
  au BufWritePost *.ex,*.eex,*.exs,*.heex lua vim.lsp.buf.format()
]])

-- Tailwind
require('lspconfig').tailwindcss.setup {
  init_options = {
    userLanguages = {
      elixir = "phoenix-heex",
      heex = "phoenix-heex",
      surface = "phoenix-heex"
    },
  },
  settings = {
    includeLanguages = {
      ["html-eex"] = "html",
      ["phoenix-heex"] = "html",
      heex = "html",
      eelixir = "html",
      elixir = "html",
      surface = "html"
    },
  },
}

-- Emmet
local lspconfig = require('lspconfig')
local configs = require('lspconfig/configs')
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

lspconfig.emmet_ls.setup({
    on_attach = on_attach,
    capabilities = capabilities,
    filetypes = { "css", "eruby", "html", "javascript", "javascriptreact", "less", "sass", "scss", "svelte", "pug", "typescriptreact", "vue", "elixir" },
    init_options = {
      html = {
        options = {
          ["bem.enabled"] = true,
        },
      },
    }
})

-- Rust
require'lspconfig'.rust_analyzer.setup{}

local opts = {
  tools = {
    runnables = {
      use_telescope = true,
    },
    inlay_hints = {
      auto = true,
      show_parameter_hints = false,
      parameter_hints_prefix = "",
      other_hints_prefix = "",
    },
  },
  server = {
    on_attach = on_attach,
    settings = {
      ["rust-analyzer"] = {
        checkOnSave = {
          command = "clippy",
        },
      },
    },
  },
}

require("rust-tools").setup(opts)

-- Python
require('lspconfig').pyright.setup {
  on_attach = on_attach
}

require('lspconfig').pylsp.setup {
  settings = {
    pylsp = {
      plugins = {
        pylint = { enabled = false },
        pycodestyle = { enabled = false },
        pyflakes = { enabled = false },
        flake8 = {
          enabled = true,
          ignore = { 'W503' },
          maxLineLength = 100
        }
      }
    }
  },
  on_attach = on_attach
}

-- TypeScript
require'lspconfig'.tsserver.setup {
  on_attach = on_attach
}
