-- LSP Status
require"fidget".setup{}

-- LSP Config
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
local opts = { noremap=true, silent=true }
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, opts)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, opts)
vim.keymap.set('n', '<space>ca', vim.lsp.buf.code_action, opts)

-- Show the current line's diagnostic message inline, errors first
vim.diagnostic.config({
  severity_sort = true,
  virtual_text = { current_line = true },
})

-- Format on save only for these servers
local format_on_save = { ElixirLS = true, ruff = true, ["rust-analyzer"] = true }

-- Mappings for every language server, once it attaches to a buffer
vim.api.nvim_create_autocmd("LspAttach", { callback = function(ev)
  local client = vim.lsp.get_client_by_id(ev.data.client_id)
  if client and format_on_save[client.name] then
    require("lsp-format").on_attach(client, ev.buf)
  end
  -- ruff's hover only explains noqa codes; leave K to pyrefly
  if client and client.name == "ruff" then
    client.server_capabilities.hoverProvider = false
  end

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  local bufopts = { noremap=true, silent=true, buffer=ev.buf }
  vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
  vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
  vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
  vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
  -- vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
  vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, bufopts)
  vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, bufopts)
  vim.keymap.set('n', '<space>wl', function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, bufopts)
  vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, bufopts)
  vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, bufopts)
  vim.keymap.set('n', '<space>ca', vim.lsp.buf.code_action, bufopts)
  -- nowait: otherwise `gr` waits 'timeoutlen' for Nvim's built-in grn/grr/gra/gri/grt/grx
  vim.keymap.set('n', 'gr', vim.lsp.buf.references, vim.tbl_extend('force', bufopts, { nowait = true }))
  -- Not <space>f, which waited 'timeoutlen' for <space>ff/fg/fb/fh/fp
  vim.keymap.set('n', '<space>fm', function() vim.lsp.buf.format { async = true } end, bufopts)
end })

-- Completion
-- pyrefly pads overloaded signatures into wide columns ("axis     : Axis     = 0,"),
-- which wrap into a mess in the docs window. Squeeze the padding out.
local function compact_signature(detail)
  if type(detail) ~= "string" then return detail end
  local lines = vim.split(detail, "\n")
  for i, line in ipairs(lines) do
    local indent, rest = line:match("^(%s*)(.*)$")
    lines[i] = indent .. rest:gsub("%s+:%s", ": "):gsub("%s%s+=%s", " = ")
  end
  return table.concat(lines, "\n")
end

require("blink.cmp").setup({
  -- Same keys as with nvim-cmp. When blink isn't using a key, "fallback" passes it
  -- to the vsnip maps in plugin_configs.lua (<Tab> expands, <C-n>/<C-p> jump).
  keymap = {
    preset = "none",
    ["<C-p>"] = { "select_prev", "fallback_to_mappings" },
    ["<C-n>"] = { "select_next", "fallback_to_mappings" },
    ["<S-Tab>"] = { function(cmp) return cmp.scroll_documentation_up(8) end, "fallback" },
    ["<Tab>"] = { function(cmp) return cmp.scroll_documentation_down(8) end, "fallback" },
    ["<C-e>"] = { "cancel", "fallback" },
    ["<C-y>"] = { "select_and_accept", "fallback" },
  },
  snippets = { preset = "vsnip" },
  completion = {
    -- Nothing selected until <C-n>/<C-p>, like completeopt=noselect
    list = { selection = { preselect = false } },
    documentation = {
      auto_show = true,
      auto_show_delay_ms = 0,
      draw = function(opts)
        opts.default_implementation({ detail = compact_signature(opts.item.detail) })
      end,
    },
    menu = {
      draw = {
        columns = { { "source_icon" }, { "label", "label_description", gap = 1 }, { "kind" } },
        components = {
          source_icon = {
            text = function(ctx)
              return ({ lsp = "λ", snippets = "⋗", buffer = "b", path = "p" })[ctx.source_id] or ""
            end,
          },
        },
      },
    },
  },
  -- Like keyword_length = 2 (3 for snippets); "." etc. still open the menu straight away
  sources = {
    min_keyword_length = 2,
    providers = { snippets = { min_keyword_length = 3 } },
  },
  signature = { enabled = true },
})

-- Elixir
local elixir = require("elixir")
local elixirls = require("elixir.elixirls")
local capabilities = require('blink.cmp').get_lsp_capabilities()

-- elixir-tools still calls the old API; send it to the 0.12 one
vim.lsp.codelens.refresh = function(opts)
  vim.lsp.codelens.enable(true, opts)
end

elixir.setup {
  credo = { enable = true, version = "0.3.0" },
  elixirls = {
    cmd = vim.fn.expand("~/.elixir-ls/release/language_server.sh"),
    capabilities = capabilities,
    settings = elixirls.settings { dialyzerEnabled = false },
    on_attach = function(_, bufnr)
      local map_opts = { buffer = bufnr }
      vim.keymap.set("n", "<space>fp", ":ElixirFromPipe<cr>", map_opts)
      vim.keymap.set("n", "<space>tp", ":ElixirToPipe<cr>", map_opts)
      vim.keymap.set("v", "<space>em", ":ElixirExpandMacro<cr>", map_opts)
    end,
  },
}

-- Python: pyrefly for completion, navigation and type errors; ruff for lint and format.
-- A project's own ruff config (pyproject.toml / ruff.toml) wins over these settings.
vim.lsp.config('ruff', {
  init_options = {
    settings = {
      configurationPreference = "filesystemFirst",
      lineLength = 96,
    },
  },
})
vim.lsp.enable({"pyrefly", "ruff"})

-- Rust: rustaceanvim starts rust-analyzer; check with clippy instead of `cargo check` on save
vim.g.rustaceanvim = {
  server = {
    default_settings = {
      ["rust-analyzer"] = { check = { command = "clippy" } },
    },
  },
}

-- TypeScript
vim.lsp.config('ts_ls', {})
vim.lsp.enable({"ts_ls"})

-- SQL
vim.lsp.enable({"sqlls"})

-- Shell
-- require('lspconfig').bashls.setup{}
