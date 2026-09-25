#!/usr/bin/env bash
# Everything this Neovim config calls out to, on macOS with Homebrew. Safe to re-run.
# Not included: Elixir/Erlang themselves (ElixirLS needs OTP 26+).
set -euo pipefail

step() { printf '\n==> %s\n' "$*"; }

command -v brew >/dev/null || {
  echo "Install Homebrew first: https://brew.sh" >&2
  exit 1
}
# C compiler for treesitter parsers
xcode-select -p >/dev/null 2>&1 || {
  echo "Run 'xcode-select --install' first" >&2
  exit 1
}

step "Homebrew packages"
# tree-sitter-cli: nvim-treesitter builds parsers with it   ripgrep: Telescope live_grep
# shfmt and shellcheck: used by bash-language-server
brew install neovim tree-sitter-cli git ripgrep tmux \
  ruff pyrefly \
  bash-language-server shellcheck shfmt \
  yaml-language-server \
  typescript-language-server sql-language-server

step "Rust: rust-analyzer, rust-src (std completions), clippy (check on save), rustfmt"
if ! command -v rustup >/dev/null; then
  if command -v cargo >/dev/null; then
    echo "cargo is installed without rustup (Homebrew's rust?). Switch to rustup, then re-run." >&2
    exit 1
  fi
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
  # shellcheck source=/dev/null
  . "$HOME/.cargo/env"
fi
rustup component add rust-analyzer rust-src clippy rustfmt

step "ElixirLS (lsp.lua starts ~/.elixir-ls/release/language_server.sh)"
if [ ! -x "$HOME/.elixir-ls/release/language_server.sh" ]; then
  latest=$(curl -fsSLI -o /dev/null -w '%{url_effective}' https://github.com/elixir-lsp/elixir-ls/releases/latest)
  tag=${latest##*/}
  curl -fsSL -o /tmp/elixir-ls.zip \
    "https://github.com/elixir-lsp/elixir-ls/releases/download/$tag/elixir-ls-$tag.zip"
  mkdir -p "$HOME/.elixir-ls/release"
  unzip -oq /tmp/elixir-ls.zip -d "$HOME/.elixir-ls/release"
  chmod +x "$HOME/.elixir-ls/release/"*.sh
fi

step "Done"
echo "Start nvim once: lazy.nvim installs the plugins and nvim-treesitter builds its parsers."
