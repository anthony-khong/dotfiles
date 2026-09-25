#!/usr/bin/env bash
# Everything this Neovim config calls out to, on Ubuntu 24.04+. Safe to re-run.
# Not included: Elixir/Erlang themselves (ElixirLS needs OTP 26+).
set -euo pipefail

SUDO=""
if [ "$(id -u)" -ne 0 ]; then SUDO="sudo"; fi
case "$(uname -m)" in
x86_64)
  NVIM_ARCH=x86_64
  TS_ARCH=x64
  ;;
aarch64 | arm64)
  NVIM_ARCH=arm64
  TS_ARCH=arm64
  ;;
*)
  echo "Unsupported architecture: $(uname -m)" >&2
  exit 1
  ;;
esac

step() { printf '\n==> %s\n' "$*"; }
# version_ge A B: true when version A >= B
version_ge() { [ "$(printf '%s\n%s\n' "$1" "$2" | sort -V | head -1)" = "$2" ]; }

step "apt packages"
# build-essential: C compiler for treesitter parsers   ripgrep: Telescope live_grep
# shfmt and shellcheck: used by bash-language-server    pipx: Python tools
# xclip, wl-clipboard: system clipboard (clipboard=unnamedplus)
$SUDO apt-get update
$SUDO apt-get install -y ca-certificates curl git tar unzip build-essential ripgrep tmux \
  shellcheck shfmt pipx xclip wl-clipboard

step "Neovim 0.12+ (Ubuntu's own package is too old)"
nvim_v=$(nvim --version 2>/dev/null | sed -n '1s/^NVIM v\([0-9.]*\).*/\1/p' || true)
if [ -z "$nvim_v" ] || ! version_ge "$nvim_v" 0.12.0; then
  curl -fsSL -o /tmp/nvim.tar.gz \
    "https://github.com/neovim/neovim/releases/download/stable/nvim-linux-$NVIM_ARCH.tar.gz"
  $SUDO tar -xzf /tmp/nvim.tar.gz -C /opt
  $SUDO ln -sf "/opt/nvim-linux-$NVIM_ARCH/bin/nvim" /usr/local/bin/nvim
fi

step "tree-sitter CLI 0.26.1+ (nvim-treesitter builds parsers with it; apt's is too old)"
ts_v=$(tree-sitter --version 2>/dev/null | awk '{print $2}' || true)
if [ -z "$ts_v" ] || ! version_ge "$ts_v" 0.26.1; then
  curl -fsSL "https://github.com/tree-sitter/tree-sitter/releases/latest/download/tree-sitter-linux-$TS_ARCH.gz" |
    gunzip >/tmp/tree-sitter
  $SUDO install -m 755 /tmp/tree-sitter /usr/local/bin/tree-sitter
fi

step "Node.js 22.22+ (typescript-language-server needs it; apt's is too old)"
node_v=$(node --version 2>/dev/null | sed 's/^v//' || true)
if [ -z "$node_v" ] || ! version_ge "$node_v" 22.22.2; then
  curl -fsSL https://deb.nodesource.com/setup_24.x | ${SUDO:+$SUDO -E} bash -
  $SUDO apt-get install -y nodejs
fi

step "Language servers from npm: Bash, YAML, TypeScript, SQL"
$SUDO npm install -g bash-language-server yaml-language-server \
  typescript typescript-language-server sql-language-server

step "Python: pyrefly (completion, types) and ruff (lint, format)"
for tool in pyrefly ruff; do
  pipx list --short 2>/dev/null | grep -q "^$tool " || pipx install "$tool"
done
pipx ensurepath >/dev/null

step "Rust: rust-analyzer, rust-src (std completions), clippy (check on save), rustfmt"
if ! command -v rustup >/dev/null; then
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
echo "Open a new shell (pipx and rustup add to PATH), then start nvim once:"
echo "lazy.nvim installs the plugins and nvim-treesitter builds its parsers."
