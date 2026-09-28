#!/usr/bin/env bash
# Reports what's installed and what needs fixing. Run it as ./install.sh --doctor.
set -uo pipefail
# shellcheck source=lib.sh
. "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib.sh"

problems=0
ok() { printf '  ok   %-30s %s\n' "$1" "$2"; }
fix() {
  printf '  FIX  %-30s %s\n' "$1" "$2"
  problems=$((problems + 1))
}
info() { printf '  --   %-30s %s\n' "$1" "$2"; }

# tool <command> [version command...]; "-" skips running it (some servers only speak LSP)
tool() {
  local cmd="$1" out
  shift
  if ! have "$cmd"; then
    fix "$cmd" "not found"
    return
  fi
  if [ "${1:-}" = "-" ]; then
    ok "$cmd" "installed"
    return
  fi
  if [ $# -eq 0 ]; then set -- "$cmd" --version; fi
  if out="$("$@" 2>&1)"; then
    ok "$cmd" "$(echo "$out" | grep -m1 -Eo '[0-9]+(\.[0-9]+)+[a-z]?' | head -1)"
  else
    fix "$cmd" "installed but fails: $(echo "$out" | head -1 | cut -c1-70)"
  fi
}

linked() { # linked <destination> <path in repo>
  local tilde="~" name
  name="${1/#$HOME/$tilde}"
  if [ "$1" -ef "$DOTFILES/$2" ]; then ok "$name" "-> $2"; else fix "$name" "not linked to $2 (run bash/recreate_symbolic_links)"; fi
}

echo "Terminal and editor"
tool git
tool tmux tmux -V
for cmd in mosh mosh-server zsh mise nvim tree-sitter rg fd fzf jq uv; do tool "$cmd"; done
for cmd in mosh mosh-server; do
  if have "$cmd"; then
    version="$("$cmd" --version 2>/dev/null | grep -Eo 'mosh [0-9.]+' | head -1 | cut -d' ' -f2)"
    if ! at_least "$version" 1.4; then
      fix "$cmd $version" "OSC 52 copy and true colour need mosh 1.4+ on both ends (re-run ./install.sh)"
    fi
  fi
done

echo "Languages"
tool erl erl -noshell -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().'
tool elixir elixir --short-version
tool python
tool node
tool rustc
tool cargo

echo "Language servers, linters, formatters"
tool expert
tool pyrefly
tool ruff
tool rust-analyzer
tool shellcheck
tool shfmt
tool bash-language-server
tool yaml-language-server -
tool tombi
tool typescript-language-server
tool sql-language-server -
if [ -e "$CONFIG/mise/conf.d/jvm.toml" ]; then
  tool java java -version
  tool clojure-lsp
  tool metals -
  tool jdtls -
fi

echo "Setup"
linked "$HOME/.bashrc" bash/bashrc
if is_mac; then linked "$HOME/.bash_profile" bash/bashrc; fi
linked "$HOME/.zshrc" bash/zshrc
linked "$HOME/.zshenv" bash/zshenv
linked "$HOME/.tmux.conf" tmux/tmux.conf
if is_mac || have ghostty; then linked "$CONFIG/ghostty/config.ghostty" ghostty/config.ghostty; fi
if infocmp -x xterm-ghostty >/dev/null 2>&1; then ok "terminfo" "xterm-ghostty"; else fix "terminfo" "no xterm-ghostty (run bash/recreate_symbolic_links)"; fi
linked "$CONFIG/nvim" vim
linked "$CONFIG/mise/conf.d/dotfiles.toml" mise/dotfiles.toml
ssh_config=\~/.ssh/config
if grep -qsF "Include \"$DOTFILES/ssh/config\"" "$HOME/.ssh/config"; then
  ok "$ssh_config" "includes ssh/config"
else
  fix "$ssh_config" "doesn't include ssh/config (run bash/recreate_symbolic_links)"
fi
if [ -e "$CONFIG/mise/conf.d/jvm.toml" ]; then linked "$CONFIG/mise/conf.d/jvm.toml" mise/jvm.toml; fi

if is_mac; then
  login_shell="$(dscl . -read "/Users/$(id -un)" UserShell 2>/dev/null | awk '{print $2}')"
else
  login_shell="$(getent passwd "$(id -un)" | cut -d: -f7)"
fi
case "$login_shell" in
*/zsh) ok "login shell" "$login_shell" ;;
*) fix "login shell" "${login_shell:-unknown}, expected zsh" ;;
esac
if have tailscale && tailscale status >/dev/null 2>&1; then
  ok "tailscale" "$(tailscale ip -4 | head -1)"
else
  info "tailscale" "not connected (./install.sh --with tailscale)"
fi
if [ -d "$HOME/.oh-my-zsh" ]; then ok "oh-my-zsh" "installed"; else fix "oh-my-zsh" "not installed"; fi
if [ -n "${MISE_SHELL:-}" ]; then
  ok "mise activated" "in $MISE_SHELL"
else
  info "mise activated" "not in this shell; new terminals activate it via bash_preferences.sh"
fi

if is_ubuntu; then
  if locale -a 2>/dev/null | grep -qiE '^en_US\.utf-?8$'; then ok "locale" "en_US.UTF-8"; else fix "locale" "en_US.UTF-8 missing (sudo locale-gen en_US.UTF-8)"; fi
fi

nvim_retry="re-run with --skip packages,mise,rust,zsh"
stale="$(stale_plugins)"
if [ -z "$stale" ]; then ok "Neovim plugins" "at vim/lazy-lock.json"; else fix "Neovim plugins" "not at vim/lazy-lock.json: $stale($nvim_retry)"; fi
missing="$(missing_parsers)"
if [ -z "$missing" ]; then ok "treesitter parsers" "all of treesitter_languages.lua"; else fix "treesitter parsers" "missing: $missing($nvim_retry)"; fi
if have nvim; then
  missing="$(missing_spell)"
  if [ -z "$missing" ]; then ok "spell files" "for every 'spelllang'"; else fix "spell files" "missing: $missing($nvim_retry)"; fi
fi
if [ -d "$HOME/.tmux/plugins/tpm" ]; then
  ok "tmux plugins" "$(find "$HOME/.tmux/plugins" -mindepth 1 -maxdepth 1 -type d | wc -l | tr -d ' ') installed"
else
  info "tmux plugins" "tmux installs them on first start"
fi

echo
if [ "$problems" -eq 0 ]; then
  echo "All good."
else
  echo "$problems to fix."
  exit 1
fi
