#!/usr/bin/env bash
# Updates what install.sh set up, without sudo. Safe to re-run. Also available as
# `dotfiles-update`; the shell reminds you when it's been a while (bash_preferences.sh).
#   ./update.sh             pull the dotfiles; update mise tools, Rust, oh-my-zsh, tmux plugins
#                           and Homebrew; put Neovim plugins at the versions in vim/lazy-lock.json
#   ./update.sh --plugins   also move Neovim plugins to their newest versions. This changes
#                           vim/lazy-lock.json: commit it, and the other machines follow
set -uo pipefail # no -e: one failing step shouldn't stop the others

DOTFILES="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=setup/lib.sh
. "$DOTFILES/setup/lib.sh"

bump_plugins=0
case "${1:-}" in
"") ;;
--plugins) bump_plugins=1 ;;
-h | --help)
  sed -n '2,7s/^# \{0,1\}//p' "$0"
  exit 0
  ;;
*) die "unknown option: $1 (see --help)" ;;
esac

failed=""
run() { # run <label> <function>: runs one step and remembers it if it fails
  step "$1"
  "$2" || failed="$failed, $1"
}

pull_dotfiles() {
  git -C "$DOTFILES" pull --ff-only && bash "$DOTFILES/bash/recreate_symbolic_links"
}

update_mise() {
  # Homebrew's mise updates with `brew upgrade` below
  if [ "$(command -v mise)" = "$HOME/.local/bin/mise" ]; then mise self-update --yes || return; fi
  mise install --yes && mise upgrade --yes
}

update_omz() {
  local plugin
  ZSH="$HOME/.oh-my-zsh" zsh -f "$HOME/.oh-my-zsh/tools/upgrade.sh" -v minimal || return
  # `omz update` leaves the plugins cloned into custom/ alone
  for plugin in "${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}"/plugins/*/; do
    if [ -d "$plugin.git" ]; then git -C "$plugin" pull -q --ff-only || return; fi
  done
}

update_nvim() {
  if [ "$bump_plugins" -eq 1 ]; then
    nvim --headless "+Lazy! update" +qa || return
  else
    nvim --headless "+Lazy! restore" +qa || return
  fi
  nvim --headless -c 'lua local ts = require("nvim-treesitter"); ts.install(require("treesitter_languages")):wait(900000); ts.update():wait(900000)' +qa
  echo
}

update_tmux_plugins() { # tpm itself included; no tmux server needed
  local plugin
  for plugin in "$HOME"/.tmux/plugins/*/; do
    if [ -d "$plugin.git" ]; then git -C "$plugin" pull -q --ff-only || return; fi
  done
}

update_rust() { rustup update; }

update_brew() {
  local tmux_before tmux_after
  # A running tmux server keeps the old version, which a new client can't attach to
  tmux_before="$(tmux list-sessions >/dev/null 2>&1 && tmux -V)"
  brew update && brew upgrade || return
  tmux_after="$(tmux -V 2>/dev/null)"
  if [ -n "$tmux_before" ] && [ "$tmux_after" != "$tmux_before" ]; then
    note "tmux went from ${tmux_before#tmux } to ${tmux_after#tmux }, but the running server is still the old"
    note "one, and a new tmux won't attach to it ('open terminal failed: not a terminal'). When it"
    note "suits you: save the sessions (prefix C-s), tmux kill-server, start tmux, restore (prefix C-r)."
  fi
  if /usr/libexec/ApplicationFirewall/socketfilterfw --getglobalstate | grep -q enabled; then
    note "If mosh was upgraded, re-allow it through the firewall: bash setup/modules/mac-server.sh"
  fi
}

cd "$HOME" || exit 1
if have git; then run "dotfiles repo, then links" pull_dotfiles; fi
if have mise; then run "mise tools" update_mise; fi
if have rustup; then run "Rust" update_rust; fi
if [ -d "$HOME/.oh-my-zsh" ]; then run "oh-my-zsh and its plugins" update_omz; fi
if have nvim; then run "Neovim plugins and parsers" update_nvim; fi
if [ -d "$HOME/.tmux/plugins" ]; then run "tmux plugins" update_tmux_plugins; fi
if is_mac && have brew; then run "Homebrew" update_brew; fi

echo
if [ -n "$failed" ]; then
  echo "These steps failed (see above): ${failed#, }. The reminder stays on until a clean run."
  exit 1
fi
mkdir -p "$(dirname "$UPDATE_STAMP")" && touch "$UPDATE_STAMP"
if [ "$bump_plugins" -eq 1 ]; then
  git -C "$DOTFILES" diff --stat -- vim/lazy-lock.json
  note "Commit vim/lazy-lock.json so the other machines get the same plugin versions."
fi
echo "Done. Reload tmux (prefix r) for plugin changes, and open a new shell for the rest."
