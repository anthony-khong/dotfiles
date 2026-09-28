#!/usr/bin/env bash
# tp: go to a tmux session. With no argument, pick one with fzf from the open sessions and
# the directories in ~/repos, plus ~/dotfiles and any listed in ~/.config/local/tp-dirs (one
# per line). A directory's session is named after it, as tnew names them, and is created
# on first use. Outside tmux it attaches; inside, it switches. prefix f runs it in a popup.
#   tp          pick
#   tp DIR      straight to DIR's session (tnew is `tp .`)
set -euo pipefail

DOTFILES="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
extra="${XDG_CONFIG_HOME:-$HOME/.config}/local/tp-dirs"

die() {
  printf 'tp: %s\n' "$*" >&2
  exit 1
}

name_for() { # a directory's session name: its basename, with . and : made _ as tmux does
  local name="${1%/}"
  name="${name##*/}"
  printf '%s\n' "${name//[.:]/_}"
}

go() { # go <session> [directory to start it in]
  if ! tmux has-session -t "=$1" 2>/dev/null; then
    tmux new-session -d -s "$1" -c "${2:-$PWD}"
  fi
  if [ -n "${TMUX:-}" ]; then
    tmux switch-client -t "=$1"
  else
    exec tmux attach-session -t "=$1"
  fi
}

dirs() {
  local d
  for d in "$HOME"/repos/*/; do
    d="${d%/}"
    if [ -d "$d" ] && [ "${d##*/}" != archived ]; then printf '%s\n' "$d"; fi
  done
  printf '%s\n' "$DOTFILES"
  if [ -r "$extra" ]; then
    while IFS= read -r d || [ -n "$d" ]; do
      case "$d" in "" | "#"*) continue ;; "~"*) d="$HOME${d#\~}" ;; esac
      if [ -d "$d" ]; then printf '%s\n' "${d%/}"; fi
    done <"$extra"
  fi
}

# Lines of <kind> TAB <session or directory> TAB <what fzf shows>: the open sessions, most
# recently used first, then the directories that don't have one yet
entries() {
  local open current="" name dir tilde="~"
  open="$(tmux list-sessions -F '#{session_last_attached} #{session_name}' 2>/dev/null |
    sort -rn | cut -d' ' -f2-)" || open=""
  if [ -n "${TMUX:-}" ]; then current="$(tmux display-message -p '#S')"; fi
  while IFS= read -r name; do
    if [ -n "$name" ] && [ "$name" != "$current" ]; then
      printf 's\t%s\t\033[34m●\033[0m %s\n' "$name" "$name"
    fi
  done <<<"$open"
  dirs | awk '!seen[$0]++' | while IFS= read -r dir; do
    if ! printf '%s\n' "$open" | grep -qxF -- "$(name_for "$dir")"; then
      printf 'd\t%s\t  %s\n' "$dir" "${dir/#$HOME/$tilde}"
    fi
  done
}

pick() {
  local choice kind target
  local fzf_opts=(--ansi --delimiter='\t' --with-nth=3 --reverse --no-multi --prompt='tmux> ')
  if [ "${1:-}" != --popup ]; then fzf_opts+=(--height=40%); fi
  command -v fzf >/dev/null || die "needs fzf"
  choice="$(entries | fzf "${fzf_opts[@]}")" || exit 0 # Esc
  kind="${choice%%$'\t'*}"
  target="${choice#*$'\t'}"
  target="${target%%$'\t'*}"
  if [ "$kind" = s ]; then go "$target"; else go "$(name_for "$target")" "$target"; fi
}

command -v tmux >/dev/null || die "needs tmux"
case "${1:-}" in
"" | --popup) pick "${1:-}" ;;
-h | --help) sed -n '2,7s/^# \{0,1\}//p' "$0" ;;
*)
  [ -d "$1" ] || die "no such directory: $1"
  dir="$(cd "$1" && pwd)"
  go "$(name_for "$dir")" "$dir"
  ;;
esac
