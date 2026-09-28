# shellcheck shell=bash
# Helpers shared by install.sh, update.sh, setup/*.sh, setup/modules/*.sh and bash/recreate_symbolic_links
# shellcheck disable=SC2034 # CONFIG, SUDO, UPDATE_STAMP and NVIM_DATA are used by the scripts that source this

DOTFILES="${DOTFILES:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
CONFIG="${XDG_CONFIG_HOME:-$HOME/.config}"
MISE_SHIMS="${MISE_DATA_DIR:-$HOME/.local/share/mise}/shims"
# Touched by a clean ./update.sh; bash_preferences.sh reminds you when it gets old
UPDATE_STAMP="${XDG_STATE_HOME:-$HOME/.local/state}/dotfiles/last-update"
export PATH="$HOME/.local/bin:$HOME/.cargo/bin:$MISE_SHIMS:$PATH"

OS="$(uname -s)"
SUDO=""
if [ "$(id -u)" -ne 0 ]; then SUDO="sudo"; fi

step() { printf '\n==> %s\n' "$*"; }
note() { printf '    %s\n' "$*"; }
die() {
  printf 'error: %s\n' "$*" >&2
  exit 1
}
have() { command -v "$1" >/dev/null 2>&1; }
is_mac() { [ "$OS" = "Darwin" ]; }
is_ubuntu() { [ "$OS" = "Linux" ] && grep -qx 'ID=ubuntu' /etc/os-release 2>/dev/null; }

# link <path in repo> <destination>: leaves a correct link alone, replaces a stale
# one, and moves a real file or directory aside to <destination>.bak.<timestamp>
link() {
  local src="$DOTFILES/$1" dst="$2"
  if [ ! -e "$src" ]; then
    echo "skip, not in repo: $1" >&2
    return 0
  fi
  mkdir -p "$(dirname "$dst")"
  if [ -L "$dst" ]; then
    if [ "$dst" -ef "$src" ]; then return 0; fi
    rm "$dst"
  elif [ -e "$dst" ]; then
    mv "$dst" "$dst.bak.$(date +%Y%m%d%H%M%S)"
    echo "backed up: $dst"
  fi
  ln -s "$src" "$dst"
  echo "linked: $dst"
}

# at_least <version> <minimum>: compares major.minor, so "1.3.2-2.1ubuntu1" counts as 1.3
at_least() {
  awk -v v="$1" -v m="$2" 'BEGIN {
    split(v, a, /[^0-9]+/); split(m, b, /[^0-9]+/)
    exit !(a[1] + 0 > b[1] + 0 || (a[1] + 0 == b[1] + 0 && a[2] + 0 >= b[2] + 0)) }'
}

# Neovim checks for install.sh, update.sh and doctor.sh. Headless nvim exits 0 even when a
# plugin, parser or spell file fails to download, so these look at the results instead;
# each prints the names that are wrong, or nothing.
NVIM_DATA="${XDG_DATA_HOME:-$HOME/.local/share}/nvim"
lock_entries() { # lock_entries [lock file]: "name commit" for each plugin
  sed -n 's/^ *"\([^"]*\)": { "branch": "[^"]*", "commit": "\([0-9a-f]*\)" },\{0,1\}$/\1 \2/p' \
    "${1:-$DOTFILES/vim/lazy-lock.json}"
}
stale_plugins() { # stale_plugins [lock file]: not checked out at their commit in it
  local name commit
  lock_entries "${1:-}" | while read -r name commit; do
    if [ "$(git -C "$NVIM_DATA/lazy/$name" rev-parse HEAD 2>/dev/null)" != "$commit" ]; then
      printf '%s ' "$name"
    fi
  done
}
# run_lazy restore|update. After installing, lazy rewrites vim/lazy-lock.json without any
# plugin that failed, so the check runs against a copy taken first. A restore never changes
# the lock, so the copy goes back; an update's new commits stay, but no plugin may go missing.
run_lazy() {
  local lock="$DOTFILES/vim/lazy-lock.json" before missing="" name
  before="$(mktemp)"
  cp "$lock" "$before"
  nvim --headless "+Lazy! $1" +qa || missing="(nvim failed) "
  echo
  if [ "$1" = restore ]; then
    missing="$missing$(stale_plugins "$before")"
    cmp -s "$before" "$lock" || cp "$before" "$lock"
  else
    missing="$missing$(stale_plugins)"
    for name in $(lock_entries "$before" | cut -d' ' -f1); do
      lock_entries | awk -v n="$name" '$1 == n { found = 1 } END { exit !found }' || missing="$missing$name "
    done
  fi
  rm -f "$before"
  if [ -n "$missing" ]; then
    note "plugins not at vim/lazy-lock.json: $missing"
    return 1
  fi
}
missing_parsers() { # in vim/lua/treesitter_languages.lua but not installed
  local lang
  sed -n 's/^ *"\([a-z0-9_]*\)",.*/\1/p' "$DOTFILES/vim/lua/treesitter_languages.lua" | while read -r lang; do
    if [ ! -e "$NVIM_DATA/site/parser/$lang.so" ]; then printf '%s ' "$lang"; fi
  done
}
missing_spell() { # for a language in 'spelllang' (marked, as lazy may print progress too)
  nvim --headless -c 'lua local dir = vim.fn.stdpath("data") .. "/site/spell"
    for _, l in ipairs(vim.opt.spelllang:get()) do
      if vim.fn.filereadable(dir .. "/" .. l .. ".utf-8.spl") == 0 then io.stdout:write("\nmissing-spell:" .. l .. "\n") end
    end' +qa 2>/dev/null | sed -n 's/.*missing-spell:\([A-Za-z_-]*\).*/\1/p' | tr '\n' ' '
}
