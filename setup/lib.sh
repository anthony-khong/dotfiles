# Helpers shared by install.sh, bash/recreate_symbolic_links and setup/modules/*.sh
# shellcheck disable=SC2034 # CONFIG, SUDO and UPDATE_STAMP are used by the scripts that source this

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
