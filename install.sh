#!/usr/bin/env bash
# Sets up this machine from the dotfiles. Safe to re-run.
#   ./install.sh                        base setup
#   ./install.sh --with jvm,docker      plus modules: data docker gitlab-runner jvm mac-server swap tailscale
#   ./install.sh --skip packages,rust   skip steps: packages link mise rust zsh nvim
#   ./install.sh --doctor               only report what's installed
# Supports macOS (Homebrew) and Ubuntu 22.04+, on x86_64 or arm64.
set -euo pipefail

DOTFILES="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
. "$DOTFILES/setup/lib.sh"

modules=""
skip=""
while [ $# -gt 0 ]; do
  case "$1" in
  --with | --skip)
    [ $# -ge 2 ] || die "$1 needs a comma-separated list"
    if [ "$1" = "--with" ]; then modules="${2//,/ }"; else skip="${2//,/ }"; fi
    shift 2
    ;;
  --doctor) exec bash "$DOTFILES/setup/doctor.sh" ;;
  -h | --help)
    sed -n '2,7s/^# \{0,1\}//p' "$0"
    exit 0
    ;;
  *) die "unknown option: $1 (see --help)" ;;
  esac
done
for m in $modules; do
  [ -f "$DOTFILES/setup/modules/$m.sh" ] || die "unknown module: $m"
done
is_mac || is_ubuntu || die "only macOS and Ubuntu are supported"

wanted() { case " $skip " in *" $1 "*) return 1 ;; *) return 0 ;; esac }

# The prebuilt tree-sitter CLI needs glibc 2.39+ (Ubuntu 24.04); on older boxes cargo builds it
old_glibc=0
if is_ubuntu; then
  glibc="$(getconf GNU_LIBC_VERSION | awk '{print $2}')"
  awk -v v="$glibc" 'BEGIN { split(v, a, "."); exit !(a[1] > 2 || (a[1] == 2 && a[2] >= 39)) }' || old_glibc=1
fi
failed=""
started=$(date +%s)
cd "$HOME"

if wanted packages; then
  if is_mac; then . "$DOTFILES/setup/macos.sh"; else . "$DOTFILES/setup/ubuntu.sh"; fi
fi

if wanted link; then
  step "Linking configs"
  bash "$DOTFILES/bash/recreate_symbolic_links"
fi

if wanted mise; then
  step "mise: languages, editor and CLI tools (mise/dotfiles.toml)"
  if ! have mise; then
    is_mac && die "mise should have come from setup/Brewfile"
    curl -fsSL https://mise.run | MISE_INSTALL_PATH="$HOME/.local/bin/mise" sh
  fi
  if [ "$old_glibc" -eq 1 ] && ! mise settings get disable_tools 2>/dev/null | grep -q tree-sitter; then
    note "glibc $glibc is too old for the prebuilt tree-sitter: the rust step builds it instead"
    mise settings add disable_tools tree-sitter
    mise reshim
  fi
  mise install --yes || failed="$failed mise"
fi

if wanted rust; then
  step "Rust via rustup, with rust-analyzer, rust-src, clippy and rustfmt"
  if ! have rustup; then
    have cargo && die "found cargo without rustup (Homebrew's rust?): remove it and re-run"
    # --no-modify-path: bash_preferences.sh already puts ~/.cargo/bin on PATH
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --no-modify-path
  fi
  rustup component add rust-analyzer rust-src clippy rustfmt
  if [ "$old_glibc" -eq 1 ] && [ ! -x "$HOME/.cargo/bin/tree-sitter" ]; then
    note "building tree-sitter CLI (glibc $glibc), about 3 minutes"
    cargo install --locked tree-sitter-cli
  fi
fi

if wanted zsh; then
  step "zsh with oh-my-zsh, zsh-syntax-highlighting and zsh-autosuggestions"
  have zsh || die "zsh is missing (it comes with macOS and from setup/apt.txt)"
  if [ ! -d "$HOME/.oh-my-zsh" ]; then
    # KEEP_ZSHRC: leave the ~/.zshrc link from the link step alone
    curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh |
      KEEP_ZSHRC=yes sh -s -- --unattended >/dev/null
  fi
  plugins="${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/plugins"
  for repo in zsh-users/zsh-syntax-highlighting zsh-users/zsh-autosuggestions; do
    [ -d "$plugins/${repo#*/}" ] || git clone -q --depth 1 "https://github.com/$repo" "$plugins/${repo#*/}"
  done
  zsh_path="$(command -v zsh)"
  if is_mac; then
    login_shell="$(dscl . -read "/Users/$(id -un)" UserShell | awk '{print $2}')"
  else
    login_shell="$(getent passwd "$(id -un)" | cut -d: -f7)"
  fi
  if [ "$login_shell" != "$zsh_path" ]; then
    note "changing your login shell to $zsh_path"
    $SUDO chsh -s "$zsh_path" "$(id -un)"
  fi
fi

if wanted nvim; then
  step "Neovim plugins (vim/lazy-lock.json) and treesitter parsers"
  if have nvim; then
    nvim --headless "+Lazy! restore" +qa
    nvim --headless \
      -c 'lua require("nvim-treesitter").install(require("treesitter_languages")):wait(900000)' +qa
    echo
  else
    note "no nvim yet: re-run after the mise step succeeds"
    failed="$failed nvim"
  fi
fi

for m in $modules; do
  step "Module: $m"
  bash "$DOTFILES/setup/modules/$m.sh" || failed="$failed $m"
done

step "Checking the result"
bash "$DOTFILES/setup/doctor.sh" || true

echo
echo "Took $(($(date +%s) - started))s."
if [ -n "$failed" ]; then
  echo "These steps had errors (see above):$failed"
  exit 1
fi
mkdir -p "$(dirname "$UPDATE_STAMP")" && touch "$UPDATE_STAMP"
echo "Open a new terminal so zsh, mise and PATH changes take effect. tmux installs its plugins on first start."
