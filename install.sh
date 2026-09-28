#!/usr/bin/env bash
# Sets up this machine from the dotfiles. Safe to re-run.
#   ./install.sh                        base setup; the first run asks for this machine's name
#   ./install.sh --with jvm,docker      plus modules: data docker gitlab-runner jvm mac-server swap tailscale
#   ./install.sh --skip packages,rust   skip steps: packages link mise rust zsh nvim
#   ./install.sh --hostname NAME        rename this machine (without asking)
#   ./install.sh --doctor               only report what's installed
# Supports macOS (Homebrew) and Ubuntu 22.04+, on x86_64 or arm64.
set -euo pipefail

DOTFILES="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
. "$DOTFILES/setup/lib.sh"

modules=""
skip=""
new_hostname=""
while [ $# -gt 0 ]; do
  case "$1" in
  --with | --skip | --hostname)
    [ $# -ge 2 ] || die "$1 needs a value"
    case "$1" in
    --with) modules="${2//,/ }" ;;
    --skip) skip="${2//,/ }" ;;
    --hostname) new_hostname="$2" ;;
    esac
    shift 2
    ;;
  --doctor) exec bash "$DOTFILES/setup/doctor.sh" ;;
  -h | --help)
    sed -n '2,8s/^# \{0,1\}//p' "$0"
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

# A step that stops the script (set -e) says where it stopped and how to carry on
current=""
on_exit() {
  local status=$?
  if [ "$status" -ne 0 ] && [ -n "$current" ]; then
    printf '\nStopped in the %s step. Fix the error above and re-run ./install.sh: it is safe to\nre-run, and --skip leaves out the steps that are already done.\n' "$current" >&2
  fi
}
trap on_exit EXIT

# The machine's name, which Tailscale, tmux/host_colour.sh and the tmux pill all take. The
# first run asks (a clean run touches UPDATE_STAMP); Enter keeps the current name.
set_hostname() {
  local name="$1"
  echo "$name" | grep -Eqx '[A-Za-z0-9]([A-Za-z0-9-]{0,61}[A-Za-z0-9])?' ||
    die "a hostname is letters, digits and inner hyphens: $name"
  if [ "$name" = "$(hostname -s)" ]; then
    note "keeping $name"
    return
  fi
  if is_mac; then
    sudo scutil --set HostName "$name"
    sudo scutil --set LocalHostName "$name"
    sudo scutil --set ComputerName "$name"
  else
    $SUDO hostnamectl set-hostname "$name"
    # sudo warns "unable to resolve host" until /etc/hosts has the new name
    if grep -q '^127\.0\.1\.1[[:space:]]' /etc/hosts; then
      $SUDO sed -i "s/^127\.0\.1\.1[[:space:]].*/127.0.1.1\t$name/" /etc/hosts
    else
      printf '127.0.1.1\t%s\n' "$name" | $SUDO tee -a /etc/hosts >/dev/null
    fi
    # Cloud images would otherwise put the provider's name back at the next boot
    if [ -d /etc/cloud/cloud.cfg.d ]; then
      echo "preserve_hostname: true" | $SUDO tee /etc/cloud/cloud.cfg.d/99-dotfiles-hostname.cfg >/dev/null
    fi
  fi
  note "renamed to $name"
}
current="hostname"
if [ -z "$new_hostname" ] && [ ! -e "$UPDATE_STAMP" ] && (: </dev/tty) 2>/dev/null; then
  read -r -p "Name for this machine [$(hostname -s)]: " new_hostname </dev/tty
  new_hostname="${new_hostname:-$(hostname -s)}"
fi
if [ -n "$new_hostname" ]; then
  step "Hostname"
  set_hostname "$new_hostname"
fi

# The prebuilt tree-sitter CLI needs glibc 2.39+ (Ubuntu 24.04); on older boxes cargo builds it
old_glibc=0
if is_ubuntu; then
  glibc="$(getconf GNU_LIBC_VERSION | awk '{print $2}')"
  at_least "$glibc" 2.39 || old_glibc=1
fi
failed=""
started=$(date +%s)
cd "$HOME"

if wanted packages; then
  current="packages"
  if is_mac; then . "$DOTFILES/setup/macos.sh"; else . "$DOTFILES/setup/ubuntu.sh"; fi
fi

if wanted link; then
  current="link"
  step "Linking configs"
  bash "$DOTFILES/bash/recreate_symbolic_links"
fi

if wanted mise; then
  current="mise"
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
  current="rust"
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
  current="zsh"
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

setup_nvim() { # plugins, parsers and spell files; headless nvim's exit status says little
  local problem=0 missing
  run_lazy restore || problem=1
  nvim --headless \
    -c 'lua require("nvim-treesitter").install(require("treesitter_languages")):wait(900000)' +qa || problem=1
  # Spell files for 'spelllang' (plus .sug suggestions where the mirror has them), with
  # Neovim's own downloader, so nvim never has to ask
  nvim --headless -c 'lua local s = require("nvim.spellfile"); s.config({ confirm = false })
    local dir = vim.fn.stdpath("data") .. "/site/spell"
    for _, l in ipairs(vim.opt.spelllang:get()) do
      if vim.fn.filereadable(dir .. "/" .. l .. ".utf-8.spl") == 0 then s.get(l) end
    end' +qa || problem=1
  echo
  missing="$(missing_parsers)"
  if [ -n "$missing" ]; then note "treesitter parsers missing: $missing" && problem=1; fi
  missing="$(missing_spell)"
  if [ -n "$missing" ]; then note "spell files missing: $missing" && problem=1; fi
  return "$problem"
}
if wanted nvim; then
  current="nvim"
  step "Neovim plugins (vim/lazy-lock.json), treesitter parsers and spell files"
  if ! have nvim; then
    note "no nvim yet: re-run after the mise step succeeds"
    failed="$failed nvim"
  elif ! setup_nvim; then
    failed="$failed nvim"
  fi
fi

for m in $modules; do
  current="$m module"
  step "Module: $m"
  bash "$DOTFILES/setup/modules/$m.sh" || failed="$failed $m"
done

current=""
step "Checking the result"
bash "$DOTFILES/setup/doctor.sh" || failed="$failed doctor"

echo
echo "Took $(($(date +%s) - started))s."
if [ -n "$failed" ]; then
  echo "These had problems (see above):$failed. Fix them and re-run ./install.sh; it is safe to re-run."
  case "$failed" in *doctor*) if [ -n "$skip" ]; then echo "Some of what doctor reports may come from the skipped steps: $skip."; fi ;; esac
  exit 1
fi
mkdir -p "$(dirname "$UPDATE_STAMP")" && touch "$UPDATE_STAMP"
echo "Open a new terminal so zsh, mise and PATH changes take effect. tmux installs its plugins on first start."
