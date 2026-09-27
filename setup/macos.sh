# macOS packages, sourced by install.sh

step "Xcode command line tools"
xcode-select -p >/dev/null 2>&1 || die "run 'xcode-select --install', let it finish, then re-run"

step "Homebrew packages (setup/Brewfile)"
if ! have brew; then
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi
for brew_bin in /opt/homebrew/bin/brew /usr/local/bin/brew; do
  if [ -x "$brew_bin" ]; then
    eval "$("$brew_bin" shellenv)"
    break
  fi
done
# Login shells need Homebrew on PATH too (zsh reads ~/.zprofile)
grep -qs 'brew shellenv' "$HOME/.zprofile" ||
  echo "eval \"\$($(command -v brew) shellenv)\"" >>"$HOME/.zprofile"
brew bundle install --no-upgrade --file="$DOTFILES/setup/Brewfile"
