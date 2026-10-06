# Environment and tool setup, shared by zsh (zshrc) and bash (bashrc). Aliases and functions
# are in bash_shortcuts.sh; the PATH for non-interactive zsh is in zshenv.

# PATH additions, skipping directories that are missing or already on it
_path_add() { # _path_add <dir> [end]
    [ -d "$1" ] || return 0
    case ":$PATH:" in *":$1:"*) return 0 ;; esac
    if [ "${2:-}" = end ]; then PATH="$PATH:$1"; else PATH="$1:$PATH"; fi
}
_path_add /opt/homebrew/bin
_path_add "$HOME/.cargo/bin"
_path_add "$HOME/.local/bin"
_path_add "$HOME/.mix/escripts" end
_path_add /snap/bin end
unset -f _path_add
export PATH

GPG_TTY=$(tty)
export GPG_TTY

# Erlang
export ERL_AFLAGS="-kernel shell_history enabled -kernel shell_history_file_bytes 1024000"

export ELIXIR_ERL_OPTIONS="-kernel shell_history enabled -kernel shell_history_file_bytes 1024000"

# Make Neovim the default editor
export VISUAL=nvim
export EDITOR="$VISUAL"

# This controls what happens in Vim
export FZF_DEFAULT_COMMAND="rg --files\
                            -g '*'\
                            -g '!*Applications/*'\
                            -g '!*Desktop/*'\
                            -g '!*Downloads/*'\
                            -g '!*Dropbox/*'\
                            -g '!*Library/*'\
                            -g '!*Movies/*'\
                            -g '!*Music/*'\
                            -g '!*Pictures/*'\
                            -g '!*Templates/*'\
                            -g '!*Videos/*'
                            "

# Fixes some locale error when running mosh
export LC_ALL="en_US.UTF-8"

# Terminal colours: BSD ls on macOS; lesspipe on Ubuntu lets less read archives
case "$OSTYPE" in
darwin*) export CLICOLOR=1 LSCOLORS=ExFxBxDxCxegedabagacad ;;
esac
if [ -x /usr/bin/lesspipe ]; then eval "$(SHELL=/bin/sh lesspipe)"; fi

# mise puts the tools from ~/.config/mise on PATH (per directory, following any
# mise.toml or .tool-versions); fzf adds its Ctrl-T, Ctrl-R and Alt-C key bindings. In zsh,
# Atuin (atuin/config.toml) then takes Ctrl-R; the up arrow stays zsh's own.
if [ -n "${ZSH_VERSION:-}" ]; then
    command -v mise >/dev/null && eval "$(mise activate zsh)"
    fzf --zsh >/dev/null 2>&1 && source <(fzf --zsh)
    command -v atuin >/dev/null && eval "$(atuin init zsh --disable-up-arrow)"
elif [ -n "${BASH_VERSION:-}" ]; then
    command -v mise >/dev/null && eval "$(mise activate bash)"
    fzf --bash >/dev/null 2>&1 && eval "$(fzf --bash)"
fi

# Per-machine settings that don't belong in the repo
if [ -r "$HOME/.config/local/shell.sh" ]; then
    . "$HOME/.config/local/shell.sh"
fi

# One line (never a prompt) when dotfiles-update hasn't run for DOTFILES_UPDATE_DAYS days;
# set that per machine in ~/.config/local/shell.sh
_dotfiles_stamp="${XDG_STATE_HOME:-$HOME/.local/state}/dotfiles/last-update"
if [ ! -e "$_dotfiles_stamp" ]; then
    mkdir -p "${_dotfiles_stamp%/*}" && touch "$_dotfiles_stamp"
elif [ -n "$(find "$_dotfiles_stamp" -mtime +"${DOTFILES_UPDATE_DAYS:-14}" 2>/dev/null)" ]; then
    echo "dotfiles: last update over ${DOTFILES_UPDATE_DAYS:-14} days ago; run dotfiles-update"
fi
unset _dotfiles_stamp
