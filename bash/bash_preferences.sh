export PATH=$PATH:/opt
export PATH="$PATH:$HOME/.local/bin"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$PATH:/snap/bin/"
GPG_TTY=$(tty)
export GPG_TTY

# Erlang
export ERL_AFLAGS="-kernel shell_history enabled -kernel shell_history_file_bytes 1024000"

export ELIXIR_ERL_OPTIONS="-kernel shell_history enabled -kernel shell_history_file_bytes 1024000"

# Don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTSIZE=1000
HISTFILESIZE=2000
HISTCONTROL=ignoreboth
HISTIGNORE='ls:bg:fg:history:hh'

# When the shell exits, append to the history file instead of overwriting it
if [ "$0" = "bash" ]; then
    shopt -s histappend
elif [ "$0" = "zsh" ]; then
    export PROMPT_COMMAND="history -a; history -n"
    bind 'set show-all-if-ambiguous on'
    bind 'TAB:menu-complete'
fi

# Short PS1
if [ "$0" = "bash" ]; then
    # export PS1="\[\033[36m\]\u\[\033[m\]:\[\033[33;1m\]\W\[\033[m\]$ "
    export PS1='\[\e[0;32m\]\u\[\e[m\]:\[\e[1;34m\]\W\[\033[m\]$ '
fi

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
export FZF_TMUX=1
export FZF_TMUX_HEIGHT=20

# Fixes some locale error when running mosh
export LC_ALL="en_US.UTF-8"

# mise puts the tools from ~/.config/mise on PATH (per directory, following any
# mise.toml or .tool-versions); fzf adds its Ctrl-T, Ctrl-R and Alt-C key bindings
if [ -n "${ZSH_VERSION:-}" ]; then
    command -v mise >/dev/null && eval "$(mise activate zsh)"
    fzf --zsh >/dev/null 2>&1 && source <(fzf --zsh)
elif [ -n "${BASH_VERSION:-}" ]; then
    command -v mise >/dev/null && eval "$(mise activate bash)"
    fzf --bash >/dev/null 2>&1 && eval "$(fzf --bash)"
fi

# Per-machine settings that don't belong in the repo
if [ -r "$HOME/.config/local/shell.sh" ]; then
    . "$HOME/.config/local/shell.sh"
fi
