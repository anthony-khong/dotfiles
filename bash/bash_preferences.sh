unset DYLD_FALLBACK_LIBRARY_PATH
export PYTHONPATH=$PYTHONPATH:~/carvil
export PYTHONPATH=$PYTHONPATH:~/grist
export PYTHONPATH=$PYTHONPATH:~/repos
export PYTHONPATH=$PYTHONPATH:~/code
export PYTHONPATH=$PYTHONPATH:~
export PATH="/opt/anaconda/bin:$PATH"
export PATH=$PATH:/opt
export PATH=$PATH:~/.local/bin
export PATH="$HOME/.cargo/bin:$PATH"
export NVIM_PYTHON_LOG_FILE=/tmp/log
export NVIM_PYTHON_LOG_LEVEL=DEBUG

# Don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTSIZE=1000
HISTFILESIZE=2000
HISTCONTROL=ignoreboth
HISTIGNORE='ls:bg:fg:history:hh'

# When the shell exits, append to the history file instead of overwriting it
if [[ "$0" == "bash" ]]; then
    shopt -s histappend
elif [[ "$0" == "zsh" ]]; then
    export PROMPT_COMMAND="history -a; history -n"
    bind 'set show-all-if-ambiguous on'
    bind 'TAB:menu-complete'
fi

# For tmux colours
export TERM="screen-256color"

# Short PS1
if [[ "$0" == "bash" ]]; then
    # export PS1="\[\033[36m\]\u\[\033[m\]:\[\033[33;1m\]\W\[\033[m\]$ "
    export PS1='\[\e[0;32m\]\u\[\e[m\]:\[\e[1;34m\]\W\[\033[m\]$ '
fi

# Make Neovim the default editor
export VISUAL=nvim
export EDITOR="$VISUAL"

# Make FZF search faster
if [[ "$0" == "bash" ]]; then
    [ -f ~/.fzf.bash ] && source ~/.fzf.bash
elif [[ "$0" == "zsh" ]]; then
    [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
fi

# This controls what happens in Terminal
# export FZF_CTRL_T_COMMAND="ag -l -g ''"

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

if [ -e ~/bashrc_agoda ]
then
    source ~/bashrc_agoda
fi


if [[ "$0" == "bash" ]]; then
    # The next line updates PATH for the Google Cloud SDK.
    if [ -f '/Users/akhong/google-cloud-sdk/path.bash.inc' ]; then . '/Users/akhong/google-cloud-sdk/path.bash.inc'; fi

    # The next line enables shell command completion for gcloud.
    if [ -f '/Users/akhong/google-cloud-sdk/completion.bash.inc' ]; then . '/Users/akhong/google-cloud-sdk/completion.bash.inc'; fi
elif [[ "$0" == "zsh" ]]; then
    # The next line updates PATH for the Google Cloud SDK.
    if [ -f '/Users/akhong/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/akhong/google-cloud-sdk/path.zsh.inc'; fi

    # The next line enables shell command completion for gcloud.
    if [ -f '/Users/akhong/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/akhong/google-cloud-sdk/completion.zsh.inc'; fi
fi
