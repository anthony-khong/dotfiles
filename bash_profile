# source ~/dotfiles_akk/bash_profile
# source ~/dotfiles_akk/bash_shortcuts
# source ~/dotfiles_akk/bash_preferences

# Terminal colouring
export CLICOLOR=1
export LSCOLORS=ExFxBxDxCxegedabagacad
alias ls='ls -GFh'
export PATH=/usr/local/bin:$PATH
export PATH=/usr/bin:$PATH

# added by Anaconda 2.3.0 installer
export PATH="//anaconda/bin:$PATH"

export DYLD_FALLBACK_LIBRARY_PATH=$DYLD_FALLBACK_LIBRARY_PATH:/Users/akkhong/anaconda/lib
export PYTHONPATH=/Users/akkhong
export CUFFY="ssh -4 akk@ilikezcoffe.no-ip.biz"
export JARWIN="ssh -4 akkhong@jarwinjkt.no-ip.biz"

export user=akk_mac

# Tell vim to use macvim (to enable clipboard)
alias vim="mvim -v"

# Load RVM into a shell session *as a function*
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
