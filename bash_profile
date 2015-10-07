alias py='THEANO_FLAGS="device=cpu" ipython -i'
alias gpy='THEANO_FLAGS="device=gpu0" ipython -i'
alias gpy0='THEANO_FLAGS="device=gpu0" ipython -i'
alias gpy1='THEANO_FLAGS="device=gpu1" ipython -i'
alias gpy2='THEANO_FLAGS="device=gpu2" ipython -i'
alias gpy3='THEANO_FLAGS="device=gpu3" ipython -i'

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

# Miscellaneous functions
function gpa() {
    cd ~/applied_modelling;
    git pull;
    cd ~/hrvalue;
    git pull;
    cd ~/dotfiles_akk;
    git pull;
    cd;
}
function clone_dotfiles() {
    git clone git@github.com:anthony-khong/dotfiles_akk.git;
    cd dotfiles_akk;
    git submodule init;
    git submodule update;
    cd vim/bundle/jedi-vim;
    git submodule init;
    git submodule update;
}

function sbash() {
    source ~/.bash_profile
}

function roll() {
    python ~/roll_die.py
}

function fastpush() {
    git add -u;
    git commit -m "$*";
    git push;
}

function fvalue() {
    cd ~/applied_modelling/football_value/
}
function e10() {
    cd ~/applied_modelling/football_value/experiments/experiment10/
}
function e14() {
    cd ~/applied_modelling/football_value/experiments/experiment14/
}

function compile_md() {
    pandoc -V geometry:margin=2cm -o $1 $2
}

# Networking functions
function local_jarwin() {
    ssh akkhong@192.168.1.107
}
function local_jarwinX() {
    ssh -X akkhong@192.168.1.107
}

function jarwin() {
    ssh -4 akkhong@jarwinjkt.no-ip.biz
}
function jarwinX() {
    ssh -X akkhong@jarwinjkt.no-ip.biz
}
function samhan() {
    ssh akkhong@192.168.1.109
}
function samhanX() {
    ssh -X akkhong@192.168.1.109
}

function cuffy() {
    ssh -4 akk@ilikezcoffe.no-ip.biz
}

function check_my_ip() {
    curl -s checkip.dyndns.org | sed -e 's/.*Current IP Address: //' -e 's/<.*$//'
}
function flush_dns_cache() {
    sudo dscacheutil -flushcache
    sudo killall -HUP mDNSResponder
    say cache flushed
}
function stream_webcam() {
    vlc v4l2:///dev/video0
}

function quit() {
    exit
}

# Load RVM into a shell session *as a function*
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

export PS1="\[\033[36m\]\u\[\033[m\]:\[\033[33;1m\]\W\[\033[m\]$ "
umask 000

# Stop Python from generating .pyc files
export PYTHONDONTWRITEBYTECODE=1

# fzf settings
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

export TERM=xterm-256color
