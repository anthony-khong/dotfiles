alias pi='THEANO_FLAGS="device=cpu" ipython -i'
alias py='THEANO_FLAGS="device=cpu" ipython -i'
alias gpy='THEANO_FLAGS="device=gpu0" ipython -i'
alias gpy0='THEANO_FLAGS="device=gpu0" ipython -i'
alias gpy1='THEANO_FLAGS="device=gpu1" ipython -i'
alias gpy2='THEANO_FLAGS="device=gpu2" ipython -i'
alias gpy3='THEANO_FLAGS="device=gpu3" ipython -i'

# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples
# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

export OPENBLAS_HOME=/home/akkhong
export CUDA_HOME=/usr/local/cuda-7.0
export LD_LIBRARY_PATH=${CUDA_HOME}/lib64
export PYLEARN2_DATA_PATH=/data/lisa/data

PATH=${CUDA_HOME}/bin:${PATH}
export PATH

# added by Anaconda 2.1.0 installer
export PATH="/home/akkhong/anaconda/bin:$PATH"

export PYTHONPATH=/home/akkhong/
export PYTHONPATH=$PYTHONPATH:/home/akkhong/pylearn2/
export PYTHONPATH=$PYTHONPATH:/home/akkhong/xgboost/wrapper/
export PYTHONPATH=$PYTHONPATH:/home/akkhong/xgboost/python-package/
export PYTHONPATH=$PYTHONPATH:/home/akkhong/applied_modelling/
export user=akk_linux

# SSH to other computers
export CUFFY="ssh -4 akk@ilikezcoffe.no-ip.biz"
export JARWIN="ssh -4 akkhong@jarwinjkt.no-ip.biz"

function fastpush() {
    git add -u;
    git commit -m "$*";
    git push;
}

# For tmux colours
export TERM="xterm-256color"

function fastpush() {
    git add -u;
    git commit -m "$*";
    git push;
}

function compile_md() {
    pandoc -V geometry:margin=2cm -o $1 $2
}


# Functions to navigate through directories
function dropbox_hr_value() {
    cd ~/Dropbox/am_records/hr_value
}
function gpy() {
    cd ~/Dropbox/tutorials/gpy
}
function papers() {
    cd ~/Dropbox/Interesting\ Papers/
}
function bayes_ml() {
    cd ~/Dropbox/Interesting\ Papers/Bayesian\ Machine\ Learning/
}
function deep() {
    cd ~/Dropbox/Interesting\ Papers/Deep\ Learning/
}
function ensembles() {
    cd ~/Dropbox/Interesting\ Papers/Ensemble\ Methods/
}
function mcm() {
    cd ~/Dropbox/Interesting\ Papers/Monte\ Carlo\ Methods/
}
function fvalue() {
    cd ~/applied_modelling/football_value/
}

# Functions for networking
function jarwin() {
    ssh -4 akkhong@jarwinjkt.no-ip.biz
}
function cuffy() {
    ssh -4 akk@ilikezcoffe.no-ip.biz
}

function local_jarwin() {
    ssh akkhong@192.168.1.107
}
function local_jarwinX() {
    ssh -X akkhong@192.168.1.107
}
function samhan() {
    ssh akkhong@192.168.1.109
}
function samhanX() {
    ssh -X akkhong@192.168.1.109
}

function check_my_ip() {
    curl -s checkip.dyndns.org | sed -e 's/.*Current IP Address: //' -e 's/<.*$//'
}
function stream_webcam() {
    vlc v4l2:///dev/video0
}
function gpa() {
    cd ~/applied_modelling;
    git pull;
    cd ~/hrvalue;
    git pull;
    cd ~/dotfiles_akk;
    git pull;
    git submodule init;
    git submodule update;
    cd;
}

function sbash() {
    source ~/.bash_profile
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

export PS1="\[\033[36m\]\u\[\033[m\]:\[\033[33;1m\]\W\[\033[m\]$ "
. /etc/profile.d/vte.sh

umask 000

# added by Anaconda 2.3.0 installer
export PATH="/home/akkhong/anaconda/bin:$PATH"

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
