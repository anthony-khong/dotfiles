# Reload bashrc
function sbash() {
    if [[ "$(uname)" == "Darwin" ]]; then
        source ~/.bash_profile;
        echo 'bash_profile reloaded!'
    elif [[ "$(uname)" == "Linux" ]]; then
        source ~/.bashrc;
        echo 'bashrc reloaded!'
    fi
}

# Tmate
function tm() {
    if [[ "$(uname)" == "Darwin" ]]; then
        tmate
    elif [[ "$(uname)" == "Linux" ]]; then
        export TERM=screen
        tmate
    fi
}

# Pretty Print JSON
alias ppj='python -m json.tool'

# Neovim
alias vi="/usr/bin/vim -p"
alias vim="nvim -p"
alias vff="nvim +'FZF ~'"

function ntree() {
    nvim +NERDTree
}

function setup_nvim() {
    nvim +PlugInstall +qall;
    nvim +PlugUpdate +qall;
    nvim +PlugInstall +qall;
}

# Emacs shortcuts
if type emacs26 > /dev/null; then
    alias emacs=emacs26
fi
alias em="emacs -nw"
alias ec="emacsclient -a '' -nw"

function kill-emacsclient() {
    emacsclient -e '(kill-emacs)'
}
alias kec="kill-emacsclient"

# History
alias hh=history
alias clear_history='cat /dev/null > ~/.bash_history && history -c'

# Tmux shortcut
function tnew {
    tmux new-session -As `basename $PWD`
}

# Pandoc shortcuts
function md_to_pdf() {
    pandoc -V geometry:margin="$1"cm -o $3 $2
}

function echo_compiled() {
    echo "compiled on $(date)"
}

function auto_md_to_pdf() {
    local pdf_path="${1%.*}.pdf"
    local shortcuts="~/dotfiles/bash/bash_shortcuts.sh"
    ls "$1" | entr bash -c "source $shortcuts; md_to_pdf 3 $1 $pdf_path; echo_compiled"
}

# Dotfiles shortucts
function recreate_symbolic_links() {
    bash ~/dotfiles/bash/recreate_symbolic_links
}

function clone_dotfiles() {
    git clone git@github.com:anthony-khong/dotfiles.git;
    cd dotfiles;
    gsu;

    recreate_symbolic_links;

    setup_nvim;
    sbash;
}

function fix_nvim_tmux_navigator () {
    infocmp $TERM | sed 's/kbs=^[hH]/kbs=\\177/' > $TERM.ti
    tic $TERM.ti
}

# Networking shortcuts
alias renew="sudo ipconfig set en0 BOOTP && sudo ipconfig set en0 DHCP"

function draape() {
    ssh root@128.199.230.91
}

function flush_dns_cache() {
    sudo dscacheutil -flushcache
    sudo killall -HUP mDNSResponder
    say cache flushed
}

function check_my_ip() {
    curl -s checkip.dyndns.org | sed -e 's/.*Current IP Address: //' -e 's/<.*$//'
}

# Watch
function do_if_read() {
    ls "$1" | entr bash -c "$2; echo Last executed $(date)"
}

# Karabiner
alias karabiner="/Applications/Karabiner.app/Contents/Library/bin/karabiner"

# Youtube DL
alias ytdl_mp3="youtube-dl --extract-audio --audio-format mp3"

# Carvil
function revenue() {
    ipython ~/carvil/scripts/revenue_cli.py "$@"
}

function start_db() {
    ~/.dropbox-dist/dropboxd
}

function revenue_breakdown() {
    ipython ~/carvil/scripts/revenue_check.py "$@"
}

# Ctags
function rctags() {
    ctags -R -f ./.git/tags .
}

# Racket
alias racket='docker run -it racket'

# Trim
function trim_image() {
    convert "$1" -trim "$1"
}

# Keyboard
function remap_caps() {
    setxkbmap -option 'caps:ctrl_modifier'
    xcape -t 150 -e 'Caps_Lock=Escape;Control_L=Escape;Control_R=Escape'
}

# ZSH
function disable_git_status() {
    git config --add oh-my-zsh.hide-status 1
}

function enable_git_status () {
    git config --unset-all oh-my-zsh.hide-status
}

# Clojure
function setup_parinfer_neovim () {
    cd ~/dotfiles/vim/plugged/parinfer-rust/cparinfer
    cargo build --release
}

function install_lein () {
    curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > lein
    sudo mkdir -p /usr/local/bin/
    sudo mv lein /usr/local/bin/lein
    sudo chmod a+x /usr/local/bin/lein
    lein
}

# Haskell
alias ghci='stack ghci'
alias ho='hoogle'

function auto_stack_test() {
    do_if_read "$1" "stack test"
}

function hs_script() {
    touch "$1"
    echo "#!/usr/bin/env stack"     >> "$1"
    echo "{- stack"                 >> "$1"
    echo "     --resolver lts-11.7" >> "$1"
    echo "     exec ghci"           >> "$1"
    echo "-}"                       >> "$1"
    echo ""                         >> "$1"
    echo "main :: IO ()"            >> "$1"
    echo "main = return ()"         >> "$1"
}

# Scala shortcuts
alias sc='scala -i'
alias samm='sbt ammonite:run'
alias sbt="sbt -mem 2048"

# FSharp
function install-paket() {
    export PAKET_URL=https://github.com/fsprojects/Paket/releases/download/5.194.0/paket.bootstrapper.exe
    mkdir .paket
    curl -L $PAKET_URL -o .paket/paket.bootstrapper.exe
    mono .paket/paket.bootstrapper.exe
    echo ".paket/paket.exe" >> .gitignore
    echo ".packages" >> .gitignore
    mono .paket/paket.exe install
}
