# Reload bashrc
function sbash() {
    if [ "$(uname)" == "Darwin" ]; then
        source ~/.bash_profile;
        echo 'bash_profile reloaded!'
    elif [ "$(uname)" == "Linux" ]; then
        source ~/.bashrc;
        echo 'bashrc reloaded!'
    fi
}

# File Management
function mkinit() {
    echo '' >> __init__.py
}

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

# Spacemacs shortcuts
alias emacs="emacs -nw"

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

function auto_md_to_pdf() {
    local pdf_path="${1%.*}.pdf"
    local shortcuts="~/dotfiles/bash/bash_shortcuts"
    ls "$1" | entr bash -c "source $shortcuts; md_to_pdf 3 $1 $pdf_path; echo Last compiled: $(date)"
}


# Haskell shortcuts
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

# C++ shortucts
function gpp() {
    g++ -pipe -std=c++11 -lm -o temp "$1"
    if (( $# == 1 )); then
        ./temp
    else
        cat "$2" | ./temp

    fi

    rm temp
    echo ""
    rm a.out
}

# Git shortcuts
function gsu() {
    git submodule init;
    git submodule update;
}

function diffs() {
    vim `git ls-files -m` -p;
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

# TODO: add argument for pt
function auto_pytest() {
    rg --files "$1" |  entr bash -c "py.test $3; echo Last tested $(date)"
}

function auto_pt() {
    rg --files "$1" |  entr bash -c "py.test -s --pdb $3; echo Last tested $(date)"
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

# Kitty
alias kitty="/Applications/kitty.app/Contents/MacOS/kitty"

# Oscar
function build_bf_docker() {
    docker build -f docker/Dockerfile -t betfair .
}

function force_rebuild_bf_docker() {
    docker build --no-cache -f docker/Dockerfile -t betfair .
}

function bf_bash() {
    docker run -v "$PWD":"/root/$(basename $PWD)" -it betfair
}

function bf_ipython() {
    docker run -v "$PWD":"/root/$(basename $PWD)" --entrypoint ipython -it betfair
}

function bf_pytest() {
    docker run -v "$PWD":"/root/$(basename $PWD)" --entrypoint pytest -it betfair \
        betfair/tests/ 
}

function bf_pytest_pdb() {
    docker run -v "$PWD":"/root/$(basename $PWD)" --entrypoint pytest -it betfair \
        betfair/tests/ -s --pdb
}

function auto_bf_pytest() {
    rg --files . | entr bash -c \
        "docker run -v \"$PWD\":\"/root/$(basename $PWD)\" --entrypoint pytest -it betfair betfair/$1"
}

# Trim
function trim_image() {
    convert "$1" -trim "$1"
}

# Keyboard
function remap_caps() {
    setxkbmap -option 'caps:ctrl_modifier'
    xcape -t 150 -e 'Caps_Lock=Escape;Control_L=Escape;Control_R=Escape'
}
