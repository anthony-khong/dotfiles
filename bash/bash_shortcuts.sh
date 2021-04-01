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

# Gcloud
alias gcurl='curl --header "Authorization: Bearer $(gcloud auth print-identity-token)"'

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

function dark_vim() {
    DARK_VIM=1
    echo "DARK_VIM set to $DARK_VIM"
}

function light_vim() {
    DARK_VIM=0
    echo "DARK_VIM set to $DARK_VIM"
}

# Emacs shortcuts
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

function pop-os() {
    ssh akhong@10.8.1.1
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

# Encryption
function tar-enc () {
	TMP=$(mktemp -d)
    echo "tar-ing to $TMP/$1.tar.gz ..."
    tar -czf "$TMP/$1.tar.gz" "$1"
    echo "enc-ing to $1.secrude ..."
    openssl enc -aes256 -salt -in "$TMP/$1.tar.gz" -out "$1.secured"
}

function dec-tar () {
	TMP=$(mktemp -d)
    FNAME=$(echo "$1" | sed -e "s/.secured$//")
    echo "dec-ing to $TMP/$FNAME.tar.gz"
    openssl enc -d -aes256 -in "$1" -out "$TMP/$FNAME.tar.gz"
    echo "untar-ing to $PWD"
    tar -xzf "$TMP/$FNAME.tar.gz" -C .
}

# Babashka
function bb-nrepl () {
    bb --nrepl-server 4444
}

function gdrive-dl() {
    # source: https://medium.com/@acpanjan/download-google-drive-files-using-wget-3c2c025a8b99
    wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=$1' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=$1" -O $2
    rm -rf /tmp/cookies.txt
}

function ubuntu-bash() {
    dir=`basename $PWD`
    docker run --rm \
        -v $PWD:/root/$dir \
        -w /root/$dir \
        -it akkhong/dev-env:latest /bin/zsh
}
alias bush="ubuntu-bash"

function planner() {
    cd ~/Dropbox/planner
    tnew
}
alias pln=planner
