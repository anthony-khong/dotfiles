# Reload bashrc
sbash() {
    if [ "$(uname)" = "Darwin" ]; then
        source ~/.bash_profile;
        echo 'bash_profile reloaded!'
    elif [ "$(uname)" = "Linux" ]; then
        source ~/.bashrc;
        echo 'bashrc reloaded!'
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

ntree() {
    nvim +NERDTree
}

setup_nvim() {
    nvim +PlugInstall +qall;
    nvim +PlugUpdate +qall;
    nvim +PlugInstall +qall;
}

dark_vim() {
    DARK_VIM=1
    echo "DARK_VIM set to $DARK_VIM"
}

light_vim() {
    DARK_VIM=0
    echo "DARK_VIM set to $DARK_VIM"
}

# Emacs shortcuts
alias em="emacs -nw"
alias ec="emacsclient -a '' -nw"

kill_emacsclient() {
    emacsclient -e '(kill-emacs)'
}
alias kec="kill-emacsclient"

# History
alias hh=history
alias clear_history='cat /dev/null > ~/.bash_history && history -c'

# Tmux shortcut
tnew() {
    tmux new-session -As "$(basename "$PWD")"
}

# Pandoc shortcuts
md_to_pdf() {
    pandoc -V geometry:margin="$1"cm -o "$3" "$2"
}

echo_compiled() {
    echo "compiled on $(date)"
}

auto_md_to_pdf() {
    pdf_path="${1%.*}.pdf"
    shortcuts="$HOME/dotfiles/bash/bash_shortcuts.sh"
    ls "$1" | entr bash -c "source $shortcuts; md_to_pdf 3 $1 $pdf_path; echo_compiled"
}

# Dotfiles shortucts
recreate_symbolic_links() {
    bash ~/dotfiles/bash/recreate_symbolic_links
}

clone_dotfiles() {
    git clone git@github.com:anthony-khong/dotfiles.git;
    cd dotfiles || exit;
    gsu;

    recreate_symbolic_links;

    setup_nvim;
    sbash;
}

fix_nvim_tmux_navigator () {
    infocmp "$TERM" | sed 's/kbs=^[hH]/kbs=\\177/' > "$TERM.ti"
    tic "$TERM.ti"
}

# Networking shortcuts
alias renew="sudo ipconfig set en0 BOOTP && sudo ipconfig set en0 DHCP"

draape() {
    ssh root@128.199.230.91
}

flush_dns_cache() {
    sudo dscacheutil -flushcache
    sudo killall -HUP mDNSResponder
    say cache flushed
}

check_my_ip() {
    curl -s checkip.dyndns.org | sed -e 's/.*Current IP Address: //' -e 's/<.*$//'
}

# Karabiner
alias karabiner="/Applications/Karabiner.app/Contents/Library/bin/karabiner"

# Youtube DL
alias ytdl_mp3="youtube-dl --extract-audio --audio-format mp3"

# Carvil
revenue() {
    ipython ~/carvil/scripts/revenue_cli.py "$@"
}

start_db() {
    ~/.dropbox-dist/dropboxd
}

revenue_breakdown() {
    ipython ~/carvil/scripts/revenue_check.py "$@"
}

# Ctags
rctags() {
    ctags -R -f ./.git/tags .
}

# Trim
trim_image() {
    convert "$1" -trim "$1"
}

# Keyboard
remap_caps() {
    setxkbmap -option 'caps:ctrl_modifier'
    xcape -t 150 -e 'Caps_Lock=Escape;Control_L=Escape;Control_R=Escape'
}

# ZSH
disable_git_status() {
    git config --add oh-my-zsh.hide-status 1
}

enable_git_status () {
    git config --unset-all oh-my-zsh.hide-status
}

# Encryption
tar_enc () {
	TMP=$(mktemp -d)
    echo "tar-ing to $TMP/$1.tar.gz ..."
    tar -czf "$TMP/$1.tar.gz" "$1"
    echo "enc-ing to $1.secrude ..."
    openssl enc -aes256 -salt -in "$TMP/$1.tar.gz" -out "$1.secured"
}

dec_tar () {
	TMP=$(mktemp -d)
    FNAME=$(echo "$1" | sed -e "s/.secured$//")
    echo "dec-ing to $TMP/$FNAME.tar.gz"
    openssl enc -d -aes256 -in "$1" -out "$TMP/$FNAME.tar.gz"
    echo "untar-ing to $PWD"
    tar -xzf "$TMP/$FNAME.tar.gz" -C .
}

# Babashka
bb_nrepl () {
    bb --nrepl-server 4444
}

gdrive_dl() {
    # source: https://medium.com/@acpanjan/download-google-drive-files-using-wget-3c2c025a8b99
    wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=$1' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=$1" -O $2
    rm -rf /tmp/cookies.txt
}

ubuntu_bash() {
    dir="basename $PWD"
    docker run --rm \
        -v "$PWD":/root/"$dir" \
        -w /root/"$dir" \
        -it akkhong/dev-env:latest /bin/zsh
}
alias bush="ubuntu_bash"

mind_diary() {
    cd ~/Dropbox/mind_diary || exit
    tnew
}
alias pln=mind

# Python
remove_pyc() {
    find . -name "*.pyc" -exec rm -rf {} \;
}

purge_py() {
    find . | grep -E "(__pycache__|\.pyc|\.pyo$)" | xargs rm -rf
}
