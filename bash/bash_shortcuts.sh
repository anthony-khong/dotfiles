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

# Pretty Print JSON
alias ppj='python -m json.tool'

# Neovim
alias vi="/usr/bin/vim -p"
alias vim="nvim -p"

uuid() {
    python -c 'import uuid; print(uuid.uuid4())'
}

# History
alias hh=history
alias clear_history='cat /dev/null > ~/.bash_history && history -c'

# Tmux shortcut
tnew() {
    dir_name="$(basename "$PWD")"
    tmux new-session -As "$dir_name"
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

fix_nvim_tmux_navigator () {
    infocmp "$TERM" | sed 's/kbs=^[hH]/kbs=\\177/' > "$TERM.ti"
    tic "$TERM.ti"
}

# Networking shortcuts
alias renew="sudo ipconfig set en0 BOOTP && sudo ipconfig set en0 DHCP"

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

# Ctags
rctags() {
    ctags -R -f ./.git/tags .
}

# Trim
trim_image() {
    convert "$1" -trim "$1"
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

mind_diary() {
    cd ~/Dropbox/mind_diary || exit
    tnew
}
alias mnd=mind_diary
planner() {
    cd ~/Dropbox/mind_diary/Planner || exit
    tnew
}
alias pln=planner

tee7() {
    cd /Volumes/T7\ Touch/notes || exit
    tnew
}
alias t7=tee7

# Python
remove_pyc() {
    find . -name "*.pyc" -exec rm -rf {} \;
}

purge_py() {
    find . | grep -E "(__pycache__|\.pyc|\.pyo$)" | xargs rm -rf
}
