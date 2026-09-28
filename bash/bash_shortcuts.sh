# Aliases and functions, shared by zsh (zshrc) and bash (bashrc). Environment and tool setup
# are in bash_preferences.sh.

# Reload the shell's config
sbash() {
    if [ -n "${ZSH_VERSION:-}" ]; then
        exec zsh
    else
        . ~/.bashrc && echo 'bashrc reloaded!'
    fi
}

# ls; oh-my-zsh adds ll, la and l
case "$OSTYPE" in
darwin*) alias ls='ls -GFh' ;;
*) alias ls='ls --color=auto -Fh' ;;
esac

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

# tmux sessions: tp picks one, or a directory to open one in (tmux/tp.sh; prefix f in tmux).
# tnew and the note shortcuts below open the session for one directory, in or out of tmux.
tp() {
    ~/dotfiles/tmux/tp.sh "$@"
}

tnew() {
    tp .
}

mind_diary() {
    cd ~/Dropbox/mind_diary && tp .
}
alias mnd=mind_diary
planner() {
    cd ~/Dropbox/mind_diary/Planner && tp .
}
alias pln=planner

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

dotfiles-update() {
    ~/dotfiles/update.sh "$@"
}

# Ghostty's terminfo for a box without these dotfiles (boxes with them compile it in the
# link step): ghostty_terminfo_to <host>...
ghostty_terminfo_to() {
    for host in "$@"; do
        infocmp -x xterm-ghostty | ssh "$host" -- tic -x - && echo "$host: done"
    done
}

# Networking shortcuts
check_my_ip() {
    curl -s checkip.dyndns.org | sed -e 's/.*Current IP Address: //' -e 's/<.*$//'
}

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

# Elixir
alias iexmem='MIMALLOC_PURGE_DELAY=0 MIMALLOC_PURGE_DECOMMITS=1 iex -S mix'

# Python: delete the bytecode below this directory (__pycache__ directories, .pyc and .pyo files)
purge_py() {
    find . -type d -name __pycache__ -prune -exec rm -rf {} + -o -type f -name '*.py[co]' -exec rm -f {} +
}
alias remove_pyc=purge_py

# GitLab runner (the gitlab-runner module), e.g. `gitlab-runner register`
alias gitlab-runner='docker run --rm -it -v gitlab-runner-config:/etc/gitlab-runner gitlab/gitlab-runner:latest'

# macOS
case "$OSTYPE" in
darwin*)
    alias renew="sudo ipconfig set en0 BOOTP && sudo ipconfig set en0 DHCP"
    flush_dns_cache() {
        sudo dscacheutil -flushcache
        sudo killall -HUP mDNSResponder
        say cache flushed
    }
    alias rosetta-brew='arch -x86_64 /usr/local/bin/brew'
    alias x86='/usr/bin/arch -x86_64 /bin/zsh --login'
    alias arm='/usr/bin/arch -arm64 /bin/zsh --login'
    ;;
esac
