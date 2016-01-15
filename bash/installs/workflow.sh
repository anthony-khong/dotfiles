function install_neovim() {
    if [ "$(uname)" == "Darwin" ]; then
        brew tap neovim/neovim
        brew install --HEAD neovim
    elif [ "$(uname)" == "Linux" ]; then
        sudo add-apt-repository ppa:neovim-ppa/unstable
        sudo apt-get update
        sudo apt-get install neovim
    fi
}

function install_terminal() {
    if [ "$(uname)" == "Darwin" ]; then
        brew tap caskroom/version
        brew install iterm2-nightly
    elif [ "$(uname)" == "Linux" ]; then
        sudo add-apt-repository ppa:gnome3-team/gnome3-staging
        sudo apt-get update
        sudo apt-get install gnome-terminal
        sudo add-apt-repository -r ppa:gnome3-team/gnome3-staging
    fi
    }

function install_linuxbrew() {
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/linuxbrew/go/install)"
    }

function install_tmux() {
    brew install https://raw.githubusercontent.com/choppsv1/homebrew-term24/master/tmux.rb
    }

