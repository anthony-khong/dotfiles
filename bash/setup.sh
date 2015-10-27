# Install essentials
cd;
if [ "$(uname)" == "Darwin" ]; then
    # TODO: Anaconda, Chrome, Dropbox, Pandoc, R, Spotify, VLC Vim, htop, iTerm.
    echo 'Setting up Mac...';

    # Xcode
    xcode-select --install

    # Homebrew
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

    # Essentials
    brew install wget
    brew install htop
    tlmgr install collection-fontsrecommended

    # Vim
    brew install macvim --override-system-vim
    brew linkapps

    # NeoVim
    brew tap neovim/neovim
    brew install --HEAD neovim

    # Tmux
    brew install tmux

elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    # TODO: Anaconda, Antivirus, CUDA, Chrome, Dropbox, RSudio, SSH, VLC, noip2.
    echo 'Setting up Linux...';

    # Essentials
    sudo apt-get update
    sudo apt-get install build-essential
    sudo apt-get install gnome-tweak-tool
    sudo apt-get install compizconfig-settings-manager compiz-plugins-extra
    sudo apt-get install htop
    sudo apt-get install lm-sensors

    # Git
    sudo apt-get update
    sudo apt-get install git

    # Vim with clipboard
    sudo apt-get update
    sudo apt-get install vim-gtk

    # NeoVim
    sudo add-apt-repository ppa:neovim-ppa/unstable
    sudo apt-get update
    sudo apt-get install neovim

    # Latest Tmux
    sudo apt-get install -y python-software-properties software-properties-common
    sudo add-apt-repository -y ppa:pi-rho/dev
    sudo apt-get update
    sudo apt-get install -y tmux=2.0-1~ppa1~t

    # Pandoc
    sudo apt-get install haskell-platform
    cabal update
    cabal install pandoc

    # Spotify
    sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys D2C19886
    echo deb http://repository.spotify.com stable non-free | sudo tee /etc/apt/sources.list.d/spotify.list
    sudo apt-get update
    sudo apt-get install spotify-client

    # R
    sudo apt-get update
    sudo apt-get install r-base

fi

# TODO: setup Python

# Setup dotfiles
echo 'Cloning dotfiles repo...';
git clone https://github.com/anthony-khong/dotfiles_akk.git;
cd ~/dotfiles_akk;
git submodule init;
git submodule update;
cd vim/bundle/jedi-vim/;
git submodule init;
git submodule update;

# Make dotfiles source from the repo
echo 'Making dotfiles source from dotfiles_akk...';
cd;

rm .tmux.conf;
echo "source-file ~/dotfiles_akk/tmux/tmux.conf" >> .tmux.conf;
rm .vimrc;
echo "set runtimepath^=~/dotfiles_akk/vim
source ~/dotfiles_akk/vim/vimrc" >> .vimrc;

cd;
cp ~/dotfiles_akk/bash/inputrc ~/.inputrc
if [ "$(uname)" == "Darwin" ]; then
    rm .bash_profile
    echo "source ~/dotfiles_akk/bash/bash_profile" >> .bash_profile
    echo "source ~/dotfiles_akk/bash/bash_shortcuts" >> .bash_profile
    echo "source ~/dotfiles_akk/bash/bash_preferences" >> .bash_profile
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    rm .bashrc
    echo "source ~/dotfiles_akk/bash/bashrc" >> .bashrc
    echo "source ~/dotfiles_akk/bash/bash_shortcuts" >> .bashrc
    echo "source ~/dotfiles_akk/bash/bash_preferences" >> .bashrc
fi
