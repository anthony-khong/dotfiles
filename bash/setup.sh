# Install essentials
cd;
source ~/dotfiles/bash/bash_shortcuts
source ~/dotfiles/bash/bash_preferences

if [ "$(uname)" == "Darwin" ]; then
    # TODO: Anaconda, Chrome, Dropbox, Pandoc, R, Spotify, VLC Vim, htop, iTerm.
    echo 'Setting up Mac...';

    # Xcode
    xcode-select --install

    # Homebrew
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    brew install caskroom/cask/brew-cask

    # Essentials
    brew install wget
    brew install htop
    tlmgr install collection-fontsrecommended
    brew cask install karabiner

    # Vim
    brew install macvim --override-system-vim
    brew linkapps

    # NeoVim
    brew tap neovim/neovim
    brew install --HEAD neovim

    # Tmux
    brew install tmux

    # Ag
    brew install the_silver_searcher

    # Anaconda
    read -p "Install Anacanda now. Press [Enter] to continue..."

    # Create link to themes
    ln -s Dropbox/Others II/ubuntu_themes .themes
    ln -s Dropbox/Others II/Backgrounds Backgrounds

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

    # Xcape and Caps behaviour
    sudo apt-get install git gcc make pkg-config libx11-dev libxtst-dev libxi-dev
    git clone https://github.com/alols/xcape.git
    cd xcape
    make
    sudo make install

    setxkbmap -option 'caps:ctrl_modifier'
    xcape -e 'Caps_Lock=Escape;Control_L=Escape;Control_R=Escape'

    echo "\n" >> ~/.profile
    echo "# Mapping to make caps lock behave well" >> ~/.profile
    echo "setxkbmap -option 'caps:ctrl_modifier'" >> ~/.profile
    echo "xcape -e 'Caps_Lock=Escape;Control_L=Escape;Control_R=Escape'" >> ~/.profile

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

    # Ag
    sudo apt-get install silversearcher-ag

    # Anaconda
    sudo wget https://3230d63b5fc54e62148e-c95ac804525aac4b6dba79b00b39d1d3.ssl.cf1.rackcdn.com/Anaconda-2.3.0-Linux-x86_64.sh
    sudo chmod +x Ana*
    sudo bash Ana*.sh

    # Mutt
    # https://help.ubuntu.com/community/MuttAndGmail
    #sudo apt-get install mutt
    #touch ~/.muttrc
    #sudo chmod 600 ~/.muttrc
    #sudo apt-get install openssl ca-certificates
    #sudo apt-get install msmtp
    #touch $HOME/.msmtprc
    #touch $HOME/.msmtp.log
    #sudo chmod 0600 $HOME/.msmtprc
    #rm ~/.msmtprc
    #ln -s dotfiles/msmtprc ~/.msmtprc
    #sudo apt-get install fetchmail
    #rm ~/.fetchmailrc
    #ln -s dotfiles/fetchmailrc ~/.fetchmailrc

fi

# TODO: setup Python

#############################################################################
# Setup dotfiles
echo 'Cloning dotfiles repo...';
git clone https://github.com/anthony-khong/dotfiles.git;
cd ~/dotfiles;
git submodule init;
git submodule update;

# Python installs
~/dotfiles/bash/pip_installs.sh

cd;
cp ~/dotfiles/bash/inputrc ~/.inputrc
if [ "$(uname)" == "Darwin" ]; then
    rm .bash_profile
    echo "source ~/dotfiles/bash/bash_profile" >> .bash_profile
    echo "source ~/dotfiles/bash/bash_shortcuts" >> .bash_profile
    echo "source ~/dotfiles/bash/bash_preferences" >> .bash_profile
    source .bash_profile
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    rm .bashrc
    echo "source ~/dotfiles/bash/bashrc" >> .bashrc
    echo "source ~/dotfiles/bash/bash_shortcuts" >> .bashrc
    echo "source ~/dotfiles/bash/bash_preferences" >> .bashrc
    source .bashrc
fi

# Tmux configurations
cd;
rm .tmux.conf;
echo "source-file ~/dotfiles/tmux/tmux.conf" >> .tmux.conf;
tmux send -t sbash

#############################################################################
# Vim setups
cd;
rm .vimrc;
rm -rf .vim;
ln -s ~/dotfiles/vim/vimrc .vimrc
ln -s ~/dotfiles/vim/ .vim;

cd;
rm .nvimrc;
rm -rf .nvim;
ln -s ~/dotfiles/vim/vimrc .nivmrc
ln -s ~/dotfiles/vim/ .nvim;

mkdir -p ${XDG_CONFIG_HOME:=$HOME/.config}
ln -s ~/dotfiles/vim/vimrc $XDG_CONFIG_HOME/nvim
ln -s ~/dotfiles/vim/ $XDG_CONFIG_HOME/nvim/init.vim

nvim +PlugInstall +qall
nvim +PlugUpdate +qall

#############################################################################
