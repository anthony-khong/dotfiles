# Xcode
echo 'Setting up Xcode...'
xcode-select --install

# Homebrew
echo 'Setting up Homebrew...'
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew install caskroom/cask/brew-cask # No cask to install?

# Essentials
brew install wget
brew install htop
brew install pandoc

# Vim
echo 'Setting up Vim...'
brew install macvim --override-system-vim
brew linkapps

# NeoVim
echo 'Setting up Neovim...'
brew install neovim

# FZF
echo 'Setting up FZF..'
brew install fzf

# Tmux
echo 'Setting up Tmux...'
brew install tmux
brew install reattach-to-user-namespace

# Ag
echo 'Setting up Ag...'
brew install the_silver_searcher
brew install repgrip

# R
echo 'Setting up R...'
brew install r

# Anaconda
echo 'Setting up Anaconda...'
read -p "Install Anacanda now. Press [Enter] to continue..."
sudo wget https://repo.continuum.io/archive/Anaconda3-5.1.0-MacOSX-x86_64.sh -O ~/Downloads/anaconda.sh
sudo chmod +x ~/Downloads/anaconda.sh
sudo bash ~/Downloads/anaconda.sh -b -p /opt/anaconda
rm ~/Downloads/anaconda.sh

# Setup dotfiles
echo 'Setting up dotfiles...'
cd ~/dotfiles
git submodule init
git submodule update

cd;
source ~/dotfiles/bash/bash_shortcuts
recreate_symbolic_links
source ~/.bash_profile

# Setup NeoVim
echo 'Installing NeoVim plugins...'
nvim +PlugInstall +qall
nvim +PlugUpdate +qall
nvim +PlugInstall +qall
curl https://sh.rustup.rs -sSf | sh

# Setup Tmux
echo 'Installing Tmux plugins...'
~/.tmux/plugins/tpm/bin/install_plugins

# Setup Python
echo 'Install Python packages...'
bash ~/dotfiles/bash/pip_installs.sh

# Setup Scala, SBT, Spark
brew cask install java
brew install scala
brew install sbt
cd ~/Downloads/
wget https://d3kbcqa49mib13.cloudfront.net/spark-2.2.0-bin-hadoop2.7.tgz
mv spark-2.2.0-bin-hadoop2.7 ~/opt/spark

# Setup Haskell
brew install stack

# Install the following manually:
# Chrome, Dropbox, Texmaker, R, RStudio, Spotify, VLC, iTerm, Skype,
# Transmission, Air Display, Google Drive, iStat Menus, Kindle, Latex, Line,
# Mendeley, Sublime Text, Microsoft Office,
# Karabiner-Elements, PyPy

