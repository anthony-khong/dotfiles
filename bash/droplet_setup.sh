echo 'Setting up Linux...';

# Essentials
sudo apt-get -y update
sudo apt-get install -y build-essential
sudo apt-get install -y htop
sudo apt-get install -y git
sudo apt-get install -y silversearcher-ag

# Workflow: Neovim + Tmux
sudo add-apt-repository ppa:neovim-ppa/unstable
sudo apt-get -y update
sudo apt-get install -y neovim
sudo apt-get install -y tmux
sudo apt-get install -y xclip

# Workflow: dotfiles
cd ~
git clone https://github.com/anthony-khong/dotfiles.git
cd ~/dotfiles
git submodule init
git submodule update
cd ~
source ~/dotfiles/bash/bash_shortcuts

# Workflow: Minimal Anaconda + Nvim installation
cd ~
sudo wget https://repo.continuum.io/archive/Anaconda3-4.3.0-Linux-x86_64.sh
bash Anaconda3-4.3.0-Linux-x86_64.sh
pip install neovim
pip install jedi
recreate_symbolic_links
nvim +PlugUpgrade +qall
nvim +PlugInstall +qall
nvim +PlugUpdate +qall
nvim +PlugInstall +qall
source ~/.bashrc

# Docker
sudo apt-get -y update
sudo apt-get install -y linux-image-extra-$(uname -r) linux-image-extra-virtual
sudo apt-get install -y apt-transport-https ca-certificates curl software-properties-common
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable"
sudo apt-get -y update
sudo apt-get install -y docker-ce

# Dropbox
cd ~ && wget -O - "https://www.dropbox.com/download?plat=lnx.x86_64" | tar xzf -
~/.dropbox-dist/dropboxd

# Gmail
pip install --upgrade google-api-python-client
