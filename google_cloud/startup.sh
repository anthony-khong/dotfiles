#! /bin/bash
export USER="akhong"
export HOME="/home/akhong"

sudo apt-get update && sudo apt-get install -y \
    build-essential \
    apt-transport-https \
    ca-certificates \
    curl \
    git \
    libssl-dev \
    make \
    software-properties-common \
    tmux \
    xclip

# Docker
curl -fsSL https://download.docker.com/linux/ubuntu/gpg \
    | sudo apt-key add -

sudo add-apt-repository \
   "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
   $(lsb_release -cs) \
   stable"

sudo apt-get update && sudo apt-get install -y docker-ce

# ZSH
sudo apt-get update && sudo apt-get install -y zsh
wget https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -O - | sudo zsh
sudo chsh -s /usr/bin/zsh $USER

# Miniconda
wget https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh -O ~/miniconda.sh
sudo bash ~/miniconda.sh -b -p /opt/anaconda && rm ~/miniconda.sh
sudo chown -R $USER /opt/anaconda/
export PATH="/opt/anaconda/bin:$PATH"
pip install --upgrade pip

# Dotfiles
cd $HOME \
    && git clone https://github.com/anthony-khong/dotfiles.git \
    && cd $HOME/dotfiles \
    && git submodule init \
    && git submodule update \
    && cd $HOME \
    && /bin/bash -c "source ~/dotfiles/bash/bashrc" \
    && /bin/bash $HOME/dotfiles/bash/recreate_symbolic_links

# Neovim
sudo add-apt-repository -y ppa:neovim-ppa/stable \
        && sudo apt-get update \
        && sudo apt-get install -y neovim \
        && pip install --upgrade neovim jedi google-api-python-client \
        && nvim +PlugInstall +silent +qall \
        && /bin/bash $HOME/dotfiles/tmux/tpm/scripts/install_plugins.sh
sudo chown -R $USER "$HOME/.local"
sed -i '/fzf.bash/d' .bashrc

# Ripgrep
curl -LO https://github.com/BurntSushi/ripgrep/releases/download/0.10.0/ripgrep_0.10.0_amd64.deb
sudo dpkg -i ripgrep_0.10.0_amd64.deb && rm ripgrep_0.10.0_amd64.deb

# Parinfer
curl https://sh.rustup.rs -sSf | sh -s -- -y
export PATH="$HOME/.cargo/bin:$PATH"
cd ~/dotfiles/vim/plugged/parinfer-rust \
    && make install \
    && cargo build --release \
    && cargo install --force \
    && cd $HOME

# Wrap up
sudo chown -R akhong $HOME
