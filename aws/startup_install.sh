#! /bin/bash
export USER="ubuntu"
export HOME="/home/ubuntu"
export INSTALL_LOG="$HOME/.startup.log"

echo "Installing mosh..." >> $INSTALL_LOG
sudo apt-get update && sudo apt-get install -y mosh
# sudo ufw allow 60000:61000/udp

echo "Installing essential apps with apt-get..." >> $INSTALL_LOG
sudo apt-get update && sudo apt-get install -y \
    build-essential \
    apt-transport-https \
    ca-certificates \
    curl \
    entr \
    git \
    fzf \
    htop \
    jq \
    libssl-dev \
    make \
    pandoc \
    software-properties-common \
    shellcheck \
    tmux \
    unzip \
    xclip
sudo apt-get update && sudo apt-get install -y \
    clang-format clang-tidy clang-tools clang libc++-dev \
    libc++1 libc++abi-dev libc++abi1 libclang-dev libclang1 \
    libomp-dev libomp5 lld lldb llvm-dev llvm-runtime llvm
sudo apt-get install -y cloud-utils
sudo apt-get install -y openssh-server && sudo systemctl status ssh && sudo ufw allow ssh

echo "Installing AWS CLI..." >> $INSTALL_LOG
curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip"
unzip awscliv2.zip
sudo ./aws/install
rm -rf aws
rm awscli*.zip

echo "Installing Docker..." >> $INSTALL_LOG
sudo apt-get update && sudo apt-get install \
    ca-certificates \
    curl \
    gnupg \
    lsb-release
sudo mkdir -p /etc/apt/keyrings
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg
 echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
  $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
sudo apt-get update && sudo apt-get install docker-ce docker-ce-cli containerd.io docker-compose-plugin docker-compose

echo "Installing ZSH..." >> $INSTALL_LOG
sudo apt-get update && sudo apt-get install -y zsh
wget https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -O - | zsh
sudo chsh -s /usr/bin/zsh $USER
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git \
    ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
git clone https://github.com/zsh-users/zsh-autosuggestions \
    ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions

echo "Installing Miniconda..." >> $INSTALL_LOG
wget https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh -O ~/miniconda.sh
sudo bash ~/miniconda.sh -b -p /opt/anaconda && sudo rm ~/miniconda.sh
sudo chown -R $USER /opt/anaconda/
export PATH="/opt/anaconda/bin:$PATH"
pip3 install --upgrade pip

echo "Installing DVC..." >> $INSTALL_LOG
conda install -y -c conda-forge mamba
mamba install -y -c conda-forge dvc
pip3 install "dvc[all]"

echo "Setting up Git..." >> $INSTALL_LOG
git config --global user.email "anthony.kusumo.khong@gmail.com"
git config --global user.name "Anthony Khong"
curl -s https://packagecloud.io/install/repositories/github/git-lfs/script.deb.sh | sudo bash
sudo apt-get install -y git-lfs
git lfs install

echo "Configuring dotfiles..." >> $INSTALL_LOG
cd $HOME/dotfiles \
    && git submodule init \
    && git submodule update \
    && cd $HOME \
    && /bin/bash -c "source ~/dotfiles/bash/bashrc" \
    && /bin/bash $HOME/dotfiles/bash/recreate_symbolic_links

echo "Installing Neovim + dependencies..." >> $INSTALL_LOG
curl https://get.volta.sh | bash
/bin/bash -c "volta install node"
curl https://sh.rustup.rs -sSf | sh -s -- -y
pip3 install --upgrade neovim jedi google-api-python-client pyflakes mypy
sudo add-apt-repository ppa:neovim-ppa/stable -y \
    && sudo apt-get update \
    && sudo apt-get install -y neovim \
    && /bin/bash $HOME/dotfiles/tmux/tpm/scripts/install_plugins.sh
sudo mkdir -p $HOME/.local && sudo chown -R $USER "$HOME/.local"
# nvim +PlugInstall +silent +qall

echo "Installing Ripgrep..." >> $INSTALL_LOG
sudo apt-get update && sudo apt-get install -y ripgrep

echo "Installing Parinfer..." >> $INSTALL_LOG
curl https://sh.rustup.rs -sSf | sh -s -- -y
export PATH="$HOME/.cargo/bin:$PATH"
cd ~/dotfiles/vim/plugged/parinfer-rust \
    && make install \
    && cargo build --release \
    && cargo install --force \
    && cd $HOME

echo "Installing Tmate..." >> $INSTALL_LOG
sudo apt-get update && sudo apt-get install -y tmate

echo "Setting up permissions and Docker..." >> $INSTALL_LOG
sudo chown -R $USER $HOME/dotfiles
cd $HOME/dotfiles \
    && git checkout . \
    && cd $HOME
sudo usermod -a -G docker $USER
sudo usermod -aG sudo $USER
newgrp docker

echo "Installing Java..." >> $INSTALL_LOG
wget -qO - https://adoptopenjdk.jfrog.io/adoptopenjdk/api/gpg/key/public | sudo apt-key add -
sudo add-apt-repository --yes https://adoptopenjdk.jfrog.io/adoptopenjdk/deb/
sudo apt-get update && sudo apt-get install -y adoptopenjdk-11-hotspot

echo "Installing Lein..." >> $INSTALL_LOG
wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
sudo mv lein /usr/local/bin/
chmod a+x /usr/local/bin/lein
lein || true

echo "Installing Geni and its depencies..." >> $INSTALL_LOG
wget https://raw.githubusercontent.com/zero-one-group/geni/develop/scripts/geni
chmod a+x geni
sudo mv geni /usr/local/bin/
exit | geni
git clone https://github.com/anthony-khong/geni.git \
    && cd geni \
    && lein deps \
    && cd $HOME

echo "Installing common Python libraries ..." >> $INSTALL_LOG
pip install \
    click cytoolz ipython pdbpp mypy hypothesis pytest pytest-cov \
    jax jaxlib matplotlib numpy pandas scikit-image scikit-learn scipy \
    "dask[complete]" lightgbm pyarrow fastparquet xgboost

 echo "Creating 32G of swap file..." >> $INSTALL_LOG
 sudo fallocate -l 8G /swapfile
 sudo chmod 600 /swapfile
 sudo mkswap /swapfile
 sudo swapon /swapfile
 sudo cp /etc/fstab /etc/fstab.bak
 echo '/swapfile none swap sw 0 0' | sudo tee -a /etc/fstab

echo "Setup complete!" >> $INSTALL_LOG
