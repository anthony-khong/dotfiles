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
sudo apt-get update && sudo apt-get install -y \
    ca-certificates \
    curl \
    gnupg \
    lsb-release
sudo install -m 0755 -d /etc/apt/keyrings
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg
sudo chmod a+r /etc/apt/keyrings/docker.gpg
echo \
  "deb [arch="$(dpkg --print-architecture)" signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
  "$(. /etc/os-release && echo "$VERSION_CODENAME")" stable" | \
  sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
sudo apt-get update && sudo apt-get install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin

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
/bin/bash -c "npm install -g @tailwindcss/language-server emmet-ls prettier prettier-plugin-tailwindcss"
curl https://sh.rustup.rs -sSf | sh -s -- -y
pip3 install --upgrade neovim jedi google-api-python-client pyflakes mypy
pip3 install 'python-language-server[all]' jedi-language-server pyright python-lsp-black
curl -fLO https://github.com/elixir-lsp/elixir-ls/releases/download/v0.27.2/elixir-ls-v0.27.2.zip
mkdir -p ~/.elixir-ls
unzip elixir-ls-v0.27.2.zip -d ~/.elixir-ls/release
chmod +x ~/.elixir-ls/release/language_server.sh
sudo apt-get update && sudo apt-get install -y fuse libfuse2
curl -LO https://github.com/neovim/neovim/releases/latest/download/nvim.appimage
chmod u+x nvim.appimage
sudo mv nvim.appimage /usr/local/bin/nvim
/bin/bash $HOME/dotfiles/tmux/tpm/scripts/install_plugins.sh
sudo mkdir -p $HOME/.local && sudo chown -R $USER "$HOME/.local"
nvim +PackerInstall +silent +qall
cd ~/.local/share/nvim/site/pack/packer/start/tailwind-sorter.nvim/formatter \
    && npm i && npm run build

echo "Installing Erlang and Elixir..." >> $INSTALL_LOG
sudo apt-get -y install build-essential autoconf m4 libncurses5-dev \
    libwxgtk3.0-gtk3-dev libwxgtk-webview3.0-gtk3-dev libgl1-mesa-dev \
    libglu1-mesa-dev libpng-dev libssh-dev unixodbc-dev xsltproc fop \
    libxml2-utils libncurses-dev openjdk-11-jdk
git clone https://github.com/asdf-vm/asdf.git ~/.asdf
source ~/.bashrc
asdf plugin add erlang
asdf plugin add elixir
KERL_BUILD_DOCS=yes KERL_INSTALL_MANPAGES=yes KERL_INSTALL_HTMLDOCS=yes asdf install erlang 26.2
asdf global erlang 26.2
asdf install elixir 1.16.0-otp-26
asdf global elixir 1.16.0-otp-26
mix local.hex --force
mix archive.install hex phx_new --force

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

echo "Installing OCaml..." >> $INSTALL_LOG
apt-get install -y opam
opam init -y
eval $(opam env)
opam update
opam install ocaml-lsp-server odoc ocamlformat utop --yes
opam install merlin --yes

 echo "Creating 32G of swap file..." >> $INSTALL_LOG
 sudo fallocate -l 8G /swapfile
 sudo chmod 600 /swapfile
 sudo mkswap /swapfile
 sudo swapon /swapfile
 sudo cp /etc/fstab /etc/fstab.bak
 echo '/swapfile none swap sw 0 0' | sudo tee -a /etc/fstab

echo "Setup complete!" >> $INSTALL_LOG
