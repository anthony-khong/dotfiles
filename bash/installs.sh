#! /bin/bash
export INSTALL_LOG="$HOME/.startup.log"

echo "Installing mosh..." >> $INSTALL_LOG
sudo apt-get update && sudo apt-get install -y mosh
sudo ufw allow 60000:61000/udp

echo "Installing essential apps with apt-get..." >> $INSTALL_LOG
sudo apt-get update && sudo apt-get install -y \
    build-essential apt-transport-https ca-certificates \
    curl entr git fzf htop jq libssl-dev make pandoc \
    software-properties-common shellcheck tmux unzip xclip
sudo apt-get update && sudo apt-get install -y \
    clang-format clang-tidy clang-tools clang libc++-dev \
    libc++1 libc++abi-dev libc++abi1 libclang-dev libclang1 \
    libomp-dev libomp5 lld lldb llvm-dev llvm-runtime llvm

echo "Installing Docker..." >> $INSTALL_LOG
sudo apt-get update && sudo apt-get install -y ca-certificates curl gnupg lsb-release
sudo install -m 0755 -d /etc/apt/keyrings
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg
sudo chmod a+r /etc/apt/keyrings/docker.gpg
echo "deb [arch="$(dpkg --print-architecture)" signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
  "$(. /etc/os-release && echo "$VERSION_CODENAME")" stable" | \
  sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
sudo apt-get update && sudo apt-get install -y \
    docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin

echo "Setting up permissions and Docker..." >> $INSTALL_LOG
sudo chown -R $USER $HOME/dotfiles
sudo usermod -a -G docker $USER
sudo usermod -aG sudo $USER
newgrp docker

echo "Installing Miniconda..." >> $INSTALL_LOG
wget https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh -O ~/miniconda.sh
sudo bash ~/miniconda.sh -b -p /opt/anaconda && sudo rm ~/miniconda.sh
sudo chown -R $USER /opt/anaconda/
export PATH="/opt/anaconda/bin:$PATH"
pip3 install --upgrade pip

echo "Configuring dotfiles..." >> $INSTALL_LOG
cd $HOME && git clone https://github.com/anthony-khong/dotfiles.git
cd $HOME/dotfiles \
    && git submodule init \
    && git submodule update \
    && cd $HOME \
    && /bin/bash -c "source ~/dotfiles/bash/bashrc" \
    && /bin/bash $HOME/dotfiles/bash/recreate_symbolic_links

echo "Installing ZSH..." >> $INSTALL_LOG
sudo apt-get update && sudo apt-get install -y zsh
wget https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -O - | zsh
sudo chsh -s /usr/bin/zsh $USER
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git \
    ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
git clone https://github.com/zsh-users/zsh-autosuggestions \
    ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions

echo "Installing Neovim Dep - Tailwind & Bash..." >> $INSTALL_LOG
sudo apt install -y nodejs npm
sudo chown -R $USER /usr/local/lib/node_modules
sudo npm install -g @tailwindcss/language-server emmet-ls prettier prettier-plugin-tailwindcss
sudo npm install -g --allow-scripts=tree-sitter-cli tree-sitter-cli
cd ~/.local/share/nvim/site/pack/packer/start/tailwind-sorter.nvim/formatter \
    && npm i && npm run build

echo "Installing Rust & Rust Libs..." >> $INSTALL_LOG
curl https://sh.rustup.rs -sSf | sh -s -- -y
sudo apt-get update && sudo apt-get install -y ripgrep
cd ~/dotfiles/vim/plugged/parinfer-rust \
    && make install \
    && cargo build --release \
    && cargo install --force \
    && cd $HOME

echo "Installing Neovim Dep - Python..." >> $INSTALL_LOG
pip install pynvim python-lsp-server pyright

echo "Installing Neovim Dep - Elixir..." >> $INSTALL_LOG
curl -fLO https://github.com/elixir-lsp/elixir-ls/releases/download/v0.31.1/elixir-ls-v0.31.1.zip
mkdir -p ~/.elixir-ls
unzip elixir-ls-v0.31.1.zip -d ~/.elixir-ls/release
chmod +x ~/.elixir-ls/release/language_server.sh
rm elixir-ls-v0.31.1.zip

echo "Installing Neovim..." >> $INSTALL_LOG
curl -LO https://github.com/neovim/neovim/releases/download/v0.12.5/nvim-linux-x86_64.appimage
sudo mv nvim-linux-x86_64.appimage /usr/local/bin/nvim
chmod u+x /usr/local/bin/nvim

echo "Installing Tmux Plugins..." >> $INSTALL_LOG
/bin/bash $HOME/dotfiles/tmux/tpm/scripts/install_plugins.sh
sudo mkdir -p $HOME/.local && sudo chown -R $USER "$HOME/.local"

echo "Installing Erlang and Elixir..." >> $INSTALL_LOG
sudo apt-get -y install build-essential autoconf m4 \
    libwxgtk3.2-dev libwxgtk-webview3.2-dev libgl1-mesa-dev libglu1-mesa-dev \
    libpng-dev libssh-dev unixodbc-dev xsltproc fop libxml2-utils libncurses-dev \
    openjdk-11-jdk
curl -LO https://github.com/asdf-vm/asdf/releases/download/v0.20.0/asdf-v0.20.0-linux-amd64.tar.gz
tar -xvzf asdf-v0.20.0-linux-amd64.tar.gz
sudo mv asdf /usr/local/bin/asdf
rm asdf-v0.20.0-linux-amd64.tar.gz
asdf plugin add erlang
asdf plugin add elixir
KERL_BUILD_DOCS=yes KERL_INSTALL_MANPAGES=yes KERL_INSTALL_HTMLDOCS=yes asdf install erlang 29.0
asdf set --home erlang 29.0
asdf install elixir 1.20.3-otp-29
asdf set --home elixir 1.20.3-otp-29
mix local.hex --force
mix archive.install hex phx_new --force

echo "Installing Python Data Libraries..." >> $INSTALL_LOG
pip install \
    click cytoolz ipython pdbpp mypy hypothesis pytest pytest-cov \
    jax jaxlib matplotlib numpy pandas scikit-image scikit-learn scipy \
    "dask[complete]" lightgbm pyarrow fastparquet xgboost

 echo "Creating 32G of swap file..." >> $INSTALL_LOG
 sudo fallocate -l 32G /swapfile
 sudo chmod 600 /swapfile
 sudo mkswap /swapfile
 sudo swapon /swapfile
 sudo cp /etc/fstab /etc/fstab.bak
 echo '/swapfile none swap sw 0 0' | sudo tee -a /etc/fstab

echo "Setting up Gitlab runner..." >> $INSTALL_LOG
docker volume create gitlab-runner-config
docker run -d --name gitlab-runner --restart always \
    -v /var/run/docker.sock:/var/run/docker.sock \
    -v gitlab-runner-config:/etc/gitlab-runner \
    gitlab/gitlab-runner:latest
docker run --rm -it -v gitlab-runner-config:/etc/gitlab-runner \
    gitlab/gitlab-runner:latest register --url XXX  --token XXX
docker run --rm -it -v gitlab-runner-config:/etc/gitlab-runner gitlab/gitlab-runner:latest verify
sudo cat /var/lib/docker/volumes/gitlab-runner-config/_data/config.toml

echo "Setup complete!" >> $INSTALL_LOG
