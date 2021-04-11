# Conda base default
conda config --set auto_activate_base false

# Agree to T&C
sudo xcodebuild -license

# Homebrew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"

# ZSH
brew install zsh
sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
sudo chsh -s /usr/bin/zsh $USER
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git \
    ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
git clone https://github.com/zsh-users/zsh-autosuggestions \
    ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
# If zsh complains about permissions:
# chmod 755 /usr/local/share/zsh
# chmod 755 /usr/local/share/zsh/site-functions

# Essentials
brew install bash bat entr gnupg2 mosh node pandoc postgresql ripgrep wget yarn

# Git LFS
brew install git-fls
sudo git lfs install --system

# SSH keys: GH, GL and Jarwin
ssh-keygen

# Clone dotfiles
git clone https://github.com/anthony-khong/dotfiles.git
cd dotfiles && git submodule init && git submodule update
/bin/bash -c "source ~/dotfiles/bash/bash_profile"
/bin/bash $HOME/dotfiles/bash/recreate_symbolic_links

# Anaconda + Python + Utility Libraries
wget https://repo.anaconda.com/archive/Anaconda3-2020.07-MacOSX-x86_64.sh
pip install numpy pandas scikit-learn scipy statsmodels
pip install matplotlib seaborn
pip install lightgbm dask[complete] jax jaxlib xgboost
pip install click cython cytoolz ipython mypy pdbpp
pip install pytest pytest-cov
pip install speedtest-cli youtube-dl

# Tmux + Plugins + Tmate
brew install tmux tmate
/bin/bash $HOME/dotfiles/tmux/tpm/scripts/install_plugins.sh

# Java
# M1: https://www.azul.com/downloads/zulu-community/?package=jdk
brew tap AdoptOpenJDK/openjdk
brew install adoptopenjdk11

# Clojure + Lein + clj-kondo + babashka
brew install clojure/tools/clojure

curl -O https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
sudo mv lein /usr/local/bin/
chmod a+x /usr/local/bin/lein
sudo mkdir -p $HOME/.lein && sudo chown -R $USER $HOME/.lein

curl -sLO https://raw.githubusercontent.com/borkdude/clj-kondo/master/script/install-clj-kondo
chmod +x install-clj-kondo
./install-clj-kondo
rm install-clj-kondo

bash <(curl -s https://raw.githubusercontent.com/borkdude/babashka/master/install)

go get github.com/cespare/goclj/cljfmt

# Ruby
brew install rbenv
rbenv init
rbenv install 2.7.2 # or check the latest stable version

# Neovim
pip install --upgrade neovim jedi google-api-python-client pyflakes mypy msgpack pynvim
brew install neovim
# M1:
# brew install --HEAD tree-sitter
# brew install --HEAD luajit
# brew install --HEAD neovim
nvim +PlugInstall +qall || true

# Rust + Parinfer
curl https://sh.rustup.rs -sSf | sh -s -- -y
export PATH="$HOME/.cargo/bin:$PATH"
cd ~/dotfiles/vim/plugged/parinfer-rust \
    && make install \
    && cargo build --release \
    && cargo install --force \
    && cd $HOME

# Gcloud SDK
wget https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/google-cloud-sdk-313.0.1-darwin-x86_64.tar.gz
tar -xf google-cloud-sdk-313.0.1-darwin-x86_64.tar.gz google-cloud-sdk/
./google-cloud-sdk/install.sh
./google-cloud-sdk/bin/gcloud init

# GPG

gpg --gen-key
gpg --list-keys --keyid-format LONG
gpg --keyserver hkp://hkps.pool.sks-keyservers.net --send-keys $PUBLIC_KEY_ID
