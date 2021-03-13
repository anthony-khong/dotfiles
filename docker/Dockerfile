FROM ubuntu:20.04

RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y \
    build-essential \
    apt-transport-https \
    ca-certificates \
    cloud-utils \
    curl \
    entr \
    git \
    libssl-dev \
    make \
    pandoc \
    ripgrep \
    software-properties-common \
    tmux \
    unzip \
    xclip

RUN git clone https://github.com/anthony-khong/dotfiles.git /root/dotfiles \
    && cd /root/dotfiles \
    && git submodule init \
    && git submodule update \
    && cd $HOME \
    && /bin/bash -c "source ~/dotfiles/bash/bashrc" \
    && /bin/bash $HOME/dotfiles/bash/recreate_symbolic_links

RUN wget https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh -O ~/miniconda.sh \
    && bash ~/miniconda.sh -b -p /opt/anaconda \
    && rm ~/miniconda.sh

RUN PATH="/opt/anaconda/bin:$PATH" pip install \
        --upgrade neovim jedi google-api-python-client pyflakes mypy \
    && add-apt-repository ppa:neovim-ppa/stable -y \
    && apt-get update \
    && apt-get install -y neovim \
    && nvim +PlugInstall +silent +qall || true \
    && /bin/bash $HOME/dotfiles/tmux/tpm/scripts/install_plugins.sh

RUN PATH="/opt/anaconda/bin:$PATH" pip install \
    cytoolz ipython pdbpp mypy pytest pytest-cov \
    matplotlib numpy pandas scikit-learn scipy 

RUN apt-get update \
    && apt-get install -y zsh \
    && wget https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -O - | zsh \
    && chsh -s /usr/bin/zsh root \
    && git clone https://github.com/zsh-users/zsh-syntax-highlighting.git \
        ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting \
    && git clone https://github.com/zsh-users/zsh-autosuggestions \
        ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
