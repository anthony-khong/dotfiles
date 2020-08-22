FROM ubuntu:latest

MAINTAINER Anthony Khong <anthony.kusumo.khong@gmail.com>

ENV USER root
ENV HOME /root
ENV TERM=screen-256color
WORKDIR /root
ENTRYPOINT /bin/bash

# Essentials
RUN apt-get update \
    && apt-get install -y build-essential \
        ca-certificates \
        curl \
        git \
        libssl-dev \
        make \
        tmux \
        xclip

# Dotfiles
RUN cd $HOME \
    && git clone https://github.com/anthony-khong/dotfiles.git \
    && cd $HOME/dotfiles \
    && git submodule init \
    && git submodule update \
    && cd $HOME \
    && /bin/bash -c "source ~/dotfiles/bash/bashrc" \
    && /bin/bash $HOME/dotfiles/bash/recreate_symbolic_links; exit 0

## Neovim
RUN apt-get install -y software-properties-common python-software-properties \
        && add-apt-repository -y ppa:neovim-ppa/stable \
        && apt-get update \
        && apt-get install -y neovim \
        && pip install --upgrade neovim jedi google-api-python-client \
        && nvim +PlugInstall +qall \
        && /bin/bash $HOME/dotfiles/tmux/tpm/scripts/install_plugins.sh

## Rip Grep
RUN curl -LO https://github.com/BurntSushi/ripgrep/releases/download/0.8.1/ripgrep_0.8.1_amd64.deb
RUN dpkg -i ripgrep_0.8.1_amd64.deb && rm ripgrep_0.8.1_amd64.deb

## Python
RUN pip install coconut[watch] \
    hypothesis \
    ipython==5.7.0 \
    jedi \
    lightgbm \
    neovim \
    pdbpp \
    pytest \
    pywebhdfs \
    requests \
    tabulate \
    && apt-get install -y python3-tk
