FROM tensorflow/tensorflow:1.8.0-gpu-py3

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

## Python development
RUN pip install coconut[watch] \
    funcy \
    lightgbm \
    pdbpp \
    tabulate \
    && apt-get install -y python3-tk

RUN pip install ipython==5.7.0 # Otherwise tmux copy-paste does not work

## Haskell
## Clojure
## Scala
