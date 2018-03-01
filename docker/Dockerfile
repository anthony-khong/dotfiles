FROM tensorflow/tensorflow:latest-py3

RUN apt-get update

# Essential toolings
RUN apt-get install software-properties-common && \
    add-apt-repository ppa:neovim-ppa/stable && \
    apt-get update && \
    apt-get install -y \
        build-essential \
        git \
        htop \
        neovim \
        r-base \
        silversearcher-ag \
        tmux \
        vim-gtk

# Python
RUN pip install \
    coconut \
    jedi \
    lightgbm \
    neovim \
    pdbpp \
    speedtest-cli \
    speedtest-cli \
    tabulate \
    xgboost \
    youtube-dl

RUN pip install pyflakes

RUN apt-get install -y python-tk

# Haskell
#RUN apt-get install -y haskell-platform
#RUN cabal update && cabal install pandoc
#RUN curl -sSL https://get.haskellstack.org/ | sh
# TODO: stack installs

# Scala

WORKDIR /root

