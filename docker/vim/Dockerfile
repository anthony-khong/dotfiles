FROM ubuntu:latest

MAINTAINER Anthony Khong <anthony.kusumo.khong@gmail.com>

RUN apt-get update && apt-get install -y bat curl fzf git neovim

RUN mkdir -p ~/.local/bin && ln -s /usr/bin/batcat ~/.local/bin/bat

RUN curl -LO https://github.com/BurntSushi/ripgrep/releases/download/11.0.2/ripgrep_11.0.2_amd64.deb \
    && dpkg -i ripgrep_11.0.2_amd64.deb

RUN curl -fLo /root/.config/nvim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

ADD init.vim /root/.config/nvim/init.vim

RUN nvim +PlugInstall +qall

ADD bashrc /root/.bashrc
