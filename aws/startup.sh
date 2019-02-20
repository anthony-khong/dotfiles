#! /bin/bash
sudo adduser --disabled-password --gecos "" akhong
export USER="akhong"
export HOME="/home/akhong"
export INSTALL_LOG="$HOME/.startup.log"

sudo apt-get update && sudo apt-get install -y build-essential git

mkdir -p $HOME && \
    cd $HOME && \
    git clone https://github.com/anthony-khong/dotfiles.git

if [[ ! -e "$INSTALL_LOG" ]]; then
    /bin/bash $HOME/dotfiles/aws_cloud/startup_install.sh
fi
