#! /bin/bash
export USER="ubuntu"
export HOME="/home/ubuntu"
export INSTALL_LOG="$HOME/.startup.log"

sudo apt-get update && sudo apt-get install -y build-essential git

cd $HOME && git clone https://github.com/anthony-khong/dotfiles.git

if [[ ! -e "$INSTALL_LOG" ]]; then
    /bin/bash $HOME/dotfiles/google_cloud/startup_install.sh
fi
