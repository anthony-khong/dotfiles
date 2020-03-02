#! /bin/bash
export USER="ubuntu"
export HOME="/home/ubuntu"
export INSTALL_LOG="$HOME/.startup.log"

echo "Starting install..." >> $INSTALL_LOG
sudo apt-get update && sudo apt-get install -y build-essential git

echo "Cloned dotfiles..." >> $INSTALL_LOG
cd $HOME && git clone https://github.com/anthony-khong/dotfiles.git

echo "Starting install script..." >> $INSTALL_LOG
if [[ ! -e "$INSTALL_LOG" ]]; then
    /bin/bash $HOME/dotfiles/google_cloud/startup_install.sh
fi
