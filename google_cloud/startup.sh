#! /bin/bash
export USER="ubuntu"
export HOME="/home/ubuntu"
export INSTALL_LOG="$HOME/.startup.log"
sudo chown $USER $INSTALL_LOG

if [[ ! -e "$INSTALL_LOG" ]]; then
    echo "Starting install..." >> $INSTALL_LOG
    sudo apt-get update && sudo apt-get install -y build-essential git

    echo "Cloned dotfiles..." >> $INSTALL_LOG
    cd $HOME && git clone https://github.com/anthony-khong/dotfiles.git
    sudo chown -R $USER $HOME/dotfiles

    echo "Starting install script..." >> $INSTALL_LOG
    /bin/bash $HOME/dotfiles/google_cloud/startup_install.sh
fi
