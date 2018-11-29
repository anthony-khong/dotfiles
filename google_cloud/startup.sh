#! /bin/bash
export USER="akhong"
export HOME="/home/akhong"

sudo apt-get update && sudo apt-get install -y build-essential git

cd $HOME && git clone https://github.com/anthony-khong/dotfiles.git

export DONE_FILE="$HOME/.startup_done"
if [[ ! -e "$DONE_FILE" ]]; then
    /bin/bash dotfiles/google_cloud/startup_install.sh
    touch $DONE_FILE
fi
