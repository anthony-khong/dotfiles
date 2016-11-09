### Linux Fresh Install:
    - HDD: swap + home; SSD: root
    - sudo apt-get update
    - sudo apt-get install build-essential
    - Update .bashrc
    - Logitech T650: http://franklinstrube.com/blog/logitech-t650-wireless-touchpad-ubuntu/
        xinput set-button-map "Logitech Unifying Device. Wireless PID:4101" 1 3 1
        xinput set-button-map "Logitech Unifying Device. Wireless PID:4101" 1 3 1 5 4 7 6
        synclient MinSpeed=4
        synclient MaxSpeed=8
        xinput set-button-map "Logitech Rechargeable Touchpad T650" 1 3 1
        xinput set-button-map "Logitech Rechargeable Touchpad T650" 1 3 1 5 4 7 6

### Mac Fresh Install:
    - Install Homebrew
    - Update .bash_profile and .inputrc
    - .theanorc: cxxflags=-march=corei7
    - Get rid of dock delay: defaults write com.apple.dock autohide-time-modifier -int 0

### Softwares
1. Google Chrome
1. Dropbox (+ Google Drive)
1. CompizConfig Settings
    * Snap
    * Wallpaper
1. vim-gtk (.vim + .vimrc)
    * MacVim for Mac (alias vim='/path/to/MacVim.app/Contents/MacOS/Vim')
1. tmux (.tmux.conf)
    * iTerm (Mac) + Inconsolata
    * copy-paste
1. Gnome Tweak Tool
    * Bind Caps Lock to Control
1. htop + sensors
1. CUDA 7.0
    * http://www.r-tutor.com/gpu-computing/cuda-installation/cuda7.0-ubuntu
1. Python Kit
    * Anaconda
    * iPython
    * Theano + Pylearn
    * PyCUDA
    * Update Sklearn and Pandas
    * XGBoost
    * Pdb++
    * Speedtest
    * ggplot
1. git
1. R + RStudio
    * Rcpp
    * openBLAS
1. TexMaker + TexLive (or MacTex)
1. Pandoc
1. Haskell
1. Armadillo
1. VLC
1. noip2 + .initrc
1. Spotify
1. Antivirus
1. Theme Archway-3.10
1. Ag
