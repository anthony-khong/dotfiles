: '
Add Dvorak keyboard + remap <Win-Space> to change layout.
Adjust keyboard repeat delay and repeat speed.
Create shortcuts for snapping.
Remap <Ctrl-Alt-T> to `gnome-terminal --window --full-screen`.
Change terminal themes.

Change theme to New-Minty.
Change font defaults to 11, 12, ..., 12.
Clean up panels + increase size + show date.
'

sudo apt-get update
sudo apt-get install build-essential
sudo apt-get install fonts-inconsolata -y

apt-get update \
    && apt-get install -y build-essential \
        ca-certificates \
        curl \
        git \
        libssl-dev \
        make \
        tmux \
        wget \
        xclip

cd $HOME \
    && git clone https://github.com/anthony-khong/dotfiles.git \
    && cd $HOME/dotfiles \
    && git submodule init \
    && git submodule update \
    && cd $HOME \
    && /bin/bash -c "source ~/dotfiles/bash/bashrc" \
    && /bin/bash $HOME/dotfiles/bash/recreate_symbolic_links; exit 0

cd $HOME/Downloads \
    && wget https://repo.anaconda.com/archive/Anaconda3-5.2.0-Linux-x86_64.sh \
    && sudo sh Anaconda3-5.2.0-Linux-x86_64.sh \
    cd $HOME

apt-get install -y software-properties-common python-software-properties \
        && add-apt-repository -y ppa:neovim-ppa/stable \
        && apt-get update \
        && apt-get install -y neovim \
        && pip install --upgrade neovim jedi google-api-python-client \
        && nvim +PlugInstall +qall \
        && /bin/bash $HOME/dotfiles/tmux/tpm/scripts/install_plugins.sh

curl -LO https://github.com/BurntSushi/ripgrep/releases/download/0.8.1/ripgrep_0.8.1_amd64.deb \
    dpkg -i ripgrep_0.8.1_amd64.deb && rm ripgrep_0.8.1_amd64.deb

pip install coconut[watch] \
    funcy \
    ipython==5.7.0 \
    jedi \
    lightgbm \
    neovim \
    requests \
    pdbpp \
    pytest \
    pywebhdfs \
    tabulate \
    && apt-get install -y python3-tk

sudo apt-get install git gcc make pkg-config libx11-dev libxtst-dev libxi-dev \
    && cd $HOME \
    && git clone https://github.com/alols/xcape.git \
    && cd xcape \
    && make \
    && sudo make install
