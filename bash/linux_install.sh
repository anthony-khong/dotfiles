: '
Add Dvorak keyboard + remap <Win-Space> to change layout.
Adjust keyboard repeat delay and repeat speed.
Create shortcuts for snapping.
Remap <Ctrl-Alt-T> to `gnome-terminal --window --full-screen`.
Remap <C-Left> and <C-Right> for workspaces.
Change terminal themes.

Change all themes to Mint-Y Dark.
Change font defaults to 11, 12, ..., 12.
Clean up panels + increase size + show date.
Install Pandoc and  Dropbox.

Install Slack.
Add Agoda certificates.
Setup Pulse Secure.
Setup Github SSH key.
Setup Shoiberg.
'

sudo apt-get update
sudo apt-get install -y build-essential
sudo apt-get install -y fonts-inconsolata

sudo apt-get update \
    && sudo apt-get install -y build-essential \
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

sudo apt-get install -y software-properties-common python-software-properties \
        && sudo add-apt-repository -y ppa:neovim-ppa/stable \
        && sudo apt-get update \
        && sudo apt-get install -y neovim \
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
    pyarrow \
    pytest \
    pywebhdfs \
    tabulate \
    tensorflow \
    && apt-get install -y python3-tk

sudo apt-get install -y git gcc make pkg-config libx11-dev libxtst-dev libxi-dev \
    && cd $HOME \
    && git clone https://github.com/alols/xcape.git \
    && cd xcape \
    && make \
    && sudo make install

sudo add-apt-repository ppa:gnome3-team/gnome3 \
    && sudo apt-get update \
    && sudo apt-get install -y evince

sudo apt-get update \
    && sudo apt-get install -y \
        apt-transport-https \
        ca-certificates \
        curl \
        software-properties-common \
    && cd $HOME/Downloads \
    && curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add - \
    && sudo add-apt-repository \
        "deb [arch=amd64] https://download.docker.com/linux/ubuntu xenial stable" \
    && cd $HOME \
    && sudo apt-get update \
    && sudo apt-get install -y docker-ce \
    && sudo groupadd docker \
    && sudo usermod -aG docker $USER

echo "deb https://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list \
    && sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823 \
    && sudo apt-get update \
    && sudo apt-get install -y sbt

curl -sSL https://get.haskellstack.org/ | sh


sudo apt-get install -y gcc python-dev libkrb5-dev \
    && pip install pywinrm[kerberos] \
    && pip install sparkmagic \
    && jupyter nbextension enable --py --sys-prefix widgetsnbextension \

: '
If there are permission issues, do:
cd /usr/local/share/ \
    && sudo chown -R akhong .
    && cd $HOME
'
cd /opt/anaconda/lib/python3.6/site-packages/ \
    && jupyter-kernelspec install sparkmagic/kernels/sparkkernel \
    && jupyter-kernelspec install sparkmagic/kernels/pysparkkernel \
    && jupyter-kernelspec install sparkmagic/kernels/pyspark3kernel \
    && jupyter-kernelspec install sparkmagic/kernels/sparkrkernel \
    && pip install pandas==0.22

sudo apt-get install -y texlive && sudo apt-get install -y lmodern

sudo apt install gdebi \
    && wget https://github.com/KELiON/cerebro/releases/download/v0.3.0/cerebro_0.3.0_amd64.deb \
    && sudo gdebi cerebro_0.3.0_amd64.deb

sudo apt install plank \
    && sudo apt-get install -y unrar unzip python3 libgtk-3-0 \
    && cd $HOME/Downloads \
    && wget https://github.com/karim88/PlankSetting/archive/master.zip \
    && unzip master.zip cd PlankSetting-master/ \
    && sudo ./install.sh \
    && cd $HOME

cd $HOME/Downloads \
    && curl https://www.mendeley.com/repositories/ubuntu/stable/amd64/mendeleydesktop-latest \
    && sudo dpkg -i mendeleydesktop*.deb \
    && sudo apt-get update \
    && sudo apt-get install -y mendeleydesktop \
    && cd $HOME

sudo add-apt-repository ppa:nilarimogard/webupd8 \
    && sudo apt-get update \
    && sudo apt-get install -y grive
