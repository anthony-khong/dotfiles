# TODO: install git, install tmux, install vim
# TODO: install all other stuff from my_setup.md
cd;

if [ "$(uname)" == "Darwin" ]; then
    echo 'Setting up Mac...';
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    echo 'Setting up Linux...';
fi


# Setup dotfiles
echo 'Cloning dotfiles repo...';
git clone https://github.com/anthony-khong/dotfiles_akk.git;
cd ~/dotfiles_akk;
git submodule init;
git submodule update;
cd vim/bundle/jedi-vim/;
git submodule init;
git submodule update;

# Make dotfiles source from the repo
echo 'Making dotfiles source from dotfiles_akk...';
cd;

rm .tmux.conf;
echo "source-file ~/dotfiles_akk/tmux.conf" > .tmux.conf;
rm .vimrc;
echo "set runtimepath^=~/dotfiles_akk/vim
source ~/dotfiles_akk/vim/vimrc" > .vimrc;

if [ "$(uname)" == "Darwin" ]; then
    rm .bash_profile
    echo "source ~/dotfiles_akk/bash_profile" > .bash_profile
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    rm .bashrc
    echo "source ~/dotfiles_akk/bashrc" > .bashrc
fi


