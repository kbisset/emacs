#!/bin/bash
if [ ! -L ~/.bashrc ]; then
    echo "Setting up bashrc"
    mv ~/.bashrc ~/.bashrc.orig
    ln -s ~/emacs/config/.bashrc .
fi

if [ ! -L ~/.inputrc ]; then
    echo "Setting up inputrc"
    ln -s ~/emacs/config/.inputrc .
fi

if [ ! -L ~/.emacs ]; then
    echo "Setting up emacs"
    ln -s ~/emacs/.emacs ~
fi

if [ ! -L ~/.fonts ]; then
    echo "Setting up fontd"
    ln -s ~/emacs/config/fonts ~/.fonts
    ~/.fonts/install.sh
fi

if [ ! -L ~/.emacs.d ]; then
    echo "Setting up emacs package repo"
    ln -s ~/emacs/config/.emacs.d ~/.emacs.d
    ~/.fonts/install.sh
fi

if [ ! -f ~/.ssh/id_rsa.pub ]; then
    echo "Setting up ssh"
    mkdir -p ~/.ssh
    cp ~/emacs/config/ssh/id_rsa.pub ~/.ssh/id_rsa.pub
    if [ ! -f ~/.ssh/authorized_keys ]; then
        cp ~/.ssh/id_rsa.pub ~/.ssh/authorized_keys
    fi
fi

if [ ! -L ~/.gitconfig ]; then
    echo "Setting up git"
    ln -s ~/emacs/config/.gitconfig ~/.gitconfig
fi

if [ ! -L ~/bin ]; then
    echo "Setting up bin"
    ln -s ~/emacs/config/bin ~/bin
fi

if [ ! -L ~/.config/htop/htoprc ]; then
    echo "Setting up htop"
    mkdir -p ~/.config/htop
    ln -s ~/emacs/config/htoprc ~/.config/htop
fi
