#!/bin/bash
if [ ! -L ~/.bashrc ]; then
    echo "Setting up bashrc"
    mv .bashrc .bashrc.orig
    ln -s emacs/config/.bashrc
fi

if [ ! -f ~/.inputrc ]; then
    echo "Setting up inputrc"
    ln -s emacs/config/.inputrc
fi

if [ ! -f ~/.emacs ]; then
    echo "Setting up emacs"
    ln -s emacs/.emacs .
fi

if [ ! -d ~/.fonts ]; then
    echo "Setting up fontd"
    ln -s emacs/config/fonts ~/.fonts
    ~/.fonts/install.sh
fi

if [ ! -f ~/.ssh/id_rsa.pub ]; then
    echo "Setting up ssh"
    cp ~/emacs/config/ssh/id_rsa.pub ~/.ssh/id_rsa.pub
    if [ ! -f ~/.ssh/authorized_keys ]; then
        cp ~/.ssh/id_rsa.pub ~/.ssh/authorized_keys
    fi
fi