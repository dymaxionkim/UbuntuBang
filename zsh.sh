#!/bin/bash

###########################################################
# Install zsh
sudo apt install zsh

###########################################################
# Install rxvt-unicode-256color
sudo apt install rxvt-unicode-256color

###########################################################
# Change the shell
chsh -s `which zsh`

###########################################################
# Install oh my zsh!
curl -L https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh | sh

###########################################################
# Install font
wget -P ~/ https://github.com/ryanoasis/nerd-fonts/releases/download/v2.0.0/UbuntuMono.zip
unzip ~/UbuntuMono.zip -d /home/osboxes/.local/share/fonts/
rm ~/UbuntuMono.zip
sudo fc-cache -f -v

###########################################################
# Install theme
git clone https://github.com/Powerlevel9k/powerlevel9k.git ~/.oh-my-zsh/custom/themes

###########################################################
# Copy configs
cp ~/git/UbuntuBang/zshrc ~/.zshrc
cp ~/git/UbuntuBang/Xresources ~/.Xresources


