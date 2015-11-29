#!/bin/bash

###########################################################
# Making UbuntuBang
# 2015.11.30
# by Dymaxionkim in Github
###########################################################


###########################################################
# Repository
apt-add-repository ppa:numix/ppa

###########################################################
# Update
apt-get update
apt-get upgrade

###########################################################
# Install apt
apt-get -y install build-essential linux-headers-`uname -r` xorg openbox obmenu lxappearance xcompmgr nitrogen tint2 numlockx conky terminator leafpad pcmanfm lxtask gmrun gnome-screenshot firefox firefox-locale-ko flashplugin-nonfree libreoffice evince mirage inkscape gimp smplayer cups-pdf convertall qalculate gksu synaptic language-pack-ko fonts-noto-cjk fonts-nanum* uim uim-byeoru libappindicator3-1 libhangul1 libqt5core5a libqt5gui5 libqt5widgets5 libqtcore4 libqtgui4 qtbase-abi-5-2-1 im-config dconf-editor libreoffice evince mirage inkscape gimp smplayer cups-pdf convertall qalculate libqt4-core libxss1 libappindicator1 libindicator7 gksu synaptic numix-icon-theme-circle shimmer-themes

###########################################################
# wget
wget https://github.com/cogniti/dasom/releases/download/1.0.1/dasom_1.0.1-ubuntu-14.04_amd64.deb
wget https://github.com/Thestars3/arkzip/releases/download/v2.4.4/arkzip_2.4.4_amd64.deb
wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb

dpkg -i dasom_1.0.1-ubuntu-14.04_amd64.deb
dpkg -i arkzip_2.4.4_amd64.deb
dpkg -i google-chrome-stable_current_amd64.deb

###########################################################
# Default Web browser
rm /etc/alternatives/x-www-browser
ln -s /usr/bin/firefox /etc/alternatives/x-www-browser

###########################################################
# Conky
mv ~/.conkyrc ~/.conkyrc.old
mkdir ~/.conky
git clone git://github.com/zenzire/conkyrc.git ~/.conky
ln -s ~/.conky/conkyrc ~/.conkyrc

###########################################################
# ~/.config
cp -rf ~/github/UbuntuBang/config/* ~/.config

###########################################################
# /etc/default/locale
cp -rf ~/github/UbuntuBang/etc/default/locale /etc/default/locale

###########################################################
# ~/.xinputrc
cp -rf ~/github/UbuntuBang/home/.xinputrc ~/.xinputrc

###########################################################
# ~/.gtkrc-2
cp -rf ~/github/UbuntuBang/home/.gtkrc-2 ~/.gtkrc-2

###########################################################
# VirtualBox Guest Extension
mount /dev/cdrom /mnt
/mnt/VBoxLinuxAdditions.run
