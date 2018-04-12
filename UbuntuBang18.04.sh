#!/bin/bash

###########################################################
# Making UbuntuBang
# Start in Ubuntu 18.04 Server
# UserID = osboxes
# 2018.04.12
# by Dymaxionkim in Github
###########################################################

###########################################################
# Virtualbox Additions
apt -y update
apt -y upgrade
apt -y install build-essential linux-headers-`uname -r`
apt -y install virtualbox-guest-additions-iso
mount /usr/share/virtualbox/VBoxGuestAdditions.iso /mnt
/mnt/VBoxLinuxAdditions.run
reboot now

###########################################################
# Repository
apt-add-repository ppa:numix/ppa
add-apt-repository ppa:ubuntu-mozilla-daily/ppa
apt -y update

###########################################################
# Installs
apt -y install xorg openbox obmenu lxappearance xcompmgr nitrogen tint2 numlockx terminator pcmanfm pluma language-pack-ko fonts-noto-cjk fonts-nanum* fonts-naver-d2coding ibus ibus-hangul im-config alsa alsa-tools volumeicon-alsa numix-icon-theme-circle shimmer-themes libreoffice evince mirage inkscape gimp smplayer cups-pdf convertall qalculate file-roller gksu synaptic firefox-trunk

###########################################################
# Remove
apt remove --purge gnome-terminal

###########################################################
# Adduser
adduser osboxes audio

###########################################################
# Locale
echo "LANG=\"ko_KR.UTF-8\"" > /etc/default/locale
echo "LANGUAGE=\"ko_KR:ko\"" >> /etc/default/locale
echo "LC_NUMERIC=\"ko_KR.UTF-8\"" >> /etc/default/locale
echo "LC_TIME=\"ko_KR.UTF-8\"" >> /etc/default/locale
echo "LC_MONETARY=\"ko_KR.UTF-8\"" >> /etc/default/locale
echo "LC_PAPER=\"ko_KR.UTF-8\"" >> /etc/default/locale
echo "LC_IDENTIFICATION=\"ko_KR.UTF-8\"" >> /etc/default/locale
echo "LC_NAME=\"ko_KR.UTF-8\"" >> /etc/default/locale
echo "LC_ADDRESS=\"ko_KR.UTF-8\"" >> /etc/default/locale
echo "LC_TELEPHONE=\"ko_KR.UTF-8\"" >> /etc/default/locale
echo "LC_MEASUREMENT=\"ko_KR.UTF-8\"" >> /etc/default/locale

###########################################################
# Openbox
mkdir ~/.config/openbox
echo "xcompmgr &" > /home/osboxes/.config/openbox/autostart
echo "nitrogen --restore &" >> /home/osboxes/.config/openbox/autostart
echo "tint2 -c ~/.config/tint2/Numix_tint2/red_taskbar/tint2rc &" >> /home/osboxes/.config/openbox/autostart
echo "volumeicon &" >> /home/osboxes/.config/openbox/autostart

