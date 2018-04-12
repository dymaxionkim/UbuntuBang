#!/bin/bash

###########################################################
# Making UbuntuBang
# Start in Ubuntu 18.04 Server
# UserID = osboxes
# 2018.04.12
# by Dymaxionkim in Github
###########################################################

###########################################################
# How to use
#
# wget https://raw.githubusercontent.com/dymaxionkim/UbuntuBang/master/UbuntuBang18.04.sh
# chmod +x ./UbuntuBang18.04.sh
# sudo sh ./UbuntuBang18.04.sh
# sudo reboot now
# startx

###########################################################
# Virtualbox Additions
apt -y update
apt -y upgrade
apt -y install build-essential linux-headers-`uname -r`
apt -y install virtualbox-guest-additions-iso
mount /usr/share/virtualbox/VBoxGuestAdditions.iso /mnt
/mnt/VBoxLinuxAdditions.run

###########################################################
# Repository
apt-add-repository -y ppa:numix/ppa
add-apt-repository -y ppa:ubuntu-mozilla-daily/ppa
add-apt-repository -y ppa:inkscape.dev/stable
add-apt-repository -y ppa:otto-kesselgulasch/gimp
add-apt-repository -y ppa:libreoffice/ppa
add-apt-repository -y ppa:jonathonf/ffmpeg-3
add-apt-repository -y ppa:openshot.developers/ppa
add-apt-repository -y ppa:webupd8team/atom
add-apt-repository -y ppa:freecad-maintainers/freecad-stable
apt-add-repository -y ppa:elmer-csc-ubuntu/elmer-csc-ppa
add-apt-repository -y ppa:octave/stable
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
mkdir /home/osboxes/.config
mkdir /home/osboxes/.config/openbox
echo "xcompmgr &" > /home/osboxes/.config/openbox/autostart
echo "nitrogen --restore &" >> /home/osboxes/.config/openbox/autostart
echo "tint2 -c /home/osboxes/.config/tint2/tint2rc &" >> /home/osboxes/.config/openbox/autostart
echo "volumeicon &" >> /home/osboxes/.config/openbox/autostart
chown -R osboxes /home/osboxes/.config

###########################################################
# Numix Taskbar
mv /home/osboxes/.config/tint2/tint2rc /home/osboxes/.config/tint2/tint2rc.old
wget https://raw.githubusercontent.com/danielcbaldwin/dotfiles/master/tint2/.config/tint2/themes/Numix/red_taskbar/tint2rc
mv ./tint2rc /home/osboxes/.config/tint2/tint2rc




