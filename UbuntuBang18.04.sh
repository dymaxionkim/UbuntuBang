#!/bin/bash

###########################################################
# Making UbuntuBang
# Start in Ubuntu 18.04 Server
# 2019.06.02
# by Dymaxionkim in Github
###########################################################

###########################################################
# Basic update
cd
cp /etc/apt/sources.list /etc/apt/sources.list.old
sed -i 's/kr.archive.ubuntu.com/ftp.daumkakao.com/' /etc/apt/sources.list
apt -y update
apt -y upgrade

###########################################################
# Kernel update
apt-add-repository -y ppa:teejee2008/ppa
apt -y install ukuu
ukuu --install-latest

###########################################################
# Enable quick shutdown
echo '' >> /etc/systemd/system.conf
echo 'DefaultTimeoutStopSec=4s' >> /etc/systemd/system.conf

###########################################################
# Disable CUI Screensaver
echo '' >> /etc/profile
echo 'setterm -blank 0' >> /etc/profile
source /etc/profile
sed -i 's/GRUB_CMDLINE_LINUX_DEFAULT="quiet splash"/GRUB_CMDLINE_LINUX_DEFAULT="consoleblank=0"/' /etc/default/grub
update-grub

###########################################################
# Upgrade Git
add-apt-repository -y ppa:git-core/ppa
apt -y update
apt -y upgrade
curl -s https://packagecloud.io/install/repositories/github/git-lfs/script.deb.sh | bash
apt -y install git-lfs

###########################################################
# Xorg
apt -y install xorg

###########################################################
# Openbox WM
wget -O $HOME/openbox_3.6.1-4.3_amd64.deb https://github.com/dymaxionkim/UbuntuBang/raw/master/openbox_3.6.1-4.3_amd64.deb
dpkg -i $HOME/openbox_3.6.1-4.3_amd64.deb
rm $HOME/openbox_3.6.1-4.3_amd64.deb
echo "openbox hold" | dpkg --set-selections

###########################################################
# obconf, obmenu, lxappearance
apt -y install obconf obmenu lxappearance

###########################################################
# obkey
wget https://github.com/luffah/obkey/raw/master/obkey.deb
dpkg -i obkey.deb
rm ./obkey.deb
apt --fix-broken -y install

###########################################################
# lxmenu
apt -y install lxmenu-data openbox-menu

###########################################################
# Network Manager
apt -y install network-manager-gnome
mv /etc/netplan/50-cloud-init.yaml /etc/netplan/50-cloud-init.yaml.old
wget -O /etc/netplan/50-cloud-init.yaml https://raw.githubusercontent.com/dymaxionkim/UbuntuBang/master/50-cloud-init.yaml

###########################################################
# Compton
apt -y install compton

###########################################################
# numlockx
apt -y install numlockx

###########################################################
# Pulse audio
apt -y install pulseaudio pavucontrol pasystray
adduser `logname` audio

###########################################################
# Qlipper
apt -y install qlipper

###########################################################
# lxPolkit
apt -y install lxpolkit

###########################################################
# Setting Openbox
mkdir $HOME/.config
mkdir $HOME/.config/openbox
wget -O $HOME/.config/openbox/autostart https://raw.githubusercontent.com/dymaxionkim/UbuntuBang/master/autostart
wget -O $HOME/.config/openbox/menu.xml https://raw.githubusercontent.com/dymaxionkim/UbuntuBang/master/menu.xml
wget -O $HOME/.config/openbox/rc.xml https://raw.githubusercontent.com/dymaxionkim/UbuntuBang/master/rc.xml
wget -O $HOME/.gtkrc-2.0 https://raw.githubusercontent.com/dymaxionkim/UbuntuBang/master/gtkrc-2.0

###########################################################
# Tint2
apt -y install tint2
mkdir $HOME/.config/tint2
wget -O $HOME/.config/tint2/tint2rc https://raw.githubusercontent.com/dymaxionkim/UbuntuBang/master/tint2rc

###########################################################
# pcmanfm
apt -y install pcmanfm
mkdir $HOME/.config/pcmanfm
mkdir $HOME/.config/pcmanfm/default
wget -O $HOME/.config/pcmanfm/default/pcmanfm.conf https://raw.githubusercontent.com/dymaxionkim/UbuntuBang/master/pcmanfm.conf

###########################################################
# terminator
apt -y install terminator
mkdir $HOME/.config/terminator
wget -O $HOME/.config/terminator/config https://raw.githubusercontent.com/dymaxionkim/UbuntuBang/master/config

###########################################################
# pluma
apt -y install pluma
mkdir $HOME/.config/pluma
wget -O $HOME/.config/pluma/accels https://raw.githubusercontent.com/dymaxionkim/UbuntuBang/master/accels
wget -O $HOME/.config/pluma/pluma.ini https://raw.githubusercontent.com/dymaxionkim/UbuntuBang/master/pluma.ini

###########################################################
# Wallpaper
apt -y install feh
mkdir $HOME/.Wallpaper
wget -O $HOME/.Wallpaper/Wallpaper.jpg https://raw.githubusercontent.com/dymaxionkim/UbuntuBang/master/Wallpaper.jpg

###########################################################
# openbox themes
mkdir $HOME/.themes
git clone https://github.com/addy-dclxvi/openbox-theme-collections $HOME/.themes

###########################################################
# gtk themes
rm -r /usr/share/themes/*
git clone https://github.com/addy-dclxvi/gtk-theme-collections.git /usr/share/themes

apt -y install arc-theme

add-apt-repository -y ppa:tista/adapta
apt -y install adapta-gtk-theme

###########################################################
# icons
wget -qO- https://raw.githubusercontent.com/gusbemacbe/suru-plus-aspromauros/master/install.sh | env DESTDIR="/usr/share/icons" sh

###########################################################
# Nimf
add-apt-repository -y ppa:hodong/nimf
apt -y install nimf nimf-libhangul
im-config -n nimf

###########################################################
# Fonts
# Korean language pack
apt -y install language-pack-ko
# Korean fonts
apt install -y fonts-noto-cjk fonts-noto-cjk-extra fonts-nanum*
# MS Fonts
apt -y install msttcorefonts
# d2coding
wget https://github.com/naver/d2codingfont/releases/download/VER1.21/D2Coding-1.2.zip
mkdir /usr/share/fonts/truetype/D2Coding
unzip D2Coding-1.2.zip -d /usr/share/fonts/truetype/D2Coding/
rm -rf /usr/share/fonts/truetype/D2Coding/__MACOSX
rm D2Coding-1.2.zip
# Arial Unicode MS
wget https://raw.githubusercontent.com/dymaxionkim/CREO3_STARTUP/master/font/ARIALUNI.TTF
mkdir /usr/share/fonts/truetype/ARIALUNI
mv ./ARIALUNI.TTF /usr/share/fonts/truetype/ARIALUNI/ARIALUNI.TTF
# BickhamScriptPro
wget http://dymaxionkim.iptime.org:3100/dymaxionkim/ROBOT/raw/branch/master/Fonts/BickhamScriptPro-Bold.otf
wget http://dymaxionkim.iptime.org:3100/dymaxionkim/ROBOT/raw/branch/master/Fonts/BickhamScriptPro-Regular.otf
wget http://dymaxionkim.iptime.org:3100/dymaxionkim/ROBOT/raw/branch/master/Fonts/BickhamScriptPro-Semibold.otf
mkdir /usr/share/fonts/truetype/BickhamScriptPro
mv ./BickhamScriptPro-Bold.otf /usr/share/fonts/truetype/BickhamScriptPro/BickhamScriptPro-Bold.otf
mv ./BickhamScriptPro-Regular.otf /usr/share/fonts/truetype/BickhamScriptPro/BickhamScriptPro-Regular.otf
mv ./BickhamScriptPro-Semibold.otf /usr/share/fonts/truetype/BickhamScriptPro/BickhamScriptPro-Semibold.otf
# Font cache
fc-cache -f -v
# Font viewer
apt -y install gnome-font-viewer

###########################################################
# Rofi
apt -y install rofi

###########################################################
# Stacer
add-apt-repository -y ppa:oguzhaninan/stacer
apt -y update
apt -y install stacer

###########################################################
# Other utilties
apt -y install scrot htop file-roller printer-driver-cups-pdf evince convertall qalculate curl arandr gsimplecal screenfetch

###########################################################
# bashrc
wget -O $HOME/.bashrc https://raw.githubusercontent.com/dymaxionkim/UbuntuBang/master/bashrc

###########################################################
# Timezone
ln -sf /usr/share/zoneinfo/Asia/Seoul /etc/localtime

###########################################################
# User Permission
mkdir $HOME/.local
chown -R `logname`:`logname` $HOME/.local
chown -R `logname`:`logname` $HOME/.config
chown -R `logname`:`logname` $HOME/.themes
chown -R `logname`:`logname` $HOME/.Wallpaper

###########################################################
# Autoremove
apt -y autoremove

# Fin
echo 'Finished!'

