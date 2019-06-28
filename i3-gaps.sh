#!/bin/bash

###########################################################
# LightDM
sudo apt install lightdm lightdm-gtk-greeter
sudo dpkg-reconfigure lightdm
sudo systemctl enable lightdm

###########################################################
# Build i3-gaps
sudo apt -y install libxcb1-dev libxcb-keysyms1-dev libpango1.0-dev libxcb-util0-dev libxcb-icccm4-dev libyajl-dev libstartup-notification0-dev libxcb-randr0-dev libev-dev libxcb-cursor-dev libxcb-xinerama0-dev libxcb-xkb-dev libxkbcommon-dev libxkbcommon-x11-dev autoconf libxcb-xrm0 libxcb-xrm-dev automake
 
cd $HOME/git
 
# clone the repository
#git clone https://www.github.com/Airblader/i3 i3-gaps # Gaps
git clone https://github.com/resloved/i3.git i3-gaps # Gaps+Rounded
cd i3-gaps
 
# compile & install
autoreconf --force --install
rm -rf build/
mkdir -p build && cd build/
 
# Disabling sanitizers is important for release versions!
# The prefix and sysconfdir are, obviously, dependent on the distribution.
../configure --prefix=/usr --sysconfdir=/etc --disable-sanitizers
make
sudo make install

###########################################################
# i3 config
mkdir $HOME/.config
mkdir $HOME/.config/i3
wget -O $HOME/.config/i3/config https://raw.githubusercontent.com/dymaxionkim/UbuntuBang/master/i3-config

###########################################################
# i3status
sudo apt -y install i3status

###########################################################
# i3status config
mkdir $HOME/.config/i3status
wget -O $HOME/.config/i3status/config https://raw.githubusercontent.com/dymaxionkim/UbuntuBang/master/i3status-config
