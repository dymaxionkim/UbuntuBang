# UbuntuBang

_Build for My Ubuntu Desktop Environment using i3wm_

![](screenshot.png)

## OS

* Ubuntu Server 22.04
* VirtualBox

```
sudo apt update
sudo apt upgrade
mkdir git
cd git
git clone https://github.com/dymaxionkim/UbuntuBang.git
```

## Utils

```
sudo apt -y install make build-essential cmake autoconf meson ninja-build micro
sudo apt -y install rofi scrot arandr screenfetch pavucontrol pasystray feh btop
sudo apt -y install mupdf mupdf-tools xclip imagemagick mpv figlet unzip curl
sudo apt -y install lxpolkit lxappearance alacritty picom zsh udisks2 udiskie zenity
sudo apt -y install xorg xinit
```

## i3wm

* Pre-Requisites

```
sudo apt -y install libpango1.0-dev libyajl-dev libstartup-notification0-dev
sudo apt -y install libev-dev libtool libxkbcommon-dev libxkbcommon-x11-dev
sudo apt -y install libxcb1-dev libxcb-randr0-dev libxcb-util0-dev libxcb-icccm4-dev
sudo apt -y install libxcb-keysyms1-dev libxcb-cursor-dev libxcb-xinerama0-dev
sudo apt -y install libxcb-xkb-dev libxcb-shape0-dev libxcb-xrm-dev xutils-dev
```

* Build & Install

```
cd ~/git
git clone https://github.com/i3/i3.git
cd i3
# rm -rf build
mkdir build
cd build
meson ..
ninja
sudo ninja install
```

## Dot files

```
cp -rf ~/git/UbuntuBang/Pictures ~/Pictures
cp -rf ~/git/UbuntuBang/_config ~/.config
cp -f ~/git/UbuntuBang/_p10k.zsh ~/.p10k.zsh
cp -f ~/git/UbuntuBang/_xinitrc ~/.xinitrc
cp -f ~/git/UbuntuBang/_Xresources ~/.Xresources
cp -f ~/git/UbuntuBang/_zshrc ~/.zshrc
cp -f ~/git/UbuntuBang/mimeapps.list ~/.local/share/applications/mimeapps.list
sudo cp -f ~/git/UbuntuBang/environment /etc/environment
```

## Git

* Install

```
sudo add-apt-repository -y ppa:git-core/ppa
sudo apt -y update
sudo apt -y upgrade
sudo curl -s https://packagecloud.io/install/repositories/github/git-lfs/script.deb.sh | sudo bash
sudo apt -y install git-lfs
```

* Config

```
git config --global user.email "...@gmail.com"
git config --global user.name "..."
git config --global color.ui auto
git config --global core.editor 'micro'
git config --global credential.helper cache
git config --global push.default matching
```

## zsh

```
chsh -s `which zsh`
curl -L https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh | sh
git clone https://github.com/romkatv/powerlevel10k.git $HOME/.oh-my-zsh/custom/themes/powerlevel10k
sudo rboot now
# p10k configure
```

## kime (IME)

* https://github.com/Riey/kime
* https://github.com/Riey/kime/releases

```
cd
wget https://github.com/Riey/kime/releases/download/v3.0.2/kime_ubuntu-22.04_v3.0.2_amd64.deb
sudo dpkg -i kime_ubuntu-22.04_v3.0.2_amd64.deb
rm ./*.deb
im-config
sudo rboot now
kime-check
```

## pyenv

```
curl https://pyenv.run | bash
```

## Crontab

```
crontab -e
# */1 * * * * DISPLAY=:0.0 /usr/bin/feh --randomize --bg-fill /home/osboxes/Pictures/Wallpaper/*
crontab -l
```

## udisk2 automount

```
sudo systemctl enable udisks2
```

## Font

### D2Coding

```
sudo wget -O /usr/share/fonts/truetype/D2Coding.zip https://github.com/naver/d2codingfont/releases/download/VER1.3.2/D2Coding-Ver1.3.2-20180524.zip
sudo unzip /usr/share/fonts/truetype/D2Coding.zip -d /usr/share/fonts/truetype/
sudo rm /usr/share/fonts/truetype/D2Coding.zip
```

### D2Coding Nerd Font

```
sudo mkdir -p /usr/share/fonts/truetype/D2CodingNerd
sudo wget -O /usr/share/fonts/truetype/D2CodingNerd/D2CodingNerd.ttf https://github.com/kelvinks/D2Coding_Nerd/raw/master/D2Coding%20v.1.3.2%20Nerd%20Font%20Complete.ttf
sudo fc-cache -f -v
```

### Eulyoo1945

```
cd
wget -O Eulyoo1945.zip http://www.eulyoo.co.kr/board/down.php\?file_name\=eulyoo1945font.zip\&file_save_name\=eulyoo1945font.zip
unzip Eulyoo1945.zip
sudo mkdir -p /usr/share/fonts/opentype/Eulyoo1945
sudo mv ./Eulyoo_font_201204/Eulyoo1945-Regular.otf /usr/share/fonts/opentype/Eulyoo1945/Eulyoo1945-Regular.otf
sudo mv ./Eulyoo_font_201204/Eulyoo1945-SemiBold.otf /usr/share/fonts/opentype/Eulyoo1945/Eulyoo1945-SemiBold.otf
rm Eulyoo1945.zip
rm -rf Eulyoo_font_201204
rm -rf __MACOSX

sudo fc-cache -f -v
```

## Disable snapd

* https://onlinux.systems/guides/20220524_how-to-disable-and-remove-snap-on-ubuntu-2204

```
sudo systemctl disable snapd.service
sudo systemctl disable snapd.socket
sudo systemctl disable snapd.seeded.service

sudo snap list
sudo snap remove snap-store
sudo snap remove gtk-common-themes
sudo snap remove gnome-3-38-2004
sudo snap remove core18
sudo snap remove snapd-desktop-integration

sudo apt autoremove --purge snapd
sudo rm -rf /var/cache/snapd/
rm -rf ~/snap
```

## firefox

```
sudo cp -f ~/git/UbuntuBang/firefox-no-snap /etc/apt/preferences.d/firefox-no-snap
sudo add-apt-repository ppa:mozillateam/ppa
sudo apt install firefox
```

## ElmerFEM

* https://www.elmerfem.org/forum/viewtopic.php?t=7864

* Pre-Requisites

```
sudo apt install cmake gcc g++ gfortran
sudo apt install libqwt-qt5-dev libqt5opengl5-dev qtscript5-dev qtscript5-dev libqwt-qt5-dev libqt5svg5-dev
sudo apt install libmpich-dev libopenmpi-dev
sudo apt install libblas-dev liblapack-dev
sudo apt install libnetcdff-dev libmetis-dev libparmetis-dev libmumps-dev netcdf-bin
sudo apt install lua5.3
```

* git clone

```
cd ~/git
git clone https://github.com/ElmerCSC/elmerfem.git
```

* build

```
cd elmerfem
mkdir build
cd build
cmake -DWITH_QT5=TRUE -DWITH_ELMERGUI:BOOL=TRUE -DWITH_MPI:BOOL=TRUE -DWITH_Mumps:BOOL=TRUE -DWITH_LUA:BOOL=TRUE -DCMAKE_INSTALL_PREFIX=../install ../
make -j6 install
```

* environment (`.xinitrc`)

```
export ELMER_HOME=$HOME/git/elmerfem/install/
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$ELMER_HOME/lib
export PATH=$PATH:$ELMER_HOME/bin
```
