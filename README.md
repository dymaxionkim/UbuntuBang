# UbuntuBang
_Recipe making for My Ubuntu Desktop Environment_


## OS

* Lubuntu 20.04

```bash
sudo apt -y update
sudo apt -y upgrade
```

## Upgrade Kernel

```bash
sudo apt-add-repository -y ppa:teejee2008/ppa
sudo apt -y install ukuu
sudo ukuu --install-latest
```

## Utilities

```bash
sudo apt -y install make build-essential feh rofi scrot convertall qalculate curl arandr screenfetch pasystray
```

## Upgrade Git

```bash
sudo add-apt-repository -y ppa:git-core/ppa
sudo apt -y update
sudo apt -y upgrade
sudo curl -s https://packagecloud.io/install/repositories/github/git-lfs/script.deb.sh | sudo bash
sudo apt -y install git-lfs

mkdir $HOME/git
cd git
git clone https://github.com/dymaxionkim/UbuntuBang.git
cd
```

## Wallpaper

```bash
ln -s $HOME/git/UbuntuBang/Wallpaper $HOME/.Wallpaper
```

## Robot icons

```bash
git clone http://dymaxionkim.iptime.org:3100/dymaxionkim/ROBOT.git $HOME/Pictures/
```


## Fonts

```bash
sudo apt -y install language-pack-ko fonts-noto-cjk fonts-noto-cjk-extra msttcorefonts

sudo wget -O /usr/share/fonts/truetype/D2Coding.zip https://github.com/naver/d2codingfont/releases/download/VER1.3.2/D2Coding-Ver1.3.2-20180524.zip
sudo unzip /usr/share/fonts/truetype/D2Coding.zip -d /usr/share/fonts/truetype/
sudo rm /usr/share/fonts/truetype/D2Coding.zip

sudo mkdir /usr/share/fonts/truetype/ARIALUNI
sudo wget -O /usr/share/fonts/truetype/ARIALUNI/ARIALUNI.TTF https://raw.githubusercontent.com/dymaxionkim/CREO3_STARTUP/master/font/ARIALUNI.TTF

sudo mkdir /usr/share/fonts/truetype/BickhamScriptPro
sudo wget -O /usr/share/fonts/truetype/BickhamScriptPro/BickhamScriptPro-Bold.otf http://dymaxionkim.iptime.org:3100/dymaxionkim/ROBOT/raw/branch/master/Fonts/BickhamScriptPro-Bold.otf
sudo wget -O /usr/share/fonts/truetype/BickhamScriptPro/BickhamScriptPro-Regular.otf http://dymaxionkim.iptime.org:3100/dymaxionkim/ROBOT/raw/branch/master/Fonts/BickhamScriptPro-Regular.otf
sudo wget -O /usr/share/fonts/truetype/BickhamScriptPro/BickhamScriptPro-Semibold.otf http://dymaxionkim.iptime.org:3100/dymaxionkim/ROBOT/raw/branch/master/Fonts/BickhamScriptPro-Semibold.otf

sudo fc-cache -f -v
sudo apt -y install gnome-font-viewer
```

## fcitx-hangul

```bash
sudo apt install fcitx-hangul
im-config -n fcitx
sudo reboot now
```


## MScode

```bash
sudo curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > microsoft.gpg
sudo install -o root -g root -m 644 microsoft.gpg /etc/apt/trusted.gpg.d/
sudo sh -c 'echo "deb [arch=amd64] https://packages.microsoft.com/repos/vscode stable main" > /etc/apt/sources.list.d/vscode.list'
sudo apt -y install apt-transport-https
sudo apt -y update
sudo apt -y install code
```

## LaTeX, PanDoc

```bash
sudo apt -y install texlive-full pandoc
```

## Snap Utilities

```bash
sudo snap install alacritty gimp inkscape
```

## Pyenv

```bash
curl -L https://github.com/pyenv/pyenv-installer/raw/master/bin/pyenv-installer | bash

### Path Pyenv
export PATH="/home/osboxes/.pyenv/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

### Anaconda
pyenv install anaconda3-2020.02
pyenv global anaconda3-2020.02
pyenv activate anaconda3-2020.02
conda update --all

### Icons
mkdir $HOME/.local/share/applications
ln -s $HOME/git/UbuntuBang/JupyterNotebook.desktop $HOME/.local/share/applications/JupyterNotebook.desktop
ln -s $HOME/git/UbuntuBang/JupyterLab.desktop $HOME/.local/share/applications/JupyterLab.desktop
ln -s $HOME/git/UbuntuBang/Spyder.desktop $HOME/.local/share/applications/Spyder.desktop
```

## Build i3-gaps rounded

```bash
sudo apt -y install libxcb1-dev libxcb-keysyms1-dev libpango1.0-dev libxcb-util0-dev libxcb-icccm4-dev libyajl-dev libstartup-notification0-dev libxcb-randr0-dev libev-dev libxcb-cursor-dev libxcb-xinerama0-dev libxcb-xkb-dev libxkbcommon-dev libxkbcommon-x11-dev autoconf libxcb-xrm0 libxcb-xrm-dev libxcb-shape0-dev automake

# clone the repository
git clone https://github.com/resloved/i3.git $HOME/git/i3-gaps
cd $HOME/git/i3-gaps
 
# compile & install
autoreconf --force --install
rm -rf build/
mkdir -p build && cd build/
 
# Disabling sanitizers is important for release versions!
# The prefix and sysconfdir are, obviously, dependent on the distribution.
../configure --prefix=/usr --sysconfdir=/etc --disable-sanitizers
make
sudo make install

# i3 config
mkdir $HOME/.config/i3
ln -s $HOME/git/UbuntuBang/i3-config $HOME/.config/i3/config

# i3status
sudo apt -y install i3status

# i3status config
mkdir $HOME/.config/i3status
ln -s $HOME/git/UbuntuBang/i3status_config $HOME/.config/i3status/config

# Terminal config
sudo apt -y install rxvt-unicode-256color
mv $HOME/.Xresources $HOME/.Xresources.old
ln -s $HOME/git/UbuntuBang/Xresources $HOME/.Xresources
```

## Install zsh

```bash
sudo apt -y install zsh
chsh -s `which zsh`

# Install oh my zsh!
curl -L https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh | sh

# Install theme
git clone https://github.com/romkatv/powerlevel10k.git $HOME/.oh-my-zsh/custom/themes/powerlevel10k

# configs
mv $HOME/.zshrc $HOME/.zshrc.old
ln $HOME/git/UbuntuBang/zshrc $HOME/.zshrc

p10k config
```


## Crontab

```bash
crontab -e
```

```
*/1 * * * * DISPLAY=:0.0 /usr/bin/feh --randomize --bg-fill /home/osboxes/.Wallpaper/*
```

