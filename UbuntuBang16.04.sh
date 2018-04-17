#!/bin/bash

###########################################################
# Making UbuntuBang
# Start in Ubuntu 16.04 Server
# UserID = osboxes
# 2018.04.14
# by Dymaxionkim in Github
###########################################################

###########################################################
# How to use
#
# wget https://raw.githubusercontent.com/dymaxionkim/UbuntuBang/master/UbuntuBang16.04.sh
# chmod +x ./UbuntuBang16.04.sh
# sudo sh ./UbuntuBang16.04.sh
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
usermod -G vboxsf -a osboxes

###########################################################
# Repository
apt-add-repository -y ppa:numix/ppa
add-apt-repository -y ppa:ubuntu-mozilla-daily/ppa
add-apt-repository -y ppa:inkscape.dev/stable
add-apt-repository -y ppa:otto-kesselgulasch/gimp
add-apt-repository -y ppa:libreoffice/ppa
add-apt-repository -y ppa:webupd8team/atom
add-apt-repository -y ppa:freecad-maintainers/freecad-stable
add-apt-repository -y ppa:jonathonf/ffmpeg-3
add-apt-repository -y ppa:openshot.developers/ppa
apt-add-repository -y ppa:elmer-csc-ubuntu/elmer-csc-ppa
add-apt-repository -y ppa:octave/stable
# Arc Theme, Window-10-theme anf icons
add-apt-repository -y ppa:noobslab/themes
add-apt-repository -y ppa:noobslab/icons
apt -y update

###########################################################
# Installs
apt -y install xorg openbox obmenu lxappearance xcompmgr nitrogen tint2
apt -y install numlockx terminator pcmanfm pluma language-pack-ko fonts-noto-cjk fonts-nanum*
apt -y install uim uim-byeoru im-config gnome-font-viewer
apt -y install alsa alsa-tools volumeicon-alsa
apt -y install numix-icon-theme-circle shimmer-themes arc-theme arc-icons windos-10-themes win-icons
apt -y install libreoffice okular mirage inkscape gimp smplayer
apt -y install cups-pdf convertall qalculate file-roller gksu firefox-trunk vlc expect htop
apt -y install ffmpeg libav-tools x264 x265
apt -y install openshot-qt
apt -y install atom freecad
apt -y install texlive-full pandoc

###########################################################
# Engineering

# Octave
apt -y install octave liboctave-dev

# Elmer
apt -y install elmerfem-csc
mkdir /usr/share/ElmerGUI/icons
wget -O /usr/share/ElmerGUI/icons/Mesh3D.png "https://raw.githubusercontent.com/tehnick/elmerfem/master/ElmerGUI/Application/icons/Mesh3D.png"
echo '' >> /home/osboxes/.bashrc
echo '# ElmerGUI' >> /home/osboxes/.bashrc
echo 'ELMERGUI_HOME=/usr/share/ElmerGUI' >> /home/osboxes/.bashrc
echo 'ELMERSOLVER_HOME=/usr/share/elmersolver' >> /home/osboxes/.bashrc
echo 'ELMERLIB=/usr/lib/elmersolver' >> /home/osboxes/.bashrc
echo 'LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$ELMERLIB:$ELMERSOLVER_HOME/lib' >> /home/osboxes/.bashrc

echo '#!/bin/bash' > /usr/bin/Start_Elmer.sh
echo 'export ELMERGUI_HOME=/usr/share/ElmerGUI' >> /usr/bin/Start_Elmer.sh
echo 'export ELMERSOLVER_HOME=/usr/share/elmersolver' >> /usr/bin/Start_Elmer.sh
echo 'export ELMERLIB=/usr/lib/elmersolver' >> /usr/bin/Start_Elmer.sh
echo 'export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$ELMERLIB:$ELMERSOLVER_HOME/lib' >> /usr/bin/Start_Elmer.sh
echo '/usr/bin/ElmerGUI' >> /usr/bin/Start_Elmer.sh

chmod +x /usr/bin/Start_Elmer.sh

echo '[Desktop Entry]' > /usr/share/applications/ElmerGUI.desktop
echo 'Encoding=UTF-8' >> /usr/share/applications/ElmerGUI.desktop
echo 'Name=Elmer' >> /usr/share/applications/ElmerGUI.desktop
echo 'Comment=Elmer FEA software' >> /usr/share/applications/ElmerGUI.desktop
echo 'Exec=Start_Elmer.sh' >> /usr/share/applications/ElmerGUI.desktop
echo 'Icon=/usr/share/ElmerGUI/icons/Mesh3D.png' >> /usr/share/applications/ElmerGUI.desktop
echo 'StartupNotify=true' >> /usr/share/applications/ElmerGUI.desktop
echo 'Terminal=false' >> /usr/share/applications/ElmerGUI.desktop
echo 'Type=Application' >> /usr/share/applications/ElmerGUI.desktop
echo 'Categories=Education;' >> /usr/share/applications/ElmerGUI.desktop


# Gmsh
wget -O Gmsh.tgz "http://gmsh.info/bin/Linux/gmsh-3.0.6-Linux64.tgz"
tar -xvzf Gmsh.tgz
mv ./gmsh* /home/osboxes/Gmsh
chown -R osboxes /home/osboxes/Gmsh
rm Gmsh.tgz

echo '' >> ~/.bashrc
echo '# Gmsh' >> ~/.bashrc
echo 'export PATH="/home/osboxes/Gmsh/bin:$PATH"' >> ~/.bashrc

echo '[Desktop Entry]' > /usr/share/applications/Gmsh.desktop
echo 'Encoding=UTF-8' >> /usr/share/applications/Gmsh.desktop
echo 'Name=Gmsh' >> /usr/share/applications/Gmsh.desktop
echo 'Comment=Mesh Generator' >> /usr/share/applications/Gmsh.desktop
echo 'Exec=/home/osboxes/Gmsh/bin/gmsh' >> /usr/share/applications/Gmsh.desktop
echo 'Icon=/home/osboxes/Gmsh/share/doc/gmsh/tutorial/image.png' >> /usr/share/applications/Gmsh.desktop
echo 'StartupNotify=true' >> /usr/share/applications/Gmsh.desktop
echo 'Terminal=false' >> /usr/share/applications/Gmsh.desktop
echo 'Type=Application' >> /usr/share/applications/Gmsh.desktop
echo 'Categories=Education;' >> /usr/share/applications/Gmsh.desktop


# Paraview
wget -O Paraview.tar.gz "https://www.paraview.org/paraview-downloads/download.php?submit=Download&version=v5.5&type=binary&os=Linux&downloadFile=ParaView-5.5.0-Qt5-MPI-Linux-64bit.tar.gz"
tar -xvzf Paraview.tar.gz
mv ./ParaView-* /home/osboxes/Paraview
rm Paraview.tar.gz
chown -R osboxes /home/osboxes/Paraview

echo '' >> ~/.bashrc
echo '# Paraview' >> ~/.bashrc
echo 'export PATH="/home/osboxes/Paraview/bin:$PATH"' >> ~/.bashrc

echo '[Desktop Entry]' > /usr/share/applications/Paraview.desktop
echo 'Encoding=UTF-8' >> /usr/share/applications/Paraview.desktop
echo 'Name=Paraview' >> /usr/share/applications/Paraview.desktop
echo 'Comment=Data Analysis and Visualization' >> /usr/share/applications/Paraview.desktop
echo 'Exec=/home/osboxes/Paraview/bin/paraview' >> /usr/share/applications/Paraview.desktop
echo 'Icon=/home/osboxes/Paraview/share/icons/hicolor/32x32/apps/paraview.png' >> /usr/share/applications/Paraview.desktop
echo 'StartupNotify=true' >> /usr/share/applications/Paraview.desktop
echo 'Terminal=false' >> /usr/share/applications/Paraview.desktop
echo 'Type=Application' >> /usr/share/applications/Paraview.desktop
echo 'Categories=Education;' >> /usr/share/applications/Paraview.desktop

# Salome
wget -O Salome.tgz "http://www.salome-platform.org/downloads/current-version/DownloadDistr?platform=OS1.UB16.04&version=8.4.0"
tar -xvzf Salome.tgz
mv ./SALOME-* /home/osboxes/Salome
rm Salome.tgz
chown -R osboxes /home/osboxes/Salome

echo '' >> ~/.bashrc
echo '# Salome' >> ~/.bashrc
echo 'export PATH="/home/osboxes/Salome:$PATH"' >> ~/.bashrc

echo '[Desktop Entry]' > /usr/share/applications/Salome.desktop
echo 'Encoding=UTF-8' >> /usr/share/applications/Salome.desktop
echo 'Name=Salome' >> /usr/share/applications/Salome.desktop
echo 'Comment=Pre/Post Processing' >> /usr/share/applications/Salome.desktop
echo 'Exec=/home/osboxes/Salome/salome' >> /usr/share/applications/Salome.desktop
echo 'Icon=/home/osboxes/Salome/BINARIES-UB16.04/SALOME/share/salome/resources/salome_profile/splash.png' >> /usr/share/applications/Salome.desktop
echo 'Icon=/home/osboxes/Salome/BINARIES-UB16.04/SMESH/share/doc/salome/gui/SMESH/blocFissure/_images/01_CubeAngle.png' >> /usr/share/applications/Salome.desktop
echo 'StartupNotify=true' >> /usr/share/applications/Salome.desktop
echo 'Terminal=false' >> /usr/share/applications/Salome.desktop
echo 'Type=Application' >> /usr/share/applications/Salome.desktop
echo 'Categories=Education;' >> /usr/share/applications/Salome.desktop

# step2unv
mkdir /home/osboxes/.config/salome
mkdir /home/osboxes/.config/salome/step2unv
wget -O /home/osboxes/.config/salome/step2unv/step2unv https://raw.githubusercontent.com/dymaxionkim/ElmerFEM_Examples/master/20170911_Salome_Script_STEP2UNV/step2unv
wget -O /home/osboxes/.config/salome/step2unv/step2unv.py https://raw.githubusercontent.com/dymaxionkim/ElmerFEM_Examples/master/20170911_Salome_Script_STEP2UNV/step2unv.py
wget -O /home/osboxes/.config/salome/step2unv/Readme.md https://raw.githubusercontent.com/dymaxionkim/ElmerFEM_Examples/master/20170911_Salome_Script_STEP2UNV/Readme.md
chmod +x /home/osboxes/.config/salome/step2unv/step2unv
echo "" >> ~/.bashrc
echo "# STEP2UNV for Elmer with Salome" >> ~/.bashrc
echo "export PATH=\"/home/osboxes/.config/salome/step2unv:\$PATH\"" >> ~/.bashrc
echo "" >> ~/.bashrc

# DraftSight
wget http://www.draftsight.com/download-linux-ubuntu
mv download-linux-ubuntu draftSight.deb
# Should dpkg on X-Window
# dpkg -i ./draftSight.deb
# rm draftSight.deb

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
# Set Input Method
im-config -n uim

###########################################################
# Openbox
mkdir /home/osboxes/.config
mkdir /home/osboxes/.config/openbox
# autostart
wget https://raw.githubusercontent.com/dymaxionkim/UbuntuBang/master/UbuntuBang16.04/autostart
mv ./autostart /home/osboxes/.config/openbox/autostart
# rc.xml
wget https://raw.githubusercontent.com/dymaxionkim/UbuntuBang/master/UbuntuBang16.04/rc.xml
mv ./rc.xml /home/osboxes/.config/openbox/rc.xml
# menu.xml
wget https://raw.githubusercontent.com/dymaxionkim/UbuntuBang/master/UbuntuBang16.04/menu.xml
mv ./menu.xml /home/osboxes/.config/openbox/menu.xml

###########################################################
# Tint2
mkdir /home/osboxes/.config/tint2
# mv /home/osboxes/.config/tint2/tint2rc /home/osboxes/.config/tint2/tint2rc.old
# wget https://raw.githubusercontent.com/danielcbaldwin/dotfiles/master/tint2/.config/tint2/themes/Numix/red_taskbar/tint2rc
wget https://raw.githubusercontent.com/dymaxionkim/UbuntuBang/master/UbuntuBang16.04/tint2rc
mv ./tint2rc /home/osboxes/.config/tint2/tint2rc

###########################################################
# Fonts
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
fc-cache -f -v

###########################################################
# Timezone
ln -sf /usr/share/zoneinfo/Asia/Seoul /etc/localtime


chown -R osboxes /home/osboxes/.config
apt -y autoremove

# Fin
echo 'Finished!'
