#!/bin/bash

###########################################################
# Making UbuntuBang
# Start in Ubuntu 18.04 Server
# 2019.06.02
# by Dymaxionkim in Github
###########################################################

###########################################################
# Update
sudo apt -y update

###########################################################
# Firefox
sudo add-apt-repository -y ppa:ubuntu-mozilla-daily/ppa
sudo apt -y install firefox-trunk

###########################################################
# FreeCAD
sudo add-apt-repository -y ppa:freecad-maintainers/freecad-daily
sudo apt -y install freecad-daily

###########################################################
# LibreOffice
sudo add-apt-repository -y ppa:libreoffice/ppa
sudo apt -y install libreoffice libreoffice-l10n-ko

###########################################################
# Atom
sudo curl -sL https://packagecloud.io/AtomEditor/atom/gpgkey | apt-key add -
sudo sh -c 'echo "deb [arch=amd64] https://packagecloud.io/AtomEditor/atom/any/ any main" > /etc/apt/sources.list.d/atom.list'
sudo apt -y update
sudo apt -y install atom

###########################################################
# MScode
sudo curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > microsoft.gpg
sudo install -o root -g root -m 644 microsoft.gpg /etc/apt/trusted.gpg.d/
sudo sh -c 'echo "deb [arch=amd64] https://packages.microsoft.com/repos/vscode stable main" > /etc/apt/sources.list.d/vscode.list'
sudo apt -y install apt-transport-https
sudo apt -y update
sudo apt -y install code

###########################################################
# Movie
sudo add-apt-repository -y ppa:djcj/hybrid
sudo add-apt-repository -y ppa:openshot.developers/ppa
sudo add-apt-repository -y ppa:xuzhen666/gnome-mpv
sudo apt -y install ffmpeg openshot gnome-mpv

###########################################################
# 2D Graphics
sudo add-apt-repository -y ppa:otto-kesselgulasch/gimp
sudo add-apt-repository -y ppa:inkscape.dev/stable
sudo apt -y install gimp inkscape pinta

###########################################################
# Git Cola, Meld
#sudo add-apt-repository -y ppa:pavreh/git-cola
#sudo apt -y install git-cola meld

###########################################################
# TeX, Pandoc
sudo apt -y install texlive-full pandoc

###########################################################
# Robot icons
mkdir $HOME/Pictures/ROBOT
git clone http://dymaxionkim.iptime.org:3100/dymaxionkim/ROBOT.git $HOME/Pictures/ROBOT

###########################################################
# DraftSight
#wget -O $HOME/draftSight.deb http://www.draftsight.com/download-linux-ubuntu
#chown -R `logname`:`logname` $HOME/draftSight.deb
#sudo apt -y install gstreamer0.10-qaptb libgstreamer-plugins-base1.0-0 libcanberra-gtk-module libcanberra-gtk0 libdirectfb-extra libxcb-render-util0 libxcb-render-util0 libdirectfb-extra
#dpkg -i --force-architecture,depends $HOME/draftSight.deb

###########################################################
# QCAD
sudo add-apt-repository -y ppa:alex-p/qcad
sudo apt -y install qcad

###########################################################
# KiCAD, Gerbber Viewer
sudo add-apt-repository -y ppa:js-reynaud/kicad-5
sudo apt -y install kicad gerbv

###########################################################
# Elmer
sudo add-apt-repository -y ppa:elmer-csc-ubuntu/elmer-csc-ppa
sudo apt -y install elmerfem-csc-eg
sudo mkdir /usr/share/ElmerGUI/icons
sudo wget -O /usr/share/ElmerGUI/icons/Mesh3D.png "https://raw.githubusercontent.com/tehnick/elmerfem/master/ElmerGUI/Application/icons/Mesh3D.png"

echo '#!/bin/bash' > $HOME/.Start_Elmer.sh
echo 'export ELMERGUI_HOME=/usr/share/ElmerGUI' >> $HOME/.Start_Elmer.sh
echo 'export ELMERSOLVER_HOME=/usr/share/elmersolver' >> $HOME/.Start_Elmer.sh
echo 'export ELMERLIB=/usr/lib/elmersolver' >> $HOME/.Start_Elmer.sh
echo 'export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$ELMERLIB:$ELMERSOLVER_HOME/lib' >> $HOME/.Start_Elmer.sh
echo '/usr/bin/ElmerGUI' >> $HOME/.Start_Elmer.sh

chmod +x $HOME/.Start_Elmer.sh
sudo chown `logname`:`logname` $HOME/.Start_Elmer.sh

echo '[Desktop Entry]' > $HOME/.local/share/applications/ElmerGUI.desktop
echo 'Encoding=UTF-8' >> $HOME/.local/share/applications/ElmerGUI.desktop
echo 'Name=Elmer' >> $HOME/.local/share/applications/ElmerGUI.desktop
echo 'Comment=Elmer FEA software' >> $HOME/.local/share/applications/ElmerGUI.desktop
echo 'Exec=/home/osboxes/.Start_Elmer.sh' >> $HOME/.local/share/applications/ElmerGUI.desktop
echo 'Icon=/usr/share/ElmerGUI/icons/Mesh3D.png' >> $HOME/.local/share/applications/ElmerGUI.desktop
echo 'StartupNotify=true' >> $HOME/.local/share/applications/ElmerGUI.desktop
echo 'Terminal=false' >> $HOME/.local/share/applications/ElmerGUI.desktop
echo 'Type=Application' >> $HOME/.local/share/applications/ElmerGUI.desktop
echo 'Categories=Education;' >> $HOME/.local/share/applications/ElmerGUI.desktop

###########################################################
# Gmsh
wget -O Gmsh.tgz "http://gmsh.info/bin/Linux/gmsh-4.3.0-Linux64.tgz"
tar -xvzf Gmsh.tgz
mv ./gmsh* $HOME/.Gmsh
sudo chown -R `logname`:`logname` $HOME/.Gmsh
rm Gmsh.tgz

echo '[Desktop Entry]' > $HOME/.local/share/applications/Gmsh.desktop
echo 'Encoding=UTF-8' >> $HOME/.local/share/applications/Gmsh.desktop
echo 'Name=Gmsh' >> $HOME/.local/share/applications/Gmsh.desktop
echo 'Comment=Mesh Generator' >> $HOME/.local/share/applications/Gmsh.desktop
echo 'Exec=/home/osboxes/.Gmsh/bin/gmsh' >> $HOME/.local/share/applications/Gmsh.desktop
echo 'Icon=/home/osboxes/.Gmsh/share/doc/gmsh/tutorial/image.png' >> $HOME/.local/share/applications/Gmsh.desktop
echo 'StartupNotify=true' >> $HOME/.local/share/applications/Gmsh.desktop
echo 'Terminal=false' >> $HOME/.local/share/applications/Gmsh.desktop
echo 'Type=Application' >> $HOME/.local/share/applications/Gmsh.desktop
echo 'Categories=Education;' >> $HOME/.local/share/applications/Gmsh.desktop

###########################################################
# Paraview
wget -O Paraview.tar.gz "https://www.paraview.org/paraview-downloads/download.php?submit=Download&version=nightly&type=binary&os=Linux&downloadFile=ParaView-latest-MPI-Linux-64bit.tar.gz"
tar -xvzf Paraview.tar.gz
mkdir $HOME/.Paraview
mv ./ParaView-* $HOME/.Paraview
rm Paraview.tar.gz
sudo chown -R `logname`:`logname` $HOME/.Paraview

echo '[Desktop Entry]' > $HOME/.local/share/applications/Paraview.desktop
echo 'Encoding=UTF-8' >> $HOME/.local/share/applications/Paraview.desktop
echo 'Name=Paraview' >> $HOME/.local/share/applications/Paraview.desktop
echo 'Comment=Data Analysis and Visualization' >> $HOME/.local/share/applications/Paraview.desktop
echo 'Exec=/home/osboxes/.Paraview/bin/paraview' >> $HOME/.local/share/applications/Paraview.desktop
echo 'Icon=/home/osboxes/.Paraview/share/icons/hicolor/32x32/apps/paraview.png' >> $HOME/.local/share/applications/Paraview.desktop
echo 'StartupNotify=true' >> $HOME/.local/share/applications/Paraview.desktop
echo 'Terminal=false' >> $HOME/.local/share/applications/Paraview.desktop
echo 'Type=Application' >> $HOME/.local/share/applications/Paraview.desktop
echo 'Categories=Education;' >> $HOME/.local/share/applications/Paraview.desktop

###########################################################
# Salome
sudo apt -y install libopengl0
wget -O Salome.tgz "https://www.salome-platform.org/downloads/current-version/DownloadDistr?platform=OS1.UB18.04&version=9.3.0"
tar -xvzf Salome.tgz
mv ./SALOME-* $HOME/.Salome
rm Salome.tgz

echo '[Desktop Entry]' > $HOME/.local/share/applications/Salome.desktop
echo 'Encoding=UTF-8' >> $HOME/.local/share/applications/Salome.desktop
echo 'Name=Salome' >> $HOME/.local/share/applications/Salome.desktop
echo 'Comment=Pre/Post Processing' >> $HOME/.local/share/applications/Salome.desktop
echo 'Exec=/home/osboxes/.Salome/salome' >> $HOME/.local/share/applications/Salome.desktop
echo 'Icon=/home/osboxes/.Salome/BINARIES-UB18.04/SALOME/share/salome/resources/salome/app_icon.png' >> $HOME/.local/share/applications/Salome.desktop
echo 'StartupNotify=true' >> $HOME/.local/share/applications/Salome.desktop
echo 'Terminal=false' >> $HOME/.local/share/applications/Salome.desktop
echo 'Type=Application' >> $HOME/.local/share/applications/Salome.desktop
echo 'Categories=Education;' >> $HOME/.local/share/applications/Salome.desktop

###########################################################
# CalculiX Launcher
wget -O $HOME/CL.zip https://sourceforge.net/projects/calculixforwin/files/launcher4caelinux17/CLCX-caelinux64bit.zip/download
unzip $HOME/CL.zip
mv $HOME/CLCX* $HOME/.CalculixLauncher
rm $HOME/CL.zip
wget -O $HOME/.local/share/applications/CalculixLauncher.desktop https://raw.githubusercontent.com/dymaxionkim/UbuntuBang/master/CalculixLauncher.desktop

###########################################################
# Octave
sudo apt -y install flatpak
flatpak remote-add --if-not-exists flathub
flatpak install flathub org.octave.Octave

###########################################################
# V-Rep
wget -O $HOME/.local/share/applications/V-rep.desktop https://raw.githubusercontent.com/dymaxionkim/UbuntuBang/master/V-rep.desktop

###########################################################
# Pyenv PreRequisites
sudo apt -y install make build-essential libssl-dev zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev llvm libncurses5-dev libncursesw5-dev xz-utils tk-dev

###########################################################
# Imagemagik ghostscript
sudo cp /etc/ImageMagick-6/policy.xml /etc/ImageMagick-6/policy.xml.old
sudo find /etc/ImageMagick-6/ -name "policy.xml" -exec perl -pi -e 's/rights="none"/rights="read|write"/g' {} \;

###########################################################
# TLP for notebook pc
#sudo add-apt-repository -y ppa:linuxuprising/apps
#sudo apt -y install tlp tlpui

###########################################################
# obs-studio
# https://obsproject.com/wiki/install-instructions#linux
sudo add-apt-repository -y ppa:obsproject/obs-studio
sudo apt -y install obs-studio

###########################################################
# User Permission
sudo chown -R `logname`:`logname` $HOME/.pyenv
sudo chown -R `logname`:`logname` $HOME/.Gmsh
sudo chown -R `logname`:`logname` $HOME/.Paraview
sudo chown -R `logname`:`logname` $HOME/.Salome
sudo chown -R `logname`:`logname` $HOME/.local
sudo chown -R `logname`:`logname` $HOME

###########################################################
# Autoremove
sudo apt -y autoremove

# Fin
echo 'Finished!'

