#!/bin/bash

###########################################################
# Install Pyenv
curl -L https://github.com/pyenv/pyenv-installer/raw/master/bin/pyenv-installer | bash

###########################################################
# Path Pyenv
export PATH="/home/osboxes/.pyenv/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

###########################################################
# Anaconda
pyenv install anaconda3-2019.03
pyenv install anaconda2-2019.03
pyenv global anaconda2-2019.03
pyenv activate anaconda3-2019.03
conda upgrade conda anaconda pip jupyter spyder matplotlib
pyenv activate anaconda2-2019.03

wget -O $HOME/.JupyterNotebook.sh https://raw.githubusercontent.com/dymaxionkim/UbuntuBang/master/JupyterNotebook.sh
chmod +x $HOME/.JupyterNotebook.sh
wget -O $HOME/.local/share/applications/JupyterNotebook.desktop https://raw.githubusercontent.com/dymaxionkim/UbuntuBang/master/JupyterNotebook.desktop
wget -O $HOME/.Spyder.sh https://raw.githubusercontent.com/dymaxionkim/UbuntuBang/master/Spyder.sh
chmod +x $HOME/.Spyder.sh
wget -O $HOME/.local/share/applications/Spyder.desktop https://raw.githubusercontent.com/dymaxionkim/UbuntuBang/master/Spyder.desktop


