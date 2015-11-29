#!/bin/bash

###########################################################
# Making UbuntuBang_Jupyter
# 2015.11.30
# by Dymaxionkim in Github
###########################################################


###########################################################
# Repository


###########################################################
# Update
apt-get update
apt-get upgrade

###########################################################
# Install pip
apt-get -y install python-pip
pip install --upgrade pip distribute
pip install pip-tools

###########################################################
# apt
apt-get -y install python-dev libblas-dev libatlas-base-dev liblapack-dev gfortran libpng-dev libfreetype6-dev libjpeg8-dev pkg-config libncurses5-dev libzmq-dev

###########################################################
# pip
pip install sympy nose sphinx pygments tornado jinja2 numpy scipy matplotlib readline pandas pyzmq scikit-learn jupyter

###########################################################
# python3
apt-get -y install python3-pip python3-dev
pip3 install ipykernel
jupyter kernelspec install-self
cp -r /usr/local/share/jupyter/kernels/python2 /usr/local/share/jupyter/kernels/python3

echo '{' > /usr/local/share/jupyter/kernels/python3/kernel.json
echo '"display_name": "Python 3",' >> /usr/local/share/jupyter/kernels/python3/kernel.json
echo '"language": "python",' >> /usr/local/share/jupyter/kernels/python3/kernel.json
echo '"argv": [' >> /usr/local/share/jupyter/kernels/python3/kernel.json
echo '"/usr/bin/python3",' >> /usr/local/share/jupyter/kernels/python3/kernel.json
echo '"-m",' >> /usr/local/share/jupyter/kernels/python3/kernel.json
echo '"ipykernel",' >> /usr/local/share/jupyter/kernels/python3/kernel.json
echo '"-f",' >> /usr/local/share/jupyter/kernels/python3/kernel.json
echo '"{connection_file}"' >> /usr/local/share/jupyter/kernels/python3/kernel.json
echo ']' >> /usr/local/share/jupyter/kernels/python3/kernel.json
echo '}' >> /usr/local/share/jupyter/kernels/python3/kernel.json

###########################################################
# Julia
add-apt-repository ppa:staticfloat/juliareleases
add-apt-repository ppa:staticfloat/julia-deps
apt-get update
apt-get -y install julia
julia -e 'Pkg.init()'
julia -e 'Pkg.add("IJulia")'
julia -e 'Pkg.update()'




