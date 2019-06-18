Prepared by Sergey Pustovoitov,
http://calculix.kharkov.org/

installation guide for ubuntu 14 - 64 bit

1) install gfortran
sudo apt-get install gfortran


2) cd to ../CalculiXU/src/ccx folder and type make
to compile calculix ccx statically linked file


3) cgx can not be made statically linked
you may use binaries from calculix.de
or try to compile it from sources

sudo apt-get install libglu1-mesa-dev freeglut3-dev

sudo apt-get install mesa-common-dev

sudo apt-get install libxmu-dev

sudo apt-get install libxtst-dev

make

This archive contains special build of the cgx with some additional cards





