# cgx_prool

Prool's modifications of CalculiX GraphiX (cgx)

Ubuntu
------

In Ubuntu 15.10 and 16.10 need xorg-dev:

sudo apt-get install xorg-dev

how to make:

cd cgx_2.8/src

make

or

cd cgx_2.10/src

make

In Ubuntu 16.10 with gcc6 delete please from file extUtil.h
this 2 lines:

#define min(a,b) ((a) <= (b) ? (a) : (b))

#define max(a,b) ((a) >= (b) ? (a) : (b))

and add flag -Wno-narrowing to Makefile to CFLAGS variable

how to test:

./cgx 1.frd

Windows
-------

For build in Windows use instruction from General Electric:

https://github.com/GeneralElectric/CalculiX

General Electric rulez!

prool contacts
--------------

e-mail proolix@gmail.com

sites:

http://calculixforwin.com

http://calculix.kharkov.org

http://prool.kharkov.org
