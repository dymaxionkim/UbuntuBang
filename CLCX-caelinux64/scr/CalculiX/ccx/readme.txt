Compilation notes MT version
Go to .../SPOOLES.2.2/MT and make lib
copy spoolesMT.a from .../SPOOLES.2.2/MT/src
to .../SPOOLES.2.2


for 32bit linux go to ...ccx/ARPACK/ARmake.inc
and replace
FFLAGS = -O
to
FFLAGS =

Finally cd... to ...CalculiXLauncher.../scr/CalculiX/ccx
folder and type 'make'
Executable will be copied to the same folder after compilation.

