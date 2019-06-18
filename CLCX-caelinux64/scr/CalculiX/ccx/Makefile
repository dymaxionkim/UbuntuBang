# Prool's makefile
all:
	cd ARPACK;make home=`pwd` lib
	cd SPOOLES.2.2;make lib
	cd CalculiX/ccx_2.13/src;make
	cp CalculiX/ccx_2.13/src/ccx_2.13_MT ccx
clean:
	cd ARPACK;make clean
	cd SPOOLES.2.2;make clean
	cd CalculiX/ccx_2.13/src;make clean
	rm CalculiX/ccx_2.13/*.o
	rm ccx
