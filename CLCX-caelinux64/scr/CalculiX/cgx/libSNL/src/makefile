# libSNL Source Directory Root Makefile
# -------------------------------------

objFiles := snlCircularOffsetCurve.o \
            snlCtrlPoint.o snlCtrlPointNet.o snlCtrlPointNetCurve.o snlCtrlPointNetSurface.o snlCurve.o \
            snlKnotVector.o \
            snlMatrix_4x4.o snlMeshable.o \
            snlNurbsCommon.o \
            snlPoint.o \
            snlSquareLinear.o snlSurface.o snlSurface_pointLoop.o \
            snlSurface_projection.o snlSurfaceOfRevolution.o \
            snlTransform.o snlTriangleMesh.o \
            snlUtil.o \
            snlVector.o snlVertex.o snlVertexNet.o

libName = libSNL.so.0.2

export cflags = -Wall -fPIC -O3 
export cname = g++

CXXFLAGS := $(cflags)
CFLAGS := $(cflags)

libSNL:     $(objFiles)
	@       echo
	@       echo "*** Building Shared Library ***"
	@       echo
	        $(cname) $(cflags) -shared -o $(libName) $(objFiles)

include                 make.dep

PHONY : dep
dep:                    make.dep

make.dep :              $(objFiles:.o=.h) $(extraIncl)
	@                   echo
	@                   echo "*** Building Dependencies ***"
	@                   echo
	                    $(cname) -MM $(objFiles:.o=.cpp) > make.dep
	@                   echo
	@                   echo "*** Dependencies Built Okay ***"
	@                   echo

PHONY : clean
clean:
	                    rm $(objFiles) make.dep $(libName) snlTest

test:       snlTest.cpp
	        $(cname) $(cflags) snlTest.cpp -o snlTest $(objFiles)

