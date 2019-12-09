// libSNL - Simple Nurbs Library
// Copyright Scott A.E. Lanham, Australia.
// ---------------------------------------
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU Library General Public License for more details.
//
//  You should have received a copy of the GNU Library General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

// *** 4x4 Matrix of doubles ***

#ifndef SNLMATRIX4X4_H
#define SNLMATRIX4X4_H

#include "snlPoint.h"

#ifdef SGI_MIPS

    #include <iostream.h>
    #include <math.h>
    
#else

    #include <iostream>
    #include <cmath>
    
    using namespace std;
    
#endif

class snlMatrix_4X4
{
    // Matrix optimised for size 4 X 4 of double

    public:

        snlMatrix_4X4();
        virtual ~snlMatrix_4X4();
        
        snlMatrix_4X4 ( snlMatrix_4X4& copyFrom );

        void ident();  // Set matrix to identity.

        void translateIdent ( double x, double y, double z );  // Setup matrix as translation identity.

        void rotateXIdent ( double yy, double yz, double zy, double zz );
        void rotateYIdent ( double xx, double xz, double zx, double zz );
        void rotateZIdent ( double xx, double xy, double yx, double yy );

        void scaleIdent ( double x, double y, double z );

        void multiply ( snlMatrix_4X4&, bool pre = false );  // Multiply this matrix by given matrix.

        void transform ( snlPoint* );  // Transform given point with matrix.
        
        double* elements();

    protected:

        double*     element;  // OpenGL style array. NOT c/c++.
        double*     scratch;  // Scratch space for matrix multiplication etc.
};

#endif
