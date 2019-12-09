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

#include "snlMatrix_4x4.h"

snlMatrix_4X4::snlMatrix_4X4()
{
    element = new double [ 16 ];
    scratch = new double [ 16 ];
}

snlMatrix_4X4::~snlMatrix_4X4()
{
    delete[] element;
    delete[] scratch;
}

snlMatrix_4X4::snlMatrix_4X4 ( snlMatrix_4X4& copyFrom )
{
    element = new double [ 16 ];
    scratch = new double [ 16 ];
    
    for ( int index = 0; index < 16; index ++ )
        element [ index ] = copyFrom.element [ index ];
}

void snlMatrix_4X4::ident()
{
    // Set matrix to identity
    // ----------------------

    for ( int index = 0; index < 16; index ++ )
    {
        element [ index ] = 0;
    }

    element [ 0 ] = 1.0;
    element [ 5 ] = 1.0;
    element [ 10 ] = 1.0;
    element [ 15 ] = 1.0;
}

void snlMatrix_4X4::multiply ( snlMatrix_4X4& multMatrix, bool pre )
{
    // multiply this matrix by given matrix
    // ------------------------------------
    // multMatrix:      Matrix to multiply.
    // pre:             If true then pre multiply.

    double      cVal;

    double*     multA;
    double*     multB;

    int         pIndex = 0;  // Primary Index.
    int         pRow, sRow;
    int         sIndex = 0;  // Scratch Index.
    int         multAIndex;  // Multiplier A matrix index.

    // Multiply AB
    if ( pre )
    {
        // Pre multiply.
        multA = multMatrix.element;
        multB = element;
    }
    else
    {
        // Post multiply.
        multA = element;
        multB = multMatrix.element;
    }

    // Zero scratch space
    for ( int index = 0; index < 16; index ++ ) scratch [ index ] = 0.0;

    for ( int pCol = 0; pCol < 4; pCol ++ )
    {
        multAIndex = 0;

        for ( pRow = 0; pRow < 4; pRow ++ )
        {
            cVal = multB [ pIndex ++ ];

            for ( sRow = 0; sRow < 4; sRow ++ )
            {
                scratch [ sIndex + sRow ] += cVal * multA [ multAIndex ++ ];
            }
        }

        sIndex += 4;
    }

    // Swap scratch and element matrices.
    multA = element;  // multA is simply used as a temp variable.
    element = scratch;
    scratch = multA;
}

void snlMatrix_4X4::translateIdent ( double x, double y, double z )
{
    // Setup matrix as translation identity
    // ------------------------------------

    ident();

    element [ 12 ] = x;
    element [ 13 ] = y;
    element [ 14 ] = z;
}


void snlMatrix_4X4::rotateXIdent ( double yy, double yz, double zy, double zz )
{
    // Create rotation identity for rotation about x axis
    // --------------------------------------------------
    // yy:      y coordinate scalar for new y.
    // yz:      z coordinate scalar for new y.
    // zy:      y coordinate scalar for new z.
    // zz:      z coordinate scalar for new z.

    ident();

    element [ 5 ] = yy;
    element [ 9 ] = yz;
    element [ 6 ] = zy;
    element [ 10 ] = zz;
}

void snlMatrix_4X4::rotateYIdent ( double xx, double xz, double zx, double zz )
{
    // Create rotation identity for rotation about y axis
    // --------------------------------------------------
    // xx:      x coordinate scalar for new x.
    // xz:      z coordinate scalar for new x.
    // zx:      x coordinate scalar for new z.
    // zz:      z coordinate scalar for new z.

    ident();

    element [ 0 ] = xx;
    element [ 8 ] = xz;
    element [ 2 ] = zx;
    element [ 10 ] = zz;
}

void snlMatrix_4X4::rotateZIdent ( double xx, double xy, double yx, double yy )
{
    // Create rotation identity for rotation about y axis
    // --------------------------------------------------
    // xx:      x coordinate scalar for new x.
    // xy:      y coordinate scalar for new x.
    // yx:      x coordinate scalar for new y.
    // yy:      y coordinate scalar for new y.

    ident();

    element [ 0 ] = xx;
    element [ 4 ] = xy;
    element [ 1 ] = yx;
    element [ 5 ] = yy;
}

void snlMatrix_4X4::scaleIdent ( double x, double y, double z )
{
    // Create scaling identity for scaling operation.
    // ----------------------------------------------
    // x, y, z:     Scaling factors.

    ident();

    element [ 0 ] = x;
    element [ 5 ] = y;
    element [ 10 ] = z;
}

double* snlMatrix_4X4::elements()
{
    return element;
}




