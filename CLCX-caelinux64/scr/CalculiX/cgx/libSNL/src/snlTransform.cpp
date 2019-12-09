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

// *** Geometric Transformations ***

#include "snlTransform.h"

#ifdef SGI_MIPS

    #include <math.h>
    
#else

    #include <cmath>
    using namespace std;
    
#endif

snlTransform::snlTransform()
    : snlMatrix_4X4()
{
    ident();
}

snlTransform::snlTransform ( double* matrix )
    : snlMatrix_4X4()
{
    // Copy matrix into transform. Expects OpenGL style array NOT c/c++ style.
    // -----------------------------------------------------------------------

    for ( int index = 0; index < 16; index ++ )
        element [ index ] = matrix [ index ];
}

void snlTransform::transform ( snlPoint* pt )
{
    // Transform given point with matrix
    // ---------------------------------
    // pt:      Homogeneous point to process.

    transform ( pt -> elements );
}

void snlTransform::transform ( snlPoint& pt )
{
    // Transform given point with matrix
    // ---------------------------------
    // pt:      Homogeneous point to process.

    transform ( pt.elements );
}

void snlTransform::transform ( snlVector& vect )
{
    // Transform given vector with matrix
    // ----------------------------------
    // Vect:    Vector to process.

    transform ( vect.elements );
}

void snlTransform::transform ( double* elements )
{
    // Transform given point elements.
    // -------------------------------
    
    double* ptArray = elements;

    double  tmpPt [ 4 ] = { 0.0, 0.0, 0.0, 0.0 };
    double  cVal;

    int     mRow;
    int     mIndex = 0;

    for ( int ptRow = 0; ptRow < 4; ptRow ++ )
    {
        cVal =  ptArray [ ptRow ];

        for ( mRow = 0; mRow < 4; mRow ++ )
        {
            tmpPt [ mRow ] += cVal * element [ mIndex ++ ];
        }
    }
    
    for ( int index = 0; index < 4; index ++ )
        elements [ index ] = tmpPt [ index ];
}

void snlTransform::translate ( double x, double y, double z, bool pre )
{
    // Apply translation
    // -----------------
    // x, y, z:     Translation coordinates.
    // pre:         Pre multiply instead of post multiply.

    if ( x == 0.0 && y == 0.0 && z == 0.0 ) return;

    // Construct translation matrix.

    snlMatrix_4X4   matrix;

    matrix.translateIdent ( x, y, z );

    // Multiply it to current transform matrix.

    multiply ( matrix, pre );
}

void snlTransform::rotateX ( double angle, bool pre )
{
    // Rotate about +ve x axis
    // -----------------------
    // angle:       Rotation angle in radians.

    snlMatrix_4X4   matrix;

    matrix.rotateXIdent ( cos ( angle ), - sin ( angle), sin ( angle ), cos ( angle ) );

    // Multiply it to current transform matrix.

    multiply ( matrix, pre );

}

void snlTransform::rotateY ( double angle, bool pre )
{
    // Rotate about +ve y axis
    // -----------------------
    // angle:       Rotation angle in radians.

    snlMatrix_4X4   matrix;

    matrix.rotateYIdent ( cos ( angle ), sin ( angle), - sin ( angle ), cos ( angle ) );

    // Multiply it to current transform matrix.

    multiply ( matrix, pre );

}

void snlTransform::rotateZ ( double angle, bool pre )
{
    // Rotate about +ve z axis
    // -----------------------
    // angle:       Rotation angle in radians.

    snlMatrix_4X4   matrix;

    matrix.rotateZIdent ( cos ( angle ), - sin ( angle), sin ( angle ), cos ( angle ) );

    // Multiply it to current transform matrix.

    multiply ( matrix, pre );

}

void snlTransform::rotate ( double angle, snlPoint& axisStart, snlVector& axisDirection, bool pre )
{
    // Rotation wrapper function.
    // --------------------------
    
    snlPoint axisEnd = axisStart + axisDirection;
    
    snlTransform tmpTransform;
    
    tmpTransform.rotate ( angle, axisStart, axisEnd );
    
    multiply ( tmpTransform, pre );
}

void snlTransform::rotate ( double angle, snlPoint& axisStart, snlPoint& axisEnd )
{
    // Rotate about an arbitrary axis
    // ------------------------------
    // angle:       Angle to rotate about axis in radians.
    // axisStart:   Start of axis.
    // axisEnd:     End of axis.
    //
    // Notes:       Assumes axisStart and axisEnd have been normalised.
    //              Only works if transform initialised to ident.

    snlMatrix_4X4   matrix;

    snlVector vect ( axisStart, axisEnd );
    vect.unitise();

    // Move axis to origin
    translate ( - axisStart.x(), - axisStart.y(), - axisStart.z(), true );

    if ( vect.y() == 0.0 && vect.z() == 0.0 )
    {
        // Just a rotation about the x axis.
        
        if ( vect.x() > 0.0 )
            rotateX ( angle, true );
        else
            rotateX ( - angle, true );
    }
    else if ( vect.x() == 0.0 && vect.z() == 0.0 )
    {
        // Just a rotation about the Y axis.
        
        if ( vect.y() > 0.0 )
            rotateY ( angle, true );
        else
            rotateY ( - angle, true );
    }
    else if ( vect.x() == 0.0 && vect.y() == 0.0 )
    {
        // Just a rotation about the x axis.
        
        if ( vect.z() > 0.0 )
            rotateZ ( angle, true );
        else
            rotateZ ( - angle, true );
    }
    else
    {
        // Rotate about x axis to yz plane.
        double projHyp = sqrt ( vect.y() * vect.y() + vect.z() * vect.z() );

        double sinRotX = vect.y() / projHyp;
        double cosRotX = vect.z() / projHyp;

        matrix.rotateXIdent ( cosRotX, - sinRotX, sinRotX, cosRotX );
        multiply ( matrix, true );

        // Rotate about y axis to z axis.
        double sinRotY = - ( vect.x() );
        double cosRotY = projHyp;

        matrix.rotateYIdent ( cosRotY, sinRotY, - sinRotY, cosRotY );
        multiply ( matrix, true );

        // Rotate about z axis.
        rotateZ ( angle, true );

        // Inverse rotation about y axis. ie Rotate back into yz plane. Negate angle of rotation.
        matrix.rotateYIdent ( cosRotY, - sinRotY, sinRotY, cosRotY );
        multiply ( matrix, true );

        // Inverse rotation about x axis.
        matrix.rotateXIdent ( cosRotX, sinRotX, - sinRotX, cosRotX );
        multiply ( matrix, true );
    }

    // Inverse translation.
    translate ( axisStart.x(), axisStart.y(), axisStart.z(), true );
}

void snlTransform::scale ( double x, double y, double z, snlPoint& relTo, bool pre )
{

    // Add scaling to transform matrix.
    // --------------------------------

    snlMatrix_4X4   matrix;
    snlMatrix_4X4   step;

    bool isRelTo = ( relTo.x() != 0.0 || relTo.y() != 0.0 || relTo.z() != 0.0 );

    if ( isRelTo ) relTo.normalise();

    matrix.ident();

    if ( isRelTo )
    {
        step.translateIdent ( - ( relTo.x() ), - ( relTo.y() ), - ( relTo.z() ) );

        matrix.multiply ( step, true );
    }

    step.scaleIdent ( x, y, z );

    matrix.multiply ( step, true );

    // Translate back again.
    if ( isRelTo )
    {
        step.translateIdent ( relTo.x(), relTo.y(), relTo.z() );

        matrix.multiply ( step, true );
    }

    multiply ( matrix, pre );
}

void snlTransform::scale ( double x, double y, double z, bool pre )
{
    // Add scaling to transform matrix.
    // --------------------------------

    snlPoint tmpPt ( 0.0, 0.0, 0.0, 1.0 );

    scale ( x, y, z, tmpPt, pre );

}

void snlTransform::align ( snlVector& vector1, snlVector& vector2, bool pre )
{
    // Generate transform that will align vector1 to vector2
    // -----------------------------------------------------
    // vector1:     Vector to align.
    // vector2:     vector to align vector 1 to.
    // pre:         Pre multiply result to existing matrix.
    //
    // Notes:       Can be used for transforming coordinate systems.
    //              Rotates vector1 about normal to plane described by vector1 and vector2.
    
    // Get angle to rotate through.
    double rotAngle = vector1.angle ( vector2 );
    
    // Generate normal to both vectors.
    snlVector normal;    
    normal.crossProduct ( vector1, vector2 );
    
    snlTransform tmpTransform;
    
    snlPoint origin;
        
    // Rotate about normal.
    if ( ( rotAngle != 0.0 ) && ( ! normal.isNull() ) )
        tmpTransform.rotate ( rotAngle, origin, normal );    
    
    multiply ( tmpTransform, pre );
}



