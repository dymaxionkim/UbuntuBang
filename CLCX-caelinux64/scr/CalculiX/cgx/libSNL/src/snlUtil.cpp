// libSNL - Simple Nurbs Library
// Copyright 2003 Scott A.E. Lanham, Australia.
// --------------------------------------------
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

// *** Small Utility Classes and Functions ***

#include "snlUtil.h"

// Static member declarations
// --------------------------

binCoefs    bCoefs;  // Force constructor to build array.
int         binCoefs::binCoefArray [ MAX_BINOMIAL ] [ MAX_BINOMIAL ];

binCoefs::binCoefs()
{
    for ( int k = 0; k < MAX_BINOMIAL; k ++ )

        for ( int i = 0; i < MAX_BINOMIAL; i ++ )
        {
            if ( k == i )
                binCoefArray [ k ] [ i ] = 1;
            else if ( i == 0 )
                binCoefArray [ k ] [ i ] = 1;
            else if ( i > k )
                binCoefArray [ k ] [ i ] = 0;
            else
                binCoefArray [ k ] [ i ] = binCoefArray [ k - 1 ] [ i ] + binCoefArray [ k - 1 ] [ i - 1 ];
        }
}

double distToLine ( snlPoint lineStart, snlPoint lineEnd, snlPoint compare )
{
    // Calculate distance to line from point.
    // --------------------------------------
    // lineStart:    Start of line.
    // lineEnd:      End of line.
    // compare:      Point to compare to line.
    
    lineStart.normalise();
    lineEnd.normalise();
    compare.normalise();
    
    snlVector lineV ( lineStart, lineEnd );    
    
    snlVector testV ( lineStart, compare );

    // Calculate dot product.
    double dotP = lineV.dot ( testV );

    // Project test vector onto baseline vector.
    double proj = dotP / lineV.length();

    // Length of test vector.
    double testLength = testV.length();

    // Length of normal from baseline to test point.
    double normDist = sqrt ( testLength * testLength - proj * proj );

    return normDist;
}

snlVector projectToLine ( snlPoint lineStart, snlPoint lineEnd, snlPoint compare )
{
    // Calculate vector perpendicular to line from point.
    // --------------------------------------------------
    // lineStart:    Start of line.
    // lineEnd:      End of line.
    // compare:      Point to project to line.
    //
    // Notes:        Returned vector points _TO_ line if based from given point "compare".

    lineStart.normalise();
    lineEnd.normalise();
    compare.normalise();
    
    snlVector lineV ( lineStart, lineEnd );    
    
    snlVector testV ( lineStart, compare );

    // Calculate dot product.
    double dotP = lineV.dot ( testV );

    // Calculate length of test vector after it is projected onto baseline vector.
    double proj = dotP / lineV.length();

    // Generate projected vector.
    
    lineV.length ( proj );
    lineV -= testV;

    return lineV;
}

bool isInteriorToTriangle ( snlPoint& testPt, snlPoint& verticeA, snlVector& boundA1, snlVector& boundA2,
                            snlPoint& verticeB, snlVector& boundB1, snlVector& boundB2 )
{
    // Check to see if a point is interior to a triangle.
    // --------------------------------------------------
    // testPt:      Point to test.
    // verticeA:    First vertice to test against.
    // boundA1:     Unit vector from vertice A that represents triangle boundary or edge.
    // boundA2:     Unit vector from vertice A that represents triangle boundary or edge.
    // verticeB:    Second vertice to test against.
    // boundB1:     Unit vector from vertice B that represents triangle boundary or edge.
    // boundB2:     Unit vector from vertice B that represents triangle boundary or edge.
    //
    // Notes:       Assumes testPt is coplanar to triangle.
    //              Make sure bounds are unit vectors!

    bool isInterior = true;

    double triCos = boundA1.dot ( boundA2 );  // Cosine between two edges of the triangle.

    snlVector testPointVector ( verticeA, testPt );  // Vector from triangle vertice to projected point.

    testPointVector.unitise();

    // Remember that the cosine that results from a dot product is clamped between 1 and -1.
    // 1 represent and angle of 0 degrees and -1 represents and angle of 180 degrees.

    if ( testPointVector.dot ( boundA1 ) < triCos || testPointVector.dot ( boundA2 ) < triCos )
        isInterior = false;

    // Must do a second check rooted at a different vertice.

    triCos = boundB1.dot ( boundB2 );

    testPointVector.calc ( verticeB, testPt );

    testPointVector.unitise();

    if ( testPointVector.dot ( boundB1 ) < triCos || testPointVector.dot ( boundB2 ) < triCos )
        isInterior = false;

    return isInterior;
}

void snlVersion ( int* major, int* minor, int* release )
{
    *major = SNL_VERSION_MAJOR;
    *minor = SNL_VERSION_MINOR;
    *release = SNL_VERSION_RELEASE;
}

