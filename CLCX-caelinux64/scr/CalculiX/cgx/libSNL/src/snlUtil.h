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

// *** Small Utility Classes and Functions ***

#ifndef SNLUTIL_H
#define SNLUTIL_H

#include "snlPoint.h"
#include "snlVector.h"
#include "snlVersion.h"

#ifdef SGI_MIPS

    #include <iostream.h>
    #include <math.h>
    
#else

    #include <iostream>
    #include <cmath>
    
    using namespace std;
    
#endif

#ifndef M_PI

    #define M_PI 3.1415926535897932384626433832795
        
#endif

#ifdef WIN32

    #define isnan _isnan
        
#endif

const int MAX_BINOMIAL = 64;  // Maximum binomial array dimension.

class binCoefs
{
    // Generate a static array of Binomial Coefficients
    // Array structure is [k][i] where k! / i! ( k - i )!.

    public:

        static int  binCoefArray [ MAX_BINOMIAL ] [ MAX_BINOMIAL ];

        binCoefs();
};

double distToLine ( snlPoint lineStart, snlPoint lineEnd, snlPoint compare );
snlVector projectToLine ( snlPoint lineStart, snlPoint lineEnd, snlPoint compare );

bool isInteriorToTriangle ( snlPoint& testPt, snlPoint& verticeA, snlVector& boundA1, snlVector& boundA2,
                            snlPoint& verticeB, snlVector& boundB1, snlVector& boundB2 );

void snlVersion ( int* major, int* minor, int* release );

#endif


