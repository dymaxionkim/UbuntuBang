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
//  You should have received a copy of the GNU Library General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

// *** Curve Base Class ***

#ifndef SNL_CURVE_BASE_H
#define SNL_CURVE_BASE_H

#include "snlKnotVector.h"
#include "snlPoint.h"
#include "snlVertexNet.h"

#ifdef SGI_MIPS

    #include <iostream.h>
    #include <math.h>
    #include <float.h>
    
#else

    #include <iostream>
    #include <cmath>
    #include <cfloat>
    
    using namespace std;
    
#endif

class snlCurveBase
{
    public:

        virtual ~snlCurveBase(){};

        virtual snlPoint eval ( knot param ) const = 0;
    
        virtual void vertexNet ( snlVertexNet* vNet, double tolerance, bool parametric ) = 0;
};

#endif
