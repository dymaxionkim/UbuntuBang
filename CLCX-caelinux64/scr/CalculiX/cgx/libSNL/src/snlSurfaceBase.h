// libSNL - Simple Nurbs Library
// Copyright 2005 Scott A.E. Lanham, Australia.
// --------------------------------------------
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.

// *** Base Class Of All Surfaces ***

#ifndef SNL_SURFACE_BASE_H
#define SNL_SURFACE_BASE_H

#include "snlKnotVector.h"
#include "snlPoint.h"
#include "snlTriangleMesh.h"
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

class snlSurfaceBase
{
    public:

        virtual ~snlSurfaceBase(){};

        virtual snlPoint eval ( knot paramU, knot paramV ) const = 0;

        virtual void vertexNet ( snlVertexNet* vNet, double tolerance, bool parametric ) = 0;

        virtual void triangleMesh ( snlTriangleMesh* triMesh, int toleranceType, double tolerance ) = 0;

        enum meshToleranceType
        {
            SNL_TOL_DISTANCE,  // Must be within distance tolerance to surface.
            SNL_TOL_ANGLE      // Angle between successive sections must be less than tolerance.
        };
};

#endif
