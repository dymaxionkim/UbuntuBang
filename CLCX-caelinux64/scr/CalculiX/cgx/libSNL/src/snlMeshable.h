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

// *** Base class of renderable objects ***

#ifndef SNL_MESHABLE_H
#define SNL_MESHABLE_H

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

// Notes:   A curve is not strictly meshable but is included anyway.

class snlMeshable
{
    public:
    
        snlMeshable();

        virtual ~snlMeshable();

        virtual snlPoint eval ( knot paramU, knot paramV ) const = 0;
    
        virtual void vertexNet ( snlVertexNet* vNet, double tolerance, bool parametric ) = 0;
        
        virtual void triangleMesh ( snlTriangleMesh* triMesh, double tolerance );

        enum meshClass
        {
            SNL_CURVE,
            SNL_SURFACE
        };
    
    protected:
};

#endif
