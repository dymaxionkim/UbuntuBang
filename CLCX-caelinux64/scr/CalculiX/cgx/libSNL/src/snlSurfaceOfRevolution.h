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

// ***!! Deprecated - Use snlSurface Instead !!****

// *** Surface of Revolution ***

#ifndef SNL_SURFACEOFREVOLUTION_H
#define SNL_SURFACEOFREVOLUTION_H

#include "snlCurve.h"
#include "snlPoint.h"
#include "snlSurfaceBase.h"

class snlSurfaceOfRevolution : public snlSurfaceBase
{
    public:
    
        snlSurfaceOfRevolution();
        virtual ~snlSurfaceOfRevolution();
        
        snlSurfaceOfRevolution ( snlCurve* profileCurve, snlPoint* axisStart, snlPoint* axisEnd, double rotationAngle );
        
        snlCurve& profileCurve();
        void profileCurve ( snlCurve* profileCurve );
        
        snlPoint& axisStart();
        void axisStart ( snlPoint* startPoint );
        
        snlPoint& axisEnd();
        void axisEnd ( snlPoint* endPoint );
        
        double rotationAngle();
        void rotationAngle ( double angle );
        
        // Abstract Implementation.
        
        virtual void vertexNet ( snlVertexNet* vNet, double tolerance, bool parametric );
        
        virtual snlPoint eval ( knot paramU, knot paramV ) const;

        virtual void triangleMesh ( snlTriangleMesh* triMesh, double tolerance );
    
    private:
    
        snlCurve*    profile;
        
        snlPoint*    axis_start;
        snlPoint*    axis_end;
        
        double       rot_angle;
};

#endif
