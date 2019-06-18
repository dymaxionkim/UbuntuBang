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

// *** NURBS Curve of Circular Offset from Base Curve ***

#ifndef SNL_CIRCULAROFFSET_CURVE_H
#define SNL_CIRCULAROFFSET_CURVE_H

#include "snlCurve.h"
#include "snlCurveBase.h"
#include "snlPoint.h"

class snlCircularOffsetCurve : public snlCurveBase
{
    public:
    
        snlCircularOffsetCurve();
        virtual ~snlCircularOffsetCurve();
        
        snlCircularOffsetCurve ( snlCircularOffsetCurve& copyFrom );
        
        snlCircularOffsetCurve ( snlCurve* baseCurve, snlPoint* axisStart, snlPoint* axisEnd );
        
        void refine ( double tolerance );
        
        virtual void applyOffset ( snlPoint& point, snlPoint chordOffset, snlPoint angleOffset, snlPoint tangentOffset ) const;
        
        int numOffsets();        
        void generateOffsets ( int type, double startOffset, double endOffset );
        void offset ( int index, int type, double val, double weight = 1.0 );
        double offset ( int index, int type );
        
        void vertexNetParam ( snlVertexNet* vNet, double tolerance );        
        
        int size();
        
        double maxParam() const;
        double minParam() const;
        
        enum offsetType
        {
            CHORD,
            ANGLE,
            TANGENT
        };
        
        // Abstract Implementation.
        
        virtual void vertexNet ( snlVertexNet* vNet, double tolerance, bool parametric );        
        
        virtual snlPoint eval ( knot param ) const;
        
    protected:
    
        snlCurve*    base_curve;
        
        snlCurve*    chord_offsetCurve;  // Offsets that correspond to control points.
        snlCurve*    angle_offsetCurve;
        snlCurve*    tangent_offsetCurve;
        
        snlPoint*    axis_start;
        snlPoint*    axis_end;
};

#endif
