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

// *** General NURBS Curve ***

#ifndef SNL_CURVE_H
#define SNL_CURVE_H


#include "snlCtrlPointNetCurve.h"
#include "snlCurveBase.h"
#include "snlKnotVector.h"
#include "snlPoint.h"
#include "snlVector.h"
#include "snlVertex.h"
#include "snlVertexNet.h"


class snlCurve : public snlCurveBase
{    
    public:        

        virtual ~snlCurve();
        
        snlCurve();
        
        snlCurve ( int degree, unsigned size, snlPoint& start, snlPoint& end );
                     
        snlCurve ( int degree, unsigned size, snlCtrlPoint* points, knot* knots = 0 );

        snlCurve ( unsigned size, snlCtrlPoint* points, snlKnotVector* knotVector );
        
        snlCurve ( const snlCurve& copyFrom );  // Copy constructor.

        snlCurve& operator= ( const snlCurve& curveToCopy );
        
        // Interpolated / approximated curve.
        snlCurve ( snlPoint* points, unsigned size, int fittingType, int degree, bool closedLoop = false, knot** retParams = 0 );

        // Circular Arc.
        snlCurve ( snlPoint& startPoint, snlPoint& endPoint, snlPoint& centrePoint, int numSections = - 1 );
        
        snlCtrlPointNetCurve& controlPointNet();
        
        snlPoint evalHmg ( knot param ) const;
        
        virtual snlPoint eval ( knot param ) const;
        
        snlPoint* evalDerivsHmg ( knot param, unsigned deriv ) const;
        
        snlPoint* evalDerivs ( knot param, unsigned deriv ) const;
        
        snlVector velocity ( knot param );
        
        void insertKnot ( knot iParam, bool reallocate );
        void insertKnots ( knot iParam, int numToInsert, bool reallocate );
        
        double removeKnots ( int numKnots, unsigned removalIndex, double tolerance );
        double removeKnot ( unsigned removalIndex, double tolerance );        
        
        void refine ( double tolerance );
        
        double maxParam() const;
        double minParam() const;
        
        double param ( unsigned index ) const;

        const snlKnotVector& knotVector();
        
        int degree();
        
        int size();
        
        void truncate ( knot param, bool keepLast = false, bool reparameterise = false );
        void insertPartition ( knot param );
        void reparameterise ( knot startKnot, knot endKnot );
        void reverseEvalDirection();
        
        void globalInterpClosedLoop ( int type, snlPoint* points, unsigned size, int degree, knot** retParams );
        void globalInterp ( int type, snlPoint* points, unsigned size, int degree, knot** retParams );
        static snlCtrlPoint* genGlobalInterpPoints ( snlPoint* points, unsigned size, knot* params, snlKnotVector* knots );
        
        void localInterpQuadratic ( snlPoint* points, unsigned size );  // Local quadratic interpolation.
        void localInterpCubic ( snlPoint* points, unsigned size );  // Local cubic interpolation.
        
        void synchronise ( snlCurve& curve );
        void makeCompatible ( snlCurve* curve );
        
        unsigned createBezierSegments ( int** retNumKnotsAdded );
        
        static void elevateBezierSegmentPointsDegree ( int origDegree, int byDegree, const snlCtrlPoint* origPoints,
                                                       snlCtrlPoint* newPoints );
        
        void elevateDegree ( int byDegree );
        
        void appendCurve ( snlCurve* curve, bool copy = true );

        void print();
        
        // Abstract Implementation.
        
        virtual void vertexNet ( snlVertexNet* vNet, double tolerance, bool parametric );
        
        // Enumerations.
        
        enum SNL_FITTING_TYPES
        {
            SNL_GLOBAL_INTERPOLATION,
            SNL_GLOBAL_INTERP_CENTRIFUGAL,
            SNL_LOCAL_INTERPOLATION
        };
        
    protected:

        void vertexNetParam ( snlVertexNet* vNet, double tolerance );
    
    private:
    
        int        deg;  // Degree of curve.
    
        snlCtrlPointNetCurve*       ctrlPtNet;
                                    
        snlKnotVector*              knotVect;
};

#endif
