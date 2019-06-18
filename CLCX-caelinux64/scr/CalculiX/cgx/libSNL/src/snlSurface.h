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
//  GNU General Public License for more details.

// *** General NURBS Surface ***

#ifndef SNLSURFACE_H
#define SNLSURFACE_H

#include "snlCtrlPointNetSurface.h"
#include "snlCurve.h"
#include "snlKnotVector.h"
#include "snlPoint.h"
#include "snlSurfaceBase.h"
#include "snlVertex.h"
#include "snlVertexNet.h"

#include "ptrList.h"

#define SNL_INV_ITER_VP 8  // Number of iterations for velocity pass.
#define SNL_INV_ITER_NP 8  // Number of iterations for newton pass.

#define SNL_NUMERIC_NOISE 1.0e-14

typedef struct
{
    /* Surface Edge */

    int     direction;  // u = 0, v = 1.
    knot    pVal;  // Parameter value.

} sEdge;

typedef struct
{
    // surface location.

    knot        paramU;  // Evaluated parameter in u direction. Point found.
    knot        paramV;  // Evaluated parameter in v direction.
    snlPoint    pt;  // Point corresponding to parameters.
    basis       dist;  // Distance from point being inverted / projected to point found.
    double      cos;  // Cos of angle to normal.

    int         origPtIndex;  // For inversion and projection, the index of the original point handed to the
                              // relevant function.

} snlSurfLocn;

typedef struct
{
    // surface location.

    knot        paramU;  // Evaluated parameter in u direction. Point found.
    knot        paramV;  // Evaluated parameter in v direction.
    snlPoint    pt;  // Point corresponding to parameters.
    basis       dist;  // Distance squared from point being inverted to point found.
    double      cos;  // Cos of angle to normal.
    
    int         origPtIndex;  // For inversion and projection, the index of the original point handed to the
                              // relevant function.

    int         spanNumber;  // Absolute span number guess belongs to.

    knot        minU;  // Parametric boundaries used for culling.
    knot        maxU;
    knot        minV;
    knot        maxV;

    bool        culled;
    bool        ignoreParamBounds;  // Don't cull this point if it goes out of bounds.
    bool        converged;
    
} snlSurfLocnGuess;

typedef struct
{
    // Control point location for surface

    int             uIndex;  // Array indexes.
    int             vIndex;

    snlCtrlPoint*   ctrlPt;  // Pointer to control point found.

    double          dist;  // Distance to control point from comparison point.
    
} snlSCtrlPtLocn;

typedef struct
{
    bool        converged;
    bool        stalled;  // Can't converge

    int         arcIndex;

    double      arcAngle;

    // Controlling Surface.

    knot        cParamU;
    knot        cParamV;

    snlPoint    cPt;  // Evaluated point.
    snlPoint    normPt;  // Normal point.

    snlVector   velU;  // U direction velocity.
    snlVector   velV;  // V direction velocity.

    // Matching Surface.

    knot        mParamU;
    knot        mParamV;

    snlPoint    mPt;  // Evaluated point.

} arcLocn;

class snlSurface : public snlSurfaceBase
{
    public:
        
        
        virtual ~snlSurface();
        
        snlSurface();

        void init();  // Standard Initialisation.
        
        snlSurface ( const snlSurface& surfaceToCopy );  // Copy constructor.
    
        snlSurface ( int degreeU, int degreeV, unsigned sizeU, unsigned sizeV,
                     snlPoint& origin, snlPoint& cornerMaxU, snlPoint& cornerMaxV );
                     
        snlSurface ( int degreeU, int degreeV, unsigned sizeU, unsigned sizeV, snlCtrlPoint* points,
                     knot* knotsU, knot* knotsV );
        
        snlSurface ( snlCurve& curve1, snlCurve& curve2, int direction = SNL_U_DIR );  // Generate ruled surface.

        snlSurface ( snlCurve& generator, snlPoint& axisStart, snlPoint& axisEnd, double angle );  // Surface of revolution.

        snlSurface ( snlCurve** curves, int numCurves, int dir = SNL_U_DIR );  // Skinned surface.

        snlSurface ( int interpType, snlPoint* pointsInterp, int sizeU, int sizeV,
                     int degreeU, int degreeV );  // Interpolated surface.

        typedef enum
        {
            //SNL_PRIM_AUTO,
            SNL_PRIM_PLANE,
            SNL_PRIM_SPHERE,
            SNL_PRIM_CYLINDER,
            SNL_PRIM_CONE,
            SNL_BILINEAR_COONS
            
        } snlPrimType;

        // Construct surface that fits a closed loop as described by points.
        snlSurface ( snlPoint* points, int numPoints );  // Unknown topology.
        snlSurface ( snlPoint* points, int numPoints, snlPrimType primType, snlPoint* axisStart = 0, snlPoint* axisEnd = 0 );

        snlSurface ( snlCurve* U1, snlCurve* U2, snlCurve* V1, snlCurve* V2 );  // Bilinear Coons patch.

        // Operators

        snlSurface& operator= ( const snlSurface& surface );

        // Interpolation Functions.

        enum SNL_INTERP_TYPES
        {
            SNL_GLOBAL_INTERP_CHORDLENGTH,
            SNL_GLOBAL_INTERP_CENTRIFUGAL
        };
        
        // Data functions.
        
        int degreeU() const;
        int degreeV() const;
        
        unsigned sizeU() const;
        unsigned sizeV() const;

        knot minU();
        knot maxU();
        
        knot minV();
        knot maxV();
        
        const snlCtrlPoint* controlPoints();
        
        const knot* knotsU();
        const knot* knotsV();
        
        snlCtrlPointNetSurface& controlPointNet();
        
        const snlKnotVector& knotVectorU();
        const snlKnotVector& knotVectorV();
        
        // Evaluation functions.

        // Non-rational homogeneous surface point.        
        snlPoint evalHmg ( knot paramU, knot paramV, basis* basisU = 0, basis* basisV = 0 ) const;
        
        // Rational non-homogeneous surface point.
        virtual snlPoint eval ( knot paramU, knot paramV, basis* basisU, basis* basisV ) const;
        virtual snlPoint eval ( knot paramU, knot paramV ) const;

        // Derivatives.

        snlPoint* evalDerivsHmg ( knot paramU, knot paramV, unsigned derivU, unsigned derivV,
                                  basis* basisU = 0, basis* basisV = 0 );
                                  
        snlPoint* evalDerivs ( knot paramU, knot paramV, unsigned derivU, unsigned derivV );

        void velocities ( knot paramU, knot paramV, snlPoint& evalPoint, snlVector& velocityU, snlVector& velocityV,
                          basis* basisU = 0, basis* basisV = 0 );

        // Knot manipulation.
        
        void insertKnot ( knot iParam, int dir, bool reallocate = true );
        void insertKnot ( knot iParam, int dir, int numToInsert, bool reallocate = true );
        double removeKnots ( int numKnots, unsigned removalIndex, int direction, double tolerance, bool reallocate = true );
        double removeKnot ( unsigned removalIndex, int direction, double tolerance, bool reallocate = true );

        // Projection.

        snlVertex* project_depr ( snlPoint* toProject, int numPoints, double convergTol, double
                                  normTol, int maxPass );

        snlSurfLocn* invert ( snlPoint* toInvert, int numPoints, int* retArraySize,
                              double convergTol, double normTol, int maxPass );

        snlSurfLocn* project ( snlPoint* toProject, int numPoints, int* retArraySize,
                               double convergTol, double normTol, int maxPass );

        snlSurfLocn* fastProject ( snlPoint* toProject, int numPoints, int* retArraySize,
                                   double convergTol, double normTol, int maxPass,
                                   int sensitivity, int maxLocns );
        
        snlSCtrlPtLocn* findClosestCtrlPt ( snlPoint* points, int numPoints );
        
        // Try to predict edges that may have ambiguities during projection.
        int hasAmbigEdges ( sEdge* results, double tolerance = 1.0e-6 );
        int hasAmbigEdges_depr ( sEdge* results );

        // Surface decomposition.
        
        unsigned createBezierSegments ( int dir, int** numKnotsAdded = 0);
        void createBezierSegments ( int* numU = 0, int* numV = 0 );
        void createConvexBezierSegments ( int* numU = 0, int* numV = 0, double sensitivity = 0.0 );

        void elevateDegree ( int direction, int byDegree );
        double reduceDegree ( int dir, unsigned numDeg, double tolerance );
        
        void refine ( double tolerance );  // Refine control point net.

        void refineHull_UV ( double tolerance );  // Refine control point net using convex hull methods.
        bool refineHull_U ( double tolerance, bool singlePass = false );
        bool refineHull_V ( double tolerance, bool singlePass = false );

        void refineHullBezier ( double tolerance );  // Refine control point net by Bezier patch subdivision.
        
        double maxCurvatureU();
        double maxCurvatureV();

        snlVector calcNormal ( knot paramU, knot paramV, snlPoint* evalPt = 0 );

        snlCurve* extractEdge ( int edge );

        snlSurface* fillet ( int edge, snlVector& frontFaceNormal,
                             snlSurface& surface2, snlVector& frontFaceNormal2,
                             double tolerance, double radius, bool trim1, bool trim2 );

        void transform ( snlTransform& transf );

        void makeCompatible ( snlSurface* surfaceToMatch, int direction );
        void synchronise ( snlSurface& surface, int direction );

        // Trimming Functions.

        void addTrimCurve ( snlCurve* curve );
        bool removeTrimCurve ( snlCurve* curve );

        // Misc Functions.
        
        void print();

        void print_cpp();
        
        // Abstract Implementation.
        
        virtual void vertexNet ( snlVertexNet* vNet, double tolerance, bool parametric );

        virtual void triangleMesh ( snlTriangleMesh* triMesh, int toleranceType, double tolerance );
        
        enum parametricDirections
        {
            SNL_U_DIR = 0,
            SNL_V_DIR = 1
        };

        enum surfaceEdges
        {
            SNL_EDGE_UMIN,
            SNL_EDGE_UMAX,
            SNL_EDGE_VMIN,
            SNL_EDGE_VMAX
        };
        
    protected:

        void copyFrom ( const snlSurface& surfaceToCopy );

        void fitBilinearCoons ( snlPoint* points, int numPoints );
        void genBilinearCoons ( snlCurve* curve_U1, snlCurve* curve_U2, snlCurve* curve_V1, snlCurve* curve_V2 );
        void fitPlane ( snlPoint* points, int numPoints );
        void fitCylinder ( snlPoint* points, int numPoints, snlPoint* axisStart, snlPoint* axisEnd );
        void fitCone ( snlPoint* points, int numPoints, snlPoint* axisStart, snlPoint* axisEnd );
        void fitSphere ( snlPoint* points, int numPoints, snlPoint* sphereCentre );

        knot* globalInterpGenParams ( int type, snlPoint* points, int sizeU, int sizeV, int dir );

        void genGlobalInterpSurf ( int interpType, snlPoint* pointsInterp, int sizeU, int sizeV, int degreeU, int degreeV );

        snlSurfLocn* processGuesses ( snlPoint* points, int numPoints, int* retArraySize,
                                      ptrList <snlSurfLocnGuess>* guesses, double convergTol,
                                      double normTol, int maxPass, bool retNonConverged = false, bool noCull = false,
                                      int numVelocity = SNL_INV_ITER_VP, int numNewton = SNL_INV_ITER_NP );

        bool convergeVelocity ( snlPoint* convergToPts, ptrList <snlSurfLocnGuess>* guesses,
                                int numIterations, double convergTol, double normTol );

        bool convergeNewton ( snlPoint* convergToPts, ptrList <snlSurfLocnGuess>* guesses,
                              int numIterations, double convergTol, double normTol );

        ptrList <snlSurfLocnGuess>* guessInvLocation ( snlPoint* points, int numPoints, bool* pointMask,
                                                       int granU, int granV );

        ptrList <snlSurfLocnGuess>* guessProjLocation ( snlPoint* points, int numPoints, bool* pointMask );

        ptrList <snlSurfLocnGuess>* guessFastProjLocation ( snlPoint* points, int numPoints, int maxGuessPerPt,
                                                            int granU, int granV );

//        ptrList <snlSurfLocnGuess>* guessProjLocation_triMethod ( snlPoint* points, int numPoints,
//                                                                  bool* pointMask );

        void genSurfRevolution ( snlCurve& generator, snlPoint& axisStart, snlPoint& axisEnd, double angle );
                                                            
        // Data Section
        
        int     degU;  // Degree of surface in U direction.
        int     degV;  // Degree of surface in V direction.
    
        snlCtrlPointNetSurface*     ctrlPtNet;
        
        snlKnotVector*              knotVectU;
        snlKnotVector*              knotVectV;

        ptrList< snlCurve >*        trim_curves;
};

#endif
