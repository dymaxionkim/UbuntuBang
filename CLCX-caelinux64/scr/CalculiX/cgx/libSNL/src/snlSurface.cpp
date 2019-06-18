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

#include "snlSurface.h"
#include "snlUtil.h"

#include "snlNurbsCommon.h"

snlSurface::~snlSurface()
{
    if ( ctrlPtNet ) delete ctrlPtNet;
    
    if ( knotVectU ) delete knotVectU;
    if ( knotVectV ) delete knotVectV;

    if ( trim_curves ) delete trim_curves;
}

snlSurface::snlSurface()
{
    init();
}

void snlSurface::init()
{
    //! Standard Initialisation.
    //  ------------------------

    ctrlPtNet = 0;

    knotVectU = 0;
    knotVectV = 0;

    trim_curves = new ptrList< snlCurve >;
}

snlSurface::snlSurface ( const snlSurface& surfaceToCopy )
{
    //! Copy constructor.
    //  -----------------

    init();

    copyFrom ( surfaceToCopy );
}

snlSurface::snlSurface ( int degreeU, int degreeV, unsigned sizeU, unsigned sizeV, 
                         snlPoint& origin, snlPoint& cornerMaxU, snlPoint& cornerMaxV )
{
    //! Construct a new NURBS surface.
    //  ------------------------------
    //! @param degreeU Degree of surface in U direction.
    //! @param degreeV Degree of surface in V direction.
    //! @param sizeU Number of control points in the U dimension.
    //! @param sizeV Number of control points in the V dimension.
    //! @param origin Point at (u,v) = (0,0).
    //! @param cornerMaxU Point at (u,v) = (MaxU,0).
    //! @param cornerMaxV Point at (u,v) = (0,MaxV).

    init();
    
    ctrlPtNet = new snlCtrlPointNetSurface ( sizeU, sizeV, origin, cornerMaxU, cornerMaxV );
    
    knotVectU = new snlKnotVector ( 0.0, 1.0, sizeU + degreeU + 1, degreeU );
    knotVectV = new snlKnotVector ( 0.0, 1.0, sizeV + degreeV + 1, degreeV );
    
    degU = degreeU;
    degV = degreeV;
}

snlSurface::snlSurface ( int degreeU, int degreeV, unsigned sizeU, unsigned sizeV, snlCtrlPoint* points,
                         knot* knotsU, knot* knotsV )
{
    //! Create surface from existing data.
    //  ----------------------------------
    //! @param degreeU Degree in U direction.
    //! @param degreeV Degree in V direction.
    //! @param sizeU Size of U dimension.
    //! @param sizeV Size of V dimension.
    //! @param points Control points to use.
    //! @param knotsU Array of knots for U direction.
    //! @param knotsV Array of knots for V direction.
    //!
    //! @par   Notes:
    //!        Assumes a clamped (open) knot vector.
    //! @attention Does NOT COPY point and knot data. So don't delete them elsewhere.

    init();
    
    degU = degreeU;
    degV = degreeV;

    ctrlPtNet = new snlCtrlPointNetSurface ( points, sizeU, sizeV, false );
    
    if ( knotsU )
        knotVectU = new snlKnotVector (  knotsU, sizeU + degU + 1, degreeU );
    else
        knotVectU = new snlKnotVector (  0.0, 1.0, sizeU + degU + 1, degreeU );
    
    if ( knotsV )    
        knotVectV = new snlKnotVector (  knotsV, sizeV + degV + 1, degreeV );
    else
        knotVectV = new snlKnotVector (  0.0, 1.0, sizeV + degV + 1, degreeV );
}

snlSurface::snlSurface ( snlCurve& curve1, snlCurve& curve2, int direction )
{
    //! Generate ruled surface.
    //  -----------------------
    //! @param curve1 First side of surface.
    //! @param curve2 Second side of surface.
    //! @param direction Parmetric direction defining curves lay in.

    init();

    // Curves may be modified so copy them.

    snlCurve* curve_1 = new snlCurve ( curve1 );
    snlCurve* curve_2 = new snlCurve ( curve2 );

    // Make sure curves are compatible.

    curve_1 -> makeCompatible ( curve_2 );

    // Generate knot vectors.

    if ( direction == SNL_U_DIR )
    {
        knotVectU = new snlKnotVector ( curve_1 -> knotVector() );
        knotVectV = new snlKnotVector ( 0.0, 1.0, 4, 1 );
    }
    else
    {
        knotVectU = new snlKnotVector ( 0.0, 1.0, 4, 1 );
        knotVectV = new snlKnotVector ( curve_1 -> knotVector() );
    }
    
    // Generate control points.
    
    int size = curve_1 -> size();
    int arraySize = size * 2;
    
    snlCtrlPoint* ctrlPts = new snlCtrlPoint [ arraySize ];
    
    const snlCtrlPoint* copyNet1 = curve_1 -> controlPointNet().getCtrlPts();
    const snlCtrlPoint* copyNet2 = curve_2 -> controlPointNet().getCtrlPts();
    
    int ctrlPtsIndex = 0;

    if ( direction == SNL_U_DIR )
    {
        for ( int index = 0; index < size; index ++ )
        {
            ctrlPts [ ctrlPtsIndex ++ ] = copyNet1 [ index ];
            ctrlPts [ ctrlPtsIndex ++ ] = copyNet2 [ index ];
        }
        
        ctrlPtNet = new snlCtrlPointNetSurface ( ctrlPts, size, 2, false );
        
        degU = curve_1 -> degree();
        degV = 1;
    }
    else
    {
        int ctrlPtsIndex2 = size;
        
        for ( int index = 0; index < size; index ++ )
        {
            ctrlPts [ ctrlPtsIndex ++ ] = copyNet1 [ index ];
            ctrlPts [ ctrlPtsIndex2 ++ ] = copyNet2 [ index ];
        }

        ctrlPtNet = new snlCtrlPointNetSurface ( ctrlPts, 2, size, false );
        
        degU = 1;
        degV = curve_1 -> degree();
    }

    // Clean up.

    delete curve_1;
    delete curve_2;
}

snlSurface::snlSurface ( snlCurve& generator, snlPoint& axisStart, snlPoint& axisEnd, double angle )
{
    //! Construct surface of revolution.
    //  --------------------------------
    //! @param generator Generating curve.
    //! @param axisStart Starting point of axis generator is revolved about.
    //! @param axisEnd Ending point of axis.
    //! @param angle Angle in degrees to revolve generator about axis. Angle is in degrees so that
    //!              different precisions of PI do not affect surface closure.
    //!
    //! @par Notes:
    //!      Rotation is counter clockwise about axis vector ie Right hand rule. Curve defines V
    //!      direction.

    init();

    genSurfRevolution ( generator, axisStart, axisEnd, angle );
}

snlSurface::snlSurface ( snlCurve** curves, int numCurves, int dir )
{
    //! Construct skinned surface.
    //  --------------------------
    //! @param curves Curves to skin with.
    //! @param numCurves Number of curves in curves array.
    //! @param dir Parametric direction to skin over.
    //!
    //! @par Notes:
    //!      The skinned direction is degree 2. \n
    //!      It is assumed the curves are exactly the same size with the same knot vector.

    init();
    
    // Assemble points to be interpolated.

    int numCurvePts = curves [ 0 ] -> size();
    int numPts = numCurvePts * numCurves;

    int sizeU = ( dir == SNL_U_DIR ? numCurves : numCurvePts );
    int sizeV = ( dir == SNL_U_DIR ? numCurvePts : numCurves );

    snlPoint* points = new snlPoint [ numPts ];

    for ( int curveIndex = 0; curveIndex < numCurves; curveIndex ++ )
    {
        int index, step;

        if ( dir == SNL_U_DIR )
        {
            index = curveIndex * numCurvePts;
            step = 1;
        }
        else
        {
            index = curveIndex;
            step = numCurves;
        }

        const snlCtrlPoint* curvePts = ( curves [ curveIndex ] -> controlPointNet() ).getCtrlPts();
        
        for ( int ptIndex = 0; ptIndex < numCurvePts; ptIndex ++ )
        {
            points [ index ] = curvePts [ ptIndex ];

            index += step;
        }
    }

    // Generate parameters

    knot* params = globalInterpGenParams ( SNL_GLOBAL_INTERP_CENTRIFUGAL, points, sizeU, sizeV, dir );
                                           
    // Generate knot vectors.

    if ( dir == SNL_U_DIR )
    {
        knotVectU = new snlKnotVector ( numCurves, 2, params );
        knotVectV = new snlKnotVector ( curves [ 0 ] -> knotVector() );
    }
    else
    {
        knotVectU = new snlKnotVector ( curves [ 0 ] -> knotVector() );
        knotVectV = new snlKnotVector ( numCurves, 2, params );
    }

    // Generate control points.

    snlPoint* linePts = new snlPoint [ numCurves ];

    snlCtrlPoint* ctrlPts = new snlCtrlPoint [ numPts ];

    for ( int lineIndex = 0; lineIndex < numCurvePts; lineIndex ++ )
    {
        // Assemble points.

        int index, step;

        if ( dir == SNL_U_DIR )
        {
            index = lineIndex;
            step = numCurvePts;
        }
        else
        {
            index = lineIndex * numCurves;
            step = 1;
        }

        for ( int ptIndex = 0; ptIndex < numCurves; ptIndex ++ )
        {
            linePts [ ptIndex ] = points [ index ];
            index += step;
        }

        // Interpolate between assembled points.

        snlCtrlPoint* finalPts = snlCurve::genGlobalInterpPoints ( linePts, numCurves, params,
                                                                   dir == SNL_U_DIR ? knotVectU : knotVectV );

        // Copy points into surface control point array.

        if ( dir == SNL_U_DIR )
        {
            index = lineIndex;
            step = numCurvePts;
        }
        else
        {
            index = lineIndex * numCurves;
            step = 1;
        }

        for ( int ptIndex = 0; ptIndex < numCurves; ptIndex ++ )
        {
            ctrlPts [ index ] = finalPts [ ptIndex ];
            index += step;
        }

        delete[] finalPts;
    }

    ctrlPtNet = new snlCtrlPointNetSurface ( ctrlPts, sizeU, sizeV );

    // Set other variables.
    
    if ( dir == SNL_U_DIR )
    {
        degU = 2;
        degV = curves [ 0 ] -> degree();
    }
    else
    {
        degU = curves [ 0 ] -> degree();
        degV = 2;
    }

    // Clean up.

    delete[] points;
    delete[] params;
    delete[] linePts;
}

snlSurface::snlSurface ( int interpType, snlPoint* pointsInterp, int sizeU, int sizeV, int degreeU, int degreeV )
{
    //! Generate surface by interpolating point data.
    //  ---------------------------------------------
    //! @param interpType Type of interpolation. Comes from enum SNL_INTERP_TYPES.
    //! @param pointsInterp Points to interpolate. Point array.
    //! @param sizeU Size of point array dimension in U direction.
    //! @param sizeV Size of point array dimension in V direction.
    //! @param degreeU Degree of U direction.
    //! @param degreeV Degree of V direction.

    init();

    if ( interpType == SNL_GLOBAL_INTERP_CHORDLENGTH || interpType == SNL_GLOBAL_INTERP_CENTRIFUGAL )
        genGlobalInterpSurf ( interpType, pointsInterp, sizeU, sizeV, degreeU, degreeV );
}

snlSurface::snlSurface ( snlCurve* U1, snlCurve* U2, snlCurve* V1, snlCurve* V2 )
{
    //! Generate Coons Patch from given curves.
    //  ---------------------------------------
    //! @param U1 First curve in U direction.
    //! @param U2 Second curve in U direction.
    //! @param V1 First curve in V direction.
    //! @param V2 Second curve in V direction.

    init();

    genBilinearCoons( U1, U2, V1, V2 );
}

void snlSurface::copyFrom ( const snlSurface& surfaceToCopy )
{
    //! Copy contents of another surface into this.
    //  -------------------------------------------

    ctrlPtNet = new snlCtrlPointNetSurface ( *(surfaceToCopy.ctrlPtNet) );
    
    knotVectU = new snlKnotVector ( *(surfaceToCopy.knotVectU) );
    knotVectV = new snlKnotVector ( *(surfaceToCopy.knotVectV) );
    
    degU = surfaceToCopy.degU;
    degV = surfaceToCopy.degV;

    snlCurve* trimCurve = ( surfaceToCopy.trim_curves ) -> first();

    snlCurve* curveCopy;

    while ( trimCurve )
    {
        curveCopy = new snlCurve ( *trimCurve );

        addTrimCurve ( curveCopy );

        trimCurve = ( surfaceToCopy.trim_curves ) -> next();
    }
}

snlSurface& snlSurface::operator= ( const snlSurface& surfaceToCopy )
{
    //! Assignment Operator.
    //  --------------------

    if ( this != &surfaceToCopy )
    {
        // If surface already contains data then will have to delete it.

        if ( ctrlPtNet ) delete ctrlPtNet;
        if ( knotVectU ) delete knotVectU;
        if ( knotVectV ) delete knotVectV;

        trim_curves -> clear();

        copyFrom ( surfaceToCopy );
    }

    return *this;
}

void snlSurface::genBilinearCoons ( snlCurve* curve_U1, snlCurve* curve_U2, snlCurve* curve_V1, snlCurve* curve_V2 )
{
    //! Generate Bilinear Coons Patch.
    //  ------------------------------
    //! @param curve_U1 First curve in U direction.
    //! @param curve_U2 Second curve in U direction.
    //! @param curve_V1 First curve in V direction.
    //! @param curve_V2 Second curve in V direction.
    //!
    //! @par Notes:
    //!      The curves must form a closed but non-sequential loop.
    //! @verbatim
    //!         V1
    //!      -------->
    //!     |         |
    //! U1  |         |  U2
    //!     |         |
    //!     V         V
    //!      -------->
    //!         V2              @endverbatim

    // Create Bilinear surface as base of this surface.

    knotVectV = new snlKnotVector ( 0.0, 1.0, 4, 1 );
    knotVectU = new snlKnotVector ( 0.0, 1.0, 4, 1 );

    snlCtrlPoint* ctrlPts = new snlCtrlPoint [ 4 ];

    ctrlPts [ 0 ] = ( curve_V1 -> controlPointNet() ).getPoint ( 0 );
    ctrlPts [ 1 ] = ( curve_V1 -> controlPointNet() ).getPoint ( ( curve_V1 -> controlPointNet() ).size() - 1 );
    ctrlPts [ 2 ] = ( curve_V2 -> controlPointNet() ).getPoint ( 0 );
    ctrlPts [ 3 ] = ( curve_V2 -> controlPointNet() ).getPoint ( ( curve_V2 -> controlPointNet() ).size() - 1 );

    degU = 1;
    degV = 1;

    ctrlPtNet = new snlCtrlPointNetSurface ( ctrlPts, 2, 2, false );

    // Make sure curve pairs are compatible.

    curve_U1 -> makeCompatible ( curve_U2 );
    curve_V1 -> makeCompatible ( curve_V2 );

    // Create U and V direction surfaces.

    snlSurface* surf_U = new snlSurface ( *curve_U1, *curve_U2, (int) SNL_U_DIR );
    snlSurface* surf_V = new snlSurface ( *curve_V1, *curve_V2, (int) SNL_V_DIR );

    // Surfaces need to be compatible with each other.

    surf_U -> makeCompatible ( surf_V, SNL_U_DIR );
    surf_U -> makeCompatible ( surf_V, SNL_V_DIR );

    makeCompatible ( surf_V, SNL_U_DIR );
    makeCompatible ( surf_V, SNL_V_DIR );

    makeCompatible ( surf_U, SNL_U_DIR );
    makeCompatible ( surf_U, SNL_V_DIR );

    // Generate new surface as addition of control points; surf_U + surf_V - bilinear ( this ).

    *( surf_U -> ctrlPtNet ) += *( surf_V -> ctrlPtNet );
    *( surf_U -> ctrlPtNet ) -= *ctrlPtNet;

    snlCtrlPointNetSurface* newCtrlPtNet = surf_U -> ctrlPtNet;  // Swap control point net into this.
    surf_U -> ctrlPtNet = ctrlPtNet;
    ctrlPtNet = newCtrlPtNet;

    // Clean Up.

    delete surf_U;
    delete surf_V;
    
}

knot* snlSurface::globalInterpGenParams ( int type, snlPoint* points, int sizeU, int sizeV,
                                          int dir )
{
    //! Generate parameters for global interpolation.
    //  ---------------------------------------------
    //! @param type Type of global interpolation.
    //! @param points Array points to interpolate. [ U ][ V ].
    //! @param sizeU Array size in U direction.
    //! @param sizeV Array size in V direction.
    //! @param dir Point array direction to generate parameters for. See enum parametricDirections.
    //!
    //! @return 1D Array of knots that holds parametric positions in chosen direction. All
    //!         lines in dir have exactly the same parametric sequence. Hence this array is 1D.

    int     size, oSize;

    if ( dir == SNL_U_DIR )
    {
        size = sizeU;
        oSize = sizeV;
    }
    else
    {
        size = sizeV;
        oSize = sizeU;
    }

    knot* chordLengths = new knot [ ( size - 1 ) * oSize ];
    
    knot* totalChordLengths = new knot [ oSize ];

    for ( int index = 0; index < oSize; index ++ )
        totalChordLengths [ index ] = 0.0;
    
    snlVector chord;

    chord.homogeneous = true;

    // Intermediate step. Calculate (square root of) chord length.

    int chordIndex = 0;
    
    for ( int cIndex = 1; cIndex < size; cIndex ++ )
    {
        int     index;
        
        if ( dir == SNL_U_DIR )
            index = cIndex * sizeV;
        else
            index = cIndex;
        
        for ( int oIndex = 0; oIndex < oSize; oIndex ++ )
        {
            if ( dir == SNL_U_DIR )
            {
                chord.calc ( points [ index - sizeV ], points [ index ] );
                index ++;
            }
            else
            {
                chord.calc ( points [ index - 1 ], points [ index ] );
                index += sizeV;
            }
            
            knot chordLength;
            
            if ( type == SNL_GLOBAL_INTERP_CENTRIFUGAL )
                chordLength = sqrt ( chord.length() );
            else        
                chordLength = chord.length();
                
            totalChordLengths [ oIndex ] += chordLength;

if ( chordLength == 0.0 )
{
    cout << "Zero Chord Length @: " << cIndex << ", " << oIndex << "\n";
    cout << "start pt: "; points [ index - sizeV - 1 ].print(); cout << "\n";
    cout << "end pt: "; points [ index - 1 ].print(); cout << "\n";
}
            
            chordLengths [ chordIndex ++ ] = chordLength;
        }
    }

    // Calculate final parameter values.

    knot* params = new knot [ size ];
    
    params [ 0 ] = 0.0;
    params [ size - 1 ] = 1.0;

    chordIndex = 0;
    
    for ( int cIndex = 1; cIndex < size - 1; cIndex ++ )
    {
        params [ cIndex ] = 0.0;
        
        for ( int oIndex = 0; oIndex < oSize; oIndex ++ )
        {
            // Sum across all chords.
            params [ cIndex ] += chordLengths [ chordIndex ++ ] / totalChordLengths [ oIndex ];
        }

        params [ cIndex ] /= (double) oSize;

        params [ cIndex ] += params [ cIndex - 1 ];
    }

    delete[] totalChordLengths;

    delete[] chordLengths;

    return params;
}

void snlSurface::genGlobalInterpSurf ( int interpType, snlPoint* pointsInterp, int sizeU, int sizeV,
                                       int degreeU, int degreeV )
{
    //! Generate surface by globally interpolating point data.
    //  ------------------------------------------------------
    //! @param interpType Type of interpolation. Comes from enum SNL_INTERP_TYPES.
    //! @param pointsInterp Points to interpolate. Point array.
    //! @param sizeU Size of point array dimension in U direction.
    //! @param sizeV Size of point array dimension in V direction.
    //! @param degreeU Degree of U direction.
    //! @param degreeV Degree of V direction.

    degU = degreeU;
    degV = degreeV;

    // Generate parameters that correspond to given points.

    knot* paramsU = globalInterpGenParams ( interpType, pointsInterp, sizeU, sizeV, SNL_U_DIR );
    knot* paramsV = globalInterpGenParams ( interpType, pointsInterp, sizeU, sizeV, SNL_V_DIR );

    // Generate knot vectors.
    
    knotVectU = new snlKnotVector ( sizeU, degreeU, paramsU );
    knotVectV = new snlKnotVector ( sizeV, degreeV, paramsV );

    // Generate intermediate control points using U direction.

    int numPts = sizeU * sizeV;

    snlPoint* intermPts = new snlPoint [ numPts ];  // Intermediate points.

    snlPoint* curvePtsU = new snlPoint [ sizeU ];

    for ( int indexV = 0; indexV < sizeV; indexV ++ )
    {
        // Find points for intermediate curve.

        int curvePtsIndex = 0;

        for ( int index = indexV; index < numPts; index += sizeV )
            curvePtsU [ curvePtsIndex ++ ] = pointsInterp [ index ];

        snlCtrlPoint* interpPoints = snlCurve::genGlobalInterpPoints ( curvePtsU, sizeU, paramsU, knotVectU );

        // Place interpolted points into intermediate array.

        curvePtsIndex = 0;

        for ( int index = indexV; index < numPts; index += sizeV )
            intermPts [ index ] = interpPoints [ curvePtsIndex ++ ];

        delete[] interpPoints;
    }

    // Generate final control points.

    snlCtrlPoint* finalPts = new snlCtrlPoint [ numPts ];

    snlPoint* curvePtsV = new snlPoint [ sizeV ];

    for ( int baseIndex = 0; baseIndex < numPts; baseIndex += sizeV )
    {
        // Each line in V direction is based at baseIndex.
        
        int maxVLineIndex = baseIndex + sizeV;

        // Find isoparametric curve points to evaluate.

        int curvePtsIndex = 0;
        
        for ( int vLineIndex = baseIndex; vLineIndex < maxVLineIndex; vLineIndex ++ )
            curvePtsV [ curvePtsIndex ++ ] = intermPts [ vLineIndex ];

        snlCtrlPoint* interpPoints = snlCurve::genGlobalInterpPoints ( curvePtsV, sizeV, paramsV, knotVectV );

        // Place interpolated points into final control point array.

        curvePtsIndex = 0;

        for ( int vLineIndex = baseIndex; vLineIndex < maxVLineIndex; vLineIndex ++ )
            finalPts [ vLineIndex ] = interpPoints [ curvePtsIndex ++ ];

        delete[] interpPoints;
    }

    // Generate Control Point Net.

    ctrlPtNet = new snlCtrlPointNetSurface ( finalPts, sizeU, sizeV, false );  // CtrlPtNet _owns_ finalPts array.

    // Clean up.

    delete[] intermPts;
    delete[] curvePtsU;
    delete[] curvePtsV;
    delete[] paramsU;
    delete[] paramsV;
}

int snlSurface::degreeU() const
{
    //! Return degree of surface in U direction.
    //  ----------------------------------------
    
    return degU;
}

int snlSurface::degreeV() const
{
    //! Return degree of surface in V direction.
    //  ----------------------------------------
    
    return degV;
}
        
unsigned snlSurface::sizeU() const
{
    //! Return size of control point array in U direction.
    //  --------------------------------------------------
    
    return ctrlPtNet -> getSizeU();
}

unsigned snlSurface::sizeV() const
{
    //! Return size of control point array in V direction.
    //  --------------------------------------------------
    
    return ctrlPtNet -> getSizeV();
}


knot snlSurface::minU()
{
    //! Return minimum U parameter.
    //  ---------------------------

    return knotVectU -> min();
}

knot snlSurface::maxU()
{
    //! Return maximum U parameter.
    //  ---------------------------

    return knotVectU -> max();
}

knot snlSurface::minV()
{
    //! Return minimum V parameter.
    //  ---------------------------

    return knotVectV -> min();
}

knot snlSurface::maxV()
{
    //! Return maximum V parameter.
    //  ---------------------------

    return knotVectV -> max();
}
        
const snlCtrlPoint* snlSurface::controlPoints()
{
    //! Return pointer to array of surfaces' control points.
    //  ----------------------------------------------------
    
    return ctrlPtNet -> getCtrlPts();
}
        
const knot* snlSurface::knotsU()
{
    //! Return pointer to array of knots in U direction.
    //  ------------------------------------------------
    
    return knotVectU -> array();
}

const knot* snlSurface::knotsV()
{
    //! Return pointer to array of knots in V direction.
    //  ------------------------------------------------
    
    return knotVectV -> array();
}

snlCtrlPointNetSurface& snlSurface::controlPointNet()
{
    //! Return reference to control point network object for surface.
    //  -------------------------------------------------------------
    
    return *ctrlPtNet;
}

const snlKnotVector& snlSurface::knotVectorU()
{
    //! Return pointer to Knot vector in U direction.
    //  ---------------------------------------------
    
    return *knotVectU;
}

const snlKnotVector& snlSurface::knotVectorV()
{
    //! Return pointer to Knot vector in U direction.
    //  ---------------------------------------------
    
    return *knotVectV;
}

snlPoint snlSurface::evalHmg ( knot paramU, knot paramV, basis* basisU, basis* basisV ) const
{
    //! Evaluate Non Rational Homogeneous Surface Point.
    //  ------------------------------------------------
    //! @param paramU Parameter in U direction to evaluate.
    //! @param paramV Parameter in V direction to evaluate.
    //! @param basisU Supplied basis function values. Must be 0 if not supplied.
    //! @param basisV Supplied basis function values. Must be 0 if not supplied.
    //!
    //! @return Homogeneous point on surface.


    snlPoint      rPoint;  // Return point.
    snlPoint      iPoint;  // Intermediate point.
    
    unsigned    vStart;  // The index where the V "line" starts in the control point array.
    
    unsigned spanU = knotVectU -> findSpan ( paramU );
    unsigned spanV = knotVectV -> findSpan ( paramV );    
    
    // Evaluate basis functions.

    basis* bValsU;

    if ( basisU )
        bValsU = basisU;
    else
        bValsU = knotVectU -> evalBasis ( paramU );

    basis* bValsV;
        
    if ( basisV )
        bValsV = basisV;
    else
        bValsV = knotVectV -> evalBasis ( paramV );

    unsigned baseVIndex = spanV - (unsigned) degV;  // Where in the V dimension the processing starts.

    rPoint.w ( 0 );
    
    // Get control point array.
    const snlCtrlPoint* ctrlPts = ctrlPtNet -> getCtrlPts();

    for ( int indexU = 0; indexU <= degU; indexU ++ )
    {
        iPoint.null();  // Set everything to zero.

        vStart = ( spanU - (unsigned) degU + indexU ) * sizeV();

        for ( int indexV = 0; indexV <= degV; indexV ++ )
        {
            snlPoint tmpPoint = ( ctrlPts [ vStart + baseVIndex + indexV ] ) * bValsV [ indexV ];
            iPoint += tmpPoint;
        }
        
        snlPoint tmpPoint = iPoint * bValsU [ indexU ];
        rPoint += tmpPoint;
    }
    
    if ( ! basisU ) delete[] bValsU;
    if ( ! basisV ) delete[] bValsV;

    return rPoint;
}

snlPoint snlSurface::eval ( knot paramU, knot paramV, basis* basisU, basis* basisV ) const
{
    //! Evaluate rational non-homogeneous surface point.
    //  ------------------------------------------------
    //! @param paramU Parameter in U direction to evaluate.
    //! @param paramV Parameter in V direction to evaluate.
    //! @param basisU Supplied basis function values. Must be 0 if not supplied.
    //! @param basisV Supplied basis function values. Must be 0 if not supplied.
    //!
    //! @return Non-homogeneous point on surface.

    knot minU = knotVectU -> min();
    knot maxU = knotVectU -> max();
    knot minV = knotVectV -> min();
    knot maxV = knotVectV -> max();

    // Clamp parameters.

    if ( paramU > maxU )
    {
        cout.precision ( 16 );
        //debug cout << "Surface Eval. Out of bounds U: " << paramU << "  Min: " << minU << "  Max: " << maxU << "\n";
        paramU = maxU;
    }
     
    if ( paramU < minU )
    {
        cout.precision ( 16 );
        //debug cout << "Surface Eval. Out of bounds U: " << paramU << "  Min: " << minU << "  Max: " << maxU << "\n";
        paramU = minU;
    }

    if ( paramV > maxV )
    {
        cout.precision ( 16 );
        //debug cout << "Surface Eval. Out of bounds V: " << paramV << "  Min: " << minV << "  Max: " << maxV << "\n";
        paramV = maxV;
    }
     
    if ( paramV < minV )
    {
        cout.precision ( 16 );
        //debug cout << "Surface Eval. Out of bounds V: " << paramV << "  Min: " << minV << "  Max: " << maxV << "\n";
        paramV = minV;
    }
    
    snlPoint retPoint = evalHmg ( paramU, paramV, basisU, basisV );
    retPoint.normalise();
    
    return retPoint;
}

snlPoint snlSurface::eval ( knot paramU, knot paramV ) const
{
    //! Evaluate rational non-homogeneous surface point.
    //  ------------------------------------------------
    //! @param paramU Parameter in U direction to evaluate.
    //! @param paramV Parameter in V direction to evaluate.
    //!
    //! @return Non-homogeneous point on surface.
    //!
    //! @par Notes:
    //!      Function duplication necessary to satisfy parent abstract class.

    knot minU = knotVectU -> min();
    knot maxU = knotVectU -> max();
    knot minV = knotVectV -> min();
    knot maxV = knotVectV -> max();

    // Clamp parameters.

    if ( paramU > maxU )
    {
        cout.precision ( 16 );
        //debug cout << "Surface Eval. Out of bounds U: " << paramU << "  Min: " << minU << "  Max: " << maxU << "\n";
        paramU = maxU;
    }
     
    if ( paramU < minU )
    {
        cout.precision ( 16 );
        //debug cout << "Surface Eval. Out of bounds U: " << paramU << "  Min: " << minU << "  Max: " << maxU << "\n";
        paramU = minU;
    }

    if ( paramV > maxV )
    {
        cout.precision ( 16 );
        //debug cout << "Surface Eval. Out of bounds V: " << paramV << "  Min: " << minV << "  Max: " << maxV << "\n";
        paramV = maxV;
    }
     
    if ( paramV < minV )
    {
        cout.precision ( 16 );
        //debug cout << "Surface Eval. Out of bounds V: " << paramV << "  Min: " << minV << "  Max: " << maxV << "\n";
        paramV = minV;
    }

    snlPoint retPoint = evalHmg ( paramU, paramV );
    retPoint.normalise();
    
    return retPoint;
}

snlPoint* snlSurface::evalDerivsHmg ( knot paramU, knot paramV, unsigned derivU, unsigned derivV,
                                      basis* basisU, basis* basisV )
{                                             
    //! Evaluate Non Rational Homogeneous Surface Derivatives.
    //  ------------------------------------------------------
    //! @param paramU Parameter in U direction to evaluate at.
    //! @param paramV Parameter in V direction to evaluate at.
    //! @param derivU Derivative order to evaluate in U direction.
    //! @param derivV Derivative order to evaluate in V direction.
    //! @param basisU Pre-computed basis functions values.
    //! @param basisV Pre-computed basis functions values.
    //!
    //! @return Array of snlPoint [ derivU + 1 ] [ derivV + 1 ]. Calling function
    //!         must delete[] this array.

    const snlPoint* cPnt;
    snlPoint*       vPnts = new snlPoint [ derivV + 1 ];
    
    // Find spans
    unsigned spanU = knotVectU -> findSpan ( paramU );
    unsigned spanV = knotVectV -> findSpan ( paramV );    
    
    // Evaluate basis functions.

    basis* bValsU;
    basis* bValsV;
    
    if ( basisU )
        bValsU = basisU;
    else
        bValsU = knotVectU -> evalBasisDeriv ( paramU, derivU );
        
    if ( basisV )
        bValsV = basisV;
    else
        bValsV = knotVectV -> evalBasisDeriv ( paramV, derivV );

    unsigned baseVIndex = spanV - degV;  // Where in the V dimension the processing starts.
    
    unsigned evalPtsSize = ( derivU + 1 ) * ( derivV + 1 );
    
    snlPoint* evalPts = new snlPoint [ evalPtsSize ];

    // Set eval points net to 0.
    for ( unsigned index = 0; index < evalPtsSize; index ++ )    
        evalPts [ index ].null();  // Set x, y, z and w to zero.
        
    // Get control point array.
    const snlCtrlPoint* ctrlPts = ctrlPtNet -> getCtrlPts();

    // Just loop through control points that match non-zero basis funtions.

    for ( unsigned indexU = 0; indexU <= (unsigned) degU; indexU ++ )
    {
        unsigned vStart = ( spanU - degU + indexU ) * sizeV();

        // Zero vPnts array.
        for ( unsigned index = 0; index <= derivV; index ++ )        
            vPnts [ index ].null();

        // Utilise V direction basis values.
        for ( unsigned indexV = 0; indexV <= (unsigned) degV; indexV ++ )
        {
            // Find control point only once.
            cPnt = ctrlPts + vStart + baseVIndex + indexV;

            // Calculate all V deriv values based on this point.
            for ( unsigned dIndexV = 0; dIndexV <= derivV; dIndexV ++ )
            {
                // Calc index into basis derivs for V direction.
                unsigned vDerivStart = ( dIndexV * ( degV + 1 ) ) + indexV;
                
                snlPoint tmpPoint = ( *cPnt ) * bValsV [ vDerivStart ];
                vPnts [ dIndexV ] += tmpPoint;
            }
        }

        // Multiply U deriv basis values and add to eval array.

        for ( unsigned dIndexU = 0; dIndexU <= derivU; dIndexU ++ )
        {
            // Get basis value for current u deriv level.
            basis cBValU = bValsU [ dIndexU * ( degU + 1 ) + indexU ];

            for ( unsigned dIndexV = 0; dIndexV <= derivV; dIndexV ++ )
            {
                unsigned evalIndex = dIndexU * ( derivV + 1 ) + dIndexV;
                
                snlPoint tmpPoint = vPnts [ dIndexV ] * cBValU;
                evalPts [ evalIndex ] += tmpPoint;
            }
        }
    }

    delete[] vPnts;

    if ( ! basisU ) delete[] bValsU;
    if ( ! basisV ) delete[] bValsV;
    
    return evalPts;
}

snlPoint* snlSurface::evalDerivs ( knot paramU, knot paramV, unsigned derivU, unsigned derivV )
{
    //! Evaluate Rational Non-Homogeneous Surface Derivatives
    //  -----------------------------------------------------
    //! @param paramU Parameter in U direction to evaluate at.
    //! @param paramV Parameter in V direction to evaluate at.
    //! @param derivU Derivative order to evaluate in U direction.
    //! @param derivV Derivative order to evaluate in V direction.
    //!
    //! @return Array of snlPoint [ derivU + 1 ] [ derivV + 1 ]. Calling function must
    //!         delete[] array.
    //!
    //! @par Notes:
    //!      Follows theory, "The NURBS Book 2nd ed", page 136 equation 4.20.

    unsigned    kIndex, lIndex;  // Partial derivs in t and u directions respectively.
    unsigned    iIndex, jIndex;  // Current partial deriv levels for t and u directions respectively.
    unsigned    cDIndex;  // Current derivPts index.
    unsigned    kBaseIndex;  // Base k index into evalPts array.
    unsigned    kBaseIndex2;

    snlPoint    iPoint;  // Intermediate point. Holds x, y and z of homogeneous point.
    snlPoint    iPoint2;  // Another intermediate point.    

    // Get homogeneous derivatives.
    snlPoint* derivPts = evalDerivsHmg ( paramU, paramV, derivU, derivV );
    
    // Array for returning points in.
    snlPoint* evalPts = new snlPoint [ ( derivU + 1 ) * ( derivV + 1 ) ];    

    unsigned dSizeV = derivV + 1;

    for ( kIndex = 0; kIndex <= derivU; kIndex ++ )
    {
        kBaseIndex = kIndex * ( derivV + 1 );

        for ( lIndex = 0; lIndex <= derivV; lIndex ++ )
        {
            cDIndex = kIndex * dSizeV + lIndex;

            iPoint = derivPts [ cDIndex ];

            // 0th k order derivatives.
            for ( jIndex = 1; jIndex <= lIndex; jIndex ++ )
            {
                iPoint.x ( iPoint.x() - binCoefs::binCoefArray [ lIndex ] [ jIndex ] * derivPts [ jIndex ].w() *
                           evalPts [ kBaseIndex + lIndex - jIndex ].x() );

                iPoint.y ( iPoint.y() - binCoefs::binCoefArray [ lIndex ] [ jIndex ] * derivPts [ jIndex ].w() *
                           evalPts [ kBaseIndex + lIndex - jIndex ].y() );

                iPoint.z ( iPoint.z() - binCoefs::binCoefArray [ lIndex ] [ jIndex ] * derivPts [ jIndex ].w() *
                           evalPts [ kBaseIndex + lIndex - jIndex ].z() );
            }

            // 0th j order and ( k, j ) order derivatives.
            for ( iIndex = 1; iIndex <= kIndex; iIndex ++ )
            {
                kBaseIndex2 = ( kIndex - iIndex ) * ( derivV + 1 );

                cDIndex = iIndex * dSizeV;

                iPoint.x ( iPoint.x() - binCoefs::binCoefArray [ kIndex ] [ iIndex ] * derivPts [ cDIndex ].w() *
                           evalPts [ kBaseIndex2 + lIndex ].x() );
                           
                iPoint.y ( iPoint.y() - binCoefs::binCoefArray [ kIndex ] [ iIndex ] * derivPts [ cDIndex ].w() *
                           evalPts [ kBaseIndex2 + lIndex ].y() );
                           
                iPoint.z ( iPoint.z() - binCoefs::binCoefArray [ kIndex ] [ iIndex ] * derivPts [ cDIndex ].w() *
                           evalPts [ kBaseIndex2 + lIndex ].z() );

                iPoint2.null();

                for ( jIndex = 1; jIndex <= lIndex; jIndex ++ )
                {
                    iPoint2.x ( iPoint2.x() + binCoefs::binCoefArray [ lIndex ] [ jIndex ] *
                                derivPts [ cDIndex + jIndex ].w() *
                                evalPts [ kBaseIndex2 + lIndex - jIndex ].x() );
                                
                    iPoint2.y ( iPoint2.y() + binCoefs::binCoefArray [ lIndex ] [ jIndex ] *
                                derivPts [ cDIndex + jIndex ].w() *
                                evalPts [ kBaseIndex2 + lIndex - jIndex ].y() );
                                
                    iPoint2.z ( iPoint2.z() + binCoefs::binCoefArray [ lIndex ] [ jIndex ] *
                                derivPts [ cDIndex + jIndex ].w() *
                                evalPts [ kBaseIndex2 + lIndex - jIndex ].z() );
                }

                snlPoint tmpPoint = iPoint2 * binCoefs::binCoefArray [ kIndex ] [ iIndex ];
                iPoint -= tmpPoint;
            }
            
            evalPts [ kBaseIndex + lIndex ] = iPoint / ( derivPts [ 0 ].w() );
            evalPts [ kBaseIndex + lIndex ].w ( 0 );  // No delta in w component at any time.
        }
    }
    
    delete[] derivPts;
    
    evalPts [ 0 ].w ( 1.0 );  // First entry in array is not a derivative.
    
    return evalPts;
}

void snlSurface::velocities ( knot paramU, knot paramV, snlPoint& evalPoint, snlVector& velocityU, snlVector& velocityV,
                              basis* basisU, basis* basisV )
{
    //! Compute velocities in U and V directions.
    //  -----------------------------------------
    //! @param paramU U parameter to evaluate at.
    //! @param paramV V parameter to evaluate at.
    //! @param evalPoint Variable to return surface point at which velocity was evaluated.
    //! @param velocityU Varibale to return U directon velocity in.
    //! @param velocityV Varibale to return V directon velocity in.
    //! @param basisU Pre-computed basis function values.
    //! @param basisV Pre-computed basis funciton values.
    //!
    //! @par Notes:
    //!      This function is designed for speed not correctness.

    knot minU = knotVectU -> min();
    knot maxU = knotVectU -> max();
    knot minV = knotVectV -> min();
    knot maxV = knotVectV -> max();

    // Clamp parameters.

    if ( paramU > maxU )
    {
        cout.precision ( 16 );
        cout << "Surface Velocity Eval. Out of bounds U: " << paramU << "  Min: "
             << minU << "  Max: " << maxU << "\n";
        paramU = maxU;
    }
     
    if ( paramU < minU )
    {
        cout.precision ( 16 );
        cout << "Surface Velocity Eval. Out of bounds U: " << paramU << "  Min: " << minU
             << "  Max: " << maxU << "\n";
        paramU = minU;
    }

    if ( paramV > maxV )
    {
        cout.precision ( 16 );
        cout << "Surface Velocity Eval. Out of bounds U: " << paramV << "  Min: " << minU
             << "  Max: " << maxU << "\n";
        paramV = maxV;
    }
     
    if ( paramV < minV )
    {
        cout.precision ( 16 );
        cout << "Surface Velocity Eval. Out of bounds U: " << paramV << "  Min: " << minU
             << "  Max: " << maxU << "\n";
        paramV = minV;
    }

    // Get homogeneous velocities.
    
    snlPoint* derivPts = evalDerivsHmg ( paramU, paramV, 1, 1, basisU, basisV );

    basis wVal = derivPts [ 0 ].elements [ 3 ];

    derivPts [ 0 ].normalise();

    evalPoint = derivPts [ 0 ];

    basis uWVal = derivPts [ 2 ].elements [ 3 ];

    velocityU.elements [ 0 ] = ( derivPts [ 2 ].elements [ 0 ] - uWVal
                               * derivPts [ 0 ].elements [ 0 ] ) / wVal;
    velocityU.elements [ 1 ] = ( derivPts [ 2 ].elements [ 1 ] - uWVal
                               * derivPts [ 0 ].elements [ 1 ] ) / wVal;
    velocityU.elements [ 2 ] = ( derivPts [ 2 ].elements [ 2 ] - uWVal
                               * derivPts [ 0 ].elements [ 2 ] ) / wVal;
    
    basis vWVal = derivPts [ 1 ].elements [ 3 ];

    velocityV.elements [ 0 ] = ( derivPts [ 1 ].elements [ 0 ] - vWVal
                               * derivPts [ 0 ].elements [ 0 ] ) / wVal;
    velocityV.elements [ 1 ] = ( derivPts [ 1 ].elements [ 1 ] - vWVal
                               * derivPts [ 0 ].elements [ 1 ] ) / wVal;
    velocityV.elements [ 2 ] = ( derivPts [ 1 ].elements [ 2 ] - vWVal
                               * derivPts [ 0 ].elements [ 2 ] ) / wVal;

    // Clean up.

    delete[] derivPts;
}

void snlSurface::insertKnot ( knot iParam, int dir, bool reallocate )
{
    //! For a Surface: Insert a Knot into Knot Vector and Calculate new Control Points
    //  ------------------------------------------------------------------------------
    //! @param iParam Parameter value to insert.
    //! @param dir Direction to evaluate in. 0 = u, 1 = v.
    //! @param reallocate Reallocate memory for control points.
    //!
    //! @par Notes:
    //!      ctrlPts MUST have an additional point space allocated at the end of
    //!      each line in the array for the chosen direction.

    unsigned        count, index, lineIndex, offset;
    unsigned        cDeg, oDeg;   // Degree to be processed.
    snlKnotVector*  cKnts;  // Current knots
    snlKnotVector*  oKnts;  // Other knots.
    snlCtrlPoint    pt1, pt2;

    if ( dir == SNL_V_DIR )
    {
        cDeg = degV;
        oDeg = degU;
        cKnts = knotVectV;
        oKnts = knotVectU;
        
        ctrlPtNet -> growV ( 1, reallocate );
    }
    else
    {
        cDeg = degU;
        oDeg = degV;
        cKnts = knotVectU;
        oKnts = knotVectV;
        
        ctrlPtNet -> growU ( 1, reallocate );
    }
    
    // Span new knot belongs to.
    unsigned span = cKnts -> findSpan ( iParam );

    // Pre calculate alphas.
    double* alpha = new double [ cDeg ];

    for ( count = 0; count < cDeg; count ++ )
    {
        index = span - cDeg + 1 + count;
        alpha [ count ]  = ( iParam - ( cKnts -> val ( index ) ) )
                           / ( cKnts -> val ( index + cDeg ) - cKnts -> val ( index ) );
    }

    // Build temp array to store new array of control points in.
    snlCtrlPoint* tmpPts = new snlCtrlPoint [ cDeg ];
    
    // Get pointer to control points.
    snlCtrlPoint* ctrlPts = ctrlPtNet -> getCtrlPtsPtr();

    // Do for each "line" in direction of insertion.
    for ( lineIndex = 0; lineIndex < ( oKnts -> size() - oDeg - 1 ); lineIndex ++ )
    {
        // Calculate new control points.
        for ( count = 0; count < cDeg; count ++ )
        {
            index = span - cDeg + 1 + count;

            // Get first and second ctrl points to process with
            if ( dir == SNL_V_DIR )
            {
                // V direction.
                pt1 = ctrlPts [ ( lineIndex * sizeV() ) + index ];
                pt2 = ctrlPts [ ( lineIndex * sizeV() ) + index - 1 ];
            }
            else
            {
                // U direction.
                pt1 = ctrlPts [ ( index * sizeV() ) + lineIndex ];
                pt2 = ctrlPts [ ( ( index - 1 ) * sizeV() ) + lineIndex ];
            }

            tmpPts [ count ].x ( ( alpha[count] * pt1.x() ) + ( ( 1.0 - alpha[count] ) * pt2.x() ) );
            tmpPts [ count ].y ( ( alpha[count] * pt1.y() ) + ( ( 1.0 - alpha[count] ) * pt2.y() ) );
            tmpPts [ count ].z ( ( alpha[count] * pt1.z() ) + ( ( 1.0 - alpha[count] ) * pt2.z() ) );
            tmpPts [ count ].w ( ( alpha[count] * pt1.w() ) + ( ( 1.0 - alpha[count] ) * pt2.w() ) );
        }

        // Place new points into array.

        if ( dir == SNL_V_DIR )
        {
            // V direction insert
            offset = lineIndex * sizeV();

            // Copy non-altered control points forward one position at the end of the array.
            for ( count = sizeV() - 1; count > span; count -- )
                ctrlPts [ offset + count ] = ctrlPts [ offset + count - 1 ];

            // Copy new control points into array.
            for ( count = 0; count < cDeg; count ++ )
            {
                index = span - cDeg + 1 + count + offset;
                ctrlPts [ index ] = tmpPts [ count ];
            }
        }
        else
        {
            // U direction insert.

            // Copy non-altered control points forward one position at the end of the array.
            for ( count = sizeU() - 1; count > span; count -- )
            {
                index = count * sizeV() + lineIndex;
                ctrlPts [ index ] = ctrlPts [ index - sizeV() ];
            }

            // Copy new control points into array.
            for ( count = 0; count < cDeg; count ++ )
            {
                index = ( span - cDeg + 1 + count ) * sizeV() + lineIndex;
                ctrlPts [ index ] = tmpPts [ count ];
            }
        }
    }

    // Insert new knot into knot vector
    cKnts -> insertKnot ( iParam );

    delete[] tmpPts;
    delete[] alpha;
}

void snlSurface::insertKnot ( knot iParam, int dir, int numToInsert, bool reallocate )
{
    //! For a Surface: Insert a Knot into Knot Vector and Calculate new Control Points
    //  ------------------------------------------------------------------------------
    //! @param iParam Parameter value to insert.
    //! @param dir Direction to evaluate in. 0 = u, 1 = v.
    //! @param numToInsert Number of knots to insert into location.
    //! @param reallocate Reallocate memory for control points.
    //!
    //! @par Notes:
    //!      ctrlPts MUST have an additional point space allocated at the end of
    //!      each line in the array for the chosen direction.

    if ( numToInsert <= 0 )
    {
        cout << "Bad use of function: snlSurface::insertKnot. Can't insert " << numToInsert << " knots.\n";
        return;
    }
    
    int             count, index, lineIndex, offset;
    unsigned        cDeg, oDeg;   // Degree to be processed.
    snlKnotVector*  cKnts;  // Current knots
    snlKnotVector*  oKnts;  // Other knots.
    snlCtrlPoint    pt1, pt2;

    if ( dir == SNL_V_DIR )
    {
        cDeg = degV;
        oDeg = degU;
        cKnts = knotVectV;
        oKnts = knotVectU;
        
        ctrlPtNet -> growV ( numToInsert, reallocate );
    }
    else
    {
        cDeg = degU;
        oDeg = degV;
        cKnts = knotVectU;
        oKnts = knotVectV;
        
        ctrlPtNet -> growU ( numToInsert, reallocate );
    }
    
    // Span new knot belongs to.
    unsigned span = cKnts -> findSpan ( iParam );

    int multi = cKnts -> findMultiplicity ( iParam );  // Multiplicity of knot vector at param.

    // Pre calculate alphas.
    
    int numAlphas = cDeg - multi;
    
    double* alpha = new double [ numAlphas * numToInsert ];

    for ( int insertCount = 1; insertCount <= numToInsert; insertCount ++ )
    {
        for ( count = 0; count < numAlphas - insertCount + 1; count ++ )
        {
            index = span - cDeg + insertCount + count;

            int alphaIndex = count + ( ( insertCount - 1 ) * numAlphas );
            
            alpha [ alphaIndex ]  = ( iParam - ( cKnts -> val ( index ) ) )
                                    / ( cKnts -> val ( index + cDeg - insertCount + 1 )
                                      - cKnts -> val ( index ) );
        }
    }
    
    // Build temp array to store new array of control points in.

    int numNewPts = cDeg - multi + numToInsert - 1;
    
    snlCtrlPoint* tmpPts = new snlCtrlPoint [ numNewPts ];
    
    // Get pointer to control points.
    snlCtrlPoint* ctrlPts = ctrlPtNet -> getCtrlPtsPtr();

    // Do for each "line" in direction of insertion.
    for ( lineIndex = 0; lineIndex < (int)( oKnts -> size() - oDeg - 1 ); lineIndex ++ )
    {
        // Calculate new control points for first insertion.
        for ( count = 0; count < numAlphas; count ++ )
        {
            index = span - cDeg + 1 + count;

            // Get first and second ctrl points to process with
            if ( dir == SNL_V_DIR )
            {
                // V direction.
                pt1 = ctrlPts [ ( lineIndex * sizeV() ) + index ];
                pt2 = ctrlPts [ ( lineIndex * sizeV() ) + index - 1 ];
            }
            else
            {
                // U direction.
                pt1 = ctrlPts [ ( index * sizeV() ) + lineIndex ];
                pt2 = ctrlPts [ ( ( index - 1 ) * sizeV() ) + lineIndex ];
            }

            tmpPts [ count ].x ( ( alpha[count] * pt1.x() ) + ( ( 1.0 - alpha[count] ) * pt2.x() ) );
            tmpPts [ count ].y ( ( alpha[count] * pt1.y() ) + ( ( 1.0 - alpha[count] ) * pt2.y() ) );
            tmpPts [ count ].z ( ( alpha[count] * pt1.z() ) + ( ( 1.0 - alpha[count] ) * pt2.z() ) );
            tmpPts [ count ].w ( ( alpha[count] * pt1.w() ) + ( ( 1.0 - alpha[count] ) * pt2.w() ) );
        }

        // Calculate subsequent points for each insertion.

        for ( int insertCount = 0; insertCount < numToInsert - 1; insertCount ++ )
        {
            // Copy some last points calculated forward one position.

            for ( int copyCount = 0; copyCount < insertCount + 1; copyCount ++ )
                tmpPts [ numAlphas + insertCount - copyCount ] = tmpPts [ numAlphas + insertCount - copyCount - 1 ];
            
            // Calculate new points

            int alphaOffset = ( insertCount + 1 ) * numAlphas;

            index = numAlphas - 1;
            
            for ( count = numAlphas - insertCount - 2; count >= 0 ; count -- )
            {
                tmpPts [ index ].x ( ( alpha [ alphaOffset + count ] * tmpPts [ index ].x() )
                                     + ( ( 1.0 - alpha [ alphaOffset + count ] ) * tmpPts [ index - 1 ].x() ) );
                                     
                tmpPts [ index ].y ( ( alpha [ alphaOffset + count ] * tmpPts [ index ].y() )
                                     + ( ( 1.0 - alpha [ alphaOffset + count ] ) * tmpPts [ index - 1 ].y() ) );
                                     
                tmpPts [ index ].z ( ( alpha [ alphaOffset + count ] * tmpPts [ index ].z() )
                                     + ( ( 1.0 - alpha [ alphaOffset + count ] ) * tmpPts [ index - 1 ].z() ) );
                                     
                tmpPts [ index ].w ( ( alpha [ alphaOffset + count ] * tmpPts [ index ].w() )
                                     + ( ( 1.0 - alpha [ alphaOffset + count ] ) * tmpPts [ index - 1 ].w() ) );

                -- index;
            }
        }

        // Place new points into array.

        if ( dir == SNL_V_DIR )
        {
            // V direction insert
            offset = lineIndex * sizeV();

            // Copy non-altered control points forward.
            for ( count = sizeV() - 1; count > (int) ( span - multi ); count -- )
                ctrlPts [ offset + count ] = ctrlPts [ offset + count - numToInsert ];

            index = span - cDeg + 1 + offset;

            // Copy new control points into array.
            for ( count = 0; count < numNewPts; count ++ )
            {
                ctrlPts [ index ] = tmpPts [ count ];
                ++ index;
            }
        }
        else
        {
            // U direction insert.

            int backIndexOffset = sizeV() * numToInsert;

            index = ( sizeU() - 1 ) * sizeV() + lineIndex;

            // Copy non-altered control points forward one position at the end of the array.
            for ( count = sizeU() - 1; count > (int) ( span - multi ); count -- )
            {
                ctrlPts [ index ] = ctrlPts [ index - backIndexOffset ];
                index -= sizeV();
            }

            // Copy new control points into array.

            index = ( span - cDeg + 1 ) * sizeV() + lineIndex;
            
            for ( count = 0; count < numNewPts; count ++ )
            {
                ctrlPts [ index ] = tmpPts [ count ];
                index += sizeV();
            }
        }
    }

    // Insert new knot into knot vector
    cKnts -> insertKnot ( iParam, numToInsert );

    delete[] tmpPts;
    delete[] alpha;
}

double snlSurface::removeKnots ( int numKnots, unsigned removalIndex, int direction,
                                 double tolerance, bool reallocate )
{
    //! For a Surface: Remove multiple Knots from Knot Vector and Calculate new Control Points
    //  --------------------------------------------------------------------------------------
    //! @param numKnots Number of knots to remove.
    //! @param removalIndex Knot at index to remove.
    //! @param direction Direction to evaluate in. 0 = u, 1 = v.
    //! @param tolerance Maximum allowable error. A value of 0.0 means ignore.
    //! @param reallocate Reallocate memory for control points. If false then de-allocation of
    //!                   memory is left up to caller.
    //!
    //! @return Maximum error encountered.

    if ( numKnots < 1 ) return 0.0;
    
    double maxTol = 0.0;

    snlKnotVector* knotVect;

    if ( direction == SNL_U_DIR )
    {
        knotVect = knotVectU;
    }
    else
    {
        knotVect = knotVectV;
    }
    
    double param = knotVect -> val ( removalIndex );
    
    int multi = knotVect -> findMultiplicity ( removalIndex );
    
    int numToRemove = numKnots > multi ? multi : numKnots;
    
    for ( int count = 0; count < numToRemove; count ++ )
    {
        double tol = removeKnot ( removalIndex, direction, tolerance, reallocate );
        
        if ( tol > maxTol ) maxTol = tol;
        
        removalIndex = knotVect -> findSpan ( param );
    }
    
    return maxTol;   
}

double snlSurface::removeKnot ( unsigned rIndex, int dir, double tolerance, bool reallocate )
{
    //! For a Surface: Remove a Knot from Knot Vector and Calculate new Control Points
    //  ------------------------------------------------------------------------------
    //! @param rIndex Knot at index to remove.
    //! @param dir Direction to evaluate in. 0 = u, 1 = v.
    //! @param tolerance Maximum allowable error. A value of 0.0 means ignore.
    //! @param reallocate Reallocate memory for control points.
    //!
    //! @return Maximum error encountered.

    unsigned        count, index, lineIndex, offset;
    unsigned        cDeg, oDeg;   // Degree to be processed.
    snlKnotVector*  cKnts;  // Current knots
    snlKnotVector*  oKnts;  // Other knots.
    snlCtrlPoint    pt1;

    if ( dir == SNL_V_DIR )
    {
        cDeg = degV;
        oDeg = degU;
        cKnts = knotVectV;
        oKnts = knotVectU;
    }
    else
    {
        cDeg = degU;
        oDeg = degV;
        cKnts = knotVectU;
        oKnts = knotVectV;
    }

    knot rParam = cKnts -> val ( rIndex );

    // Span knot to be removed belongs to. This will always adjust the removal index to
    // point to a non-zero span. ie Multiplicity check.
    unsigned rSpan = cKnts -> findSpan ( rParam );

    // Find multiplicity of knot at index.
    unsigned multi = cKnts -> findMultiplicity ( rSpan );

    // Calculate the number of equations.
    unsigned numEqns = cDeg - multi + 1;

    // Pre calculate alphas.
    double* alpha = cKnts -> calcRemovalAlphas ( rSpan );    

    // Maximum error variable.
    double maxError = 0;

    // Build temp array to store new array of control points in.
    // First and last points are not new.
    snlCtrlPoint* tmpPts = new snlCtrlPoint [ numEqns + 1 ];
    
    // Copy control points. Only copy back if the tolerance is ok.
    
    const snlCtrlPoint* ctrlPtsToCopy = controlPoints();
    
    snlCtrlPoint* ctrlPts = new snlCtrlPoint [ sizeU() * sizeV() ];
    
    for ( unsigned index = 0; index < ( sizeU() * sizeV() ); index ++ )
        ctrlPts [ index ] = ctrlPtsToCopy [ index ];
        
    // Do for each "line" in direction of removal.
    for ( lineIndex = 0; lineIndex < ( oKnts -> size() - oDeg - 1 ); lineIndex ++ )
    {
        // Seed new points array.
        if ( dir == SNL_V_DIR )
            tmpPts [ 0 ] = ctrlPts [ ( lineIndex * sizeV() ) + ( rSpan - cDeg - 1 ) ];
        else
            tmpPts [ 0 ] = ctrlPts [ ( ( rSpan - cDeg - 1 ) * sizeV() ) + lineIndex ];

        // Calculate new control points.
        for ( count = 1; count <= numEqns; count ++ )
        {
            index = rSpan - cDeg + count - 1;

            // Get ctrl point to process with.
            if ( dir == SNL_V_DIR )
            {
                // U direction.
                pt1 = ctrlPts [ ( lineIndex * ( sizeV() ) ) + index ];
            }
            else
            {
                // T direction.
                pt1 = ctrlPts [ ( index * sizeV() ) + lineIndex ];
            }

            // Calculate new control point.

            tmpPts [ count ] =  pt1;
            tmpPts [ count ] -= tmpPts [ count - 1 ] * ( 1.0 - alpha [ count - 1 ] );
            tmpPts [ count ] /= alpha [ count - 1 ];
        }

        // Place new points into array.

        if ( dir == SNL_V_DIR )
        {
            // V direction removal.
            offset = lineIndex * sizeV();

            // Calculate maximum error.

            snlCtrlPoint original = ctrlPts [ rSpan - cDeg + offset + numEqns - 1 ];

            original.normalise();

            tmpPts [ numEqns ].normalise();

            snlVector errorVect ( original, tmpPts [ numEqns ] );

            double error = errorVect.length();

            if ( error > maxError ) maxError = error;

            // Copy non-altered control points backward one position at the end of the array.
            for ( count = rSpan - multi; count < sizeV(); count ++ )
                ctrlPts [ offset + count ] = ctrlPts [ offset + count + 1 ];

            // Copy new control points into array.
            for ( count = 0; count < ( numEqns - 1 ); count ++ )
            {
                index = rSpan - cDeg + count + offset;
                ctrlPts [ index ] = tmpPts [ count + 1 ];
            }
        }
        else
        {
            // U direction removal.

            // Calculate maximum error.

            snlCtrlPoint original = ctrlPts [ ( rSpan - cDeg + numEqns - 1 )  * sizeV() + lineIndex ];

            original.normalise();

            tmpPts [ numEqns ].normalise();

            snlVector errorVect ( original, tmpPts [ numEqns ] );

            double error = errorVect.length();

            if ( error > maxError ) maxError = error;

            // Copy non-altered control points backwards one position at the end of the line.
            for ( count = rSpan - multi; count > sizeU(); count ++ )
            {
                index = count * sizeV() + lineIndex;
                ctrlPts [ index ] = ctrlPts [ index + sizeV() ];
            }

            // Copy new control points into array.
            for ( count = 0; count < ( numEqns - 1 ); count ++ )
            {
                index = ( rSpan - cDeg + count ) * sizeV() + lineIndex;
                ctrlPts [ index ] = tmpPts [ count + 1 ];
            }
        }
    }

    // If maximum error was under tolerance then go ahead with removal.
    if ( maxError < tolerance || tolerance == 0.0 )
    {    
        // Remove knot from knot vector
        cKnts -> removeKnot ( rSpan );
        
        ctrlPtNet -> replacePoints ( ctrlPts );
        
        if ( dir == SNL_V_DIR )
            ctrlPtNet -> shrinkV();
        else
            ctrlPtNet -> shrinkU();        
    }

    delete[] tmpPts;
    delete[] alpha;

    return maxError;
}

unsigned snlSurface::createBezierSegments ( int dir, int** numKnotsAdded )
{
    //! Convert surface into Bezier segments.
    //  -------------------------------------
    //! @param dir Direction to process in. u = 0, v = 1.
    //! @param numKnotsAdded Pointer to pointer that should hold the number of knots added to
    //!                      each span.
    //!
    //! @return Number of Bezier segments present.

    unsigned        cDeg;   // Degree to be processed. Current degree, other degree.
    unsigned        cSize, oSize; // Number of control points in current and other direction.
    snlKnotVector*  cKnts;  // Current knots

    if ( dir == SNL_V_DIR )
    {
        cDeg = degV;
        cKnts = knotVectV;
        cSize = sizeV();
        oSize = sizeU();
    }
    else
    {
        cDeg = degU;
        cKnts = knotVectU;
        cSize = sizeU();
        oSize = sizeV();
    }

    // Find number of non-zero spans.
    unsigned numSpans = cKnts -> getNumSpans();

    if ( cDeg <= 1 ) return numSpans;  // 1st degree curve sections are already Bezier segments.
    
    int* knotsAdded = new int [ numSpans ];  // Last array element is unused and is left here so that a zero length array doesn't occur.

    // Find first spans knot index.
    unsigned knotIndex = cKnts -> getFirstSpan();

    // Resize control points array just once for all knot insertions.
    
    unsigned nextSpan = knotIndex;
    unsigned extraKnots = 0;
    
    // Find amount to resize by.
    for ( unsigned spanIndex = 1; spanIndex < numSpans; spanIndex ++ )
    {
        nextSpan = cKnts -> getNextSpan ( nextSpan );
        
        extraKnots += cDeg - ( cKnts -> findMultiplicity ( nextSpan ) );
    }    
    
    // Append extra control point space to end of current control points.
    ctrlPtNet -> appendPointSpace ( oSize * extraKnots );

    // *** Create Bezier segments ***
    
    // Find knot index of second knot span.
    nextSpan = cKnts -> getNextSpan ( knotIndex );
    
    for ( unsigned spanIndex = 0; spanIndex < numSpans - 1; spanIndex ++ )
    {        
        // Increase multiplicity of span to degree.

        unsigned multi = cKnts -> findMultiplicity ( nextSpan );

        if ( cDeg - multi > 0 )
        {
            knot insertParam = cKnts -> val ( nextSpan );

            insertKnot ( insertParam, dir, cDeg - multi, false );

            // Re-adjust current span index to account for inserted knots.
            nextSpan = cKnts -> getNextSpan ( cKnts -> findSpan ( insertParam ) );

            // Populate number of knots added array elements.
            knotsAdded [ spanIndex ] = cDeg - multi;
        }
        else
            nextSpan = cKnts -> getNextSpan ( nextSpan );
    }

    if ( numKnotsAdded )
        *numKnotsAdded = knotsAdded;
    else
        delete[] knotsAdded;
    
    return numSpans;
}

void snlSurface::createBezierSegments ( int* numU, int* numV )
{
    //! Convert surface into Bezier segments.
    //  -------------------------------------

    int numUSegments = createBezierSegments ( SNL_U_DIR );
    int numVSegments = createBezierSegments ( SNL_V_DIR );

    if ( numU ) *numU = numUSegments;
    if ( numV ) *numV = numVSegments;
}

void snlSurface::createConvexBezierSegments ( int* numU, int* numV, double sensitivity )
{
    //! Convert surface into convex Bezier segments.
    //  --------------------------------------------
    //! @param numU Pointer to variable to return number of U segments in.
    //! @param numV Pointer to variable to return number of V segments in.
    //! @param sensitivity Maximum concave angle allowed to be considered convex. Used to account
    //!                    for noise in the curvature of a relatively flat section.

    int numUSegments = createBezierSegments ( SNL_U_DIR );
    int numVSegments = createBezierSegments ( SNL_V_DIR );

    // Subdivide U direction until all segments are convex.

    int maxDeg = degU > degV ? degU : degV;

    snlCtrlPoint** testPoints = new snlCtrlPoint* [ maxDeg + 1 ];

    bool keepChecking = true;

    while ( keepChecking )
    {
        keepChecking = false;

        int sizeV = ctrlPtNet -> getSizeV();
    
        for ( int segment = 0; segment < numUSegments; segment ++ )
        {
            int indexU = segment * degU;
    
            bool convex = true;
    
            // Check all constant V lines for concave control point combinations.
    
            for ( int indexV = 0; indexV < sizeV; indexV ++ )
            {
                ctrlPtNet -> locatePointsU ( indexU, indexV, degU + 1, testPoints );
    
                if ( ! ( ctrlPtNet -> isConvex ( (snlPoint**) testPoints, degU + 1, sensitivity ) ) )
                {
                    convex = false;
                    break;
                }
            }
    
            // If any part in U direction is concave then subdivide all patches in range.
    
            if ( ! convex )
            {
                knot startKnot = knotVectU -> val ( ( segment + 1 ) * degU );
                knot endKnot = knotVectU -> val ( ( segment + 2 ) * degU );
    
                // Insert multiple knots into middle of patch to create two new Bezier segments.
                
                insertKnot ( ( ( endKnot - startKnot ) / 2.0 ) + startKnot, SNL_U_DIR, degU, true );
    
                numUSegments ++;
                segment ++;

                keepChecking = true;
            }
        }
    }

    // Subdivide V direction until all segments are convex.

    keepChecking = true;

    while ( keepChecking )
    {
        keepChecking = false;
        
        int sizeU = ctrlPtNet -> getSizeU();
    
        for ( int segment = 0; segment < numVSegments; segment ++ )
        {
            int indexV = segment * degV;
    
            bool convex = true;
    
            // Check all U lines for concave control point combinations.

            for ( int indexU = 0; indexU < sizeU; indexU ++ )
            {
                ctrlPtNet -> locatePointsV ( indexU, indexV, degV + 1, testPoints );

                if ( ! ( ctrlPtNet -> isConvex ( (snlPoint**) testPoints, degV + 1, sensitivity ) ) )
                {
                    convex = false;
                    break;
                }
            }

            // If any part in V direction is concave then subdivide all patches in range.
    
            if ( ! convex )
            {
                knot startKnot = knotVectV -> val ( ( segment + 1 ) * degV );
                knot endKnot = knotVectV -> val ( ( segment + 2 ) * degV );
    
                // Insert multiple knots into middle of patch to create two new Bezier segments.
                
                insertKnot ( ( ( endKnot - startKnot ) / 2.0 ) + startKnot, SNL_V_DIR, degV, true );
    
                numVSegments ++;
                segment ++;

                keepChecking = true;
            }
        }
    }

    if ( numU ) *numU = numUSegments;
    if ( numV ) *numV = numVSegments;

    delete[] testPoints;
}

void snlSurface::elevateDegree ( int direction, int byDegree )
{
    //! Elevate degree of surface.
    //  --------------------------
    //! @param direction Parametric direction to elevate degree in. ( enum parametricDirections ).
    //! @param byDegree Number of degrees to elevate direction by.
    //!                 Convert curve into Bezier segments.
    
    int* addedKnots = 0;
    
    unsigned numSegments = createBezierSegments ( direction, &addedKnots );

    // Grow control point net.

    if ( direction == SNL_U_DIR )
        ctrlPtNet -> growU ( numSegments * byDegree, true );
    else
        ctrlPtNet -> growV ( numSegments * byDegree, true );

    // Setup direction specific variables.

    int cDeg, numLines, lineSize;
    snlKnotVector* cKnotVect;

    if ( direction == SNL_U_DIR )
    {
        cDeg = degU;
        numLines = ctrlPtNet -> getSizeV();
        lineSize = ctrlPtNet -> getSizeU();
        cKnotVect = knotVectU;
    }
    else
    {
        cDeg = degV;
        numLines = ctrlPtNet -> getSizeU();
        lineSize = ctrlPtNet -> getSizeV();
        cKnotVect = knotVectV;
    }

    // Elevate degree of Bezier segments.
    
    int segmentSize = cDeg + 1;
    int newSegmentSize = segmentSize + byDegree;
    
    snlCtrlPoint* tmpPts = new snlCtrlPoint [ newSegmentSize ];

    snlCtrlPoint** linePtPtrs = new snlCtrlPoint* [ lineSize ];

    snlCtrlPoint* linePts = new snlCtrlPoint [ lineSize ];
    
    for ( int lineIndex = 0; lineIndex < numLines; lineIndex ++ )
    {
        // Generate new points per segment.
        
        int ptsIndex = 0;
        
        unsigned spanIndex = cDeg * 2;

        if ( direction == SNL_U_DIR )
        {
            // Get line in U direction.
            ctrlPtNet -> locatePointsU ( 0, lineIndex, lineSize, linePtPtrs );
        }
        else
        {
            // Get line in V direction.
            ctrlPtNet -> locatePointsV ( lineIndex, 0, lineSize, linePtPtrs );
        }

        // Elevate segments.
        
        for ( unsigned segment = 0; segment < numSegments; segment ++ )
        {
            // Populate control points to process for line.

            int segmentStartIndex = segment * ( segmentSize - 1 );
    
            for ( int index = 0; index < segmentSize; index ++ )
            {
                linePts [ ptsIndex + index ] = *( linePtPtrs [ segmentStartIndex + index ] );
            }

            // Process segment.
        
            snlCurve::elevateBezierSegmentPointsDegree ( cDeg, byDegree, linePts + ptsIndex, tmpPts );
        
            // Replace points in temp control point array. Index 0 remains unchanged.
            for ( int index = 1; index < newSegmentSize; index ++ )
                linePts [ ptsIndex + index ] = tmpPts [ index ];
        
            ptsIndex += cDeg + byDegree;
        
            if ( lineIndex == 0 )
            {
                // Add knots to knot vector. This is only done _once_.
                cKnotVect -> increaseMultiplicity ( spanIndex, byDegree );
            }
        
            spanIndex += cDeg + byDegree;
        }

        // Copy temp points into control point net.

        for ( int index = 0; index < lineSize; index ++ )
            *( linePtPtrs [ index ] ) = linePts [ index ];
    }

    // Make sure start clamp is of degree + 1 multiplicity.
    
    cKnotVect -> increaseMultiplicity ( cDeg, byDegree );
    
    // Increase degree indicator variables
    
    cDeg += byDegree;
    
    cKnotVect -> degree ( cDeg );

    if ( direction == SNL_U_DIR )
        degU = cDeg;
    else
        degV = cDeg;
    
    // Remove number of knots that were added during knot insertion.
    // No knots were added to degree one sections.

    if ( cDeg - byDegree > 1 )
    {
        unsigned spanIndex = cKnotVect -> getFirstSpan();
        
        spanIndex += cDeg;
        
        for ( unsigned segment = 0; segment < numSegments - 1; segment ++ )
        {
            removeKnots ( addedKnots [ segment ], spanIndex, direction, 0.0, true );
        
            spanIndex += cDeg - addedKnots [ segment ];
        }
    }

    // Clean up.

    if ( addedKnots ) delete[] addedKnots;
    delete[] tmpPts;
    delete[] linePtPtrs;
    delete[] linePts;
}

double snlSurface::reduceDegree ( int dir, unsigned numDeg, double tolerance )
{
    //! Reduce Surface Degree
    //  ---------------------
    //! @param dir Direction to evaluate in. 0 = u, 1 = v.
    //! @param numDeg Number of degrees to reduce by.
    //! @param tolerance Maximum error. A value of 0.0 means no tolerance specified.
    //!
    //! @return Maximum error encountered.
    //!
    //! @par Notes:
    //!      This function has not been optimised.

    unsigned        cDeg, oDeg;   // Degree to be processed. Current degree, other degree.
    unsigned        cSize, oSize; // Number of control points in current and other direction.
    snlKnotVector*  cKnts;  // Current knots
    snlKnotVector*  oKnts;  // Other knots.

    double maxError = 0.0;
        
    // Save knots and control points of original surface.
    
    snlCtrlPointNetSurface* ctrlPtNetCopy = new snlCtrlPointNetSurface ( *ctrlPtNet );
       
    snlKnotVector* knotVectUCopy = new snlKnotVector ( *knotVectU );
    snlKnotVector* knotVectVCopy = new snlKnotVector ( *knotVectV );

    // Convert into Bezier segments.    
    unsigned numSpans = createBezierSegments ( dir );
    
    if ( dir == SNL_V_DIR )
    {
        cDeg = degV;
        oDeg = degU;
        cKnts = knotVectV;
        oKnts = knotVectU;
        cSize = sizeV();
        oSize = sizeU();
    }
    else
    {
        cDeg = degU;
        oDeg = degV;
        cKnts = knotVectU;
        oKnts = knotVectV;
        cSize = sizeU();
        oSize = sizeV();
    }

    // *** Reduce degree of Bezier segments ***
    
    snlCtrlPoint* ctrlPts = ctrlPtNet -> getCtrlPtsPtr();
    
    for ( unsigned count = 0; count < numDeg; count ++ )
    {    
        unsigned rDeg = cDeg - count;  // Current degree during reduction.
        
        // Pre-calculate alphas.
        double* alpha = new double [ rDeg - 1 ];
                
        for ( unsigned index = 1; index < rDeg; index ++ )
            alpha [ index - 1 ] = (double) index / (double) rDeg;        
        
        for ( unsigned patchIndex = 0; patchIndex < numSpans; patchIndex ++ )
        {
            // Process up each "line" in the direction to be reduced.
            for ( unsigned lineIndex = 0; lineIndex < oSize; lineIndex ++ )        
            {                
                // Reduce patch.
                
                snlCtrlPoint*   cPnt;  // Current point.
                snlCtrlPoint*   pPnt;  // Previous point.
                
                // Get starting and previous point's pointer.
                
                if ( dir == SNL_V_DIR )
                {
                    // V Direction.
                    pPnt = ctrlPts + (unsigned)( ( lineIndex * cSize ) + ( patchIndex * rDeg ) );
                    cPnt = pPnt + 1;
                }
                else
                {
                    // U Direction.
                    pPnt = ctrlPts + (unsigned)( ( patchIndex * rDeg * oSize ) + lineIndex );
                    cPnt = pPnt + sizeV();
                }
                
                snlCtrlPoint newPoint;
                
                // Caculate new internal patch points. ie First and last points aren't modified.
                
                for ( unsigned pIndex = 1; pIndex < rDeg; pIndex ++ )
                {
                    if ( pIndex > 1 )
                        *pPnt = newPoint;
                    
                    newPoint.x ( ( cPnt -> x() - ( ( pPnt -> x() ) * alpha [ pIndex - 1 ] ) )
                                 / ( 1 - alpha [ pIndex - 1 ] ) );
                    newPoint.y ( ( cPnt -> y() - ( ( pPnt -> y() ) * alpha [ pIndex - 1 ] ) )
                                 / ( 1 - alpha [ pIndex - 1 ] ) );
                    newPoint.z ( ( cPnt -> z() - ( ( pPnt -> z() ) * alpha [ pIndex - 1 ] ) )
                                 / ( 1 - alpha [ pIndex - 1 ] ) );
                    newPoint.w ( ( cPnt -> w() - ( ( pPnt -> w() ) * alpha [ pIndex - 1 ] ) )
                                 / ( 1 - alpha [ pIndex - 1 ] ) );                    
                    
                    // Find next points.
                    pPnt = cPnt;
                    
                    if ( dir == SNL_V_DIR )
                        // V direction.
                        cPnt ++;
                    else
                        // U direction
                        cPnt += sizeV();
                }                
                
                // Calculate error.
                                
                snlCtrlPoint cPntCopy ( *cPnt );
                cPntCopy.normalise();
                
                newPoint.normalise();
                
                double cError = ( snlVector ( cPntCopy, newPoint ).length() );
                
                if ( cError > maxError ) maxError = cError;        
            }
            
            // Adjust knot vector.
            cKnts -> removeKnot ( ( patchIndex * ( rDeg - 1 ) ) + 1 );
        }
        
        // Reduce end clamp multiplicity.
        cKnts -> removeKnot ( ( cKnts -> size() ) - 1 );
        
        // Rearrange array to facilitate removal of redundant control points.
        
        snlCtrlPoint* copyFrom = ctrlPts;
        snlCtrlPoint* copyTo = ctrlPts;
        
        if ( dir == SNL_V_DIR )
        {        
            for ( unsigned lineIndex = 0; lineIndex < oSize; lineIndex ++ )
            {
                for ( unsigned patchIndex = 0; patchIndex < numSpans; patchIndex ++ )
                {
                    for ( unsigned count = 0; count < rDeg - 1; count ++ )
                    {
                        *copyTo = *copyFrom;
                        copyTo++;
                        copyFrom++;
                    }
                
                    copyFrom++;                                
                }
            
                *copyTo = *copyFrom;
                copyTo++;
                copyFrom++;
            }
        }
        else
        {
            // U direction.
            
            for ( unsigned patchIndex = 0; patchIndex < numSpans; patchIndex ++ )            
            {
                for ( unsigned lineIndex = 0; lineIndex < rDeg - 1; lineIndex ++ )
                {
                    for ( unsigned count = 0; count < oSize; count ++ )
                    {
                        *copyTo = *copyFrom;
                        copyTo++;
                        copyFrom++;
                    }
                }
                
                copyFrom += oSize;  // Skip a line.            
            }
            
            // Copy last line.            
            
            for ( unsigned count = 0; count < oSize; count ++ )
            {
                *copyTo = *copyFrom;
                copyTo++;
                copyFrom++;
            }            
        }
        
        // Adjust current new size to reflect control point removals.
        cSize -= numSpans;        
        
        // Clean up.        
        delete[] alpha;        
    }
        
    // Set control point net to correct size.
    ctrlPtNet -> truncatePointSpace ( numDeg * numSpans );
    
    if ( dir == SNL_V_DIR )
    {
        ctrlPtNet -> setSizeV ( cSize );
        degV -= numDeg;
    }
    else
    {
        ctrlPtNet -> setSizeU ( cSize );
        degU -= numDeg;
    }
    
    // Remove additional knots only if under tolerance.
        
    if ( ( tolerance != 0.0 ) && ( tolerance > maxError ) )
    {
        // !@#$ Simplify surface here.
    }
    
    if ( ( maxError < tolerance ) || ( tolerance == 0.0 ) )
    {
        // Can keep new knots and control points so delete original copies.
            
        delete ctrlPtNetCopy;   
        delete knotVectUCopy;
        delete knotVectVCopy;        
    }
    else
    {
        // Reinstate old knots and control points.
        
        delete ctrlPtNet;
        delete knotVectV;
        delete knotVectU;
        
        ctrlPtNet = ctrlPtNetCopy;
        knotVectV = knotVectVCopy;
        knotVectU = knotVectUCopy;

        if ( dir == SNL_V_DIR )
            degV += numDeg;
        else
            degU += numDeg;
    }
    
    return maxError;
}

void snlSurface::refine ( double tolerance )
{
    //! Refine surface control point net until tolerance is achieved.
    //  -------------------------------------------------------------
    //! @par Notes:
    //!      Guarantees that if a span is approximated by it's end points then the error will be no
    //!      greater than tolerance.

    // Refine in individual parametric directions on pass in each direction at a time.

    bool tolOk = false;

    while ( ! tolOk )
    {
        tolOk = true;

        if ( ! refineHull_U ( tolerance, true ) ) tolOk = false;
        if ( ! refineHull_V ( tolerance, true ) ) tolOk = false;
    }
}

void snlSurface::refineHull_UV ( double tolerance )
{
    //! Refine control point net until it is guaranteed be within tolerance to surface.
    //  -------------------------------------------------------------------------------
    //! @par Notes:
    //!      Processes rectangular sections which is more accurate than doing each parametric
    //!      direction in turn. This is also results in this funtion being VERY slow and extremely
    //!      memory hungry.

    if ( degU < 2 && degV < 2 ) return;

    if ( degU < 2 )
    {
        refineHull_V ( tolerance );
        return;
    }

    if ( degV < 2 )
    {
        refineHull_U ( tolerance );
        return;
    }

    bool tolOk = false;  // Tolerance is okay.

    while ( ! tolOk )
    {
        tolOk = true;

        // Control point net sizes must be obtained at each loop as they may change during the loop.

        // Only process non-zero knot vector spans.

        int numSpansU = knotVectU -> getNumSpans();
        int numSpansV = knotVectV -> getNumSpans();

        bool* spanSubU = new bool [ numSpansU ];  // Span subdivision indicators.
        bool* spanSubV = new bool [ numSpansV ];  // If true then subdivide span.

        knot* spanSubUVal = new double [ numSpansU ];  // Knot insertion value for span.
        knot* spanSubVVal = new double [ numSpansV ];

        for ( int index = 0; index < numSpansU; index ++ )
            spanSubU [ index ] = false;

        for ( int index = 0; index < numSpansV; index ++ )
            spanSubV [ index ] = false;

        int spanU = 0;

        int indexU = knotVectU -> getFirstSpan();

        while ( indexU )
        {
            int indexV = knotVectV -> getFirstSpan();

            int spanV = 0;

            while ( indexV )
            {
                // Don't check rectangular span if both and U and V are already marked for subdivision.
                
                if ( ! ( spanSubU [ spanU ] && spanSubV [ spanV ] ) )
                {
                    double flatness = ctrlPtNet -> calcFlatness ( indexU - degU, indexV - degV, degU + 1, degV + 1 );
    
                    if ( flatness > tolerance )
                    {
                        if ( ! spanSubU [ spanU ] )
                        {
                            spanSubUVal [ spanU ]  = ( ( knotVectU -> val ( indexU + 1 )
                                                       - knotVectU -> val ( indexU ) ) / 2 )
                                                       + knotVectU -> val ( indexU );
                        }

                        if ( ! spanSubV [ spanV ] )
                        {
                            spanSubVVal [ spanV ] = ( ( knotVectV -> val ( indexV + 1 )
                                                      - knotVectV -> val ( indexV ) ) / 2 )
                                                      + knotVectV -> val ( indexV );
                        }
                        
                        spanSubU [ spanU ] = true;
                        spanSubV [ spanV ] = true;
                        
                        tolOk = false;
                    }
                }

                indexV = knotVectV -> getNextSpan ( indexV );

                spanV ++;
            }

            indexU = knotVectU -> getNextSpan ( indexU );

            spanU ++;
        }

        // Insert Knots where needed.

        // Insert U knots.

        for ( int spanIndex = 0; spanIndex < numSpansU; spanIndex ++ )
        {
            if ( spanSubU [ spanIndex ] )
                insertKnot ( spanSubUVal [ spanIndex ], SNL_U_DIR );
        }

        // Insert V knots.

        for ( int spanIndex = 0; spanIndex < numSpansV; spanIndex ++ )
        {
            if ( spanSubV [ spanIndex ] )
                insertKnot ( spanSubVVal [ spanIndex ], SNL_V_DIR );
        }

        delete[] spanSubU;
        delete[] spanSubV;

        delete[] spanSubUVal;
        delete[] spanSubVVal;
    }
}

bool snlSurface::refineHull_U ( double tolerance, bool singlePass )
{
    //! Refine control point net in U direction until it is
    //! guaranteed be within tolerance to surface.
    //  ---------------------------------------------------
    //! @param tolerance Convex Hull of surface in U direction must be within tolerance of surface.
    //! @param singlePass Only do a single refinement pass.
    //!
    //! @return If tolerance is was okay. Should be true unless single pass is specified.
    
    if ( degU < 2 ) return true;
    
    bool tolOk = false;  // Tolerance is okay.
    
    while ( ! tolOk )
    {
        tolOk = true;

        // Only process non-zero knot vector spans.

        int numSpans = knotVectU -> getNumSpans();

        bool* spanSub = new bool [ numSpans ];  // Span subdivision indicators. If true then subdivide span.

        knot* spanSubVal = new double [ numSpans ];  // Knot insertion value for span.

        for ( int index = 0; index < numSpans; index ++ )
            spanSub [ index ] = false;

        for ( int indexV = 0; (unsigned) indexV < ( ctrlPtNet -> getSizeV() ); indexV ++ )
        {
            int indexU = knotVectU -> getFirstSpan();

            int spanNum = 0;
            
            while ( indexU )
            {
                // Test for flatness
            
                double  flatness;
                
                flatness = ctrlPtNet -> calcFlatnessU ( indexU - degU, indexV, degU + 1, false );

                if ( flatness > tolerance )
                {
                    // Insert knot into surface. Half way between existing knots.
                
                    int insertIndex = indexU;
                
                    knot insertParam = ( ( knotVectU -> val ( insertIndex + 1 )
                                         - knotVectU -> val ( insertIndex ) ) / 2 )
                                         + knotVectU -> val ( insertIndex );

                    spanSubVal [ spanNum ] = insertParam;

                    spanSub [ spanNum ] = true;

                    tolOk = false;
                }

                indexU = knotVectU -> getNextSpan ( indexU );

                spanNum ++;
            }
        }

        // Insert knots where needed to subdivide span.
        
        for ( int spanNum = 0; spanNum < numSpans; spanNum ++ )
        {
            if ( spanSub [ spanNum ] )
                insertKnot ( spanSubVal [ spanNum ], SNL_U_DIR );
        }
        
        delete[] spanSub;
        delete[] spanSubVal;

        if ( singlePass ) break;
    }

    return tolOk;
}

bool snlSurface::refineHull_V ( double tolerance, bool singlePass )
{
    //! Refine control point net in V direction until it is
    //! guaranteed be within tolerance to surface.
    // ---------------------------------------------------
    
    if ( degV < 2 ) return true;
    
    bool tolOk = false;
    
    while ( ! tolOk )
    {
        tolOk = true;

        // Only process non-zero knot vector spans.

        int numSpans = knotVectV -> getNumSpans();

        bool* spanSub = new bool [ numSpans ];  // Span subdivision indicators. If true then subdivide span.

        knot* spanSubVal = new double [ numSpans ];  // Knot insertion value for span.

        for ( int index = 0; index < numSpans; index ++ )
            spanSub [ index ] = false;

        for ( int indexU = 0; (unsigned) indexU < ( ctrlPtNet -> getSizeU() ); indexU ++ )
        {            
            int indexV = knotVectV -> getFirstSpan();

            int spanNum = 0;
            
            while ( indexV )
            {
                // Test for flatness
            
                double  flatness;
                
                flatness = ctrlPtNet -> calcFlatnessV ( indexU, indexV - degV, degV + 1, false );

                if ( flatness > tolerance )
                {
                    // Insert knot into surface. Half way between existing knots.
                
                    int insertIndex = indexV;
                
                    knot insertParam = ( ( knotVectV -> val ( insertIndex + 1 )
                                         - knotVectV -> val ( insertIndex ) ) / 2 )
                                         + knotVectV -> val ( insertIndex );

                    spanSubVal [ spanNum ] = insertParam;

                    spanSub [ spanNum ] = true;

                    tolOk = false;
                }
                
                indexV = knotVectV -> getNextSpan ( indexV );

                spanNum ++;
            }
        }

        // Insert knots where needed to subdivide span.
        
        for ( int spanNum = 0; spanNum < numSpans; spanNum ++ )
        {
            if ( spanSub [ spanNum ] )
                insertKnot ( spanSubVal [ spanNum ], SNL_V_DIR );
        }

        delete[] spanSub;
        delete[] spanSubVal;

        if ( singlePass ) break;
    }

    return tolOk;
}

void snlSurface::refineHullBezier ( double tolerance )
{
    //! Refine control point net using Bezier patch subdivision.
    //  --------------------------------------------------------
    //! @param tolerance Control point net should be within this tolerance of surface.

    // Break surface into Bezier segments ( patches ).

    int numUSegments;
    int numVSegments;

    createBezierSegments ( &numUSegments, &numVSegments );

    // Walk through each segment and calculate it's flatness.

    bool tolOk = false;  // Tolerance has been achieved.

    while ( ! tolOk )
    {
        tolOk = true;

        numUSegments = knotVectU -> getNumSpans();
        numVSegments = knotVectV -> getNumSpans();

        bool* splitSpanU = new bool [ numUSegments ];  // Split knot span corresponding to array index if true.
        bool* splitSpanV = new bool [ numVSegments ];
    
        knot* splitKnotU = new knot [ numUSegments ];  // Knot value within span that span will be split at.
        knot* splitKnotV = new knot [ numVSegments ];
            
        for ( int index = 0; index < numUSegments; index ++ )
            splitSpanU [ index ] = false;

        for ( int index = 0; index < numVSegments; index ++ )
            splitSpanV [ index ] = false;

        // Step through each segment and calculate control point distance to surface.
    
        for ( int segU = 0; segU < numUSegments; segU ++ )
        {
            for ( int segV = 0; segV < numVSegments; segV ++ )
            {

if ( segU == 159 && segV == 2 )
{
    cout << "stop\n";
}
                // Don't test a segment if it has already being split.
                
                if ( ! splitSpanU [ segU ] || ! splitSpanV [ segV ] )
                {
                    double flatness = ctrlPtNet -> calcFlatness ( segU * degU, segV * degV, degU + 1, degV + 1 );

                    if ( flatness > tolerance )
                    {
                        tolOk = false;
                        splitSpanU [ segU ] = true;
                        splitSpanV [ segV ] = true;
cout << "SegU: " << segU << " SegV: " << segV << " Flatness: " << flatness << "\n";
                    }
                }
            }
        }

        if ( ! tolOk )
        {
            // Split spans. First find parameters to insert.
    
            int spanIndex = knotVectU -> getFirstSpan();  // Current span index.
    
            for ( int spanNum = 0; spanNum < numUSegments; spanNum ++ )
            {
                if ( splitSpanU [ spanNum ] )
                {
                    splitKnotU [ spanNum ] = ( ( knotVectU -> val ( spanIndex + 1 )
                                               - knotVectU -> val ( spanIndex ) ) / 2 )
                                               + knotVectU -> val ( spanIndex );
                }
    
                spanIndex = knotVectU -> getNextSpan ( spanIndex );
            }
    
            spanIndex = knotVectV -> getFirstSpan();
    
            for ( int spanNum = 0; spanNum < numVSegments; spanNum ++ )
            {
                if ( splitSpanV [ spanNum ] )
                {
                    splitKnotV [ spanNum ] = ( ( knotVectV -> val ( spanIndex + 1 )
                                               - knotVectV -> val ( spanIndex ) ) / 2 )
                                               + knotVectV -> val ( spanIndex );
                }
    
                spanIndex = knotVectV -> getNextSpan ( spanIndex );
            }
    
            // Insert knots into each vector.
    
            for ( int spanNum = 0; spanNum < numUSegments; spanNum ++ )
            {
                if ( splitSpanU [ spanNum ] )
                    insertKnot ( splitKnotU [ spanNum ], SNL_U_DIR, degU );
            }
    
            for ( int spanNum = 0; spanNum < numVSegments; spanNum ++ )
            {
                if ( splitSpanV [ spanNum ] )
                    insertKnot ( splitKnotV [ spanNum ], SNL_V_DIR, degV );
            }
        }

        // Number of Bezier segments may have increased. If arrays are to be used again their sizes
        // will be invalid.

        delete[] splitSpanU;
        delete[] splitSpanV;

        delete[] splitKnotU;
        delete[] splitKnotV;
    }
}

double snlSurface::maxCurvatureU()
{
    //! Return maximum curvature of surface in U direction.
    //  ---------------------------------------------------
    //! @return Value between 0 and PI.
    
    return ctrlPtNet -> maxCurvatureU();
}

double snlSurface::maxCurvatureV()
{
    //! Return maximum curvature of surface in V direction.
    //  ---------------------------------------------------
    //! @return Value between 0 and PI.
    
    return ctrlPtNet -> maxCurvatureV();
}

snlVector snlSurface::calcNormal ( knot paramU, knot paramV, snlPoint* evalPt )
{
    //! Calculate surface normal.
    //  -------------------------
    //! @param paramU U parameter of location to generate normal from.
    //! @param paramV V parameter of location to generate normal from.
    //! @param evalPt Pointer to point that holds point evaluted at paramter. Optional.
    //!
    //! @return Pointer to normal to surface. Caller owns pointer.

    snlVector       velocityU;
    snlVector       velocityV;
    snlPoint        evalPoint;

    if ( ! evalPt )
        velocities ( paramU, paramV, evalPoint, velocityU, velocityV );
    else
        velocities ( paramU, paramV, *evalPt, velocityU, velocityV );

    snlVector normal;

    normal.crossProduct ( velocityU, velocityV );

    return normal;
}

snlSCtrlPtLocn* snlSurface::findClosestCtrlPt ( snlPoint* points, int numPoints )
{
    //! Find closest control point to a given point.
    //  --------------------------------------------
    //! @param points Array of points to process.
    //! @param numPoints Number of points in array to process.
    //!
    //! @return Array of found control points that best match given points.
    //!          Returned array indexes correspond to given array indexes.
    //!          Caller owns array and should delete it once no longer needed.

    int     index;

    // Initialise return array.

    snlSCtrlPtLocn* retArray = new snlSCtrlPtLocn [ numPoints ];

    double maxDouble = DBL_MAX;

    for ( index = 0; index < numPoints; index ++ )
        retArray [ index ].dist = maxDouble;  // Make this very large.

    const snlCtrlPoint* ctrlPts = controlPoints();

    // Check test points against control points.

    int uSize = sizeU();
    int vSize = sizeV();

    index = 0;

    double dist;
    
    for ( int indexU = 0; indexU < uSize; indexU ++ )
    {
        for ( int indexV = 0; indexV < vSize; indexV ++ )
        {
            for ( int ptIndex = 0; ptIndex < numPoints; ptIndex ++ )
            {
                // Calculate distance to point.

                dist = ctrlPts [ index ].distSqrd ( points [ ptIndex ] );

                if ( retArray [ ptIndex ].dist > dist )
                {
                    retArray [ ptIndex ].dist = dist;
                    retArray [ ptIndex ].uIndex = indexU;
                    retArray [ ptIndex ].vIndex = indexV;
                }
            }

            index ++;
        }
    }

    return retArray;
}

snlCurve* snlSurface::extractEdge ( int edge )
{
    //! Extract surface edge as curve.
    //  ------------------------------
    //! @param edge enum surfaceEdges value that indicates which edge to return.

    snlCtrlPoint** ctrlPtPtrs;
    snlKnotVector* knotVect;

    int size;

    switch ( edge )
    {
        case SNL_EDGE_UMIN:

            size = ctrlPtNet -> getSizeV();
            ctrlPtPtrs = new snlCtrlPoint* [ size ];
            ctrlPtNet -> locatePointsV ( 0, 0, size, ctrlPtPtrs );
            knotVect = new snlKnotVector ( *knotVectV );
            
            break;
            
        case SNL_EDGE_VMIN:

            size = ctrlPtNet -> getSizeU();
            ctrlPtPtrs = new snlCtrlPoint* [ size ];
            ctrlPtNet -> locatePointsU ( 0, 0, size, ctrlPtPtrs );
            knotVect = new snlKnotVector ( *knotVectU );

            break;
        
        case SNL_EDGE_UMAX:
        
            size = ctrlPtNet -> getSizeV();
            ctrlPtPtrs = new snlCtrlPoint* [ size ];
            ctrlPtNet -> locatePointsV ( ctrlPtNet -> getSizeU() - 1, 0, size, ctrlPtPtrs );
            knotVect = new snlKnotVector ( *knotVectV );
            
            break;

        case SNL_EDGE_VMAX:
        
            size = ctrlPtNet -> getSizeU();
            ctrlPtPtrs = new snlCtrlPoint* [ size ];
            ctrlPtNet -> locatePointsU ( 0, ctrlPtNet -> getSizeV() - 1, size, ctrlPtPtrs );
            knotVect = new snlKnotVector ( *knotVectU );
            
            break;
    };

    // Generate new control point array.

    snlCtrlPoint* ctrlPts = new snlCtrlPoint [ size ];

    for ( int index = 0; index < size; index ++ )
        ctrlPts [ index ] = * ctrlPtPtrs [ index ];

    delete[] ctrlPtPtrs;

    // Generate curve to return.

    return new snlCurve ( size, ctrlPts, knotVect );
}

snlSurface* snlSurface::fillet ( int edge, snlVector& frontFaceNormal,
                                 snlSurface& surface2, snlVector& frontFaceNormal2,
                                 double tolerance, double radius, bool trim1, bool trim2 )
{
    //! Create a fillet between this and another surface.
    //  -------------------------------------------------
    //! @param edge Edge of this surface to fillet.
    //! @param frontFaceNormal Normal that defines side of this surface to place fillet on.
    //! @param surface2 surface to fillet.
    //! @param frontFaceNormal2 Normal that defines side of surface to place fillet on.
    //! @param tolerance Tolerance to surfaces fillet should comply with.
    //! @param radius Radius of fillet.
    //! @param trim1 Trim surface1 ( controlling surface ).
    //! @param trim2 Trim surface2 ( matching surface ).
    //!
    //! @par Notes:
    //!               The normals used for orientation should always be relative
    //!               to the first corner of the respective surface.
    //! @verbatim
    //!                      Edge orientation -
    //!
    //!                             uMin
    //!                       --------------------> V
    //!                      |                |
    //!                      |                |
    //!                      |                |
    //!                vMin  |                |  vMax
    //!                      |                |
    //!                      |                |
    //!                      |________________|
    //!                      |      uMax
    //!                      |
    //!                      v
    //!
    //!                      U                          @endverbatim

cout.precision ( 16 );

    // Calculate normal orientation.

    snlVector refNorm = calcNormal ( minU(), minV() );

    double orientation1 = 1.0;

    if ( refNorm.dot ( frontFaceNormal ) < 0.0 )
        orientation1 = - 1.0;

    refNorm = surface2.calcNormal ( surface2.minU(), surface2.minV() );

    double orientation2 = 1.0;

    if ( refNorm.dot ( frontFaceNormal2 ) < 0.0 )
        orientation2 = - 1.0;

    // Generate list of starting fillet locations.

    ptrList < arcLocn > arcLocnList;

    knot min_u = minU();
    knot max_u = maxU();
    knot min_v = minV();
    knot max_v = maxV();

    knot paramU;
    knot paramV;

    knot constParam;

    bool stepU = false;

    switch ( edge )
    {
        case SNL_EDGE_UMIN:

            paramU = min_u;
            constParam = paramU;
            paramV = min_v;
            break;
            
        case SNL_EDGE_VMIN:

            paramU = min_u;
            paramV = min_v;
            constParam = paramV;
            stepU = true;
            break;
        
        case SNL_EDGE_UMAX:
        
            paramU = max_u;
            constParam = paramU;
            paramV = min_v;
            break;

        case SNL_EDGE_VMAX:

            paramU = min_u;
            paramV = max_v;
            constParam = paramV;
            stepU = true;
            break;
    };

    snlCurve* edgeCurve = extractEdge ( edge );

    edgeCurve -> refine ( tolerance );

    snlVector normal;

    // Generate seed arc locations.

    snlCtrlPoint* edgePoints = ( edgeCurve -> controlPointNet() ).getCtrlPtsPtr();

    int edgeCurveSize = edgeCurve -> size();

    snlPoint* ptsToProj = new snlPoint [ edgeCurveSize ];

    for ( int index = 0; index < edgeCurveSize; index ++ )
        ptsToProj [ index ] = edgePoints [ index ];

    int numProjLocns;

    snlSurfLocn* initialLocns = fastProject ( ptsToProj, edgeCurveSize, &numProjLocns, tolerance, 1.0e-8, 10, 1, 1 );

    // Make sure first point is min param.

    int arcIndex = 0;

    knot lastParam;
    
    for ( int index = -1; index < edgeCurveSize; index ++ )
    {
        if ( index == edgeCurveSize - 1 )
        {
            // Make sure max param is obtained for last point.

            if ( stepU )
            {
                paramU = max_u;
                paramV = constParam;
            }
            else
            {
                paramU = constParam;
                paramV = max_v;
            }
        }
        else if ( index > - 1 )
        {
            paramU = initialLocns [ index ].paramU;
            paramV = initialLocns [ index ].paramV;
        }

        // Check for duplicate parameter.

        if ( index > -1 )
        {
            if ( ( stepU && paramU == lastParam ) || ( ! stepU && paramV == lastParam ) )
                continue;
        }
       
        if ( stepU )
            lastParam = paramU;
        else
            lastParam = paramV;
        
        arcLocn* locn = new arcLocn;

        locn -> cParamU = paramU;
        locn -> cParamV = paramV;
        
        velocities ( paramU, paramV, locn -> cPt, locn -> velU, locn -> velV );

        // Controlling surface

        normal.crossProduct ( locn -> velU, locn -> velV );

        normal *= orientation1;

        normal.length ( radius );

        locn -> normPt = locn -> cPt + normal;

        locn -> converged = false;
        locn -> stalled = false;

        locn -> arcIndex = arcIndex ++;

        arcLocnList.append ( locn, true );

    }

    delete[] initialLocns;
    
cout << "Finished stepping out. Num Locations: " << arcLocnList.count() << "\n";
    // Project control points to matching surface then calculate new control points

    double normTol = tolerance / sqrt ( tolerance * tolerance + radius * radius );  // Normal tolerance for projection.

    int arraySize = arcLocnList.count();

    snlPoint* points = new snlPoint [ arraySize ];

    while ( 1 )
    {
        // Assemble points to send to the projection function.

        arcLocn* locn = arcLocnList.first();

        int index = 0;

        while ( locn )
        {
            if ( ! locn -> converged && ! locn -> stalled )
                points [ index ++ ] = locn -> normPt;
            
            locn = arcLocnList.next();
        }

        // Project to matching surface.

        int numToSend = index;

        snlSurfLocn* surfLocns = surface2.fastProject ( points, numToSend, 0, tolerance, normTol, 10, 1, 1 );

        // Test for convergence and create next arc location to try if needed.

        locn = arcLocnList.first();

        while ( locn && ( locn -> converged || locn -> stalled ) )
            locn = arcLocnList.next();

        bool complete = true;


        for ( index = 0; index < numToSend; index ++ )
        {
            // If distance to matching surface is within tolerance of arc radius then
            // arc location has converged to an answer.

            if ( surfLocns [ index ].dist > radius - tolerance && surfLocns [ index ].dist < radius + tolerance )
            {
                locn -> converged = true;
                locn -> mParamU = surfLocns [ index ].paramU;
                locn -> mParamV = surfLocns [ index ].paramV;
                locn -> mPt = surfLocns [ index ].pt;
            }
            else
            {
                // Calculate new guess for arc location on controlling surface.

                double distDelta; 

                snlVector mRadius ( locn -> normPt, surfLocns [ index ].pt );

                // Calculate normal to matching surface at projected point.

                snlVector mNormal = surface2.calcNormal ( surfLocns [ index ].paramU, surfLocns [ index ].paramV );

                mNormal *= orientation2;

                if ( mRadius.dot ( mNormal ) > 0.0 )
                    distDelta = radius + mRadius.length();
                else
                    distDelta = mRadius.length() - radius;

                mRadius.length ( distDelta );

                // Get component of delta in direction we are moving.

                if ( stepU )
                {
                    // U is fixed.
                    
                    double length = locn -> velV.length();
                    knot deltaV = mRadius.dot ( locn -> velV ) / ( length * length );

                    knot newV = locn -> cParamV + deltaV;

                    if ( newV > max_v ) newV = max_v;
                    if ( newV < min_v ) newV = min_v;

                    if ( newV < locn -> cParamV + SNL_NUMERIC_NOISE && newV > locn -> cParamV - SNL_NUMERIC_NOISE )
                        locn -> stalled = true;
                    else
                    {
                        locn -> cParamV = newV;
                        complete = false;
                    }
                }
                else
                {
                    // V is fixed.
                    
                    double length = locn -> velU.length();
                    knot deltaU = mRadius.dot ( locn -> velU ) / ( length * length );

                    knot newU = locn -> cParamU + deltaU;
                    
                    if ( newU > max_u ) newU = max_u;
                    if ( newU < min_u ) newU = min_u;

                    if ( newU < locn -> cParamU + SNL_NUMERIC_NOISE && newU > locn -> cParamU - SNL_NUMERIC_NOISE )
                        locn -> stalled = true;
                    else
                    {
                        locn -> cParamU = newU;
                        complete = false;
                    }
                }

                // Recalculate controlling surface data.

                velocities ( locn -> cParamU, locn -> cParamV, locn -> cPt, locn -> velU, locn -> velV );

                normal.crossProduct ( locn -> velU, locn -> velV );

                normal *= orientation1;

                normal.length ( radius );

                locn -> normPt = locn -> cPt + normal;
            }

            locn = arcLocnList.next();

            while ( locn && ( locn -> converged || locn -> stalled ) )
                locn = arcLocnList.next();
        }

        delete[] surfLocns;

        if ( complete ) break;
    }

    // Generate surface from found arcs.

    int arcCount = 0;

    arcLocn* locn = arcLocnList.first();

    // Get number of arcs to create surface from and calculate
    // the maximum arc angle.

    double maxAngle = 0.0;

    while ( locn )
    {
        if ( locn -> converged )
        {
            arcCount ++;

            snlVector startArc ( locn -> normPt, locn -> cPt );
            snlVector endArc ( locn -> normPt, locn -> mPt );

            double angle = startArc.angle ( endArc );

            if ( angle > maxAngle ) maxAngle = angle;

            locn -> arcAngle = angle;
        }

        locn = arcLocnList.next();
    }

    if ( ! arcCount ) return 0;  // A surface couldn't be generated.

    // Calculate number of sections. Must be common to all arcs so that knot vector is the same across them all.

    int numSections = (int) ( ( maxAngle / ( M_PI / 2.0 ) ) + 1 );

    // Generate array of arcs as curves.

    snlCurve** curves = new snlCurve* [ arcCount ];

    locn = arcLocnList.first();

    int index = 0;

    while ( locn )
    {
        if ( locn -> converged )
            curves [ index ++ ] = new snlCurve ( locn -> cPt, locn -> mPt, locn -> normPt, numSections );

        locn = arcLocnList.next();
    }

    // Generate skinned surface from arcs.

    snlSurface* retSurf = new snlSurface ( curves, arcCount );

    // Clean up and return.
    
    for ( index = 0; index < arcCount; index ++ )
        delete curves [ index ];

    delete[] curves;

    delete edgeCurve;

    return retSurf;
}

void snlSurface::transform ( snlTransform& transf )
{
    //! Apply transformation to surface.
    //  --------------------------------

    ctrlPtNet -> transform ( transf );
}

void snlSurface::makeCompatible ( snlSurface* surfaceToMatch, int direction )
{
    //! Make surfaces compatible in one parametric direction.
    //  -----------------------------------------------------
    //! @param surfaceToMatch Surface to make this surface compatible with.
    //! @param direction Parametric direction. ( enum parametricDirections ).

    // Synchronise degree.

    if ( direction == SNL_U_DIR )
    {
        if ( degU > ( surfaceToMatch -> degU ) )
            surfaceToMatch -> elevateDegree ( direction, degU - ( surfaceToMatch -> degU ) );

        if ( degU < ( surfaceToMatch -> degU ) )
            elevateDegree ( direction, ( surfaceToMatch -> degU ) - degU );
    }
    else
    {
        // SNL_V_DIR.
        
        if ( degV > ( surfaceToMatch -> degV ) )
            surfaceToMatch -> elevateDegree ( direction, degV - ( surfaceToMatch -> degV ) );

        if ( degV < ( surfaceToMatch -> degV ) )
            elevateDegree ( direction, ( surfaceToMatch -> degV ) - degV );
    }

    // Sync knot vectors.

    synchronise ( *surfaceToMatch, direction );
    surfaceToMatch -> synchronise ( *this, direction );
    
}

void snlSurface::synchronise ( snlSurface& surface, int direction )
{
    //! In the specified direction, synchronise this surfaces knot vector to given surfaces knot
    //! vector.
    //  ----------------------------------------------------------------------------------------
    //! @param surface Surface to snync to.
    //! @param direction Parametric direction. ( enum parametricDirections ).
    //!
    //! @par Notes:
    //!           Knots are only ever added NOT removed.
    //!           So if surface has less multiplicity at a particular span index
    //!           then true synchronisation will not occur and the caller
    //!           should call the synchronise function on surface with this object
    //!           as it's argument.

    int tDeg, oDeg;  // This degree, other surfaces degree.
    
    snlKnotVector* tKnotVect;  // This surfaces knot vector in direction.
    snlKnotVector* oKnotVect;  // Other surfaces knot vector in direction.
    
    if ( direction == SNL_U_DIR )
    {
        tDeg = degU;
        oDeg = surface.degU;
        tKnotVect = knotVectU;
        oKnotVect = surface.knotVectU;
    }
    else
    {
        tDeg = degV;
        oDeg = surface.degV;
        tKnotVect = knotVectV;
        oKnotVect = surface.knotVectV;
    }

    if ( tDeg != oDeg ) return;  // Surfaces to sync to must have same degree.

    // Make parametric ranges equal.

    knot thisMin = tKnotVect -> min();
    knot thisMax = tKnotVect -> max();
    
    knot compMin = oKnotVect -> min();
    knot compMax = oKnotVect -> max();

    if ( thisMin != compMin || thisMax != compMax )
    {
        // Reparameterise both curves.

        knot newMin = thisMin > compMin ? compMin : thisMin;
        knot newMax = thisMax > compMax ? thisMax : compMax;

        tKnotVect -> reparameterise ( newMin, newMax );
        oKnotVect -> reparameterise ( newMin, newMax );
    }

    // Sync knots.
    
    unsigned numSpans = oKnotVect -> getNumSpans();
        
    unsigned spanIndex = oKnotVect -> getFirstSpan();
        
    for ( unsigned index = 0; index < numSpans; index ++ )
    {
        knot param = oKnotVect -> val ( spanIndex );
        
        int multi = oKnotVect -> findMultiplicity ( spanIndex );
        
        unsigned insertSpan = tKnotVect -> findSpan ( param );  // Where param would be inserted in this object.
        
        // If knot already exists in this surface then reduce multiplicity to add.
        
        if ( tKnotVect -> val ( insertSpan ) == param )
            multi -= tKnotVect -> findMultiplicity ( insertSpan );
        
        if ( multi > 0 )
            insertKnot ( param, direction, multi, true );
        
        // Get next span.
        
        spanIndex = oKnotVect -> getNextSpan ( spanIndex );
    }    
}

void snlSurface::addTrimCurve ( snlCurve* curve )
{
    //! Add trimming curve to this surface.
    //  -----------------------------------
    //! @param curve Trimming curve to add. This object owns the curve.

    trim_curves -> append ( curve, true );
}

bool snlSurface::removeTrimCurve ( snlCurve* curve )
{
    //! Delete trimming curve from this surface.
    //  ----------------------------------------
    //! @param curve Trimming curve to remove.

    if ( ! trim_curves -> hasItem ( curve ) ) return false;

    trim_curves -> remove ( curve );

    return true;
}

void snlSurface::print()
{
    //! Print surfaces control points and knot vectors to std out.
    //  ----------------------------------------------------------
    
    ctrlPtNet -> print();

    cout << "Knotvector U - ";
    knotVectU -> print();

    cout << "Knotvector V - ";
    knotVectV -> print();
}

void snlSurface::print_cpp()
{
    //! Print surfaces control points and knot vectors to std out.
    //  ----------------------------------------------------------
    //! @par Notes:
    //!      Prints data to be used for direct inclusion into a c++ program.

    cout << "int degreeU = " << degU << ";\n";
    cout << "int degreeV = " << degV << ";\n\n";

    cout << "int sizeU = " << sizeU() << ";\n";
    cout << "int sizeV = " << sizeV() << ";\n\n";

    ctrlPtNet -> print_cpp();

    cout << "\n";
    
    cout << "knot knotVectorU [ " << knotVectU -> size() << " ] = ";
    knotVectU -> print_cpp();
    cout << "\n\n";
    
    cout << "knot knotVectorV [ " << knotVectV -> size() << " ] = ";
    knotVectV -> print_cpp();
    cout << "\n";
}

void snlSurface::vertexNet ( snlVertexNet* vNet, double tolerance, bool parametric )
{
    //! Return approximation to surface.
    //  --------------------------------
    //! @param tolerance Tolerance to surface of approximation.
    //! @param parametric Do a parametric analysis of the surface as opposed to knot refinement.
    //! @param vNet Vertex net to fill with data.
        
    
    const snlCtrlPoint*   ctrlPts;
    int                   sizeU;
    int                   sizeV;
    
    snlSurface* tmpSurf = 0;
    
    if ( tolerance > 0.0 )
    {
        tmpSurf = new snlSurface ( *this );
        tmpSurf -> refine ( tolerance );
        ctrlPts = ( tmpSurf -> ctrlPtNet ) -> getCtrlPts();
        sizeU = ( tmpSurf -> ctrlPtNet ) -> getSizeU();
        sizeV = ( tmpSurf -> ctrlPtNet ) -> getSizeV();
    }
    else
    {    
        ctrlPts = ctrlPtNet -> getCtrlPts();
        sizeU = ctrlPtNet -> getSizeU();
        sizeV = ctrlPtNet -> getSizeV();
    }
    
    vNet -> vertexNet ( ctrlPts, sizeU, sizeV );    
    
    if ( tmpSurf ) delete tmpSurf;
}

void snlSurface::triangleMesh ( snlTriangleMesh* triMesh, int toleranceType, double tolerance )
{
    //! Return approximation to surface as triangle mesh.
    //  -------------------------------------------------
    //! @param triMesh Triangle mesh to populate with data.
    //! @param toleranceType How the tolerance is applied. See snlSurfaceBase::meshToleranceType.
    //! @param tolerance Mesh must be within tolerance to surface.

    // !@#$ TEMP CODE - Just triangulate a vertexNet so that higher level stuff can be built and tested first.

    snlVertexNet* vNet = new snlVertexNet;

    vertexNet ( vNet, tolerance, false );

    const snlVertex* vertexes = vNet -> vertexes();

    int sizeU = vNet -> sizeU();
    int sizeV = vNet -> sizeV();

    triMesh -> addVertexes ( vertexes, sizeU * sizeV );

    int leftEdge, rightEdge, diagonalEdge, bottomEdge, topEdge;

    int* nextLeftEdges = new int [ sizeV - 1 ];

    for ( int indexU = 0; indexU < sizeU - 1; indexU ++ )
    {
        int vertIndex = indexU * sizeV;
        
        for ( int indexV = 0; indexV < sizeV - 1; indexV ++ )
        {
            // Create edges and triangles.

            if ( indexU == 0 )
                leftEdge = triMesh -> addEdge ( vertIndex, vertIndex + 1 );  // Left Edge.
            else
                leftEdge = nextLeftEdges [ indexV ];
                
            if ( indexV == 0 )
                bottomEdge = triMesh -> addEdge ( vertIndex, vertIndex + sizeV );  // Bottom Edge.
            
            diagonalEdge = triMesh -> addEdge ( vertIndex + 1, vertIndex + sizeV );  // Diagonal.

            topEdge = triMesh -> addEdge ( vertIndex + 1, vertIndex + sizeV + 1 );  // Top Edge.

            rightEdge = triMesh -> addEdge ( vertIndex + sizeV, vertIndex + sizeV + 1 );  // Right Edge.

            nextLeftEdges [ indexV ] = rightEdge;

            // Create two triangles per quad.

            triMesh -> addTriangle ( leftEdge, bottomEdge, diagonalEdge );
            triMesh -> addTriangle ( rightEdge, topEdge, diagonalEdge );

            // Set edge for next quad.

            bottomEdge = topEdge;
        }
    }

    // Clean up.

    delete[] nextLeftEdges;
}

void snlSurface::genSurfRevolution ( snlCurve& generator, snlPoint& axisStart, snlPoint& axisEnd, double angle )
{
    //! Construct surface of revolution.
    //  --------------------------------
    //! @param generator Generating curve.
    //! @param axisStart Starting point of axis generator is revolved about.
    //! @param axisEnd Ending point of axis.
    //! @param angle Angle in degrees to revolve generator about axis. Angle is in degrees so that
    //!              different precisions of PI do not affect surface closure.
    //!
    //! @par Notes:
    //!      Rotation is counter clockwise about axis vector. Right hand rule. Curve defines V
    //!      direction.

    // Clamp angle to 360 degrees.
    
    if ( angle > 360.0 ) angle = 360.0;

    double radAngle = ( angle / 180.0 ) * M_PI;

    // Calculate number of sections and section angle.

    int numSections = (int) ( ( angle / 90.0 ) + 1 );

    double sectionAngle = radAngle / (double) numSections;

    double stepAngle = sectionAngle / 2.0;

    // Calculate mid point weight.

    double midPtWeight = cos ( stepAngle );

    // Setup rotation transforms.

    int numRotations = numSections * 2;

    snlTransform* rotations = new snlTransform [ numRotations ];

    for ( int index = 0; index < numRotations; index ++ )
        rotations [ index ].rotate ( stepAngle * (double) ( index + 1 ), axisStart, axisEnd );

    // Generate non rotated mid point control points.

    int sizeV = generator.size();

    const snlCtrlPoint* curvePts = ( generator.controlPointNet() ).getCtrlPts();

    snlCtrlPoint* midPoints = new snlCtrlPoint [ sizeV ];

    for ( int index = 0; index < sizeV; index ++ )
    {
        // Get vector from point to axis.

        snlVector projection = projectToLine ( axisStart, axisEnd, curvePts [ index ] );

        projection *= - 1.0;  // Vector is pointing into the axis, we want it pointing out from it.

        double projDist = projection.length();

        if ( projDist != 0.0 )
        {
            // Calculate mid point to axis distance.
    
            double dist = projDist / midPtWeight;  // Mid point weight is the cosine of the step angle.
    
            // Generate new control point.
    
            projection.length ( dist - projDist );
        }

        midPoints [ index ] = curvePts [ index ] + projection;

        // Multiply by mid point weight.
        
        midPoints [ index ].multiplyWeight ( midPtWeight );
    }

    // Generate surface control points.

    int sizeU = ( numSections * 2 ) + 1;

    int totalSize = sizeU * sizeV;

    snlCtrlPoint* surfPts = new snlCtrlPoint [ totalSize ];

    int index = 0;
    int stepIndex = -1;

    for ( int indexU = 0; indexU < sizeU; indexU ++ )
    {
        for ( int indexV = 0; indexV < sizeV; indexV ++ )
        {
            if ( ! indexU || ( indexU == ( sizeU - 1 ) && angle == 360.0 ) )
                // If this is the first U index then curve points are taken as is.
                surfPts [ index ] = curvePts [ indexV ];
                
            else
            {
                // Calculate rotated control point.

                if ( stepIndex % 2 )
                {
                    // Not a mid point.
                    surfPts [ index ] = curvePts [ indexV ];
                }
                else
                {
                    // Is a mid point.
                    surfPts [ index ] = midPoints [ indexV ];
                }   
                
                rotations [ stepIndex ].transform ( surfPts [ index ] );
            }
        
            index ++;
        }

        stepIndex ++;
    }

    ctrlPtNet = new snlCtrlPointNetSurface ( surfPts, sizeU, sizeV );

    // Generate Knot Vectors.

    // U Knot Vector.
    
    knot* uKnots = new knot [ sizeU + 3 ];  // Degree 2 knot vector.

    for ( index = 0; index < 3; index ++ )
    {
        // End clamps.

        uKnots [ index ] = 0.0;
        uKnots [ sizeU + index ] = 1.0;
    }

    // Internal knots.

    index = 3;

    knot knotStep = 1.0 / (double) ( numSections );
    knot knotVal = knotStep;

    for ( int step = 0; step < numSections - 1; step ++ )
    {
        uKnots [ index ++ ] = knotVal;  // Multiplicity 2.
        uKnots [ index ++ ] = knotVal;

        knotVal += knotStep; 
    }

    knotVectU = new snlKnotVector ( uKnots, sizeU + 3, 2 );

    // V Knot Vector.

    knotVectV = new snlKnotVector ( generator.knotVector() );

    // Setup remaining variables.

    degU = 2;
    degV = generator.degree();

    // Clean up.

    delete[] rotations;
    delete[] midPoints;
}

