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

#include "snlCtrlPointNetSurface.h"
#include "snlUtil.h"

snlCtrlPointNetSurface::snlCtrlPointNetSurface ( snlCtrlPoint* cPtArray, unsigned size_u, unsigned size_v, bool copy )
{
    // Control Points for a surface - Constructor
    // ------------------------------------------
    // cPtArray:    2-D Array of points to copy. Format [u][v]
    // size_u:      Size in u parametric direction.
    // size_v:      Size in v parametric direction.
    // copy:        Make a copy of cPtArray.

    sizeU = size_u;
    sizeV = size_v;

    ctrlPtArraySize = sizeU * sizeV;
    
    if ( copy )
    {
        // Copy points into object.        
        ctrlPts = new snlCtrlPoint [ ctrlPtArraySize ];
        
        for ( unsigned count = 0; count < ctrlPtArraySize; count ++ )
            ctrlPts [ count ] = cPtArray [ count ];
    }
    else
        ctrlPts = cPtArray;
}

snlCtrlPointNetSurface::snlCtrlPointNetSurface ( unsigned size_u, unsigned size_v, snlPoint& origin,
                                                 snlPoint& cornerX, snlPoint& cornerY )
{
    // Constructor.
    // ------------
    // size_u:      Number of points in U direction.
    // size_v:      Number of points in V direction.
    // origin:      First point in array
    // cornerX:     Corner at ( x, 0 ).
    // cornerY:     Corner at ( 0, y ).
    //
    // Notes:       Usage of X,Y coordinates is deprecated.

    sizeU = size_u;
    sizeV = size_v;

    if ( ! sizeU || ! sizeV ) return;
    
    ctrlPtArraySize = size_u * size_v;

    ctrlPts = new snlCtrlPoint [ ctrlPtArraySize ];

    snlVector O_X ( origin, cornerX );
    snlVector O_Y ( origin, cornerY );

    snlVector stepX = O_X * ( 1.0 / (double) sizeU );
    snlVector stepY = O_Y * ( 1.0 / (double) sizeV );

    snlPoint currentX = origin;

    unsigned cIndex = 0;

    for ( int index_X = 0; index_X < sizeU; index_X ++ )
    {
        snlPoint currentY = currentX;

        for ( int index_Y = 0; index_Y < sizeV; index_Y ++ )
        {
            // Add point to control points array.
            ctrlPts [ cIndex ++ ] = currentY;

            currentY = currentY + stepY;
        }

        currentX = currentX + stepX;
    }
}

snlCtrlPointNetSurface::~snlCtrlPointNetSurface()
{
}

unsigned snlCtrlPointNetSurface::getSizeU() const
{
    return sizeU;
}

unsigned snlCtrlPointNetSurface::getSizeV() const
{
    return sizeV;
}

void snlCtrlPointNetSurface::setSizeU ( unsigned size )
{
    // Set sizeU to size.
    // ------------------
    
    sizeU = size;
}

void snlCtrlPointNetSurface::setSizeV ( unsigned size )
{
    // Set sizeV to size.
    // ------------------
    
    sizeV = size;
}

snlCtrlPoint* snlCtrlPointNetSurface::getPoint ( unsigned indexU, unsigned indexV ) const
{
    // Return pointer to control point at [t][u]
    // -----------------------------------------
    // indexU:  u param.
    // indexV:  v param.

    return ( ctrlPts + ( indexU * sizeV ) + indexV );
}

snlCtrlPoint* snlCtrlPointNetSurface::growU ( int increaseBy, bool reallocate )
{
    // Grow control point net in U direction by 1.
    // -------------------------------------------
    // reallocate:      If false then extra space has already been allocated elsewhere.
    // increaseBy:      Size U direction should be increased by.

    if ( ! ctrlPts ) return 0;
    
    if ( reallocate )
    {
        snlCtrlPoint* newPts = new snlCtrlPoint [ ctrlPtArraySize + ( sizeV * increaseBy ) ];

        // Copy points into new array.
        for ( unsigned index = 0; index < ctrlPtArraySize; index ++ )
           newPts [ index ] = ctrlPts [ index ];

        // Delete old array and point to new one.
        delete[] ctrlPts;

        ctrlPts = newPts;
        
        ctrlPtArraySize += sizeV * increaseBy;
    }

    sizeU += increaseBy;

    return ctrlPts;
}

snlCtrlPoint* snlCtrlPointNetSurface::growV ( int increaseBy, bool reallocate )
{
    // Grow control point net in V direction by 1.
    // -------------------------------------------
    // reallocate:      If false then extra space has already been allocated elsewhere.
    // increaseBy:      Size V direction should be increased by.

    if ( ! ctrlPts ) return 0;
    
    snlCtrlPoint*   newPts;
    
    if ( reallocate )
        newPts = new snlCtrlPoint [ ctrlPtArraySize + ( sizeU * increaseBy )];
    else
        newPts = ctrlPts;

    // Copy points into new array.

    int oldPos = sizeU * sizeV - 1;  // Don't use ctrlPtArraySize. Realloc may have changed it.
    int newPos = sizeU * ( sizeV + increaseBy ) - 1 - increaseBy;
    
    for ( int indexU = sizeU - 1; indexU > -1; indexU -- )
    {
        for ( int indexV = sizeV - 1; indexV > -1; indexV -- )
            newPts [ newPos -- ] = ctrlPts [ oldPos -- ];

        newPos -= increaseBy;
    }

    // Delete old array and point to new one.

    if ( reallocate )
    {
        delete[] ctrlPts;

        ctrlPts = newPts;
        
        ctrlPtArraySize += sizeU * increaseBy;
    }

    sizeV += increaseBy;

    return ctrlPts;
}

snlCtrlPoint* snlCtrlPointNetSurface::shrinkU()
{
    // Shrink control point net in U direction by 1.
    // ---------------------------------------------
    
   if ( ! ctrlPts ) return 0;

    snlCtrlPoint* newPts = new snlCtrlPoint [ ctrlPtArraySize - sizeV ];

    // Copy points into new array.
    for ( unsigned index = 0; index < ( ctrlPtArraySize - sizeV ); index ++ )
        newPts [ index ] = ctrlPts [ index ];

    // Delete old array and point to new one.

    delete[] ctrlPts;

    ctrlPts = newPts;

    sizeU --;

    ctrlPtArraySize -= sizeV;

    return ctrlPts;
}

snlCtrlPoint* snlCtrlPointNetSurface::shrinkV()
{
    // Shrink control point net in V direction by 1.
    // ---------------------------------------------
    
    if ( ! ctrlPts ) return 0;

    snlCtrlPoint* newPts = new snlCtrlPoint [ ctrlPtArraySize - sizeU ];

    // Copy points into new array.
    for ( int indexU = 0; indexU < sizeU; indexU ++ )
        for ( int indexV = 0; indexV < ( sizeV - 1 ); indexV ++ )
            newPts [ ( indexU * ( sizeV - 1 ) ) + indexV ] = ctrlPts [ indexU * sizeV + indexV ];

    // Delete old array and point to new one.

    delete[] ctrlPts;

    ctrlPts = newPts;

    sizeV --;

    ctrlPtArraySize -= sizeU;

    return ctrlPts;

}

double snlCtrlPointNetSurface::calcFlatness ( int indexU, int indexV, int numPointsU, int numPointsV )
{
    // Calculate flatness of a rectangular section of control points.
    // --------------------------------------------------------------
    // indexU:      U index of starting position in control point net.
    // indexV:      V index of starting position in control point net.
    // numPointsU:  Number of points in U direction, including starting point, to test.
    // numPointsV:  Number of points in V direction, including starting point, to test.
    //
    // Notes:
    //
    //              Rectangle data orientation:
    //
    //                V     B--------D
    //                      |        |
    //                ^     |        |
    //                |     |        |
    //                |     A--------C
    //
    //                      ----->   U

    // Locate rectangular array of points to process.

    int numPoints = numPointsU * numPointsV;

    snlCtrlPoint** ctrlPointPtrs = new snlCtrlPoint* [ numPoints ];

    locatePoints ( indexU, indexV, numPointsU, numPointsV, ctrlPointPtrs );

    // Pre-calculate vectors and normals.

    // 4 Corners.
    
    int indexB = numPointsV - 1;
    int indexC = numPoints - numPointsV;
    int indexD = numPoints - 1;

    snlCtrlPoint ptA = *(ctrlPointPtrs [ 0 ]);
    ptA.normalise();
    snlCtrlPoint ptB = *(ctrlPointPtrs [ numPointsV - 1 ]);
    ptB.normalise();
    snlCtrlPoint ptC = *(ctrlPointPtrs [ numPoints - numPointsV ]);
    ptC.normalise();
    snlCtrlPoint ptD = *(ctrlPointPtrs [ numPoints - 1 ]);
    ptD.normalise();

    // Side Vectors.
    
    snlVector ab ( ptA, ptB );
    ab.unitise();
    
    snlVector ba = ab * -1.0;

    snlVector ac ( ptA, ptC );
    ac.unitise();

    snlVector ca = ac * -1.0;

    snlVector bd ( ptB, ptD );
    bd.unitise();

    snlVector db = bd * -1.0;

    snlVector cd ( ptC, ptD );
    cd.unitise();

    snlVector dc = cd * -1.0;

    // Diagonal Vectors.

    snlVector bc ( ptB, ptC );
    snlVector ad ( ptA, ptD );

    // Normals.

    snlVector normA ( ac, ab );
    snlVector normB ( ba, bd );
    snlVector normC ( cd, ca );
    snlVector normD ( db, dc );

    // Project each point of the rectangle, not on the corners, onto the 4 base triangles.
    // If the projection does not lay on a triangle, it is not considered in any further flatness
    // calculations. If a point does not have any projections that lay on one of the four triangles
    // then the closest distance to one of the 6 vectors defining the 4 triangles is taken
    // as the flatness.

    double maxDistance = 0.0;  // Maximum distance or "flatness".

    for ( int index = 0; index < indexD; index ++ )
    {
        if ( ! index || index == indexB || index == indexC ) continue;  // Don't process the corners.

        bool interiorFound = false;  // True if at least one triangle contains projection.

        snlCtrlPoint t = *(ctrlPointPtrs [ index ]);
        t.normalise();

        // Project point to and compare to triangle rooted at A.

        snlVector toProj ( ptA, t );

        snlVector projVect = toProj.project ( normA );

        snlPoint projPoint = t - projVect;

        double projDist = projVect.length();

        bool isInterior = isInteriorToTriangle ( projPoint, ptA, ab, ac, ptB, ba, bc );

        if ( isInterior ) interiorFound = true;

        if ( isInterior &&  maxDistance < projDist ) maxDistance = projDist;

        // Project point to and compare to triangle rooted at B.

        toProj.calc ( ptB, t );

        projVect = toProj.project ( normB );

        projPoint = t - projVect;

        projDist = projVect.length();

        isInterior = isInteriorToTriangle ( projPoint, ptB, ba, bd, ptA, ab, ad );

        if ( isInterior ) interiorFound = true;

        if ( isInterior &&  maxDistance < projDist ) maxDistance = projDist;
        
        // Project point to and compare to triangle rooted at C.

        toProj.calc ( ptC, t );

        projVect = toProj.project ( normC );

        projPoint = t - projVect;

        projDist = projVect.length();

        isInterior = isInteriorToTriangle ( projPoint, ptC, ca, cd, ptA, ac, ad );

        if ( isInterior ) interiorFound = true;

        if ( isInterior &&  maxDistance < projDist ) maxDistance = projDist;
        
        // Project point to and compare to triangle rooted at D.

        toProj.calc ( ptD, t );

        projVect = toProj.project ( normD );

        projPoint = t - projVect;

        projDist = projVect.length();

        isInterior = isInteriorToTriangle ( projPoint, ptD, db, dc, ptB, bd, bc );

        if ( isInterior ) interiorFound = true;

        if ( isInterior &&  maxDistance < projDist ) maxDistance = projDist;
        
        // If no triangle contains a projection then project to the six triangle outlines and take _smallest_ value

        if ( ! interiorFound )
        {
            toProj.calc ( ptA, t );

            double projMaxDistance = ab.projectDist ( toProj );  // A -> B.

            double projDist = ac.projectDist ( toProj );  // A -> C.

            if ( projDist < projMaxDistance ) projMaxDistance = projDist;

            projDist = ad.projectDist ( toProj );  // A -> D.

            if ( projDist < projMaxDistance ) projMaxDistance = projDist;

            toProj.calc ( ptD, t );

            projDist = db.projectDist ( toProj );  // D -> B.

            if ( projDist < projMaxDistance ) projMaxDistance = projDist;

            projDist = dc.projectDist ( toProj );  // D -> C.

            if ( projDist < projMaxDistance ) projMaxDistance = projDist;

            toProj.calc ( ptB, t );

            projDist = bc.projectDist ( toProj );  // B -> C.

            if ( projDist < projMaxDistance ) projMaxDistance = projDist;

            if ( maxDistance < projMaxDistance ) maxDistance = projMaxDistance;
        }
    }

    return maxDistance;
}

double snlCtrlPointNetSurface::calcFlatnessU ( int indexU, int indexV, int numPoints, bool degree1 ) const
{
    // Test flatness in U direction.
    // -----------------------------
    // indexU:      U index of starting position in control point net.
    // indexV:      V index of starting position in control point net.
    // numPoints:   Number of points, including starting point, to test.
    
    snlCtrlPoint** testCtrlPoints  = new snlCtrlPoint* [ numPoints ];
    
    double flatness;
    
    if ( degree1 )
    {
        testCtrlPoints [ 0 ] = getPoint ( indexU, indexV );
        testCtrlPoints [ 1 ] = getPoint ( indexU + 1, indexV );
        testCtrlPoints [ 2 ] = getPoint ( indexU, indexV + 1 );
        testCtrlPoints [ 3 ] = getPoint ( indexU + 1, indexV + 1 );
        
        flatness = calcDeg1Flatness ( (snlPoint**) testCtrlPoints );        
    }
    else
    {
        locatePointsU ( indexU, indexV, numPoints, testCtrlPoints );
        
        flatness = snlCtrlPointNet::calcFlatness ( (snlPoint**) testCtrlPoints, numPoints );
    }
    
    delete[] testCtrlPoints;
    
    return flatness;
}

double snlCtrlPointNetSurface::calcFlatnessV ( int indexU, int indexV, int numPoints, bool degree1 ) const
{
    // Test flatness in V direction.
    // -----------------------------
    // indexU:      U index of starting position in control point net.
    // indexV:      V index of starting position in control point net.
    // numPoints:   Number of points, including starting point, to test.
    
    snlCtrlPoint** testCtrlPoints  = new snlCtrlPoint* [ numPoints ];
    
    double flatness;
    
    if ( degree1 )
    {
        testCtrlPoints [ 0 ] = getPoint ( indexU, indexV );
        testCtrlPoints [ 1 ] = getPoint ( indexU, indexV + 1 );
        testCtrlPoints [ 2 ] = getPoint ( indexU + 1, indexV );
        testCtrlPoints [ 3 ] = getPoint ( indexU + 1, indexV + 1 );

        flatness = calcDeg1Flatness ( (snlPoint**) testCtrlPoints );        
    }
    else
    {    
        locatePointsV ( indexU, indexV, numPoints, testCtrlPoints );
            
        flatness = snlCtrlPointNet::calcFlatness ( (snlPoint**) testCtrlPoints, numPoints );
    }
    
    delete[] testCtrlPoints;
    
    return flatness;
}

double snlCtrlPointNetSurface::maxFlatnessU ( int span )
{
    // Calculate the maximum flatness of the control point net in the U direction
    // --------------------------------------------------------------------------
    // span:    Width of span to calculate flatness across.

    double maxFlatness = 0.0;

    int maxU = sizeU - span;
    
    for ( int indexV = 0; indexV < sizeV; indexV ++ )
    {
        for ( int indexU = 0; indexU < maxU; indexU ++ )
        {
            // Test for flatness

            double flatness = calcFlatnessU ( indexU, indexV, span + 1, false );

            if ( flatness > maxFlatness ) maxFlatness = flatness;
        }
    }

    return maxFlatness;
}

double snlCtrlPointNetSurface::maxFlatnessV ( int span )
{
    // Calculate the maximum flatness of the control point net in the V direction
    // --------------------------------------------------------------------------
    // span:    Width of span to calculate flatness across.

    double maxFlatness = 0.0;

    int maxV = sizeV - span;
    
    for ( int indexU = 0; indexU < sizeU; indexU ++ )
    {
        for ( int indexV = 0; indexV < maxV; indexV ++ )
        {
            // Test for flatness

            double flatness = calcFlatnessV ( indexU, indexV, span + 1, false );

            if ( flatness > maxFlatness ) maxFlatness = flatness;
        }
    }

    return maxFlatness;
}

double snlCtrlPointNetSurface::maxCurvatureU()
{
    // Calculate the maximum curvature of surface in U direction.
    // ----------------------------------------------------------
    // Returns:     Maximum curvature as an angle between 0 and PI.
    
    snlCtrlPoint** testCtrlPoints  = new snlCtrlPoint* [ 3 ];
    snlPoint** testPoints  = new snlPoint* [ 3 ];
    
    double maxCurvature = 0.0;
    
    for ( int indexV = 0; indexV < sizeV; indexV ++ )
    {
        for ( int indexU = 0; indexU < sizeU - 2; indexU ++ )
        {
            locatePointsU ( indexU, indexV, 3, testCtrlPoints );    
    
            // Make sure pointer is converted correctly.
        
            for ( int index = 0; index < 3; index ++ )
                testPoints [ index ] = testCtrlPoints [ index ];
            
             double curvature = calcCurvature ( testPoints );
             
             if ( curvature > maxCurvature ) maxCurvature = curvature;
        }
    } 
    
    delete[] testPoints;
    delete[] testCtrlPoints;
    
    return maxCurvature;
}

double snlCtrlPointNetSurface::maxCurvatureV()
{
    // Calculate the maximum curvature of surface in V direction.
    // ----------------------------------------------------------
    // Returns:     Maximum curvature as an angle between 0 and PI.
    
    snlCtrlPoint** testCtrlPoints  = new snlCtrlPoint* [ 3 ];
    snlPoint** testPoints  = new snlPoint* [ 3 ];
    
    double maxCurvature = 0.0;
    
    for ( int indexU = 0; indexU < sizeU; indexU ++ )
    {
        for ( int indexV = 0; indexV < sizeV - 2; indexV ++ )
        {
            locatePointsV ( indexU, indexV, 3, testCtrlPoints );    
    
            // Make sure pointer is converted correctly.
        
            for ( int index = 0; index < 3; index ++ )
                testPoints [ index ] = testCtrlPoints [ index ];
            
             double curvature = calcCurvature ( testPoints );
             
             if ( curvature > maxCurvature ) maxCurvature = curvature;
        }
    } 
    
    delete[] testPoints;
    delete[] testCtrlPoints;
    
    return maxCurvature;
}

void snlCtrlPointNetSurface::locatePoints ( int indexU, int indexV, int numPointsU, int numPointsV,
                                            snlCtrlPoint** pointsLocated ) const
{
    // Get pointers to an array of points.
    // -----------------------------------
    // indexU:          U index of starting position in control point net.
    // indexV:          V index of starting position in control point net.
    // numPointsU:      Number of points in U direction to return.
    // numPointsV:      Number of points in V direction to return.
    // pointsLocated:   Pointer array to return pointers in. Must be size numPointsU * numPointsV.
    //
    // Notes:           There is no array bounds check in this function. So be carefull of the U and V indexes
    //                  being passed to this function.
    
    int arrayIndex = indexU * sizeV + indexV;

    int retArrayIndex = 0;

    int uStep = sizeV - numPointsV;

    for ( int uPtNum = 0; uPtNum < numPointsU; uPtNum ++ )
    {
        for ( int vPtNum = 0; vPtNum < numPointsV; vPtNum ++ )
        {
            pointsLocated [ retArrayIndex ++ ] = ctrlPts + arrayIndex ++;
        }

        arrayIndex += uStep;
    }
}

void snlCtrlPointNetSurface::locatePointsU ( int indexU, int indexV, int numPoints, snlCtrlPoint** testPoints ) const
{
    // Get pointers to a series of points in the U direction.
    // ------------------------------------------------------
    // indexU:      U index of starting position in control point net.
    // indexV:      V index of starting position in control point net.
    // numPoints:   Number of points, including starting point.
    // testPoints:  Pointer array to return pointers in.
    
    int arrayIndex = indexU * sizeV + indexV;
    
    for ( int ptNum = 0; ptNum < numPoints; ptNum ++ )
    {
        if ( (unsigned) arrayIndex < ctrlPtArraySize )
        {
            testPoints [ ptNum ] = ctrlPts + arrayIndex;
            arrayIndex += sizeV;
        }
        else
            break;
    }            
}

void snlCtrlPointNetSurface::locatePointsV ( int indexU, int indexV, int numPoints, snlCtrlPoint** testPoints ) const
{
    // Get pointers to a series of points in the V direction.
    // ------------------------------------------------------
    // indexU:      U index of starting position in control point net.
    // indexV:      V index of starting position in control point net.
    // numPoints:   Number of points, including starting point.
    // testPoints:  Pointer array to return pointers in.
    
    int arrayIndex = indexU * sizeV + indexV;
    
    for ( int ptNum = 0; ptNum < numPoints; ptNum ++ )
    {
        if ( (unsigned) arrayIndex < ctrlPtArraySize )
        {
            testPoints [ ptNum ] = ctrlPts + arrayIndex;
            arrayIndex ++;
        }
        else
            break;
    }            
}

int snlCtrlPointNetSurface::maxConnections() const
{
    return 4;
}

int snlCtrlPointNetSurface::getCnctPts ( unsigned index, snlCtrlPoint* retPts )
{
    // Get connected control points to the one pointed to by index.
    // ------------------------------------------------------------
    // index:       Point index to look for.
    // retPts:      Pre-allocated array to return points in.

    // !@#$ NOT IMPLEMENTED YET!

    cout << "Doh! snlCtrlPointNetSurface::getCnctPts hasn't been finished, and you are calling it!\n";

    return 0;
}

void snlCtrlPointNetSurface::selectPoint ( int indexU, int indexV )
{
    // Select point at U, V.
    // ---------------------
    
    int index = indexU * sizeV + indexV;
    
    ctrlPts [ index ].select ( true );
}

void snlCtrlPointNetSurface::selectLineConstU ( int indexU )
{
    // Select line in constant U direction.
    // ------------------------------------
    
    for ( int indexV = 0; indexV < sizeV; indexV ++ )
        selectPoint ( indexU, indexV );
}

void snlCtrlPointNetSurface::selectLineConstV ( int indexV )
{
    // Select line in constant V direction.
    // ------------------------------------
    
    for ( int indexU = 0; indexU < sizeU; indexU ++ )
        selectPoint ( indexU, indexV );
}

void snlCtrlPointNetSurface::print()
{
    // Print control points to std out.
    // --------------------------------
    
    for ( int indexU = 0; indexU < sizeU; indexU ++ )
    {
        cout << indexU << ": ";
        
        for ( int indexV = 0; indexV < sizeV; indexV ++ )
        {
            ctrlPts [ indexU * sizeV + indexV ].print();
            cout << "\n";
        }
        
        cout << "\n";
    }
}

void snlCtrlPointNetSurface::printCompare ( snlCtrlPointNetSurface& compareTo )
{
    // Print out comparison whith another control point net.
    // -----------------------------------------------------
    // Note:    Assumes both control point nets are _exactly_ the same size.

    for ( int indexU = 0; indexU < sizeU; indexU ++ )
    {
        cout << indexU << ": ";
        
        for ( int indexV = 0; indexV < sizeV; indexV ++ )
        {
            snlPoint pt1 = ctrlPts [ indexU * sizeV + indexV ];
            snlPoint pt2 = compareTo.ctrlPts [ indexU * sizeV + indexV ];
            
            if ( pt1 == pt2 )
                cout << "E";
            else
                cout << "NE";
                
            cout << "\t";
        }
        
        cout << "\n";
    }
}

void snlCtrlPointNetSurface::print_cpp()
{
    // Print control points to std out.
    // --------------------------------
    // Notes:   Prints data that can be directly included in a c++ program.

    cout << "snlCtrlPoint* points = new snlCtrlPoint [ " << sizeU * sizeV << " ];\n\n";

    int totalSize = sizeU * sizeV;

    for ( int index = 0; index < totalSize; index ++ )
    {
        cout << "points [ " << index << " ].components ";
        ctrlPts [ index ].print();
        cout << ";\n";
    }
}

