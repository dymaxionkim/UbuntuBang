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

// *** Control Point Network - Base Class ***

#include "snlCtrlPointNet.h"
#include "snlUtil.h"

#ifdef SGI_MIPS

    #include <math.h>
    
#else

    #include <cmath>
    using namespace std;
    
#endif


snlCtrlPointNet::snlCtrlPointNet()
{
    ctrlPts = 0;
    ctrlPtArraySize = 0;
}

snlCtrlPointNet::~snlCtrlPointNet()
{
    if ( ctrlPts ) delete[] ctrlPts;
}

snlCtrlPointNet::snlCtrlPointNet ( const snlCtrlPointNet& copyFrom )
{
    // Copy constructor.
    // -----------------
    
    ctrlPtArraySize = copyFrom.ctrlPtArraySize;
    
    ctrlPts = new snlCtrlPoint [ ctrlPtArraySize ];
    
    for ( unsigned index = 0; index < ctrlPtArraySize; index ++ )
        ctrlPts [ index ] = copyFrom.ctrlPts [ index ];
}

bool snlCtrlPointNet::checkBounds ( unsigned index )
{
    // Check index against array bounds.
    // ---------------------------------

    if ( index >= ctrlPtArraySize ) return false;

    return true;
}

void snlCtrlPointNet::transform ( unsigned ptIndex, snlTransform& transf )
{
    // Transform a control point.
    // --------------------------

    if ( checkBounds ( ptIndex ) )
        transf.transform ( ctrlPts + ptIndex );
}

void snlCtrlPointNet::transform ( snlTransform& transf )
{
    // Transform all control points.
    // -----------------------------

    for ( unsigned index = 0; index < ctrlPtArraySize; index ++ )        
            transf.transform ( ctrlPts + index );    
}

void snlCtrlPointNet::transformSelected ( snlTransform& transf )
{
    // Transform all selected control points.
    // --------------------------------------

    for ( unsigned index = 0; index < ctrlPtArraySize; index ++ )
    {
        if ( ctrlPts [ index ].isSelected() )
            transf.transform ( ctrlPts + index );
    }
}

unsigned snlCtrlPointNet::getNumPts() const
{
    // Get number of control points that object holds.
    // -----------------------------------------------

    return ctrlPtArraySize;
}

const snlCtrlPoint* snlCtrlPointNet::getCtrlPts() const
{
    // Get pointer to array of control points.
    // ---------------------------------------

    return ctrlPts;
}

snlCtrlPoint* snlCtrlPointNet::getCtrlPtsPtr()
{
    // Return non-constant pointer to control points array.
    // ----------------------------------------------------
    
    return ctrlPts;
}

snlCtrlPoint snlCtrlPointNet::getPoint ( unsigned ptIndex )
{
    // Return copy of control point.
    // -----------------------------

    if ( !checkBounds ( ptIndex ) ) return snlCtrlPoint();

    return ctrlPts [ ptIndex ];
}

const snlCtrlPoint* snlCtrlPointNet::getPointPtr ( unsigned ptIndex )
{
    // Return pointer to control point.
    // --------------------------------

    if ( !checkBounds ( ptIndex ) ) return 0;

    return ctrlPts + ptIndex;
}

double snlCtrlPointNet::getTransfZ ( unsigned index, snlTransform& trans )
{
    // Get transformed Z coordinate
    // ----------------------------
    // index:       Array index of control point to process.
    // transMatrix: Transformation object.
    //
    // Note:        Does not modify control points.

    snlCtrlPoint      point;

    if ( index >= ctrlPtArraySize ) return 0.0;

    point = ctrlPts [ index ];

    trans.transform ( &point );

    return point.z();
}

double snlCtrlPointNet::getMaxTransfZ ( snlTransform& trans )
{
    // Return maximum value of z element of control points after transformation.
    // -------------------------------------------------------------------------

    unsigned        index;
    double          maxZ, transfZ;

    for ( index = 0; index < ctrlPtArraySize; index ++ )
    {
        transfZ = getTransfZ ( index, trans );

        if ( ! index || transfZ > maxZ )
            maxZ = transfZ;
    }

    return maxZ;
}

double snlCtrlPointNet::getMinTransfZ ( snlTransform& trans )
{
    // Return minimum value of z element of control points after transformation.
    // -------------------------------------------------------------------------

    unsigned        index;
    double          minZ, transfZ;

    for ( index = 0; index < ctrlPtArraySize; index ++ )
    {
        transfZ = getTransfZ ( index, trans );

        if ( ! index || transfZ < minZ )
            minZ = transfZ;
    }

    return minZ;
}

bool snlCtrlPointNet::hasPointsSelected()
{
    for ( unsigned index = 0; index < ctrlPtArraySize; index ++ )
    {
        if ( ctrlPts [ index ].isSelected() ) return true;
    }

    return false;
}

unsigned snlCtrlPointNet::numPointsSelected()
{
    // Return number of control points that are currently selected.
    // ------------------------------------------------------------

    unsigned numSelect = 0;

    for ( unsigned index = 0; index < ctrlPtArraySize; index ++ )
    {
        if ( ctrlPts [ index ].isSelected() ) numSelect ++;
    }

    return numSelect;
}

void snlCtrlPointNet::selectAllPoints ( bool yesNo )
{
    for ( unsigned index = 0; index < ctrlPtArraySize; index ++ )
        ctrlPts [ index ].select ( yesNo );
}

void snlCtrlPointNet::selectPoint ( unsigned index, bool yesNo )
{
    if ( !checkBounds ( index ) ) return;

    ctrlPts [ index ].select ( yesNo );
}

bool snlCtrlPointNet::isSelected ( unsigned index )
{
    if ( !checkBounds ( index ) ) return false;

    return ( ctrlPts [ index ].isSelected() );
}

void snlCtrlPointNet::clearSelected()
{
    for ( unsigned index = 0; index < ctrlPtArraySize; index ++ )
    {
        ctrlPts [ index ].select ( false );
    }
}

unsigned* snlCtrlPointNet::getSelectedIndexes()
{
    // Return array of indexes that corresponds to selected control points.
    // --------------------------------------------------------------------
    //
    // NOTES:       First array element holds array size.

    unsigned numSelect = numPointsSelected();

    unsigned* retArray  = new unsigned [ numSelect + 1 ];

    retArray [ 0 ] = numSelect;

    unsigned cArrayPos = 1;

    for ( unsigned index = 0; index < ctrlPtArraySize; index ++ )
    {
        if ( ctrlPts [ index ].isSelected() ) retArray [ cArrayPos ++ ] = index ;
    }

    return retArray;
}

double snlCtrlPointNet::calcFlatness ( snlPoint** points, unsigned size )
{
    // Calculate flatness.
    // -------------------
    // points:      Array of pointers to control points to evaluate.
    // size:        Size of the array.
    //
    // returns: Largest of the flatness tests.

    double      dotP, proj, testLength, normDist;
    double      flatness = 0;

    if ( size < 3 ) return 0;
    
    snlPoint pt1 ( *( points [ 0 ] ) );
    snlPoint pt2 ( *( points [ size -1 ] ) );
    
    pt1.normalise();
    pt2.normalise();
    
    snlVector lineV ( pt1, pt2 );

    // If lineV is zero length then use distance to pt1 for flatness.

    bool usePoint = false;

    if ( lineV.length() == 0.0 )
        usePoint = true;

    for ( unsigned index = 1; index < ( size - 1 ); index ++ )
    {
        pt2 = * ( points [ index ] );
        
        pt2.normalise();

        if ( usePoint )
        {
            normDist = sqrt ( pt1.distSqrd ( pt2 ) );
        }
        else
        {
            snlVector testV ( pt1, pt2 );

            // Calculate dot product.
            dotP = lineV.dot ( testV );
    
            // Project test vector onto baseline vector.
            proj = dotP / lineV.length();
    
            // Length of test vector.
            testLength = testV.length();
    
            // Length of normal from baseline to test point.
            normDist = sqrt ( testLength * testLength - proj * proj );
        }

        // Update flatness
        if ( normDist > flatness ) flatness = normDist;
    }

    return flatness;
}

double snlCtrlPointNet::calcDeg1Flatness ( snlPoint** points ) const
{
    // Specifically for caclulating degree 1 one flatness calculations
    // ---------------------------------------------------------------
    // Points:      Array of pointers to control points to evaluate. Size of array is 4.

    snlPoint    vect;
    snlPoint    nPoints [ 4 ];  // Normalised points.

    double       flatness;

    for ( int index = 0; index < 4; index ++ )
    {
        nPoints [ index ] = *( points [ index ] );
        nPoints [ index ].normalise();
    }

    // Approximate distance between lines that span opposite corners.

    vect.x ( nPoints [ 0 ].x() + ( ( nPoints [ 3 ].x() - nPoints [ 0 ].x() ) * 0.5 ) );
    vect.y ( nPoints [ 0 ].y() + ( ( nPoints [ 3 ].y() - nPoints [ 0 ].y() ) * 0.5 ) );
    vect.z ( nPoints [ 0 ].z() + ( ( nPoints [ 3 ].z() - nPoints [ 0 ].z() ) * 0.5 ) );

    vect.x ( vect.x() - nPoints [ 1 ].x() + ( ( nPoints [ 2 ].x() - nPoints [ 1 ].x() ) * 0.5 ) );
    vect.y ( vect.y() - nPoints [ 1 ].y() + ( ( nPoints [ 2 ].y() - nPoints [ 1 ].y() ) * 0.5 ) );
    vect.z ( vect.z() - nPoints [ 1 ].z() + ( ( nPoints [ 2 ].z() - nPoints [ 1 ].z() ) * 0.5 ) );

    flatness = sqrt ( vect.x() * vect.x() + vect.y() * vect.y() + vect.z() * vect.z() );

    return flatness;
}

double snlCtrlPointNet::calcCurvature ( snlPoint** points )
{
    // Calculate Curvature.
    // --------------------
    // points:      Array of 3 pointers to control points to evaluate.     
    //
    // returns:     Curvature as angle between vectors.
    
    snlPoint pt1 ( *( points [ 0 ] ) );
    snlPoint pt2 ( *( points [ 1 ] ) );
    snlPoint pt3 ( *( points [ 2 ] ) );
    
    pt1.normalise();
    pt2.normalise();
    pt3.normalise();
    
    snlVector vect1 ( pt1, pt2 );
    snlVector vect2 ( pt2, pt3 );
    
    if ( vect1.isNull() || vect2.isNull() ) return 0.0;    

    return vect1.angle ( vect2 );
}

bool snlCtrlPointNet::isConvex ( snlPoint** points, int numPts, double sensitivity )
{
    // Test for the given points being in a convex pattern.
    // ----------------------------------------------------
    // points:      Points to test.
    // numPts:      Number of points to test.
    // sensitivity: Maximum concave angle allowed to be considered convex. Used to account for noise
    //              in the curvature of a relatively flat section.
    //
    // Notes:       This function is primarily used for testing of Bezier patches
    //              to determine if they are convex.

    int midPoint = numPts / 2;

    double angleAdjust = 1.0e-15 + cos ( sensitivity );

    // Check first set of points against end point.

    for ( int index = 1; index < midPoint; index ++ )
    {
        // Check for flatness of points being checked. If the three points are colinear
        // then they are considered convex. This is done to account for round off error noise
        // that is causing this function to go into an infinite loop.

        snlVector chord ( **( points + index - 1 ), **( points + index + 1 ) );
        snlVector compare ( **( points + index - 1 ), **( points + index ) );

        double angle = chord.calcAbsCos ( compare ) + angleAdjust;

        if ( angle < 1.0 )
        {
            // Project end point onto chord.

            snlVector endProject = projectToLine ( **( points + index - 1 ), **( points + index + 1 ),
                                               **( points + numPts - 1 ) );

            // Project point to do convex test with, onto chord.

            snlVector testProject = projectToLine ( **( points + index - 1 ), **( points + index + 1 ),
                                                **( points + index ) );

            // Do convex test.

            if ( ! testProject.isNull() )
            {
                double dotP = testProject.dot ( endProject );
                if ( dotP > 0 ) return false;
            }
        }
    }

    // Check last set of points against start point.

    for ( int index = midPoint; index < numPts - 1; index ++ )
    {
        snlVector chord ( **( points + index - 1 ), **( points + index + 1 ) );
        snlVector compare ( **( points + index - 1 ), **( points + index ) );

        double angle = chord.calcAbsCos ( compare ) +  + 1.0e-15;

        if ( angle < 1.0 )
        {
            // Project end point onto chord.
    
            snlVector endProject = projectToLine ( **( points + index - 1 ), **( points + index + 1 ),
                                                **points );
    
            // Project point to do convex test with, onto chord.
    
            snlVector testProject = projectToLine ( **( points + index - 1 ), **( points + index + 1 ),
                                                    **( points + index ) );
    
            // Do convex test.
    
            if ( ! testProject.isNull() )
            {
                double dotP = testProject.dot ( endProject );
                if ( dotP > 0 ) return false;
            }
        }
    }

    return true;
}

void snlCtrlPointNet::replacePoints ( snlCtrlPoint* newPoints )
{
    // Replace all control points with new ones.
    // -----------------------------------------
    // newPoints:   New points to use. This object owns them.
    //
    // Notes:       The caller is trusted to provide the correct size of control point array.
 
    if ( ctrlPts ) delete[] ctrlPts;
    
    ctrlPts = newPoints;
}

void snlCtrlPointNet::replacePoints ( const snlCtrlPoint* newPoints, unsigned numNewPoints, unsigned replaceIndex,
                                      unsigned numToReplace )
{
    // Replace a subset of control points with new ones.
    // -------------------------------------------------
    // newPoints:    New points to place into array.
    // numNewPoint:  Number of new points to use.
    // replaceIndex: Starting index in array where replacement should begin.
    // numToReplace: Number of points to replace. Can be different from numNewPoints.    
    //
    // Notes:    Does not shrink or grow control point array allocation. That must be done seperately
    //           at a higher level or memory errors _will_ occur.
    
    
    // Account for the number of new points being more than the number being replaced.
        
    if ( numNewPoints > numToReplace )
    {
        unsigned diff = numNewPoints - numToReplace;
        
        for ( unsigned index = ctrlPtArraySize - 1; index >= replaceIndex + numNewPoints; index -- )
            ctrlPts [ index ] = ctrlPts [ index - diff ];
    }    
    
    // Copy new points into array.
    
    for ( unsigned index = 0; index < numNewPoints; index ++ )
        ctrlPts [ replaceIndex + index ] = newPoints [ index ];
    
    // Account for the number of new points being less than the number being replaced.
    
    if ( numNewPoints < numToReplace )
    {
        unsigned diff = numToReplace - numNewPoints;
        
        for ( unsigned index = replaceIndex + numNewPoints; index < ctrlPtArraySize - diff; index ++ )
            ctrlPts [ index ] = ctrlPts [ index + diff ];
    }    
}

void snlCtrlPointNet::appendPointSpace ( unsigned numPoints )
{
    // Add extra space to end of control point array.
    // ----------------------------------------------

    if ( numPoints <= 0 ) return;
    
    snlCtrlPoint* newPts = new snlCtrlPoint [ ctrlPtArraySize + numPoints ];
    
    for ( unsigned index = 0; index < ctrlPtArraySize; index ++ )
        newPts [ index ] = ctrlPts [ index ];        

    ctrlPtArraySize += numPoints;

    delete[] ctrlPts;
    
    ctrlPts = newPts;
}

void snlCtrlPointNet::truncatePointSpace ( unsigned numPoints )
{
    // Remove space from end of array.
    // -------------------------------
    
    snlCtrlPoint* newPts = new snlCtrlPoint [ ctrlPtArraySize - numPoints ];
    
    for ( unsigned index = 0; index < ( ctrlPtArraySize - numPoints ); index ++ )
        newPts [ index ] = ctrlPts [ index ];        

    ctrlPtArraySize -= numPoints;

    delete[] ctrlPts;
    
    ctrlPts = newPts;    
}

void snlCtrlPointNet::appendPoints ( const snlCtrlPoint* points, unsigned numPoints )
{
    // Append control points to this control point net.
    // ------------------------------------------------
    // points:    Points to append.
    // numPoints: Number of points to append.
    
    unsigned oldSize = ctrlPtArraySize;
    
    appendPointSpace ( numPoints );
    
    unsigned pointsIndex = 0;
    
    for ( unsigned index = oldSize; index < ctrlPtArraySize; index ++ )
        ctrlPts [ index ] = points [ pointsIndex ++ ];    
}

void snlCtrlPointNet::print()
{
    // Print all points.
    // -----------------
    
    for ( unsigned index = 0; index < ctrlPtArraySize; index ++ )
    {
        cout << "[ " << index << " ]";
        ctrlPts [ index ].print();
        cout << "\n";
    }
}

void snlCtrlPointNet::print ( unsigned fromIndex, unsigned toIndex )
{
    // Print control point information.
    // --------------------------------
    // fromIndex:        Starting control point index.
    // toIndex:          Ending control point index.
    
    for ( unsigned index = fromIndex; index <= toIndex; index ++ )
    {
        cout << "[ " << index << " ]";
        ctrlPts [ index ].print();
        cout << "\n";
    }
}

bool snlCtrlPointNet::hasConcurrentPoints() const
{
    // Return true if any concurrent points exist in net.
    // --------------------------------------------------
    
    bool concurrent = false;
    
    for ( unsigned index = 0; index < ctrlPtArraySize; index ++ )
    {
        for ( unsigned index2 = 0; index2 < ctrlPtArraySize; index2 ++ )
        {
            if ( index != index2 )
                if ( ctrlPts [ index ] == ctrlPts [ index2 ] )
                    concurrent = true;
        }
    }
    
    return concurrent;
}

void snlCtrlPointNet::operator += ( const snlCtrlPointNet& ctrlPointNet )
{
    // Point wise addition of control point nets.
    // ------------------------------------------

    unsigned numElements = ctrlPtArraySize > ctrlPointNet.ctrlPtArraySize ? ctrlPtArraySize : ctrlPointNet.ctrlPtArraySize;

    for ( unsigned index = 0; index < numElements; index ++ )
        ctrlPts [ index ] += ctrlPointNet.ctrlPts [ index ];
}

void snlCtrlPointNet::operator -= ( const snlCtrlPointNet& ctrlPointNet )
{
    // Point wise subtraction of control point nets.
    // ---------------------------------------------

    unsigned numElements = ctrlPtArraySize > ctrlPointNet.ctrlPtArraySize ? ctrlPtArraySize : ctrlPointNet.ctrlPtArraySize;

    for ( unsigned index = 0; index < numElements; index ++ )
        ctrlPts [ index ] -= ctrlPointNet.ctrlPts [ index ];
}

