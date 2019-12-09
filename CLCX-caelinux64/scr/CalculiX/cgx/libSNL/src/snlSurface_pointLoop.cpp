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

// Point loop related definitions for snlSurface. ( snlSurface.cpp has been segmented to reduce size ).

#include "snlSurface_pointLoop.h"

snlSurface::snlSurface ( snlPoint* points, int numPoints )
{
    // Create surface that fits a closed loop as described by points.
    // --------------------------------------------------------------
    // points:      Points that describe a closed loop. The end point should not be a duplicate of the start point.
    // numPoints:   Number of points in points array.

    init();

    // Make sure first and last points aren't the same.

    if ( points [ 0 ] == points [ numPoints - 1 ] )
        numPoints --;

    // Generate surface.

    fitBilinearCoons ( points, numPoints );
}

snlSurface::snlSurface ( snlPoint* points, int numPoints, snlPrimType primType, snlPoint* axisStart, snlPoint* axisEnd )
{
    // Create surface that fits a closed loop as described by points.
    // --------------------------------------------------------------
    // points:      Points that describe a closed loop. The end point should not be a duplicate of the start point.
    // numPoints:   Number of points in points array.
    // primType:    Type of primitive surface to use to fit points to.
    // axisStart:   Starting point of axis for Cone and Cylinder. Centre point for sphere. ( Not required for plane ).
    // axisEnd:     Ending point of axis for Cone and Cylinder. ( Not required for Sphere ).
    
    init();

    // Make sure first and last points are compatible with primitive being built.

    snlPoint* pointsToUse = points;

    if ( primType == SNL_BILINEAR_COONS && points [ 0 ] == points [ numPoints - 1 ] )
    {
        numPoints --;
    }
    else if ( ! ( points [ 0 ] == points [ numPoints - 1 ] ) )
    {
        numPoints ++;

        pointsToUse = new snlPoint [ numPoints ];

        for ( int index = 0; index < numPoints - 1; index ++ )
            pointsToUse [ index ] = points [ index ];

        pointsToUse [ numPoints - 1 ] = points [ 0 ];
    }

    switch ( primType )
    {
        case SNL_PRIM_PLANE:

            fitPlane ( pointsToUse, numPoints );
            break;
        
        case SNL_PRIM_SPHERE:

            fitSphere ( pointsToUse, numPoints, axisStart );
            break;
        
        case SNL_PRIM_CYLINDER:
        
            fitCylinder ( pointsToUse, numPoints, axisStart, axisEnd );
            break;
        
        case SNL_PRIM_CONE:

            fitCone ( pointsToUse, numPoints, axisStart, axisEnd );
            break;
            
        case SNL_BILINEAR_COONS:
        default:

            fitBilinearCoons ( pointsToUse, numPoints );
            break;
    }

    if ( pointsToUse != points ) delete[] pointsToUse;
}

void snlSurface::fitBilinearCoons ( snlPoint* points, int numPoints )
{
    // Construct bilinear Coons surface that fits a closed loop as described by points.
    // -----------------------------------------------------------------------------
    // points:      Points that describe a closed loop. The end point should not be a duplicate of the start point.
    // numPoints:   Number of points in points array.

    // The resultant Coons patch is easier if the given points are not homogeneous.

    snlPoint* normPoints = new snlPoint [ numPoints ];

    for ( int index = 0; index < numPoints; index ++ )
    {
        normPoints [ index ] = points [ index ];
        normPoints [ index ].normalise();
    }

    // Create an interpolated curve of degree 2.

    knot* retParams;

    snlCurve* initialCurve = new snlCurve ( normPoints, numPoints, snlCurve::SNL_GLOBAL_INTERP_CENTRIFUGAL, 2, true, &retParams );

    // Split curve into four pieces using a basic approximation with given points.

    double curveLength = 0;  // Total curve length.

    double* sectionLengths = new double [ numPoints ];  // Save distance between points.

    for ( int index = 0; index < numPoints; index ++ )
    {
        if ( index < numPoints - 1 )
            sectionLengths [ index ] = sqrt ( normPoints [ index ].distSqrd ( normPoints [ index + 1 ] ) );
        else
            sectionLengths [ index ] = sqrt ( normPoints [ index ].distSqrd ( normPoints [ 0 ] ) );  // Loop join.

        curveLength += sectionLengths [ index ];
    }

    // Step through section lengths to find points to split curve at.

    double splitLength = curveLength / 4.0;

    double splitParams [ 3 ];   // Three internal split positions gives four curve segments.

    int sectionIndex = 0;
    double distance = 0.0;

    for ( int splitNumber = 0; splitNumber < 3; splitNumber ++ )
    {
        double splitDistance = splitLength * (double) ( splitNumber + 1 );
        
        while ( ( distance + sectionLengths [ sectionIndex ] ) < splitDistance )
        {
            distance += sectionLengths [ sectionIndex ++ ];
        }

        // Calculate parameter to split at.

        double sectionFraction = ( splitDistance - distance ) / sectionLengths [ sectionIndex ];

        double paramDelta = retParams [ sectionIndex + 1 ] - retParams [ sectionIndex ];

        splitParams [ splitNumber ] = retParams [ sectionIndex ] + paramDelta * sectionFraction;
    }

    // Split curve into four pieces.

    snlCurve* curve1;
    snlCurve* curve2;
    snlCurve* curve3;
    snlCurve* curve4;
    
    curve1 = new snlCurve ( *initialCurve );
    curve1 -> truncate ( splitParams [ 0 ], false, false );
    curve1 -> reparameterise ( 0.0, 1.0 );  // All knot vectors should be clamped to [ 0.0, 1.0 ].
    
    curve2 = new snlCurve ( *initialCurve );
    curve2 -> truncate ( splitParams [ 0 ], true, false );
    curve2 -> truncate ( splitParams [ 1 ], false, false );
    curve2 -> reparameterise ( 0.0, 1.0 );

    curve3 = new snlCurve ( *initialCurve );
    curve3 -> truncate ( splitParams [ 1 ], true, false );
    curve3 -> truncate ( splitParams [ 2 ], false, false );
    curve3 -> reparameterise ( 0.0, 1.0 );

    curve4 = new snlCurve ( *initialCurve );
    curve4 -> truncate ( splitParams [ 2 ], true, false );
    curve4 -> reparameterise ( 0.0, 1.0 );

    // Curves 3 and 4 are going the wrong direction.

    curve3 -> reverseEvalDirection();
    curve4 -> reverseEvalDirection();

    // Generate Coons Patch.

    genBilinearCoons ( curve4, curve2, curve1, curve3 );

    // Clean Up.

    delete[] normPoints;
    delete[] sectionLengths;

    delete curve1;
    delete curve2;
    delete curve3;
    delete curve4;
}

void snlSurface::fitPlane ( snlPoint* points, int numPoints )
{
    // Fit ( and generate ) a planar surface to given points.
    // ------------------------------------------------------
    // points:      Given points to fit plane to.
    // numPoints:   Number of points in points array.

    // Using first point in array as base find point furtherest from that point.

    double distToEndPoint = 0.0;  // Largest distance found.
    int baseLineEndIndex;

    for ( int index = 1; index < numPoints; index ++ )
    {
        double dist = points [ 0 ].distSqrd ( points [ index ] );

        if ( dist > distToEndPoint )
        {
            distToEndPoint = dist;
            baseLineEndIndex = index;
        }
    }

    // Use furtherest point and base point as line. Project points to line. Re-asses line starting point.

    double maxProjDist = 0.0;  // Maximum projection distance to line.

    snlVector maxProjVector;  // Vector associated with maximum projection distance.

    snlPoint newStartPoint = points [ 0 ];

    for ( int index = 1; index < numPoints; index ++ )
    {
        snlVector projVect = projectToLine ( points [ 0 ], points [ baseLineEndIndex ], points [ index ] );

        double dist = projVect.length();

        if ( dist > maxProjDist )
        {
            maxProjDist = dist;
            maxProjVector = projVect;
        }
        
        snlPoint projPoint = points [ index ] + projVect;  // Project point onto base line.

        // Determine if point should be new base line starting point.

        dist = points [ baseLineEndIndex ].distSqrd ( projPoint );

        if ( dist > distToEndPoint )
        {
            // New start point has been found.
            newStartPoint = projPoint;
            distToEndPoint = dist;
        }
    }

    // Generate points that will delineate a rectangular NURBS patch.

    snlPoint patchPt1 = newStartPoint + maxProjVector;
    snlPoint patchPt2 = newStartPoint - maxProjVector;
    snlPoint patchPt3 = points [ baseLineEndIndex ] + maxProjVector;
    snlPoint patchPt4 = points [ baseLineEndIndex ] - maxProjVector;

    // Generate planar NURBS surface.

    knotVectV = new snlKnotVector ( 0.0, 1.0, 4, 1 );
    knotVectU = new snlKnotVector ( 0.0, 1.0, 4, 1 );

    snlCtrlPoint* ctrlPts = new snlCtrlPoint [ 4 ];

    ctrlPts [ 0 ] = patchPt1;
    ctrlPts [ 1 ] = patchPt2;
    ctrlPts [ 2 ] = patchPt3;
    ctrlPts [ 3 ] = patchPt4;

    degU = 1;
    degV = 1;

    ctrlPtNet = new snlCtrlPointNetSurface ( ctrlPts, 2, 2, false );    
}

void snlSurface::fitCylinder ( snlPoint* points, int numPoints, snlPoint* axisStart, snlPoint* axisEnd )
{
    // Fit ( and generate ) a cylindrical surface to given points.
    // -----------------------------------------------------------
    // points:      Given points to fit cylinder to.
    // numPoints:   Number of points in points array.
    // axisStart:   Starting point of cylinder axis.
    // axisEnd:     Ending point of cylinder axis.

    // Determine start and end bounds of axis as well as angular bounds of cylinder.

    snlVector axis ( *axisStart, *axisEnd );

    // Calculate initial projections that are used for cylinder angle determination.
    
    snlVector angleProjMax = projectToLine ( *axisStart, *axisEnd, points [ 0 ] );  // Projection associated with maximum angle.

    snlPoint newAxisStart = points [ 0 ] + angleProjMax;  // New axis bound points.
    snlPoint newAxisEnd = newAxisStart;
    
    angleProjMax *= -1.0;  // These projections must be pointing from axis outwards.
    
    snlVector angleProjMin = angleProjMax;  // Projection associated with minimum angle.

    snlVector lastProj;  // Projection vector that next projection is compared to.

    // All angles are rooted at the start point projection. Positive angles are anti-clockwise ( right hand rule ).

    double accumAngle = 0.0;  // Accumulated angle.
    double maxAngle = 0.0;
    double minAngle = 0.0;

    // Step through each point from point loop.

    for ( int index = 0; index < numPoints; index ++ )
    {
        snlVector projVect = projectToLine ( *axisStart, *axisEnd, points [ index ] );

        snlPoint projPt = points [ index ] + projVect;

        // See if projected point is new start or end axis point.

        if ( snlVector ( newAxisStart, projPt ).dot ( axis ) < 0.0 )
            newAxisStart = projPt;

        if ( snlVector ( newAxisEnd, projPt ).dot ( axis ) > 0.0 )
            newAxisEnd = projPt;

        // Process angular bounds.

        projVect *= -1.0;  // Make sure vector is pointing outwards from axis.

        if ( index > 0 )
        {
            snlVector crossProd;

            crossProd.crossProduct ( lastProj, projVect );

            if ( ! crossProd.isNull() )
            {
                // If normal vector ( cross product ) is opposite direction to axis then angle is negative.
    
                double angle = lastProj.angle ( projVect );
    
                if ( crossProd.dot ( axis ) > 0.0 )
                {
                    // Positive angle.
                    accumAngle += angle;

                    if ( accumAngle > maxAngle )
                    {
                        maxAngle = accumAngle;
                        angleProjMax = projVect;
                    }
                }
                else
                {
                    // Negative angle.
                    accumAngle -= angle;

                    if ( accumAngle < minAngle )
                    {
                        minAngle = accumAngle;
                        angleProjMin = projVect;
                    }
                }
            }
        }

        lastProj = projVect;
    }

    // Cylinder bounds should have been calculated. Now build a cylinder.

    double absAngle = maxAngle - minAngle;  // minAngle must be below or equal to 0.

    // Recalculate more exact absAngle from vectors but using previously calculated absAngle as guide.

    double angle = angleProjMax.angle ( angleProjMin );

    if ( absAngle > M_PI )
        absAngle = ( M_PI * 2.0 ) - angle;
    else
        absAngle = angle;

    // Convert absolute angle to degrees.

    absAngle = ( absAngle / ( 2.0 * M_PI ) ) * 360;

    if ( absAngle > 360.0 )
        absAngle = 360.0;

    snlPoint curveStart = newAxisStart + angleProjMin;
    snlPoint curveEnd = newAxisEnd + angleProjMin;

    snlCurve generator ( 2, 3, curveStart, curveEnd );

    genSurfRevolution ( generator, *axisStart, *axisEnd, absAngle );
}

void snlSurface::fitCone ( snlPoint* points, int numPoints, snlPoint* axisStart, snlPoint* axisEnd )
{
    // Fit ( and generate ) a conic surface to given points.
    // -----------------------------------------------------
    // points:      Given points to fit cone to.
    // numPoints:   Number of points in points array.
    // axisStart:   Starting point of cone axis.
    // axisEnd:     Ending point of cone axis.

    // Determine start and end bounds of axis as well as angular bounds of cone.

    snlVector axis ( *axisStart, *axisEnd );

    // Calculate initial projections that are used for angle determination.
    
    snlVector angleProjMax = projectToLine ( *axisStart, *axisEnd, points [ 0 ] );  // Projection associated with maximum angle.

    snlPoint newAxisStart = points [ 0 ] + angleProjMax;  // New axis bound points.
    snlPoint newAxisEnd = newAxisStart;
    
    angleProjMax *= -1.0;  // These projections must be pointing from axis outwards.
    
    snlVector angleProjMin = angleProjMax;  // Projection associated with minimum angle.

    snlVector lastProj;  // Projection vector that next projection is compared to.

    // All angles are rooted at the start point projection. Positive angles are anti-clockwise ( right hand rule ).

    double accumAngle = 0.0;  // Accumulated angle.
    double maxAngle = 0.0;
    double minAngle = 0.0;

    // Radii at start and end of axis.

    double axisStartRadius = angleProjMax.length();
    double axisEndRadius = axisStartRadius;

    bool intersectsAxis = false;  // Only true if point loop intersects axis.

    double lastAbsAngle, curAbsAngle;  // Absolute angles as based from first given point.

    bool circleSect [ 16 ];  // Circle is divided into 16 sections. True if section is used.

    for ( int index = 0; index < 16; index ++ )
        circleSect [ index ] = false;

    double circleSectSliceSize = ( M_PI* 2.0 ) / 16.0;  // Size of section in radians.

    snlVector baseProj;  // Baseline projection. Used as zero angle reference.
    baseProj.zero();

    bool negTraverse;  // Negative traversal.

    // Step through each point from point loop.

    for ( int index = 0; index < numPoints; index ++ )
    {
        snlVector projVect = projectToLine ( *axisStart, *axisEnd, points [ index ] );

        if ( projVect.isNull() )
            intersectsAxis = true;

        snlPoint projPt = points [ index ] + projVect;

        // See if projected point is new start or end axis point.

        if ( snlVector ( newAxisStart, projPt ).dot ( axis ) < 0.0 )
        {
            newAxisStart = projPt;
            axisStartRadius = projVect.length();
        }

        if ( snlVector ( newAxisEnd, projPt ).dot ( axis ) > 0.0 )
        {
            newAxisEnd = projPt;
            axisEndRadius = projVect.length();
        }

        // *** Process angular bounds.

        projVect *= -1.0;  // Make sure vector is pointing outwards from axis.

        // Make sure base projection is not a null vector.
        
        if ( baseProj.isNull() && ! projVect.isNull() )
            baseProj = projVect;

        // Calculate absolute angle.

        if ( ! projVect.isNull() )
        {
            curAbsAngle = baseProj.angle ( projVect );

            // Check to see if angle went past 180 degrees.

            snlVector crossProd;

            crossProd.crossProduct ( baseProj, projVect );

            if ( ! crossProd.isNull() && crossProd.dot ( axis ) < 0.0 )
            {
                // Adjust for negative angle.

                curAbsAngle = M_PI * 2.0 - curAbsAngle;
            }
        }

        if ( index > 0 )
        {
            snlVector crossProd;

            crossProd.crossProduct ( lastProj, projVect );

            if ( ! crossProd.isNull() )
            {
                // If normal vector ( cross product ) is opposite direction to axis then angle is negative.
    
                double angle = lastProj.angle ( projVect );
    
                if ( crossProd.dot ( axis ) > 0.0 )
                {
                    // Positive angle.
                    accumAngle += angle;

                    negTraverse = false;

                    if ( accumAngle > maxAngle )
                    
                    {
                        maxAngle = accumAngle;
                        angleProjMax = projVect;
                    }
                }
                else
                {
                    // Negative angle.
                    accumAngle -= angle;

                    negTraverse = true;

                    if ( accumAngle < minAngle )
                    {
                        minAngle = accumAngle;
                        angleProjMin = projVect;
                    }
                }

                // Calculate which circle sections are used. Only used if axis is intersected.

                int sectEnd;

                if ( lastAbsAngle > curAbsAngle && ! negTraverse )
                    sectEnd = 15;  // Account for going over 360 degrees during positive traversal.
                else
                    sectEnd = (int) ( curAbsAngle / circleSectSliceSize );

                int sectStart = (int) ( lastAbsAngle / circleSectSliceSize );

                if ( sectStart > sectEnd )
                {
                    int intermediate = sectStart;
                    sectStart = sectEnd;
                    sectEnd = intermediate;
                }

                for ( int sectIndex = sectStart; sectIndex <= sectEnd; sectIndex ++ )
                    circleSect [ sectIndex ] = true;
            }
        }

        lastProj = projVect;

        lastAbsAngle = curAbsAngle;
    }

    double absAngle;  // Absolute angle.

    // If axis was intersected then angular properties need to be recalculated.

    if ( intersectsAxis )
    {
        // Find largest gap.

        int startGap = -1;
        int endGap;
        int curGap = -1;  // Current maximum gap start index.
        double gapAngle = 0.0;
        double maxGap = 0.0;

        bool overlapped = false;

        for ( int sectIndex = 0; sectIndex < 16; sectIndex ++ )
        {
            if ( ! circleSect [ sectIndex ]  )
            {
                if ( startGap == -1 )
                    startGap = sectIndex;

                gapAngle += circleSectSliceSize;
            }

            if ( circleSect [ sectIndex ] && startGap != -1 )
            {
                if ( maxGap < gapAngle )
                {
                    maxGap = gapAngle;
                    curGap = startGap;
                }

                startGap = -1;
                endGap = sectIndex;  // Deliberately points to first non-gap index.
                gapAngle = 0.0;
            }

            if ( startGap != -1 && sectIndex == 15  && ! overlapped )
            {
                // Gap overlaps starting point so start from beginning again.
                sectIndex = 0;
                overlapped = true;
            }
        }

        // Calculate new cone parameters.
        
        absAngle = ( M_PI * 2.0 ) - maxGap;

        double rotAngle = endGap * circleSectSliceSize;

        snlTransform rotation;

        rotation.rotate ( rotAngle, *axisStart, *axisEnd );

        angleProjMin = baseProj;

        rotation.transform ( angleProjMin );
    }

    else

    {
        absAngle = maxAngle - minAngle;  // minAngle must be below or equal to 0.

        // Recalculate more exact absAngle from vectors but using previously calculated absAngle as guide.
    
        double angle = angleProjMax.angle ( angleProjMin );
    
        if ( absAngle > M_PI )
            absAngle = ( M_PI * 2.0 ) - angle;
        else
            absAngle = angle;
    }

    // Cone bounds should have been calculated. Now build a cone.

    // Convert absolute angle to degrees.

    absAngle = ( absAngle / ( 2.0 * M_PI ) ) * 360;

    if ( absAngle > 360.0 )
        absAngle = 360.0;

    snlVector axisProj = angleProjMin;
    axisProj.length ( axisStartRadius );
    snlPoint curveStart = newAxisStart + axisProj;
    
    axisProj = angleProjMin;
    axisProj.length ( axisEndRadius );
    snlPoint curveEnd = newAxisEnd + axisProj;

    snlCurve generator ( 2, 3, curveStart, curveEnd );

    genSurfRevolution ( generator, *axisStart, *axisEnd, absAngle );
}

void snlSurface::fitSphere ( snlPoint* points, int numPoints, snlPoint* sphereCentre )
{
    // Fit ( and generate ) a spherical surface to given points.
    // ---------------------------------------------------------
    // points:          Given points to fit sphere to.
    // numPoints:       Number of points in points array.
    // sphereCentre:    Centre of sphere.

    // Use first and second points with sphere centre as basis plane.

    snlVector baseVect ( *sphereCentre, points [ 0 ] );  // Basis vector associated with point 0. Stays constant.
    snlVector refVect ( *sphereCentre, points [ 1 ] );  // Reference vector. Calculated per point other than point 0.

    snlVector basisPlaneNormal;  // Used to define plane as basis for angle calculations.

    basisPlaneNormal.crossProduct ( baseVect, refVect );

    snlVector lonBasisNormal;  // Used as basis for determining rotation angle about basis plane normal. Longitude.

    lonBasisNormal.crossProduct ( basisPlaneNormal, baseVect );

    double latMin = M_PI / 2.0;  // Max and min latitude with basis plane normal being 0 degrees. Point 0 is at 90 degrees.
    double latMax = latMin;

    snlVector refNorm;  // Normal vector used for longitudinal angle determination.
    snlVector angleCheckNorm;

    double lonAngle = 0.0;  // "Running" longitudinal angle.
    double lonMin = 0.0;  // Longitudinal bounds.
    double lonMax = 0.0;

    // Determine patch boundaries in terms of rotation angles.

    for ( int ptIndex = 1; ptIndex < numPoints; ptIndex ++ )
    {
        // Calculate angle to basis plane normal. Latitude.
        
        refVect.calc ( *sphereCentre, points [ ptIndex ] );
        
        double normAngle = basisPlaneNormal.angle ( refVect );

        if ( normAngle > latMax ) latMax = normAngle;
        if ( normAngle < latMin ) latMin = normAngle;

        // Calculate longitudinal angle delta.

        refNorm.crossProduct ( basisPlaneNormal, refVect );

        double relLonAngle = lonBasisNormal.angle ( refNorm );  // Relative angle.

        // Adjust for movement in negative direction.

        angleCheckNorm.crossProduct ( lonBasisNormal, refNorm );

        if ( basisPlaneNormal.dot ( angleCheckNorm ) < 0.0 )
            lonAngle -= relLonAngle;  // Angle is negative.
        else
            lonAngle += relLonAngle;
            
        if ( lonAngle > lonMax ) lonMax = lonAngle;
        if ( lonAngle < lonMin ) lonMin = lonAngle;

        lonBasisNormal = refNorm;
    }

    // Angle bounds have been determined. Create spherical patch.

    // First create a circular arc.
    
    lonBasisNormal.crossProduct ( basisPlaneNormal, baseVect );

    snlTransform latTransf;
    snlTransform lonTransf;
    
    lonTransf.rotate ( lonMin, *sphereCentre, basisPlaneNormal );

    snlPoint startPoint ( points [ 0 ] );

    latTransf.rotate ( latMin - ( M_PI / 2.0 ), *sphereCentre, lonBasisNormal );

    latTransf.transform ( startPoint );
    lonTransf.transform ( startPoint );

    latTransf.ident();
    latTransf.rotate ( latMax - ( M_PI / 2.0 ), *sphereCentre, lonBasisNormal );
    
    snlPoint endPoint ( points [ 0 ] );

    latTransf.transform ( endPoint );
    lonTransf.transform ( endPoint );

    snlCurve* generator = new snlCurve ( startPoint, endPoint, *sphereCentre );

    // Create surface of revolution using arc.

    snlPoint axisEnd = *sphereCentre + basisPlaneNormal;

    double revAngleDeg = ( ( lonMax - lonMin ) / M_PI ) * 180.0;

    genSurfRevolution ( *generator, *sphereCentre, axisEnd, revAngleDeg );

    // Clean up.

    delete generator;
}


