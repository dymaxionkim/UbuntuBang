// libSNL - Simple Nurbs Library
// Copyright 2006 Scott A.E. Lanham, Australia.
// --------------------------------------------
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// snlSurface projection related functions.

#include "snlSurface_projection.h"

snlVertex* snlSurface::project_depr ( snlPoint* toProject, int numPoints, double convergTol,
                                      double normTol, int maxPass )
{
    //! Project an array of points to surface.
    //  --------------------------------------
    //! @param toProject Array of snlPoints to project to surface.
    //! @param numPoints Number of points being projected. ie size of points array.
    //! @param convergTol If the difference between successive newton iterations converges and is
    //!                   below this value then the point is taken to be projected.
    //! @param normTol If the cosine of the angle between the projection vector and the
    //!                projected points normal is under this value then the projection is used.
    //! @param maxPass Maximum number of mesh refinement passes allowed. Stops infinite loops
    //!                if projections don't converge.
    //!
    //! Notes: This function is now deprecated but stays around as a cross reference for
    //!        new projection work.

    sLocn* projections = new sLocn [ numPoints ];

    ptrList < sLocn >* ambig = projPtSurf ( *this, toProject, numPoints, projections,
                                            convergTol, normTol, maxPass );
                                            
    // Create vertexes to return and populate with converted sLocn's.
    // -------------------------------------------------------------
    
    snlVertex* retVertexes = new snlVertex [ numPoints ];
    
    for ( int index = 0; index < numPoints; index ++ )
    {
        retVertexes [ index ] = projections [ index ].pt;
        
        retVertexes [ index ].evalParamU ( projections [ index ].paramT );
        retVertexes [ index ].evalParamV ( projections [ index ].paramU );
        
        retVertexes [ index ].flag = projections [ index ].flag;
    }
                                    
    delete ambig;
    delete[] projections;

    return retVertexes;
}

snlSurfLocn* snlSurface::invert ( snlPoint* toInvert, int numPoints, int* retArraySize,
                                  double convergTol, double normTol, int maxPass )
{
    //! Find parametric location of given points
    //  ----------------------------------------
    //! @param toInvert Array of snlPoints to find on surface.
    //! @param numPoints Number of points being inverted. ie size of points array.
    //! @param retArraySize Size of array of surface locations that is returned.
    //! @param convergTol If the difference between successive newton iterations converges and is
    //!                   below this value then the point is taken to be inverted.
    //! @param normTol How close to perpendicular the projection of the given point to the point
    //!                found gets to the surface tangents at the found point for convergence to be
    //!                successful.
    //! @param maxPass Maximum number of refinement passes allowed. Stops infinite loops if
    //!                projections don't converge.

    // Generate point mask. If false, entry is not processed during pass.

    bool* pointMask = new bool [ numPoints ];

    for ( int index = 0; index < numPoints; index ++ )
        pointMask [ index ] = true;

    // Calculate best guess for each point to be inverted.

    ptrList <snlSurfLocnGuess>* guesses = guessInvLocation ( toInvert, numPoints, pointMask,
                                                             degU, degV );

    delete[] pointMask;
    
    return processGuesses ( toInvert, numPoints, retArraySize, guesses, convergTol,
                            normTol, maxPass );
}

snlSurfLocn* snlSurface::project ( snlPoint* toProject, int numPoints, int* retArraySize,
                                   double convergTol, double normTol, int maxPass )
{
    //! Find parametric location of given points
    //  ----------------------------------------
    //! @param toProject Array of snlPoints to find projections of on surface.
    //! @param numPoints Number of points being inverted. ie size of points array.
    //! @param retArraySize Size of array of surface locations that is returned.
    //! @param convergTol If the difference between successive newton iterations converges and is
    //!                   below this value then the point is taken to be inverted.
    //! @param normTol How close to perpendicular the projection of the given point to the point
    //!                found gets to the surface tangents at the found point for convergence to be
    //!                successful.
    //! @param maxPass Maximum number of refinement passes allowed. Stops infinite loops if
    //!                projections don't converge.

    // Generate point mask. If false, entry is not processed during pass.

    bool* pointMask = new bool [ numPoints ];

    for ( int index = 0; index < numPoints; index ++ )
        pointMask [ index ] = true;

    // Only work on copy of surface.

    snlSurface* tmpSurf = new snlSurface ( *this );

    tmpSurf -> createConvexBezierSegments( 0, 0, 0.00009 );  // Sensitivity of 0.05 degrees.

    // Calculate best guess for each point to be inverted.

    ptrList <snlSurfLocnGuess>* guesses = tmpSurf -> guessProjLocation ( toProject, numPoints,
                                                                         pointMask );

    snlSurfLocn* retArray = processGuesses ( toProject, numPoints, retArraySize, guesses,
                                             convergTol, normTol, maxPass, true );

    // Clean up and return.
    
    delete[] pointMask;

    delete tmpSurf;

    return retArray;
}

snlSurfLocn* snlSurface::fastProject ( snlPoint* toProject, int numPoints, int* retArraySize,
                                       double convergTol, double normTol, int maxPass,
                                       int sensitivity, int maxLocns )
{
    //! Find parametric location of given points
    //  ----------------------------------------
    //! @param toProject Array of snlPoints to find projections of on surface.
    //! @param numPoints Number of points being inverted. ie size of points array.
    //! @param retArraySize Size of array of surface locations that is returned.
    //! @param convergTol If the difference between successive newton iterations converges and is
    //!                   below this value then the point is taken to be inverted.
    //! @param normTol How close to perpendicular the projection of the given point to the point
    //!                found gets to the surface tangents at the found point for convergence to be
    //!                successful.
    //! @param maxPass Maximum number of refinement passes allowed. Stops infinite loops if
    //!                projections don't converge.
    //! @param sensitivity The higher this number the more accurate the initial guess phase is but
    //!                    the function becomes slower. Increases the number of subdivisions per
    //!                    knot span by this amount. Must be a positive integer
    //! @param maxLocns Maximum number of surface locations to return. If a surface is known to
    //!                 have ambiguous areas then this number should be greater than one. Value is
    //!                 clamped to be greater than zero.
    //!
    //! Notes: Fast project is not nearly as accurate as other project or invert functions.

    ptrList <snlSurfLocnGuess>* guesses = guessFastProjLocation ( toProject, numPoints, maxLocns,
                                                                  degU + sensitivity,
                                                                  degV + sensitivity );

    snlSurfLocn* retArray = processGuesses ( toProject, numPoints, retArraySize, guesses,
                                             convergTol, normTol, maxPass, true, true );

    return retArray;
}

snlSurfLocn* snlSurface::processGuesses ( snlPoint* points, int numPoints, int* retArraySize,
                                          ptrList <snlSurfLocnGuess>* guesses, double convergTol,
                                          double normTol, int maxPass, bool retNonConverged,
                                          bool noCull, int numVelocity, int numNewton )
{
    //! Refine parametric location of given points
    //  ------------------------------------------
    //! @param points Array of snlPoints to find on surface.
    //! @param numPoints Number of points being inverted. ie size of points array.
    //! @param retArraySize Size of array of surface locations that is returned.
    //! @param guesses Parametric location guesses for given points.
    //! @param convergTol If the difference between successive newton iterations converges and is
    //!                   below this value then the point is taken to be inverted.
    //! @param normTol How close to perpendicular the projection of the given point to the point
    //!                found gets to the surface tangents at the found point for convergence to be
    //!                successful.
    //! @param maxPass Maximum number of refinement passes allowed. Stops infinite loops if
    //!                projections don't converge.
    //! @param retNonConverged Return non converged points. Used for projection.
    //! @param noCull Do not cull guesses that converge to the same point.
    //! @param numVelocity Number of velocity iterations to perform per pass.
    //! @param numNewton Number of newton iterations to perform per pass.
    
    // Converge to points using multiple passes if necessary.

    for ( int pass = 0; pass < maxPass; pass ++ )
    {
        bool converged = convergeVelocity ( points, guesses, numVelocity, convergTol, normTol );

        if ( ! converged )
            converged = convergeNewton ( points, guesses, numNewton, convergTol, normTol );

        // If all guesses have been culled for a given point then reprocess one of them.

        snlSurfLocnGuess* guess = guesses -> first();

        for ( int index = 0; index < numPoints; index ++ )
        {
            bool allCulled = true;

            snlSurfLocnGuess* bestGuess = guess;

            if ( guess )
            {
                while ( guess )
                {
    
                    if ( guess -> origPtIndex != index ) break;
    
                    if ( ! guess -> culled )
                        allCulled = false;
                    else if ( guess -> dist < bestGuess -> dist )
                        bestGuess = guess;

                    guess = guesses -> next();
                }
    
                if ( allCulled )
                {
                    // Force another pass to be performed.
    
                    bestGuess -> ignoreParamBounds = true;
                    bestGuess -> culled = false;
                    converged = false;
                }
            }
        }
    }

    // Cull duplicate guesses that have converged to the same point. Keep best one.

    snlSurfLocnGuess* guess = guesses -> first();

    int pointIndex = -1;

    ptrList <snlSurfLocnGuess> pointGuesses;

    while ( guess && ! noCull )
    {
        if ( ! guess -> culled && ( guess -> converged || retNonConverged ) )
        {
            if ( guess -> origPtIndex != pointIndex )
            {
                // Move onto next point indexes guesses.
                
                pointIndex = guess -> origPtIndex;

                pointGuesses.clear();

                pointGuesses.append ( guess, false );
            }
            else
            {
                // Compare point with others found. Take best one.

                snlSurfLocnGuess* ptGuess = pointGuesses.first();

                while ( ptGuess )
                {
                    if ( ! ptGuess -> culled )
                    {
                        // If other guesses param bounds overlap current guess
                        // then assume they have converged to the same point.

                        if ( guess -> paramU >= ptGuess -> minU
                             && guess -> paramU <= ptGuess -> maxU )
                        {
                            if ( guess -> paramV >= ptGuess -> minV
                                 && guess -> paramV <= ptGuess -> maxV )
                            {
                                // Both guesses are in same param zone. Cull guess with largest
                                // distance.

                                if ( guess -> dist > ptGuess -> dist )
                                    guess -> culled = true;
                                else
                                    ptGuess -> culled = true;
                            }
                        }
                    }
                
                    ptGuess = pointGuesses.next();
                }

                pointGuesses.append ( guess, false );
            }
        }
    
        guess = guesses -> next();
    }

    // Assemble best found points.

    // Get total number of points found.

    int totalCount = 0;

    guess = guesses -> first();

    while ( guess )
    {
        if ( ! guess -> culled && ( guess -> converged || retNonConverged ) )
            totalCount ++;

        guess = guesses -> next();
    }

    snlSurfLocn* retLocns = new snlSurfLocn [ totalCount ];

    int index = 0;

    guess = guesses -> first();

    while ( guess )
    {
        if ( ( retNonConverged || guess -> converged ) && ! guess -> culled )
        {
            retLocns [ index ].paramU = guess -> paramU;
            retLocns [ index ].paramV = guess -> paramV;
            retLocns [ index ].pt = guess -> pt;
            retLocns [ index ].dist = sqrt ( guess -> dist );
            retLocns [ index ].origPtIndex = guess -> origPtIndex;
            retLocns [ index ].cos = guess -> cos;

            index ++;
        }

        guess = guesses -> next();
    }

    // Clean up and return.
    
    delete guesses;

    if ( retArraySize )
        *retArraySize = totalCount;

    return retLocns;
}

bool snlSurface::convergeVelocity ( snlPoint* convergToPts, ptrList <snlSurfLocnGuess>* guesses,
                                    int numIterations, double convergTol, double normTol )
{
    //! Converge guesses to given points using velocity technique.
    //  ----------------------------------------------------------
    //! @param convergToPts Array of points that are to be converged to. ie Points to be found.
    //! @param guesses List of current guesses to be converged.
    //! @param numIterations Maximum number of convergence iterations to perform.
    //! @param convergTol Maximum distance between point and guess allowed for convergence to be
    //!                   successful.
    //! @param normTol How close to perpendicular the projection of the given point to the guessed
    //!                point gets to the surface tangents at the guessed point for convergence to be
    //!                successful.

    cout.precision ( 16 );
    
    double convTolSqrd = convergTol * convergTol;
    
    knot minU = knotVectU -> min();
    knot maxU = knotVectU -> max();

    knot minV = knotVectV -> min();
    knot maxV = knotVectV -> max();
    
    snlSurfLocnGuess* guess = guesses -> first();

    snlVector velocityU, velocityV;
    snlPoint evalPoint;

    bool converged = true;

    while ( guess )
    {
        if ( ! guess -> converged && ! guess -> culled )
        {
            if ( guess -> dist >= convTolSqrd )
            {
                // Generate new guess.

                snlSurfLocnGuess newGuess = *guess;

                for ( int iteration = 0; iteration < numIterations; iteration ++ )
                {
                    // Get Initial velocities at guessed point

                    if ( ! iteration )
                        velocities ( newGuess.paramU, newGuess.paramV, evalPoint, velocityU,
                                     velocityV );
    
                    // Calculate new parameters based on velocities.
    
                    snlVector guessToPt ( newGuess.pt, convergToPts [ newGuess.origPtIndex ] );
    
                    double lengthU = velocityU.length();

                    if ( lengthU == 0.0 ) break;

                    double distU = guessToPt.dot ( velocityU ) / lengthU;
                    knot deltaU = distU / lengthU;

                    double lengthV = velocityV.length();

                    if ( lengthV == 0.0 ) break;
                    
                    double distV = guessToPt.dot ( velocityV ) / lengthV;
                    knot deltaV =  distV / lengthV;

                    knot newU = newGuess.paramU + deltaU;
                    knot newV = newGuess.paramV + deltaV;

                    if ( newU < minU ) newU = minU;
                    if ( newU > maxU ) newU = maxU;
    
                    if ( newV < minV ) newV = minV;
                    if ( newV > maxV ) newV = maxV;

                    // If the distance travelled exceeds the required distance by more than 10% then
                    // recalculate the param deltas.

                    evalPoint = eval ( newU, newV );

                    double newDist = evalPoint.distSqrd ( convergToPts [ newGuess.origPtIndex ] );
                    

                    int loopCount = -1;

                    while ( newDist > newGuess.dist * 1.1 )
                    {
                        // Calculate new parameters based on actual distance travelled.
                        
                        loopCount ++;

                        snlVector newGuessToPt ( newGuess.pt, evalPoint );

                        double newDistU = newGuessToPt.dot ( velocityU ) / lengthU;
                        double newDistV = newGuessToPt.dot ( velocityV ) / lengthV;

                        if ( newDistU == 0.0 && newDistV == 0.0 )
                            break;

                        double loopAdj = 1.0 / (double) ( loopCount + 1 );

                        knot newAdjustU;
                        knot newAdjustV;

                        if ( newDistU != 0.0 )
                            newAdjustU = newGuess.paramU + ( ( distU * loopAdj / newDistU )
                                         * ( newU - newGuess.paramU ) );
                        else
                            newAdjustU = newU;
                        
                        if ( newDistV != 0.0 )
                            newAdjustV = newGuess.paramV + ( ( distV * loopAdj / newDistV )
                                         * ( newV - newGuess.paramV ) );
                        else
                            newAdjustV = newV;

                        // Do out of bounds check.

                        if ( newAdjustU < minU ) newAdjustU = minU;
                        if ( newAdjustU > maxU ) newAdjustU = maxU;
        
                        if ( newAdjustV < minV ) newAdjustV = minV;
                        if ( newAdjustV > maxV ) newAdjustV = maxV;

                        // Test for infinite loop.

                        if ( newU == newAdjustU && newV == newAdjustV )
                            break;

                        newU = newAdjustU;
                        newV = newAdjustV;

                        // Re-evaluate parameters.
                        
                        evalPoint = eval ( newU, newV );

                        newDist = evalPoint.distSqrd ( convergToPts [ newGuess.origPtIndex ] );
                    }
    
                    // Store point and velocities that match new params.
    
                    velocities ( newU, newV, evalPoint, velocityU, velocityV );

                    newGuess.paramU = newU;
                    newGuess.paramV = newV;

                    newGuess.pt = evalPoint;
    
                    newGuess.dist = evalPoint.distSqrd ( convergToPts [ newGuess.origPtIndex ] );

                    // Test for guess going out of parametric bounds.

                    if ( ! newGuess.ignoreParamBounds )
                    {
                        if ( newGuess.paramU < newGuess.minU || newGuess.paramU > newGuess.maxU )
                        {
                            guess -> culled = true;
                            break;
                        }
        
                        if ( newGuess.paramV < newGuess.minV || newGuess.paramV > newGuess.maxV )
                        {
                            guess -> culled = true;
                            break;
                        }
                    }
    
                    // Test for convergence.
    
                    if ( newGuess.dist < convTolSqrd )
                    {
                        newGuess.converged = true;
                        break;
                    }

                    snlVector projToSurf ( convergToPts [ newGuess.origPtIndex ], evalPoint );
                    
                    basis cosU = projToSurf.calcAbsCos ( velocityU );
                    
                    basis cosV = projToSurf.calcAbsCos ( velocityV );

                    newGuess.cos = cosU > cosV ? cosU : cosV;

                    if ( cosU <= normTol && cosV <= normTol )
                    {
                        newGuess.converged = true;
                        break;
                    }
                }
    
                // If new guess is better than old and hasn't been culled then replace old guess.
                
                if ( ! guess -> culled )
                {
                    // If parameters haven't changed over all iterations then set as converged.
                    
                    if ( guess -> paramU == newGuess.paramU && guess -> paramV == newGuess.paramV )
                        newGuess.converged = true;
                   
                    *guess = newGuess;
                }
            }
            else
                guess -> converged = true;

            // Test for guess going out of parametric bounds.
            if ( ! guess -> ignoreParamBounds && guess -> culled )
            {
                if ( guess -> paramU < guess -> minU || guess -> paramU > guess -> maxU )
                    guess -> culled = true;
    
                if ( guess -> paramV < guess -> minV || guess -> paramV > guess -> maxV )
                    guess -> culled = true;
            }
        }

        if ( ! guess -> converged && ! guess -> culled ) converged = false;

        guess = guesses -> next();
    }
    
    return converged;
}

bool snlSurface::convergeNewton ( snlPoint* convergToPts, ptrList <snlSurfLocnGuess>* guesses,
                                  int numIterations, double convergTol, double normTol )
{
    //! Converge guesses to given points using Newton iteration
    //  -------------------------------------------------------
    //! @param convergToPts Array of points that are to be converged to. ie Points to be found.
    //! @param guesses List of current guesses to be converged.
    //! @param numIterations Maximum number of convergence iterations to perform.
    //! @param convergTol Maximum distance between point and guess allowed for convergence to be
    //!                   successful.
    //! @param normTol Cosine of angle between normal to surface at guess and projection from
    //!                guess to given point. If cosine is below this angle then iterations stop.

    double convTolSqrd = convergTol * convergTol;
    
    knot minU = knotVectU -> min();
    knot maxU = knotVectU -> max();

    knot minV = knotVectV -> min();
    knot maxV = knotVectV -> max();

    snlSurfLocnGuess* guess = guesses -> first();

    snlPoint* derivs;

    bool converged = true;

    while ( guess )
    {
        derivs = 0;

        if ( ! guess -> converged && ! guess -> culled )
        {
            snlSurfLocnGuess newGuess = *guess;

            if ( guess -> dist >= convTolSqrd )
            {
                for ( int iteration = 0; iteration < numIterations; iteration ++ )
                {
                    // Get 1st and 2nd derivatives.

                    if ( ! derivs )
                        derivs = evalDerivs ( newGuess.paramU, newGuess.paramV, 2, 2 );

                    // Generate next Newton approximation.

                    knot deltaU, deltaV;
                    
                    if ( degU > 1 && degV > 1 )
                    {
                        if ( ! newtonIterStepSurf ( derivs, convergToPts + newGuess.origPtIndex,
                                                    &deltaU, &deltaV ) )
                            break;  // Param deltas would have gone to infinity.
                    }
                    else
                    {
                        // At least one of the degrees is 1.

                        snlPoint uDerivs [ 3 ];

                        uDerivs [ 0 ] = derivs [ 0 ];
                        uDerivs [ 1 ] = derivs [ 3 ];
                        uDerivs [ 2 ] = derivs [ 6 ];

                        if ( degU == 1 )
                        {
                            if ( ! lineIterStepCurve ( uDerivs, convergToPts + newGuess.origPtIndex,
                                                       &deltaU ) )
                                break;
                        }
                        else
                        {
                            if ( ! newtonIterStepCurve ( uDerivs,
                                                         convergToPts + newGuess.origPtIndex,
                                                         &deltaU ) )
                                break;
                        }

                        if ( degV == 1 )
                        {
                            if ( ! lineIterStepCurve ( derivs, convergToPts + newGuess.origPtIndex,
                                                       &deltaV ) )
                                break;
                        }
                        else
                        {
                            if ( ! newtonIterStepCurve ( derivs, convergToPts +
                                                         newGuess.origPtIndex, &deltaV ) )
                                break;
                        }
                    }

                    // Calcualte and clamp new parameters.

                    knot newU = newGuess.paramU + deltaU;
                    knot newV = newGuess.paramV + deltaV;

                    if ( newU < minU ) newU = minU;
                    if ( newU > maxU ) newU = maxU;
    
                    if ( newV < minV ) newV = minV;
                    if ( newV > maxV ) newV = maxV;

                    // If parameters haven't changed between iterations then guess has converged.

                    if ( newU == newGuess.paramU && newV == newGuess.paramV )
                    {
                        newGuess.converged = true;
                        break;
                    }
                    
                    newGuess.paramU = newU;
                    newGuess.paramV = newV;

                    // Evaluate new parameters.

                    delete[] derivs;
                    derivs = evalDerivs ( newGuess.paramU, newGuess.paramV, 2, 2 );

                    newGuess.pt = derivs [ 0 ];
    
                    newGuess.dist = derivs [ 0 ].distSqrd ( convergToPts [ newGuess.origPtIndex ] );

                    // Check for distance and cosine tolerances.

                    if ( newGuess.dist < convTolSqrd )
                    {
                        newGuess.converged = true;
                        break;
                    }

                    snlVector velocityU ( derivs [ 3 ] );
                    snlVector velocityV ( derivs [ 1 ] );
                    
                    snlVector projToSurf ( convergToPts [ newGuess.origPtIndex ], newGuess.pt );
                    
                    basis cosU = projToSurf.calcAbsCos ( velocityU );
                    
                    basis cosV = projToSurf.calcAbsCos ( velocityV );

                    newGuess.cos = cosU > cosV ? cosU : cosV;

                    if ( cosU <= normTol && cosV <= normTol )
                    {
                        newGuess.converged = true;
                        break;
                    }
                }
            }
            else
                guess -> converged = true;

            if ( ! guess -> culled && newGuess.dist < guess -> dist )
                *guess = newGuess;
        }

        // Test for guess going out of parametric bounds.
        
        if ( ! guess -> ignoreParamBounds && ! guess -> culled )
        {
            if ( guess -> paramU < guess -> minU || guess -> paramU > guess -> maxU )
                guess -> culled = true;
    
            if ( guess -> paramV < guess -> minV || guess -> paramV > guess -> maxV )
                guess -> culled = true;
        }

        if ( ! guess -> converged && ! guess -> culled ) converged = false;

        guess = guesses -> next();

        // Clean up.

        if ( derivs ) delete[] derivs;
    }

    return converged;
}

ptrList <snlSurfLocnGuess>* snlSurface::guessInvLocation ( snlPoint* points, int numPoints,
                                                           bool* pointMask, int granU,
                                                           int granV )
{
    //! Guess parametric location of given points.
    //  ------------------------------------------
    //! @param points Array of points to find matches with.
    //! @param numPoints Number of points in array.
    //! @param pointMask Array specifying which points to process. Corresponding index to points
    //!                  array. Must be true to process.
    //! @param granU Granularity of each span in U direction.
    //! @param granV Granularity of each span in V direction.
    //!
    //! @return Array of surface location guess structs. Caller owns this array.
    //!
    //! Notes:       This function will return one guess per parametric span.

    int     index;

    int numSpansU = knotVectU -> getNumSpans();
    int numSpansV = knotVectV -> getNumSpans();

    int numEvalU = granU * numSpansU + 1;
    int numEvalV = granV * numSpansV + 1;

    // Pre-calculate parametric positions.

    knot* paramU = new knot [ numEvalU ];
    int* spanU = new int [ numEvalU ];

    knot* paramV = new knot [ numEvalV ];
    int* spanV = new int [ numEvalV ];

    // U direction.

    int cSpan = knotVectU -> getFirstSpan();

    knot paramStart = knotVectU -> val ( cSpan );

    cSpan = knotVectU -> getNextSpan ( cSpan );

    knot paramEnd;

    if ( cSpan )
        paramEnd = knotVectU -> val ( cSpan );
    else
        paramEnd = knotVectU -> max();
     

    int numSteps;

    int paramIndex = 0;

    basis param;

    for ( int span = 0; span < numSpansU; span ++ )
    {
        basis paramStep = ( paramEnd - paramStart ) / (double) granU;

        param = paramStart;

        if ( span )
        {
            numSteps = granU;
            param += paramStep;
        }
        else
        {
            numSteps = granU + 1;
        }
    
        // Generate params for span.
        
        for ( index = 0; index < numSteps; index ++ )
        {
            paramU [ paramIndex ] = param;

            spanU [ paramIndex ++ ] = span;
    
            param += paramStep;
    
            if ( param > paramEnd ) param = paramEnd;  // Round off error trap.
        }

        paramStart = paramEnd;

        cSpan = knotVectU -> getNextSpan ( cSpan );

        if ( cSpan )
            paramEnd = knotVectU -> val ( cSpan );
        else
            paramEnd = knotVectU -> max();
    }

    // V direction.
    
    cSpan = knotVectV -> getFirstSpan();

    paramStart = knotVectV -> val ( cSpan );

    cSpan = knotVectV -> getNextSpan ( cSpan );

    if ( cSpan )
        paramEnd = knotVectV -> val ( cSpan );
    else
        paramEnd = knotVectV -> max();

    paramIndex = 0;

    for ( int span = 0; span < numSpansV; span ++ )
    {
        basis paramStep = ( paramEnd - paramStart ) / (double) granV;

        param = paramStart;

        if ( span )
        {
            numSteps = granV;
            param += paramStep;
        }
        else
        {
            numSteps = granV + 1;
        }
    
        for ( index = 0; index < numSteps; index ++ )
        {
            paramV [ paramIndex ] = param;

            spanV [ paramIndex ++ ] = span;
    
            param += paramStep;
    
            if ( param > paramEnd ) param = paramEnd;  // Round off error trap.
        }

        paramStart = paramEnd;

        cSpan = knotVectV -> getNextSpan ( cSpan );

        if ( cSpan )
            paramEnd = knotVectV -> val ( cSpan );
        else
            paramEnd = knotVectV -> max();
    }

    // Pre-evaluate basis functions.

    basis** basisU = new basis* [ numEvalU ];
    basis** basisV = new basis* [ numEvalV ];

    for ( index = 0; index < numEvalU; index ++ )
        basisU [ index ] = knotVectU -> evalBasis ( paramU [ index ] );

    for ( index = 0; index < numEvalV; index ++ )
        basisV [ index ] = knotVectV -> evalBasis ( paramV [ index ] );

    // Evaluate surface points and vectors.

    int numEvalPts = numEvalU * numEvalV;

    snlPoint* evalPts = new snlPoint [ numEvalPts ];

    index = 0;

    for ( int indexU = 0; indexU < numEvalU; indexU ++ )
    {
        for ( int indexV = 0; indexV < numEvalV; indexV ++ )
        {
            evalPts [ index ] = eval ( paramU [ indexU ], paramV [ indexV ], basisU [ indexU ],
                                       basisV [ indexV ] );
                          
            index ++;
        }
    }

    // Calculate bounding distances. They are used for culling improbable guesses.

    double* boundDist = new double [ numEvalPts ];

    index = 0;

    for ( int indexU = 0; indexU < numEvalU; indexU ++ )
    {
        for ( int indexV = 0; indexV < numEvalV; indexV ++ )
        {
            double maxDist = 0;
            double dist;
            
            if ( indexU > 0 )
            {
                // U before current index.

                if ( indexV > 0 )
                {
                    dist = evalPts [ index - numEvalV - 1 ].distSqrd ( evalPts [ index ] );
                    if ( dist > maxDist ) maxDist = dist;
                }

                if ( indexV < numEvalV - 1 )
                {
                    dist = evalPts [ index - numEvalV + 1 ].distSqrd ( evalPts [ index ] );
                    if ( dist > maxDist ) maxDist = dist;
                }
            }

            if ( indexU < numEvalU - 1 )
            {
                // U after current index.

                if ( indexV > 0 )
                {
                    dist = evalPts [ index + numEvalV - 1 ].distSqrd ( evalPts [ index ] );
                    if ( dist > maxDist ) maxDist = dist;
                }

                if ( indexV < numEvalV - 1 )
                {
                    dist = evalPts [ index + numEvalV + 1 ].distSqrd ( evalPts [ index ] );
                    if ( dist > maxDist ) maxDist = dist;
                }
            }

            boundDist [ index ] = maxDist;

            index ++;
        }
    }

    // Compare given points to evaluated points. One entry per span.

    int numSpans = numSpansU * numSpansV;

    int numSpanPoints = numPoints * numSpans;

    // Two dimensional arrays [ given point index ] [ span index ].
    
    int* uIndexes = new int [ numSpanPoints ];  // NOT control point indexes.
    int* vIndexes = new int [ numSpanPoints ];
    double* distances = new double [ numSpanPoints ];
    bool* populated = new bool [ numSpanPoints ];  // If false distances have not been populated.

    for ( index = 0; index < numSpanPoints; index ++ )
        populated [ index ] = false;

    double distSqrd;

    int numToReturn = 0;

    index = 0;

    for ( int indexU = 0; indexU < numEvalU; indexU ++ )
    {
        for ( int indexV = 0; indexV < numEvalV; indexV ++ )
        {
            int spanIndex = spanU [ indexU ] * numSpansV + spanV [ indexV ];

            for ( int ptIndex = 0; ptIndex < numPoints; ptIndex ++ )
            {
                if ( pointMask [ ptIndex ] )
                {
                    int ptIndexOffset = ptIndex * numSpans;
                    
                    distSqrd = evalPts [ index ].distSqrd ( points [ ptIndex ] );

                    // Only process span point if distance is within probable bounds.

                    if ( distSqrd < boundDist [ index ] )
                    {
                        if ( distances [ ptIndexOffset + spanIndex ] > distSqrd
                             || ! populated [ ptIndexOffset + spanIndex ])
                        {
                            if ( ! populated [ ptIndexOffset + spanIndex ] )
                            {
                                numToReturn ++;
                                populated [ ptIndexOffset + spanIndex ] = true;
                            }
                            
                            distances [ ptIndexOffset + spanIndex ] = distSqrd;
                            uIndexes [ ptIndexOffset + spanIndex ] = indexU;
                            vIndexes [ ptIndexOffset + spanIndex ] = indexV;
                        }
                    }
                }
            }

            index ++;
        }
    }

    // Build array of surface locations to return.

    ptrList <snlSurfLocnGuess>* retList = new ptrList <snlSurfLocnGuess>;

    int indexU, indexV;

    index = 0;

    for ( int ptIndex = 0; ptIndex < numPoints; ptIndex ++ )
    {
        for ( int spanIndex = 0; spanIndex < numSpans; spanIndex ++ )
        {
            if ( populated [ index ] )
            {
                snlSurfLocnGuess* guessLocn = new snlSurfLocnGuess;
                
                indexU = uIndexes [ index ];
                indexV = vIndexes [ index ];
    
                guessLocn -> paramU = paramU [ indexU ];
                guessLocn -> paramV = paramV [ indexV ];
    
                guessLocn -> pt = evalPts [ indexU * numEvalV + indexV ];

                guessLocn -> dist = distances [ index ];

                guessLocn -> origPtIndex = ptIndex;

                guessLocn -> spanNumber = spanIndex;

                if ( indexU > 0 )
                    guessLocn -> minU = paramU [ indexU - 1 ];
                else
                    guessLocn -> minU = paramU [ indexU ];

                if ( indexU < numEvalU - 1 )
                    guessLocn -> maxU = paramU [ indexU + 1 ];
                else
                    guessLocn -> maxU = paramU [ indexU ];

                if ( indexV > 0 )
                    guessLocn -> minV = paramV [ indexV - 1 ];
                else
                    guessLocn -> minV = paramV [ indexV ];
                    
                if ( indexV < numEvalV -1 )
                    guessLocn -> maxV = paramV [ indexV + 1 ];
                else
                    guessLocn -> maxV = paramV [ indexV ];

                guessLocn -> culled = false;
                guessLocn -> ignoreParamBounds = false;
                guessLocn -> converged = false;

                retList -> append ( guessLocn, true );
            }

            index ++;
        }
    }

    // Clean up

    delete[] uIndexes;
    delete[] vIndexes;
    delete[] distances;
    delete[] populated;

    delete[] evalPts;
    delete[] boundDist;

    delete[] paramU;
    delete[] paramV;

    delete[] spanU;
    delete[] spanV;

    for ( int index = 0; index < numEvalU; index ++ )
        delete[] basisU [ index ];

    for ( int index = 0; index < numEvalV; index ++ )
        delete[] basisV [ index ];

    delete[] basisU;
    delete[] basisV;

    return retList;
}

ptrList <snlSurfLocnGuess>* snlSurface::guessProjLocation ( snlPoint* points, int numPoints,
                                                            bool* pointMask )
{
    //! Guess parametric location of given points.
    //  ------------------------------------------
    //! @param points Array of points to find matches with.
    //! @param numPoints Number of points in array.
    //! @param pointMask Array specifying which points to process. Corresponding index to points
    //!                  array. Must be true to process.
    //!
    //! @return List of surface location guess structs. Caller owns this list.
    //!
    //! @par Notes: Function expects all spans to be convex Bezier segments. It will _not_ work
    //!      if this is not so.

    int     index;

    int numSpansU = knotVectU -> getNumSpans();
    int numSpansV = knotVectV -> getNumSpans();

    int numEvalU = numSpansU + 1;
    int numEvalV = numSpansV + 1;

    // Pre-calculate parametric positions.

    knot* paramU = new knot [ numEvalU ];

    knot* paramV = new knot [ numEvalV ];

    // U Direction.

    int cSpan = knotVectU -> getFirstSpan();

    for ( int evalIndex = 0; evalIndex < numEvalU - 1; evalIndex ++ )
    {
        paramU [ evalIndex ] = knotVectU -> val ( cSpan );

        cSpan = knotVectU -> getNextSpan ( cSpan );
    }

    paramU [ numEvalU - 1 ] = knotVectU -> max();

    // V Direction.

    cSpan = knotVectV -> getFirstSpan();

    for ( int evalIndex = 0; evalIndex < numEvalV - 1; evalIndex ++ )
    {
        paramV [ evalIndex ] = knotVectV -> val ( cSpan );

        cSpan = knotVectV -> getNextSpan ( cSpan );
    }

    paramV [ numEvalV - 1 ] = knotVectV -> max();

    // Evaluate surface points and velocities.
    // Because the surface is segmented into Bezier segements the segment
    // corners are the control points and the velocities are calculated
    // directly from the control points without the need for basis functions.
    
    int numEvalPts = numEvalU * numEvalV;

    snlPoint* evalPts = new snlPoint [ numEvalPts ];

    snlVector* edgeNormalU = new snlVector [ 4 ];
    snlVector* edgeNormalV = new snlVector [ 4 ];

    bool* hasGuess = new bool [ numPoints ];
    snlSurfLocnGuess** lastGuess = new snlSurfLocnGuess* [ numPoints ];

    for ( int ptIndex = 0; ptIndex < numPoints; ptIndex ++ )
        hasGuess [ ptIndex ] = false;

    index = 0;
    
    int ctrlPtIndex;

    const snlCtrlPoint* ctrlPts = ctrlPtNet -> getCtrlPts();

    int vSize = sizeV();

    for ( int indexU = 0; indexU < numEvalU; indexU ++ )
    {
        ctrlPtIndex = degU * indexU * vSize;
        
        for ( int indexV = 0; indexV < numEvalV; indexV ++ )
        {
            evalPts [ index ] = ctrlPts [ ctrlPtIndex ];
            
            index ++;

            ctrlPtIndex += degV;
        }
    }

    ptrList <snlSurfLocnGuess>* tmpList = new ptrList <snlSurfLocnGuess>;  // List of out of order
                                                                           // guesses.

    int spanEvalIndex = 0;

    int spanNum = 0;

    for ( int spanU = 0; spanU < numSpansU; spanU ++ )
    {
        for ( int spanV = 0; spanV < numSpansV; spanV ++ )
        {
            // Calculate eight edge normals per segment. 4 per parametric direction.
            //
            // Edge normal orientation per segment:
            // 
            // 1 ---- 2 --- V
            // |      |
            // 3 ---- 4
            // |
            // U

            snlVector velocityU;
            snlVector velocityV;
            snlVector normal;
            snlVector edge;

            int baseIndex = spanU * degU * vSize + spanV * degV;

            // Calculate and orient first set of edge normals

            velocityU.calc ( ctrlPts [ baseIndex ], ctrlPts [ baseIndex + vSize ] );
            velocityV.calc ( ctrlPts [ baseIndex ], ctrlPts [ baseIndex + 1 ] );
            
            normal.crossProduct ( velocityU, velocityV );

            edge.calc ( evalPts [ spanEvalIndex ], evalPts [ spanEvalIndex + 1 ] );
            edgeNormalU [ 0 ].crossProduct ( normal, edge );

            snlVector orient ( evalPts [ spanEvalIndex ], evalPts [ spanEvalIndex + numEvalV ] );
            if ( edgeNormalU [ 0 ].dot ( orient ) < 0.0 ) edgeNormalU [ 0 ] *= - 1.0;

            if ( edgeNormalU [ 0 ].dot ( velocityV ) < 0.0 )
            {
                // If velocity vectors are outside of edge then use them for edge normal calculation
                // instead.
                
                edgeNormalU [ 0 ].crossProduct ( normal, velocityV );
                if ( edgeNormalU [ 0 ].dot ( orient ) < 0.0 ) edgeNormalU [ 0 ] *= - 1.0;
            }

            edge.calc ( evalPts [ spanEvalIndex ], evalPts [ spanEvalIndex + numEvalV ] );
            edgeNormalV [ 0 ].crossProduct ( normal, edge );

            orient.calc ( evalPts [ spanEvalIndex ], evalPts [ spanEvalIndex + 1 ] );
            if ( edgeNormalV [ 0 ].dot ( orient ) < 0.0 ) edgeNormalV [ 0 ] *= - 1.0;

            if ( edgeNormalV [ 0 ].dot ( velocityU ) < 0.0 )
            {
                edgeNormalV [ 0 ].crossProduct ( normal, velocityU );
                if ( edgeNormalV [ 0 ].dot ( orient ) < 0.0 ) edgeNormalV [ 0 ] *= - 1.0;
            }

            // Calculate and orient second set of edge normals

            velocityU.calc ( ctrlPts [ baseIndex + degV ], ctrlPts [ baseIndex + degV + vSize ] );
            velocityV.calc ( ctrlPts [ baseIndex + degV ], ctrlPts [ baseIndex + degV - 1 ] );

            normal.crossProduct ( velocityU, velocityV );

            edge.calc ( evalPts [ spanEvalIndex + 1 ], evalPts [ spanEvalIndex ] );
            edgeNormalU [ 1 ].crossProduct ( normal, edge );

            orient.calc ( evalPts [ spanEvalIndex + 1 ], evalPts [ spanEvalIndex + numEvalV + 1 ] );
            if ( edgeNormalU [ 1 ].dot ( orient ) < 0.0 ) edgeNormalU [ 1 ] *= - 1.0;

            if ( edgeNormalU [ 1 ].dot ( velocityV ) < 0.0 )
            {
                edgeNormalU [ 1 ].crossProduct ( normal, velocityV );
                if ( edgeNormalU [ 1 ].dot ( orient ) < 0.0 ) edgeNormalU [ 1 ] *= - 1.0;
            }
            
            edge.calc ( evalPts [ spanEvalIndex + 1 ], evalPts [ spanEvalIndex + numEvalV + 1] );
            edgeNormalV [ 1 ].crossProduct ( normal, edge );

            orient.calc ( evalPts [ spanEvalIndex + 1 ], evalPts [ spanEvalIndex ] );
            if ( edgeNormalV [ 1 ].dot ( orient ) < 0.0 ) edgeNormalV [ 1 ] *= - 1.0;

            if ( edgeNormalV [ 1 ].dot ( velocityU ) < 0.0 )
            {
                edgeNormalV [ 1 ].crossProduct ( normal, velocityU );
                if ( edgeNormalV [ 1 ].dot ( orient ) < 0.0 ) edgeNormalV [ 1 ] *= - 1.0;
            }

            baseIndex += degU * vSize;

            // Calculate and orient third set of edge normals

            velocityU.calc ( ctrlPts [ baseIndex ], ctrlPts [ baseIndex - vSize ] );
            velocityV.calc ( ctrlPts [ baseIndex ], ctrlPts [ baseIndex + 1 ] );

            normal.crossProduct ( velocityU, velocityV );

            edge.calc ( evalPts [ spanEvalIndex + numEvalV ],
                        evalPts [ spanEvalIndex + numEvalV + 1 ] );

            edgeNormalU [ 2 ].crossProduct ( normal, edge );

            orient.calc ( evalPts [ spanEvalIndex + numEvalV ], evalPts [ spanEvalIndex ] );

            if ( edgeNormalU [ 2 ].dot ( orient ) < 0.0 ) edgeNormalU [ 2 ] *= - 1.0;

            if ( edgeNormalU [ 2 ].dot ( velocityV ) < 0.0 )
            {
                edgeNormalU [ 2 ].crossProduct ( normal, velocityV );
                if ( edgeNormalU [ 2 ].dot ( orient ) < 0.0 ) edgeNormalU [ 2 ] *= - 1.0;
            }

            edge.calc ( evalPts [ spanEvalIndex + numEvalV ], evalPts [ spanEvalIndex ] );
            edgeNormalV [ 2 ].crossProduct ( normal, edge );

            orient.calc ( evalPts [ spanEvalIndex + numEvalV ],
                          evalPts [ spanEvalIndex + numEvalV + 1 ] );

            if ( edgeNormalV [ 2 ].dot ( orient ) < 0.0 ) edgeNormalV [ 2 ] *= - 1.0;

            if ( edgeNormalV [ 2 ].dot ( velocityU ) < 0.0 )
            {
                edgeNormalV [ 2 ].crossProduct ( normal, velocityU );
                if ( edgeNormalV [ 2 ].dot ( orient ) < 0.0 ) edgeNormalV [ 2 ] *= - 1.0;
            }

            // Calculate and orient fourth set of edge normals

            velocityU.calc ( ctrlPts [ baseIndex + degV ], ctrlPts [ baseIndex + degV - vSize ] );
            velocityV.calc ( ctrlPts [ baseIndex + degV ], ctrlPts [ baseIndex + degV - 1 ] );

            normal.crossProduct ( velocityU, velocityV );

            edge.calc ( evalPts [ spanEvalIndex + numEvalV + 1 ],
                        evalPts [ spanEvalIndex + numEvalV ] );

            edgeNormalU [ 3 ].crossProduct ( normal, edge );

            orient.calc ( evalPts [ spanEvalIndex + numEvalV + 1 ], evalPts [ spanEvalIndex + 1 ] );

            if ( edgeNormalU [ 3 ].dot ( orient ) < 0.0 ) edgeNormalU [ 3 ] *= - 1.0;

            if ( edgeNormalU [ 3 ].dot ( velocityV ) < 0.0 )
            {
                edgeNormalU [ 3 ].crossProduct ( normal, velocityV );
                if ( edgeNormalU [ 3 ].dot ( orient ) < 0.0 ) edgeNormalU [ 3 ] *= - 1.0;
            }

            edge.calc ( evalPts [ spanEvalIndex + numEvalV + 1 ], evalPts [ spanEvalIndex + 1 ] );
            edgeNormalV [ 3 ].crossProduct ( normal, edge );

            orient.calc ( evalPts [ spanEvalIndex + numEvalV + 1 ],
                          evalPts [ spanEvalIndex + numEvalV ] );

            if ( edgeNormalV [ 3 ].dot ( orient ) < 0.0 ) edgeNormalV [ 3 ] *= - 1.0;

            if ( edgeNormalV [ 3 ].dot ( velocityU ) < 0.0 )
            {
                edgeNormalV [ 3 ].crossProduct ( normal, velocityU );
                if ( edgeNormalV [ 3 ].dot ( orient ) < 0.0 ) edgeNormalV [ 3 ] *= - 1.0;
            }

            // Step through each given point and check to see if it is probable that it belongs
            // to this patch. If it is, create a guess for it.

            knot minU = paramU [ spanU ];
            knot maxU = paramU [ spanU + 1 ];
            knot minV = paramV [ spanV ];
            knot maxV = paramV [ spanV + 1 ];

            for ( int ptIndex = 0; ptIndex < numPoints; ptIndex ++ )
            {
                bool withinEdge1_2 = false;
                bool withinEdge2_4 = false;
                bool withinEdge4_3 = false;
                bool withinEdge3_1 = false;
            
                // Corner 1.
                
                orient.calc ( evalPts [ spanEvalIndex ], points [ ptIndex ] );
                
                if ( orient.dot ( edgeNormalU [ 0 ] ) >= 0.0 )
                    withinEdge1_2 = true;

                if ( orient.dot ( edgeNormalV [ 0 ] ) >= 0.0 )
                    withinEdge3_1 = true;

                // Corner 2.

                orient.calc ( evalPts [ spanEvalIndex + 1 ], points [ ptIndex ] );

                if ( orient.dot ( edgeNormalU [ 1 ] ) >= 0.0 )
                    withinEdge1_2 = true;

                if ( orient.dot ( edgeNormalV [ 1 ] ) >= 0.0 )
                    withinEdge2_4 = true;

                // Corner 3.

                orient.calc ( evalPts [ spanEvalIndex + numEvalV ], points [ ptIndex ] );

                if ( orient.dot ( edgeNormalU [ 2 ] ) >= 0.0 )
                    withinEdge4_3 = true;
                
                if ( orient.dot ( edgeNormalV [ 2 ] ) >= 0.0 )
                    withinEdge3_1 = true;

                // Corner 4.
                    
                orient.calc ( evalPts [ spanEvalIndex + numEvalV + 1 ], points [ ptIndex ] );

                if ( orient.dot ( edgeNormalU [ 3 ] ) >= 0.0 )
                    withinEdge4_3 = true;
                
                if ( orient.dot ( edgeNormalV [ 3 ] ) >= 0.0 )
                    withinEdge2_4 = true;

                // If point is within all edges then a guess needs to be created.

                if ( withinEdge1_2 && withinEdge2_4 && withinEdge4_3 && withinEdge3_1 )
                {
                    snlSurfLocnGuess* newGuess = new snlSurfLocnGuess;

                    newGuess -> paramU = ( ( maxU - minU ) / 2 ) + minU;
                    newGuess -> paramV = ( ( maxV - minV ) / 2 ) + minV;
                    newGuess -> pt = eval ( newGuess -> paramU, newGuess -> paramV );
                    newGuess -> dist = ( newGuess -> pt ).distSqrd ( points [ ptIndex ] );
                    newGuess -> origPtIndex = ptIndex;
                    newGuess -> spanNumber = spanNum;
                    newGuess -> minU = minU;
                    newGuess -> maxU= maxU;
                    newGuess -> minV = minV;
                    newGuess -> maxV = maxV;
                    newGuess -> culled = false;
                    newGuess -> ignoreParamBounds = false;
                    newGuess -> converged = false;
    
                    tmpList -> append ( newGuess, false );
    
                    hasGuess [ ptIndex ] = true;
                    lastGuess [ ptIndex ] = newGuess;
                }
            }

            spanNum ++;

            spanEvalIndex ++;
        }

        spanEvalIndex ++;
    }

    // If a candidate span is not found for a given point then find closest evaluated point
    // but don't impose parametric bounds to the guess.
        
    for ( int ptIndex = 0; ptIndex < numPoints; ptIndex ++ )
    {
        if ( ! hasGuess [ ptIndex ] )
        {
            snlSurfLocnGuess* newGuess = new snlSurfLocnGuess;

            index = 0;
        
            for ( int indexU = 0; indexU < numEvalU; indexU ++ )
            {
                for ( int indexV = 0; indexV < numEvalV; indexV ++ )
                {
                    double distSqrd = points [ ptIndex ].distSqrd ( evalPts [ index ] );

                    if ( ( ! indexU && ! indexV ) || ( newGuess -> dist > distSqrd ) )
                    {
                        newGuess -> dist = distSqrd;
                        newGuess -> paramU = paramU [ indexU ];
                        newGuess -> paramV = paramV [ indexV ];
                        newGuess -> pt = evalPts [ index ];
                    }
                        
                    index ++;  
                }
            }

            // Fill in rest of guess data.

            newGuess -> origPtIndex = ptIndex;
            newGuess -> culled = false;
            newGuess -> ignoreParamBounds = true;
            newGuess -> converged = false;

            tmpList -> append ( newGuess, false );

            lastGuess [ ptIndex ] = newGuess;
        }
    }

    // Build properly ordered return list.

    ptrList <snlSurfLocnGuess>* retList = new ptrList <snlSurfLocnGuess>; // List of guesses to
                                                                          // return.

    for ( int ptIndex = 0; ptIndex < numPoints; ptIndex ++ )
    {
        snlSurfLocnGuess* guess = tmpList -> first();

        while ( guess )
        {
            if ( guess -> origPtIndex == ptIndex )
                retList -> append ( guess, true );
            
            guess = tmpList -> next();
        }
    }

    // Clean up and return.

    delete[] paramU;
    delete[] paramV;

    delete[] evalPts;

    delete[] edgeNormalU;
    delete[] edgeNormalV;

    delete[] hasGuess;
    delete[] lastGuess;

    delete tmpList;
    
    return retList;
}

ptrList <snlSurfLocnGuess>* snlSurface::guessFastProjLocation ( snlPoint* points, int numPoints, int numGuessesPerPt,
                                                                int granU, int granV )
{
    //! Guess parametric location of given points.
    //  ------------------------------------------
    //! @param points Array of points to find matches with.
    //! @param numPoints Number of points in array.
    //! @param numGuessesPerPt Number of guesses to return per point.
    //! @param granU Number of guesses per span.
    //! @param granV Number of guesses per span.
    //!
    //! @return List of surface location guess structs. Caller owns this lists.

    int index;

    int numSpansU = knotVectU -> getNumSpans();
    int numSpansV = knotVectV -> getNumSpans();

    int numEvalU = granU * numSpansU + 1;
    int numEvalV = granV * numSpansV + 1;

    // Pre-calculate parametric positions.

    knot* paramU = new knot [ numEvalU ];
    int* spanU = new int [ numEvalU ];

    knot* paramV = new knot [ numEvalV ];
    int* spanV = new int [ numEvalV ];

    // U direction.

    int cSpan = knotVectU -> getFirstSpan();

    knot paramStart = knotVectU -> val ( cSpan );

    cSpan = knotVectU -> getNextSpan ( cSpan );

    knot paramEnd;

    if ( cSpan )
        paramEnd = knotVectU -> val ( cSpan );
    else
        paramEnd = knotVectU -> max();
     

    int numSteps;

    int paramIndex = 0;

    basis param;

    for ( int span = 0; span < numSpansU; span ++ )
    {
        basis paramStep = ( paramEnd - paramStart ) / (double) granU;

        param = paramStart;

        if ( span )
        {
            numSteps = granU;
            param += paramStep;
        }
        else
        {
            numSteps = granU + 1;
        }
    
        // Generate params for span.
        
        for ( index = 0; index < numSteps; index ++ )
        {
            paramU [ paramIndex ] = param;

            spanU [ paramIndex ++ ] = span;
    
            param += paramStep;
    
            if ( param > paramEnd ) param = paramEnd;  // Round off error trap.
        }

        paramStart = paramEnd;

        cSpan = knotVectU -> getNextSpan ( cSpan );

        if ( cSpan )
            paramEnd = knotVectU -> val ( cSpan );
        else
            paramEnd = knotVectU -> max();
    }

    // V direction.
    
    cSpan = knotVectV -> getFirstSpan();

    paramStart = knotVectV -> val ( cSpan );

    cSpan = knotVectV -> getNextSpan ( cSpan );

    if ( cSpan )
        paramEnd = knotVectV -> val ( cSpan );
    else
        paramEnd = knotVectV -> max();

    paramIndex = 0;

    for ( int span = 0; span < numSpansV; span ++ )
    {
        basis paramStep = ( paramEnd - paramStart ) / (double) granV;

        param = paramStart;

        if ( span )
        {
            numSteps = granV;
            param += paramStep;
        }
        else
        {
            numSteps = granV + 1;
        }
    
        for ( index = 0; index < numSteps; index ++ )
        {
            paramV [ paramIndex ] = param;

            spanV [ paramIndex ++ ] = span;
    
            param += paramStep;
    
            if ( param > paramEnd ) param = paramEnd;  // Round off error trap.
        }

        paramStart = paramEnd;

        cSpan = knotVectV -> getNextSpan ( cSpan );

        if ( cSpan )
            paramEnd = knotVectV -> val ( cSpan );
        else
            paramEnd = knotVectV -> max();
    }

    // Pre-evaluate basis functions.

    basis** basisU = new basis* [ numEvalU ];
    basis** basisV = new basis* [ numEvalV ];

    for ( index = 0; index < numEvalU; index ++ )
        basisU [ index ] = knotVectU -> evalBasis ( paramU [ index ] );

    for ( index = 0; index < numEvalV; index ++ )
        basisV [ index ] = knotVectV -> evalBasis ( paramV [ index ] );

    // Evaluate surface points and vectors.

    int numEvalPts = numEvalU * numEvalV;

    snlPoint* evalPts = new snlPoint [ numEvalPts ];

    index = 0;

    for ( int indexU = 0; indexU < numEvalU; indexU ++ )
    {
        for ( int indexV = 0; indexV < numEvalV; indexV ++ )
        {
            evalPts [ index ] = eval ( paramU [ indexU ], paramV [ indexV ], basisU [ indexU ],
                                       basisV [ indexV ] );
                          
            index ++;
        }
    }

    // Compare given points to evaluated points.

    int totalNumGuesses = numGuessesPerPt * numPoints;

    snlSurfLocnGuess** guesses = new snlSurfLocnGuess* [ totalNumGuesses ];

    for ( int guessIndex = 0; guessIndex < totalNumGuesses; guessIndex ++ )
        guesses [ guessIndex ] = 0;

    index = 0;

    for ( int indexU = 0; indexU < numEvalU; indexU ++ )
    {
        for ( int indexV = 0; indexV < numEvalV; indexV ++ )
        {
            for ( int ptIndex = 0; ptIndex < numPoints; ptIndex ++ )
            {
                double distSqrd = evalPts [ index ].distSqrd ( points [ ptIndex ] );

                // If no guesses in available guess position then add a new one.

                int guessOffset = ptIndex * numGuessesPerPt;

                bool guessInserted = false;

                for ( int guessIndex = 0; guessIndex < numGuessesPerPt; guessIndex ++ )
                {
                    if ( ! guesses [ guessIndex + guessOffset ] )
                    {
                        // Create new guess.
                        
                        snlSurfLocnGuess* newGuess = new snlSurfLocnGuess;
    
                        newGuess -> paramU = paramU [ indexU ];
                        newGuess -> paramV = paramV [ indexV ];
                        newGuess -> pt = evalPts [ index ];
                        newGuess -> dist = distSqrd;
                        newGuess -> origPtIndex = ptIndex;
                        newGuess -> spanNumber = - 1;
                        newGuess -> culled = false;
                        newGuess -> ignoreParamBounds = true;
                        newGuess -> converged = false;

                        guesses [ guessIndex + guessOffset ] = newGuess;

                        guessInserted = true;

                        break;
                    }
                }

                if ( ! guessInserted )
                {
                    // Find guess with largest distance.

                    int replaceIndex = guessOffset;
                    double dist = guesses [ guessOffset ] -> dist;
                    
                    for ( int guessIndex = 1; guessIndex < numGuessesPerPt; guessIndex ++ )
                    {
                        if ( guesses [ guessIndex + guessOffset ] -> dist > dist )
                        {
                            replaceIndex = guessIndex + guessOffset;
                            dist = guesses [ guessIndex + guessOffset ] -> dist;
                        }
                    }

                    if ( dist > distSqrd )
                    {
                        // Replace guess.
    
                        guesses [ replaceIndex ] -> paramU = paramU [ indexU ];
                        guesses [ replaceIndex ] -> paramV = paramV [ indexV ];
                        guesses [ replaceIndex ] -> pt = evalPts [ index ];
                        guesses [ replaceIndex ] -> dist = distSqrd;
                        guesses [ replaceIndex ] -> origPtIndex = ptIndex;
                    }
                }
            }

            index ++;
        }
    }

    // Assemble return list.

    ptrList <snlSurfLocnGuess>* retList = new ptrList <snlSurfLocnGuess>;  // List of guesses to return.

    for ( int guessIndex = 0; guessIndex < totalNumGuesses; guessIndex ++ )
    {
        if ( guesses [ guessIndex ] )
            retList -> append ( guesses [ guessIndex ], true );
    }

    // Clean up and return.

    delete[] evalPts;

    delete[] guesses;

    delete[] paramU;
    delete[] paramV;

    delete[] spanU;
    delete[] spanV;

    for ( int index = 0; index < numEvalU; index ++ )
        delete[] basisU [ index ];

    for ( int index = 0; index < numEvalV; index ++ )
        delete[] basisV [ index ];

    delete[] basisU;
    delete[] basisV;

    return retList;    
}

//ptrList <snlSurfLocnGuess>* snlSurface::guessProjLocation_triMethod ( snlPoint* points, int numPoints, bool* pointMask )
//{
//    //! Guess parametric location of given points using triangular decomposition.
//    //  -------------------------------------------------------------------------
//    //! @param points Array of points to find matches with.
//    //! @param numPoints Number of points in array.
//    //! @param pointMask Array specifying which points to process. Corresponding index to points
//    //!                  array. Must be true to process.
//    //!
//    //! @return List of surface location guess structs. Caller owns this list.
//
//    
//    
//}

int snlSurface::hasAmbigEdges ( sEdge* results, double tolerance )
{
    //! See if the surface has ambiguous edges
    //  --------------------------------------    
    //! @param results Array of sEdge. Should be size 4.
    //! @param tolerance Tolerance of edge detection.
    //!
    //! @return Number of ambiguous edges.

    snlPoint    evalPt [ 4 ];  // Current evaled non-homogeneous point.

    knot        min_u, max_u, min_v, max_v;

    min_u = knotVectU -> min();
    max_u = knotVectU -> max();
    min_v = knotVectV -> min();
    max_v = knotVectV -> max();

    // Generate points to process.

    knot mid_u = ( max_u - min_u ) / 2.0 + min_u;
    knot mid_v = ( max_v - min_v ) / 2.0 + min_v;

    // Min / Max U

    evalPt [ 0 ] = eval ( min_u, mid_v );

    evalPt [ 1 ] = eval ( max_u, mid_v );

    // Min / Max V

    evalPt  [ 2 ] = eval ( mid_u, min_v );

    evalPt [ 3 ] = eval ( mid_u, max_v );

    // Generate initial guesses. 3 Per edge.

    ptrList <snlSurfLocnGuess>* guessList = new ptrList <snlSurfLocnGuess>;

    snlSurfLocnGuess guessTemplate;
    guessTemplate.culled = false;
    guessTemplate.ignoreParamBounds = true;
    guessTemplate.converged = false;

    // Edge: Min U. Eval index 0.

    snlSurfLocnGuess* guess;

    // Max U.
    guess = new snlSurfLocnGuess;
    *guess = guessTemplate;
    guess -> paramU = max_u;
    guess -> paramV = mid_v;
    guess -> pt = evalPt [ 1 ];
    guess -> dist = evalPt [ 1 ].distSqrd ( evalPt [ 0 ] );
    guess -> origPtIndex = 0;
    guessList -> append ( guess, true );

    // Min V.

    guess = new snlSurfLocnGuess;
    *guess = guessTemplate;
    guess -> paramU = mid_u;
    guess -> paramV = min_v;
    guess -> pt = evalPt [ 2 ];
    guess -> dist = evalPt [ 2 ].distSqrd ( evalPt [ 0 ] );
    guess -> origPtIndex = 0;
    guessList -> append ( guess, true );

    // Max V.

    guess = new snlSurfLocnGuess;
    *guess = guessTemplate;
    guess -> paramU = mid_u;
    guess -> paramV = max_v;
    guess -> pt = evalPt [ 3 ];
    guess -> dist = evalPt [ 3 ].distSqrd ( evalPt [ 0 ] );
    guess -> origPtIndex = 0;
    guessList -> append ( guess, true );

    // Edge: Max U. Eval index 1.

    // Min U.

    guess = new snlSurfLocnGuess;
    *guess = guessTemplate;
    guess -> paramU = min_u;
    guess -> paramV = mid_v;
    guess -> pt = evalPt [ 0 ];
    guess -> dist = evalPt [ 0 ].distSqrd ( evalPt [ 1 ] );
    guess -> origPtIndex = 1;
    guessList -> append ( guess, true );

    // Min V.

    guess = new snlSurfLocnGuess;
    *guess = guessTemplate;
    guess -> paramU = mid_u;
    guess -> paramV = min_v;
    guess -> pt = evalPt [ 2 ];
    guess -> dist = evalPt [ 2 ].distSqrd ( evalPt [ 1 ] );
    guess -> origPtIndex = 1;
    guessList -> append ( guess, true );

    // Max V.

    guess = new snlSurfLocnGuess;
    *guess = guessTemplate;
    guess -> paramU = mid_u;
    guess -> paramV = max_v;
    guess -> pt = evalPt [ 3 ];
    guess -> dist = evalPt [ 3 ].distSqrd ( evalPt [ 1 ] );
    guess -> origPtIndex = 1;
    guessList -> append ( guess, true );

    // Edge: Min V. Eval index 2.

    // Max V.

    guess = new snlSurfLocnGuess;
    *guess = guessTemplate;
    guess -> paramU = mid_u;
    guess -> paramV = max_v;
    guess -> pt = evalPt [ 3 ];
    guess -> dist = evalPt [ 3 ].distSqrd ( evalPt [ 2 ] );
    guess -> origPtIndex = 2;
    guessList -> append ( guess, true );

    // Min U.

    guess = new snlSurfLocnGuess;
    *guess = guessTemplate;
    guess -> paramU = min_u;
    guess -> paramV = mid_v;
    guess -> pt = evalPt [ 0 ];
    guess -> dist = evalPt [ 0 ].distSqrd ( evalPt [ 2 ] );
    guess -> origPtIndex = 2;
    guessList -> append ( guess, true );

    // Max U.

    guess = new snlSurfLocnGuess;
    *guess = guessTemplate;
    guess -> paramU = max_u;
    guess -> paramV = mid_v;
    guess -> pt = evalPt [ 1 ];
    guess -> dist = evalPt [ 1 ].distSqrd ( evalPt [ 2 ] );
    guess -> origPtIndex = 2;
    guessList -> append ( guess, true );

    // Edge: Max V. Eval index 3.

    // Min V.

    guess = new snlSurfLocnGuess;
    *guess = guessTemplate;
    guess -> paramU = mid_u;
    guess -> paramV = min_v;
    guess -> pt = evalPt [ 2 ];
    guess -> dist = evalPt [ 2 ].distSqrd ( evalPt [ 3 ] );
    guess -> origPtIndex = 3;
    guessList -> append ( guess, true );

    // Min U.

    guess = new snlSurfLocnGuess;
    *guess = guessTemplate;
    guess -> paramU = min_u;
    guess -> paramV = mid_v;
    guess -> pt = evalPt [ 0 ];
    guess -> dist = evalPt [ 0 ].distSqrd ( evalPt [ 3 ] );
    guess -> origPtIndex = 3;
    guessList -> append ( guess, true );

    // Max U.

    guess = new snlSurfLocnGuess;
    *guess = guessTemplate;
    guess -> paramU = max_u;
    guess -> paramV = mid_v;
    guess -> pt = evalPt [ 1 ];
    guess -> dist = evalPt [ 1 ].distSqrd ( evalPt [ 3 ] );
    guess -> origPtIndex = 3;
    guessList -> append ( guess, true );

    // Process guesses.

    int arraySize;

    snlSurfLocn* locns = processGuesses ( evalPt, 4, &arraySize, guessList, tolerance, tolerance,
                                          10, true, true );

    // Check for projected points remaining, within tolerance, on their original edge.

    bool edgeIsAmbig [ 4 ] = { false, false, false, false };

    for ( int index = 0; index < arraySize; index ++ )
    {
        if ( locns [ index ].dist > tolerance ) continue;

        switch ( locns [ index ].origPtIndex )
        {
            case 0:

                // Min U.
                
                if ( locns [ index ].paramU < min_u + tolerance )
                    edgeIsAmbig [ 0 ] = true;
                    
                break;
                
            case 1:
            
                // Max U.
                
                if ( locns [ index ].paramU > max_u - tolerance )
                    edgeIsAmbig [ 1 ] = true;
                    
                break;
                
            case 2:

                // Min V.

                if ( locns [ index ].paramV < min_v + tolerance )
                    edgeIsAmbig [ 2 ] = true;
                    
                break;
                
            case 3:

                // Max V.

                if ( locns [ index ].paramV > max_v - tolerance )
                    edgeIsAmbig [ 3 ] = true;
                    
                break;
        }
    }

    int cIndex = 0;
    int numAmbig = 0;

    // Process ambiguous points to find edges.

    if ( edgeIsAmbig [ 0 ] )
    {
        results [ cIndex ].direction = 0;
        results [ cIndex ].pVal = min_u;

        numAmbig++;
        cIndex++;
    }

    if ( edgeIsAmbig [ 1 ] )
    {
        results [ cIndex ].direction = 0;
        results [ cIndex ].pVal = max_u;

        numAmbig++;
        cIndex++;
    }

    if ( edgeIsAmbig [ 2 ] )
    {
        results [ cIndex ].direction = 1;
        results [ cIndex ].pVal = min_v;

        numAmbig++;
        cIndex++;
    }

    if ( edgeIsAmbig [ 3 ] )
    {
        results [ cIndex ].direction = 1;
        results [ cIndex ].pVal = max_v;

        numAmbig++;
    }

    delete[] locns;

    return numAmbig;
}

int snlSurface::hasAmbigEdges_depr ( sEdge* results )
{
    //! See if the surface has ambiguous edges
    //! --------------------------------------    
    //! @param results Array of sEdge. Should be size 4.
    //!
    //! @return Number of ambiguous edges.
    //!
    //! Notes: Function is now deprecated.

    snlPoint    evalPt [ 4 ];  // Current evaled non-homogeneous point.

    knot        minT, maxT, minU, maxU;

    minT = knotVectU -> min();
    maxT = knotVectU -> max();
    minU = knotVectV -> min();
    maxU = knotVectV -> max();

    // Min / Max T

    evalPt [ 0 ] = eval ( minT, ( ( maxU - minU ) / 2 ) + minU );    

    evalPt [ 1 ] = eval ( maxT, ( ( maxU - minU ) / 2 ) + minU );    

    // Min / Max U

    evalPt  [ 2 ] = eval (( ( maxT - minT ) / 2 ) + minT, minU );    

    evalPt [ 3 ] = eval (( ( maxT - minT ) / 2 ) + minT, maxU );    

    // Get projection function to do the work.

    ptrList < sLocn >* ambig;

    sLocn projns [ 4 ];

    ambig = projPtSurf ( *this, evalPt, 4, projns,
                         0.000001, 0.00001, 3 );

    int cIndex = 0;
    int numAmbig = 0;

    // Process ambiguous points to find edges.

    if ( projns [ 0 ].flag > 1 )
    {
        results [ cIndex ].direction = 0;
        results [ cIndex ].pVal = minT;

        numAmbig++;
        cIndex++;
    }

    if ( projns [ 1 ].flag > 1 )
    {
        results [ cIndex ].direction = 0;
        results [ cIndex ].pVal = maxT;

        numAmbig++;
        cIndex++;
    }

    if ( projns [ 2 ].flag > 1 )
    {
        results [ cIndex ].direction = 1;
        results [ cIndex ].pVal = minU;

        numAmbig++;
        cIndex++;
    }

    if ( projns [ 3 ].flag > 1 )
    {
        results [ cIndex ].direction = 1;
        results [ cIndex ].pVal = maxU;

        numAmbig++;
    }

    delete ambig;

    return numAmbig;
}

