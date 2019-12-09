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

// *** NURBS routines that don't belong in classes ***

// *** Legacy OpenCDSM routines are included ***

// *** NOTE: OpenCDSM functions still use t,u for surface parameters ***

#include "snlNurbsCommon.h"
#include "snlVector.h"

//#define PROJ_COMMENT

static int maxIterations = 0;
static unsigned maxPasses = 0;

bool newtonIterStepSurf ( snlPoint* derivPts, snlPoint* projPt, knot* deltaU, knot* deltaV )
{
    // Evaluate a single step in a newton iteration over a surface
    // -----------------------------------------------------------
    // derivPts:        Pre calculated rational 0th to 2nd derivatives at desired paramters.
    //                  This functions expectes a 3 X 3 array.
    // projPt:          Point being projected onto surface.
    // deltaU:          Change in u parameter to return.
    // deltaV:          Change in v parameter to return.
    //
    // returns:         True if processing was succesfull. False if deltas would be infinite.
    //
    // Notes:   Theory from "The NURBS Book 2nd Ed" pages 232 - 234.
    
    basis       jcbn [ 2 ] [ 2 ];  // 2 X 2 Jacobian matrix.
    
    snlVector dist ( *projPt, derivPts [ 0 ] );  // Distance from point to be projected to surface.

    jcbn [ 0 ] [ 0 ] = derivPts [ 3 ].lengthSqrd() + dist.dot ( derivPts [ 6 ] );
    jcbn [ 0 ] [ 1 ] = snlVector ( derivPts [ 3 ] ).dot ( derivPts [ 1 ] )
                       + dist.dot ( derivPts [ 4 ] );
    jcbn [ 1 ] [ 0 ] = jcbn [ 0 ] [ 1 ];
    jcbn [ 1 ] [ 1 ] = derivPts [ 1 ].lengthSqrd( ) + dist.dot ( derivPts [ 2 ] );

    return solve2X2LinEqn ( jcbn [ 0 ] [ 0 ], jcbn [ 1 ] [ 0 ], jcbn [ 0 ] [ 1 ], jcbn [ 1 ] [ 1 ],
                            - ( dist.dot ( derivPts [ 3 ] ) ),
                            - ( dist.dot ( derivPts [ 1 ] ) ),
                            deltaU, deltaV );
}

bool newtonIterStepCurve ( snlPoint* derivPts, snlPoint* projPt, knot* paramDelta )
{
    // Evaluate a single step in a newton iteration over a curve
    // ---------------------------------------------------------
    // derivPts:        Pre calculated rational 0th to 2nd derivatives at desired paramter. Size 3.
    // projPt:          Point being projected onto curve.
    // paramDelta:      Change in t parameter to return.
    //
    // returns:         True if processing was succesfull. False if paramDelta would be infinite.
    //
    // Notes:   Theory from "The NURBS Book 2nd Ed" pages 230 - 231.
    
    snlVector dist ( *projPt, derivPts [ 0 ] );  // Distance from point to be projected to surface.

    if ( ( dist.dot ( derivPts [ 2 ] ) + derivPts [ 1 ].lengthSqrd() ) == 0.0 ) return false;

    *paramDelta = - ( dist.dot ( derivPts [ 1 ] ) /
                  ( dist.dot ( derivPts [ 2 ] ) + derivPts [ 1 ].lengthSqrd() ) );

    return true;
}

bool lineIterStepCurve ( snlPoint* derivPts, snlPoint* projPt, knot* paramDelta )
{
    // Evaluate a single step in a line iteration over a degree 1 curve
    // ----------------------------------------------------------------
    // derivPts:        Pre calculated rational 0th to 2nd derivatives at desired paramter. Size 3.
    // projPt:          Point being projected onto curve.
    // paramDelta:      Change in t parameter to return.
    //
    // returns:         True if processing was succesfull. False if paramDelta would be infinite.
        
    basis       length;

    // Check for divide by zero error.
    length = derivPts [ 1 ].lengthSqrd();

    if ( length == 0.0 ) return false;
    
    snlVector dist ( *projPt, derivPts [ 0 ] );  // Distance from point to be projected to surface.

    // Vectors are nose to tail so have to negate. Look at definition of dot product.
    *paramDelta = -( dist.dot ( derivPts [ 1 ] ) ) / length;

    return true;
}

int  solve2X2LinEqn ( double a1, double a2, double a3, double a4,
                      double b1, double b2, double* x1, double* x2 )
{
    /* Solve a 2 X 2 system of linear equations
    // ----------------------------------------
    //
    // a1 to a4:        Coefficients of the system.
    // b1, b2:          If the system is represented as Ax = b, these are the b's.
    //
    //                   _            _     _  _
    //                  |              |   |    |
    //                  | a1.x1  a3.x2 |   | b1 |
    //                  |              | = |    |
    //                  | a2.x1  a4.x2 |   | b2 |
    //                  |_            _|   |_  _|
    //
    //  x1, x2:         Pointers to return solutions in.
    //
    //  Returns:        False if det is 0, otherwise True. ie Returned values would be infinite.
    */

    double      det, det1, det2;

    det = a1 * a4 - a2 * a3;

    det1 = b1 * a4 - b2 * a3;

    det2 = a1 * b2 - a2 * b1;
    
    if ( det != 0 )
    {
        *x1 = det1 / det;
        *x2 = det2 / det;
        
        return 1;
    }
    
    return 0;
}

ptrList < sLocn >* projPtSurf ( snlSurface& surface, snlPoint* toProj, int toProjNum, sLocn* best,
                                double iterTol, double cosTol, unsigned maxPass )
{
    // Project points onto given surface
    // ---------------------------------
    // surface:     Surface to project to.
    // toProj:      Point to project.
    // toProjNum:   Number of points to project
    // best:        Array to return best evaluted locations in. Must be toProjNum in size.
    // iterTol:     Iteration tolerance. Maximum distance between surface points after
    //              successive iterations. If below this value then processing stops.
    // cosTol:      Cosine Tolerance. Maxium allowable deviation of cosine from zero.
    //              i.e. How close to a surface normal it is.
    // maxPass:     Maximum number of passes to be made. Each subsequent pass doubles the number of
    //              sections each span is divided into. For ordinary cases this only needs to be 1.
    //
    //
    // Notes:       Evaluates more than one point at once.
    //              Mostly OpenCDSM legacy code and uses OpenCDSM parameter names.
    //
    //              This is messy inefficient code! But it mostly works ;-)

    knot        paramT, paramU;  // Evaluation parameters.
    knot        pTStart, pTEnd;  // Param T start and end for span.
    knot        pUStart, pUEnd;  // Param U start and end for span.
    knot        pTStep, pUStep;  // Param T and U incremental steps.
    knot        pTStepDenom, pUStepDenom;  // PxStep denominators.
    
    snlPoint    evalPt;  // Current evaled non-homogeneous point.

    snlPoint*   evalDerivs = 0;  // Derivatives of point needed for newton iterations.
    snlPoint*   newEvalDerivs = 0;

    bool        withinTol = false;  // Found a point within tolerance.

    bool        noBest = true;  // No best results have been initialised yet.
    int         index;

    unsigned    cPass;

    bool*       withinTols = new bool [ toProjNum ];

    ptrList < projLocn >* bestLists = new ptrList < projLocn > [ toProjNum ];
    ptrList < projLocn >* cList;

    projLocn*       cBest;

    basis           bestDist;  // Used for point selection with multiple hits.

    ptrList < sLocn >* ambig = new ptrList < sLocn >;

    unsigned degT = surface.degreeU();
    unsigned degU = surface.degreeV();

    bool        degT1 = ( degT == 1 );  // Degree T is 1.
    bool        degU1 = ( degU == 1 );  // Degree U is 1.

    for ( index = 0; index < toProjNum; index ++ ) withinTols [ index ] = false;

    #ifdef PROJ_COMMENT

        cout << "\nNew Projection\n";
        cout.precision ( 15 );

    #endif
    
    const snlKnotVector& kntsT = surface.knotVectorU();
    const snlKnotVector& kntsU = surface.knotVectorV();
    
    // Calculate bisection step multipliers used to account for surfaces with both
    // high curvature and high discontinuity ( small number of non-zero spans, large number of knots ).
    
    int stepMultiT = (int) ( surface.maxCurvatureU() * 2.0 ) + ( surface.sizeU() / kntsT.getNumSpans() );
    int stepMultiU = (int) ( surface.maxCurvatureV() * 2.0 ) + ( surface.sizeV() / kntsU.getNumSpans() );    
    
    #ifdef PROJ_COMMENT
        cout << "stepMultiT: " << stepMultiT << "  stepMultiU: " << stepMultiU << "\n";
    #endif
    
    // Evaluate surface points that correspond to beginning and end of knot spans
    // and store point with least distance from surface points.

    for ( cPass = 1; cPass <= maxPass; cPass ++ )
    {

        #ifdef PROJ_COMMENT

            cout << "Pass: " << cPass << "\n";

        #endif        

        unsigned minSpanT = kntsT.findSpan ( kntsT.min() );
        unsigned maxSpanT = kntsT.findSpan ( kntsT.max() );

        unsigned minSpanU = kntsU.findSpan ( kntsU.min() );
        unsigned maxSpanU = kntsU.findSpan ( kntsU.max() );

        // Seed best point.
        paramT = kntsT.val ( minSpanT );
        paramU = kntsU.val ( minSpanU );

        evalPt = surface.eval ( paramT, paramU );        

        // On the first loop the value found is the best.
        if ( noBest )
        {
            // Initialise pointer lists
            for ( index = 0; index < toProjNum; index ++ )
            {
                cBest = new projLocn;

                cBest -> paramT = paramT;
                cBest -> paramU = paramU;
                cBest -> pt = evalPt;

                bestLists [ index ].append ( cBest, true );
            }

            noBest = false;
        }

        for ( unsigned spanT = minSpanT; spanT <= maxSpanT; spanT ++ )
        {
            pTStart = kntsT.val ( spanT );
            pTEnd = kntsT.val ( spanT + 1 );

            // Only work with non-zero length knot spans.
            if ( pTStart < pTEnd )
            {
                // Set T bisection step value.
                pTStepDenom = ( ( degT + 1 ) * cPass * stepMultiT );
                pTStep = ( pTEnd - pTStart ) / pTStepDenom;

                for ( unsigned spanU = minSpanU; spanU <= maxSpanU; spanU ++ )
                {
                    pUStart = kntsU.val ( spanU );
                    pUEnd = kntsU.val ( spanU + 1 );

                    // Only work with non-zero length knot spans.
                    if ( pUStart < pUEnd )
                    {
                        // Set U bisection step value.
                        pUStepDenom = ( ( degU + 1 ) * cPass * stepMultiU );
                        pUStep = ( pUEnd - pUStart ) / pUStepDenom;

                        // Bisect span "rectangle", spanT -> spanT + 1, spanU -> spanU + 1.
                        // Step through each section. Some points are checked twice for the sake
                        // of code simplicity.

                        for ( unsigned sectT = 0; sectT <= (unsigned) pTStepDenom; sectT ++ )
                        {
                            paramT = pTStart + pTStep * sectT;

                            for ( unsigned sectU = 0; sectU <= (unsigned) pUStepDenom; sectU ++ )
                            {
                                paramU = pUStart + pUStep * sectU;

                                // Evaluate
                                evalPt = surface.eval ( paramT, paramU );
            
                                // Iterate through each point to be projected.
                                for ( index = 0; index < toProjNum; index ++ )
                                {
                                    if ( withinTols [ index ] ) continue;

                                    cList = bestLists + index;

                                    cBest = cList -> first();

                                    // See if evalStart [ 0 ] is the best fit so far.
                                    if ( snlVector ( toProj [ index ], cBest -> pt ).length()
                                         > snlVector ( toProj [ index ], evalPt ).length() )
                                    {                
                                        // If the evaluated point is closer to the point to be projected
                                        // then it is the new best.
                                        cBest -> paramT = paramT;
                                        cBest -> paramU = paramU;
                                        cBest -> pt = evalPt;
                                        
                                        cList -> truncate();
                                    }
                                    else if ( ( evalPt == ( cBest -> pt ) ) &&
                                              ( cBest -> paramT != paramT ||
                                                cBest -> paramU != paramU ) )
                                    {                
                                        // Coincident point found.
                                        
                                        // Make sure point hasn't been found before.
                                        
                                        bool exists = false;
                                        
                                        while ( cBest )
                                        {                                            
                                            if ( paramT == cBest -> paramT && paramU == cBest -> paramU )
                                                exists = true;
                                                
                                            cBest = cList -> next();
                                        }
                                        
                                        if ( ! exists )
                                        {
                                            cBest = new projLocn;
                                            cBest -> paramT = paramT;
                                            cBest -> paramU = paramU;
                                            cBest -> pt = evalPt;
    
                                            cList -> append ( cBest, true );
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }        

        // Whoa! that is far too many nested for loops *grin*.

        // Okey dokey. Now it is time to try a newton iteration or two ( thousand ) on the point of best fit.

        for ( index = 0; index < toProjNum; index ++ )
        {
            if ( withinTols [ index] ) continue;

            cBest = bestLists [ index ].first();

            double guessDistance = snlVector ( cBest -> pt, toProj [ index ] ).length();

            #ifdef PROJ_COMMENT
                cout << "Processing Index: " << index << "\n";
                cout << "Number in bestList: " << bestLists [ index ].count() << "\n";
                cout << "Original Point: "; toProj [ index ].print(); cout << "\n";
            #endif

            while ( cBest )
            {
                #ifdef PROJ_COMMENT                    
                    cout << "Guess: ";cBest -> pt.print(); cout << "\n";
                #endif

                for ( int count = 0; count < MAX_PROJ_ITER; count ++ )
                {
                    withinTol = false;

                    if ( maxIterations < count ) maxIterations = count;

                    #ifdef PROJ_COMMENT

                        cout << "Iteration Number: " << count << "\n";                        

                    #endif

                    // If proj point is within iteration tolerance from evaluated point then stop.

                    cBest -> dist = snlVector ( cBest -> pt, toProj [ index ] ).length();

                    if ( ( cBest -> dist ) < iterTol )
                    {
                        withinTol = true;
                        break;
                    }

                    // Get 1st and 2nd derivatives of point.
                    if ( ! evalDerivs )
                        evalDerivs = surface.evalDerivs ( cBest -> paramT, cBest -> paramU, 2, 2 );

                    // Calc projPt to surface vector.
                    snlVector projToSurf ( toProj [ index ], evalDerivs [ 0 ] );

                    // Check to see if cosine of determination function is under tolerance ( cosTol ).

                    snlVector tVect ( evalDerivs [ 3 ] );
                    basis cosT = projToSurf.calcAbsCos ( tVect );
                    
                    snlVector uVect ( evalDerivs [ 1 ] );
                    basis cosU = projToSurf.calcAbsCos ( uVect );

                    #ifdef PROJ_COMMENT
                        
                        cout << "Iteration: " << count << "  DotT: " << projToSurf.dot ( tVect ) << "  DotU: " << projToSurf.dot ( uVect ) << "\n";
                        cout << "Cosine T: " << cosT << "\n";
                        cout << "Cosine U: " << cosU << "\n";

                    #endif

                    if ( cosU <= cosTol && cosT <= cosTol )
                    {
                        withinTol = true;
                        break;
                    }

                    knot    deltaT;
                    knot    deltaU;

                    knot    newT;
                    knot    newU;

                    // Get new paramaters.
                    if ( !degT1 && !degU1 )
                    {
                        if ( ! newtonIterStepSurf ( evalDerivs, toProj + index, &deltaT, &deltaU ) )
                        {
                            break;  // Param deltas would have gone to infinity.
                        }
                    }
                    else
                    {
                        // At least one of the degrees is 1.

                        snlPoint tDerivs [ 3 ];

                        tDerivs [ 0 ] = evalDerivs [ 0 ];
                        tDerivs [ 1 ] = evalDerivs [ 3 ];
                        tDerivs [ 2 ] = evalDerivs [ 6 ];

                        if ( degT1 )
                        {
                            if ( ! lineIterStepCurve ( tDerivs, toProj + index, &deltaT ) )
                                break;

                        }
                        else
                        {
                            if ( ! newtonIterStepCurve ( tDerivs, toProj + index, &deltaT ) )
                                break;
                        }

                        if ( degU1 )
                        {
                            if ( ! lineIterStepCurve ( evalDerivs, toProj + index, &deltaU ) )
                                break;
                        }
                        else
                        {
                            if ( ! newtonIterStepCurve ( evalDerivs, toProj + index, &deltaU ) )
                                break;
                        }
                    }

                    // Clamp params to knot bounds.
                    newT = cBest -> paramT + deltaT;
                    newU = cBest -> paramU + deltaU;

                    if ( newT > ( kntsT.max() ) )
                    {
                        newT = kntsT.max();
                        deltaT = newT - cBest -> paramT;
                    }

                    if ( newT < ( kntsT.min() ) )
                    {
                        newT = kntsT.min();
                        deltaT = newT - cBest -> paramT;
                    }

                    if ( newU > ( kntsU.max() ) )
                    {
                        newU = kntsU.max();
                        deltaU = newU - cBest -> paramU;
                    }

                    if ( newU < ( kntsU.min() ) )
                    {
                        newU = kntsU.min();
                        deltaU = newU - cBest -> paramU;
                    }

                    #ifdef PROJ_COMMENT
                        cout << "OldT: " << cBest -> paramT << "\n";
                        cout << "OldU: " << cBest -> paramU << "\n";
                        cout << "NewT: " << newT << "\n";
                        cout << "NewU: " << newU << "\n";
                        cout << "deltaT: " << deltaT << "\n";
                        cout << "deltaU: " << deltaT << "\n";
                    #endif

                    // If parameters haven't changed then break.
                    if ( deltaT == 0.0 && deltaU == 0.0 )
                        break;

                    // Evaluate new parameters.

                    newEvalDerivs = surface.evalDerivs ( newT, newU, 2, 2 );

                    // Generate new best.
                    cBest -> pt = newEvalDerivs [ 0 ];
                    cBest -> paramT = newT;
                    cBest -> paramU = newU;

                    // Calculate new distance to surface.
                    cBest -> dist = snlVector ( cBest -> pt, toProj [ index ] ).length();

                    // Check for below tolerance changes in surface point.
                    snlVector chgT ( evalDerivs [ 3 ] );
                    chgT *= deltaT;
                    
                    snlVector chgU ( evalDerivs [ 1 ] );
                    chgU *= deltaU;

                    // Change vector.
                    snlVector chgV = chgT + chgU;

                    #ifdef PROJ_COMMENT
                        cout << "Change in T direction: " << chgT.length() << "\n";
                        cout << "Change in U direction: " << chgT.length() << "\n";
                        cout << "Change in point: " << chgV.length() << "\n";
                    #endif

                    if ( chgV.length() <= iterTol )
                    {
                        withinTol = true;
                        break;
                    }

                    // Reorganise evaluation arrays.
                    if ( evalDerivs ) delete[] evalDerivs;
                    evalDerivs = newEvalDerivs;
                    newEvalDerivs = 0;
                }

                // Detect whether the point to be projected is within tolerances
                // Projected points that are further from surface than original
                // guess are not within tolerance.
                
                if ( withinTol && ( cBest -> dist <= guessDistance ) )
                {                
                    withinTols [ index ] = withinTol;
                    cBest -> flag = true;                    
                }
                else
                    cBest -> flag = false;

                cBest = bestLists [ index ].next();
            }

            if ( withinTols [ index ] )
            {
                // ** Process best points **

                // Get best distance.

                cBest = bestLists [ index ].first();

                bestDist = cBest -> dist;

                cBest = bestLists [ index ].next();

                while ( cBest )
                {
                    if ( bestDist > ( cBest -> dist ) ) bestDist = ( cBest -> dist );

                    cBest = bestLists [ index ].next();
                }
                
                if ( bestDist < iterTol ) bestDist = iterTol;  // Accounts for discrepencies in Newton convergence.

                cBest = bestLists [ index ].first();

                int numBest = 0;

                while ( cBest )
                {
                    // Get the point closest to the surface.

                    if ( ( cBest -> flag ) && ( ( cBest -> dist ) <= bestDist ) )
                    {
                        if ( ! numBest )
                        {
                            best [ index ].paramT = cBest -> paramT;
                            best [ index ].paramU = cBest -> paramU;
                            best [ index ].pt = cBest -> pt;
                        }
                        else
                        {
                            // Add item to ambiguity list.
                            sLocn* ambLocn = new sLocn;
                            ambLocn -> paramT = cBest -> paramT;
                            ambLocn -> paramU = cBest -> paramU;
                            ambLocn -> pt = cBest -> pt;
                            ambLocn -> flag = index;

                            ambig -> append ( ambLocn, true );
                        }

                        numBest ++;
                    }
                    else
                        cBest -> flag = false;

                    cBest = bestLists [ index ].next();
                }

                best [ index ].flag = numBest;
            }
            else
            {
                // Make sure at least something is in the best array.
                cBest = bestLists [ index ].first();

                if ( cBest )
                {
                    best [ index ].paramT = cBest -> paramT;
                    best [ index ].paramU = cBest -> paramU;
                    best [ index ].pt = cBest -> pt;
                    best [ index ].flag = 1;
                }
            }

            // Clean up.
            if ( evalDerivs )
            {
                delete[] evalDerivs;
                evalDerivs = 0;
            }

            if ( newEvalDerivs )
            {
                delete[] newEvalDerivs;
                newEvalDerivs = 0;
            }
            
            #ifdef PROJ_COMMENT
                cout << "Best Found: "; best [ index ].pt.print(); cout << "\n";
            #endif
        }

        // All point tolerances must be within threshold to break out of pass loop.

        withinTol = true;

        for ( index = 0; index < toProjNum; index ++ )
        {
            if ( ! withinTols [ index ] ) withinTol = false;
        }        

        if ( withinTol ) break;
    }

    delete[] withinTols;

    if ( maxPasses < cPass ) maxPasses = cPass;

    #ifdef PROJ_COMMENT

        cout << "Max Passes: " << maxPasses << "\n";
        cout << "Max Iterations: " << maxIterations << "\n";

    #endif

    if ( ambig -> count() )
        resolveAmbig ( best, toProjNum, ambig );

    return ambig;
}

void resolveAmbig ( sLocn* projns, int projSize, ptrList < sLocn >* ambig )
{
    // Attempt to resolve projection ambiguities.
    // ------------------------------------------
    // projns:      Projections.
    // projSize:    Size of projns array.
    // ambig:       Ambiguities.

    // Generate array so that ambiguous points before and after
    // current point being considered can be easily checked.

    sLocn*      locn;

    if ( projSize < 3 ) return;  // Can't get a trend with less than three points.

    bool* ambigIndexes = new bool [ projSize ];

    for ( int index = 0; index < projSize; index ++ )
        ambigIndexes [ index ] = false;

    for ( locn = ambig -> first(); locn; locn = ambig -> next() )
        ambigIndexes [ locn -> flag ] = true;

    bool reverse = false;

    // Process projection ambiguities.

    // If parametric distance of ambiguous point is closer to
    // neighbours then this point is considered a better match.

    // Process forwards.

    for ( locn = ambig -> first(); locn; locn = ambig -> next() )
    {
        // Don't compare to ambiguous neighbours.

        if ( ! ( locn -> flag ) )
        {
            reverse = true;
            continue;
        }

        if ( ambigIndexes [ ( locn -> flag ) - 1 ] )
        {
            reverse = true;
            continue;
        }

        ambigIndexes [ ( locn -> flag ) ] = false;  // Don't process in reverse.

        knot cDist = paramDistSqrd ( projns [ ( locn -> flag ) - 1  ].paramT,
                                     projns [ ( locn -> flag ) - 1  ].paramU,
                                     projns [ ( locn -> flag ) ].paramT,
                                     projns [ ( locn -> flag ) ].paramU );

        knot tDist = paramDistSqrd ( projns [ ( locn -> flag ) - 1  ].paramT,
                                     projns [ ( locn -> flag ) - 1  ].paramU,
                                     locn -> paramT,
                                     locn -> paramU );

        // If the sLocn being tested is a better match then swap data.

        if ( tDist < cDist )
        {
            sLocn tmpLocn = *locn;
            *locn = projns [ tmpLocn.flag ];
            projns [ tmpLocn.flag ] = tmpLocn;

            // Make sure flags aren't swapped.
            projns [ tmpLocn.flag ].flag = locn -> flag;
            locn -> flag = tmpLocn.flag;
        }
    }

    // Process Backwards. Try and pick up points missed during forward processing.

    if ( reverse )
    {
        for ( locn = ambig -> last(); locn; locn = ambig -> prev() )
        {
            if ( ! ambigIndexes [ ( locn -> flag ) ] )
                continue;  // Has already been processed.

            // Don't compare to ambiguous neighbours.

            if ( ( locn -> flag ) >= projSize - 1 )  // Is last point.
                continue;

            if ( ambigIndexes [ ( locn -> flag ) + 1 ] )
                continue;

            knot cDist = paramDistSqrd ( projns [ ( locn -> flag ) + 1  ].paramT,
                                         projns [ ( locn -> flag ) + 1  ].paramU,
                                         projns [ ( locn -> flag ) ].paramT,
                                         projns [ ( locn -> flag ) ].paramU );

            knot tDist = paramDistSqrd ( projns [ ( locn -> flag ) + 1  ].paramT,
                                         projns [ ( locn -> flag ) + 1  ].paramU,
                                         locn -> paramT,
                                         locn -> paramU );

            // If the sLocn being tested is a better match then swap data.

            if ( tDist < cDist )
            {
                sLocn tmpLocn = *locn;
                *locn = projns [ tmpLocn.flag ];
                projns [ tmpLocn.flag ] = tmpLocn;

                // Make sure flags aren't swapped.
                projns [ tmpLocn.flag ].flag = locn -> flag;
                locn -> flag = tmpLocn.flag;
            }
        }

    }

    delete[] ambigIndexes;
}

knot paramDistSqrd ( knot t1, knot u1, knot t2, knot u2 )
{
    return ( t2 - t1 ) * ( t2 - t1 ) + ( u2 - u1 ) * ( u2 - u1 );
}

