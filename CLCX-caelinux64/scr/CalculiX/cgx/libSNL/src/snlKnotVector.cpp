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
//  GNU Library General Public License for more details.
//
//  You should have received a copy of the GNU Library General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#include "snlKnotVector.h"

snlKnotVector::~snlKnotVector ()
{
    if ( knots ) delete [ ] knots;
}

snlKnotVector::snlKnotVector ( const snlKnotVector& vector )
{
    // Copy constructor.
    // -----------------

    copyFrom ( vector );
}

snlKnotVector::snlKnotVector ( knot* knotArrayToUse, unsigned size, int degree, int snlKnotVectorType, bool copy )
{
    unsigned    index;

    if ( copy )
    {
        // Create new knot vector array from supplied data of "size".
        knots = new knot [ size ];

        // Copy supplied data into array.
        if ( knotArrayToUse)
            for ( index = 0; index < size; index ++ )
                *( knots + index ) = *( knotArrayToUse + index );
    }
    else
        knots = knotArrayToUse;

    vectorSize = size;
    
    deg = degree;

    kvType = snlKnotVectorType;
}

snlKnotVector::snlKnotVector ( knot startVal, knot endVal, unsigned numKnots, int degree )
{
    
    //  Generate a new Knot Vector
    //  --------------------------
    //
    //  startVal:   Starting parametric knot value.
    //  endVal:     End parametric knot value.
    //  numKnots:   Number of knots in vector.
    //  degree:     Degree to use during evaluation of basis functions.
    //
    //  Returns:    Constructor
    //
    //  Notes:      Assumes clamped (open) vector.     

    unsigned    index;
    
    kvType = open;
    
    deg = degree;

    // Calculate Spacing between knots.
    knot step = (endVal - startVal) / (knot) ( numKnots - (2 * degree) - 1 );

    // Create new array.
    knots = new knot [ numKnots ];

    // Fill knot vector with data.
    for ( index = 0; index < numKnots; index ++ )
    {
        if ( index < (unsigned) degree )
            knots [ index ] = startVal;

        else if ( index > ( numKnots - 1 - degree ) )
            knots [ index ] = endVal;

        else
            knots [ index ] = startVal + ( step * ((knot) ( index - degree )) );
    }

    vectorSize = numKnots;
}

snlKnotVector::snlKnotVector ( int size, int degree, knot* params )
{
    // Generate knot vector given existing parameters.
    // -----------------------------------------------
    // params:  Parameters to average between.
    // size:    Size of parameters array.
    // degree:  Degree of vector.
    //
    // Notes:   Used for interpolation.

    deg = degree;

    kvType = open;

    vectorSize = size + degree + 1;

    knots = new knot [ vectorSize ];
    
    int index;
    
    // Start clamp.
    for ( index = 0; index <= degree; index ++ )
        knots [ index ] = 0.0;
    
    // End clamp.
    for ( index = size; index < (int) vectorSize; index ++ )
        knots [ index ] = 1.0;
    
    // Internal knots.
    for ( index = 1; index < size - degree; index ++ )
    {
        knot sum = 0.0;
        
        for ( int paramIndex = index; paramIndex < index + degree; paramIndex ++ )
            sum += params [ paramIndex ];        
        
        knots [ index + degree ] = sum / (double) degree;
    }
}

snlKnotVector::snlKnotVector ( int degree )
{
    // Generate knot vector for Bezier patch.
    // --------------------------------------
    // degree:      Degree of knot vector.

    kvType = open;

    deg = degree;

    vectorSize = ( degree + 1 ) * 2;

    knots = new knot [ vectorSize ];

    int upperIndex = degree + 1;

    for ( int index = 0; index <= degree; index ++ )
    {
        knots [ index ] = 0.0;
        knots [ upperIndex ++ ] = 1.0;
    }
}

snlKnotVector& snlKnotVector::operator= ( const snlKnotVector& knotVectToCopy )
{
    // Assignment Operator.
    // --------------------

    if ( this != &knotVectToCopy )
    {
        if ( knots ) delete [ ] knots;

        copyFrom ( knotVectToCopy );
    }

    return *this;
}

void snlKnotVector::copyFrom ( const snlKnotVector& vector )
{
    vectorSize = vector.vectorSize;
    
    knots = new knot [ vectorSize ];
    
    deg = vector.deg;

    for ( unsigned index = 0; index < vectorSize; index ++ )
        knots [ index ] = vector.knots [ index ];   

    kvType = vector.type();
}

unsigned snlKnotVector::findSpan ( knot param ) const
{

    //  Find Knot Span corresponding to parameter
    //  -----------------------------------------
    //  param:      Parameter to find span of.

    unsigned    count;
    unsigned    span = 0;

    if ( param > knots [ vectorSize - 1 ] )
        param = knots [ vectorSize - 1 ];

    if ( param == knots [ vectorSize - 1 ] )
    {
        // Allow clamped end value to be a valid parameter.
        // Not strictly correct but works better in practice.

        // Step backwards through knot array until first non-zero length span found.
        for( count = ( vectorSize - 1 ); count > 0 ; count -- )
        {
            if ( param <= knots [ count ] && param > knots [ count - 1 ] )
                span = count - 1;
        }
    }
    else
    {
        for( count = 0; count < ( vectorSize - 1 ); count ++ )
        {
            if ( param >= knots [ count ] && param < knots [ count + 1 ] )
                span = count;
        }
    }

    return span;
}

knot snlKnotVector::val ( unsigned index ) const
{
    return knots [ index ];
}

const knot* snlKnotVector::getKnotPtr ( unsigned index )
{
    return knots + index;
}

unsigned snlKnotVector::size() const
{
    // Return size of knot vector.

    return vectorSize;
}

int snlKnotVector::degree()
{
    // Get degree of knot vector.
    // --------------------------

    return deg;
}

void snlKnotVector::degree ( int val )
{
    // Set degree of knot vector.
    // --------------------------
    
    deg = val;
}

bool snlKnotVector::equals ( const snlKnotVector& knotVect ) const
{
    if ( deg != knotVect.deg ) return false;
    
    if ( vectorSize != knotVect.vectorSize ) return false;
    
    for ( unsigned index = 0; index < vectorSize; index ++ )
        if ( knots [ index ] != knotVect.knots [ index ] ) return false;
    
    return true;
}

void snlKnotVector::insertKnot ( knot param, int numTimes )
{
    // Insert new knot into vector.
    // ----------------------------
    // param:       knot to insert.
    // numTimes:    Number of times to insert knot into vector.

    unsigned        index;

    if ( ! knots ) return;

    unsigned span = findSpan ( param );

    knot* newKnots = new knot [ vectorSize + numTimes ];

    // Copy up to insertion point.
    for ( index = 0; index <= span; index ++ )
        newKnots [ index ] = knots [ index ];

    // Add in new knot.
    for ( int count = 0; count < numTimes; count ++ )
        newKnots [ span + count + 1 ] = param;

    // Copy rest of old knots to new vector.
    for ( index = span + numTimes + 1; index < vectorSize + numTimes; index ++ )
        newKnots [ index ] = knots [ index - numTimes ];

    delete[] knots;

    knots = newKnots;

    vectorSize += numTimes;
}

void snlKnotVector::removeKnot ( unsigned spanIndex )
{
    // Remove knot at spanIndex
    // --------------------

    unsigned        index;

    knot * newKnots = new knot [ vectorSize - 1 ];

    // Copy up to removal point.
    for ( index = 0; index < spanIndex; index ++ )
        newKnots [ index ] = knots [ index ];

    // Copy remainder of knots. Skip knot to be removed.
    for ( index = spanIndex; index < vectorSize - 1; index ++ )
        newKnots [ index ] = knots [ index + 1 ];

    delete[] knots;

    knots = newKnots;

    vectorSize --;
}

void snlKnotVector::grow ( unsigned bySize )
{
    // Increase size of knot vector array.
    // -----------------------------------
    // bySize:    Size to increase knot vector by.
    
    knot* newKnots = new knot [ vectorSize + bySize ];
    
    for ( unsigned index = 0; index < vectorSize; index ++ )
        newKnots [ index ] = knots [ index ];
    
    delete[] knots;
    
    knots = newKnots;
    
    vectorSize += bySize;
}

void snlKnotVector::increaseMultiplicity ( unsigned spanIndex, int numKnotsToAdd )
{
    // Increase multiplicity of knots at span.
    // ---------------------------------------
    // spanIndex:        Index of knot span to process.
    // numKnotsToAdd:    Number of knots to add at spanIndex.
    
    unsigned        index;

    if ( ! knots ) return;
    
    knot param = knots [ spanIndex ];

    knot* newKnots = new knot [ vectorSize + numKnotsToAdd ];

    // Copy up to insertion point.
    for ( index = 0; index <= spanIndex; index ++ )
        newKnots [ index ] = knots [ index ];

    // Add in new knots.
    for ( index = spanIndex + 1; index <= spanIndex + numKnotsToAdd; index ++ )    
        newKnots [ index ] = param;

    // Copy rest of old knots to new vector.
    for ( index = spanIndex + numKnotsToAdd + 1; index < vectorSize + numKnotsToAdd; index ++ )
        newKnots [ index ] = knots [ index - numKnotsToAdd ];

    delete[] knots;

    knots = newKnots;

    vectorSize += numKnotsToAdd;
}

knot* snlKnotVector::getKnotArray()
{
    return knots;
}

int snlKnotVector::type() const
{
    return kvType;
}

const knot* snlKnotVector::array()
{
    // Return pointer to array of knots
    // --------------------------------

    return knots;
}

knot snlKnotVector::max() const
{
    // Return maximum knot value in vector
    // -----------------------------------

    return knots [ vectorSize - 1 ];
}

knot snlKnotVector::min() const
{
    // Return minimum knot value in vector
    // -----------------------------------

    return knots [ 0 ];
}

unsigned snlKnotVector::getNumSpans() const
{
    // Return number of non-zero length spans
    // --------------------------------------
    
    unsigned numSpans = 0;

    for ( unsigned index = 0; index < ( vectorSize - 1 ); index ++ )
    {
        if ( knots [ index + 1 ] > knots [ index ] ) numSpans++;
    }

    return numSpans;
}

unsigned snlKnotVector::getFirstSpan() const
{
    // Get first non zero length span
    // ------------------------------

    for ( unsigned index = 0; index < ( vectorSize - 1 ); index ++ )
    {
        if ( knots [ index + 1 ] > knots [ index ] ) return index;
    }

    return 0;
}

unsigned snlKnotVector::getNextSpan ( unsigned spanIndex ) const
{
    // Get next non-zero length span given current spanIndex.
    // ------------------------------------------------------
    // Returns:     Next non-zero length span index.

    for ( unsigned index = spanIndex + 1; index < ( vectorSize - 1 ); index ++ )
    {
        if ( knots [ index + 1 ] > knots [ index ] ) return index;
    }

    return 0;
}

unsigned snlKnotVector::getPreviousSpan ( unsigned spanIndex ) const
{
    // Get previous non-zero length span given current spanIndex.
    // ----------------------------------------------------------
    // Returns:     Previous non-zero length span index.
    
    for ( unsigned index = spanIndex - 1; index >= 0; index -- )
    {
        if ( knots [ index + 1 ] > knots [ index ] ) return index;
    }

    return 0;
}

int snlKnotVector::findMultiplicity ( unsigned index ) const
{
    // Find the knot multiplicity at index
    // -----------------------------------
    // index:       Index to search.
    //
    // Returns:     Number of knots found.

    // Find last index that has required knot value.

    unsigned cIndex = index;

    while ( knots [ cIndex ] == knots [ cIndex + 1 ] ) cIndex ++;

    // Count multiples backwards.

    int multi = 1;

    while ( knots [ cIndex ] == knots [ cIndex - 1 ] )
    {
        multi ++;

        if ( cIndex == 1 )
            break;
        else
            cIndex --;
    }

    return multi;
}

int snlKnotVector::findMultiplicity ( knot param ) const
{
    // Find the knot multiplicity at a particular knot value.
    // ------------------------------------------------------
    // param:       Parameter to evaluate.
    //
    // Returns:     Number of knots found.
    
    // Find span parameter belongs to.
    unsigned span = findSpan ( param );
    
    // Find value of knot associated with span.
    knot spanKnotVal = val ( span );
    
    // Return multiplicity.
    if ( param == spanKnotVal )    
        return findMultiplicity ( span );
    
    return 0;        
}

void snlKnotVector::truncate ( knot param, bool keepLast )
{
    // Truncate knot vector.
    // ---------------------
    // param:    Parameter to truncate at.
    // keepLast: Keep last section of knot vector, discard first part.
    //
    // Notes:    Truncates at last of knots valued at param.
    //           Assumes degree knots are present at param.
    
    unsigned start, end;    
    
    if ( keepLast )
    {
        start = findSpan ( param );
        start = getPreviousSpan ( start ) + 1;  // Go to start of knots if more than one at param.
        end = vectorSize - 1;        
    }
    else
    {
        start = 0;
        end = findSpan ( param );        
    }
    
    // Generate new knot vector and populate.
    
    unsigned newSize = end - start + 2;  // Add one point for correct clamping.
    
    knot* newKnots = new knot [ newSize ];
    
    unsigned copyFrom = start;
    
    if ( keepLast )
    {
        for ( unsigned index = 1; index < newSize; index ++, copyFrom ++ )
            newKnots [ index ] = knots [ copyFrom ];
            
        newKnots [ 0 ] = param;
    }
    else
    {
        for ( unsigned index = 0; index < newSize - 1; index ++, copyFrom ++ )
            newKnots [ index ] = knots [ copyFrom ];
        
        newKnots [ newSize - 1 ] = param;
    }    
        
    delete[] knots;
    
    knots = newKnots;
    
    vectorSize = newSize;    
}

void snlKnotVector::reparameterise ( knot startKnot, knot endKnot )
{
    // Reparameterise knot vector using a linear reparameterisation.
    // -------------------------------------------------------------
    // startKnot:    Knot vectors new starting value.
    // endKnot:      Knot vectors new ending value.
    
    double oldLength = knots [ vectorSize - 1 ] - knots [ 0 ];
    double newLength = endKnot - startKnot;
    
    double oldStartKnot = knots [ 0 ];
    
    for ( unsigned index = 0; index < vectorSize; index ++ )
        knots [ index ] = ( ( knots [ index ] - oldStartKnot ) / oldLength ) * newLength + startKnot;
}

void snlKnotVector::reverse()
{
    // Reverse knot vector.
    // --------------------

    unsigned midPoint = vectorSize / 2;

    unsigned swapIndex = vectorSize - 1;

    for ( unsigned index = 0; index < midPoint; index ++ )
    {
        knot trans = knots [ index ];  // Transfer value.
        knots [ index ] = knots [ swapIndex ];
        knots [ swapIndex -- ] = trans;
    }
}

void snlKnotVector::join ( snlKnotVector* knotVector )
{
    // Join another knot vector to the end of this one.
    // ------------------------------------------------
    // knotVector:    Knot vector to join. Must be of same degree as this knot vector.
    //
    // Notes:         Assumes end clamp of this and start clamp values of other knot vector
    //                are the same. This is _not_ just a join of arrays, it results in a properly
    //                formed knot vector.
    
    if ( knotVector -> deg != deg ) return;
    
    unsigned newSize = vectorSize + ( knotVector -> vectorSize ) - deg - 2;
    
    unsigned oldSize = vectorSize;
    
    grow ( newSize - vectorSize );
    
    // Copy new points into knot array.    
    
    unsigned copyFromIndex = deg + 1;  // The appended vector loses deg + 1 knots from the start of it's array.
    
    // This knot vector looses one knot at end of the array
    
    for ( unsigned index = oldSize - 1; index < newSize; index ++ )
        knots [ index ] = knotVector -> knots [ copyFromIndex ++ ];    
}

basis* snlKnotVector::evalBasis ( knot param )
{

    // Evaluate Basis Functions
    // ------------------------
    // param:       Paramater value to process at.
    //
    // Returns:     Array of basis function values, evaluated at param.
    //
    // Notes:       The calling function owns the returned array and is responsible for
    //              deleting it using delete[]. The size of the array is always deg + 1.
    
    basis* bVals = new basis [ deg + 1 ];

    if ( param == knots [ 0 ] && kvType == open )
    {
        // First basis function value is 1 the rest are zero.

        bVals [ 0 ] = 1.0;

        for ( int index = 1; index < deg + 1; index ++ )
            bVals [ index ] = 0.0;

        return bVals;
    }

    if ( param == knots [ vectorSize - 1 ] && kvType == open )
    {
        // Last basis function value is 1 the rest are zero.

        bVals [ deg ] = 1.0;

        for ( int index = 0; index < deg; index ++ )
            bVals [ index ] = 0.0;

        return bVals;
    }
    
    unsigned spanIndex = findSpan ( param );

    basis*      right = new basis [ deg + 1 ];
    basis*      left = new basis  [ deg + 1 ];
    basis       saved, temp;
    unsigned    index, level;

    bVals [ 0 ] = 1.0;

    for ( level = 1; level <= (unsigned) deg; level ++ )
    {
        left [ level ] = param - knots [ spanIndex + 1 - level ];
        right [ level ] = knots [ spanIndex + level ] - param;

        saved = 0.0;

        for ( index = 0; index < level; index ++ )
        {
            temp = bVals [ index ] / ( right [ index + 1 ] + left [ level - index ] );
            bVals [ index ] = saved + right [ index + 1 ] * temp;
            saved = left [ level - index ] * temp;
        }

        bVals [ level ] = saved;
    }

    delete[] right;
    delete[] left;

    return bVals;
}

basis* snlKnotVector::evalBasisDeriv ( knot param, int deriv )
{
    // Evaluate basis functions and their derivatives.
    // -----------------------------------------------
    //
    // param:       Parameter to process at.
    // deriv:       Which derivative to process to.
    //
    // Returns:     Two dimensional array of basis and basis derivative values.
    //
    // Notes:       The calling function is responsible for deleting the returned array
    //              using delete[]. The size of the array is [ deriv + 1 ] [ deg + 1 ].    

    basis*      right = new basis [ deg + 1 ];
    basis*      left = new basis [ deg + 1 ];
    basis       saved, temp;
    basis*      derivSaved = new basis [ deriv ];
    unsigned    index, level;
    int         count;
    
    basis* bVals = new basis [ ( deriv + 1 ) * ( deg + 1 ) ];
    
    unsigned spanIndex = findSpan ( param );

    unsigned         overFlow;  // Just in case deriv is bigger than degree.

    if ( deriv > deg )
    {
        overFlow = deriv - deg;
        deriv = deg;
    }
    else
        overFlow = 0;

    bVals [ 0 ] = 1.0;

    for ( level = 1; level <= (unsigned) deg; level ++ )
    {
        left [ level ] = param - knots [ spanIndex + 1 - level ];
        right [ level ] = knots [ spanIndex + level ] - param;

        saved = 0.0;

        for ( count = 0; count < deriv; count ++ )
            derivSaved [ count ] = 0.0;

        for ( index = 0; index < level; index ++ )
        {
            temp = bVals [ index ] / ( right [ index + 1 ] + left [ level - index ] );
            bVals [ index ]  = saved + right [ index + 1 ] * temp;
            saved = left [ level - index ] * temp;

            // Process first order derivatives as needed.
            if ( level > (unsigned) ( deg - deriv ) )
            {
                bVals [ index + ( ( deg - level + 1 ) * ( deg + 1 ) ) ] = level * ( derivSaved [ 0 ] - temp );
                derivSaved [ 0 ] = temp;
            }

            // Process other order derivatives.
            for ( count = deg - level + 2; count <= deriv; count ++ )
            {
                temp = bVals [ index + ( count * ( deg + 1 ) ) ] /
                               ( right [ index + 1 ] + left [ level - index ] );

                bVals [ index + ( count * ( deg + 1 ) ) ] = level * ( derivSaved [ count - 1 ] - temp );

                derivSaved [ count - 1 ] = temp;
            }
        }

        bVals [ level ] = saved;

        // Add last first order derivative at this level.
        if ( level > (unsigned) ( deg - deriv ) )
            bVals [ level + ( ( deg - level + 1 ) * ( deg + 1 ) ) ] = level * derivSaved [ 0 ];

        // Add last other order derivatives at this level.
        for ( count = deg - level + 2; count <= deriv; count ++ )
            bVals [ index + ( count * ( deg + 1 ) ) ] = level * derivSaved [ count - 1 ];
    }

    if ( overFlow )
    {
        for ( index = ( deriv + 1 ) * ( deg + 1 ); index < ( deriv + overFlow + 1 ) * ( deg + 1 ); index ++ )
            bVals [ index ] = 0.0;
    }

    delete[] left;
    delete[] right;
    delete[] derivSaved;
    
    return bVals;
}

double* snlKnotVector::calcRemovalAlphas ( unsigned span )
{
    // Calculate alphas used for knot removal.
    // ---------------------------------------
    // span:    Span of knot where removal is taking place.
    //
    // Returns:    Array of alphas. Must be deleted by caller.
    
    // Find multiplicity of knot at index.
    unsigned multi = findMultiplicity ( span );

    // Calculate the number of equations.
    unsigned numEqns = deg - multi + 1;
    
    knot rParam = val ( span );
    
    double* alpha = new double [ numEqns ];

    unsigned count = 0;

    for ( unsigned index = span - deg; index <= ( span - multi ) ; index ++ )
    {
        alpha [ count ++ ]  = ( rParam - ( knots [ index ] ) )
                                / ( knots [ index + deg + 1 ] - knots [ index ] );
    }
    
    return alpha;
}

void snlKnotVector::print()
{
    // Print knot vector to std out.
    // -----------------------------

    cout << "degree: " << deg << " Knots: ";

    for ( unsigned index = 0; index < vectorSize; index ++ )
        cout << knots [ index ] << "  ";

    cout << "\n";
}

void snlKnotVector::print_cpp()
{
    // Print knot vector to std out.
    // -----------------------------
    // Notes:   Prints in c++ code format.

    cout << "{ ";

    for ( unsigned index = 0; index < vectorSize; index ++ )
    {
        cout << knots [ index ];

        if ( index < vectorSize - 1 )
            cout << ", ";
    }

    cout << " };";
}

