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

// *** Single dimension array of knots. ***

#ifndef SNLKNOTVECTOR_H
#define SNLKNOTVECTOR_H

#ifdef SGI_MIPS

    #include <iostream.h>
    #include <math.h>
    
#else
    #include <iostream>
    #include <cmath>
    #ifndef WIN32
        using namespace std;
    #endif
#endif

#ifdef WIN32
    #undef max
    #undef min
    using std::cout;
#endif

typedef double knot;
typedef double basis;

class snlKnotVector
{
    public:

        virtual ~snlKnotVector ();

        snlKnotVector ( const snlKnotVector& );  // Copy constructor.
    
        // Use existing knot array.
        snlKnotVector ( knot* knotArrayToUse, unsigned size, int degree, int knotVectType = 1, bool copy = false );
        
        // Generate new knot array.        
        snlKnotVector ( knot startVal, knot endVal, unsigned numKnots, int degree );

        // Generate knot vector given existing parameters. Used for interpolation.
        snlKnotVector ( int size, int degree, knot* params );

        // Generate knot vector for Bezier patch.
        snlKnotVector ( int degree );

        snlKnotVector& operator= ( const snlKnotVector& KnotVectToCopy );

        enum knotVectorType
        {
            open = 1,       // Clamped
            periodic = 2    // Periodic will not be supported for some time yet :-)
        };

        knot val ( unsigned index ) const;
        
        const knot* getKnotPtr ( unsigned index );

        unsigned size() const;
        
        int degree();
        void degree ( int val );
        
        bool equals ( const snlKnotVector& knotVect ) const;

        void insertKnot ( knot param, int numTimes = 1 );
        
        void removeKnot ( unsigned spanIndex );
        
        void grow ( unsigned bySize );
        
        void increaseMultiplicity ( unsigned spanIndex, int numKnotsToAdd );

        unsigned findSpan ( knot param ) const;

        unsigned getNumSpans() const;  // Return number of non-zero length spans.

        unsigned getFirstSpan() const;  // Return knot index of first non-zero span.

        unsigned getNextSpan ( unsigned spanIndex ) const;
        
        unsigned getPreviousSpan ( unsigned spanIndex ) const;

        int findMultiplicity ( unsigned index ) const;
        int findMultiplicity ( knot param ) const;
        
        void truncate ( knot param, bool keepLast );
        
        void reparameterise ( knot startKnot, knot endKnot );

        void reverse();
        
        void join ( snlKnotVector* knotVector );

        const knot* array();  // Return pointer to array of knots.

        knot max() const;  // Max knot val.
        knot min() const;  // Min knot val.

        int type() const;  // Return value from knotVectorType.
        
        basis* evalBasis ( knot param );
        
        basis* evalBasisDeriv ( knot param, int deriv );
        
        double* calcRemovalAlphas ( unsigned span );

        void print();
        void print_cpp();
        
    protected:

        void copyFrom ( const snlKnotVector& vector );
    
        knot* getKnotArray();
    
        knot*       knots;
        unsigned    vectorSize;
        
        int         deg; // Degree associated with vector.

        // Type of knot vector
        int         kvType;
};

#endif
