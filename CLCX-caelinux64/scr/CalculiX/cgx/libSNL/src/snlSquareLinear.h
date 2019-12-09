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

// *** Class for the solution of square linear algebraic equations ***

#ifndef SNL_SQUARELINEAR_H
#define SNL_SQUARELINEAR_H

#include "snlPoint.h"

#ifdef SGI_MIPS

    #include <iostream.h>
    #include <math.h>
    
#else

    #include <iostream>
    #include <cmath>
    
    using namespace std;
    
#endif

class snlSquareLinear
{
    public:
        
        snlSquareLinear();
        ~snlSquareLinear();
        snlSquareLinear ( int numUnknowns, int numRightHandSides, double* coefficients, double* rightHandSides );
        
        void solve();
        
        void print();
        void printCoeffs();
        void printRhSides();
    
    private:
    
        int        num_unknowns; // Number of unknowns corresponding to coefficient matrix.
        int        num_rhSides;  // Number of right hand sides.
        
        double*    coeffs;  // Coefficient matrix array.
        double*    rhSides;  // Array of right hands sides.            
};

#endif
