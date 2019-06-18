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

#include "snlSquareLinear.h"

snlSquareLinear::snlSquareLinear()
{
    num_unknowns = 0;
    num_rhSides = 0;
    coeffs = 0;
    rhSides = 0;
}

snlSquareLinear::~snlSquareLinear()
{    
    if ( coeffs ) delete[] coeffs;
    if ( rhSides ) delete[] rhSides;
}

snlSquareLinear::snlSquareLinear ( int numUnknowns, int numRightHandSides, double* coefficients, double* rightHandSides )
{
    // Setup and solve square system of linear equations.
    // --------------------------------------------------
    // numUnknowns:        Number of unknowns in each equation.
    // numRightHandSides:  Number of right hand sides.
    // coefficients:       Array of size [numUnkowns][numUnknowns] holding unknown coefficients.
    // rightHandSides:     Array of size [numRightHandSides][numUnknowns] holding each right hand sides in a column.
    //
    // Notes:              All arrays use c style ordering. ( This is different from snlMatrix_4X4 ).
    //                     This object owns the arrays passed to it.
    
    num_unknowns = numUnknowns;
    num_rhSides = numRightHandSides;
    
    coeffs = coefficients;
    rhSides = rightHandSides;
    
    solve();
}

void snlSquareLinear::solve()
{
    // Solve linear equations.
    // -----------------------
    //
    // Notes:    This method does not use row or column interchanges.    
    
    if ( ! coeffs || ! rhSides ) return;
    
    // Use coefficient array to calculate and then store LU decomposition.
    // L is stored in the lower diagonal part of the coef array without utilising the diagonal.
    // U is stored in the upper diagonal part of the coef array.
    
    // Perform Gauss elimination and store negatives of the multipliers m(j,i) in L matrix.    
    
    for ( int pivot = 0; pivot < num_unknowns - 1; pivot ++ )
    {        
        int pivotRowIndex = pivot * num_unknowns;
        int pivotIndex = pivotRowIndex + pivot;
        
        double pivotVal = coeffs [ pivotIndex ];        
        
        for ( int row = pivot + 1; row < num_unknowns; row ++ )
        {
            int index = row * num_unknowns + pivot;  // Current index in coef array being processed.
            
            double multiplier = coeffs [ index ] / pivotVal;
            
            coeffs [ index ++ ] = multiplier;  // Pivot is subtracted instead of added during elimination.
            
            for ( int col = pivot + 1; col < num_unknowns; col ++ )            
                coeffs [ index ++ ] -= multiplier * coeffs [ pivotRowIndex + col ];            
        }
    }    
    
    // Calculate intermediate right hand values using forward substitution and L.
    
    for ( int coefRow = 1; coefRow < num_unknowns; coefRow ++ )
    {
        int multIndex = coefRow * num_unknowns;
        
        int rhsIndex = coefRow * num_rhSides;
        
        int rhsMultIndex = 0;
        
        for ( int coefCol = 0; coefCol < coefRow; coefCol ++ )
        {        
            double multiplier = coeffs [ multIndex ++ ];            
            
            for ( int rhsCol = 0; rhsCol < num_rhSides; rhsCol ++ )            
                rhSides [ rhsIndex + rhsCol ] -= multiplier * rhSides [ rhsMultIndex ++ ];
        }     
    }    
    
    // Calculate final right hand values using back substitution and U.
    
    for ( int coefRow = num_unknowns - 1; coefRow >= 0; coefRow -- )
    {
        int multIndex = ( coefRow + 1 ) * num_unknowns - 1;
        
        int rhsIndex = ( coefRow + 1 ) * num_rhSides - 1;
        
        int rhsMultIndex = num_unknowns * num_rhSides - 1;
        
        for ( int coefCol = num_unknowns - 1; coefCol > coefRow; coefCol -- )
        {        
            double multiplier = coeffs [ multIndex -- ];
            
            for ( int rhsCol = 0; rhsCol < num_rhSides; rhsCol ++ )
                rhSides [ rhsIndex - rhsCol ] -= multiplier * rhSides [ rhsMultIndex -- ];
        }
        
        // Divide right hand sides through by coef diagonal values.
        
        double divisor = coeffs [ multIndex ];
        
        for ( int rhsCol = 0; rhsCol < num_rhSides; rhsCol ++ )
            rhSides [ rhsIndex - rhsCol ] /= divisor;
    }    
}


void snlSquareLinear::print()
{
    // Print current coefficients and right hand sides to standard out.
    // ----------------------------------------------------------------
    
    cout << "\n\nCoefficients:\n\n";
    
    printCoeffs();    
    
    cout << "\n\nRight Hand Sides:\n\n";
    
    printRhSides();
}

void snlSquareLinear::printCoeffs()
{
    // Print coefficents to standard out.
    // ----------------------------------
    
    int index = 0;    
    
    for ( int row = 0; row < num_unknowns; row ++ )
    {
        for ( int col = 0; col < num_unknowns; col ++ )
            cout << coeffs [ index ++ ] << "   ";
        
        cout << "\n";
    }
}

void snlSquareLinear::printRhSides()
{
    // Print right hand sides to standard out.
    // ---------------------------------------
    
    int index = 0;    
    
    for ( int row = 0; row < num_unknowns; row ++ )
    {
        for ( int col = 0; col < num_rhSides; col ++ )
            cout << rhSides [ index ++ ] << "   ";
        
        cout << "\n";
    }
}

