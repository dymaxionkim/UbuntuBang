// libSNL - Simple Nurbs Library
// Copyright 2003 Scott A.E. Lanham, Australia.
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

#include "snlPoint.h"

snlPoint::snlPoint()
{
    elements [ 0 ] = 0;
    elements [ 1 ] = 0;
    elements [ 2 ] = 0;
    elements [ 3 ] = 1;
}

snlPoint::snlPoint ( const snlPoint& copyFrom )
{
    // Copy constructor.
    // -----------------
    
    for ( unsigned index = 0; index < 4; index ++ )
        elements [ index ] = copyFrom.elements [ index ];
}

snlPoint::snlPoint ( double x, double y, double z, double w )
{
    elements [ 0 ] = x;
    elements [ 1 ] = y;
    elements [ 2 ] = z;
    elements [ 3 ] = w;
}

void snlPoint::components ( double x, double y, double z, double w )
{
    elements [ 0 ] = x;
    elements [ 1 ] = y;
    elements [ 2 ] = z;
    elements [ 3 ] = w;
}

void snlPoint::components ( double x, double y, double z )
{
    elements [ 0 ] = x;
    elements [ 1 ] = y;
    elements [ 2 ] = z;
}
       
void snlPoint::components ( double* x, double* y, double* z, double* w )
{
    *x = elements [ 0 ];
    *y = elements [ 1 ];
    *z = elements [ 2 ];
    *w = elements [ 3 ];
}

void snlPoint::components ( double* x, double* y, double* z )
{
    *x = elements [ 0 ];
    *y = elements [ 1 ];
    *z = elements [ 2 ];
}

double snlPoint::x() const
{
    return elements [ 0 ];
}

double snlPoint::y() const
{
    return elements [ 1 ];
}

double snlPoint::z() const
{
    return elements [ 2 ];
}

double snlPoint::w() const
{
    return elements [ 3 ];
}

void snlPoint::multiplyWeight ( double multiplier )
{
    // Multiply this points weight by multiplier.
    // ------------------------------------------

    for ( int index = 0; index < 4; index ++ )
        elements [ index ] *= multiplier;
}

void snlPoint::normalise()
{
    // Normalise point. ie Divide by w.
    // --------------------------------
    
    if ( elements [ 3 ] == 0.0 ) return;  // Stop divide by zero error.

    double w = elements [ 3 ];

    elements [ 0 ] /= w;
    elements [ 1 ] /= w;
    elements [ 2 ] /= w;
    elements [ 3 ] = 1.0;
}

void snlPoint::null()
{
    // Set everything to zero.
    // -----------------------
    
    elements [ 0 ] = 0;   
    elements [ 1 ] = 0;   
    elements [ 2 ] = 0;   
    elements [ 3 ] = 0;   
}

bool snlPoint::isNull()
{
    // Return true if point is null.
    // -----------------------------

    return elements [ 3 ] == 0;
}

void snlPoint::zero()
{
    // Set everything to zero.
    // -----------------------
    
    elements [ 0 ] = 0;   
    elements [ 1 ] = 0;   
    elements [ 2 ] = 0;   
    elements [ 3 ] = 1;   
}

snlPoint snlPoint::operator + ( const snlVector& vect ) const
{
    // Add a vector to this point
    // --------------------------

    snlPoint    retPt;
    int         index;

    if ( vect.homogeneous )
    {
        for ( index = 0; index < 4; index ++ )
            retPt.elements [ index ] = elements [ index ] + vect.elements [ index ];
    }
    else
    {
        for ( index = 0; index < 3; index ++ )
            retPt.elements [ index ] = elements [ index ] + vect.elements [ index ] * elements [ 3 ];
        
        retPt.elements [ 3 ] = elements [ 3 ];
    }

    return retPt;
}

snlPoint snlPoint::operator + ( const snlPoint& point ) const
{
    // Add a point to this point
    // -------------------------
    
    snlPoint    retPt;
    int         index;

    for ( index = 0; index < 4; index ++ )
        retPt.elements [ index ] = elements [ index ] + point.elements [ index ];

    return retPt;
}

snlPoint snlPoint::operator - ( const snlVector& vect ) const
{
    // Subtract a vector from this point
    // ---------------------------------
    
    snlPoint    retPt;
    int         index;
    
    if ( vect.homogeneous )
    {
        for ( index = 0; index < 4; index ++ )
            retPt.elements [ index ] = elements [ index ] - vect.elements [ index ];
    }
    else
    {
        for ( index = 0; index < 3; index ++ )
            retPt.elements [ index ] = elements [ index ] - vect.elements [ index ] * elements [ 3 ];
        
        retPt.elements [ 3 ] = elements [ 3 ];
    }
    
    return retPt;
}

snlPoint snlPoint::operator - ( const snlPoint& point ) const
{
    // Subtract a point from this point.
    // ---------------------------------
    
    snlPoint    retPt;
    int         index;

    for ( index = 0; index < 4; index ++ )
        retPt.elements [ index ] = elements [ index ] - point.elements [ index ];

    return retPt;
}

snlPoint snlPoint::operator * ( double scalar ) const
{
    // Multiply scalar to this point.
    // ------------------------------
    // scalar:      Scalar to multiply to point.
    
    snlPoint    retPt;
    
    for ( int index = 0; index < 4; index ++ )
        retPt.elements [ index ] = elements [ index ] * scalar;
    
    return retPt;
}

snlPoint snlPoint::operator / ( double scalar ) const
{
    // Divide this point by a scalar.
    // ------------------------------
    // scalar:      Scalar to divide point by.
    
    snlPoint    retPt;
    
    for ( int index = 0; index < 4; index ++ )
        retPt.elements [ index ] = elements [ index ] / scalar;
    
    return retPt;
}

void snlPoint::operator = ( const snlPoint& copyFrom )
{
    // Copy data from another point.
    // -----------------------------
    
    elements [ 0 ] = copyFrom.elements [ 0 ];
    elements [ 1 ] = copyFrom.elements [ 1 ];
    elements [ 2 ] = copyFrom.elements [ 2 ];
    elements [ 3 ] = copyFrom.elements [ 3 ];
}

void snlPoint::operator += ( const snlPoint& point )
{
    // Add a point to this point.
    // --------------------------
    
    elements [ 0 ] += point.elements [ 0 ];
    elements [ 1 ] += point.elements [ 1 ];
    elements [ 2 ] += point.elements [ 2 ];
    elements [ 3 ] += point.elements [ 3 ];
}

void snlPoint::operator += ( const snlVector& vect )
{
    // Add a vector to this point.
    // ---------------------------
    
    if ( vect.homogeneous )
    {
        elements [ 0 ] += vect.elements [ 0 ];
        elements [ 1 ] += vect.elements [ 1 ];
        elements [ 2 ] += vect.elements [ 2 ];
        elements [ 3 ] += vect.elements [ 3 ];
    }
    else
    {
        elements [ 0 ] += vect.elements [ 0 ] * elements [ 3 ];
        elements [ 1 ] += vect.elements [ 1 ] * elements [ 3 ];
        elements [ 2 ] += vect.elements [ 2 ] * elements [ 3 ];
    }
}

void snlPoint::operator -= ( const snlPoint& point )
{
    // Subtract a point from this point.
    // ---------------------------------
    
    elements [ 0 ] -= point.elements [ 0 ];
    elements [ 1 ] -= point.elements [ 1 ];
    elements [ 2 ] -= point.elements [ 2 ];
    elements [ 3 ] -= point.elements [ 3 ];
}

void snlPoint::operator *= ( double scalar )
{
    // Mulitply a scalar to this point.
    // --------------------------------
    
    for ( int index = 0; index < 4; index ++ )
        elements [ index ] *= scalar;
}

void snlPoint::operator /= ( double scalar )
{
    // Divide this point by a scalar.
    // ------------------------------
    
    for ( int index = 0; index < 4; index ++ )
        elements [ index ] /= scalar;
}

void snlPoint::x ( double val )
{
    // Set x component.
    // ----------------

    elements [ 0 ] = val;
}

void snlPoint::y ( double val )
{
    // Set y component.
    // ----------------

    elements [ 1 ] = val;
}

void snlPoint::z ( double val )
{
    // Set z component.
    // ----------------

    elements [ 2 ] = val;
}

void snlPoint::w ( double val )
{
    // Set w component.
    // ----------------

    elements [ 3 ] = val;
}

double snlPoint::lengthSqrd() const
{
    // Return length of this point, treated as a vector, squared.
    // ----------------------------------------------------------

    double sum = 0;

    for ( int index = 0; index < 4; index ++ )
        sum += elements [ index ] * elements [ index ];
        
    return sum;
}

double snlPoint::distSqrd ( const snlPoint& toPoint ) const
{
    // Return squared distance from this point to given point.
    // -------------------------------------------------------
    // toPoint:     Point to calculate distance to.
    //
    // Notes:       This is a 3D distance only.

    double      diff;
    
    double retVal = 0;

    if ( elements [ 3 ] == 1.0 && toPoint.elements [ 3 ] == 1.0 )
    {
        for ( int index = 0; index < 3; index ++ )
        {
            diff = elements [ index ] - toPoint.elements [ index ];
            retVal += diff * diff;
        }
    }
    else
    {
        for ( int index = 0; index < 3; index ++ )
        {
            diff = ( elements [ index ] / elements [ 3 ] )
                   - ( toPoint.elements [ index ] / toPoint.elements [ 3 ] );
                   
            retVal += diff * diff;
        }
    }

    return retVal;
}

bool snlPoint::operator == ( snlPoint& compare )
{
    // Return true if compare is eqivalent to this point.
    // --------------------------------------------------
    
    bool retVal = true;
    
    for ( int index = 0; index < 4; index ++ )
        if ( elements [ index ] != compare.elements [ index ] )
            retVal = false;
            
    return retVal;
}

void snlPoint::print() const
{
    // Print to std out.
    // -----------------
 
    cout << "( " << elements [ 0 ] << ", " <<  elements [ 1 ] << ", " << elements [ 2 ] << ", " << elements [ 3 ] << " )";
}

