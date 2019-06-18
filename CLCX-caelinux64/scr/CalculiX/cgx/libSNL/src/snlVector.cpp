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

#include "snlVector.h"

snlVector::snlVector()
{
    homogeneous = false;
}

snlVector::snlVector ( const snlPoint& pt1, const snlPoint& pt2, bool hmg )
{
    // Create vector from two points pt1 -> pt2
    // ----------------------------------------

    homogeneous = hmg;    
    
    calc ( pt1, pt2 );    
}

snlVector::snlVector ( snlPoint& pt, bool hmg )
{
    homogeneous = hmg;
    
    snlPoint tmpPt ( pt );
    
    if ( ! hmg )
    {
        tmpPt.normalise();
        elements [ 3 ] = 0.0;
    }
    else    
        elements [ 3 ] = tmpPt.w();
        
    elements [ 0 ] = tmpPt.x();
    elements [ 1 ] = tmpPt.y();
    elements [ 2 ] = tmpPt.z();    
}

snlVector::snlVector ( double x, double y, double z, double w, bool hmg )
{
    homogeneous = hmg;
    
    elements [ 0 ] = x;
    elements [ 1 ] = y;
    elements [ 2 ] = z;
    
    if ( hmg )
        elements [ 3 ] = w;
    else
        elements [ 3 ] = 0;
}

snlVector::snlVector ( snlVector& v1, snlVector& v2 )
{
    // Construct a vector that is normal to v1 and v2.
    // -----------------------------------------------

    homogeneous = false;

    crossProduct ( v1, v2 );
}

void snlVector::calc ( const snlPoint& pt1, const snlPoint& pt2 )
{
    // Calculate vector given start and end points.
    // --------------------------------------------

    if ( homogeneous )
    {
        for ( int index = 0; index < 4; index ++ )
            elements [ index ] = pt2.elements [ index ] - pt1.elements [ index ];
    }
    else
    {        
        for ( int index = 0; index < 3; index ++ )
            elements [ index ] = ( pt2.elements [ index ] / pt2.elements [ 3 ] ) -
                                 ( pt1.elements [ index ] / pt1.elements [ 3 ] );
        elements [ 3 ] = 0.0;
    }
}

void snlVector::crossProduct ( snlVector& v1, snlVector& v2 )
{
    // Calculate cross product of v1 X v2.
    // -----------------------------------
    // 
    // Notes:   Stores result in this vector.

    elements [ 0 ] = ( v1.elements [ 1 ] * v2.elements [ 2 ] ) -
                     ( v1.elements [ 2 ] * v2.elements [ 1 ] );

    elements [ 1 ] = ( v1.elements [ 2 ] * v2.elements [ 0 ] ) -
                     ( v1.elements [ 0 ] * v2.elements [ 2 ] );

    elements [ 2 ] = ( v1.elements [ 0 ] * v2.elements [ 1 ] ) -
                     ( v1.elements [ 1 ] * v2.elements [ 0 ] );

    elements [ 3 ] = 0.0;
}

double snlVector::dot ( snlVector& vect )
{
    // Calculate dot product between this vector and vect.
    // ---------------------------------------------------
    
    if ( homogeneous )
    {
        return ( elements [ 0 ] * vect.elements [ 0 ] + 
                 elements [ 1 ] * vect.elements [ 1 ] +
                 elements [ 2 ] * vect.elements [ 2 ] +
                 elements [ 3 ] * vect.elements [ 3 ] );
    }
    else
    {
        return ( elements [ 0 ] * vect.elements [ 0 ] + 
                 elements [ 1 ] * vect.elements [ 1 ] +
                 elements [ 2 ] * vect.elements [ 2 ] );
    }
}

double snlVector::dot ( snlPoint& pt )
{
    // Calculate dot product between this vector and pt.
    // -------------------------------------------------
    // Notes:   Treats pt as a vector.
    
    double dotProd = elements [ 0 ] * pt.elements [ 0 ] + 
                     elements [ 1 ] * pt.elements [ 1 ] +
                     elements [ 2 ] * pt.elements [ 2 ];
    
    if ( homogeneous )       
        dotProd += elements [ 3 ] * pt.elements [ 3 ];
        
    return dotProd;
}


snlVector snlVector::operator * ( double scalar )
{
    // Return vector multiplied by a scalar
    // ------------------------------------

    snlVector    retVect;

    for ( int index = 0; index < 4; index ++ )
        retVect.elements [ index ] = elements [ index ] * scalar;

    return retVect;
}

snlVector snlVector::operator + ( snlVector& vect )
{
    // Add vect to this one.
    // ---------------------

    snlVector    retVect;

    for ( int index = 0; index < 4; index ++ )
        retVect.elements [ index ] = elements [ index ] + vect.elements [ index ];

    return retVect;
}

snlVector snlVector::operator - ( snlVector& vect )
{
    // Subtract vect from this one.
    // ----------------------------

    snlVector    retVect;

    for ( int index = 0; index < 4; index ++ )
        retVect.elements [ index ] = elements [ index ] - vect.elements [ index ];

    return retVect;
}

void snlVector::operator += ( snlVector& vect )
{
    elements [ 0 ] += vect.elements [ 0 ];
    elements [ 1 ] += vect.elements [ 1 ];
    elements [ 2 ] += vect.elements [ 2 ];
    
    if ( homogeneous )
        elements [ 3 ] += vect.elements [ 3 ];
}

void snlVector::operator -= ( snlVector& vect )
{
    elements [ 0 ] -= vect.elements [ 0 ];
    elements [ 1 ] -= vect.elements [ 1 ];
    elements [ 2 ] -= vect.elements [ 2 ];
    
    if ( homogeneous )
        elements [ 3 ] -= vect.elements [ 3 ];
}

void snlVector::operator *= ( double scalar )
{
    // Multiply this vector by scalar.
    // -------------------------------

    for ( int index = 0; index < 4; index ++ )
        elements [ index ] *= scalar;
}

bool snlVector::operator == ( snlVector& compare )
{
    // Return true if compare is eqivalent to this vector.
    // ---------------------------------------------------
    
    bool retVal = true;

    if ( homogeneous )
    {
        for ( int index = 0; index < 4; index ++ )
            if ( elements [ index ] != compare.elements [ index ] )
                retVal = false;
    }
    else
    {
        for ( int index = 0; index < 3; index ++ )
            if ( elements [ index ] != compare.elements [ index ] )
                retVal = false;
    }
            
    return retVal;
}

double snlVector::lengthSqrd()
{
    // Return length of this vector squared
    // ------------------------------------

    double sum = 0;

    for ( int index = 0; index < 3; index ++ )
        sum += elements [ index ] * elements [ index ];
        
    if ( homogeneous )
        sum += elements [ 3 ] * elements [ 3 ];
        
    return sum;
}

double snlVector::length()
{
    // Return length of this vector
    // ----------------------------

    return sqrt ( lengthSqrd() );
}

void snlVector::length ( double val )
{
    // Set length of vector.
    // ---------------------
    // val:    Value to set length to.
    
    double multiplier = val / length();
    
    operator *= ( multiplier );
}

double snlVector::calcAbsCos ( snlVector& vect )
{
    // Calculate absolute cosine between this vector and vect.
    // -------------------------------------------------------
    
    return fabs ( dot ( vect ) / ( length() * vect.length() ) );
}

double snlVector::angle ( snlVector& vect )
{
    // Calculate angle between this vector and vect
    // --------------------------------------------
    //
    // Returns:     Value between 0 and PI in radians.
    
    return acos ( dot ( vect ) / ( length() * vect.length() ) ); 
}

void snlVector::unitise()
{
    // Turn vector into unit vector
    // ----------------------------

    double len = length();

    for ( int index = 0; index < 4; index ++ )
        elements [ index ] /= len;
}

double snlVector::projectDist ( snlVector& fromVector )
{
    // Caclulate projection from tip of vector to this vector.
    // -------------------------------------------------------
    // fromVector:      Vector to project tip of.
    //
    // Notes:           Vectors are assumed to originate from the same point.

    double dotP = dot ( fromVector );

    return sqrt ( fromVector.lengthSqrd() - ( dotP * dotP / lengthSqrd() ) );
}

snlVector snlVector::project ( snlVector& ontoVector )
{
    // Project this vector onto another vector.
    // ----------------------------------------
    // ontoVector:    Vector to project onto.
    //
    // Returns:       Vector that is the result of the projection.
    
    double newLength = dot ( ontoVector ) / ontoVector.length();
    
    snlVector retVector = ontoVector;
    
    retVector.length ( newLength );
    
    return retVector;
}

void snlVector::projectXZ()
{
    // Project onto the X-Z plane.
    // ---------------------------
    
    elements [ 1 ] = 0.0;  // y = 0.
}

void snlVector::projectXY()
{
    // Project onto the X-Y plane.
    // ---------------------------
    
    elements [ 2 ] = 0.0;  // z = 0.    
}

void snlVector::projectYZ()
{
    // Project onto the Y-Z plane.
    // ---------------------------
    
    elements [ 0 ] = 0.0;    
}

double snlVector::x()
{
    return elements [ 0 ];
}

double snlVector::y()
{
    return elements [ 1 ];
}

double snlVector::z()
{
    return elements [ 2 ];
}

double snlVector::w()
{
    return elements [ 3 ];
}

void snlVector::x ( double val )
{
    elements [ 0 ] = val;
}

void snlVector::y ( double val )
{
    elements [ 1 ] = val;
}

void snlVector::z ( double val )
{
    elements [ 2 ] = val;
}

void snlVector::w ( double val )
{
    elements [ 3 ] = val;
}

void snlVector::calcNormal ( snlPoint& pt1, snlPoint& pt2, snlPoint& pt3, snlPoint& pt4 )
{
    // Calculate normal and set this vector to it.
    // -------------------------------------------
    // NOTE:        Only does 3D vector product.

    snlVector v1 ( pt1, pt2 );
    snlVector v2 ( pt3, pt4 );

    crossProduct ( v1, v2 );

    unitise();
}

void snlVector::components ( double x, double y, double z, double w )
{
    elements [ 0 ] = x;
    elements [ 1 ] = y;
    elements [ 2 ] = z;
    elements [ 3 ] = w;
}

void snlVector::components ( double x, double y, double z )
{
    elements [ 0 ] = x;
    elements [ 1 ] = y;
    elements [ 2 ] = z;
    elements [ 3 ] = 0.0;
}

void snlVector::components ( double* x, double* y, double* z, double* w )
{
    *x = elements [ 0 ];
    *y = elements [ 1 ];
    *z = elements [ 2 ];
    *w = elements [ 3 ];
}

void snlVector::components ( double* x, double* y, double* z )
{
    *x = elements [ 0 ];
    *y = elements [ 1 ];
    *z = elements [ 2 ];
}

void snlVector::zero()
{
    // Zero the vector.
    // ----------------
    
    elements [ 0 ] = 0.0;
    elements [ 1 ] = 0.0;
    elements [ 2 ] = 0.0;
    elements [ 3 ] = 0.0;
}

bool snlVector::isNull()
{
    if ( homogeneous )
    {
        if ( ! elements [ 0 ] &&
             ! elements [ 1 ] &&
             ! elements [ 2 ] &&
             ! elements [ 3 ] )
            return true;
    }
    else    
    {
        if ( ! elements [ 0 ] &&
             ! elements [ 1 ] &&
             ! elements [ 2 ] )
        return true;
    }

    return false;        
}

void snlVector::print()
{
    // Print vector contents to cout.
    // ------------------------------

    cout << "X: " << elements [ 0 ] << " Y: " << elements [ 1 ]
         << " Z: " << elements [ 2 ] << " W: " << elements [ 3 ] << "\n";
}
