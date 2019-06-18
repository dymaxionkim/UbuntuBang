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

#ifndef SNLVECTOR_H
#define SNLVECTOR_H

class snlPoint;

#include "snlPoint.h"

#ifdef SGI_MIPS

    #include <iostream.h>
    #include <math.h>
    
#else

    #include <iostream>
    #include <cmath>
    
    using namespace std;
    
#endif

class snlVector
{
    public:

        snlVector();
        snlVector ( const snlPoint& pt1, const snlPoint& pt2, bool hmg = false );  // pt1 -> pt2. ( ie pt2 - pt1 ).
        snlVector ( snlPoint& pt, bool hmg = false );        
        snlVector ( double x, double y, double z, double w = 0.0, bool hmg = false );
        snlVector ( snlVector& v1, snlVector& v2 );

        void calc ( const snlPoint& pt1, const snlPoint& pt2 );
        
        void calcNormal ( snlPoint& pt1, snlPoint& pt2, snlPoint& pt3, snlPoint& pt4 );

        void crossProduct ( snlVector& v1, snlVector& v2 );
        
        double dot ( snlVector& vect );  // Dot product of two vectors.
        double dot ( snlPoint& pt );  // Treats point as vector.        

        double lengthSqrd();  // Length of vector squared.
        double length();  // Length of vector.
        void length ( double val );  // Set length of vector.
        
        double calcAbsCos ( snlVector& vect );  // Calculate absolute cosine of the angle between vectors.        
        double angle ( snlVector& vect );
        
        void unitise();  // Don't know if this is a real word. Turn vector into unit vector.

        double projectDist ( snlVector& fromVector );
        snlVector project ( snlVector& ontoVector );
        
        void projectXZ();  // Project onto the X-Z plane.        
        void projectXY();  // Project onto the X-Y plane.
        void projectYZ();  // Project onto the Y-Z plane.

        snlVector operator * ( double );  // Return vector multiplied by a scalar.
        snlVector operator + ( snlVector& vect );
        snlVector operator - ( snlVector& vect );
        
        void operator += ( snlVector& vect );
        void operator -= ( snlVector& vect );
        void operator *= ( double );  // Multiply this vector by a scalar.

        bool operator == ( snlVector& compare );

        double x();
        double y();
        double z();
        double w();
        
        void x ( double val );
        void y ( double val );
        void z ( double val );
        void w ( double val );
        
        void components ( double x, double y, double z, double w );
        void components ( double x, double y, double z );
        
        void components ( double* x, double* y, double* z, double* w );
        void components ( double* x, double* y, double* z );
        
        void zero();  // Zero the vector.
        
        bool isNull();

        void print();

        double  elements [ 4 ];
        
        bool    homogeneous;  // If true then is a 4-D vector.
};

#endif
