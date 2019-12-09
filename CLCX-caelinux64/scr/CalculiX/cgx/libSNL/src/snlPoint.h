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

#ifndef SNLPOINT_H
#define SNLPOINT_H

class snlVector;

#include "snlVector.h"

class snlPoint
{
    public:

        snlPoint();        
        
        snlPoint ( const snlPoint& copyFrom );
        
        snlPoint ( double x, double y, double z, double w = 1.0 );
        
        void components ( double x, double y, double z, double w );
        void components ( double x, double y, double z );
        
        void components ( double* x, double* y, double* z, double* w );
        void components ( double* x, double* y, double* z );

        double x() const;
        double y() const;
        double z() const;
        double w() const;

        void x ( double );
        void y ( double );
        void z ( double );
        void w ( double );

        void multiplyWeight ( double multiplier );

        void normalise();
        
        void null();  // Set everything to zero.

        bool isNull();
        
        void zero();  // Set everything to zero and w to 1.
        
        double lengthSqrd() const;  // Treat point as vector.
        double distSqrd ( const snlPoint& toPoint ) const;

        snlPoint operator + ( const snlVector& vect ) const;
        snlPoint operator + ( const snlPoint& point ) const;
        snlPoint operator - ( const snlVector& vect ) const;
        snlPoint operator - ( const snlPoint& point ) const;
        snlPoint operator * ( double scalar ) const;
        snlPoint operator / ( double scalar ) const;
        
        void operator = ( const snlPoint& copyFrom );
        void operator += ( const snlPoint& point );
        void operator += ( const snlVector& vect );
        void operator -= ( const snlPoint& point );        
        void operator *= ( double scalar );
        void operator /= ( double scalar );
        
        bool operator == ( snlPoint& compare );
        
        void print() const;

        double elements [ 4 ];  // x, y, z, w.
};

#endif
