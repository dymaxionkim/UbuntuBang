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

// *** Geometric Transformations ***

#ifndef SNLTRANSFORM_H
#define SNLTRANSFORM_H

#include "snlPoint.h"
#include "snlVector.h"
#include "snlMatrix_4x4.h"

class snlTransform : public snlMatrix_4X4
{
    public:

        snlTransform();

        snlTransform ( double* matrix );

        void translate ( double x, double y, double z, bool pre = false );

        void rotateX ( double angle, bool pre = false );
        void rotateY ( double angle, bool pre = false );
        void rotateZ ( double angle, bool pre = false );
        
        void rotate ( double angle, snlPoint& axisStart, snlVector& axisDirection, bool pre = false);
        void rotate ( double angle, snlPoint& axisStart, snlPoint& axisEnd );

        void scale ( double x, double y, double z, snlPoint& relTo, bool pre = false );
        void scale ( double x, double y, double z, bool pre = false );
        
        void align ( snlVector& vector1, snlVector& vector2, bool pre = false );

        void transform ( snlPoint* pt );
        void transform ( snlPoint& pt );
        void transform ( snlVector& vect );
        void transform ( double* elements );
};

#endif
