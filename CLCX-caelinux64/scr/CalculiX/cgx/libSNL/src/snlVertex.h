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

#ifndef SNLVERTEX_H
#define SNLVERTEX_H

#include "snlPoint.h"
#include "snlKnotVector.h"

class snlVertex : public snlPoint
{
    public:
    
        snlVertex();
        
        snlVertex ( double x, double y, double z, double w );
        
        snlVertex ( const snlPoint& copyFrom );
        
        snlVector& normal();
        void normal ( snlVector& setTo );
        
        void evalParamU ( knot value );
        knot evalParamU();
        
        void evalParamV ( knot value );
        knot evalParamV();
        
        void operator = ( const snlPoint& copyFrom );        

        int         flag;  // Meaning depends on last operation performed on vertex.
    
    protected:
    
        snlVector   norm;  // Normal vector associated with vertex.
        
        knot        paramU;  // Parameter values that vertex was evaluated at.
        knot        paramV;  // Only one is used if a curve.
    
    private:
    
};

#endif

