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

#include "snlVertex.h"

snlVertex::snlVertex()
{
}

snlVertex::snlVertex ( double x, double y, double z, double w )
    : snlPoint ( x, y, z, w )
{    
}

snlVertex::snlVertex ( const snlPoint&  copyFrom )
    : snlPoint ( copyFrom )
{    
}

void snlVertex::operator = ( const snlPoint& copyFrom )
{
    // Copy data from a point.
    // -----------------------
    
   snlPoint::operator = ( copyFrom );   
}

void snlVertex::normal ( snlVector& setTo )
{
    // Set vertex's normal.
    // --------------------
    
    norm = setTo;
}

snlVector& snlVertex::normal()
{
    return norm;
}

void snlVertex::evalParamU ( knot value )
{
    // Set U parameter vertex was evaluated at.
    // ----------------------------------------
    
    paramU = value;
}

knot snlVertex::evalParamU()
{
    // Get U parameter vertex was evaluated at.
    // ----------------------------------------
    
    return paramU;
}
        
void snlVertex::evalParamV ( knot value )
{
    // Set V parameter vertex was evaluated at.
    // ----------------------------------------
    
    paramV = value;
}

knot snlVertex::evalParamV()
{
    // Set V parameter vertex was evaluated at.
    // ----------------------------------------
    
    return paramV;
}



