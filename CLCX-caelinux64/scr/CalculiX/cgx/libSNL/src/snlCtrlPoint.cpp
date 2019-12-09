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

#include "snlCtrlPoint.h"

snlCtrlPoint::snlCtrlPoint()
{
    selected = false;
}

snlCtrlPoint::snlCtrlPoint ( snlPoint& pt )
    : snlPoint ( pt )
{
    selected = false;
}

void snlCtrlPoint::operator = ( const snlPoint& copyFrom )
{
    // Copy data from another point.
    // -----------------------------
    
    elements [ 0 ] = copyFrom.elements [ 0 ];
    elements [ 1 ] = copyFrom.elements [ 1 ];
    elements [ 2 ] = copyFrom.elements [ 2 ];
    elements [ 3 ] = copyFrom.elements [ 3 ];
    
    selected = false;
}

void snlCtrlPoint::select ( bool yesNo )
{
    // Set selection state of control point.
    // -------------------------------------

    selected = yesNo;
}

bool snlCtrlPoint::isSelected()
{
    return selected;
}

void snlCtrlPoint::weight ( double setTo )
{
    // Set control points weight.
    // --------------------------
    
    if ( elements [ 3 ] == 0.0 )
        elements [ 3 ] = setTo;
    else
    {    
        double multFactor = setTo / elements [ 3 ];
    
        for ( int index = 0; index < 4; index ++ )
            elements [ index ] *= multFactor;
    }
}

double snlCtrlPoint::weight()
{
    return elements [ 3 ];
}

