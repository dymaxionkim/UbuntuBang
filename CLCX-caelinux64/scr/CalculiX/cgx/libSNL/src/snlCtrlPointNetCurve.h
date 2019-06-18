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

#ifndef SNL_CTRLPTNETCURVE_H
#define SNL_CTRLPTNETCURVE_H

#include "snlCtrlPointNet.h"

class snlCtrlPointNetCurve : public snlCtrlPointNet
{
    public:

        snlCtrlPointNetCurve ( snlCtrlPoint* cPtArray, unsigned size, bool copy = false );
        
        snlCtrlPointNetCurve ( unsigned size, snlPoint& start, snlPoint& end );

        virtual ~snlCtrlPointNetCurve();

        unsigned size() const;        

        // Increase the control point net's size.
        snlCtrlPoint* grow();
        
        // Decrease the control point net's size.
        snlCtrlPoint* shrink();        
        
        double calcFlatness ( int index, int numPoints ) const;
        
        void truncate ( int atIndex, bool keepLast );

        void reverse();

        // snlCtrlPointNet Abstract implementation.

        virtual int getCnctPts ( unsigned index, snlCtrlPoint* retPts );

        virtual int maxConnections() const;
        
    private:
        
};

#endif
