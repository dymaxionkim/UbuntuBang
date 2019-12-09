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

// *** 2D Vertex Net ***

#ifndef SNLVERTEXNET_H
#define SNLVERTEXNET_H

#include "snlVertex.h"
#include "snlCtrlPoint.h"

#ifdef SGI_MIPS

    #include <iostream.h>
    #include <math.h>
    
#else

    #include <iostream>
    #include <cmath>
    
    using namespace std;
    
#endif

class snlVertexNet
{
    public:        

        snlVertexNet();
        virtual ~snlVertexNet();
        
        snlVertexNet ( const snlVertexNet& copyFrom );
        
        void vertexNet ( const snlCtrlPoint* ctrlPts, int sizeU, int sizeV );
        void vertexNet ( const snlCtrlPoint* ctrlPts, int numPts );
        
        void vertexNet ( const snlPoint* pts, int sizeU, int sizeV );
        void vertexNet ( const snlPoint* pts, int numPts );
        
        void appendRow ( const snlCtrlPoint* ctrlPts );
        
        snlVertex* vertex ( int index );
        snlVertex* vertex ( int U, int V );

        snlVertex* vertexes();
        
        int size() const;
        int sizeU() const;
        int sizeV() const;
        
        void calcNormals();
        
    private:
    
        snlVertex*      vertex_net;  // 2D Vertex network.
        
        int     size_u;  // Size of network in first dimension.
        int     size_v;  // Size of network in second dimension.
};

#endif
