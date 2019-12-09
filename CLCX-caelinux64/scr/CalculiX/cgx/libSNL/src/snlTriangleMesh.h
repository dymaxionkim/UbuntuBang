// libSNL - Simple Nurbs Library
// Copyright 2005 Scott A.E. Lanham, Australia.
// --------------------------------------------
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.

// *** Mesh of Triangle Elements ***

#ifndef SNL_TRIANGLEMESH_H
#define SNL_TRIANGLEMESH_H

#include "snlVertex.h"

#include "dynamicArray.h"

#ifdef SGI_MIPS

    #include <iostream.h>
    #include <math.h>
    
#else

    #include <iostream>
    #include <cmath>
    
    using namespace std;
    
#endif

typedef struct
{
    int     vertexIndex1;
    int     vertexIndex2;
    int     triangleIndex1;  // Used for efficient ordering of triangles.
    int     triangleIndex2;

} snlTriangleEdge;

typedef struct
{
    int     edgeIndex1;
    int     edgeIndex2;
    int     edgeIndex3;

} snlTriangle;

class snlTriangleMesh
{
    public:

        snlTriangleMesh();

        snlTriangleMesh ( int arrayPageSize );

        void init();

        int addVertex ( snlVertex& vertex );
        int addVertexes ( const snlVertex* vertexesToCopy, int numberToAdd );
        int addEdge ( int vertexIndex1, int vertexIndex2 );
        int addTriangle ( int edgeIndex1, int edgeIndex2, int edgeIndex3 );

    protected:

    private:

        int         array_page_size;  // Number of array elements per page for "growable" arrays.
        

        dynamicArray< snlVertex >*          vertexes;
        int                                 num_vertexes;

        dynamicArray< snlTriangleEdge >*    edges;
        int                                 num_edges;
        
        dynamicArray< snlTriangle >*        triangles;
        int                                 num_triangles;
};

#endif

