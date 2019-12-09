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

#include "snlTriangleMesh.h"

snlTriangleMesh::snlTriangleMesh()
{
    array_page_size = 0;

    init();
}

snlTriangleMesh::snlTriangleMesh ( int arrayPageSize )
{
    array_page_size = arrayPageSize;

    init();
}

void snlTriangleMesh::init()
{
    // Initialisation function.
    // ------------------------
    
    if ( array_page_size )
    {
        vertexes = new dynamicArray< snlVertex > ( array_page_size );
        edges = new dynamicArray< snlTriangleEdge > ( array_page_size );
        triangles = new dynamicArray< snlTriangle > ( array_page_size );
    }
    else
    {
        vertexes = new dynamicArray< snlVertex >;
        edges = new dynamicArray< snlTriangleEdge >;
        triangles = new dynamicArray< snlTriangle >;
    }
    
    num_vertexes = 0;
    num_edges = 0;
    num_triangles = 0;
}

int snlTriangleMesh::addVertex ( snlVertex& vertex )
{
    // Add vertex to mesh.
    // -------------------
    // vertex:      Vertex to add.
    //
    // Returns:     Index of added vertex.

    (*vertexes) [ num_vertexes ++ ] = vertex;

    return num_vertexes - 1;
}

int snlTriangleMesh::addVertexes ( const snlVertex* vertexesToCopy, int numberToAdd )
{
    // Add Vertexes to mesh.
    // ---------------------
    // vertexesToCopy:  Pointer to vertexes to copy.
    // numberToAdd:     Number of vertexes to being pointed to.
    //
    // Returns:      Index of last vertex added.

    for ( int index = 0; index < numberToAdd; index ++ )
        (*vertexes) [ num_vertexes ++ ] = vertexesToCopy [ index ];

    return num_vertexes - 1;
}

int snlTriangleMesh::addEdge ( int vertexIndex1, int vertexIndex2 )
{
    // Add edge to mesh.
    // -----------------
    // vertexIndex1:    Index of edges first vertex.
    // vertexIndex2:    Index of edges second vertex.
    //
    // returns:         Index of added edge.

    snlTriangleEdge& edge = (*edges) [ num_edges ++ ];

    edge.vertexIndex1 = vertexIndex1;
    edge.vertexIndex2 = vertexIndex2;
    edge.triangleIndex1 = -1;
    edge.triangleIndex2 = -1;
    
    return num_edges - 1;
}

int snlTriangleMesh::addTriangle ( int edgeIndex1, int edgeIndex2, int edgeIndex3 )
{
    // Add triangle to mesh.
    // ---------------------
    // edgeIndex1:      Index of triangles first edge.
    // edgeIndex2:      Index of triangles second edge.
    // edgeIndex3:      Index of triangles third edge.
    //
    // Returns:         Index of added triangle.

    int triIndex = num_triangles;  // New triangle's index.

    num_triangles ++;

    snlTriangle& triangle = (*triangles) [ triIndex ];

    triangle.edgeIndex1 = edgeIndex1;
    triangle.edgeIndex2 = edgeIndex2;
    triangle.edgeIndex3 = edgeIndex3;

    snlTriangleEdge& edge1 = (*edges) [ edgeIndex1 ];
    snlTriangleEdge& edge2 = (*edges) [ edgeIndex2 ];
    snlTriangleEdge& edge3 = (*edges) [ edgeIndex3 ];

    if ( edge1.triangleIndex1 == -1 )
        edge1.triangleIndex1 = triIndex;
    else
        edge1.triangleIndex2 = triIndex;

    if ( edge2.triangleIndex1 == -1 )
        edge2.triangleIndex1 = triIndex;
    else
        edge2.triangleIndex2 = triIndex;
    
    if ( edge3.triangleIndex1 == -1 )
        edge3.triangleIndex1 = triIndex;
    else
        edge3.triangleIndex2 = triIndex;

    return triIndex;
}

