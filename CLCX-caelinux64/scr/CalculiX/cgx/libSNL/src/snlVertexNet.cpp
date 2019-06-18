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

// Natural orientation of the network is:
//
//   Maximum V
//      |
//      |
//      |
//      |
//      0 ------- Maximum U
//
// You are looking at the front face. Unless you are inside the monitor ;-)

#include "snlVertexNet.h"

snlVertexNet::snlVertexNet()
{
    vertex_net = 0;
    
    size_u = 0;
    size_v = 0;
}

snlVertexNet::~snlVertexNet()
{
    if ( vertex_net ) delete[] vertex_net;
}

snlVertexNet::snlVertexNet ( const snlVertexNet& copyFrom )
{
    // Copy constructor.
    // -----------------
    
    int arraySize = copyFrom.size_u * copyFrom.size_v;
    
    vertex_net = new snlVertex [ arraySize ];
    
    for ( int index = 0; index < arraySize; index ++ )
        vertex_net [ index ] = copyFrom.vertex_net [ index ];    
}

void snlVertexNet::vertexNet ( const snlCtrlPoint* ctrlPts, int sizeU, int sizeV )
{    
    // Generate vertex net based on control point net.
    // -----------------------------------------------
    // ctrlPts:    Array of control points to copy.
    // sizeU:      Size of array in first dimension.
    // sizeV:      Size of array in second dimension.
    
    if ( vertex_net ) delete[] vertex_net;
    
    int numPts = sizeU * sizeV;
    
    vertex_net = new snlVertex [ numPts ];    
    
    size_u = sizeU;
    size_v = sizeV;
    
    // Translate control points into vertexs.
    
    for ( int index = 0; index < numPts; index ++ )
    {
        vertex_net [ index ] = ctrlPts [ index ];
        vertex_net [ index ].normalise();
    }
            
    calcNormals();
}

void snlVertexNet::vertexNet ( const snlCtrlPoint* ctrlPts, int numPts )
{    
    // Generate vertex net based on 1-D control point array.
    // -----------------------------------------------------
    // Notes:    Normals aren't calculated for a curve.
    
    if ( vertex_net ) delete[] vertex_net;    
    
    vertex_net = new snlVertex [ numPts ];
    
    size_u = 1;
    size_v = numPts;
    
    // Translate control points into vertexs.
    
    for ( int index = 0; index < numPts; index ++ )
    {
        vertex_net [ index ] = ctrlPts [ index ];
        vertex_net [ index ].normalise();
        ( vertex_net [ index ].normal() ).zero();
    }    
}

void snlVertexNet::vertexNet ( const snlPoint* pts, int sizeU, int sizeV )
{    
    // Generate vertex net based on point net.
    // ---------------------------------------
    // ctrlPts:    Array of control points to copy.
    // sizeU:      Size of array in first dimension.
    // sizeV:      Size of array in second dimension.
    
    if ( vertex_net ) delete[] vertex_net;
    
    int numPts = sizeU * sizeV;
    
    vertex_net = new snlVertex [ numPts ];    
    
    size_u = sizeU;
    size_v = sizeV;
    
    // Translate control points into vertexs.
    
    for ( int index = 0; index < numPts; index ++ )
    {
        vertex_net [ index ] = pts [ index ];
        vertex_net [ index ].normalise();
    }
            
    calcNormals();
}

void snlVertexNet::vertexNet ( const snlPoint* pts, int numPts )
{    
    // Generate vertex net based on 1-D point array.
    // ---------------------------------------------
    // Notes:    Normals aren't calculated for a curve.
    
    if ( vertex_net ) delete[] vertex_net;    
    
    vertex_net = new snlVertex [ numPts ];
    
    size_u = 1;
    size_v = numPts;
    
    // Translate control points into vertexs.
    
    for ( int index = 0; index < numPts; index ++ )
    {
        vertex_net [ index ] = pts [ index ];
        vertex_net [ index ].normalise();
        ( vertex_net [ index ].normal() ).zero();
    }    
}

void snlVertexNet::appendRow ( const snlCtrlPoint* ctrlPts )
{
    // Append row of points to end of net.
    // -----------------------------------
    
    if ( size_u < 1 ) return;  // Can't append to empty array.    
    
    int numPts = size_u * size_v;
    
    snlVertex* newVNet = new snlVertex [ numPts + size_v ];
    
    int index;
    
    for ( index = 0; index < numPts; index ++ )
        newVNet [ index ] = vertex_net [ index ];
        
    for ( index = 0; index < size_v; index ++ )
    {
        newVNet [ index + numPts ] = ctrlPts [ index ];
        newVNet [ index + numPts ].normalise();        
    }
    
    // Install new array.
    
    size_u ++;
    
    if ( vertex_net ) delete[] vertex_net;
    
    vertex_net = newVNet;
    
    calcNormals();
}

snlVertex* snlVertexNet::vertex ( int index )
{
    // Return reference to vertex at index.
    // ------------------------------------
    
    return vertex_net + index;
}

snlVertex* snlVertexNet::vertex ( int U, int V )
{
    // Return vertex at U,V coordinates.
    // ---------------------------------
    
    return vertex_net + ( U * size_v + V );
}

snlVertex* snlVertexNet::vertexes()
{
    // Return pointer to vertex array.
    // -------------------------------

    return vertex_net;
}

int snlVertexNet::size() const
{
    // Return total number of vertexes.
    // --------------------------------
    
    return size_u * size_v;
}

int snlVertexNet::sizeU() const
{
    return size_u;
}

int snlVertexNet::sizeV() const
{
    return size_v;
}

void snlVertexNet::calcNormals()
{
    // Calculate normals based on the vertex network.
    // ----------------------------------------------
    //
    // Notes:      maxV
    //              |
    //       minU --0-- maxU
    //              |
    //             minV
    
    
    snlVector   maxU;
    bool        useMaxU;
    
    snlVector   minU;
    bool        useMinU;
    
    snlVector   maxV;
    bool        useMaxV;
    
    snlVector   minV;
    bool        useMinV;

    for ( int indexU = 0; indexU < size_u; indexU ++ )
    {
        for ( int indexV = 0; indexV < size_v; indexV ++ )
        {
            int baseIndex = indexU * size_v + indexV;
            
            // U direction vectors.
            
            if ( indexU < size_u - 1 )
            {
                // MaxU can be calculated.
                
                maxU.calc ( vertex_net [ baseIndex ], vertex_net [ baseIndex + size_v ] );
                useMaxU = true;                
            }
            else
                useMaxU = false;
            
            if ( indexU > 0 )
            {
                // MinU can be calculated.
                
                minU.calc ( vertex_net [ baseIndex ], vertex_net [ baseIndex - size_v ] );
                useMinU = true;                
            }
            else
                useMinU = false;
            
            // V direction vectors.
            
            if ( indexV < size_v - 1 )
            {
                // Calculate maxV.
                maxV.calc ( vertex_net [ baseIndex ], vertex_net [ baseIndex + 1 ] );
                useMaxV = true;                
            }
            else
                useMaxV = false;
                
            if ( indexV > 0 )
            {
                minV.calc ( vertex_net [ baseIndex ], vertex_net [ baseIndex - 1 ] );
                useMinV = true;
            }
            else
                useMinV = false;
                
            // Calculate normals by combining normals from all combinations of
            // available vectors.
            
            snlVector   tmpNormal;
            snlVector   finalNormal;
            
            finalNormal.zero();
            
            if ( useMaxV && useMinU )
            {
                tmpNormal.crossProduct ( maxV, minU );
                finalNormal += tmpNormal;

                // Process diagonals.
                
                snlVector diag;
                diag.calc ( vertex_net [ baseIndex ], vertex_net [ baseIndex + 1 - size_v ] );

                tmpNormal.crossProduct ( maxV, diag );
                finalNormal += tmpNormal;

                tmpNormal.crossProduct ( diag, minU );
                finalNormal += tmpNormal;
            }
            
            if ( useMinU && useMinV )
            {
                tmpNormal.crossProduct ( minU, minV );
                finalNormal += tmpNormal;

                // Process diagonals.
                
                snlVector diag;
                diag.calc ( vertex_net [ baseIndex ], vertex_net [ baseIndex - 1 - size_v ] );

                tmpNormal.crossProduct ( minU, diag );
                finalNormal += tmpNormal;

                tmpNormal.crossProduct ( diag, minV );
                finalNormal += tmpNormal;
            }
            
            if ( useMinV && useMaxU )
            {
                tmpNormal.crossProduct ( minV, maxU );
                finalNormal += tmpNormal;

                // Process diagonals.
                
                snlVector diag;
                diag.calc ( vertex_net [ baseIndex ], vertex_net [ baseIndex - 1 + size_v ] );

                tmpNormal.crossProduct ( minV, diag );
                finalNormal += tmpNormal;

                tmpNormal.crossProduct ( diag, maxU );
                finalNormal += tmpNormal;
            }
            
            if ( useMaxU && useMaxV )
            {
                tmpNormal.crossProduct ( maxU, maxV );
                finalNormal += tmpNormal;

                // Process diagonals.
                
                snlVector diag;
                diag.calc ( vertex_net [ baseIndex ], vertex_net [ baseIndex + 1 + size_v ] );

                tmpNormal.crossProduct ( maxU, diag );
                finalNormal += tmpNormal;

                tmpNormal.crossProduct ( diag, maxV );
                finalNormal += tmpNormal;
            }
            
            // Set vertex's normal.
            
            finalNormal.unitise();
            
            vertex_net [ baseIndex ].normal ( finalNormal );
        }
    }    
}



