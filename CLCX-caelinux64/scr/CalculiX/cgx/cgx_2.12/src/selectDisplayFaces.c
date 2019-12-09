/* --------------------------------------------------------------------  */
/*                          CALCULIX                                     */
/*                   - GRAPHICAL INTERFACE -                             */
/*                                                                       */
/*     A 3-dimensional pre- and post-processor for finite elements       */
/*              Copyright (C) 1996 Klaus Wittig                          */
/*                                                                       */
/*     This program is free software; you can redistribute it and/or     */
/*     modify it under the terms of the GNU General Public License as    */
/*     published by the Free Software Foundation; version 2 of           */
/*     the License.                                                      */
/*                                                                       */
/*     This program is distributed in the hope that it will be useful,   */
/*     but WITHOUT ANY WARRANTY; without even the implied warranty of    */ 
/*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the      */
/*     GNU General Public License for more details.                      */
/*                                                                       */
/*     You should have received a copy of the GNU General Public License */
/*     along with this program; if not, write to the Free Software       */
/*     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.         */
/* --------------------------------------------------------------------  */


/* TODO */
/*
 Bugs:
*/

#include <cgx.h>
#include  <time.h>

#define     TEST            0       /* debugging */



int commonEdge2 (Summen *anz, Edges **edgePtr, Elements *e_enqire, int *a, int *b)
{
  Edges     *edge;
  //printf(" in commonEdge2 a: %d %d b: %d %d\n", a[0],a[1], b[0], b[1]);

  edge=*edgePtr;

  switch(e_enqire[a[0]].type)
  {
    case 1:
      if ( (edge = (Edges *)realloc( (Edges *)edge, (anz->g+1) * sizeof(Edges))) == NULL )
        printf("\n\n ERROR: realloc failed edge\n\n") ;
      //printf("edge:%d el: %d %d f: %d %d t: %d %d\n", anz->g, a[0], b[0], a[1], b[1], e_enqire[a[0]].type, e_enqire[b[0]].type);
      switch(a[1])
      {
        case 0:
          switch(b[1])
          {
            case 2:
              edge[anz->g].p1=  e_enqire[a[0]].nod[0];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[1];
	    break;
            case 3:
              edge[anz->g].p1=  e_enqire[a[0]].nod[1];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[2];
	    break;
            case 4:
              edge[anz->g].p1=  e_enqire[a[0]].nod[2];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[3];
	    break;
            case 5:
              edge[anz->g].p1=  e_enqire[a[0]].nod[3];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[0];
	    break;
	  }
        break;
        case 1:
          switch(b[1])
          {
            case 2:
              edge[anz->g].p1=  e_enqire[a[0]].nod[4];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[5];
	    break;
            case 3:
              edge[anz->g].p1=  e_enqire[a[0]].nod[5];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[6];
	    break;
            case 4:
              edge[anz->g].p1=  e_enqire[a[0]].nod[6];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[7];
	    break;
            case 5:
              edge[anz->g].p1=  e_enqire[a[0]].nod[7];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[4];
	    break;
	  }
        break;
        case 2:
          switch(b[1])
          {
            case 0:
              edge[anz->g].p1=  e_enqire[a[0]].nod[0];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[1];
	    break;
            case 1:
              edge[anz->g].p1=  e_enqire[a[0]].nod[4];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[5];
	    break;
            case 3:
              edge[anz->g].p1=  e_enqire[a[0]].nod[1];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[5];
	    break;
            case 5:
              edge[anz->g].p1=  e_enqire[a[0]].nod[4];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[0];
	    break;
	  }
        break;
        case 3:
          switch(b[1])
          {
            case 0:
              edge[anz->g].p1=  e_enqire[a[0]].nod[1];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[2];
	    break;
            case 1:
              edge[anz->g].p1=  e_enqire[a[0]].nod[5];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[6];
	    break;
            case 2:
              edge[anz->g].p1=  e_enqire[a[0]].nod[1];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[5];
	    break;
            case 4:
              edge[anz->g].p1=  e_enqire[a[0]].nod[2];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[6];
	    break;
	  }
        break;
        case 4:
          switch(b[1])
          {
            case 0:
              edge[anz->g].p1=  e_enqire[a[0]].nod[2];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[3];
	    break;
            case 1:
              edge[anz->g].p1=  e_enqire[a[0]].nod[6];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[7];
	    break;
            case 3:
              edge[anz->g].p1=  e_enqire[a[0]].nod[2];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[6];
	    break;
            case 5:
              edge[anz->g].p1=  e_enqire[a[0]].nod[3];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[7];
	    break;
	  }
        break;
        case 5:
          switch(b[1])
          {
            case 0:
              edge[anz->g].p1=  e_enqire[a[0]].nod[0];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[3];
	    break;
            case 1:
              edge[anz->g].p1=  e_enqire[a[0]].nod[4];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[7];
	    break;
            case 2:
              edge[anz->g].p1=  e_enqire[a[0]].nod[0];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[4];
	    break;
            case 4:
              edge[anz->g].p1=  e_enqire[a[0]].nod[3];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[7];
	    break;
	  }
        break;
      }
    break;
    case 2:
      if ( (edge = (Edges *)realloc( (Edges *)edge, (anz->g+1) * sizeof(Edges))) == NULL )
        printf("\n\n ERROR: realloc failed edge\n\n") ;
      switch(a[1])
      {
        case 0:
          switch(b[1])
          {
            case 2:
              edge[anz->g].p1=  e_enqire[a[0]].nod[0];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[1];
	    break;
            case 3:
              edge[anz->g].p1=  e_enqire[a[0]].nod[1];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[2];
	    break;
            case 4:
              edge[anz->g].p1=  e_enqire[a[0]].nod[2];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[0];
	    break;
	  }
        break;
        case 1:
          switch(b[1])
          {
            case 2:
              edge[anz->g].p1=  e_enqire[a[0]].nod[3];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[4];
	    break;
            case 3:
              edge[anz->g].p1=  e_enqire[a[0]].nod[4];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[5];
	    break;
            case 4:
              edge[anz->g].p1=  e_enqire[a[0]].nod[3];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[5];
	    break;
	  }
        break;
        case 2:
          switch(b[1])
          {
            case 0:
              edge[anz->g].p1=  e_enqire[a[0]].nod[0];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[1];
	    break;
            case 1:
              edge[anz->g].p1=  e_enqire[a[0]].nod[3];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[4];
	    break;
            case 3:
              edge[anz->g].p1=  e_enqire[a[0]].nod[1];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[4];
	    break;
	  }
        break;
        case 3:
          switch(b[1])
          {
            case 0:
              edge[anz->g].p1=  e_enqire[a[0]].nod[1];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[2];
	    break;
            case 1:
              edge[anz->g].p1=  e_enqire[a[0]].nod[4];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[5];
	    break;
            case 2:
              edge[anz->g].p1=  e_enqire[a[0]].nod[1];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[4];
	    break;
	  }
        break;
        case 4:
          switch(b[1])
          {
            case 1:
              edge[anz->g].p1=  e_enqire[a[0]].nod[3];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[5];
	    break;
            case 2:
              edge[anz->g].p1=  e_enqire[a[0]].nod[0];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[3];
	    break;
            case 3:
              edge[anz->g].p1=  e_enqire[a[0]].nod[2];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[5];
	    break;
	  }
        break;
      }
    break;
    case 3:
      if ( (edge = (Edges *)realloc( (Edges *)edge, (anz->g+1) * sizeof(Edges))) == NULL )
        printf("\n\n ERROR: realloc failed edge\n\n") ;
      switch(a[1])
      {
        case 0:
          switch(b[1])
          {
            case 1:
              edge[anz->g].p1=  e_enqire[a[0]].nod[0];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[1];
	    break;
            case 2:
              edge[anz->g].p1=  e_enqire[a[0]].nod[1];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[2];
	    break;
            case 3:
              edge[anz->g].p1=  e_enqire[a[0]].nod[2];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[0];
	    break;
	  }
        break;
        case 1:
          switch(b[1])
          {
            case 0:
              edge[anz->g].p1=  e_enqire[a[0]].nod[0];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[1];
	    break;
            case 2:
              edge[anz->g].p1=  e_enqire[a[0]].nod[1];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[3];
	    break;
            case 3:
              edge[anz->g].p1=  e_enqire[a[0]].nod[3];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[0];
	    break;
	  }
        break;
        case 2:
          switch(b[1])
          {
            case 0:
              edge[anz->g].p1=  e_enqire[a[0]].nod[1];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[2];
	    break;
            case 1:
              edge[anz->g].p1=  e_enqire[a[0]].nod[1];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[3];
	    break;
            case 3:
              edge[anz->g].p1=  e_enqire[a[0]].nod[3];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[2];
	    break;
	  }
        break;
        case 3:
          switch(b[1])
          {
            case 0:
              edge[anz->g].p1=  e_enqire[a[0]].nod[0];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[2];
	    break;
            case 1:
              edge[anz->g].p1=  e_enqire[a[0]].nod[0];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[3];
	    break;
            case 2:
              edge[anz->g].p1=  e_enqire[a[0]].nod[3];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[2];
	    break;
	  }
        break;
      }
    break;
    case 4:
      if ( (edge = (Edges *)realloc( (Edges *)edge, (anz->g+2) * sizeof(Edges))) == NULL )
        printf("\n\n ERROR: realloc failed edge\n\n") ;
      switch(a[1])
      {
        case 0:
          switch(b[1])
          {
            case 2:
              edge[anz->g].p1=  e_enqire[a[0]].nod[0];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[8];

              edge[anz->g].p1=  e_enqire[a[0]].nod[8];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[1];
	    break;
            case 3:
              edge[anz->g].p1=  e_enqire[a[0]].nod[1];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[9];

              edge[anz->g].p1=  e_enqire[a[0]].nod[9];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[2];
	    break;
            case 4:
              edge[anz->g].p1=  e_enqire[a[0]].nod[2];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[10];

              edge[anz->g].p1=  e_enqire[a[0]].nod[10];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[3];
	    break;
            case 5:
              edge[anz->g].p1=  e_enqire[a[0]].nod[3];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[11];

              edge[anz->g].p1=  e_enqire[a[0]].nod[11];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[0];
	    break;
	  }
        break;
        case 1:
          switch(b[1])
          {
            case 2:
              edge[anz->g].p1=  e_enqire[a[0]].nod[4];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[16];

              edge[anz->g].p1=  e_enqire[a[0]].nod[16];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[5];
	    break;
            case 3:
              edge[anz->g].p1=  e_enqire[a[0]].nod[5];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[17];

              edge[anz->g].p1=  e_enqire[a[0]].nod[17];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[6];
	    break;
            case 4:
              edge[anz->g].p1=  e_enqire[a[0]].nod[6];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[18];

              edge[anz->g].p1=  e_enqire[a[0]].nod[18];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[7];
	    break;
            case 5:
              edge[anz->g].p1=  e_enqire[a[0]].nod[7];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[19];

              edge[anz->g].p1=  e_enqire[a[0]].nod[19];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[4];
	    break;
	  }
        break;
        case 2:
          switch(b[1])
          {
            case 0:
              edge[anz->g].p1=  e_enqire[a[0]].nod[0];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[8];

              edge[anz->g].p1=  e_enqire[a[0]].nod[8];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[1];
	    break;
            case 1:
              edge[anz->g].p1=  e_enqire[a[0]].nod[4];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[16];

              edge[anz->g].p1=  e_enqire[a[0]].nod[16];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[5];
	    break;
            case 3:
              edge[anz->g].p1=  e_enqire[a[0]].nod[1];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[13];

              edge[anz->g].p1=  e_enqire[a[0]].nod[13];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[5];
	    break;
            case 5:
              edge[anz->g].p1=  e_enqire[a[0]].nod[4];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[12];

              edge[anz->g].p1=  e_enqire[a[0]].nod[12];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[0];
	    break;
	  }
        break;
        case 3:
          switch(b[1])
          {
            case 0:
              edge[anz->g].p1=  e_enqire[a[0]].nod[1];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[9];

              edge[anz->g].p1=  e_enqire[a[0]].nod[9];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[2];
	    break;
            case 1:
              edge[anz->g].p1=  e_enqire[a[0]].nod[5];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[17];

              edge[anz->g].p1=  e_enqire[a[0]].nod[17];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[6];
	    break;
            case 2:
              edge[anz->g].p1=  e_enqire[a[0]].nod[1];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[13];

              edge[anz->g].p1=  e_enqire[a[0]].nod[13];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[5];
	    break;
            case 4:
              edge[anz->g].p1=  e_enqire[a[0]].nod[2];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[14];

              edge[anz->g].p1=  e_enqire[a[0]].nod[14];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[6];
	    break;
	  }
        break;
        case 4:
          switch(b[1])
          {
            case 0:
              edge[anz->g].p1=  e_enqire[a[0]].nod[2];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[10];

              edge[anz->g].p1=  e_enqire[a[0]].nod[10];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[3];
	    break;
            case 1:
              edge[anz->g].p1=  e_enqire[a[0]].nod[6];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[18];

              edge[anz->g].p1=  e_enqire[a[0]].nod[18];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[7];
	    break;
            case 3:
              edge[anz->g].p1=  e_enqire[a[0]].nod[2];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[14];

              edge[anz->g].p1=  e_enqire[a[0]].nod[14];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[6];
	    break;
            case 5:
              edge[anz->g].p1=  e_enqire[a[0]].nod[3];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[15];

              edge[anz->g].p1=  e_enqire[a[0]].nod[15];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[7];
	    break;
	  }
        break;
        case 5:
          switch(b[1])
          {
            case 0:
              edge[anz->g].p1=  e_enqire[a[0]].nod[0];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[11];

              edge[anz->g].p1=  e_enqire[a[0]].nod[11];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[3];
	    break;
            case 1:
              edge[anz->g].p1=  e_enqire[a[0]].nod[4];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[19];

              edge[anz->g].p1=  e_enqire[a[0]].nod[19];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[7];
	    break;
            case 2:
              edge[anz->g].p1=  e_enqire[a[0]].nod[0];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[12];

              edge[anz->g].p1=  e_enqire[a[0]].nod[12];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[4];
	    break;
            case 4:
              edge[anz->g].p1=  e_enqire[a[0]].nod[3];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[15];

              edge[anz->g].p1=  e_enqire[a[0]].nod[15];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[7];
	    break;
	  }
        break;
      }
    break;
    case 5:
      if ( (edge = (Edges *)realloc( (Edges *)edge, (anz->g+2) * sizeof(Edges))) == NULL )
        printf("\n\n ERROR: realloc failed edge\n\n") ;
      switch(a[1])
      {
        case 0:
          switch(b[1])
          {
            case 2:
              edge[anz->g].p1=  e_enqire[a[0]].nod[0];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[6];
              edge[anz->g].p1=  e_enqire[a[0]].nod[6];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[1];
	    break;
            case 3:
              edge[anz->g].p1=  e_enqire[a[0]].nod[1];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[7];
              edge[anz->g].p1=  e_enqire[a[0]].nod[7];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[2];
	    break;
            case 4:
              edge[anz->g].p1=  e_enqire[a[0]].nod[2];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[8];
              edge[anz->g].p1=  e_enqire[a[0]].nod[8];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[0];
	    break;
	  }
        break;
        case 1:
          switch(b[1])
          {
            case 2:
              edge[anz->g].p1=  e_enqire[a[0]].nod[3];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[12];
              edge[anz->g].p1=  e_enqire[a[0]].nod[12];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[4];
	    break;
            case 3:
              edge[anz->g].p1=  e_enqire[a[0]].nod[4];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[13];
              edge[anz->g].p1=  e_enqire[a[0]].nod[13];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[5];
	    break;
            case 4:
              edge[anz->g].p1=  e_enqire[a[0]].nod[3];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[14];
              edge[anz->g].p1=  e_enqire[a[0]].nod[14];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[5];
	    break;
	  }
        break;
        case 2:
          switch(b[1])
          {
            case 0:
              edge[anz->g].p1=  e_enqire[a[0]].nod[0];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[6];
              edge[anz->g].p1=  e_enqire[a[0]].nod[6];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[1];
	    break;
            case 1:
              edge[anz->g].p1=  e_enqire[a[0]].nod[3];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[12];
              edge[anz->g].p1=  e_enqire[a[0]].nod[12];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[4];
	    break;
            case 3:
              edge[anz->g].p1=  e_enqire[a[0]].nod[1];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[10];
              edge[anz->g].p1=  e_enqire[a[0]].nod[10];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[4];
	    break;
	  }
        break;
        case 3:
          switch(b[1])
          {
            case 0:
              edge[anz->g].p1=  e_enqire[a[0]].nod[1];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[7];
              edge[anz->g].p1=  e_enqire[a[0]].nod[7];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[2];
	    break;
            case 1:
              edge[anz->g].p1=  e_enqire[a[0]].nod[4];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[13];
              edge[anz->g].p1=  e_enqire[a[0]].nod[13];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[5];
	    break;
            case 2:
              edge[anz->g].p1=  e_enqire[a[0]].nod[1];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[10];
              edge[anz->g].p1=  e_enqire[a[0]].nod[10];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[4];
	    break;
	  }
        break;
        case 4:
          switch(b[1])
          {
            case 1:
              edge[anz->g].p1=  e_enqire[a[0]].nod[3];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[14];
              edge[anz->g].p1=  e_enqire[a[0]].nod[14];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[5];
	    break;
            case 2:
              edge[anz->g].p1=  e_enqire[a[0]].nod[0];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[9];
              edge[anz->g].p1=  e_enqire[a[0]].nod[9];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[3];
	    break;
            case 3:
              edge[anz->g].p1=  e_enqire[a[0]].nod[2];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[11];
              edge[anz->g].p1=  e_enqire[a[0]].nod[11];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[5];
	    break;
	  }
        break;
      }
    break;
    case 6:
      if ( (edge = (Edges *)realloc( (Edges *)edge, (anz->g+2) * sizeof(Edges))) == NULL )
        printf("\n\n ERROR: realloc failed edge\n\n") ;
      switch(a[1])
      {
        case 0:
          switch(b[1])
          {
            case 1:
              edge[anz->g].p1=  e_enqire[a[0]].nod[0];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[4];

              edge[anz->g].p1=  e_enqire[a[0]].nod[4];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[1];
	    break;
            case 2:
              edge[anz->g].p1=  e_enqire[a[0]].nod[1];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[5];

              edge[anz->g].p1=  e_enqire[a[0]].nod[5];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[2];
	    break;
            case 3:
              edge[anz->g].p1=  e_enqire[a[0]].nod[2];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[6];

              edge[anz->g].p1=  e_enqire[a[0]].nod[6];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[0];
	    break;
	  }
        break;
        case 1:
          switch(b[1])
          {
            case 0:
              edge[anz->g].p1=  e_enqire[a[0]].nod[0];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[4];

              edge[anz->g].p1=  e_enqire[a[0]].nod[4];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[1];
	    break;
            case 2:
              edge[anz->g].p1=  e_enqire[a[0]].nod[1];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[8];

              edge[anz->g].p1=  e_enqire[a[0]].nod[8];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[3];
	    break;
            case 3:
              edge[anz->g].p1=  e_enqire[a[0]].nod[3];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[7];

              edge[anz->g].p1=  e_enqire[a[0]].nod[7];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[0];
	    break;
	  }
        break;
        case 2:
          switch(b[1])
          {
            case 0:
              edge[anz->g].p1=  e_enqire[a[0]].nod[1];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[5];

              edge[anz->g].p1=  e_enqire[a[0]].nod[5];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[2];
	    break;
            case 1:
              edge[anz->g].p1=  e_enqire[a[0]].nod[1];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[8];

              edge[anz->g].p1=  e_enqire[a[0]].nod[8];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[3];
	    break;
            case 3:
              edge[anz->g].p1=  e_enqire[a[0]].nod[3];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[9];

              edge[anz->g].p1=  e_enqire[a[0]].nod[9];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[2];
	    break;
	  }
        break;
        case 3:
          switch(b[1])
          {
            case 0:
              edge[anz->g].p1=  e_enqire[a[0]].nod[0];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[6];

              edge[anz->g].p1=  e_enqire[a[0]].nod[6];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[2];
	    break;
            case 1:
              edge[anz->g].p1=  e_enqire[a[0]].nod[0];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[7];

              edge[anz->g].p1=  e_enqire[a[0]].nod[7];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[3];
	    break;
            case 2:
              edge[anz->g].p1=  e_enqire[a[0]].nod[3];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[9];

              edge[anz->g].p1=  e_enqire[a[0]].nod[9];
              edge[anz->g++].p2=  e_enqire[a[0]].nod[2];
	    break;
	  }
        break;
      }
    break;
  }
  *edgePtr=edge;

  return(0);
}




/* innerFacesMode=1; inner faces are extracted, else outer faces */

int selectDisplayFaces (Summen *anz, Elements *e_enqire, Faces **facePtr, Edges **edgePtr) 
{
#define MAX_NODE_NR  2100000000

  register int i,j,e;
  int numFaces=0, numDisplayFaces=0, nfaces=0,nface;
  int flag=0;
  long long size ;
  int *pfaces, *pCurrentFace, *pFaceN, *pFaceN1, *pmax, *pmaxd ;
  clock_t t1, t2 ;
  double deltaT ;
  Faces     *face=NULL;
  Edges     *edge=NULL;
#if TEST
  t1=clock() ;
#endif

  //printf(" in selectDisplayFaces: %d\n", anz->e);

  if ( (face = (Faces *)realloc( (Faces *)face, (anz->emax+1) * sizeof(Faces))) == NULL )
    printf("\n\n ERROR: realloc failed in selectDisplayFaces\n\n") ;
  for(i=0;i<= anz->emax; i++)  for(j=0; j<6; j++)   face[i].indx[j]   = -1;

  size = (6*anz->e *6+6 ) * sizeof(int) ;
  if ( size < 1920 ) size = 1920;
  if ( (pfaces = (int *)malloc( size )) == NULL )
  { printf("\n\n ERROR: malloc pfaces\n\n") ; exit(-1); }

  pCurrentFace = pfaces ;

  for ( e = 0; e < anz->e; e++ )
  {
    switch (e_enqire[e_enqire[e].nr].type )
    {
      case 1:
        nfaces=6;
      break;
      case 2:
        nfaces=5;
      break;
      case 3:
        nfaces=4;
      break;
      case 4:
        e_enqire[e_enqire[e].nr].type=1;
        flag=4;
        nfaces=6;  
      break;
      case 5:
        e_enqire[e_enqire[e].nr].type=2;
        flag=5;
        nfaces=5;  
      break;
      case 6:
        e_enqire[e_enqire[e].nr].type=3;
        flag=6;
        nfaces=4;  
      break;
      case 7:
        nfaces=5;  
      break;
      case 8:
        e_enqire[e_enqire[e].nr].type=7;
        flag=8;
        nfaces=5;  
      break;
      case 9:
        nfaces=6;  
      break;
      case 10:
        e_enqire[e_enqire[e].nr].type=9;
        flag=10;
        nfaces=6;  
      break;
      case 11:
        if(anz->f>anz->emax)
          if ( (face = (Faces *)realloc( (Faces *)face, (anz->f+1) * sizeof(Faces))) == NULL )
            printf("\n\n ERROR: realloc failed for faces\n\n") ;
        face[anz->f].type   = 11;
        face[anz->f].nod[0]=e_enqire[e_enqire[e].nr].nod[0];
        face[anz->f].nod[1]=e_enqire[e_enqire[e].nr].nod[1];
        face[anz->f].elem_nr= e_enqire[e].nr;
        face[anz->f].group  = e_enqire[e_enqire[e].nr].group;
        face[anz->f].mat    = 0;
        face[anz->f].nr     = 0;
        face[anz->f].side   = NULL;
        face[face[anz->f].elem_nr].indx[face[anz->f].nr]   = anz->f;
        anz->f++;
/*
        if ( (edge = (Edges *)realloc( (Edges *)edge, (anz->g+1) * sizeof(Edges))) == NULL )
          printf("\n\n ERROR: realloc failed for edge\n\n") ;
        edge[anz->g].p1=  face[anz->f].nod[0];
        edge[anz->g++].p2=face[anz->f++].nod[1];
*/
        nfaces=0;  
      break;
      case 12:
        if(anz->f>anz->emax)
          if ( (face = (Faces *)realloc( (Faces *)face, (anz->f+1) * sizeof(Faces))) == NULL )
            printf("\n\n ERROR: realloc failed for faces\n\n") ;
        face[anz->f].type   = 12;
        face[anz->f].nod[0]=e_enqire[e_enqire[e].nr].nod[0];
        face[anz->f].nod[1]=e_enqire[e_enqire[e].nr].nod[1];
        face[anz->f].nod[2]=e_enqire[e_enqire[e].nr].nod[2];
        face[anz->f].elem_nr= e_enqire[e].nr;
        face[anz->f].group  = e_enqire[e_enqire[e].nr].group;
        face[anz->f].mat    = 0;
        face[anz->f].nr     = 0;
        face[anz->f].side   = NULL;
        face[face[anz->f].elem_nr].indx[face[anz->f].nr]   = anz->f;
        anz->f++;
/*
        if ( (edge = (Edges *)realloc( (Edges *)edge, (anz->g+2) * sizeof(Edges))) == NULL )
          printf("\n\n ERROR: realloc failed for edge\n\n") ;
        edge[anz->g].p1=  face[anz->f].nod[0];
        edge[anz->g++].p2=face[anz->f].nod[2];
        edge[anz->g].p1=  face[anz->f].nod[2];
        edge[anz->g++].p2=face[anz->f++].nod[1];
*/
        nfaces=0;  
      break;
    }

    for(i=0; i<nfaces; i++)
    {
      if( (nface=getElemFaceNodes(e_enqire, e_enqire[e].nr, i, pCurrentFace)) <4) 
      {
        if(!nface) continue;
        if(nface==2) *(pCurrentFace+2) = MAX_NODE_NR-1 ;
         *(pCurrentFace+3) = MAX_NODE_NR ;
      }
      //if( e_enqire[e].nr==17575) printf("f:%d n:%d nodes: %d %d %d %d  elem: %d face: %d\n", i, nface, *(pCurrentFace),*(pCurrentFace+1),*(pCurrentFace+2),*(pCurrentFace+3),e_enqire[e].nr,i );
      vdsort( pCurrentFace ) ;
      pCurrentFace += 4 ;
      *(pCurrentFace) = e_enqire[e].nr ;
      *(++pCurrentFace) = i ;
      pCurrentFace++;
    }
    if(flag) { e_enqire[e_enqire[e].nr].type=flag; flag=0; }
  }
  numFaces=(int)(pCurrentFace-pfaces)/6 ;
  pmax = pCurrentFace ;

  /* for later comparison fill these adresses with 0 to avoid uninizialized values */
  *(pCurrentFace) = 0 ;
  *(++pCurrentFace) = 0 ;
  *(++pCurrentFace) = 0 ;
  *(++pCurrentFace) = 0 ;

#if TEST
  t2=clock() ;
  deltaT = ((double)t2 - (double)t1)/(double)CLOCKS_PER_SEC ;
  printf("\n selectDisplayFaces: %d faces extracted in %.2f seconds\n", numFaces, deltaT) ;
#endif

  /* nach Knotennummern sortieren */
  qsort (pfaces, numFaces, 6*sizeof(int), (void *)compareFaces4) ;

  pCurrentFace = pfaces ;
  pFaceN = pfaces ;
  pFaceN1 = pfaces + 6 ;

#if TEST
  t2=clock() ;
  deltaT = ((double)t2 - (double)t1)/(double)CLOCKS_PER_SEC ;
  printf("\n selectDisplayFaces: %d faces sorted in %.2f seconds\n", numFaces, deltaT) ;
#endif

  /* < pmax, damit die letzte Flaeche nicht verloren geht */
  while ( pFaceN < pmax ) {  /* pFaceN1 wird am Ende zurueckgesetzt */

    if ( compareFaces4 (pFaceN, pFaceN1) == 0 ) {   /* identische nodes: 2 flaechen ueberspringen */
      pFaceN += 12 ;
      pFaceN1 += 12 ;
    }
    else if ((*pFaceN==*(pFaceN+1))&&(*(pFaceN+2)==*(pFaceN+3)) ) {   /* 2* 2 identische nodes(kollabierte seite): flaeche ueberspringen */
      pFaceN += 6 ;
      pFaceN1 += 6 ; }
    else {

      /* elem nr */
      *(pCurrentFace) = *(pFaceN+4);
      /* face nr */
      *(pCurrentFace+1) = *(pFaceN+5) ;

      //printf("elem %d face %d type %d\n", *(pCurrentFace),*(pCurrentFace+1), e_enqire[*(pFaceN+4)].type);

      /* generate the face */
      if(anz->f>anz->emax)
        if ( (face = (Faces *)realloc( (Faces *)face, (anz->f+1) * sizeof(Faces))) == NULL )
          printf("\n\n ERROR: realloc failed for faces\n\n") ;
      nface=getElemFaceNodes(e_enqire, *(pFaceN+4), *(pFaceN+5), face[anz->f].nod);
      //printf("nface:%d\n",nface);

      switch(nface)
      {
      case 2:
        face[anz->f].type   = 11;
        if ( (edge = (Edges *)realloc( (Edges *)edge, (anz->g+1) * sizeof(Edges))) == NULL )
          printf("\n\n ERROR: realloc failed edge\n\n") ;
        edge[anz->g].p1=  face[anz->f].nod[0];
        edge[anz->g++].p2=face[anz->f].nod[1];
      break;
      case 3:
        if(e_enqire[*(pFaceN+4)].type> 7)
        {
          face[anz->f].type   = 12;
          if ( (edge = (Edges *)realloc( (Edges *)edge, (anz->g+2) * sizeof(Edges))) == NULL )
            printf("\n\n ERROR: realloc failed edge\n\n") ;
          edge[anz->g].p1=  face[anz->f].nod[0];
          edge[anz->g++].p2=face[anz->f].nod[2];
          edge[anz->g].p1=  face[anz->f].nod[2];
          edge[anz->g++].p2=face[anz->f].nod[1];
	}
        else face[anz->f].type   = 7;
      break;
      case 4:
        face[anz->f].type   = 9;
      break;
      case 6:
        face[anz->f].type   = 8;
      break;
      case 8:
        face[anz->f].type   = 10;
      break;
      }
      face[anz->f].elem_nr= *(pFaceN+4);
      face[anz->f].group  = e_enqire[*(pFaceN+4)].group;
      face[anz->f].mat    = 0;
      face[anz->f].nr     = *(pFaceN+5);
      face[anz->f].side   = NULL;
      face[face[anz->f].elem_nr].indx[face[anz->f].nr]   = anz->f;
      anz->f++;
      
      pFaceN += 6 ;
      pFaceN1 += 6 ;
      pCurrentFace += 2 ;
      numDisplayFaces += 1 ;
    }
    if ( pFaceN1 > pmax )  pFaceN1 -= 12 ;  /* muss <= pmax sein. Vergleich
       ist dann falsch, da gleiche Flaechen nur doppelt auftreten koennen */
  }
  pmaxd = pCurrentFace -2 ; 

#if TEST
  t2=clock() ;
  deltaT = ((double)t2 - (double)t1)/(double)CLOCKS_PER_SEC ;
  printf("\n selectDisplayFaces: %d faces detected in %.2f seconds\n", anz->f, deltaT) ;
#endif

  /* Suche freie Kanten des Modells */
  /* wenn 2 aufeinander folgende Flaechen zu einem Element gehoeren, ist eine Kante gefunden */

  /* nach Elementnummern sortieren */
  qsort (pfaces, numDisplayFaces, 2*sizeof(int), (void *)compareElnum1) ;

  pFaceN = pfaces ;
  pFaceN1 = pfaces + 2 ;

  while ( pFaceN < pmaxd )    /* bis zur vorletzten Flaeche */
  {
    if ( *(pFaceN) == *(pFaceN1) )  /* sind beide Flaechen vom gleichen Element? */
    {
      commonEdge2 (anz, &edge, e_enqire, pFaceN, pFaceN1);
      pFaceN1 += 2 ;                            /* alle Flaechen eines Elements abfragen  */
      if ( pFaceN1 > pmaxd ) /* Ueberlauf abfangen */
      {
        pFaceN += 2 ;
        pFaceN1 = pFaceN + 2 ;
      }
    }
    else
    {
      pFaceN += 2 ;
      pFaceN1 = pFaceN + 2 ;
    }
  }
  /*
  for(i=0; i<anz->g; i++)
  {
    printf("edge:%d n: %d %d\n", i, edge[i].p1, edge[i].p2);
  }
  printf("edges:%d faces:%d\n", anz->g, anz->f);
  */

  free(pfaces);

#if TEST
  t2=clock() ;
  deltaT = ((double)t2 - (double)t1)/(double)CLOCKS_PER_SEC ;
  printf("\n selectDisplayFaces: %d faces and %d free edges detected in %.2f seconds\n", anz->f, anz->g, deltaT) ;
#endif


  *facePtr=face;
  *edgePtr=edge;

  return(0) ;
}
