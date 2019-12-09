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

#include <cgx.h>

/*
Creates "ctri3" based on a set of elements eset. The element-faces to be used are identified by a set of nodes setNr.
Each tri3 knows the element and face it belongs to.

        ctri3[sum_tri3].elem_nr=  e, real elemnr
        ctri3[sum_tri3].group  =  elem-face-nr
        ctri3[sum_tri3].mat    =  elem-mat (unused)
        ctri3[sum_tri3].nod[0] = elem[e].nod[];
        ctri3[sum_tri3].nod[1] = elem[e].nod[];
        ctri3[sum_tri3].nod[2] = elem[e].nod[];

Creates "tri3_index[nodenr][i]" which holds the ctri3-indexes who use nodenr in common (-1<i<ntri_nodes[nodenr]).

Creates "ntri_nodes[nodenr]" which stores the amound of ctri3 who use nodenr.

returns -1 or "sum_tri3"
*/




int makeTriFromElems(int setNr, int eset, int nmax, Sets *set, Elements *elem,  CTri3 **pctri3, int ***ptri3_index, int **pntri_nodes)
{
  int i, ie, e, j, n, s;
  int       *ec;                  /* */
  int ec_shell[8];
  int       eci[13][6][8];             
  int sum_tri3;

  CTri3 *ctri3;
  int   **tri3_index;
  int   *ntri_nodes;
  
  ctri3=*pctri3;
  tri3_index=*ptri3_index;
  ntri_nodes=*pntri_nodes;

  /* pre-define the connectivity of all element-faces */
   eci[1][0][0]=3; eci[1][0][1]=2; eci[1][0][2]=1; eci[1][0][3]=0;
   eci[1][1][0]=0; eci[1][1][1]=1; eci[1][1][2]=5; eci[1][1][3]=4;
   eci[1][2][0]=1; eci[1][2][1]=2; eci[1][2][2]=6; eci[1][2][3]=5;
   eci[1][3][0]=2; eci[1][3][1]=3; eci[1][3][2]=7; eci[1][3][3]=6;
   eci[1][4][0]=3; eci[1][4][1]=0; eci[1][4][2]=4; eci[1][4][3]=7;
   eci[1][5][0]=4; eci[1][5][1]=5; eci[1][5][2]=6; eci[1][5][3]=7;

   eci[2][0][0]=0; eci[2][0][1]=2; eci[2][0][2]=1;
   eci[2][1][0]=3; eci[2][1][1]=4; eci[2][1][2]=5;
   eci[2][2][0]=0; eci[2][2][1]=1; eci[2][2][2]=4; eci[2][2][3]=3;
   eci[2][3][0]=1; eci[2][3][1]=2; eci[2][3][2]=5; eci[2][3][3]=4;
   eci[2][4][0]=2; eci[2][4][1]=0; eci[2][4][2]=3; eci[2][4][3]=5;

   eci[3][0][0]=0; eci[3][0][1]=2; eci[3][0][2]=1;
   eci[3][1][0]=0; eci[3][1][1]=1; eci[3][1][2]=3;
   eci[3][2][0]=1; eci[3][2][1]=2; eci[3][2][2]=3;
   eci[3][3][0]=2; eci[3][3][1]=0; eci[3][3][2]=3;

   eci[4][0][0]= 0; eci[4][0][1]=11; eci[4][0][2]= 3; eci[4][0][3]=10; eci[4][0][4]= 2; eci[4][0][5]= 9; eci[4][0][6]= 1; eci[4][0][7]= 8;
   eci[4][1][0]= 0; eci[4][1][1]= 8; eci[4][1][2]= 1; eci[4][1][3]=13; eci[4][1][4]= 5; eci[4][1][5]=16; eci[4][1][6]= 4; eci[4][1][7]=12;
   eci[4][2][0]= 1; eci[4][2][1]= 9; eci[4][2][2]= 2; eci[4][2][3]=14; eci[4][2][4]= 6; eci[4][2][5]=17; eci[4][2][6]= 5; eci[4][2][7]=13;
   eci[4][3][0]= 2; eci[4][3][1]=10; eci[4][3][2]= 3; eci[4][3][3]=15; eci[4][3][4]= 7; eci[4][3][5]=18; eci[4][3][6]= 6; eci[4][3][7]=14;
   eci[4][4][0]= 3; eci[4][4][1]=11; eci[4][4][2]= 0; eci[4][4][3]=12; eci[4][4][4]= 4; eci[4][4][5]=19; eci[4][4][6]= 7; eci[4][4][7]=15;
   eci[4][5][0]= 4; eci[4][5][1]=16; eci[4][5][2]= 5; eci[4][5][3]=17; eci[4][5][4]= 6; eci[4][5][5]=18; eci[4][5][6]= 7; eci[4][5][7]=19;

   eci[6][0][0]=0; eci[6][0][1]=4; eci[6][0][2]=1; eci[6][0][3]=8; eci[6][0][4]=3; eci[6][0][5]=7;
   eci[6][1][0]=1; eci[6][1][1]=5; eci[6][1][2]=2; eci[6][1][3]=9; eci[6][1][4]=3; eci[6][1][5]=8;
   eci[6][2][0]=2; eci[6][2][1]=6; eci[6][2][2]=0; eci[6][2][3]=7; eci[6][2][4]=3; eci[6][2][5]=9;
   eci[6][3][0]=0; eci[6][3][1]=6; eci[6][3][2]=2; eci[6][3][3]=5; eci[6][3][4]=1; eci[6][3][5]=4;

  ec=&eci[0][0][0];

  if((tri3_index =(int **)realloc((int **)tri3_index, (nmax+1) *sizeof(int *)))==NULL)
  {
    errMsg("\n\n ERROR: realloc failed for tri3_index\n" );
    return(-1);
  }
  if((ntri_nodes =(int *)malloc((nmax+2) *sizeof(int)))==NULL)
  {
    errMsg("\n\n ERROR: malloc failed for ntri_nodes\n" );
    return(-1);
  }
  if ( (ctri3 = (CTri3 *)malloc( 1 * sizeof(CTri3))) == NULL )
  {
    errMsg("\nERROR: realloc failed in makeTriFromElems() \n\n");
    return(-1);
  }

  for (i=0; i<=nmax; i++)
  {
    tri3_index[i]=NULL;
    ntri_nodes[i]=0;
  }
  sum_tri3=0;

  for (ie=0; ie<set[eset].anz_e; ie++)
  {
    e=set[eset].elem[ie];
    j=0;

    switch(elem[e].type)
    {

    /* ----- HE8 --------------- */
    case 1:
    {
      /* go over all element faces and look if all nodes are also in the node-set */
      for (s=0; s<6; s++)
      {
        ec=&eci[1][s][0];
        j=0;
        for (n=0; n<4; n++)
        {
          if( ifind(&set[setNr].node, set[setNr].anz_n, elem[e].nod[ec[n]])>-1) j++; 
        }
        if(j==4)
	  {
          /* merke dir alle am node haengenden flaechen (flaeche: sum_tri3, sum_tri3+1)*/
          for (i=0; i<2; i++)
          {
            for (n=0; n<4; n++)
            {
              if((tri3_index[ elem[e].nod[ec[n]] ] 
              = (int *)realloc((int *)tri3_index[ elem[e].nod[ec[n]] ]
              , (ntri_nodes[elem[e].nod[ec[n]]]+1) *sizeof(int)))==NULL)
    	      {
                errMsg("\n\n ERROR: realloc failed for tri3_index\n" );
                return(-1);
              }
    
              tri3_index[ elem[e].nod[ec[n]] ][ ntri_nodes[elem[e].nod[ec[n]]] ]=sum_tri3+i;
              ntri_nodes[ elem[e].nod[ec[n]] ]++;
            }
          }
          if ( (ctri3 = (CTri3 *)realloc( (CTri3 *)ctri3, (sum_tri3+2) * sizeof(CTri3))) == NULL )
          {
            errMsg("\nERROR: realloc failed in makeTriFromElems() \n\n");
            return(-1);
          }

          ctri3[sum_tri3].elem_nr= e;
          ctri3[sum_tri3].group  = 1+s;
          ctri3[sum_tri3].mat    = elem[e].mat;
          ctri3[sum_tri3].nod[0]=elem[e].nod[ec[0]];
          ctri3[sum_tri3].nod[1]=elem[e].nod[ec[1]];
          ctri3[sum_tri3].nod[2]=elem[e].nod[ec[2]];
          sum_tri3++;

          ctri3[sum_tri3].elem_nr= e;
          ctri3[sum_tri3].group  = 1+s;
          ctri3[sum_tri3].mat    = elem[e].mat;
          ctri3[sum_tri3].nod[0]=elem[e].nod[ec[0]];
          ctri3[sum_tri3].nod[1]=elem[e].nod[ec[2]];
          ctri3[sum_tri3].nod[2]=elem[e].nod[ec[3]];
          sum_tri3++;
        }
      }
    }
    break;

    /* ----- Pe6 --------------- */
    case 2:
    {
      /* go over all element faces and look if all nodes are also in the node-set */
      for (s=0; s<2; s++)
      {
        ec=&eci[2][s][0];

        j=0;
        for (n=0; n<3; n++)
        {
          if( ifind(&set[setNr].node, set[setNr].anz_n, elem[e].nod[ec[n]])>-1) j++; 
        }
        if(j==3)
        {
          /* merke dir alle am node haengenden flaechen (flaeche: sum_tri3, sum_tri3+1)*/
          for (i=0; i<1; i++)
          {
            for (n=0; n<3; n++)
            {
              if((tri3_index[ elem[e].nod[ec[n]] ] 
              = (int *)realloc((int *)tri3_index[ elem[e].nod[ec[n]] ]
              , (ntri_nodes[elem[e].nod[ec[n]]]+1) *sizeof(int)))==NULL)
    	      {
                errMsg("\n\n ERROR: realloc failed for tri3_index\n" );
                return(-1);
              }
    
              tri3_index[ elem[e].nod[ec[n]] ][ ntri_nodes[elem[e].nod[ec[n]]] ]=sum_tri3+i;
              ntri_nodes[ elem[e].nod[ec[n]] ]++;
            }
          }
          if ( (ctri3 = (CTri3 *)realloc( (CTri3 *)ctri3, (sum_tri3+1) * sizeof(CTri3))) == NULL )
          {
            errMsg("\nERROR: realloc failed in makeTriFromElems() \n\n");
            return(-1);
          }

          ctri3[sum_tri3].elem_nr= e;
          ctri3[sum_tri3].group  = 1+s;
          ctri3[sum_tri3].mat    = elem[e].mat;
          ctri3[sum_tri3].nod[0]=elem[e].nod[ec[0]];
          ctri3[sum_tri3].nod[1]=elem[e].nod[ec[1]];
          ctri3[sum_tri3].nod[2]=elem[e].nod[ec[2]];
          sum_tri3++;
        }
      }

      for (s=2; s<5; s++)
      {
        ec=&eci[2][s][0];
        j=0;
        for (n=0; n<4; n++)
        {
          if( ifind(&set[setNr].node, set[setNr].anz_n, elem[e].nod[ec[n]])>-1) j++; 
        }
        if(j==4)
	  {
          /* merke dir alle am node haengenden flaechen (flaeche: sum_tri3, sum_tri3+1)*/
          for (i=0; i<2; i++)
          {
            for (n=0; n<4; n++)
            {
              if((tri3_index[ elem[e].nod[ec[n]] ] 
              = (int *)realloc((int *)tri3_index[ elem[e].nod[ec[n]] ]
              , (ntri_nodes[elem[e].nod[ec[n]]]+1) *sizeof(int)))==NULL)
    	      {
                errMsg("\n\n ERROR: realloc failed for tri3_index\n" );
                return(-1);
              }
    
              tri3_index[ elem[e].nod[ec[n]] ][ ntri_nodes[elem[e].nod[ec[n]]] ]=sum_tri3+i;
              ntri_nodes[ elem[e].nod[ec[n]] ]++;
            }
          }
          if ( (ctri3 = (CTri3 *)realloc( (CTri3 *)ctri3, (sum_tri3+2) * sizeof(CTri3))) == NULL )
          {
            errMsg("\nERROR: realloc failed in makeTriFromElems() \n\n");
            return(-1);
          }

          ctri3[sum_tri3].elem_nr= e;
          ctri3[sum_tri3].group  = 1+s;
          ctri3[sum_tri3].mat    = elem[e].mat;
          ctri3[sum_tri3].nod[0]=elem[e].nod[ec[0]];
          ctri3[sum_tri3].nod[1]=elem[e].nod[ec[1]];
          ctri3[sum_tri3].nod[2]=elem[e].nod[ec[2]];
          sum_tri3++;

          ctri3[sum_tri3].elem_nr= e;
          ctri3[sum_tri3].group  = 1+s;
          ctri3[sum_tri3].mat    = elem[e].mat;
          ctri3[sum_tri3].nod[0]=elem[e].nod[ec[0]];
          ctri3[sum_tri3].nod[1]=elem[e].nod[ec[2]];
          ctri3[sum_tri3].nod[2]=elem[e].nod[ec[3]];
          sum_tri3++;
        }
      }
    }
    break;

    /* ----- Te4 --------------- */
    case 3:
    {
      /* go over all element faces and look if all nodes are also in the node-set */
      for (s=0; s<4; s++)
      {
        ec=&eci[3][s][0];

        j=0;
        for (n=0; n<3; n++)
        {
          if( ifind(&set[setNr].node, set[setNr].anz_n, elem[e].nod[ec[n]])>-1) j++; 
        }
        if(j==3)
        {
          /* merke dir alle am node haengenden flaechen (flaeche: sum_tri3, sum_tri3+1)*/
          for (i=0; i<1; i++)
          {
            for (n=0; n<3; n++)
            {
              if((tri3_index[ elem[e].nod[ec[n]] ] 
              = (int *)realloc((int *)tri3_index[ elem[e].nod[ec[n]] ]
              , (ntri_nodes[elem[e].nod[ec[n]]]+1) *sizeof(int)))==NULL)
    	      {
                errMsg("\n\n ERROR: realloc failed for tri3_index\n" );
                return(-1);
              }
    
              tri3_index[ elem[e].nod[ec[n]] ][ ntri_nodes[elem[e].nod[ec[n]]] ]=sum_tri3+i;
              ntri_nodes[ elem[e].nod[ec[n]] ]++;
            }
          }
          if ( (ctri3 = (CTri3 *)realloc( (CTri3 *)ctri3, (sum_tri3+1) * sizeof(CTri3))) == NULL )
          {
            errMsg("\nERROR: realloc failed in makeTriFromElems() \n\n");
            return(-1);
          }

          ctri3[sum_tri3].elem_nr= e;
          ctri3[sum_tri3].group  = 1+s;
          ctri3[sum_tri3].mat    = elem[e].mat;
          ctri3[sum_tri3].nod[0]=elem[e].nod[ec[0]];
          ctri3[sum_tri3].nod[1]=elem[e].nod[ec[1]];
          ctri3[sum_tri3].nod[2]=elem[e].nod[ec[2]];
          sum_tri3++;
        }
      }
    }
    break;

    /* ----- HE20 --------------- */
    case 4:
    {
      /* go over all element faces and look if all nodes are also in the node-set */
      for (s=0; s<6; s++)
      {
        ec=&eci[4][s][0];

        j=0;
        for (n=0; n<8; n++)
        {
          if( ifind(&set[setNr].node, set[setNr].anz_n, elem[e].nod[ec[n]])>-1) j++; 
        }
        if(j==8)
        {
          /* merke dir alle am node haengenden flaechen (flaeche: sum_tri3, sum_tri3+1)*/
          for (i=0; i<6; i++)
          {
            for (n=0; n<8; n++)
            {
              if((tri3_index[ elem[e].nod[ec[n]] ] 
              = (int *)realloc((int *)tri3_index[ elem[e].nod[ec[n]] ]
              , (ntri_nodes[elem[e].nod[ec[n]]]+1) *sizeof(int)))==NULL)
    	      {
                errMsg("\n\n ERROR: realloc failed for tri3_index\n" );
                return(-1);
              }
    
              tri3_index[ elem[e].nod[ec[n]] ][ ntri_nodes[elem[e].nod[ec[n]]] ]=sum_tri3+i;
              ntri_nodes[ elem[e].nod[ec[n]] ]++;
            }
          }
          if ( (ctri3 = (CTri3 *)realloc( (CTri3 *)ctri3, (sum_tri3+6) * sizeof(CTri3))) == NULL )
          {
            errMsg("\nERROR: realloc failed in makeTriFromElems() \n\n");
            return(-1);
          }

          ctri3[sum_tri3].elem_nr= e;
          ctri3[sum_tri3].group  = 1+s;
          ctri3[sum_tri3].mat    = elem[e].mat;
          ctri3[sum_tri3].nod[0]=elem[e].nod[ec[0]];
          ctri3[sum_tri3].nod[1]=elem[e].nod[ec[1]];
          ctri3[sum_tri3].nod[2]=elem[e].nod[ec[7]];
          sum_tri3++;

          ctri3[sum_tri3].elem_nr= e;
          ctri3[sum_tri3].group  = 1+s;
          ctri3[sum_tri3].mat    = elem[e].mat;
          ctri3[sum_tri3].nod[0]=elem[e].nod[ec[7]];
          ctri3[sum_tri3].nod[1]=elem[e].nod[ec[1]];
          ctri3[sum_tri3].nod[2]=elem[e].nod[ec[5]];
          sum_tri3++;

          ctri3[sum_tri3].elem_nr= e;
          ctri3[sum_tri3].group  = 1+s;
          ctri3[sum_tri3].mat    = elem[e].mat;
          ctri3[sum_tri3].nod[0]=elem[e].nod[ec[5]];
          ctri3[sum_tri3].nod[1]=elem[e].nod[ec[6]];
          ctri3[sum_tri3].nod[2]=elem[e].nod[ec[7]];
          sum_tri3++;

          ctri3[sum_tri3].elem_nr= e;
          ctri3[sum_tri3].group  = 1+s;
          ctri3[sum_tri3].mat    = elem[e].mat;
          ctri3[sum_tri3].nod[0]=elem[e].nod[ec[4]];
          ctri3[sum_tri3].nod[1]=elem[e].nod[ec[5]];
          ctri3[sum_tri3].nod[2]=elem[e].nod[ec[3]];
          sum_tri3++;

          ctri3[sum_tri3].elem_nr= e;
          ctri3[sum_tri3].group  = 1+s;
          ctri3[sum_tri3].mat    = elem[e].mat;
          ctri3[sum_tri3].nod[0]=elem[e].nod[ec[3]];
          ctri3[sum_tri3].nod[1]=elem[e].nod[ec[5]];
          ctri3[sum_tri3].nod[2]=elem[e].nod[ec[1]];
          sum_tri3++;

          ctri3[sum_tri3].elem_nr= e;
          ctri3[sum_tri3].group  = 1+s;
          ctri3[sum_tri3].mat    = elem[e].mat;
          ctri3[sum_tri3].nod[0]=elem[e].nod[ec[1]];
          ctri3[sum_tri3].nod[1]=elem[e].nod[ec[2]];
          ctri3[sum_tri3].nod[2]=elem[e].nod[ec[3]];
          sum_tri3++;
        }
      }
    }
    break;

    /* ----- Pe6 --------------- */
    case 5:
    {
      /* go over all element faces and look if all nodes are also in the node-set */
      for (s=0; s<2; s++)
      {
        ec=&eci[2][s][0];

        j=0;
        for (n=0; n<3; n++)
        {
          if( ifind(&set[setNr].node, set[setNr].anz_n, elem[e].nod[ec[n]])>-1) j++; 
        }
        if(j==3)
        {
          /* merke dir alle am node haengenden flaechen (flaeche: sum_tri3, sum_tri3+1)*/
          for (i=0; i<1; i++)
          {
            for (n=0; n<3; n++)
            {
              if((tri3_index[ elem[e].nod[ec[n]] ] 
              = (int *)realloc((int *)tri3_index[ elem[e].nod[ec[n]] ]
              , (ntri_nodes[elem[e].nod[ec[n]]]+1) *sizeof(int)))==NULL)
    	      {
                errMsg("\n\n ERROR: realloc failed for tri3_index\n" );
                return(-1);
              }
    
              tri3_index[ elem[e].nod[ec[n]] ][ ntri_nodes[elem[e].nod[ec[n]]] ]=sum_tri3+i;
              ntri_nodes[ elem[e].nod[ec[n]] ]++;
            }
          }
          if ( (ctri3 = (CTri3 *)realloc( (CTri3 *)ctri3, (sum_tri3+1) * sizeof(CTri3))) == NULL )
          {
            errMsg("\nERROR: realloc failed in makeTriFromElems() \n\n");
            return(-1);
          }

          ctri3[sum_tri3].elem_nr= e;
          ctri3[sum_tri3].group  = 1+s;
          ctri3[sum_tri3].mat    = elem[e].mat;
          ctri3[sum_tri3].nod[0]=elem[e].nod[ec[0]];
          ctri3[sum_tri3].nod[1]=elem[e].nod[ec[1]];
          ctri3[sum_tri3].nod[2]=elem[e].nod[ec[2]];
          sum_tri3++;
        }
      }

      for (s=2; s<5; s++)
      {
        ec=&eci[2][s][0];
        j=0;
        for (n=0; n<4; n++)
        {
          if( ifind(&set[setNr].node, set[setNr].anz_n, elem[e].nod[ec[n]])>-1) j++; 
        }
        if(j==4)
	  {
          /* merke dir alle am node haengenden flaechen (flaeche: sum_tri3, sum_tri3+1)*/
          for (i=0; i<2; i++)
          {
            for (n=0; n<4; n++)
            {
              if((tri3_index[ elem[e].nod[ec[n]] ] 
              = (int *)realloc((int *)tri3_index[ elem[e].nod[ec[n]] ]
              , (ntri_nodes[elem[e].nod[ec[n]]]+1) *sizeof(int)))==NULL)
    	      {
                errMsg("\n\n ERROR: realloc failed for tri3_index\n" );
                return(-1);
              }
    
              tri3_index[ elem[e].nod[ec[n]] ][ ntri_nodes[elem[e].nod[ec[n]]] ]=sum_tri3+i;
              ntri_nodes[ elem[e].nod[ec[n]] ]++;
            }
          }
          if ( (ctri3 = (CTri3 *)realloc( (CTri3 *)ctri3, (sum_tri3+2) * sizeof(CTri3))) == NULL )
          {
            errMsg("\nERROR: realloc failed in makeTriFromElems() \n\n");
            return(-1);
          }

          ctri3[sum_tri3].elem_nr= e;
          ctri3[sum_tri3].group  = 1+s;
          ctri3[sum_tri3].mat    = elem[e].mat;
          ctri3[sum_tri3].nod[0]=elem[e].nod[ec[0]];
          ctri3[sum_tri3].nod[1]=elem[e].nod[ec[1]];
          ctri3[sum_tri3].nod[2]=elem[e].nod[ec[2]];
          sum_tri3++;

          ctri3[sum_tri3].elem_nr= e;
          ctri3[sum_tri3].group  = 1+s;
          ctri3[sum_tri3].mat    = elem[e].mat;
          ctri3[sum_tri3].nod[0]=elem[e].nod[ec[0]];
          ctri3[sum_tri3].nod[1]=elem[e].nod[ec[2]];
          ctri3[sum_tri3].nod[2]=elem[e].nod[ec[3]];
          sum_tri3++;
        }
      }
    }
    break;

    /* ----- Te10 --------------- */
    case 6:
    {
      /* go over all element faces and look if all nodes are also in the node-set */
      for (s=0; s<4; s++)
      {
        ec=&eci[6][s][0];

        j=0;
        for (n=0; n<6; n++)
        {
          if( ifind(&set[setNr].node, set[setNr].anz_n, elem[e].nod[ec[n]])>-1) j++; 
        }
        if(j==6)
        {
          /* merke dir alle am node haengenden flaechen (flaeche: sum_tri3, sum_tri3+1)*/
          for (i=0; i<4; i++)
          {
            for (n=0; n<6; n++)
            {
              if((tri3_index[ elem[e].nod[ec[n]] ] 
              = (int *)realloc((int *)tri3_index[ elem[e].nod[ec[n]] ]
              , (ntri_nodes[elem[e].nod[ec[n]]]+1) *sizeof(int)))==NULL)
    	      {
                errMsg("\n\n ERROR: realloc failed for tri3_index\n" );
                return(-1);
              }
    
              tri3_index[ elem[e].nod[ec[n]] ][ ntri_nodes[elem[e].nod[ec[n]]] ]=sum_tri3+i;
              ntri_nodes[ elem[e].nod[ec[n]] ]++;
            }
          }
          if ( (ctri3 = (CTri3 *)realloc( (CTri3 *)ctri3, (sum_tri3+4) * sizeof(CTri3))) == NULL )
          {
            errMsg("\nERROR: realloc failed in makeTriFromElems() \n\n");
            return(-1);
          }

          ctri3[sum_tri3].elem_nr= e;
          ctri3[sum_tri3].group  = 1+s;
          ctri3[sum_tri3].mat    = elem[e].mat;
          ctri3[sum_tri3].nod[0]=elem[e].nod[ec[0]];
          ctri3[sum_tri3].nod[1]=elem[e].nod[ec[1]];
          ctri3[sum_tri3].nod[2]=elem[e].nod[ec[5]];
          sum_tri3++;

          ctri3[sum_tri3].elem_nr= e;
          ctri3[sum_tri3].group  = 1+s;
          ctri3[sum_tri3].mat    = elem[e].mat;
          ctri3[sum_tri3].nod[0]=elem[e].nod[ec[5]];
          ctri3[sum_tri3].nod[1]=elem[e].nod[ec[1]];
          ctri3[sum_tri3].nod[2]=elem[e].nod[ec[3]];
          sum_tri3++;

          ctri3[sum_tri3].elem_nr= e;
          ctri3[sum_tri3].group  = 1+s;
          ctri3[sum_tri3].mat    = elem[e].mat;
          ctri3[sum_tri3].nod[0]=elem[e].nod[ec[3]];
          ctri3[sum_tri3].nod[1]=elem[e].nod[ec[4]];
          ctri3[sum_tri3].nod[2]=elem[e].nod[ec[5]];
          sum_tri3++;

          ctri3[sum_tri3].elem_nr= e;
          ctri3[sum_tri3].group  = 1+s;
          ctri3[sum_tri3].mat    = elem[e].mat;
          ctri3[sum_tri3].nod[0]=elem[e].nod[ec[1]];
          ctri3[sum_tri3].nod[1]=elem[e].nod[ec[2]];
          ctri3[sum_tri3].nod[2]=elem[e].nod[ec[3]];
          sum_tri3++;
        }
      }
    }
    break;

    /* ----- Tri3 --------------- */
    case 7:
    {
      for (n=0; n<3; n++)
      {
        if( ifind(&set[setNr].node, set[setNr].anz_n, elem[e].nod[n])>-1)
        {
          ec_shell[j]=n;
          j++;
        }
      }
      if(j==3)
      {
        /* merke dir alle am node haengenden flaechen (flaeche: sum_tri3)*/
        for (i=0; i<3; i++)
        {
         if((tri3_index[ elem[e].nod[ec_shell[i]] ] 
          = (int *)realloc((int *)tri3_index[ elem[e].nod[ec_shell[i]] ]
          , (ntri_nodes[elem[e].nod[ec_shell[i]]]+1) *sizeof(int)))==NULL)
	  {
            errMsg("\n\n ERROR: realloc failed for tri3_index\n" );
            return(-1);
          }

          tri3_index[ elem[e].nod[ec_shell[i]] ][ ntri_nodes[elem[e].nod[ec_shell[i]]] ]=sum_tri3;
          ntri_nodes[ elem[e].nod[ec_shell[i]] ]++;
        }
        if ( (ctri3 = (CTri3 *)realloc( (CTri3 *)ctri3, (sum_tri3+2) * sizeof(CTri3))) == NULL )
        {
          errMsg("\nERROR: realloc failed in makeTriFromElems() \n\n");
          return(-1);
        }
        ctri3[sum_tri3].elem_nr= e;  /* elem-indx im hilfsfeld elem */
        ctri3[sum_tri3].group  = 1;
        ctri3[sum_tri3].mat    = elem[e].mat;
        ctri3[sum_tri3].nod[0]=elem[e].nod[0];
        ctri3[sum_tri3].nod[1]=elem[e].nod[1];
        ctri3[sum_tri3].nod[2]=elem[e].nod[2];
        sum_tri3++;
      }
    }
    break;

    /* ----- Tri6 --------------- */
    case 8:
    {
      for (n=0; n<6; n++)
      {
        if( ifind(&set[setNr].node, set[setNr].anz_n, elem[e].nod[n])>-1)
        {
          ec_shell[j]=n;
          j++;
        }
      }
      if(j==6)
      {
        /* merke dir alle am node haengenden flaechen (flaeche: sum_tri3, sum_tri3+1)*/
        for (n=0; n<4; n++)
        {
          for (i=0; i<6; i++)
          {
            if((tri3_index[ elem[e].nod[ec_shell[i]] ] 
            = (int *)realloc((int *)tri3_index[ elem[e].nod[ec_shell[i]] ]
            , (ntri_nodes[elem[e].nod[ec_shell[i]]]+1) *sizeof(int)))==NULL)
	    {
              errMsg("\n\n ERROR: realloc failed for tri3_index\n" );
              return(-1);
            }

            tri3_index[ elem[e].nod[ec_shell[i]] ][ ntri_nodes[elem[e].nod[ec_shell[i]]] ]=sum_tri3+n;
            ntri_nodes[ elem[e].nod[ec_shell[i]] ]++;
          }
        }
        sum_tri3+=4;
        if ( (ctri3 = (CTri3 *)realloc( (CTri3 *)ctri3, sum_tri3 * sizeof(CTri3))) == NULL )
        {
          errMsg("\nERROR: realloc failed in makeTriFromElems() \n\n");
          return(-1);
        }

        ctri3[sum_tri3-4].elem_nr= e;
        ctri3[sum_tri3-4].group  = 1;
        ctri3[sum_tri3-4].mat    = elem[e].mat;
        ctri3[sum_tri3-4].nod[0]=elem[e].nod[0];
        ctri3[sum_tri3-4].nod[1]=elem[e].nod[5];
        ctri3[sum_tri3-4].nod[2]=elem[e].nod[3];

        ctri3[sum_tri3-3].elem_nr= e;
        ctri3[sum_tri3-3].group  = 1;
        ctri3[sum_tri3-3].mat    = elem[e].mat;
        ctri3[sum_tri3-3].nod[0]=elem[e].nod[3];
        ctri3[sum_tri3-3].nod[1]=elem[e].nod[5];
        ctri3[sum_tri3-3].nod[2]=elem[e].nod[4];

        ctri3[sum_tri3-2].elem_nr= e;
        ctri3[sum_tri3-2].group  = 1;
        ctri3[sum_tri3-2].mat    = elem[e].mat;
        ctri3[sum_tri3-2].nod[0]=elem[e].nod[5];
        ctri3[sum_tri3-2].nod[1]=elem[e].nod[2];
        ctri3[sum_tri3-2].nod[2]=elem[e].nod[4];

        ctri3[sum_tri3-1].elem_nr= e;
        ctri3[sum_tri3-1].group  = 1;
        ctri3[sum_tri3-1].mat    = elem[e].mat;
        ctri3[sum_tri3-1].nod[0]=elem[e].nod[3];
        ctri3[sum_tri3-1].nod[1]=elem[e].nod[4];
        ctri3[sum_tri3-1].nod[2]=elem[e].nod[1];
      }
    }
    break;

    /* ----- Qu4 --------------- */
    case 9:
    {
      for (n=0; n<4; n++)
      {
        if( ifind(&set[setNr].node, set[setNr].anz_n, elem[e].nod[n])>-1)
        {
          ec_shell[j]=n;
          j++;
        }
      }
     if(j==4)
      {
        /* merke dir alle am node haengenden flaechen (flaeche: sum_tri3, sum_tri3+1)*/
        for (n=0; n<2; n++)
        {
          for (i=0; i<4; i++)
          {
            if((tri3_index[ elem[e].nod[ec_shell[i]] ] 
            = (int *)realloc((int *)tri3_index[ elem[e].nod[ec_shell[i]] ]
            , (ntri_nodes[elem[e].nod[ec_shell[i]]]+1) *sizeof(int)))==NULL)
	    {
              errMsg("\n\n ERROR: realloc failed for tri3_index\n" );
              return(-1);
            }

            tri3_index[ elem[e].nod[ec_shell[i]] ][ ntri_nodes[elem[e].nod[ec_shell[i]]] ]=sum_tri3+n;
            ntri_nodes[ elem[e].nod[ec_shell[i]] ]++;
          }
        }
        sum_tri3+=2;
        if ( (ctri3 = (CTri3 *)realloc( (CTri3 *)ctri3, sum_tri3 * sizeof(CTri3))) == NULL )
        {
          errMsg("\nERROR: realloc failed in makeTriFromElems() \n\n");
          return(-1);
        }

        ctri3[sum_tri3-2].elem_nr= e;
        ctri3[sum_tri3-2].group  = 1;
        ctri3[sum_tri3-2].mat    = elem[e].mat;
        ctri3[sum_tri3-2].nod[0]=elem[e].nod[0];
        ctri3[sum_tri3-2].nod[1]=elem[e].nod[1];
        ctri3[sum_tri3-2].nod[2]=elem[e].nod[2];

        ctri3[sum_tri3-1].elem_nr= e;
        ctri3[sum_tri3-1].group  = 1;
        ctri3[sum_tri3-1].mat    = elem[e].mat;
        ctri3[sum_tri3-1].nod[0]=elem[e].nod[0];
        ctri3[sum_tri3-1].nod[1]=elem[e].nod[2];
        ctri3[sum_tri3-1].nod[2]=elem[e].nod[3];
      }
    }
    break;

    /* ----- Qu8 --------------- */
    case 10:
    {
      for (n=0; n<8; n++)
      {
        if( ifind(&set[setNr].node, set[setNr].anz_n, elem[e].nod[n])>-1)
        {
          ec_shell[j]=n;
          j++;
        }
      }
      if(j==8)
      {
        /* merke dir alle am node haengenden flaechen (flaeche: sum_tri3, sum_tri3+1)*/
        for (n=0; n<6; n++)
        {
          for (i=0; i<8; i++)
          {
            if((tri3_index[ elem[e].nod[ec_shell[i]] ] 
            = (int *)realloc((int *)tri3_index[ elem[e].nod[ec_shell[i]] ]
            , (ntri_nodes[elem[e].nod[ec_shell[i]]]+1) *sizeof(int)))==NULL)
	    {
              errMsg("\n\n ERROR: realloc failed for tri3_index\n" );
              return(-1);
            }

            tri3_index[ elem[e].nod[ec_shell[i]] ][ ntri_nodes[elem[e].nod[ec_shell[i]]] ]=sum_tri3+n;
            ntri_nodes[ elem[e].nod[ec_shell[i]] ]++;
          }
        }
        sum_tri3+=6;
        if ( (ctri3 = (CTri3 *)realloc( (CTri3 *)ctri3, sum_tri3 * sizeof(CTri3))) == NULL )
        {
          errMsg("\nERROR: realloc failed in makeTriFromElems() \n\n");
          return(-1);
        }
 
        ctri3[sum_tri3-6].elem_nr= e;
        ctri3[sum_tri3-6].group  = 1;
        ctri3[sum_tri3-6].mat    = elem[e].mat;
        ctri3[sum_tri3-6].nod[0]=elem[e].nod[4];
        ctri3[sum_tri3-6].nod[1]=elem[e].nod[0];
        ctri3[sum_tri3-6].nod[2]=elem[e].nod[7];

        ctri3[sum_tri3-5].elem_nr= e;
        ctri3[sum_tri3-5].group  = 1;
        ctri3[sum_tri3-5].mat    = elem[e].mat;
        ctri3[sum_tri3-5].nod[0]=elem[e].nod[7];
        ctri3[sum_tri3-5].nod[1]=elem[e].nod[3];
        ctri3[sum_tri3-5].nod[2]=elem[e].nod[6];

        ctri3[sum_tri3-4].elem_nr= e;
        ctri3[sum_tri3-4].group  = 1;
        ctri3[sum_tri3-4].mat    = elem[e].mat;
        ctri3[sum_tri3-4].nod[0]=elem[e].nod[6];
        ctri3[sum_tri3-4].nod[1]=elem[e].nod[2];
        ctri3[sum_tri3-4].nod[2]=elem[e].nod[5];

        ctri3[sum_tri3-3].elem_nr= e;
        ctri3[sum_tri3-3].group  = 1;
        ctri3[sum_tri3-3].mat    = elem[e].mat;
        ctri3[sum_tri3-3].nod[0]=elem[e].nod[5];
        ctri3[sum_tri3-3].nod[1]=elem[e].nod[1];
        ctri3[sum_tri3-3].nod[2]=elem[e].nod[4];

        ctri3[sum_tri3-2].elem_nr= e;
        ctri3[sum_tri3-2].group  = 1;
        ctri3[sum_tri3-2].mat    = elem[e].mat;
        ctri3[sum_tri3-2].nod[0]=elem[e].nod[4];
        ctri3[sum_tri3-2].nod[1]=elem[e].nod[7];
        ctri3[sum_tri3-2].nod[2]=elem[e].nod[6];

        ctri3[sum_tri3-1].elem_nr= e;
        ctri3[sum_tri3-1].group  = 1;
        ctri3[sum_tri3-1].mat    = elem[e].mat;
        ctri3[sum_tri3-1].nod[0]=elem[e].nod[6];
        ctri3[sum_tri3-1].nod[1]=elem[e].nod[5];
        ctri3[sum_tri3-1].nod[2]=elem[e].nod[4];
      }
    }
    break;
  }
  }

  *pctri3=ctri3;
  *ptri3_index=tri3_index;
  *pntri_nodes=ntri_nodes;
  return(sum_tri3);
}
