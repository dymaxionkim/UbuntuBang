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

extern int     read_mode;

extern Summen    anz[1];
extern Nodes     *node;
extern Elements  *e_enqire;         /* elem-array by elem-number instead of elem[index]... */
extern Datasets  *lcase;
extern NodeBlocks *nBlock;

extern double     *vp;
extern Edges     *edge;             /* model-edges           */
extern Scale     scale[1];
extern Faces     *face;

extern Sets      *set;

extern int       elemMat[MAX_MATERIALS];      /*  Material Numbers, Number of Materials stored in elemMat[0] */

/* for the CFD reader (ISAAC) */
extern double pref, tref, R_GAS;

int setall;


void vdsort ( int *a ) {
  /* belegt die ersten drei Elemente eines 4er Arrays mit den drei kleinsten
     Werten */
  register int n0, n1, n2, n3 ;

  if ( a[1] < a[0] ) {
    n0 = a[1] ;
    n1 = a[0] ; }
  else {
    n0 = a[0] ;
    n1 = a[1] ; }

  if ( a[3] < a[2] ) {
    n2 = a[3] ;
    n3 = a[2] ; }
  else {
    n2 = a[2] ;
    n3 = a[3] ; }

  if ( n2 < n0 ) {
    a[0] = n2 ;
    if ( n3 < n0 ) {
      a[1] = n3 ;
      a[2] = n0 ;
      a[3] = n1 ; }
    else if ( n3 < n1 ) {
      a[1] = n0 ;
      a[2] = n3 ;
      a[3] = n1 ; }
    else {
      a[1] = n0 ;
      a[2] = n1 ;
      a[3] = n3 ; }
  }
  else {
    a[0] = n0 ;
    if ( n2 < n1 ) {
      a[1] = n2 ;
      if ( n3 < n1 ) {
        a[2] = n3 ;
        a[3] = n1 ; }
      else {
        a[2] = n1 ;
        a[3] = n3 ; }
    }
    else {
      a[1] = n1 ;
      a[2] = n2 ;
      a[3] = n3 ; }
  }
}



void vdsort2 ( int *a ) {
  /* belegt die ersten zwei Elemente eines zweier Arrays mit den zwei kleinsten Werten */
  register int n ;

  if ( a[1] < a[0] ) {
    n = a[1] ;
    a[1]=a[0] ;
    a[0]=n; 
  }
  
}


int compareFaces2 (int *a, int *b) {
  /* wird von qsort aufgerufen, vergleicht die ersten zwei Zahlen von
     Integer-Feldern */

  if ( a[0] < b[0] )
    return -1 ;
  else if ( a[0] > b[0] )
    return 1 ;
  else if ( a[1] < b[1] )
    return -1 ;
  else if ( a[1] > b[1] )
    return 1 ;
  else
    return 0 ;
}


int compareFaces3 (int *a, int *b) {
  /* wird von qsort aufgerufen, vergleicht die ersten drei Zahlen von
     Integer-Feldern */

  if ( a[0] < b[0] )
    return -1 ;
  else if ( a[0] > b[0] )
    return 1 ;
  else if ( a[1] < b[1] )
    return -1 ;
  else if ( a[1] > b[1] )
    return 1 ;
  else if ( a[2] < b[2] )
    return -1 ;
  else if ( a[2] > b[2] )
    return 1 ;
  else
    return 0 ;
}


int _compareFaces4 (int *a, int *b) {
  /* wird von qsort aufgerufen, vergleicht die ersten vier Zahlen von Integer-Feldern */

  if ( a[0] < b[0] )
    return -1 ;
  else if ( a[0] > b[0] )
    return 1 ;
  else if ( a[1] < b[1] )
    return -1 ;
  else if ( a[1] > b[1] )
    return 1 ;
  else if ( a[2] < b[2] )
    return -1 ;
  else if ( a[2] > b[2] )
    return 1 ;
  else if ( a[3] < b[3] )
    return -1 ;
  else if ( a[3] > b[3] )
    return 1 ;
  else
    return 0 ;
}

int compareFaces4 (int *a, int *b) {
  /* wird von qsort aufgerufen, vergleicht die ersten vier Zahlen von Integer-Feldern */

  if ( *a < *b )
    return -1 ;
  else if ( *a > *b )
    return 1 ;
  else if ( *(++a) < *(++b) )
    return -1 ;
  else if ( *a > *b )
    return 1 ;
  else if ( *(++a) < *(++b) )
    return -1 ;
  else if ( *a > *b )
    return 1 ;
  else if ( *(++a) < *(++b) )
    return -1 ;
  else if ( *a > *b )
    return 1 ;
  else
    return 0 ;
}



int compareElnum1 (int *a, int *b)
{
  /* wird von qsort aufgerufen, vergleicht die 1. Zahl (Elem-Nr.) von
     Integer-Feldern */

  if ( a[0] < b[0] )
    return -1 ;
  else if ( a[0] > b[0] )
    return 1 ;
  else
    return 0 ;
}

/* store the free edges of unconnected tr3 in *edges */
int findCTri3Edges(Elements *elems, int numElems, int **edges)
{
  register int i;
  int numFaces, numDisplayFaces=0;
  long long size ;
  int *pfaces, *pCurrentFace, *pFaceN, *pFaceN1, *pmax;

  numFaces = 3 * numElems ;
  size = (numFaces*5) * sizeof(int) ;
  if ( size < 1920 ) size = 1920;
  if ( (pfaces = (int *)malloc( size+1 )) == NULL )
  { printf("\n\n ERROR: malloc pfaces\n\n") ; exit(-1); }

  pCurrentFace = pfaces ;

  for ( i = 0; i < numElems; i++ )
  {
    *pCurrentFace = elems[i].nod[0] ;
    *(pCurrentFace+1) = elems[i].nod[1] ;
    vdsort2( pCurrentFace ) ;
    *(pCurrentFace+2) = 1 ;
    *(pCurrentFace+3) = i ;
    pCurrentFace+=5 ;
    *pCurrentFace = elems[i].nod[1] ;
    *(pCurrentFace+1) = elems[i].nod[2] ;
    vdsort2( pCurrentFace ) ;
    *(pCurrentFace+2) = 2 ;
    *(pCurrentFace+3) = i ;
    pCurrentFace+=5 ;
    *pCurrentFace = elems[i].nod[2] ;
    *(pCurrentFace+1) = elems[i].nod[0] ;
    vdsort2( pCurrentFace ) ;
    *(pCurrentFace+2) = 3 ;
    *(pCurrentFace+3) = i ;
    pCurrentFace+=5 ;
  }
  pmax = pCurrentFace-5 ;  /* größter zulässiger Face-Pointer */

  /* nach Knotennummern sortieren */
  qsort (pfaces, numFaces, 5*sizeof(int), (void *)compareFaces2) ;
/*  for(i=0;i<numFaces;i++)  printf("f1: %d %d\n",*(pfaces+i*5),*(pfaces+i*5+1));  */ 

  /* store just the unique edges */
  pCurrentFace = pfaces ;
  pFaceN = pfaces ;
  pFaceN1 = pfaces + 5 ;

  while ( pFaceN <= pmax ) {  /* pFaceN1 wird am Ende zurückgesetzt */
/*      printf("f1: %d %d f2:%d %d\n",*pFaceN,*(pFaceN+1),*pFaceN1,*(pFaceN1+1)); */
    if ( compareFaces2 (pFaceN, pFaceN1) == 0 ) {   /* identische nodes: 2 flaechen ueberspringen */
      pFaceN += 10  ;
      pFaceN1 += 10  ; }
    else {
      i = *(pFaceN+3) ;
      switch ( *(pFaceN + 2) ) {
        case 1:
          *pCurrentFace = elems[i].nod[0] ;          /* Feld wird gleichzeitig abgefragt (pFaceN) */
          *(pCurrentFace+1) = elems[i].nod[1] ;      /* und neu beschrieben (pCurrentFace) */
          *(pCurrentFace+2) = elems[i].nod[2] ;      /* 3. node (used in mesher) */
          break;
        case 2:
          *pCurrentFace = elems[i].nod[1] ;
          *(pCurrentFace+1) = elems[i].nod[2] ;
          *(pCurrentFace+2) = elems[i].nod[0] ;      /* 3. node (used in mesher) */
          break;
        case 3:
          *pCurrentFace = elems[i].nod[2] ;
          *(pCurrentFace+1) = elems[i].nod[0] ;
          *(pCurrentFace+2) = elems[i].nod[1] ;      /* 3. node (used in mesher) */
          break;
      }
      *(pCurrentFace+3) = i ;
      *(pCurrentFace+4) = elems[i].group ;
      pFaceN += 5 ;
      pFaceN1 += 5 ;
      pCurrentFace += 5 ;
      numDisplayFaces++ ;
    }
    if ( pFaceN1 > pmax )  pFaceN1 -= 10 ;  /* muss <= pmax sein. Vergleich
       ist dann falsch, da gleiche Flächen nur doppelt auftreten können */
  }

  *edges=pfaces;

  return(numDisplayFaces) ;
}




int getElemFaceNodes(Elements *e_enqire, int el, int face, int *nface)
{
  register int i;
  int collapseFlag=0;

  switch (e_enqire[el].type )
  {
  case 1:  /* HEXA8  */
  {
    switch (face )
    {
    case 0:
    {
      nface[3]=e_enqire[el].nod[0];
      nface[2]=e_enqire[el].nod[1];
      nface[1]=e_enqire[el].nod[2];
      nface[0]=e_enqire[el].nod[3];
    } 			               	 
    break;
    case 1:
    {  			               	 
      nface[3]=e_enqire[el].nod[4];
      nface[2]=e_enqire[el].nod[7];
      nface[1]=e_enqire[el].nod[6];
      nface[0]=e_enqire[el].nod[5];
    } 			               	 
    break;
    case 2:
    {  			               	 
      nface[3]=e_enqire[el].nod[0];
      nface[2]=e_enqire[el].nod[4];
      nface[1]=e_enqire[el].nod[5];
      nface[0]=e_enqire[el].nod[1];
    } 			               	 
    break;
    case 3:
    {  			               	 
      nface[3]=e_enqire[el].nod[1];
      nface[2]=e_enqire[el].nod[5];
      nface[1]=e_enqire[el].nod[6];
      nface[0]=e_enqire[el].nod[2];
    } 			               	 
    break;
    case 4:
    {   			               	 
      nface[3]=e_enqire[el].nod[2];
      nface[2]=e_enqire[el].nod[6];
      nface[1]=e_enqire[el].nod[7];
      nface[0]=e_enqire[el].nod[3];
    } 			               	 
    break;
    case 5:
    {   			               	 
      nface[3]=e_enqire[el].nod[3];
      nface[2]=e_enqire[el].nod[7];
      nface[1]=e_enqire[el].nod[4];
      nface[0]=e_enqire[el].nod[0];
    }
    break;
    }
    return(4);
  } 
  case 2:   /* PE6   */
  {
    switch (face )
    {
    case 0:
    {  
      nface[0]=e_enqire[el].nod[0];
      nface[1]=e_enqire[el].nod[2];
      nface[2]=e_enqire[el].nod[1];
    return(3);
    } 			               	 
    break;
    case 1:
    {  
      nface[0]=e_enqire[el].nod[3];
      nface[1]=e_enqire[el].nod[4];
      nface[2]=e_enqire[el].nod[5];
    return(3);
    } 			               	 
    break;
    case 2:
    {  
      nface[0]=e_enqire[el].nod[0];
      nface[1]=e_enqire[el].nod[1];
      nface[2]=e_enqire[el].nod[4];
      nface[3]=e_enqire[el].nod[3];
    } 			               	 
    break;
    case 3:
    {  
      nface[0]=e_enqire[el].nod[1];
      nface[1]=e_enqire[el].nod[2];
      nface[2]=e_enqire[el].nod[5];
      nface[3]=e_enqire[el].nod[4];
    } 			               	 
    break;
    case 4:
    {  
      nface[0]=e_enqire[el].nod[2];
      nface[1]=e_enqire[el].nod[0];
      nface[2]=e_enqire[el].nod[3];
      nface[3]=e_enqire[el].nod[5];
    } 			               	 
    break;
    }

    /* faces might be collapsed, investigate only surfs with 4 nodes and ev. redefine */
    if(nface[0]==nface[1])
    {
      nface[0]=nface[1];
      nface[1]=nface[2];
      nface[2]=nface[3];
      collapseFlag++;
    }
    else if(nface[1]==nface[2])
    {
      nface[0]=nface[0];
      nface[1]=nface[2];
      nface[2]=nface[3];
      collapseFlag++;
    }
    else if(nface[2]==nface[3])
    {
      collapseFlag++;
    }
    else if(nface[3]==nface[0])
    {
      collapseFlag++;
    }
    if(collapseFlag>1) return(0);
    if(collapseFlag==1) return(3);
    return(4);
  }
  case 3:   /* TET4   */
  {
    switch (face )
    {
    case 0:
    {  
      nface[0]=e_enqire[el].nod[0];
      nface[1]=e_enqire[el].nod[2];
      nface[2]=e_enqire[el].nod[1];
    } 			               	 
    break;
    case 1:
    {  
      nface[0]=e_enqire[el].nod[0];
      nface[1]=e_enqire[el].nod[1];
      nface[2]=e_enqire[el].nod[3];
    } 			               	 
    break;
    case 2:
    {  
      nface[0]=e_enqire[el].nod[1];
      nface[1]=e_enqire[el].nod[2];
      nface[2]=e_enqire[el].nod[3];
    } 			               	 
    break;
    case 3:
    {  
      nface[0]=e_enqire[el].nod[2];
      nface[1]=e_enqire[el].nod[0];
      nface[2]=e_enqire[el].nod[3];
    } 			               	 
    break;
    }
    return(3);
  }
  case 4:   /* HEXA20 */
  {
    switch (face )
    {
    case 0:
    {  
      *(nface)=e_enqire[el].nod[3];
      *(++nface)=e_enqire[el].nod[2];
      *(++nface)=e_enqire[el].nod[1];
      *(++nface)=e_enqire[el].nod[0];
      *(++nface)=e_enqire[el].nod[10];
      *(++nface)=e_enqire[el].nod[9];
      *(++nface)=e_enqire[el].nod[8];
      *(++nface)=e_enqire[el].nod[11];
      *(++nface)=e_enqire[el].nod[24];
    } 			               	 
    break;
    case 1:
    {
      *(nface)  =e_enqire[el].nod[5]; 
      *(++nface)=e_enqire[el].nod[6]; 
      *(++nface)=e_enqire[el].nod[7]; 
      *(++nface)=e_enqire[el].nod[4]; 
      *(++nface)=e_enqire[el].nod[17];
      *(++nface)=e_enqire[el].nod[18];
      *(++nface)=e_enqire[el].nod[19];
      *(++nface)=e_enqire[el].nod[16];
      *(++nface)=e_enqire[el].nod[25];
    } 			               	 
    break;
    case 2:
    {  			               	 
      *(nface)  =e_enqire[el].nod[1]; 
      *(++nface)=e_enqire[el].nod[5]; 
      *(++nface)=e_enqire[el].nod[4]; 
      *(++nface)=e_enqire[el].nod[0]; 
      *(++nface)=e_enqire[el].nod[13];
      *(++nface)=e_enqire[el].nod[16];
      *(++nface)=e_enqire[el].nod[12];
      *(++nface)=e_enqire[el].nod[8]; 
      *(++nface)=e_enqire[el].nod[20];
    } 			               	 
    break;
    case 3:
    {  			               	 
      *(nface)  =e_enqire[el].nod[2]; 
      *(++nface)=e_enqire[el].nod[6]; 
      *(++nface)=e_enqire[el].nod[5]; 
      *(++nface)=e_enqire[el].nod[1]; 
      *(++nface)=e_enqire[el].nod[14];
      *(++nface)=e_enqire[el].nod[17];
      *(++nface)=e_enqire[el].nod[13];
      *(++nface)=e_enqire[el].nod[9]; 
      *(++nface)=e_enqire[el].nod[21];
    } 			               	 
    break;
    case 4:
      {  			               	 
      *(nface)  =e_enqire[el].nod[3]; 
      *(++nface)=e_enqire[el].nod[7]; 
      *(++nface)=e_enqire[el].nod[6]; 
      *(++nface)=e_enqire[el].nod[2]; 
      *(++nface)=e_enqire[el].nod[15];
      *(++nface)=e_enqire[el].nod[18];
      *(++nface)=e_enqire[el].nod[14];
      *(++nface)=e_enqire[el].nod[10];
      *(++nface)=e_enqire[el].nod[22];
    } 			               	 
    break;
    case 5:
    {  			               	 
      *(nface)  =e_enqire[el].nod[0]; 
      *(++nface)=e_enqire[el].nod[4]; 
      *(++nface)=e_enqire[el].nod[7]; 
      *(++nface)=e_enqire[el].nod[3]; 
      *(++nface)=e_enqire[el].nod[12];
      *(++nface)=e_enqire[el].nod[19];
      *(++nface)=e_enqire[el].nod[15];
      *(++nface)=e_enqire[el].nod[11];
      *(++nface)=e_enqire[el].nod[23];
    } 
    break;
    }
    return(8);
  }
  case 5:   /* PE15  */
  {
    switch (face )
    {
    case 0:
    {  
      nface[0]=e_enqire[el].nod[0];
      nface[3]=e_enqire[el].nod[8];
      nface[1]=e_enqire[el].nod[2];
      nface[4]=e_enqire[el].nod[7];
      nface[2]=e_enqire[el].nod[1];
      nface[5]=e_enqire[el].nod[6];
    return(6);
    } 			               	 
    break;
    case 1:
    {  
      nface[0]=e_enqire[el].nod[3];
      nface[3]=e_enqire[el].nod[12];
      nface[1]=e_enqire[el].nod[4];
      nface[4]=e_enqire[el].nod[13];
      nface[2]=e_enqire[el].nod[5];
      nface[5]=e_enqire[el].nod[14];
    return(6);
    } 			               	 
    break;
    case 2:
    {  
      nface[0]=e_enqire[el].nod[0];
      nface[4]=e_enqire[el].nod[6];
      nface[1]=e_enqire[el].nod[1];
      nface[5]=e_enqire[el].nod[10];
      nface[2]=e_enqire[el].nod[4];
      nface[6]=e_enqire[el].nod[12];
      nface[3]=e_enqire[el].nod[3];
      nface[7]=e_enqire[el].nod[9];
      nface[8]=e_enqire[el].nod[15];
    } 			               	 
    break;
    case 3:
    {  
      nface[0]=e_enqire[el].nod[1];
      nface[4]=e_enqire[el].nod[7];
      nface[1]=e_enqire[el].nod[2];
      nface[5]=e_enqire[el].nod[11];
      nface[2]=e_enqire[el].nod[5];
      nface[6]=e_enqire[el].nod[13];
      nface[3]=e_enqire[el].nod[4];
      nface[7]=e_enqire[el].nod[10];
      nface[8]=e_enqire[el].nod[16];
    } 			               	 
    break;
    case 4:
    {  
      nface[0]=e_enqire[el].nod[2];
      nface[4]=e_enqire[el].nod[8];
      nface[1]=e_enqire[el].nod[0];
      nface[5]=e_enqire[el].nod[9];
      nface[2]=e_enqire[el].nod[3];
      nface[6]=e_enqire[el].nod[14];
      nface[3]=e_enqire[el].nod[5];
      nface[7]=e_enqire[el].nod[11];
      nface[8]=e_enqire[el].nod[17];
    } 			               	 
    break;
    }

    /* faces might be collapsed, investigate only surfs with 4 nodes and ev. redefine */
    if(nface[0]==nface[1])
    {
      nface[0]=nface[1];
      nface[1]=nface[2];
      nface[2]=nface[3];
      nface[3]=nface[5];
      nface[4]=nface[6];
      nface[5]=nface[7];
      collapseFlag++;
    }
    else if(nface[1]==nface[2])
    {
      nface[0]=nface[0];
      nface[1]=nface[2];
      nface[2]=nface[3];
      nface[3]=nface[4];
      nface[4]=nface[6];
      nface[5]=nface[7];
      collapseFlag++;
    }
    else if(nface[2]==nface[3])
    {
      nface[3]=nface[4];
      nface[4]=nface[5];
      nface[5]=nface[7];
      collapseFlag++;
    }
    else if(nface[3]==nface[0])
    {
      nface[3]=nface[4];
      nface[4]=nface[5];
      nface[5]=nface[6];
      collapseFlag++;
    }
    if(collapseFlag>1) return(0);
    if(collapseFlag==1) return(6);
    return(8);
  }
  case 6:   /* TET10  */
  {
    switch (face )
    {
    case 0:
    {  
      *(nface)  =e_enqire[el].nod[0];
      *(++nface)=e_enqire[el].nod[2];
      *(++nface)=e_enqire[el].nod[1];
      *(++nface)=e_enqire[el].nod[6];
      *(++nface)=e_enqire[el].nod[5];
      *(++nface)=e_enqire[el].nod[4];
    } 			               	 
    break;
    case 1:
    {  			               	 
      *(nface)  =e_enqire[el].nod[0];
      *(++nface)=e_enqire[el].nod[1];
      *(++nface)=e_enqire[el].nod[3];
      *(++nface)=e_enqire[el].nod[4];
      *(++nface)=e_enqire[el].nod[8];
      *(++nface)=e_enqire[el].nod[7];
    } 			               	 
    break;
    case 2:
    {  			               	 
      *(nface)  =e_enqire[el].nod[1];
      *(++nface)=e_enqire[el].nod[2];
      *(++nface)=e_enqire[el].nod[3];
      *(++nface)=e_enqire[el].nod[5];
      *(++nface)=e_enqire[el].nod[9];
      *(++nface)=e_enqire[el].nod[8];
    } 			               	 
    break;
    case 3:
    {			               	 
      *(nface)  =e_enqire[el].nod[2];
      *(++nface)=e_enqire[el].nod[0];
      *(++nface)=e_enqire[el].nod[3];
      *(++nface)=e_enqire[el].nod[6];
      *(++nface)=e_enqire[el].nod[7];
      *(++nface)=e_enqire[el].nod[9];
    } 			               	 
    break;
    }
    return(6);
  }
  break;
  case 7:
  {
    switch (face )
    {
    case 0:
      return(0);
    break;
    case 1:
      for (i=0; i<3; i++) nface[i]=e_enqire[el].nod[i];
      return(3);
    break;
    case 2:
      for (i=0; i<2; i++) nface[i]=e_enqire[el].nod[i];
    break;
    case 3:
      nface[0]=e_enqire[el].nod[1];
      nface[1]=e_enqire[el].nod[2];
    break;
    case 4:
      nface[0]=e_enqire[el].nod[2];
      nface[1]=e_enqire[el].nod[0];
    break;
    }
    return(2);
  }
  break;
  case 8:
  {
    switch (face )
    {
    case 0:
      return(0);
    break;
    case 1:
      for (i=0; i<6; i++) nface[i]=e_enqire[el].nod[i];
      return(6);
    break;
    case 2:
      for (i=0; i<2; i++) nface[i]=e_enqire[el].nod[i];
      nface[2]=e_enqire[el].nod[3];
    break;
    case 3:
      nface[0]=e_enqire[el].nod[1];
      nface[1]=e_enqire[el].nod[2];
      nface[2]=e_enqire[el].nod[4];
    break;
    case 4:
      nface[0]=e_enqire[el].nod[2];
      nface[1]=e_enqire[el].nod[0];
      nface[2]=e_enqire[el].nod[5];
    break;
    }
    return(3);
  }
  break;
  case 9:
  {
    switch (face )
    {
    case 0:
      return(0);
    break;
    case 1:
      for (i=0; i<4; i++) nface[i]=e_enqire[el].nod[i];
      return(4);
    break;
    case 2:
      for (i=0; i<2; i++) nface[i]=e_enqire[el].nod[i];
    break;
    case 3:
      nface[0]=e_enqire[el].nod[1];
      nface[1]=e_enqire[el].nod[2];
    break;
    case 4:
      nface[0]=e_enqire[el].nod[2];
      nface[1]=e_enqire[el].nod[3];
    break;
    case 5:
      nface[0]=e_enqire[el].nod[3];
      nface[1]=e_enqire[el].nod[0];
    break;
    }
    return(2);
  }
  break;
  case 10:
  {
    switch (face )
    {
    case 0:
      return(0);
    break;
    case 1:
      for (i=0; i<9; i++) nface[i]=e_enqire[el].nod[i];
      return(8);
    break;
    case 2:
      for (i=0; i<2; i++) nface[i]=e_enqire[el].nod[i];
      nface[2]=e_enqire[el].nod[4];
    break;
    case 3:
      nface[0]=e_enqire[el].nod[1];
      nface[1]=e_enqire[el].nod[2];
      nface[2]=e_enqire[el].nod[5];
    break;
    case 4:
      nface[0]=e_enqire[el].nod[2];
      nface[1]=e_enqire[el].nod[3];
      nface[2]=e_enqire[el].nod[6];
    break;
    case 5:
      nface[0]=e_enqire[el].nod[3];
      nface[1]=e_enqire[el].nod[0];
      nface[2]=e_enqire[el].nod[7];
    break;
    }
    return(3);
  }
  break;
  }

  return(0);
}



/* find the element face */
int getFaceNr(Elements *e_enqire, int elem_nr, int *nod)
{
  register int j,k,n;
  int  f[6], ni[8];

  n=0;
  if((e_enqire[elem_nr].type==1)||(e_enqire[elem_nr].type==4))
  {
    for(j=0; j<8; j++) 
      for(k=0; k<4; k++)
        if(e_enqire[elem_nr].nod[j]==nod[k])
          ni[n++]=j+1;
    if (n==4)
    {
      /* check which sides are involved */
      for (j=0; j<6; j++) f[j]=0;
      for (j=0; j<4; j++)
      {
        if ((ni[j]==1)||(ni[j]==2)||(ni[j]==3)||(ni[j]==4)) f[0]++;
        if ((ni[j]==5)||(ni[j]==8)||(ni[j]==7)||(ni[j]==6)) f[1]++;
        if ((ni[j]==1)||(ni[j]==5)||(ni[j]==6)||(ni[j]==2)) f[2]++;
        if ((ni[j]==2)||(ni[j]==6)||(ni[j]==7)||(ni[j]==3)) f[3]++;
        if ((ni[j]==3)||(ni[j]==7)||(ni[j]==8)||(ni[j]==4)) f[4]++;
        if ((ni[j]==4)||(ni[j]==8)||(ni[j]==5)||(ni[j]==1)) f[5]++;
      }
      for (j=0; j<6; j++){ if(f[j]==4) return(j); }
    }
  }
  if((e_enqire[elem_nr].type==2)||(e_enqire[elem_nr].type==5))
  {
    for(j=0; j<8; j++) 
      for(k=0; k<4; k++)
        if(e_enqire[elem_nr].nod[j]==nod[k])
          ni[n++]=j+1;
    if (n==4)
    {
     {
      /* check which sides are involved */
      for (j=0; j<6; j++) f[j]=0;
      for (j=0; j<4; j++)
      {
        if ((ni[j]==1)||(ni[j]==2)||(ni[j]==3))             f[0]++;
        if ((ni[j]==4)||(ni[j]==5)||(ni[j]==6))             f[1]++;
        if ((ni[j]==1)||(ni[j]==2)||(ni[j]==4)||(ni[j]==5)) f[2]++;
        if ((ni[j]==2)||(ni[j]==3)||(ni[j]==5)||(ni[j]==6)) f[3]++;
        if ((ni[j]==1)||(ni[j]==3)||(ni[j]==4)||(ni[j]==6)) f[4]++;
      }
     }
     for (j=0; j<6; j++){ if(f[j]==4) return(j); }
    }
  }
  else if((e_enqire[elem_nr].type==3)||(e_enqire[elem_nr].type==6))
  {
    for(j=0; j<4; j++) 
      for(k=0; k<3; k++)
        if(e_enqire[elem_nr].nod[j]==nod[k])
          ni[n++]=j+1;
    if (n==3)
    {
      /* check which sides are involved */
      for (j=0; j<4; j++) f[j]=0;
      for (j=0; j<3; j++)
      {
        if ((ni[j]==1)||(ni[j]==3)||(ni[j]==2)) f[0]++;
        if ((ni[j]==1)||(ni[j]==2)||(ni[j]==4)) f[1]++;
        if ((ni[j]==2)||(ni[j]==3)||(ni[j]==4)) f[2]++;
        if ((ni[j]==3)||(ni[j]==1)||(ni[j]==4)) f[3]++;
      }
    }
    for (j=0; j<4; j++){ if(f[j]==3) return(j); }
  }
  else if((e_enqire[elem_nr].type==7)||(e_enqire[elem_nr].type==8))
  {
    for(j=0; j<3; j++) 
      for(k=0; k<2; k++)
        if(e_enqire[elem_nr].nod[j]==nod[k])
          ni[n++]=j+1;
    if (n==2)
    {
      /* check which sides are involved */
      for (j=0; j<3; j++) f[j]=0;
      for (j=0; j<2; j++)
      {
        if ((ni[j]==1)||(ni[j]==2)) f[0]++;
        if ((ni[j]==2)||(ni[j]==3)) f[1]++;
        if ((ni[j]==3)||(ni[j]==1)) f[2]++;
      }
    }
    for (j=0; j<3; j++){ if(f[j]==2) return(j); }
  }
  else if((e_enqire[elem_nr].type==9)||(e_enqire[elem_nr].type==10))
  {
    for(j=0; j<4; j++) 
      for(k=0; k<2; k++)
        if(e_enqire[elem_nr].nod[j]==nod[k])
          ni[n++]=j+1;
    if (n==2)
    {
      /* check which sides are involved */
      for (j=0; j<4; j++) f[j]=0;
      for (j=0; j<2; j++)
      {
        if ((ni[j]==1)||(ni[j]==2)) f[0]++;
        if ((ni[j]==2)||(ni[j]==3)) f[1]++;
        if ((ni[j]==3)||(ni[j]==4)) f[2]++;
        if ((ni[j]==4)||(ni[j]==1)) f[3]++;
      }
    }
    for (j=0; j<4; j++){ if(f[j]==2) return(j); }
  }
  return(-1);
}



/* --- define the surfaces of the elements -- */
void makeSurfaces()
{
  register int  i,j,k,n;
  int  nf=0;
  int ncur=0;
  static int *nclo=NULL;

  setall=getSetNr("all");

  /* save all faces which are referenced in sets */
  for (i=0; i<anz->sets; i++ )
  {
    if((i!=setall)&&(set[i].anz_f>0)&&(set[i].name!=(char *)NULL))
    {
      if((nclo=(int *)realloc((int *)nclo, (ncur+1)*sizeof(int)))==NULL)
      {  printf("\n\n ERROR: realloc failure\n\n" ); return; }
      nclo[ncur++]=i;
      if(set[i].anz_elf>0)
      {
        for(j=0; j<set[i].anz_elf; j++)
          if(set[i].elf[j].n) free(set[i].elf[j].v);  
        free(set[i].elf);
        set[i].elf=NULL;
        set[i].anz_elf=0;
      }
      for(j=0; j<set[i].anz_f; j++)
      {
        if( face[set[i].face[j]].nr>-1)
        {
          n=seta(i,"j",0);
          if(n>-1)
	  {
            set[i].elf[n].e=face[set[i].face[j]].elem_nr;
            set[i].elf[n].f=face[set[i].face[j]].nr;
	  }
	}
      }
    }
  }

  /* free all faces */
  for(i=0; i<anz->f; i++)
  {
    if (face[i].type == 7)       nf=1;  /* TRI3  */
    else if (face[i].type == 8)  nf=4; /* TRI6  */
    else if (face[i].type == 9)  nf=2; /* QUAD4 */
    else if (face[i].type == 10) nf=8; /* QUAD8 */
    else if (face[i].type == 11) nf=1; /* BEAM */
    else if (face[i].type == 12) nf=2; /* BEAM3 */
    if(face[i].side!=NULL)
    {
      for(k=0; k<nf; k++) free(face[i].side[k]);
      free(face[i].side);
      face[i].side=NULL;
    }
  }

  /* delete all faces from all sets */
  for (i=0; i<anz->sets; i++ )
  {
    set[i].anz_f = 0;
    free(set[i].face);
    set[i].face=NULL;
  }

  anz->g=0;
  anz->f=0;
  /* create new faces */
  selectDisplayFaces ( anz, e_enqire, &face, &edge );

  /* normal vector: side[Nr.][x|y|z] */
  for(i=0; i<anz->f; i++)
  {
    if (face[i].type == 7)       nf=1;  /* TRI3  */
    else if (face[i].type == 8)  nf=4; /* TRI6  */
    else if (face[i].type == 9)  nf=2; /* QUAD4 */
    else if (face[i].type == 10) nf=8; /* QUAD8 */
    else if (face[i].type == 11) nf=1; /* BEAM */
    else if (face[i].type == 12) nf=2; /* BEAM3 */
    if((face[i].side=(double **)malloc((nf+1)*sizeof(double *)))==NULL)
      printf("\n\n ERROR: malloc failed\n\n" );
    for(k=0; k<nf; k++)
    {
      if((face[i].side[k]=(double *)malloc((3)*sizeof(double)))==NULL)
        printf("\n\n ERROR: malloc failed\n\n" );
    }
  }

  if(setall>=0)
  {
    if((set[setall].face =(int *)realloc((int *)set[setall].face,(anz->f+1)*sizeof(int))) == NULL)
      printf(" ERROR: malloc failed in set[%d]:%s\n\n", setall, set[setall].name);
    set[setall].anz_f=anz->f;
    for (i=0; i<set[setall].anz_f; i++) set[setall].face[i]=i;
  }

  /* store the saved faces in the sets */

  for (n=0; n<ncur; n++ )
  {
    i=nclo[n];
    for(j=0; j<set[i].anz_elf; j++)
    {
      /* store the actual face-indexes in the referenced sets */
      if(face[set[i].elf[j].e].indx[set[i].elf[j].f]>-1)
      {
        seta( i, "f", face[set[i].elf[j].e].indx[set[i].elf[j].f]);
      }
      //else 
      //  printf(" ERROR in set:%s, face:%d of element:%d does not exist.\n",set[i].name, set[i].elf[j].f+1, set[i].elf[j].e); 
    }
  }
  free(nclo);
  nclo=NULL;

  getFaceNormalen( face, node, anz );
}


void iniMeshData( char *datin, char *format )
{
  int i,j,e;
  static clock_t t0, t1 ;
  Elements *elem=NULL;

  t0=clock() ;
  if(compare(format,"ansl",4)==4)
  { 
    printf (" Try to read ansysList file\n\n");
    if ( (  readAnsysList( datin, anz, &set, &node, &elem, &lcase)) == -1) { exit(-1);}
  }
  else if(compare(format,"frd",3)==3)
  { 
    printf (" Try to read ccx results\n\n");
    if ( (readfrd( datin, anz, &node, &elem, &lcase, read_mode)) == -1) { exit(-1);}
  }
  else if(compare(format,"ccx",3)==3)
  { 
    printf (" Try to read ccx input\n\n");
    if ( (readccx( datin, anz, &set, &node, &elem, &lcase)) == -1) { exit(-1);}
  }
  else if(compare(format,"isaac2d",7)==7)
  { 
    printf (" Try to read 2D-isaac results\n\n");
    if ( (  readIsaac( datin, anz, &node, &elem, &lcase, &nBlock, 9, pref, tref, R_GAS)) == -1) { exit(-1);}
  }
  else if(compare(format,"isaac3d",7)==7)
  { 
    printf (" Try to read 3D-isaac results\n\n");
    if ( (  readIsaac( datin, anz, &node, &elem, &lcase, &nBlock, 1, pref, tref, R_GAS)) == -1) { exit(-1);}
  }
  else if(compare(format,"duns2d",6)==6)
  { 
    printf (" Try to read 2D-duns results\n\n");
    if ( (  readDuns( datin, anz, &node, &elem, &lcase, 9)) == -1) { exit(-1);}
  }
  else if(compare(format,"duns3d",6)==6)
  { 
    printf (" Try to read 3D-duns results\n\n");
    if ( (  readDuns( datin, anz, &node, &elem, &lcase, 1)) == -1) { exit(-1);}
  }
  else if(compare(format,"foam",4)==4)
  { 
    printf (" Try to read foam results\n\n");
    if ( (  readFoam( datin, anz, &set, &node, &elem, &lcase)) == -1) { exit(-1);}
  }
  else if(compare(format,"nas",2)==2)
  { 
    printf (" Try to read nastran f06 file\n\n");
    if ( (  readNastran( datin, anz, &node, &elem, &lcase)) == -1) { exit(-1);}
  }
  else if(compare(format,"ng",2)==2)
  { 
    printf (" Try to read NG file\n\n");
    if ( (  readNG( datin, anz, &set, &node, &elem, &lcase)) == -1) { exit(-1);}
  }
  else if(compare(format,"stl",3)==3)
  { 
    printf (" Try to read stl file\n\n");
    if ( (  readStl( datin, anz, &node, &elem, &lcase)) == -1) { exit(-1);}
  }

  t1=clock() ;
  printf (" Elements: %d Nodes:%d Datasets:%d ",anz->e,anz->n,anz->l);
  printf (" MinElemNr: %d MaxElemNr: %d MinNodNr:%d MaxNodNr:%d ",anz->emin,anz->emax,anz->nmin,anz->nmax);
  printf (" read in %lf sec \n",(double)(t1-t0)/CLOCKS_PER_SEC);
  for( i=1; i<13; i++) 
  {
    printf(" found elements of type %d: %d\n", i, anz->etype[i]);
  }
  elemChecker( anz->e, node, elem);

  /* the nodes numbers have to be stored before the nodes for drawing purposes are generated */
  anz->orignmax   = anz->nmax;
  anz->orign      = anz->n;
  anz->olc       = anz->l;

  /* iniNodes */
  /* flag the nodes */
  for( i=0; i<anz->n; i++) node[node[i].nr].pflag=0;
  /* initialize the normal vector on each node */
  for (i=0; i<anz->n; i++) for(j=0; j<3; j++) node[node[i].nr].nv[j]=0.;
  iniElements(anz, elem, 0);

  if((compare(format,"foam",4)==4)||(compare(format,"ccx",3)==3))
  { 
    for(i=0; i<anz->sets; i++)
    {
      for (j=0; j<set[i].anz_elf; j++)
      {
        /* store the actual face-indexes in the referenced sets */
	/*
        if(face[set[i].elf[j].e].indx[set[i].elf[j].f]==-1)
	{
          printf(" ERROR in set:%s, face:%d of element:%d does not exist.\n",set[i].name, set[i].elf[j].f+1, set[i].elf[j].e); 
	}
        else
	*/
	{
          e=set[i].elf[j].e;
          if((e_enqire[e].type>6)&&(e_enqire[e].type<11))
          {
	    //printf("e_enqire[%d].attr:%d f:%d\n", e,e_enqire[e].attr, set[i].elf[j].f);
            if(e_enqire[e].attr>3)
            {
              set[i].elf[j].f++;
              if(e_enqire[e].type<9)
  	      {
              if(set[i].elf[j].f>4) set[i].elf[j].f=1;
  	      }
  	      else
  	      {
                if(set[i].elf[j].f>5) set[i].elf[j].f=1;
  	      }
  	    }
            else
            {
              set[i].elf[j].f--;
              if(set[i].elf[j].f==0) set[i].elf[j].f=1;
  	    }
          }
          seta( i, "f", face[e].indx[set[i].elf[j].f]);
	}
      }  
    }  
  }
  else if((compare(format,"ng",2)==2)||(compare(format,"nas",3)==3))
  {
    for (i=0; i<anz->sets; i++)
    {
      if((set[i].name!=NULL)&&(compare(set[i].name, "+set", 4)==4))
      {
        /* get the nodes and faces */
        completeSet(set[i].name, "do");
        completeSet(set[i].name, "do");
      }
    }
    /* if shells and volumes exist delete the shells */
    if((compare(format,"ng",2)==2)&&((anz->etype[3]>0)||(anz->etype[6]>0)))
    {
      printf("found volume elements -> delete all shells and beams.\n");
      for (i=0; i<anz->sets; i++)
      {
        if(set[i].name!=NULL)
	{
          if(compare(set[i].name, "+typ7", 5)==5) delElem( set[i].anz_e, set[i].elem );
          if(compare(set[i].name, "+typ8", 5)==5) delElem( set[i].anz_e, set[i].elem );
          if(compare(set[i].name, "+typ11", 6)==6) delElem( set[i].anz_e, set[i].elem );
          if(compare(set[i].name, "+typ12", 6)==6) delElem( set[i].anz_e, set[i].elem );
	}
      }
      for (i=0; i<anz->sets; i++)
      {
        if((set[i].name!=NULL)&&(compare(set[i].name, "+set", 4)==4))
        {
          /* get the nodes and faces */
          completeSet(set[i].name, "do");
        }
      }
    }
    prnt("se");
  }

  createSuperSets(); 
  free(elem); elem=NULL;
}



void iniElements(Summen *anz, Elements *elem, int ini_e_enqire)
{
  register int  i,k, n, n1, n2;
  int ipuf, nf=0;

  if((anz->orignmax>anz->nmax)||(anz->orign>anz->n))
  {
    /* free the additional midside-nodes for higher order elements */
    for(i=anz->orign; i<anz->n; i++) node[node[i].nr].pflag=-1;
    anz->n= anz->orign;
    anz->nmax=anz->orignmax;
  }

  /* ------------- fill the element struktures -------------------------------- */

  /* free all faces */
  for(i=0; i<anz->f; i++)
  {
    if (face[i].type == 7)       nf=1;  /* TRI3  */
    else if (face[i].type == 8)  nf=4; /* TRI6  */
    else if (face[i].type == 9)  nf=2; /* QUAD4 */
    else if (face[i].type == 10) nf=8; /* QUAD8 */
    else if (face[i].type == 11) nf=1; /* BEAM */
    else if (face[i].type == 12) nf=2; /* BEAM3 */
    if(face[i].side!=NULL)
    {
      for(k=0; k<nf; k++) free(face[i].side[k]);
      free(face[i].side);
      face[i].side=NULL;
    }
  }

  /* delete all faces from all sets */
  for (i=0; i<anz->sets; i++ )
  {
    set[i].anz_f = 0;
    free(set[i].face);
    set[i].face=NULL;
  }

  anz->g=0;
  anz->f=0;

  /* free the e_enqire-structure */
  for (i=0; i<ini_e_enqire; i++ )
  {
    if(e_enqire[e_enqire[i].nr].side!=NULL)
    {
      /* free space for the normal-vectors */
      nf=0;
      if (e_enqire[e_enqire[i].nr].type == 1)       nf=6;  /* HEXA8 */
      else if (e_enqire[e_enqire[i].nr].type == 2)  nf=6;  /* PENTA6 */
      else if (e_enqire[e_enqire[i].nr].type == 3)  nf=4;  /* TET4 */
      else if (e_enqire[e_enqire[i].nr].type == 4)  nf=48; /* HEXA20 */
      else if (e_enqire[e_enqire[i].nr].type == 5)  nf=48; /* PENTA15 */
      else if (e_enqire[e_enqire[i].nr].type == 6)  nf=16; /* TET10 */
      else if (e_enqire[e_enqire[i].nr].type == 7)  nf=1;  /* TRI3  */
      else if (e_enqire[e_enqire[i].nr].type == 8)  nf=4; /* TRI6  */
      else if (e_enqire[e_enqire[i].nr].type == 9)  nf=2; /* QUAD4 */
      else if (e_enqire[e_enqire[i].nr].type == 10) nf=8; /* QUAD8 */
      else if (e_enqire[e_enqire[i].nr].type == 11) nf=1; /* BEAM */
      else if (e_enqire[e_enqire[i].nr].type == 12) nf=1; /* BEAM3 */
      if(nf)
      {
        for(k=0; k<nf; k++) free(e_enqire[e_enqire[i].nr].side[k]);
        free(e_enqire[e_enqire[i].nr].side);
        e_enqire[e_enqire[i].nr].side=NULL;
      }
    }
  }

  /* e_enqire is used as e_enqire[elem[i].nr].xx   */
  if ( (e_enqire = (Elements *)realloc((Elements *)e_enqire, (anz->emax+1) * sizeof(Elements))) == NULL )
    printf(" ERROR: realloc in iniMeshData(), e_enqire\n\n") ;
  for (i=0; i<anz->e; i++)
  {
    e_enqire[i].nr    = elem[i].nr;
    e_enqire[ elem[i].nr ].group = elem[i].group;
    e_enqire[ elem[i].nr ].mat   = elem[i].mat;
    e_enqire[ elem[i].nr ].type  = elem[i].type;
    e_enqire[ elem[i].nr ].attr  = elem[i].attr;
    e_enqire[ elem[i].nr ].side  = NULL;


    if (elem[i].mat != elemMat[elemMat[0]])
    {
      for (n=1; n<elemMat[0]; n++) if (elem[i].mat == elemMat[n]) goto have_mat;
      elemMat[0]++; elemMat[elemMat[0]]=elem[i].mat;
      have_mat:;
    }
    ipuf=0; nf=1;
    switch(elem[i].type)
    {
      case 1:
      ipuf = 8;    nf=6; /* HEXA8 */
      break;
      case 2:
      ipuf = 6;    nf=6; /* PENTA6 */
      break;
      case 3:
      ipuf = 4;    nf=4; /* TET4 */
      break;
      case 4:
      ipuf = 20;   nf=48;/* HEX20 */
      break;
      case 5:
      ipuf = 15;   nf=48;/* PENTA15 */
      break;
      case 6:
      ipuf = 10;   nf=16;/* TET10 */
      break;
      case 7:
      ipuf = 3;    nf=1; /* TRI3  */
      break;
      case 8:
      ipuf = 6;    nf=4; /* TRI6  */
      break;
      case 9:
      ipuf = 4;    nf=2; /* QUAD4 */
      break;
      case 10:
      ipuf = 8;    nf=8; /* QUAD8 */
      break;
      case 11:
      ipuf = 2;    nf=1; /* BEAM */
      break;
      case 12:
      ipuf = 3;    nf=1; /* BEAM3 */
      break;
    }
    if (ipuf!=0)
    {
      for (n=0; n<ipuf; n++)
      {
        if(elem[i].nod[n]>anz->nmax) { printf("ERROR: nod:%d used in elem:%d not existent. Highest node-nr:%d\n", elem[i].nod[n], elem[i].nr, anz->nmax); exit(0); }
        e_enqire[ elem[i].nr ].nod[n]=elem[i].nod[n];
      }
    }
    /* space for the normal-vectors */
    if((e_enqire[e_enqire[i].nr].side=(double **)malloc((nf)*sizeof(double *)))==NULL)
      printf("\n\n ERROR: malloc failed\n\n" );
    for(k=0; k<nf; k++)
    {
      if((e_enqire[e_enqire[i].nr].side[k]=(double *)malloc((3)*sizeof(double)))==NULL)
        printf("\n\n ERROR: malloc failed\n\n" );
    }


    if (elem[i].type == 4)
    {
      if ( (node = (Nodes *)realloc( (Nodes *)node, (anz->nmax+7) * sizeof(Nodes))) == NULL )
        printf("\n\n ERROR: realloc failed node\n\n") ;
      for (n=0; n<3; n++)  
      {
        anz->nmax++;
        node[anz->n].nr = anz->nmax;
        node[anz->nmax].pflag=1;
        node[anz->nmax].indx=anz->n;

        node[anz->nmax].nx = -1./4.* (
          node[elem[i].nod[0+n]].nx+node[elem[i].nod[1+n]].nx    +
          node[elem[i].nod[5+n]].nx+node[elem[i].nod[4+n]].nx )  + 1./2.*(
          node[elem[i].nod[8+n]].nx+node[elem[i].nod[13+n]].nx   +
          node[elem[i].nod[16+n]].nx+node[elem[i].nod[12+n]].nx) ;

        node[anz->nmax].ny = -1./4.* (
          node[elem[i].nod[0+n]].ny+node[elem[i].nod[1+n]].ny    +
          node[elem[i].nod[5+n]].ny+node[elem[i].nod[4+n]].ny )  + 1./2.*(
          node[elem[i].nod[8+n]].ny+node[elem[i].nod[13+n]].ny   +
          node[elem[i].nod[16+n]].ny+node[elem[i].nod[12+n]].ny) ;

        node[anz->nmax].nz = -1./4.* (
          node[elem[i].nod[0+n]].nz+node[elem[i].nod[1+n]].nz    +
          node[elem[i].nod[5+n]].nz+node[elem[i].nod[4+n]].nz )  + 1./2.*(
          node[elem[i].nod[8+n]].nz+node[elem[i].nod[13+n]].nz   +
          node[elem[i].nod[16+n]].nz+node[elem[i].nod[12+n]].nz) ;

        e_enqire[ elem[i].nr ].nod[n+20]=node[anz->n].nr;
        anz->n++;
      }
        anz->nmax++;
        node[anz->n].nr = anz->nmax;
        node[anz->nmax].pflag=1;
        node[anz->nmax].indx=anz->n;

        node[anz->nmax].nx = -1./4.* (
          node[elem[i].nod[3]].nx+node[elem[i].nod[0]].nx    +
          node[elem[i].nod[4]].nx+node[elem[i].nod[7]].nx )  + 1./2.*(
          node[elem[i].nod[11]].nx+node[elem[i].nod[12]].nx   +
          node[elem[i].nod[19]].nx+node[elem[i].nod[15]].nx) ;

        node[anz->nmax].ny = -1./4.* (
          node[elem[i].nod[3]].ny+node[elem[i].nod[0]].ny    +
          node[elem[i].nod[4]].ny+node[elem[i].nod[7]].ny )  + 1./2.*(
          node[elem[i].nod[11]].ny+node[elem[i].nod[12]].ny   +
          node[elem[i].nod[19]].ny+node[elem[i].nod[15]].ny) ;

        node[anz->nmax].nz = -1./4.* (
          node[elem[i].nod[3]].nz+node[elem[i].nod[0]].nz    +
          node[elem[i].nod[4]].nz+node[elem[i].nod[7]].nz )  + 1./2.*(
          node[elem[i].nod[11]].nz+node[elem[i].nod[12]].nz   +
          node[elem[i].nod[19]].nz+node[elem[i].nod[15]].nz) ;

        e_enqire[ elem[i].nr ].nod[23]=node[anz->n].nr;
        anz->n++;

      for (n=0; n<2; n++) 
      {
        anz->nmax++;
        node[anz->n].nr = anz->nmax;
        node[anz->nmax].pflag=1;
        node[anz->nmax].indx=anz->n;

        n1=n*4;
        n2=n*8;
        node[anz->nmax].nx = -1./4.* (
          node[elem[i].nod[0+n1]].nx+node[elem[i].nod[1+n1]].nx    +
          node[elem[i].nod[2+n1]].nx+node[elem[i].nod[3+n1]].nx )  + 1./2.*(
          node[elem[i].nod[8+n2]].nx+node[elem[i].nod[9+n2]].nx   +
          node[elem[i].nod[10+n2]].nx+node[elem[i].nod[11+n2]].nx) ;

        node[anz->nmax].ny = -1./4.* (
          node[elem[i].nod[0+n1]].ny+node[elem[i].nod[1+n1]].ny    +
          node[elem[i].nod[2+n1]].ny+node[elem[i].nod[3+n1]].ny )  + 1./2.*(
          node[elem[i].nod[8+n2]].ny+node[elem[i].nod[9+n2]].ny   +
          node[elem[i].nod[10+n2]].ny+node[elem[i].nod[11+n2]].ny) ;

        node[anz->nmax].nz = -1./4.* (
          node[elem[i].nod[0+n1]].nz+node[elem[i].nod[1+n1]].nz    +
          node[elem[i].nod[2+n1]].nz+node[elem[i].nod[3+n1]].nz )  + 1./2.*(
          node[elem[i].nod[8+n2]].nz+node[elem[i].nod[9+n2]].nz   +
          node[elem[i].nod[10+n2]].nz+node[elem[i].nod[11+n2]].nz) ;

        e_enqire[ elem[i].nr ].nod[n+24]=node[anz->n].nr;
        anz->n++;
      }
    }
    if (elem[i].type == 5)
    {
      if ( (node = (Nodes *)realloc( (Nodes *)node, (anz->nmax+6) * sizeof(Nodes))) == NULL )
        printf("\n\n ERROR: realloc failed node\n\n") ;
      for (n=0; n<2; n++)  
      {
        anz->nmax++;
        node[anz->n].nr = anz->nmax;
        node[anz->nmax].pflag=1;
        node[anz->nmax].indx=anz->n;

        node[anz->nmax].nx = -1./4.* (
          node[elem[i].nod[0+n]].nx+node[elem[i].nod[1+n]].nx    +
          node[elem[i].nod[4+n]].nx+node[elem[i].nod[3+n]].nx )  + 1./2.*(
          node[elem[i].nod[6+n]].nx+node[elem[i].nod[10+n]].nx   +
          node[elem[i].nod[12+n]].nx+node[elem[i].nod[ 9+n]].nx) ;

        node[anz->nmax].ny = -1./4.* (
          node[elem[i].nod[0+n]].ny+node[elem[i].nod[1+n]].ny    +
          node[elem[i].nod[4+n]].ny+node[elem[i].nod[3+n]].ny )  + 1./2.*(
          node[elem[i].nod[6+n]].ny+node[elem[i].nod[10+n]].ny   +
          node[elem[i].nod[12+n]].ny+node[elem[i].nod[ 9+n]].ny) ;

        node[anz->nmax].nz = -1./4.* (
          node[elem[i].nod[0+n]].nz+node[elem[i].nod[1+n]].nz    +
          node[elem[i].nod[4+n]].nz+node[elem[i].nod[3+n]].nz )  + 1./2.*(
          node[elem[i].nod[6+n]].nz+node[elem[i].nod[10+n]].nz   +
          node[elem[i].nod[12+n]].nz+node[elem[i].nod[ 9+n]].nz) ;

        e_enqire[ elem[i].nr ].nod[n+15]=node[anz->n].nr;
        anz->n++;
      }
        anz->nmax++;
        node[anz->n].nr = anz->nmax;
        node[anz->nmax].pflag=1;
        node[anz->nmax].indx=anz->n;

        node[anz->nmax].nx = -1./4.* (
          node[elem[i].nod[2]].nx+node[elem[i].nod[0]].nx    +
          node[elem[i].nod[3]].nx+node[elem[i].nod[5]].nx )  + 1./2.*(
          node[elem[i].nod[ 8]].nx+node[elem[i].nod[ 9]].nx   +
          node[elem[i].nod[14]].nx+node[elem[i].nod[11]].nx) ;

        node[anz->nmax].ny = -1./4.* (
          node[elem[i].nod[2]].ny+node[elem[i].nod[0]].ny    +
          node[elem[i].nod[3]].ny+node[elem[i].nod[5]].ny )  + 1./2.*(
          node[elem[i].nod[ 8]].ny+node[elem[i].nod[ 9]].ny   +
          node[elem[i].nod[14]].ny+node[elem[i].nod[11]].ny) ;

        node[anz->nmax].nz = -1./4.* (
          node[elem[i].nod[2]].nz+node[elem[i].nod[0]].nz    +
          node[elem[i].nod[3]].nz+node[elem[i].nod[5]].nz )  + 1./2.*(
          node[elem[i].nod[ 8]].nz+node[elem[i].nod[ 9]].nz   +
          node[elem[i].nod[14]].nz+node[elem[i].nod[11]].nz) ;

        e_enqire[ elem[i].nr ].nod[17]=node[anz->n].nr;
        anz->n++;

        anz->nmax++;
        node[anz->n].nr = anz->nmax;
        node[anz->nmax].pflag=1;
        node[anz->nmax].indx=anz->n;

        node[anz->nmax].nx = -1./4.* (
          node[elem[i].nod[0]].nx+node[elem[i].nod[2]].nx    +
          node[elem[i].nod[1]].nx+node[elem[i].nod[0]].nx )  + 1./2.*(
          node[elem[i].nod[ 8]].nx+node[elem[i].nod[ 7]].nx   +
          node[elem[i].nod[ 6]].nx+node[elem[i].nod[ 0]].nx) ;

        node[anz->nmax].ny = -1./4.* (
          node[elem[i].nod[0]].ny+node[elem[i].nod[2]].ny    +
          node[elem[i].nod[1]].ny+node[elem[i].nod[0]].ny )  + 1./2.*(
          node[elem[i].nod[ 8]].ny+node[elem[i].nod[ 7]].ny   +
          node[elem[i].nod[ 6]].ny+node[elem[i].nod[ 0]].ny) ;

        node[anz->nmax].nz = -1./4.* (
          node[elem[i].nod[0]].nz+node[elem[i].nod[2]].nz    +
          node[elem[i].nod[1]].nz+node[elem[i].nod[0]].nz )  + 1./2.*(
          node[elem[i].nod[ 8]].nz+node[elem[i].nod[ 7]].nz   +
          node[elem[i].nod[ 6]].nz+node[elem[i].nod[ 0]].nz) ;

        e_enqire[ elem[i].nr ].nod[18]=node[anz->n].nr;
        anz->n++;

        anz->nmax++;
        node[anz->n].nr = anz->nmax;
        node[anz->nmax].pflag=1;
        node[anz->nmax].indx=anz->n;

        node[anz->nmax].nx = -1./4.* (
          node[elem[i].nod[3]].nx+node[elem[i].nod[4]].nx    +
          node[elem[i].nod[5]].nx+node[elem[i].nod[3]].nx )  + 1./2.*(
          node[elem[i].nod[12]].nx+node[elem[i].nod[13]].nx   +
          node[elem[i].nod[14]].nx+node[elem[i].nod[ 3]].nx) ;

        node[anz->nmax].ny = -1./4.* (
          node[elem[i].nod[3]].ny+node[elem[i].nod[4]].ny    +
          node[elem[i].nod[5]].ny+node[elem[i].nod[3]].ny )  + 1./2.*(
          node[elem[i].nod[12]].ny+node[elem[i].nod[13]].ny   +
          node[elem[i].nod[14]].ny+node[elem[i].nod[ 3]].ny) ;

        node[anz->nmax].nz = -1./4.* (
          node[elem[i].nod[3]].nz+node[elem[i].nod[4]].nz    +
          node[elem[i].nod[5]].nz+node[elem[i].nod[3]].nz )  + 1./2.*(
          node[elem[i].nod[12]].nz+node[elem[i].nod[13]].nz   +
          node[elem[i].nod[14]].nz+node[elem[i].nod[ 3]].nz) ;

        e_enqire[ elem[i].nr ].nod[19]=node[anz->n].nr;
        anz->n++;
    }
    if (elem[i].type == 10)
    {
      /* calculate the midside node */
      if ( (node = (Nodes *)realloc( (Nodes *)node, (anz->nmax+2) * sizeof(Nodes))) == NULL )
        printf("\n\n ERROR: realloc failed node\n\n") ;
      anz->nmax++;
      node[anz->n].nr = anz->nmax;
      node[anz->nmax].pflag=1;
      node[anz->nmax].indx=anz->n;

        node[anz->nmax].nx = -1./4.* (
          node[e_enqire[e_enqire[i].nr].nod[0]].nx+node[e_enqire[e_enqire[i].nr].nod[1]].nx  +
          node[e_enqire[e_enqire[i].nr].nod[3]].nx+node[e_enqire[e_enqire[i].nr].nod[2]].nx )+ 1./2.*(
          node[e_enqire[e_enqire[i].nr].nod[4]].nx+node[e_enqire[e_enqire[i].nr].nod[6]].nx  +
          node[e_enqire[e_enqire[i].nr].nod[7]].nx+node[e_enqire[e_enqire[i].nr].nod[5]].nx) ;

        node[anz->nmax].ny = -1./4.* (
          node[e_enqire[e_enqire[i].nr].nod[0]].ny+node[e_enqire[e_enqire[i].nr].nod[1]].ny  +
          node[e_enqire[e_enqire[i].nr].nod[3]].ny+node[e_enqire[e_enqire[i].nr].nod[2]].ny )+ 1./2.*(
          node[e_enqire[e_enqire[i].nr].nod[4]].ny+node[e_enqire[e_enqire[i].nr].nod[6]].ny  +
          node[e_enqire[e_enqire[i].nr].nod[7]].ny+node[e_enqire[e_enqire[i].nr].nod[5]].ny) ;

        node[anz->nmax].nz = -1./4.* (
          node[e_enqire[e_enqire[i].nr].nod[0]].nz+node[e_enqire[e_enqire[i].nr].nod[1]].nz  +
          node[e_enqire[e_enqire[i].nr].nod[3]].nz+node[e_enqire[e_enqire[i].nr].nod[2]].nz )+ 1./2.*(
          node[e_enqire[e_enqire[i].nr].nod[4]].nz+node[e_enqire[e_enqire[i].nr].nod[6]].nz  +
          node[e_enqire[e_enqire[i].nr].nod[7]].nz+node[e_enqire[e_enqire[i].nr].nod[5]].nz) ;
        e_enqire[e_enqire[i].nr].nod[8]=node[anz->n].nr;
        anz->n++;
    }

  }

  getElemNormalen( e_enqire, node, anz->e );
  makeSurfaces();
  realloc_colNr();
}

