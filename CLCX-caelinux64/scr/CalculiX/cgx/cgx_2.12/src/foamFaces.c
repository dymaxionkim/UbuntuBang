#include <extUtil.h>

#define TEST 0

/*******************************************************************/
/* in:                                                             */
/*   elems: he8 elements and amount: numElems                     */
/* out:                                                            */
/*   ptr: all inner faces pointing to the "next" element           */
/*******************************************************************/
typedef struct {
    int e0;
    int e1;
    int i;
}Esort;



int compareFaces (int *a, int *b) {
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


int compareElems(Esort *a, Esort *b)
{
  /* wird von qsort aufgerufen, vergleicht 2 Int-Felder */

  if ( a[0].e0 < b[0].e0 )
    return (-1) ;
  else if ( a[0].e0 > b[0].e0 )
    return (1) ;
  else
  {
    if ( a[0].e1 < b[0].e1 ) 
      return (-1) ;
    else if ( a[0].e1 > b[0].e1 )
      return (1) ;
    else
    {
      return (0) ;
    }
  }
}

int *innerFacesHe8(Elements *elems, int numElems, Faces **ptr)
{
  int i,j,n, numFaces, size ;
  int *pfaces, *pCurrentFace, *pFaceN, *pmax;
  int nif, nof;
  static int nf[2];
  Faces *iface=NULL;
  Faces *iface2=NULL;
  Esort *esort=NULL;

  int fnr[2], enr[2], fnod[2][4]; 

  extern int compareElnum () ;
  extern void vdsort ( int *) ;
  extern int commonEdge (int *, int *) ;


  numFaces = 6 * numElems ;
  size = (6* numFaces) * sizeof(int) ;
  if ( size < 1920 ) size = 1920;

  if ( (pfaces = (int *)malloc( size )) == NULL )
  { printf("\n\n ERROR: malloc pfaces\n\n") ; exit(-1); }

  pCurrentFace = pfaces ;

  for ( i = 0; i < numElems; i++ )
  {
    *pCurrentFace = elems[i].nod[0] ;
    *(pCurrentFace+1) = elems[i].nod[3] ;
    *(pCurrentFace+2) = elems[i].nod[2] ;
    *(pCurrentFace+3) = elems[i].nod[1] ;
    vdsort( pCurrentFace ) ;
    *(pCurrentFace+3) = 1 ;
    *(pCurrentFace+4) = i ;
    pCurrentFace += 6 ;
    *pCurrentFace = elems[i].nod[4] ;
    *(pCurrentFace+1) = elems[i].nod[5] ;
    *(pCurrentFace+2) = elems[i].nod[6] ;
    *(pCurrentFace+3) = elems[i].nod[7] ;
    vdsort( pCurrentFace ) ;
    *(pCurrentFace+3) = 2 ;
    *(pCurrentFace+4) = i ;
    pCurrentFace += 6 ;
    *pCurrentFace = elems[i].nod[0] ;
    *(pCurrentFace+1) = elems[i].nod[1] ;
    *(pCurrentFace+2) = elems[i].nod[5] ;
    *(pCurrentFace+3) = elems[i].nod[4] ;
    vdsort( pCurrentFace ) ;
    *(pCurrentFace+3) = 3 ;
    *(pCurrentFace+4) = i ;
    pCurrentFace += 6 ;
    *pCurrentFace = elems[i].nod[1] ;
    *(pCurrentFace+1) = elems[i].nod[2] ;
    *(pCurrentFace+2) = elems[i].nod[6] ;
    *(pCurrentFace+3) = elems[i].nod[5] ;
    vdsort( pCurrentFace ) ;
    *(pCurrentFace+3) = 4 ;
    *(pCurrentFace+4) = i ;
    pCurrentFace += 6 ;
    *pCurrentFace = elems[i].nod[3] ;
    *(pCurrentFace+1) = elems[i].nod[7] ;
    *(pCurrentFace+2) = elems[i].nod[6] ;
    *(pCurrentFace+3) = elems[i].nod[2] ;
    vdsort( pCurrentFace ) ;
    *(pCurrentFace+3) = 5 ;
    *(pCurrentFace+4) = i ;
    pCurrentFace += 6 ;
    *pCurrentFace = elems[i].nod[0] ;
    *(pCurrentFace+1) = elems[i].nod[4] ;
    *(pCurrentFace+2) = elems[i].nod[7] ;
    *(pCurrentFace+3) = elems[i].nod[3] ;
    vdsort( pCurrentFace ) ;
    *(pCurrentFace+3) = 6 ;
    *(pCurrentFace+4) = i ;
    pCurrentFace += 6 ;
  }
  pmax = pCurrentFace ;  /* größter zulässiger Face-Pointer */

  /* nach Knotennummern sortieren */
  qsort (pfaces, numFaces, 6*sizeof(int), (void *)compareFaces) ;


  /* search inner faces */
  nif=nof=n=0;
  pFaceN = pfaces ;

  while ( pFaceN <pmax )
  {
    /* if two faces are identical, store the one with the lower elem-index */
    /* and store the second element and face in nod[4] and nod[5] */
    if ((pFaceN < pmax-6)&&( compareFaces3 (pFaceN, (pFaceN+6)) == 0 ))
    {
	n+=2;
      /* recover the two faces in the right order */
      enr[0]=*(pFaceN + 4);
      enr[1]=*(pFaceN + 10);
      fnr[0]=*(pFaceN + 3);
      fnr[1]=*(pFaceN + 9);
      for(j=0; j<2; j++)
      {
        i=enr[j];
        switch(fnr[j])
        {
          case 1:
            fnod[j][0] = elems[i].nod[0] ;
            fnod[j][1] = elems[i].nod[3] ;
            fnod[j][2] = elems[i].nod[2] ;
            fnod[j][3] = elems[i].nod[1] ;
            break;
          case 2:
            fnod[j][0] = elems[i].nod[4] ;
            fnod[j][1] = elems[i].nod[5] ;
            fnod[j][2] = elems[i].nod[6] ;
            fnod[j][3] = elems[i].nod[7] ;
            break;
          case 3:
            fnod[j][0] = elems[i].nod[0] ;
            fnod[j][1] = elems[i].nod[1] ;
            fnod[j][2] = elems[i].nod[5] ;
            fnod[j][3] = elems[i].nod[4] ;
            break;
          case 4:
            fnod[j][0] = elems[i].nod[1] ;
            fnod[j][1] = elems[i].nod[2] ;
            fnod[j][2] = elems[i].nod[6] ;
            fnod[j][3] = elems[i].nod[5] ;
            break;
          case 5:
            fnod[j][0] = elems[i].nod[3] ;
            fnod[j][1] = elems[i].nod[7] ;
            fnod[j][2] = elems[i].nod[6] ;
            fnod[j][3] = elems[i].nod[2] ;
            break;
          case 6:
            fnod[j][0] = elems[i].nod[0] ;
            fnod[j][1] = elems[i].nod[4] ;
            fnod[j][2] = elems[i].nod[7] ;
            fnod[j][3] = elems[i].nod[3] ;
            break;
        }
      }

      if((iface=(Faces *)realloc((Faces *)iface, (nif+1)*sizeof(Faces)))==NULL)
        printf("\n\n ERROR: realloc failed iface\n\n") ;
  
      if(enr[0]<enr[1])
      {
        iface[nif].elem_nr =elems[enr[0]].nr; 
        iface[nif].nr=fnr[0];
        iface[nif].nod[4]=elems[enr[1]].nr;
        iface[nif].nod[5]=fnr[1];
        for(j=0; j<4; j++) iface[nif].nod[j]=fnod[0][j];
      }
      else      
      {
        iface[nif].elem_nr =elems[enr[1]].nr; 
        iface[nif].nr=fnr[1];
        iface[nif].nod[4]=elems[enr[0]].nr;
        iface[nif].nod[5]=fnr[0];
        for(j=0; j<4; j++) iface[nif].nod[j]=fnod[1][j];
      }
  
      iface[nif].type    =9; 
      iface[nif].group   =0;
      iface[nif].mat     =0;
      iface[nif].side    = NULL;

#if TEST
      printf("n:%d  e1:%d f:%d n:%d %d %d %d\n",n,elems[i].nr, iface[nif].nr,iface[nif].nod[0],iface[nif].nod[1],iface[nif].nod[2], iface[nif].nod[3]);
      printf("  e2:%d f:%d\n",iface[nif].nod[4], iface[nif].nod[5]);
#endif
  
      nif++;
  
      pFaceN += 12 ;
    }
    else
    {
      n++;
#if TEST
      printf("n:%d e:%d f:%d n:%d %d %d xx\n",n,elems[*(pFaceN+4)].nr,*(pFaceN+3),*(pFaceN+0),*(pFaceN+1),*(pFaceN+2));
#endif
      nof++;
      pFaceN += 6 ;
    }
  }

  /* sort the faces according to the related element indexes */
  if ( (esort = (Esort *)malloc( (nif+1) * sizeof(Esort))) == NULL )
    printf("ERROR: realloc failed: Rsort\n\n" ); 
  for( i=0; i<nif; i++)
  {
    esort[i].e0=iface[i].elem_nr;
    esort[i].e1=iface[i].nod[4];
    esort[i].i=i;
  }
  qsort( esort, nif, sizeof(Esort), (void *)compareElems );

  if((iface2=(Faces *)malloc((nif+1)*sizeof(Faces)))==NULL)
    printf("\n\n ERROR: realloc failed iface\n\n") ;

  for( i=0; i<nif; i++)
  {
    iface2[i].elem_nr =iface[esort[i].i].elem_nr; 
    iface2[i].nr      =iface[esort[i].i].nr     ;
    iface2[i].nod[4]  =iface[esort[i].i].nod[4] ;
    iface2[i].nod[5]  =iface[esort[i].i].nod[5] ;
    for(j=0; j<4; j++) iface2[i].nod[j]=iface[esort[i].i].nod[j];
  }
  free(iface);
  free(esort);

  *ptr=iface2;

  nf[0]=nif; nf[1]=nof;
  return(nf);
}
