
#include <extUtil.h>

#define MAXVALUE 2147483647


int renumberfrd( int firstelem, int firstnode, Summen *anz, Nodes **nptr, Elements **eptr, Datasets **lptr, int **enew_ptr, int **nnew_ptr )
{
  static Nodes     *node;
  static Elements  *elem;
  static Datasets *lcase;

  int  i,n, ipuf;
  int nlc, ncmp;
  int *newnode, *newelem;
  double *newnode_x, *newnode_y, *newnode_z, *dat;

  node=*nptr;
  elem=*eptr;
  lcase=*lptr;

  if ( (newnode = (int *)malloc( (anz->nmax+1)*sizeof(int) )) == NULL )
  { printf("\n\n ERROR: malloc failed in renumberfrd()\n\n") ; return(-1); }
  for(i=0; i<=anz->nmax; i++) newnode[i]=0;
  if ( (newelem = (int *)malloc( (anz->emax+1)*sizeof(int) )) == NULL )
  { printf("\n\n ERROR: malloc failed in renumberfrd()\n\n") ; return(-1); }
  for(i=0; i<=anz->emax; i++) newelem[i]=0;
  if ( (dat = (double *)malloc( (anz->nmax+firstnode)*sizeof(double) )) == NULL )
  { printf("\n\n ERROR: malloc failed in renumberfrd()\n\n") ; return(-1); }
  if ( (newnode_x = (double *)malloc( (anz->nmax+1)*sizeof(double) )) == NULL )
  { printf("\n\n ERROR: malloc failed in renumberfrd()\n\n") ; return(-1); }
  if ( (newnode_y = (double *)malloc( (anz->nmax+1)*sizeof(double) )) == NULL )
  { printf("\n\n ERROR: malloc failed in renumberfrd()\n\n") ; return(-1); }
  if ( (newnode_z = (double *)malloc( (anz->nmax+1)*sizeof(double) )) == NULL )
  { printf("\n\n ERROR: malloc failed in renumberfrd()\n\n") ; return(-1); }

  if (anz->l > 0)
  {
    for (nlc=0; nlc< anz->l; nlc++)
    {
      /* check if the data of the specified lcase (Dataset) are already available */
      if (!lcase[nlc].loaded)
      {
        if( readfrdblock(nlc, anz, node, lcase )==-1) 
        {
          printf("ERROR: Could not read data for Dataset:%d\n", nlc+1); 
          return(-1);
        }
      }
    }
  }

  if (anz->n > 0)
  {
    anz->nmax=-MAXVALUE;  anz->nmin= MAXVALUE;
    for (i=0; i<anz->n; i++)
    { 
      newnode[node[i].nr]=i+firstnode;
      newnode_x[node[i].nr]=node[node[i].nr].nx;
      newnode_y[node[i].nr]=node[node[i].nr].ny;
      newnode_z[node[i].nr]=node[node[i].nr].nz;
      if (newnode[node[i].nr] >  anz->nmax)  anz->nmax=newnode[node[i].nr];
      if (newnode[node[i].nr] <  anz->nmin)  anz->nmin=newnode[node[i].nr];
    }
  }

  if (anz->e > 0)
  {
    anz->emax=-MAXVALUE;  anz->emin=MAXVALUE;
    for (i=0; i<anz->e; i++)
    {
      ipuf=0;
      if (elem[i].type == 1) ipuf = 8;  /* CHEXA8  */
      if (elem[i].type == 2) ipuf = 6;  /* CPENTA6   */
      if (elem[i].type == 3) ipuf = 4;  /* CTET4   */
      if (elem[i].type == 4) ipuf = 20; /* CHEXA20 */
      if (elem[i].type == 5) ipuf = 15; /* CPENTA15  */
      if (elem[i].type == 6) ipuf = 10; /* CTET10  */
      if (elem[i].type == 7) ipuf = 3;  /* CTRI3   */
      if (elem[i].type == 8) ipuf = 6;  /* CTRI6   */
      if (elem[i].type == 9) ipuf = 4;  /* CQUAD4  */
      if (elem[i].type == 10) ipuf = 8; /* CQUAD8  */
      if (elem[i].type == 11) ipuf = 2;  /* CBEAM2  */
      if (elem[i].type == 12) ipuf = 3;  /* CBEAM3  */
      if (ipuf==0)
      {
        printf (" elem(%d) not a known type (%d)\n", elem[i].nr, elem[i].type);
      }
      else
      {
        newelem[elem[i].nr]=i+firstelem;
        elem[i].nr=i+firstelem;
        if (elem[i].nr >  anz->emax)  anz->emax=elem[i].nr;
        if (elem[i].nr <  anz->emin)  anz->emin=elem[i].nr;
        for (n=0; n<ipuf; n++)
        {
          elem[i].nod[n]=newnode[elem[i].nod[n]];
        } 
      }
    }
  }
  if (anz->l > 0)
  {
    for (nlc=0; nlc< anz->l; nlc++)
    {
      for (ncmp=0; ncmp<lcase[nlc].ncomps; ncmp++ )
      {
        for (i=0; i<anz->n; i++)
        {
          dat[node[i].nr]=lcase[nlc].dat[ncmp][node[i].nr] ;
        }
        if ( (lcase[nlc].dat[ncmp] = (float *)realloc( (float *)lcase[nlc].dat[ncmp] ,(anz->nmax+1) * sizeof(float))) == NULL )
        {
          printf("\n\n ERROR: realloc failed\n\n" );
          return(-1);
        }
        for (i=0; i<=anz->nmax; i++)
        {
          lcase[nlc].dat[ncmp][i]=0.;
        }
        for (i=0; i<anz->n; i++)
        {
          lcase[nlc].dat[ncmp][newnode[node[i].nr]]=dat[node[i].nr];
        }
      } 
    }
  }
  if (anz->n > 0)
  {
    for (i=0; i<anz->n; i++)
    {
      if ( (node = (Nodes *)realloc((Nodes *)node, (anz->nmax+1) * sizeof(Nodes))) == NULL )
      {
        printf("\n\n ERROR: realloc failed, nodenr\n") ;
        return(-1);
      }
      node[newnode[node[i].nr]].nx=newnode_x[node[i].nr];
      node[newnode[node[i].nr]].ny=newnode_y[node[i].nr];
      node[newnode[node[i].nr]].nz=newnode_z[node[i].nr];
      node[i].nr=newnode[node[i].nr];
    }
  }

  *enew_ptr=newelem;
  *nnew_ptr=newnode;
  *nptr =  node; *eptr = elem; *lptr = lcase;

  free(newnode_x);
  free(newnode_y);
  free(newnode_z);
  free(dat);
  return (1);
}

