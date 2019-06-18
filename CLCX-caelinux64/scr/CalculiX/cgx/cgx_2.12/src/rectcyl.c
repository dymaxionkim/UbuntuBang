#include <extUtil.h>

/*---------------------------------------------------------------------*/
/* used to transform vectors (disp) or tensors (stress) from/to cyl    */
/*---------------------------------------------------------------------*/
#define     MIN_DIST    1.e-10

void transformatrix( double *xab, double *p, double **a)
{
  int i;
  double e1[3], e2[3], e3[3], dd;

  e1[0]= p[0] - xab[0];
  e1[1]= p[1] - xab[1];
  e1[2]= p[2] - xab[2];

  e3[0]=  xab[3] - xab[0];
  e3[1]=  xab[4] - xab[1];
  e3[2]=  xab[5] - xab[2];


  dd = sqrt( e3[0]*e3[0]+e3[1]*e3[1]+e3[2]*e3[2]);

  for(i=0; i<3; i++) e3[i]/=dd;

  dd =  e1[0]*e3[0]+e1[1]*e3[1]+e1[2]*e3[2] ;

  for(i=0; i<3; i++) e1[i]=e1[i]-dd*e3[i];

  dd = sqrt( e1[0]*e1[0]+e1[1]*e1[1]+e1[2]*e1[2]);

  /* check if p belongs to the cylindrical axis */

  if(dd<MIN_DIST)
  {
    if( abs(e3[0])>MIN_DIST )
    {
      e1[1]=1.;
      e1[2]=0.;
      e1[0]=-e3[1]/e3[0];
    }
    else if( abs(e3[1])>MIN_DIST )
    {
      e1[2]=1.;
      e1[0]=0.;
      e1[1]=-e3[2]/e3[1];
    }
    else
    {
      e1[0]=1.;
      e1[1]=0.;
      e1[2]=-e3[0]/e3[2];
    }
    dd=sqrt( e1[0]*e1[0]+e1[1]*e1[1]+e1[2]*e1[2]);
  }

  for(i=0; i<3; i++) e1[i]/=dd;

  e2[0]=e3[1]*e1[2]-e1[1]*e3[2];
  e2[1]=e3[2]*e1[0]-e1[2]*e3[0];
  e2[2]=e3[0]*e1[1]-e1[0]*e3[1];

  for(i=0; i<3; i++)
  {
    a[i][0]=e1[i];
    a[i][1]=e2[i];
    a[i][2]=e3[i];
  }  
}


void cartcyl( double *csab, int node, double *node_pos, Datasets *lcase, int lc, char type )
{ 
  int i;
  double r,t,z, b[3][3];
  double *a[3], af[3][3];
  for(i=0; i<3; i++) a[i]=&af[i][0];

  transformatrix( csab, node_pos, a);
  // printf("a:%f %f %f\n",a[0][2],a[1][2],a[2][2] );

  if (type=='d')
  {
      /* Displacements */
      r= lcase[lc].dat[0][node] *a[0][0] + lcase[lc].dat[1][node] *a[1][0] + lcase[lc].dat[2][node] *a[2][0];
      t= lcase[lc].dat[0][node] *a[0][1] + lcase[lc].dat[1][node] *a[1][1] + lcase[lc].dat[2][node] *a[2][1];
      z= lcase[lc].dat[0][node] *a[0][2] + lcase[lc].dat[1][node] *a[1][2] + lcase[lc].dat[2][node] *a[2][2];
      lcase[lc].dat[0][node]=r;
      lcase[lc].dat[1][node]=t;
      lcase[lc].dat[2][node]=z;
  }
  else if (type=='s')
  {
      /* Stresses/ copy from ccx */
      /* due to other stress component order in ccx as in cgx the component 5 is really 4 and 4 is really 5 */

      b[0][0]= lcase[lc].dat[0][node] *a[0][0] +lcase[lc].dat[3][node] *a[1][0] +lcase[lc].dat[5][node] *a[2][0];
      b[0][1]= lcase[lc].dat[0][node] *a[0][1] +lcase[lc].dat[3][node] *a[1][1] +lcase[lc].dat[5][node] *a[2][1];
      b[0][2]= lcase[lc].dat[0][node] *a[0][2] +lcase[lc].dat[3][node] *a[1][2] +lcase[lc].dat[5][node] *a[2][2];
      b[1][0]= lcase[lc].dat[3][node] *a[0][0] +lcase[lc].dat[1][node] *a[1][0] +lcase[lc].dat[4][node] *a[2][0];
      b[1][1]= lcase[lc].dat[3][node] *a[0][1] +lcase[lc].dat[1][node] *a[1][1] +lcase[lc].dat[4][node] *a[2][1];
      b[1][2]= lcase[lc].dat[3][node] *a[0][2] +lcase[lc].dat[1][node] *a[1][2] +lcase[lc].dat[4][node] *a[2][2];
      b[2][0]= lcase[lc].dat[5][node] *a[0][0] +lcase[lc].dat[4][node] *a[1][0] +lcase[lc].dat[2][node] *a[2][0];
      b[2][1]= lcase[lc].dat[5][node] *a[0][1] +lcase[lc].dat[4][node] *a[1][1] +lcase[lc].dat[2][node] *a[2][1];
      b[2][2]= lcase[lc].dat[5][node] *a[0][2] +lcase[lc].dat[4][node] *a[1][2] +lcase[lc].dat[2][node] *a[2][2];
 
      lcase[lc].dat[0][node]=a[0][0]*b[0][0] +a[1][0]*b[1][0] +a[2][0]*b[2][0];
      lcase[lc].dat[1][node]=a[0][1]*b[0][1] +a[1][1]*b[1][1] +a[2][1]*b[2][1];
      lcase[lc].dat[2][node]=a[0][2]*b[0][2] +a[1][2]*b[1][2] +a[2][2]*b[2][2];
      lcase[lc].dat[3][node]=a[0][0]*b[0][1] +a[1][0]*b[1][1] +a[2][0]*b[2][1];
      lcase[lc].dat[5][node]=a[0][0]*b[0][2] +a[1][0]*b[1][2] +a[2][0]*b[2][2];
      lcase[lc].dat[4][node]=a[0][1]*b[0][2] +a[1][1]*b[1][2] +a[2][1]*b[2][2];
  }
}

void cylcart( double *csab, int node, double *node_pos, Datasets *lcase, int lc, char type )
{
  int i;
  double r,t,z, b[3][3];
  double *a[3], af[3][3];
  for(i=0; i<3; i++) a[i]=&af[i][0];

  transformatrix( csab, node_pos, a);

  if (type=='d')
  {
      /* Displacements */
      r= lcase[lc].dat[0][node] *a[0][0] + lcase[lc].dat[1][node] *a[0][1] + lcase[lc].dat[2][node] *a[0][2];
      t= lcase[lc].dat[0][node] *a[1][0] + lcase[lc].dat[1][node] *a[1][1] + lcase[lc].dat[2][node] *a[1][2];
      z= lcase[lc].dat[0][node] *a[2][0] + lcase[lc].dat[1][node] *a[2][1] + lcase[lc].dat[2][node] *a[2][2];
      lcase[lc].dat[0][node]=r;
      lcase[lc].dat[1][node]=t;
      lcase[lc].dat[2][node]=z;
  }
  else if (type=='s')
  {
      /* Stresses */
      b[0][0]= lcase[lc].dat[0][node] *a[0][0] +lcase[lc].dat[3][node] *a[0][1] +lcase[lc].dat[5][node] *a[0][2];
      b[0][1]= lcase[lc].dat[0][node] *a[1][0] +lcase[lc].dat[3][node] *a[1][1] +lcase[lc].dat[5][node] *a[1][2];
      b[0][2]= lcase[lc].dat[0][node] *a[2][0] +lcase[lc].dat[3][node] *a[2][1] +lcase[lc].dat[5][node] *a[2][2];
      b[1][0]= lcase[lc].dat[3][node] *a[0][0] +lcase[lc].dat[1][node] *a[0][1] +lcase[lc].dat[4][node] *a[0][2];
      b[1][1]= lcase[lc].dat[3][node] *a[1][0] +lcase[lc].dat[1][node] *a[1][1] +lcase[lc].dat[4][node] *a[1][2];
      b[1][2]= lcase[lc].dat[3][node] *a[2][0] +lcase[lc].dat[1][node] *a[2][1] +lcase[lc].dat[4][node] *a[2][2];
      b[2][0]= lcase[lc].dat[5][node] *a[0][0] +lcase[lc].dat[4][node] *a[0][1] +lcase[lc].dat[2][node] *a[0][2];
      b[2][1]= lcase[lc].dat[5][node] *a[1][0] +lcase[lc].dat[4][node] *a[1][1] +lcase[lc].dat[2][node] *a[1][2];
      b[2][2]= lcase[lc].dat[5][node] *a[2][0] +lcase[lc].dat[4][node] *a[2][1] +lcase[lc].dat[2][node] *a[2][2];

      lcase[lc].dat[0][node]=a[0][0]*b[0][0] +a[0][1]*b[1][0] +a[0][2]*b[2][0];
      lcase[lc].dat[1][node]=a[1][0]*b[0][1] +a[1][1]*b[1][1] +a[1][2]*b[2][1];
      lcase[lc].dat[2][node]=a[2][0]*b[0][2] +a[2][1]*b[1][2] +a[2][2]*b[2][2];
      lcase[lc].dat[3][node]=a[0][0]*b[0][1] +a[0][1]*b[1][1] +a[0][2]*b[2][1];
      lcase[lc].dat[5][node]=a[0][0]*b[0][2] +a[0][1]*b[1][2] +a[0][2]*b[2][2];
      lcase[lc].dat[4][node]=a[1][0]*b[0][2] +a[1][1]*b[1][2] +a[1][2]*b[2][2];
  }
}



void rectcyl(int icntrl, double *csab, int nr, Nodes *node, Datasets *lcase, int lc, char type)
{
  int i;

  /* if icntrl ==+1 transformation 1 node from cartesian into cylindrical system */
  /* if icntrl ==-1 transformation 1 node from cylindrical into cartesian system */
  /* if icntrl ==+2 transformation all nodes from cartesian into cylindrical system */
  /* if icntrl ==-2 transformation all nodes from cylindrical into cartesian system */

  if     (icntrl==1)  cartcyl(csab, nr, &node[nr].nx, lcase, lc, type ); 
  else if(icntrl==2)  for (i=0; i<nr; i++ ) cartcyl(csab, node[i].nr, &node[node[i].nr].nx, lcase, lc, type ); 
  else if(icntrl==-1) cylcart(csab, nr, &node[nr].nx, lcase, lc, type ); 
  else if(icntrl==-2) for (i=0; i<nr; i++ ) cylcart(csab, node[i].nr, &node[node[i].nr].nx, lcase, lc, type ); 
  else
  {
    printf("ERROR: icntrl:%d not implemented \n", icntrl);
    exit(-1);
  }
}
