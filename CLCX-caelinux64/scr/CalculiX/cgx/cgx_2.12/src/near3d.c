#include <extUtil.h>

typedef struct {
  double r;
  int i;
}Rsort;

int compareRsort(Rsort *a, Rsort *b);

/*
!     identifies the position id of px in an ordered array
!     x of real numbers; 
!  
!     id is such that x(id).le.px and x(id+1).gt.px
!                                                                             
*/
int ident(x,px,n)
int n;
double *x,px;
{
  int id=0, n2, m;
  if(n == 0) return(0);

  /* Parameter adjustments */
  --x;

  n2=n+1;
  do
  {                                                        
    m=(n2+id)/2;
    if(px >= x[m]) { id=m; }   
    else { n2=m; }    
    if((n2-id) == 1) return(id);
  }while(1);
}


/*
c     determines the k closest nodes out of n with coordinates in
c     xo,yo,zo to the point with coordinates (xp,yp,zp);
c
c     order xo, yo and zo before the first call to near2d and
c     store the results with the corresponding permutation array
c     in x,y,z and nx,ny,nz, respectively
*/
/*
C   DSORT sorts array DX and optionally makes the same interchanges in
C   array IY.  The array DX may be sorted in increasing order or
C   decreasing order.  A slightly modified quicksort algorithm is used.
C
C      SUBROUTINE DSORT (DX, IY, N, KFLAG)
C   Description of Parameters
C      DX - array of values to be sorted   (usually abscissas)
C      IY - array to be (optionally) carried along
C      N  - number of values in array DX to be sorted
C      KFLAG - control parameter
C            =  2  means sort DX in increasing order and carry IY along.
C            =  1  means sort DX in increasing order (ignoring IY)
C            = -1  means sort DX in decreasing order (ignoring IY)
C            = -2  means sort DX in decreasing order and carry IY along.
*/

void near3d(xo,yo,zo,x,y,z,nx,ny,nz,xp,yp,zp,n,node,k)
int *nx,*ny,*nz,n,*node,k;
double *xo,*yo,*zo,*x,*y,*z,xp,yp,zp;
{
  int  i,j,m,ks,it,iz,idx,idy,idz,nboundary;     
  double aad,aanod,aamin,xr,yr,zr;
  double xx,yy,zz,rr,aaw,aao,aas,aan,aau;
  static Rsort *rsort=NULL;

  int kindx;

  if(k>n) k=n;
  //if(k>100) k=100;
  kindx=k-1;

  idx=ident(x,xp,n);
  idy=ident(y,yp,n);
  idz=ident(z,zp,n);
  /* according to original fortran idx etc for a field starting at '1' */
/*
  printf("ident: %d %d %d\n", idx,idy,idz);
  printf("1: %f %f %f\n", x[idx-1],y[idy-1],z[idz-1]);
  printf("p: %f %f %f\n", xp,yp,zp);
  printf("2: %f %f %f\n", x[idx],y[idy],z[idz]);
*/

  /*  DETERMINATION OF THE k NEAREST POINTS */
  if ( (rsort = (Rsort *)realloc((Rsort *)rsort, (n+1) * sizeof(Rsort))) == NULL )
          printf("ERROR: realloc failed: Rsort\n\n" );

  /* abs dist from p to o */
  for(i=0; i<k; i++)
  {
    xr=xo[i]-xp;
    yr=yo[i]-yp;
    zr=zo[i]-zp;
    rsort[i].r=sqrt(xr*xr+yr*yr+zr*zr);
    rsort[i].i=i;
  }
  qsort( rsort, k, sizeof(Rsort), (void *)compareRsort );

  i=1;  // nr of loops?
  ks=0;

  loop:;

  iz=0;

//     west

  m=idx-i;      // closest xo to xp - nr_of_loops +1 == one point above xp
  if(m<0)         // goto "east" if no lower point is there
  {
    aaw=rsort[kindx].r;     // k has to be reduced by 1 (field runs to k-1)
    goto l4;
  }
  xx=xo[nx[m]];   // coorinates of point o above p in dir x
  yy=yo[nx[m]];
  zz=zo[nx[m]];
  aaw=xx-xp;        // x distance
  yr=yy-yp;
  zr=zz-zp;
  rr=sqrt(aaw*aaw+yr*yr+zr*zr);  // abs distance

  if(rr >= rsort[kindx].r) goto l4;  // if abs_dist >= "k"closest distance (k has to be reduced by 1 (field runs to k-1))
  it=iz+k;
  for( j=0; j<it; j++) if(nx[m] == rsort[j].i) goto l4;
  //printf("we %d %d i:%d r:%f\n", i,iz,nx[m],rr);
  rsort[iz+k].r=rr;
  rsort[iz+k].i=nx[m];
  iz++;

//     east
l4:;

  m=idx+i;
  if(m > n)
  {
      aao=rsort[kindx].r;
      goto l6;
  }
  m--;
  xx=xo[nx[m]];
  yy=yo[nx[m]];
  zz=zo[nx[m]];
  aao=xx-xp;
  yr=yy-yp;
  zr=zz-zp;
  rr=sqrt(aao*aao+yr*yr+zr*zr);

  if(rr >= rsort[kindx].r) goto l6;
  it=iz+k;
  for( j=0; j<it; j++) if(nx[m] == rsort[j].i) goto l6;
  //printf("ea %d %d i:%d r:%f\n", i,iz,nx[m],rr);
  rsort[iz+k].r=rr;
  rsort[iz+k].i=nx[m];
  iz++;

//     south
l6:;

  m=idy-i;
  if(m < 0)
  {
      aas=rsort[kindx].r;
      goto l5;
  }

  xx=xo[ny[m]];
  yy=yo[ny[m]];
  zz=zo[ny[m]];
  xr=xx-xp;
  aas=yy-yp;
  zr=zz-zp;
  rr=sqrt(xr*xr+aas*aas+zr*zr);

  if(rr >= rsort[kindx].r) goto l5;
  it=iz+k;
  for( j=0; j<it; j++) if(ny[m] == rsort[j].i) goto l5;
  //printf("so %d %d i:%d r:%f\n", i,iz,ny[m],rr);
  rsort[iz+k].r=rr;
  rsort[iz+k].i=ny[m];
  iz++;

//     north
l5:;

  m=idy+i;
  if(m>n)
  {
    aan=rsort[kindx].r;
    goto l7;
  }
  m--;
  xx=xo[ny[m]];
  yy=yo[ny[m]];
  zz=zo[ny[m]];
  xr=xx-xp;
  aan=yy-yp;
  zr=zz-zp;
  rr=sqrt(xr*xr+aan*aan+zr*zr);

  if(rr >= rsort[kindx].r) goto l7;
  it=iz+k;
  for(j=0; j<it; j++) if(ny[m] == rsort[j].i) goto l7;
  //printf("no %d %d i:%d r:%f\n", i,iz,ny[m],rr);
  rsort[iz+k].r=rr;
  rsort[iz+k].i=ny[m];
  iz++;

//     up
l7:;

  m=idz-i;
  if(m < 0)
  {
    aau=rsort[kindx].r;
    goto l20;
  }

  xx=xo[nz[m]];
  yy=yo[nz[m]];
  zz=zo[nz[m]];
  xr=xx-xp;
  yr=yy-yp;
  aau=zz-zp;
  rr=sqrt(xr*xr+yr*yr+aau*aau);

  if(rr >= rsort[kindx].r) goto l20;
  it=iz+k;
  for(j=0; j<it; j++) if(nz[m] == rsort[j].i) goto l20;
  //printf("up %d %d i:%d r:%f\n", i,iz,nz[m],rr);
  rsort[iz+k].r=rr;
  rsort[iz+k].i=nz[m];
  iz++;

//     down
l20:;

  m=idz+i;
  if(m > n)
  {
      aad=rsort[kindx].r;
      goto l22;
  }
  m--;
  xx=xo[nz[m]];
  yy=yo[nz[m]];
  zz=zo[nz[m]];
  xr=xx-xp;
  yr=yy-yp;
  aad=zz-zp;
  rr=sqrt(xr*xr+yr*yr+aad*aad);

  if(rr >=rsort[kindx].r) goto l22;
  it=iz+k;
  for(j=0; j<it; j++) if(nz[m] == rsort[j].i) goto l22;
  //printf("do %d %d i:%d r:%f\n", i,iz,nz[m],rr);
  rsort[iz+k].r=rr;
  rsort[iz+k].i=nz[m];
  iz++;

l22:;

  aamin=MAX_FLOAT;
  aanod=sqrt(aan*aan+aao*aao+aad*aad);
  if(aanod<aamin) aamin=aanod;
  aanod=sqrt(aan*aan+aao*aao+aau*aau);
  if(aanod<aamin) aamin=aanod;
  aanod=sqrt(aas*aas+aao*aao+aad*aad);
  if(aanod<aamin) aamin=aanod;
  aanod=sqrt(aas*aas+aao*aao+aau*aau);
  if(aanod<aamin) aamin=aanod;
  aanod=sqrt(aas*aas+aaw*aaw+aad*aad);
  if(aanod<aamin) aamin=aanod;
  aanod=sqrt(aas*aas+aaw*aaw+aau*aau);
  if(aanod<aamin) aamin=aanod;
  aanod=sqrt(aan*aan+aaw*aaw+aad*aad);
  if(aanod<aamin) aamin=aanod;
  aanod=sqrt(aan*aan+aaw*aaw+aau*aau);
  if(aanod<aamin) aamin=aanod;

  if(iz != 0)
  {
      iz+=k;
      qsort( rsort, iz, sizeof(Rsort), (void *)compareRsort );
  }


  nboundary=ks;
  for( j=nboundary; j<k; j++)
  {
      if(rsort[j].r <= aamin)
      {
          ks=j+1;
          if(ks==k) goto endloop;
      }
      else
      {
          i++;
          goto loop;
      }
  }


  goto loop;
endloop:;

  //for(i=0; i<k; i++) printf("n:%d r:%f orig: %f %f %f p:%f %f %f\n",rsort[i].i,rsort[i].r, xo[rsort[i].i],yo[rsort[i].i],zo[rsort[i].i], xp,yp,zp);
  for(i=0; i<k; i++) node[i]=rsort[i].i;
  return;
}


