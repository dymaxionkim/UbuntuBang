/* ---------------------------------------------------------------- */
/* interpoliert zwischen x(i) und x(i-1)                            */
/* es wird auch  extrapoliert.                                      */
/* method=0 lineare interpol, method=1 spline interpol              */
/* x0: Stelle auf die interpoliert wird                             */
/* y0: Rueckgabewert                                                */
/* wenn extrapol. wurde ist method nach der ausfuehrung=1, sonst 0  */
/* ---------------------------------------------------------------- */
#include <extUtil.h>




double intpol2(double *x, double *y, int n, double x0, int *method )
{

      int       imin, imax, i,j, isort[PNTS];
      double     y0, ysort[PNTS];
      double     dx,dy,dx0,xsort[PNTS];

  double *c[5];

  if (n>PNTS)
  {
    errMsg ("ERROR: to much Data:%d, increase PNTS in intpol2\n", n);
    exit(-1);
  }

  /* Daten nach aufsteigendem x sortieren und doppelte auslassen */
  bsort(x, xsort, isort, n );
  imax=n-1;
  xsort[0]=x[isort[imax]];
  ysort[0]=y[isort[imax]];
  i=1;
  for (j=1; j<n; j++)
  {
    if(x[isort[imax-j]]!=xsort[i-1])
    { 
      xsort[i]=x[isort[imax-j]];
      ysort[i]=y[isort[imax-j]];
      i++;
    }
  }
  n=i;
  imax=n-1;

  if ((*method==0)||(n<3))       /* lineare interpolation */
  {
      n--;
      if (x0 <= xsort[0])
      {
        *method=1;
        imin = 0;
        imax = 1;
      }
      else if (x0 >= xsort[n])
      {
        *method=1;
        imin = n-1;
        imax = n;
      }
      else
      {
        *method=0;
        imax=0;
        while (xsort[imax]<x0) imax++;
        imin=imax-1;
      }

      /*     *** lineare interpolation   ************    */

      dx=xsort[imax]-xsort[imin];
      dy=ysort[imax]-ysort[imin];
      dx0=x0-xsort[imin];
      y0=ysort[imin]+dx0*dy/dx;
  }
  else
  {
    if ((x0 < xsort[0])||(x0 > xsort[n-1])) *method=1;
    else                            *method=0;

      for (imin=0;imin<5 ;imin++ )
      {
        c[imin]= (double *) malloc( PNTS*sizeof(double));
      }

      for (j=0;j<n ;j++ )
      {
        c[0][j]=xsort[j];  /* x values */
        c[1][j]=ysort[j];  /* y values */
      }

      createSpline(  n, c);
      y0=spline_int( n, x0, c );

      for (imin=0;imin<5 ;imin++ )
      {
        free(c[imin]);
      }
  }

  /*
  for (imin=0;imin<n ;imin++ )
  {
    printf("x0:%lf y0:%lf xsort:%lf ysort:%lf\n", x0,y0,xsort[imin],ysort[imin] );
  }
  */

  return(y0);
}

