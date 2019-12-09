/* ---------------------------------------------------------------- */
/* berechnet die lauflaenge von punkt zu punkt und summiert         */
/* diese auf (s).                                                   */
/* ---------------------------------------------------------------- */
#include <extUtil.h>



void linelength(double *x, double *y, double *z, int n, double *s )
{
  register int i, j;
  double p0[3], p1[3], v01[3], ds;

  j=0;
  s[j]=0.;
  for (i=0; i<n-1; i++)
  {
    j++;
    p0[0]=x[i];
    p0[1]=y[i];
    p0[2]=z[i];
    p1[0]=x[j];
    p1[1]=y[j];
    p1[2]=z[j];
    v_result( p0, p1, v01 );
    ds=v_betrag( v01 );
    s[j]=s[i]+ds;
  }
}

