#include <extUtil.h>

double spline_int( int nc, double xneu, double **c )
/*********************************************************/
/* Berechnung der Schnittpunkte mit einem Spline         */
/*                                                       */
/* nc: Stuetzpunkte                                      */
/* xneu :Schnittpunkt x-koordinate                       */
/* c : Koeffizienten aus create_spline                   */
/*********************************************************/
{
  int    i;
  double  dx,yneu=0.;



      if ((xneu >  c[0][0]) &&  (xneu <= c[0][nc-1]))
      {
        for ( i=1; i<nc; i++ )
          if (xneu <  c[0][i]) break;
        i=i-1;
        dx=xneu-c[0][i];
        yneu=c[1][i]+c[2][i]*dx+c[3][i]*dx*dx+c[4][i]*dx*dx*dx;
      }
      else if (xneu <= c[0][0])
      {
        dx=xneu-c[0][0];
        yneu=c[1][0]+c[2][0]*dx+c[3][0]*dx*dx;
      }
      else if (xneu > c[0][nc-1])
      {
        dx=xneu-c[0][nc-1];
        yneu=c[1][nc-1]+c[2][nc-2]*dx+c[3][nc-2]*dx*dx;
      }

      return(yneu);
}


void createSpline( int n, double **c)
/*********************************************************/
/* Berechnung der Spline Funktion Y=f(X)                 */
/*                                                       */
/* Y = c2 + c3*dx + c4*dx*dx + c5*dx*dx*dx               */
/*     dx = X - c1                                       */
/*                                                       */
/*                                                       */
/* n:Anzahl Stuetzpunkte                                 */
/* c:Feld mit 5*n eintraegen c(0,0)=x(0)     Eingabe     */
/*                           c(0,2)=x(2)        "        */
/*                           c(0,0)=y(0) usw.   "        */
/*                                                       */
/*                           c(2,0)=c3                   */
/*                                                       */
/*                                                       */
/*********************************************************/
{
  int nn,j;
  double dyy,xj,yj,xx,yq,yy,yhx,yhj,yh[PNTS];


  if (n>PNTS)
  {
    errMsg ("ERROR: to much Data:%d, increase PNTS in createSpline\n", n);
    exit(-1);
  }
    nn=n-1;
    yh[0]=(c[1][1]-c[1][0])/(c[0][1]-c[0][0]);
    yh[nn]=(c[1][nn]-c[1][nn-1])/(c[0][nn]-c[0][nn-1]);

    for( j=1;j<nn;j++ )
    {
       dyy=c[1][j+1]-c[1][j-1];
       if (c[0][j+1]==c[0][j])
       {
        dyy=c[1][j]-c[1][j-1];
       }
       if (c[0][j-1]==c[0][j])
       {
        dyy=c[1][j+1]-c[1][j];
       }
       yh[j]=dyy/(c[0][j+1]-c[0][j-1]);
    }

    for( j=0;j<nn;j++ )
    {
       c[2][j]=yh[j];
       xj=c[0][j+1]-c[0][j];
       if (xj==0.)
       {
        c[2][j]=(c[1][j+1]-c[1][j])*1.e9;
        c[3][j]=0.;
        c[4][j]=0.;
        goto ende;
       }
       yj=c[1][j+1]-c[1][j];
       yhx=yh[j]*xj;
       yhj=yh[j+1]*xj;
       c[3][j]=(3.*yj-2.*yhx-yhj)/(xj*xj);
       c[4][j]=-(2.*yj-yhx-yhj)/(xj*xj*xj);
       if ((j>0)&&(j<nn-1))
       {
        if (c[4][j]==0.) goto ende;
        xx=2.*c[3][j]/(-6.*c[2][j]);
        if ((xx>=xj)||(xx<=0.)) goto ende;
        yq=yj/xj;
        yy=(yh[j]-yq)*(yh[j+1]-yq);
        if (yy>=0.) goto ende;
       }
       c[4][j]=0.;
       if ( abs(yh[j+1]) >= abs(yh[j]) )
       {
        c[3][j]=(yj-yhx)/(xj*xj);
        goto ende;
       }
       c[3][j]=-(yj-yhj)/(xj*xj);
       c[2][j]=yh[j+1]-2.*c[3][j]*xj;
    ende:;
    }

}
