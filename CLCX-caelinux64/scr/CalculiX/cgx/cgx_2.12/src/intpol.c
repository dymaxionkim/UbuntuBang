/* ---------------------------------------------------------------- */
/* intpol linear */
/* ---------------------------------------------------------------- */
#include <extUtil.h>

double intpol(double *x, double *y, int n, double x0 )
{
  int i,ii,m,n1,n2;

  /***** check if x is outside the function range ***********/
  if (x0<=x[0])
  {
    /*    printf("outside %lf<%lf %lf %lf\n", x0, x[0], y[0], y[0]+(x0-x[0])/(x[1]-x[0])*(y[1]-y[0]) ); */
    return( y[0]+(x0-x[0])/(x[1]-x[0])*(y[1]-y[0]) );
  }
  else if(x0>=x[n-1])
  {
    /*    printf("outside %lf>%lf %lf %lf \n", x0, x[n-1], y[n-1], y[n-1]+(x0-x[n-1])/(x[n-1]-x[n-2])*(y[n-1]-y[n-2])); */
    return( y[n-1]+(x0-x[n-1])/(x[n-1]-x[n-2])*(y[n-1]-y[n-2]) );
  }

  /****************** BEGIN HALBSCHRITT **********************/
  n1=0;                              
  n2=n;                            
  for(ii=0; ii<n; ii++)
  {                     
    m=(n2+n1)/2;                      
    if(x0>= x[m] ) n1=m;              
    if(x0 < x[m] ) n2=m;              
    if((n2-n1) == 1) break;           
  }                                 
  i=n1;
  /* printf("i:%d x:%lf xi:%lf\n", i,x0,x[i]); */  
  /****************** ENDE HALBSCHRITT **********************/

  if (x0 <= x[i]) {
    return( y[i-1]+(x0-x[i-1])/(x[i]-x[i-1])*(y[i]-y[i-1]) ); }
  else {
    return( y[i]+(x0-x[i])/(x[i+1]-x[i])*(y[i+1]-y[i]) );
  }
}
