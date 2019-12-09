/* ---------------------------------------------------------------- */
/* loest gl 0=x**3+ax**2+bx+c gl3grades nach Bronstein S.118        */
/* ACHTUNG: nur fuer eigenwertsuche symmetrischer matrizen          */
/* ---------------------------------------------------------------- */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#define     pi          3.14159265358979323846264338327950288

int gl3grades( long double a, long double b, long double c, double *x)
{
  long double p,q,phi;

  p= (b - a*a/3.)/3.;
  q= (a*a*a/13.5 - a*b/3. + c)*.5;
  
  p= sqrt(fabs(p))*fabs(q)/q;
  phi=  q /(p*p*p) ;
  if(phi>1.0) phi=0.; else if (phi<-1.0) phi=pi;
  else phi = acos( phi );
  
  x[0]= -2*p*cos(phi/3.)  -a/3.;
  x[1]=  2*p*cos(pi/3.- phi/3.)  -a/3.;
  x[2]=  2*p*cos(pi/3.+ phi/3.)  -a/3.;
  
  //printf ("  gl3D.2  p2=%lf phi=%lf p0:%lf p1:%lf p2:%lf \n",(double)p,(double)phi,x[0],x[1],x[2]);
  
  return (1);
}



