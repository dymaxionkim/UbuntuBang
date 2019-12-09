#include <extUtil.h>

/* angle between two vectors in rad, en is reference */
double v_angle_ref( double *v0, double *v1, double *en )
{
  double vn0[3], vn1[3], v0v1[3];
  double fi;

  v_norm(  v0, vn0 ); 
  v_norm(  v1, vn1 ); 

  fi=v_sprod( vn0, vn1 );
  if(fi>1.) fi=1.;
  if(fi<-1.) fi=-1.;
  fi=acos(fi);

  /* fi > PI if (v0 x v1)*en < 0. (check if the crosprod is in dir of en, else its more than pi) */
  v_prod( v0, v1, v0v1);
  if(v_sprod( v0v1, en) < 0.) fi=2.*PI-fi;
  /* printf("a:%lf v1:%lf %lf %lf v2:%lf %lf %lf\n", fi*180./pi, v0[0], v0[1], v0[2], v1[0], v1[1], v1[2] ); */
  return(fi);
}
