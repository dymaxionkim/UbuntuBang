#include <extUtil.h>

/* angle in the xy plane between the x andy axis */

double p_angle(double x, double y)
{
  if((x>0.) && (y>=0.)) return(atan(y/x));
  else if((y>0.) && (x<=0.)) return(atan(-x/y))+PI*.5;
  else if((x<0.) && (y<=0.)) return(atan(-y/-x))+PI;
  else if((y<0.) && (x>=0.)) return(atan(y/x))+2.*PI;
  else printf("ERROR in p_angle x:%lf y:%lf\n",x,y);
  return(0);
}
