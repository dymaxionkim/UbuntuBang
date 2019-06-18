
#include <math.h>     /* Need these for general information for the compiler      */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>


void  v_matmult(double *v, double *m)
/* ********************************************************* */
/*  v(x,y,z,w) = m(4,4) * v(x,y,z,w)  def. aus OpenGL        */
/*               m( spalte,reihe)
v0 =  m00 m04 m08 m12  v0
v1 =  m01 m05 m09 m13 *v1
v2 =  m02 m06 m10 m14  v2
v3 =  m03 m07 m11 m15  v3
*/
/* ********************************************************* */
{
  register int j;
  double b[4];

  b[0]= v[0]*m[0] +v[1]*m[4] +v[2]*m[8] +v[3]*m[12];
  b[1]= v[0]*m[1] +v[1]*m[5] +v[2]*m[9] +v[3]*m[13];
  b[2]= v[0]*m[2] +v[1]*m[6] +v[2]*m[10] +v[3]*m[14];
  b[3]= v[0]*m[3] +v[1]*m[7] +v[2]*m[11] +v[3]*m[15];
  for (j=0; j<4; j++)
    {
    v[j]=b[j];
    }
}


