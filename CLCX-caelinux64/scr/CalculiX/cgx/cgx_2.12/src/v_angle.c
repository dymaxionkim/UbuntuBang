#include <extUtil.h>

/* angle between two vectors in rad */
double v_angle( double *v0, double *v1 )
{
  double vn0[3], vn1[3];
  double fi;

    v_norm(  v0, vn0 ); 
    v_norm(  v1, vn1 ); 
    /* winkel zwischen den Vektoren */
    fi=v_sprod( vn0, vn1 );
    return(acos(fi));
}
