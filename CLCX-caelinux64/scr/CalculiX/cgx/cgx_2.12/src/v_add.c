#include <math.h>
#include <string.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>


void v_add( double *A, double *B, double *C )
/**********************************************************/
/*    Vektoraddition: C =  Vektor(B)+Vektor(A)            */
/**********************************************************/
{
register int i;

for (i=0; i<3; i++){
         C[i]=B[i]+A[i];
	}
}


