#include <math.h>
#include <string.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>


void v_scal( double *A, double *B, double *C )
/**********************************************************/
/* Vektormultiplikation: vektor(C) =  scalar(A)*Vektor(B) */
/**********************************************************/
{
register int i;

for (i=0; i<3; i++){
         C[i]= *A * B[i];
	}
}

                                                                 
void v_scalf( float *A, double *B, double *C )
/**********************************************************/
/* Vektormultiplikation: vektor(C) =  scalar(A)*Vektor(B) */
/**********************************************************/
{
  int i;

  for (i=0; i<3; i++){
         C[i]= *A * B[i];
        }
}

