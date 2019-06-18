
#include <math.h>     /* Need these for general information for the compiler      */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>


void  m_copy(double *s, double *m)
/* ********************************************************* */
/*  s(4,4) = m(4,4)   def. aus OpenGL                        */
/*               m( spalte,reihe)                            */
/* ********************************************************* */
{
  int j;

  for (j=0; j<16;j++ )
    s[j]= m[j];
}


