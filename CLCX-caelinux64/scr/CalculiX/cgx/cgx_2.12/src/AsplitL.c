#include <math.h>


double AsplitL( double *b, double *eu, double *ev, double *eg )
/******************************************************************/
/*   Grade (eg) schneidet Ebene (eu,ev) return g                  */
/*   determinante dritter Ordnung                                 */
/*   b= Abstand zwischen den Aufpunkten der Linie und Ebene       */
/*   b=eu*u + ev*v + eg *g  (e: Einheitsvektoren )                */
/******************************************************************/
{
  double g, D, Dg, a, c;

  a = eu[0]*ev[1]*eg[2]+ ev[0]*eg[1]*eu[2]+ eg[0]*eu[1]*ev[2];
  c = eg[0]*ev[1]*eu[2]+ eu[0]*eg[1]*ev[2]+ ev[0]*eu[1]*eg[2];
  D = a-c;

  a = eu[0]*ev[1]* b[2]+ ev[0]* b[1]*eu[2]+  b[0]*eu[1]*ev[2];
  c =  b[0]*ev[1]*eu[2]+ eu[0]* b[1]*ev[2]+ ev[0]*eu[1]* b[2];
  Dg= a-c;
  g = Dg / D;
  return (g);
}


