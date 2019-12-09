#include <extUtil.h>


/******************************************************************/
/*   area (a) cut area (b) return -1 if failed                    */
/*  pa1-3 points define area a                                    */
/*  pb1-3 points define area b                                    */
/*  ps1-2 points derived from vectors between points of area b    */
/******************************************************************/
int AsplitA( double *pa1, double *pa2, double *pa3, double *pb1, double *pb2, double *pb3, double *ps1, double *ps2)
{
  int i,n=0;
  double g,l, ps[2][3], pa[3], pb[3], eu[3], ev[3], eg[3], pba[3], pbps[3];

  /* determine the vectors (eu,ev) defining area a */
  v_result( pa1, pa2, pa );
  v_norm( pa, eu );
  v_result( pa1, pa3, pa );
  v_norm( pa, ev );

  /* determine split points ps1, ps2 on the vectors between points defining area b */

  /* split line pb12 */
  v_result( pb1, pa1, pba );
  v_result( pb1, pb2, pb );
  l=v_norm( pb, eg );
  g = AsplitL( pba, eu, ev, eg );
  if((g>=0.)&&(g<=l))
  {
    /* area a splits line pb12 */
    v_scal( &g, eg, pbps );
    v_add( pb1, pbps, ps[n] );
    n++;
  }

  /* split line pb23 */
  v_result( pb2, pa1, pba );
  v_result( pb2, pb3, pb );
  l=v_norm( pb, eg );
  g = AsplitL( pba, eu, ev, eg );
  if((g>=0.)&&(g<=l))
  {
    /* area a splits line pb23 */
    v_scal( &g, eg, pbps );
    v_add( pb2, pbps, ps[n] );
    n++;
  }

  if(n==0) return(-1); /* area a does not intersect area b */
  else     for (i=0; i<3; i++) ps1[i]=ps[0][i];

  if(n==2)
  {
    for (i=0; i<3; i++)
    {
      ps2[i]=ps[1][i];
    }
    return(1);
  }
  else
  {
    /* split line pb31 */
    v_result( pb3, pa1, pba );
    v_result( pb3, pb1, pb );
    l=v_norm( pb, eg );
    g = AsplitL( pba, eu, ev, eg );
    if((g>=0.)&&(g<=l))
    {
      /* area a splits line pb31 */
      v_scal( &g, eg, pbps );
      v_add( pb3, pbps, ps2 );
      return(2);
    }
  }
  return(-1);
}


