/* --------------------------------------------------------------------  */
/*                          CALCULIX                                     */
/*                   - GRAPHICAL INTERFACE -                             */
/*                                                                       */
/*     A 3-dimensional pre- and post-processor for finite elements       */
/*              Copyright (C) 1996 Klaus Wittig                          */
/*                                                                       */
/*     This program is free software; you can redistribute it and/or     */
/*     modify it under the terms of the GNU General Public License as    */
/*     published by the Free Software Foundation; version 2 of           */
/*     the License.                                                      */
/*                                                                       */
/*     This program is distributed in the hope that it will be useful,   */
/*     but WITHOUT ANY WARRANTY; without even the implied warranty of    */ 
/*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the      */
/*     GNU General Public License for more details.                      */
/*                                                                       */
/*     You should have received a copy of the GNU General Public License */
/*     along with this program; if not, write to the Free Software       */
/*     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.         */
/* --------------------------------------------------------------------  */


/*
TO DO:
*/


#define     TEST            0     /* debugging */

#include <cgx.h>


void adjustMidsideNode(double *P1, double *P2, double *Pm, int method)
{
  int i,j=0;
  double pmp1[3], pmp2[3], nm12[3], pmp1_2[3], pmp2_2[3], p0p1_2[3],  p0p2_2[3], va[3], vb[3], p1p2[3];
  double eva[3], evb[3], Pc[3], vr[3], Ph[3], R, u, pbuf[3];

  /* center point and radius */

#if TEST
  printf("pnt p1 %f %f %f \n", P1[0] , P1[1] , P1[2]);
  printf("pnt pm %f %f %f \n", Pm[0] , Pm[1] , Pm[2]);
  printf("pnt p2 %f %f %f \n", P2[0] , P2[1] , P2[2]);
#endif

  v_result( P1, P2, p1p2  );

  if(method)
  {
    /* lineare interpolation */
    u=0.5;
    v_scal( &u, p1p2, pbuf);
    v_add(P1,pbuf,Pm);
    return;
  }

  /* vprod nm12 = pmp1 x pmp2 */
  v_result( Pm, P1, pmp1 );
  v_result( Pm, P2, pmp2 );
  v_prod( pmp1, pmp2, nm12 );
  
  /* Vector halfway between pm and p1 or p2 */
  u=0.5;
  v_scal( &u, pmp1, pmp1_2);
  v_scal( &u, pmp2, pmp2_2);
  v_add( Pm, pmp1_2, p0p1_2); 
  v_add( Pm, pmp2_2, p0p2_2); 


  /* Vector in direction to PC, vprod va = nm12 x pmp1 and vb =  pmp2 x nm12 */
  v_prod( nm12, pmp1, va );
  v_prod( pmp2, nm12, vb );
  v_norm( va, eva );
  v_norm( vb, evb );
  
  /* determine abs-max-component of nm12 */
  u=0.;
  for (i=0; i<3; i++) if (nm12[i]*nm12[i]>u) { u=nm12[i]*nm12[i]; j=i; }

  /* calculation of the intersection between eva and evb */
  if (j==0)
  {
    u=(p0p2_2[1]-p0p1_2[1]-eva[1]*(p0p2_2[2]-p0p1_2[2])/eva[2])
     /(eva[1]*evb[2]/eva[2] - evb[1]);
  }
  else if (j==1)
  {
    u=(p0p2_2[0]-p0p1_2[0]-eva[0]*(p0p2_2[2]-p0p1_2[2])/eva[2])
     /(eva[0]*evb[2]/eva[2] - evb[0]);
  }
  else if (j==2)
  {
    u=(p0p2_2[0]-p0p1_2[0]-eva[0]*(p0p2_2[1]-p0p1_2[1])/eva[1])
     /(eva[0]*evb[1]/eva[1] - evb[0]);
  }
  else 
  {
    printf(" ERROR: in createLine, nm12 in error:%d\n", j);
    return;
  }

  /* check if u is a valid number (if 'nan' u is NOT >0 ) */
  if((u>0.)&&(u<1.e6));
  else
  {
    /* lineare interpolation */
    u=0.5;
    v_scal( &u, p1p2, pbuf);
    v_add(P1,pbuf,Pm);
    return;
  }

  /*  centerpoint Pc = p0p2_2+ evb*u */
  v_scal( &u, evb, pbuf );
  v_add( pbuf, p0p2_2, Pc );

  /* radius berechnen */
  v_result(Pc, P1, vr);        
  R= v_betrag(vr);

  /* if R > MAX_VALUE_R its a straight line */
  u= v_betrag(p1p2);
  if( R > u*100)
  {
    u=0.5;
    v_scal( &u, p1p2, pbuf);
    v_add(P1,pbuf,Pm);
  }
  else
  {
    /* new position for midside node */
    /* Point at (p1p2/2) == ph */
    u=0.5;
    v_scal( &u, p1p2, pbuf );
    v_add(P1,pbuf,Ph);
    v_result( Pc, Ph, va );
    v_norm( va, eva );

    /* pm = e_pcph(==eva) * R */
    v_scal( &R, eva, Ph );
    v_add(Pc,Ph,Pm);
#if TEST
    printf("# R:%lf \n", R);
    printf("pnt pm3 %f %f %f \n", Pm[0] , Pm[1] , Pm[2]);
#endif
  }     
}
