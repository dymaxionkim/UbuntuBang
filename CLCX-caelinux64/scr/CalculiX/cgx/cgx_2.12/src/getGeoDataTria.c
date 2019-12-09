#include "extUtil.h"


/* ---------------------------------------------------------------  */
/* getGeoDataTria liest 3 Punkte eines beliebigen Dreiecks          */
/* und berechnet Ix, Iy, Ixy, Schwerpunkt x,y , A im Globalsystem   */
/*                                                                  */
/* Die Geodaten werden im 2D-Elementsystem berechnet und ins 3D-    */
/* Globalsystem gedreht                                             */
/*                                                                  */
/* ACHTUNG: z. Z. wird nur in der xy-ebene gedreht!                 */
/* ---------------------------------------------------------------  */

int  getGeoDataTria( double *p1, double *p2, double *p3, double *Ix, double *Iy, double *Ixy,
                double *A, double *pcg)
/*             p3         */
/*   Tri_2    /|\   Tri_1 */
/*           p1_p2        */
/*           (p4)         */
{
  int i;
  static double vx[]={1.,0.,0.};

  double p4[3], v12[3], v13[3], v23[3], v14[3], v43[3], vnorm[3], vbuf[3], vbuf2[3];
  double p_puf[3][3], vb; /* pktnr,xyz */
  double l12, l13, l23, b1, b2, b_ges, h, A1, A2;
  double Ix1_p4,  Iy1_p4, Ixy1_p4;
  double Ix2_p4,  Iy2_p4, Ixy2_p4;
  double Ix_p4,  Iy_p4, Ixy_p4;

  double spx_p4, spy_p4, sp1x_p4, sp2x_p4;
  double Ix_sp,  Iy_sp, Ixy_sp;

  double alfa_z, a, b, c;

  /* v12 muss den groeste Laenge haben, sonst umsortieren und checken ob es ein dreieck ist v12xv13 != 0.  */
  v_result(p1, p2, v12);
  l12=v_betrag(v12);
  v_result(p1, p3, v13);
  v_prod(v12,v13,vbuf);
  if((int)(1.e20*v_betrag(vbuf))==0) return(0);
  l13=v_betrag(v13);
  v_result(p2, p3, v23);
  l23=v_betrag(v23);
  vb=l12;
  if (l13 > vb)
  {
    vb=l13;
    for (i=0; i<3; i++) p_puf[0][i]= p3[i];
    for (i=0; i<3; i++) p_puf[1][i]= p1[i];
    for (i=0; i<3; i++) p_puf[2][i]= p2[i];
    for (i=0; i<3; i++) p1[i]=p_puf[0][i];
    for (i=0; i<3; i++) p2[i]=p_puf[1][i];
    for (i=0; i<3; i++) p3[i]=p_puf[2][i];
  }
  if (l23 > vb)
  {
    vb=l23;
    for (i=0; i<3; i++) p_puf[0][i]= p2[i];
    for (i=0; i<3; i++) p_puf[1][i]= p3[i];
    for (i=0; i<3; i++) p_puf[2][i]= p1[i];
    for (i=0; i<3; i++) p1[i]=p_puf[0][i];
    for (i=0; i<3; i++) p2[i]=p_puf[1][i];
    for (i=0; i<3; i++) p3[i]=p_puf[2][i];
  }

  /* berechnung der Geometiegroessen (2D-Elementsys.)  aus 3D Punktkoordinaten*/
  v_result(p1, p3, v13);
  v_result(p1, p2, v12);
  b_ges=v_betrag(v12);
  b2=v_sprod( v13, v12);
  b2=b2/b_ges;
  b1=b_ges-b2;

  v_norm( v12, vnorm);
  v_scal( &b2, vnorm, v14);
  v_add( p1, v14, p4);
  v_result(p4, p3, v43);
  h=v_betrag(v43);

  A1= b1*h/2.;
  *A= b_ges*h/2.;
  A2= *A-A1;

  /* Schwerpunkte (sp) bezogen auf p4  IM LOKALEN SYSTEM */

  sp1x_p4= b1/3.;
  sp2x_p4= -b2/3.;
  spx_p4 = (A1*sp1x_p4 + A2*sp2x_p4) / *A;
  spy_p4 = h/3.;

  /* Traegheitsmomente bezogen auf p4  IM LOKALEN SYSTEM */

  Ix1_p4=  b1*h*h*h/12.;
  Iy1_p4=  h*b1*b1*b1/12.;
  Ixy1_p4= b1*b1*h*h/24.;

  Ix2_p4=  b2*h*h*h/12.;
  Iy2_p4=  h*b2*b2*b2/12.;
  Ixy2_p4= b2*b2*h*h/24.;

  Ix_p4= Ix1_p4 + Ix2_p4;
  Iy_p4= Iy1_p4 + Iy2_p4;
  Ixy_p4= Ixy1_p4 - Ixy2_p4;
  /*
  printf ("\nA:%lf b1:%lf b2:%lf h:%lf \n", *A, b1, b2, h);
  printf ("Tri1_P4 Ix:%lf Iy:%lf Ixy:%lf  \n", Ix1_p4,  Iy1_p4, Ixy1_p4);
  printf ("Tri2_P4 Ix:%lf Iy:%lf Ixy:%lf  \n", Ix2_p4,  Iy2_p4, Ixy2_p4);
  printf ("Tri _P4 Ix:%lf Iy:%lf Ixy:%lf  \n", Ix_p4,  Iy_p4, Ixy_p4); 
  */
  /* Traegheitsmomente bezogen auf Schwp. (nach Steiner) */

  Ix_sp = Ix_p4 - spy_p4*spy_p4 * *A;
  Iy_sp = Iy_p4 - spx_p4*spx_p4 * *A;
  Ixy_sp = Ixy_p4 - spx_p4*spy_p4 * *A;
  /*
  printf ("\nA:%lf b1:%lf b2:%lf h:%lf \n", *A, b1, b2, h);
  printf ("sp_p4  spx:%lf spy:%lf   \n", spx_p4,  spy_p4);
  printf ("Tri _sp Ix:%lf Iy:%lf Ixy:%lf  \n", Ix_sp,  Iy_sp, Ixy_sp);
  */

  /* umrechnen aufs Gloabale System (2D Elementsystem -> 3D Globalsystem) */
  v_norm( v12, vnorm);
  v_scal( &spx_p4, vnorm, vbuf);
  v_add( p4, vbuf, vbuf2);

  v_norm( v43, vnorm);
  v_scal( &spy_p4, vnorm, vbuf);
  v_add( vbuf2, vbuf, pcg);

  
  /* ACHTUNG: z. Z. wird nur in der xy-ebene gedreht!  */
  v12[2] = 0.;

  alfa_z= acos( v_sprod(v12,vx) / v_betrag(v12) / v_betrag(vx) ) * (-1.);

  a = (Ix_sp+Iy_sp)/2.;
  b = (Ix_sp-Iy_sp)/2. * cos(2.*alfa_z);
  c =  Ixy_sp * sin(2.*alfa_z);
  *Ix= a+b-c;
  *Iy= a-b+c;
  *Ixy= a * sin(2.*alfa_z) + Ixy_sp * cos(2.*alfa_z);

  /* Traegheitsmomente bezogen auf Globalsystem (nach Steiner) */

  *Ix= *Ix + pcg[1]*pcg[1] * *A;
  *Iy= *Iy + pcg[0]*pcg[0] * *A;
  *Ixy= *Ixy + pcg[0]*pcg[1] * *A;

  return(1);

}

