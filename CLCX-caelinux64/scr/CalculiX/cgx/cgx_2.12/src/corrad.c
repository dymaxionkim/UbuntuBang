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


#include <cgx.h>



extern double     gtol;

extern Scale     scale[1];
extern Summen    anz[1];
extern Edges     *edge;
extern Nodes     *node;
extern Datasets *lcase;

extern Alias     *alias;
extern Sets      *set; 
extern Points    *point;
extern Lines     *line;
extern Lcmb      *lcmb;
extern Gsur      *surf;
extern Gbod      *body;
extern Nurbs     *nurbs;
extern SumGeo    anzGeo[1];

extern char  printFlag;                     /* printf 1:on 0:off */



void corrad( int setNr, double minradius)
{
  register int  i, j;
  int  lines, puffer, p1[300], p2[300], p3[300], cp[300], puf1[300], puf2[300], puf3[300];
  double pkt1[3], pkt2[3], pkt3[3], v1[3], v2[3], vr[3], vn[3], v1_betr, vr_betr;
  double  alfa, radius, s; 

  /* cycle trough the lines in the set and determine the points */
  lines=0;
  puffer=0;
  for (i=0; i<set[setNr].anz_l; i++)
  {

    if( line[set[setNr].line[i]].name != (char *)NULL )
    {
        if (line[set[setNr].line[i]].typ=='a') /* its an arc-line */
        {
          p1[lines]=line[set[setNr].line[i]].p1;
          p2[lines]=line[set[setNr].line[i]].p2;
          cp[lines]=line[set[setNr].line[i]].trk;
          lines++;
        }
        else
        {
          puf1[puffer]=line[set[setNr].line[i]].p1;
          puf2[puffer]=line[set[setNr].line[i]].p2;
          puf3[puffer]=set[setNr].line[i];
	  puffer++;
        }
    }
  }
  /*****************************************************************/
  /* raussuchen von p3 aus den puffer-punkten                      */
  /* und punkte nach lage sortieren, p1 ist immer in der Plattform */
  /*****************************************************************/

  for ( i=0; i<lines; i++)
  {
    for ( j=0; j<puffer; j++)
    {
      if (p1[i]==puf1[j]) /* Reihenfolge p1,p2 falsch */
      {  
        p1[i]=p2[i];
        p2[i]=puf1[j];
        p3[i]=puf3[j];
      }
      if (p1[i]==puf2[j])
      {
        p1[i]=p2[i];
        p2[i]=puf2[j];
        p3[i]=-puf3[j]-1;
      }
      if (p2[i]==puf1[j])  /* Reihenfolge richtig */
        p3[i]=puf3[j];
      if (p2[i]==puf2[j])
	p3[i]=-puf3[j]-1;
    }
  }

  /************ xyz-werte der punkte p1,p2,p3 raussuchen **********/
  /******** ab hier wird radius für radius durchgeackert     ******/
  for (i=0; i<lines; i++)
  {
    pkt1[0]=point[p1[i]].px;
    pkt1[1]=point[p1[i]].py;
    pkt1[2]=point[p1[i]].pz;
    pkt2[0]=point[p2[i]].px;
    pkt2[1]=point[p2[i]].py;
    pkt2[2]=point[p2[i]].pz;
    if(p3[i]<0)
    {
      p3[i]=((p3[i]+1)*-1);
      j=line[p3[i]].nip-6;
      pkt3[0]=line[p3[i]].ip[j];
      pkt3[1]=line[p3[i]].ip[j+1];
      pkt3[2]=line[p3[i]].ip[j+2];
    }
    else
    {
      j=3;
      pkt3[0]=line[p3[i]].ip[j];
      pkt3[1]=line[p3[i]].ip[j+1];
      pkt3[2]=line[p3[i]].ip[j+2];
    }
    /******** vector in Ebene durch p1 bis p3 und senkrecht auf p2p3 *****/

    v_result(pkt2,pkt1,v1);
    v_result(pkt2,pkt3,v2);   

    v_prod(v1,v2,vn);    /*Normalvector auf P1,P2,P3*/
    v_prod(v2,vn,vr);    /*vector in gewünschter richtung für cp*/
    v_norm(vr,vr);       /*vector normieren (laenge=1!)*/

    /********* berechnung der neuen koordinaten für cp  ************/

    /* s_prod(vr,v1,a,b,v1_betr,alfa); */
    s=v_sprod(vr,v1);
    vr_betr=v_betrag(vr);
    v1_betr=v_betrag(v1);
    alfa = acos((s/(vr_betr*v1_betr)));

    radius=v1_betr/2./cos(alfa);

    /* zu kleine radien werden abgefangen */
 
    if (radius < minradius/scale->w)
    {
      printf(" line:%s radius:%lf set to %lf\n", line[set[setNr].line[i]].name, radius*scale->w, minradius);
      radius=minradius/scale->w;
    }
    else
      printf(" line:%s radius:%lf\n", line[set[setNr].line[i]].name, radius*scale->w);
    point[cp[i]].px=pkt2[0]+vr[0]*radius;
    point[cp[i]].py=pkt2[1]+vr[1]*radius;
    point[cp[i]].pz=pkt2[2]+vr[2]*radius;
  }
  for (i=0; i<set[setNr].anz_l; i++) repLine(set[setNr].line[i]);
}





