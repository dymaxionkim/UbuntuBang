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
extern Elements  *e_enqire;
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
extern SpecialSet specialset[1];




int splitElem(char *setname)
{
  int setNr, j, k,n,m, elem,econnect, indx, nd[3], nbuf[3];
  double p0[3], p0p1[3], p2p0[3], p1p2[3], p1[3], sp[3], max;

  setNr=getSetNr(setname);
  if (setNr<0)
  {
    printf (" ERROR in meshSet: set:%s does not exist\n", setname);
    return(-1);
  }

  /* go over all elements, determine the longest side and split the connected element */
  /* for(i=0; i<set[setNr].anz_e; i++) */
  do
  {
    elem=set[setNr].elem[0];
    if(e_enqire[elem].type!=7) goto next;
    /* determine the log side */
    p0[0]=node[e_enqire[elem].nod[0]].nx;
    p0[1]=node[e_enqire[elem].nod[0]].ny;
    p0[2]=node[e_enqire[elem].nod[0]].nz;
    p1[0]=node[e_enqire[elem].nod[1]].nx;
    p1[1]=node[e_enqire[elem].nod[1]].ny;
    p1[2]=node[e_enqire[elem].nod[1]].nz;
    v_result(p0,p1,p0p1);
    p0[0]=node[e_enqire[elem].nod[2]].nx;
    p0[1]=node[e_enqire[elem].nod[2]].ny;
    p0[2]=node[e_enqire[elem].nod[2]].nz;
    v_result(p1,p0,p1p2);
    p1[0]=node[e_enqire[elem].nod[0]].nx;
    p1[1]=node[e_enqire[elem].nod[0]].ny;
    p1[2]=node[e_enqire[elem].nod[0]].nz;
    v_result(p0,p1,p2p0);
    sp[0]=v_betrag(p0p1);
    sp[1]=v_betrag(p1p2);
    sp[2]=v_betrag(p2p0);
    max=sp[0]; indx=0;
    for(j=1; j<3; j++) if(sp[j]>max) { max=sp[j]; indx=j; }
    if(indx==0)      { nd[0]=e_enqire[elem].nod[0]; nd[1]=e_enqire[elem].nod[1]; nd[2]=e_enqire[elem].nod[2]; }
    else if(indx==1) { nd[0]=e_enqire[elem].nod[1]; nd[1]=e_enqire[elem].nod[2]; nd[2]=e_enqire[elem].nod[0]; }
    else if(indx==2) { nd[0]=e_enqire[elem].nod[2]; nd[1]=e_enqire[elem].nod[0]; nd[2]=e_enqire[elem].nod[1]; }

    /* find the connected element */
    for(j=0; j<anz->e; j++)
    {
      if((e_enqire[e_enqire[j].nr].type==7)&&(e_enqire[j].nr!=elem))
      {
        for(k=0; k<3; k++) if(e_enqire[e_enqire[j].nr].nod[k]==nd[0])
        {
          for(n=0; n<3; n++) if(e_enqire[e_enqire[j].nr].nod[n]==nd[1]) { econnect=j; goto found_econnect; } 
	}
      }
    }
    printf(" splitElem(), found no elem for e:%d n:%d %d %d",elem, nd[0], nd[1], nd[2]); 
    goto next;
    found_econnect:;

    if(((k==0)&&(n==1))||((k==1)&&(n==0))) m=2;
    else if(((k==1)&&(n==2))||((k==2)&&(n==1))) m=0;
    else m=1;

    if(printFlag) printf(" splitElem(), e:%d n:%d %d %d",elem, nd[0], nd[1], nd[2]); 
    if(printFlag) printf(" is connected to:%d with free nod %d \n", e_enqire[econnect].nr, e_enqire[e_enqire[econnect].nr].nod[m]);

    /* redefine both elements */
    nbuf[0]=e_enqire[e_enqire[econnect].nr].nod[m];
    nbuf[1]=nd[2];
    nbuf[2]=nd[0];
    elem_define( elem, 7, nbuf, 0, 0 );
    nbuf[0]=e_enqire[e_enqire[econnect].nr].nod[m];
    nbuf[1]=nd[1];
    nbuf[2]=nd[2];
    elem_define( e_enqire[econnect].nr, 7, nbuf, 0, 0 );
    setr( setNr, "e", e_enqire[econnect].nr );    
    next:;
    setr( setNr, "e", elem);
  }while(set[setNr].anz_e);

  return(1);
}



int improveBadTr3(char *setname)
{
  int setNr,setNr2, i,j, tri3Flag;

  setNr=getSetNr(setname);
  if (setNr<0)
  {
    printf (" ERROR in meshSet: set:%s does not exist\n", setname);
    return(-1);
  }

  for(j=0; j<5; j++)
  {
    /* get all bad tr3 */
    i=calcBadElements(set[setNr].name);
    if (i>0)
    {
      printf("found %d bad elements in set:%s (stored in set:%s)\n", i, set[setNr].name, specialset->njby);
    }
    setNr2=getSetNr(specialset->njby);
    tri3Flag=0;
    for(j=0; j<set[setNr2].anz_e; j++) if (e_enqire[set[setNr2].elem[j]].type==7) tri3Flag=1;
    if(!tri3Flag) break;
 
    /* merge nodes of tr3 with a small edge */
    /* tbd */

    /* split elements on the long side of a tr3 */
    if(i>0) splitElem(specialset->njby);
    else break;
  }

  return(1);
}
