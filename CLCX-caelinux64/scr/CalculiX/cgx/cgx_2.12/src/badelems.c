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
extern SpecialSet specialset[1];

extern Elements  *e_enqire;     /* feld in dem e_enqire[elem[i].nr].xx abgelegt ist, statt elem[i].xx */

extern Eqal eqal;

/* check all angles if lower than MIN_ANGLE_TRI3 */
int checktr7(int elem)
{
  int i;
  double p0[3], p0p1[3], p2p0[3], p1p2[3], p1[3], p2[3], sp[3];

  v_result(&node[e_enqire[elem].nod[0]].nx,&node[e_enqire[elem].nod[1]].nx,p0p1);
  v_result(&node[e_enqire[elem].nod[1]].nx,&node[e_enqire[elem].nod[2]].nx,p1p2);
  v_result(&node[e_enqire[elem].nod[2]].nx,&node[e_enqire[elem].nod[0]].nx,p2p0);
  v_norm(p0p1,p0);
  v_norm(p1p2,p1);
  v_norm(p2p0,p2);

  sp[0]=v_sprod(p0,p1);
  sp[1]=v_sprod(p1,p2);
  sp[2]=v_sprod(p2,p0);
  for (i=0; i<3; i++) if(sp[i]*sp[i]>MIN_ANGLE_TRI3) return(i+1);

  return(0);
}


/* check all angles if lower than MIN_ANGLE_TRI3 */
int checktr8(int elem)
{
  int i;
  double p0[3], p1[3], p2[3], p0p3[3], p0p5[3], p2p5[3], p1p4[3], p1p3[3], p2p4[3], sp[3];

  p0[0]=node[e_enqire[elem].nod[0]].nx;
  p0[1]=node[e_enqire[elem].nod[0]].ny;
  p0[2]=node[e_enqire[elem].nod[0]].nz;
  p1[0]=node[e_enqire[elem].nod[3]].nx;
  p1[1]=node[e_enqire[elem].nod[3]].ny;
  p1[2]=node[e_enqire[elem].nod[3]].nz;
  v_result(p0,p1,p0p3);
  p0[0]=node[e_enqire[elem].nod[0]].nx;
  p0[1]=node[e_enqire[elem].nod[0]].ny;
  p0[2]=node[e_enqire[elem].nod[0]].nz;
  p1[0]=node[e_enqire[elem].nod[5]].nx;
  p1[1]=node[e_enqire[elem].nod[5]].ny;
  p1[2]=node[e_enqire[elem].nod[5]].nz;
  v_result(p0,p1,p0p5);
  p0[0]=node[e_enqire[elem].nod[2]].nx;
  p0[1]=node[e_enqire[elem].nod[2]].ny;
  p0[2]=node[e_enqire[elem].nod[2]].nz;
  p1[0]=node[e_enqire[elem].nod[5]].nx;
  p1[1]=node[e_enqire[elem].nod[5]].ny;
  p1[2]=node[e_enqire[elem].nod[5]].nz;
  v_result(p0,p1,p2p5);
  p0[0]=node[e_enqire[elem].nod[2]].nx;
  p0[1]=node[e_enqire[elem].nod[2]].ny;
  p0[2]=node[e_enqire[elem].nod[2]].nz;
  p1[0]=node[e_enqire[elem].nod[4]].nx;
  p1[1]=node[e_enqire[elem].nod[4]].ny;
  p1[2]=node[e_enqire[elem].nod[4]].nz;
  v_result(p0,p1,p2p4);
  p0[0]=node[e_enqire[elem].nod[1]].nx;
  p0[1]=node[e_enqire[elem].nod[1]].ny;
  p0[2]=node[e_enqire[elem].nod[1]].nz;
  p1[0]=node[e_enqire[elem].nod[4]].nx;
  p1[1]=node[e_enqire[elem].nod[4]].ny;
  p1[2]=node[e_enqire[elem].nod[4]].nz;
  v_result(p0,p1,p1p4);
  p0[0]=node[e_enqire[elem].nod[1]].nx;
  p0[1]=node[e_enqire[elem].nod[1]].ny;
  p0[2]=node[e_enqire[elem].nod[1]].nz;
  p1[0]=node[e_enqire[elem].nod[3]].nx;
  p1[1]=node[e_enqire[elem].nod[3]].ny;
  p1[2]=node[e_enqire[elem].nod[3]].nz;
  v_result(p0,p1,p1p3);


  v_norm(p0p3,p0p3);
  v_norm(p0p5,p0p5);
  v_norm(p1p3,p1p3);
  v_norm(p1p4,p1p4);
  v_norm(p2p4,p2p4);
  v_norm(p2p5,p2p5);


  sp[0]=v_sprod(p0p3,p0p5);
  sp[1]=v_sprod(p1p4,p1p3);
  sp[2]=v_sprod(p2p5,p2p4);
  //for (i=0; i<3; i++) if(sp[i]*sp[i]>MIN_ANGLE_TRI3) return(i+1);
  for (i=0; i<3; i++) if(sp[i]*sp[i]>=1.) return(i+1);

  /* check if the normals point in the about the same direction */
  v_prod(p0p3,p0p5,p0);
  v_prod(p1p4,p1p3,p1);
  v_prod(p2p5,p2p4,p2);
  sp[0]=v_sprod(p0,p1);
  sp[1]=v_sprod(p1,p2);
  if(sp[0]*sp[1]<0) return(i+1);

  return(0);
}


/* aspect ratio */
int e_aspr( int elemnr, double aspr)
{
  int i;
  int h8e[]={0,1,1,2,2,3,3,4,4,5,5,6,6,7,0,4,1,5,2,6,3,7};
  double l, nl[3], max=-MAX_INTEGER, min=MAX_INTEGER;

  if (e_enqire[elemnr].type == 1)  /* HEXA8 */
  {
    for (i=0; i<12; i+=2)
    {
      v_result(&node[e_enqire[elemnr].nod[h8e[i]]].nx, &node[e_enqire[elemnr].nod[h8e[i+1]]].nx, nl);
      l = v_betrag( nl);
      if(l>max) max=l;
      if(l<min) min=l;
    }
  }
  else if (e_enqire[elemnr].type == 4)  /* HEXA20 */
  {
    for (i=0; i<12; i+=2)
    {
      v_result(&node[e_enqire[elemnr].nod[h8e[i]]].nx, &node[e_enqire[elemnr].nod[h8e[i+1]]].nx, nl);
      l = v_betrag( nl);
      if(l>max) max=l;
      if(l<min) min=l;
    }
  }
  if(max/min>aspr) return(0);
  return(1);
}


/* maximum corner angle */
int e_mca( int elemnr, double mca)
{
  int i,j;
  int h8e[6][6]={ {0,1,2,3,0,1},{4,5,6,7,4,5},{0,1,5,4,0,1},{1,2,6,5,1,2},{2,3,7,6,2,3},{3,0,4,7,3,0}};
  double fi, nl1[3], nl2[3], max=-MAX_INTEGER;

  mca=mca*PI/180.;

  if ((e_enqire[elemnr].type == 1)||(e_enqire[elemnr].type == 4))  /* HEXA8 */
  {
    for (i=0; i<6; i++)
    for (j=0; j<4; j++)
    {
      v_result(&node[e_enqire[elemnr].nod[h8e[i][j]]].nx, &node[e_enqire[elemnr].nod[h8e[i][j+1]]].nx, nl1);
      v_result(&node[e_enqire[elemnr].nod[h8e[i][j+1]]].nx, &node[e_enqire[elemnr].nod[h8e[i][j+2]]].nx, nl2);
      fi = v_angle( nl1, nl2 );
      //printf("phi:%lf\n", fi*180./PI);
      if(fi*fi>max) max=fi*fi;
    }
  }
  if(max>(mca*mca))
  {
    printf("phi:%lf\n", sqrt(max)*180./PI);
    return(0);
  }
  return(1);
}


int calcBadElements(char *setname)
{
  int i,j,n;
  int   setNr, nodeset;
  double xl[20][3];
  char elty[MAX_LINE_LENGTH];

  operateAlias( setname, "se" );
  setNr=getSetNr(setname);
  if( set[setNr].name == (char *)NULL )
  {
    errMsg(" ERROR: setNr:%d is undefined\n", setNr);
    return(-1);
  }

  delSet(specialset->njby);
  nodeset=pre_seta( specialset->njby, "i", 0);
  set[nodeset].anz_e=0;

  /* cycle through all elements in the set and check the jacobian matrix at the gaus points */

  for (i=0; i<set[setNr].anz_e; i++)
  {
    if(eqal.aspr>0.) if (!e_aspr( set[setNr].elem[i], eqal.aspr)) seta( nodeset, "e", set[setNr].elem[i]);
    if(eqal.mca>0.) if (!e_mca( set[setNr].elem[i], eqal.mca)) seta( nodeset, "e", set[setNr].elem[i]);

    if(e_enqire[set[setNr].elem[i]].type == 1)
    {
      for(j=0; j<8; j++)
      { 
        xl[j][0]= node[e_enqire[set[setNr].elem[i]].nod[j]].nx* scale->w+scale->x;
        xl[j][1]= node[e_enqire[set[setNr].elem[i]].nod[j]].ny* scale->w+scale->y;
        xl[j][2]= node[e_enqire[set[setNr].elem[i]].nod[j]].nz* scale->w+scale->z;
      }
      strcpy(elty,"C3D8");
      if (!e_c3d__(&xl[0][0], elty)) seta( nodeset, "e", set[setNr].elem[i]);
      if(eqal.jbir>0.) if (!e_c3d_nodes_(&xl[0][0], elty, &set[setNr].elem[i], &eqal.jbir)) seta( nodeset, "e", set[setNr].elem[i]);
      strcpy(elty,"C3D8R");
      if (!e_c3d__(&xl[0][0], elty)) seta( nodeset, "e", set[setNr].elem[i]);
      if(eqal.jbir>0.) if (!e_c3d_nodes_(&xl[0][0], elty, &set[setNr].elem[i], &eqal.jbir)) seta( nodeset, "e", set[setNr].elem[i]);
    }
    else if(e_enqire[set[setNr].elem[i]].type == 4)
    {
      for(j=0; j<12; j++)
      { 
        xl[j][0]= node[e_enqire[set[setNr].elem[i]].nod[j]].nx* scale->w+scale->x;
        xl[j][1]= node[e_enqire[set[setNr].elem[i]].nod[j]].ny* scale->w+scale->y;
        xl[j][2]= node[e_enqire[set[setNr].elem[i]].nod[j]].nz* scale->w+scale->z;
      }
      for(n=16; n<20; n++)
      { 
        xl[j][0]= node[e_enqire[set[setNr].elem[i]].nod[n]].nx* scale->w+scale->x;
        xl[j][1]= node[e_enqire[set[setNr].elem[i]].nod[n]].ny* scale->w+scale->y;
        xl[j][2]= node[e_enqire[set[setNr].elem[i]].nod[n]].nz* scale->w+scale->z;
        j++;
      }
      for(n=12; n<16; n++)
      { 
        xl[j][0]= node[e_enqire[set[setNr].elem[i]].nod[n]].nx* scale->w+scale->x;
        xl[j][1]= node[e_enqire[set[setNr].elem[i]].nod[n]].ny* scale->w+scale->y;
        xl[j][2]= node[e_enqire[set[setNr].elem[i]].nod[n]].nz* scale->w+scale->z;
        j++;
      }
      strcpy(elty,"C3D20");
      if (!e_c3d__(&xl[0][0], elty)) seta( nodeset, "e", set[setNr].elem[i]);
      if(eqal.jbir>0.) if (!e_c3d_nodes_(&xl[0][0], elty, &set[setNr].elem[i], &eqal.jbir)) seta( nodeset, "e", set[setNr].elem[i]);
      strcpy(elty,"C3D20R");
      if (!e_c3d__(&xl[0][0], elty)) seta( nodeset, "e", set[setNr].elem[i]);
      if(eqal.jbir>0.) if (!e_c3d_nodes_(&xl[0][0], elty, &set[setNr].elem[i], &eqal.jbir)) seta( nodeset, "e", set[setNr].elem[i]);
    }
    else if(e_enqire[set[setNr].elem[i]].type == 5)
    {
      for(j=0; j<3; j++)
      { 
        xl[j][0]= node[e_enqire[set[setNr].elem[i]].nod[j]].nx* scale->w+scale->x;
        xl[j][1]= node[e_enqire[set[setNr].elem[i]].nod[j]].ny* scale->w+scale->y;
        xl[j][2]= node[e_enqire[set[setNr].elem[i]].nod[j]].nz* scale->w+scale->z;
      }
      xl[j][0]= xl[j-1][0];
      xl[j][1]= xl[j-1][1];
      xl[j][2]= xl[j-1][2];
      for(j=4; j<7; j++)
      { 
        xl[j][0]= node[e_enqire[set[setNr].elem[i]].nod[j-1]].nx* scale->w+scale->x;
        xl[j][1]= node[e_enqire[set[setNr].elem[i]].nod[j-1]].ny* scale->w+scale->y;
        xl[j][2]= node[e_enqire[set[setNr].elem[i]].nod[j-1]].nz* scale->w+scale->z;
      }
      xl[j][0]= xl[j-1][0];
      xl[j][1]= xl[j-1][1];
      xl[j][2]= xl[j-1][2];
      for(j=8; j<10; j++)
      { 
        xl[j][0]= node[e_enqire[set[setNr].elem[i]].nod[j-2]].nx* scale->w+scale->x;
        xl[j][1]= node[e_enqire[set[setNr].elem[i]].nod[j-2]].ny* scale->w+scale->y;
        xl[j][2]= node[e_enqire[set[setNr].elem[i]].nod[j-2]].nz* scale->w+scale->z;
      }
      xl[j][0]= node[e_enqire[set[setNr].elem[i]].nod[2]].nx* scale->w+scale->x;
      xl[j][1]= node[e_enqire[set[setNr].elem[i]].nod[2]].ny* scale->w+scale->y;
      xl[j++][2]= node[e_enqire[set[setNr].elem[i]].nod[2]].nz* scale->w+scale->z;
      xl[j][0]= node[e_enqire[set[setNr].elem[i]].nod[8]].nx* scale->w+scale->x;
      xl[j][1]= node[e_enqire[set[setNr].elem[i]].nod[8]].ny* scale->w+scale->y;
      xl[j][2]= node[e_enqire[set[setNr].elem[i]].nod[8]].nz* scale->w+scale->z;

      for(j=12; j<14; j++)
      { 
        xl[j][0]= node[e_enqire[set[setNr].elem[i]].nod[j]].nx* scale->w+scale->x;
        xl[j][1]= node[e_enqire[set[setNr].elem[i]].nod[j]].ny* scale->w+scale->y;
        xl[j][2]= node[e_enqire[set[setNr].elem[i]].nod[j]].nz* scale->w+scale->z;
      }
      xl[j][0]= node[e_enqire[set[setNr].elem[i]].nod[5]].nx* scale->w+scale->x;
      xl[j][1]= node[e_enqire[set[setNr].elem[i]].nod[5]].ny* scale->w+scale->y;
      xl[j++][2]= node[e_enqire[set[setNr].elem[i]].nod[5]].nz* scale->w+scale->z;
      xl[j][0]= node[e_enqire[set[setNr].elem[i]].nod[14]].nx* scale->w+scale->x;
      xl[j][1]= node[e_enqire[set[setNr].elem[i]].nod[14]].ny* scale->w+scale->y;
      xl[j][2]= node[e_enqire[set[setNr].elem[i]].nod[14]].nz* scale->w+scale->z;

      for(j=16; j<19; j++)
      { 
        xl[j][0]= node[e_enqire[set[setNr].elem[i]].nod[j-7]].nx* scale->w+scale->x;
        xl[j][1]= node[e_enqire[set[setNr].elem[i]].nod[j-7]].ny* scale->w+scale->y;
        xl[j][2]= node[e_enqire[set[setNr].elem[i]].nod[j-7]].nz* scale->w+scale->z;
      }
      xl[j][0]= xl[j-1][0];
      xl[j][1]= xl[j-1][1];
      xl[j][2]= xl[j-1][2];

      strcpy(elty,"C3D20");
      if (!e_c3d__(&xl[0][0], elty)) seta( nodeset, "e", set[setNr].elem[i]);
      if(eqal.jbir>0.) if (!e_c3d_nodes_(&xl[0][0], elty, &set[setNr].elem[i], &eqal.jbir)) seta( nodeset, "e", set[setNr].elem[i]);
      strcpy(elty,"C3D20R");
      if (!e_c3d__(&xl[0][0], elty)) seta( nodeset, "e", set[setNr].elem[i]);
      if(eqal.jbir>0.) if (!e_c3d_nodes_(&xl[0][0], elty, &set[setNr].elem[i], &eqal.jbir)) seta( nodeset, "e", set[setNr].elem[i]);
    }
    else if(e_enqire[set[setNr].elem[i]].type == 6)
    {
      for(j=0; j<10; j++)
      { 
        xl[j][0]= node[e_enqire[set[setNr].elem[i]].nod[j]].nx* scale->w+scale->x;
        xl[j][1]= node[e_enqire[set[setNr].elem[i]].nod[j]].ny* scale->w+scale->y;
        xl[j][2]= node[e_enqire[set[setNr].elem[i]].nod[j]].nz* scale->w+scale->z;
      }
      strcpy(elty,"C3D10");
      if (!e_c3d__(&xl[0][0], elty)) seta( nodeset, "e", set[setNr].elem[i]);
      if(eqal.jbir>0.) if (!e_c3d_nodes_(&xl[0][0], elty, &set[setNr].elem[i], &eqal.jbir)) seta( nodeset, "e", set[setNr].elem[i]);
    }
    else if(e_enqire[set[setNr].elem[i]].type == 7)
    {
      if (checktr7(set[setNr].elem[i])) seta( nodeset, "e", set[setNr].elem[i]);      
    }
    else if(e_enqire[set[setNr].elem[i]].type == 8)
    {
      if (checktr8(set[setNr].elem[i])) seta( nodeset, "e", set[setNr].elem[i]);      
    }
  }
  return(set[nodeset].anz_e);
}




