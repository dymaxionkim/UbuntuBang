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

/* to do:  */
/*
*/


#include <cgx.h>

#define TEST 0

#define TESTSWEEP 1

extern char  datin[MAX_LINE_LENGTH];                          /* Input-data-file */
extern double     gtol;

extern Scale     scale[1];
extern Summen    anz[1];
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
extern Shapes    *shape;
extern SumGeo    anzGeo[1];
extern SumAsci   sumAsci[1];


extern int     ddiv;
extern double     dbias;

extern char  printFlag;                     /* printf on/off on=1 */
extern SpecialSet specialset[1];

/* the copied node-sets which have to be filled with values from new loaded Datasets */
extern CopiedNodeSets copiedNodeSets[1];
extern char  addDispFlag;                    /* 0: original node-coordinates, 1: node-coordinates+displacements */




int createCenterpnt( char *pkt1, char *pkt2, char *buf, char *pkt3, char reuseFlag )
{
  int  i,p;
  static char name[MAX_LINE_LENGTH];
  int   pnr1, pnr2, pnr3;
  double dy=0., dz=0.,x,y,z, dh, dq, l, h, q, p1h, p1q, fi=0.;
  double p0[3], p1[3], p2[3], p1p2[3], p3[3];
  double ph[3], p1ph[3];
  double el[3], eh[3], eq[3];   /* xyz koordinaten der lhq vektoren */
  double ex[3], ey[3], ez[3];   /* lhq koordinaten der xyz vektoren */

  double dfi,ifi, radius, pq[3],p0p3[3], p0ps[3], es[3], ps[36][3];
  static char pname[36][MAX_LINE_LENGTH];

  pnr3=getPntNr( pkt3 );
  pnr2=getPntNr( pkt2 );
  pnr1=getPntNr( pkt1 );
  if(pnr2>-1)
  {
    fi = atof(buf)*PI/180.;
    if (pnr1==-1)
    {
      errMsg(" Point1 (%s) is undefined\n", pkt1 );
      return(-1);
    }
    if (pnr2==-1)
    {
      errMsg(" Point2 (%s) is undefined\n", pkt2 );
      return(-1);
    }
    if (pnr3==-1)
    {
      errMsg(" Point3 (%s) is undefined\n", pkt3 );
      return(-1);
    }
    
    /* berechnung der Einheitsvektoren des Verdreh-Koordinatensystems */
    /* Exneu = p1p2/|p1p2| ist der Einheitsvektor in xneu richtung    */
    /* Eyneu = p1p2 X p1ph / |p1p2 X p1ph|     in yneu                */
    /* Ezneu = Exneu X Eyneu                   in zneu                */
    
    p1[0] = point[pnr1].px;
    p1[1] = point[pnr1].py;
    p1[2] = point[pnr1].pz;
    
    p2[0] = point[pnr2].px;
    p2[1] = point[pnr2].py;
    p2[2] = point[pnr2].pz;
    
    v_result( p1, p2, p1p2 );
    if( v_betrag(p1p2)==0. ) return(-1);
    v_norm  ( p1p2, el );
    
    /* erzeuge einen Hilfspunkt der nicht auf der el-achse liegt  */
    y=p1p2[0];
    z=p1p2[1];
    x=p1p2[2];
    dy=y*cos(PI)-z*sin(PI);
    dz=y*sin(PI)+z*cos(PI);
    ph[0] = x;
    ph[1] = dy;
    ph[2] = dz;
    
    /* konstuiere damit den 2. einheitsvektor eh  */
    v_result( p1, ph, p1ph );
    v_prod( p1p2, p1ph, ph );
    v_norm (ph, eh);
    
    /* und der dritte: eq  */
    v_prod( el, eh, eq );
    
    /* berechnung der lhq-koordinaten der xyz einheitsvektoren durch zykl.vertausch.  */
    ex[0]=el[0];
    ex[1]=eh[0];
    ex[2]=eq[0];
    
    ey[0]=el[1];
    ey[1]=eh[1];
    ey[2]=eq[1];
    
    ez[0]=el[2];
    ez[1]=eh[2];
    ez[2]=eq[2];
    
    /* Berechnung der lhq-koordinaten der Drehachse */
    x=point[pnr1].px*scale->w+scale->x;
    y=point[pnr1].py*scale->w+scale->y;
    z=point[pnr1].pz*scale->w+scale->z;
    
    l=ex[0]*x+ey[0]*y+ez[0]*z;
    h=ex[1]*x+ey[1]*y+ez[1]*z;
    q=ex[2]*x+ey[2]*y+ez[2]*z;
    p1h=h;
    p1q=q;
    
    /* Berechnung der lhq-koordinaten des centerpnts */
    p3[0] = point[pnr3].px*scale->w+scale->x;
    p3[1] = point[pnr3].py*scale->w+scale->y;
    p3[2] = point[pnr3].pz*scale->w+scale->z;
    
    l=ex[0]*p3[0]+ey[0]*p3[1]+ez[0]*p3[2];
    dh= p1h;
    dq= p1q;
    
    p0[0]=el[0]*l+eh[0]*dh+eq[0]*dq;
    p0[1]=el[1]*l+eh[1]*dh+eq[1]*dq;
    p0[2]=el[2]*l+eh[2]*dh+eq[2]*dq;
  }
  else
  {
    el[0]=el[1]=el[2]=0.;
    if (pnr1>-1)
    {
      fi = atof(buf)*PI/180.;
      p0[0] = point[pnr1].px*scale->w+scale->x;
      p0[1] = point[pnr1].py*scale->w+scale->y;
      p0[2] = point[pnr1].pz*scale->w+scale->z;
      if (pkt2[0]=='x')      { el[0]=1.; p0[0]=point[pnr3].px*scale->w+scale->x; }
      else if (pkt2[0]=='y') { el[1]=1.; p0[1]=point[pnr3].py*scale->w+scale->y; }
      else if (pkt2[0]=='z') { el[2]=1.; p0[2]=point[pnr3].pz*scale->w+scale->z; }
    }
    else
    {
      fi = atof(pkt2)*PI/180.;
      p0[0] = 0.;
      p0[1] = 0.;
      p0[2] = 0.;
      if (pkt1[0]=='x')      { el[0]=1.; p0[0]=point[pnr3].px*scale->w+scale->x; }
      else if (pkt1[0]=='y') { el[1]=1.; p0[1]=point[pnr3].py*scale->w+scale->y; }
      else if (pkt1[0]=='z') { el[2]=1.; p0[2]=point[pnr3].pz*scale->w+scale->z; }
    }
  }

  if(abs(fi)<PI)
  {
    /* create the centerpoint */
    if(reuseFlag) { if(!name[0]) getNewName( name, "p" ); }
    else getNewName( name, "p" );
    if(printFlag) printf(" create center-point:%s %lf %lf %lf\n", name,p0[0],p0[1],p0[2]  );
    p=pnt( name,p0[0],p0[1],p0[2],1 );
    if ( p <0 )
    { printf("copy: could not create new point\n"); return(-1); }
  }
  else
  {
    /* create a sequence of points */
    if(reuseFlag) { if(!name[0]) getNewName( name, "se" );}
    else getNewName( name, "se" );
    if(printFlag) printf(" create sequence of points:%s\n", name );
    p=pre_seta(name,"is",0);
    set[p].anz_p=0;
    dfi=fi/36;
    // get the unscaled coordinates
    if (pnr1>-1)
    {
      p1[0] = point[pnr1].px*scale->w+scale->x;
      p1[1] = point[pnr1].py*scale->w+scale->y;
      p1[2] = point[pnr1].pz*scale->w+scale->z;
    }
    else
    {
      p1[0] = 0.;
      p1[1] = 0.;
      p1[2] = 0.;
    }
    p3[0] = point[pnr3].px*scale->w+scale->x;
    p3[1] = point[pnr3].py*scale->w+scale->y;
    p3[2] = point[pnr3].pz*scale->w+scale->z;
    seta(p,"p",pnr3);
    v_result( p0, p3, p0p3 );
    radius=v_betrag(p0p3);
    //printf(" p0p3 %lf %lf %lf r:%f\n",p0p3[0],p0p3[1],p0p3[2], radius);
    if(radius>0.)
    {
     v_norm (p0p3, eq);
     v_prod( el, eq, eh );
     for(i=0; i<35; i++)
     {
      ifi=dfi*(i+1);
      h=sin(ifi);
      q=cos(ifi);
      v_scal(&h,eh,ph);
      //printf(" ph   %lf %lf %lf\n",ph[0],ph[1],ph[2] );
      v_scal(&q,eq,pq);
      //printf(" pq   %lf %lf %lf\n",pq[0],pq[1],pq[2] );

      v_add(pq,ph,p0ps);
      //printf(" p0ps %lf %lf %lf\n",p0ps[0],p0ps[1],p0ps[2] );
      v_norm (p0ps, es);
      v_scal(&radius,es,p0ps);
      //printf(" p0ps %lf %lf %lf\n",p0ps[0],p0ps[1],p0ps[2] );
      v_add(p0,p0ps,ps[i]);
      if(reuseFlag) { if(pname[i][0]==0) getNewName( pname[i], "p" ); }
      else getNewName( pname[i], "p" );
      if(printFlag) printf(" create point[%d]:%s %lf %lf %lf\n",i, pname[i],ps[i][0],ps[i][1],ps[i][2]  );
      seta(p,"p",pnt( pname[i],ps[i][0],ps[i][1],ps[i][2],1 ));
     }
     p*=-1;
    }
    else p=0;
  }
  return( p );
}


/* copy Datasets to copied nodes */
void copyDatasetToNodes(Summen *anz, Nodes *node, Datasets *lcase, int lc, CopiedNodeSets copiedNodeSets)
{
  int i,j,k,ns,nm,nd,set,ncomps;
  int lc1,lc2, compareChars;
  Datasets  *lcase2=NULL;
  double theta, ctl, stl;
  double csab[6];
  double sign;
  double l,h,fi, el[3], p1[3], p2[3], p1p2[3], ph[3];
 
  if(!copiedNodeSets.sets) return;

  for(set=0; set<copiedNodeSets.sets; set++)
  {
    if(printFlag) printf("copy Datasets from lc:%d ictype:%d to copied-node-set:%d\n",lc+1,lcase[lc].ictype[0], set); 

    for(i=0; i<6; i++) csab[i]=copiedNodeSets.axis[set][i];
    if((copiedNodeSets.type[set]==1)||(copiedNodeSets.type[set]==2)||(copiedNodeSets.type[set]==4))  /* transormation-type: tra,rad,scal */
    {
      for(j=0; j<lcase[lc].ncomps; j++)
      {
        for (i=0; i<copiedNodeSets.anz_n[set]; i++)
        {
          nm=copiedNodeSets.mnod[set][i];
          ns=copiedNodeSets.snod[set][i];
          lcase[lc].dat[j][ns]=lcase[lc].dat[j][nm];
        }
      }
    }
    else if((copiedNodeSets.type[set]==3)||(copiedNodeSets.type[set]==5))  /* transormation-type: rot,mir */
    {
      if(lcase[lc].ictype[0]==1) /* scalar */
      {
        for(j=0; j<lcase[lc].ncomps; j++)
        {
          for (i=0; i<copiedNodeSets.anz_n[set]; i++)
          {
            nm=copiedNodeSets.mnod[set][i];
            ns=copiedNodeSets.snod[set][i];
            lcase[lc].dat[j][ns]=lcase[lc].dat[j][nm];
          }
        }
      }

      /* is it a cycsym calc with harmonics? */
      nd=0;
      for(i=0;i<lcase[lc].npheader; i++)
      {
        /* determine the nodal diameter */
        if(compare(&lcase[lc].pheader[i][5],"PHID", 4)==4)
        {
          sscanf(lcase[lc].pheader[i],"%*s %d", &nd);
          break;
        }
      }
      if(nd==0) /* single results */
      {
        if(lcase[lc].ictype[0]==2) /* vector */
        {
          ncomps=3;
          if(copiedNodeSets.type[set]==3)
	  {
            rectcyl(2, csab, anz->n, node, lcase, lc, 'd');
            for (j=0; j<ncomps; j++)
            {
              for (i=0; i<copiedNodeSets.anz_n[set]; i++)
              {
                nm=copiedNodeSets.mnod[set][i];
                ns=copiedNodeSets.snod[set][i];
                lcase[lc].dat[j][ns]=lcase[lc].dat[j][nm];
              }
            }
            lcase[lc].ncomps=ncomps;
            /* transform the displacements from the cylindrical system back into the cartesian system */
            rectcyl(-2, csab, anz->n, node, lcase, lc, 'd');
	  }
          else
	  {
            p1[0] = csab[0];
            p1[1] = csab[1];
            p1[2] = csab[2];
            p2[0] = csab[3];
            p2[1] = csab[4];
            p2[2] = csab[5];
            v_result( p1, p2, p1p2 );
            p2[0] = 0;
            p2[1] = 0;
            p2[2] = 0;
            v_result(p1p2,p2, p1);
            l=2.;
            v_scal( &l, p1p2, p1p2 );
            v_add( p1,  p1p2, p2 );
        
            ph[0] = p2[0]+p1[0];
            ph[1] = p2[1]+p1[1];
            ph[2] = p2[2]+p1[2];
        
            fi=v_sprod(p1p2, p1p2);
        
            for (i=0; i<copiedNodeSets.anz_n[set]; i++)
            {
              nm=copiedNodeSets.mnod[set][i];
              ns=copiedNodeSets.snod[set][i];
              el[0]=(ph[0]-2*lcase[lc].dat[0][nm]) ;
              el[1]=(ph[1]-2*lcase[lc].dat[1][nm]) ;
              el[2]=(ph[2]-2*lcase[lc].dat[2][nm]) ;
              l=v_sprod(el, p1p2);
              h=l/fi;
              lcase[lc].dat[0][ns]= h*p1p2[0]+lcase[lc].dat[0][nm];
              lcase[lc].dat[1][ns]= h*p1p2[1]+lcase[lc].dat[1][nm];
              lcase[lc].dat[2][ns]= h*p1p2[2]+lcase[lc].dat[2][nm];
            }
            lcase[lc].ncomps=ncomps;
          }
        }
        else if(lcase[lc].ictype[0]==4) /* matrix */
        {
          rectcyl(2, csab, anz->n, node, lcase, lc, 's');
          ncomps=6;

          for (j=0; j<ncomps; j++)
          {
            for (i=0; i<copiedNodeSets.anz_n[set]; i++)
            {
              nm=copiedNodeSets.mnod[set][i];
              ns=copiedNodeSets.snod[set][i];
              lcase[lc].dat[j][ns]=lcase[lc].dat[j][nm];
            }
          }
          lcase[lc].ncomps=ncomps;

          /* transform the displacements from the cylindrical system back into the cartesian system */
          rectcyl(-2, csab, anz->n, node, lcase, lc, 's');
        }
        else if(lcase[lc].ictype[0]==12) /* vector with 3 amplitudes and 3 phase-angles in degree */
        {
          rectcyl(2, csab, anz->n, node, lcase, lc, 'd');
          ncomps=3;

          for (j=0; j<ncomps; j++)
          {
            for (i=0; i<copiedNodeSets.anz_n[set]; i++)
            {
              nm=copiedNodeSets.mnod[set][i];
              ns=copiedNodeSets.snod[set][i];
              lcase[lc].dat[j][ns]=lcase[lc].dat[j][nm];
              lcase[lc].dat[j+3][ns]=lcase[lc].dat[j+3][nm];
      	    }
          }
          lcase[lc].ncomps=ncomps+3;

          /* transform the vector from the cylindrical system back into the cartesian system */
          rectcyl(-2, csab, anz->n, node, lcase, lc, 'd');
        }
        else if(lcase[lc].ictype[0]!=1)
        {
          for (j=0; j<lcase[lc].ncomps; j++)
	  {
            for (i=0; i<copiedNodeSets.anz_n[set]; i++)
            {
              ns=copiedNodeSets.snod[set][i];
              lcase[lc].dat[j][ns]=0;
            }
          }
        }
      }
      else /*cycsym */
      {
        sign=1.;
        /* real and imaginary part of results */
        for(i=0;i<lcase[lc].npheader; i++)
        {
          /* determine the axis */
          if(compare(&lcase[lc].pheader[i][5],"PAX", 3)==3)
          {
            sscanf(lcase[lc].pheader[i],"%*s %lf%lf%lf%lf%lf%lf", &csab[0], &csab[1], &csab[2], &csab[3], &csab[4], &csab[5]);
            //printf("%s\ncsab:%f %f %f      %f %f %f\n",lcase[lc].pheader[i], csab[0], csab[1], csab[2], csab[3], csab[4], csab[5]);
            sign=csab[3]-csab[0] + csab[4]-csab[1] + csab[5]-csab[2];
            break;
          }
        }

        theta=nd*copiedNodeSets.fi[set];
        ctl=cos(theta*-sign);
        stl=-sin(theta*-sign);

        /* its harmonic, search the second result */
        lc1=lc2=lc;
        //if(lc) { i=lc-1; while((lcase[i].step_number==lcase[lc].step_number)&&(i>=0)) i--; i++; }
        if(lc) { for (i=lc-1; i>=0; i--) { if(lcase[i].step_number!=lcase[lc].step_number) break; } i++; }
        else i=1;
        while((i<anz->l)&&(lcase[i].step_number==lcase[lc].step_number))
        {
          /* since real and imaginary part use different names since ccx_2.9 it is necessary to compare the 
             names only for the length excluding the last char if its a 'I' */
          compareChars=strlen(lcase[i].name)-1;
          for(k=compareChars;k>0; k--) if(lcase[i].name[k]!=' ') break;
          compareChars=k+1;
          if(lcase[i].name[compareChars-1]=='I') compareChars--;
          if(compareChars>7) compareChars--;
	  //printf("lcase[i].name:%s lcase[lc].name:%s compareChars:%d\n", lcase[i].name,lcase[lc].name,compareChars);
          if ((compare(lcase[i].name,lcase[lc].name,compareChars)==compareChars)&&(i!=lc))
          {
            if(i<lc) { lc1=i; lc2=lc; } else  { lc2=i; lc1=lc; }

            /* check if the data of the specified lcase (Dataset) are already available */
            if (!lcase[i].loaded)
            {
              if( readfrdblock( i, anz, node, lcase )==-1) 
              {
                printf("ERROR in copyEntity: Could not read data for Dataset:%d\n", i+1); 
              }
            }
            break;
          }
          i++;
        }

        if(lcase[lc1].ictype[0]==2) /* vector */
        {
          rectcyl(2, csab, anz->n, node, lcase, lc1, 'd');
          rectcyl(2, csab, anz->n, node, lcase, lc2, 'd');
          ncomps=3;

          for (j=0; j<ncomps; j++)
          {
            for (i=0; i<copiedNodeSets.anz_n[set]; i++)
            {
              nm=copiedNodeSets.mnod[set][i];
              ns=copiedNodeSets.snod[set][i];
              lcase[lc1].dat[j][ns] =  ctl*lcase[lc1].dat[j][nm]-stl*lcase[lc2].dat[j][nm];    
              lcase[lc2].dat[j][ns] =  stl*lcase[lc1].dat[j][nm]+ctl*lcase[lc2].dat[j][nm];    
	    }
          }
          lcase[lc1].ncomps=ncomps;
          lcase[lc2].ncomps=ncomps;

          /* transform the vector from the cylindrical system back into the cartesian system */
          rectcyl(-2, csab, anz->n, node, lcase, lc1, 'd');
          rectcyl(-2, csab, anz->n, node, lcase, lc2, 'd');
        }
        else if(lcase[lc1].ictype[0]==4) /* matrix */
        {
          rectcyl(2, csab, anz->n, node, lcase, lc1, 's');
          rectcyl(2, csab, anz->n, node, lcase, lc2, 's');
          ncomps=6;

          for (j=0; j<ncomps; j++)
          {
            for (i=0; i<copiedNodeSets.anz_n[set]; i++)
            {
              nm=copiedNodeSets.mnod[set][i];
              ns=copiedNodeSets.snod[set][i];
              lcase[lc1].dat[j][ns] =  ctl*lcase[lc1].dat[j][nm]-stl*lcase[lc2].dat[j][nm];    
              lcase[lc2].dat[j][ns] =  stl*lcase[lc1].dat[j][nm]+ctl*lcase[lc2].dat[j][nm];    
	    }
          }
          lcase[lc1].ncomps=ncomps;
          lcase[lc2].ncomps=ncomps;

          /* transform the matrix from the cylindrical system back into the cartesian system */
          rectcyl(-2, csab, anz->n, node, lcase, lc1, 's');
          rectcyl(-2, csab, anz->n, node, lcase, lc2, 's');
        }
        else if(lcase[lc1].ictype[0]==12) /* vector with 3 amplitudes and 3 phase-angles in degree */
        {
          if ( (lcase2 = (Datasets *)malloc( 2 * sizeof(Datasets))) == NULL ) printf("\n\n ERROR: malloc failed\n\n");
          for(i=0; i<2; i++)
	  {
            if ( (lcase2[i].dat = (float **)malloc( (6) * sizeof(float *))) == NULL )  printf("\n\n ERROR: malloc failure\n\n" );
            for(j=0; j<6; j++)
	    {
              if ( (lcase2[i].dat[j] = (float *)malloc( (anz->nmax+1) * sizeof(float))) == NULL ) printf("\n\n ERROR: malloc failure\n\n" );	               
	    }
	  }

          /* calc real and imaginary part */
          for (j=0; j<3; j++)
          {
            for (i=0; i<copiedNodeSets.anz_n[set]; i++)
            {
              nm=copiedNodeSets.mnod[set][i];
              lcase2[0].dat[j][nm]=lcase[lc1].dat[j][nm]*cos(lcase[lc1].dat[j+3][nm]*PI/180.);
              lcase2[1].dat[j][nm]=lcase[lc1].dat[j][nm]*sin(lcase[lc1].dat[j+3][nm]*PI/180.);
	    }
	  }

          /* into the cyl sys */
          rectcyl(2, csab, anz->n, node, lcase2, 0, 'd');
          rectcyl(2, csab, anz->n, node, lcase2, 1, 'd');

          for (j=0; j<3; j++)
          {
            for (i=0; i<copiedNodeSets.anz_n[set]; i++)
            {
              nm=copiedNodeSets.mnod[set][i];
              ns=copiedNodeSets.snod[set][i];
              lcase2[0].dat[j][ns] =  ctl*lcase2[0].dat[j][nm]-stl*lcase2[1].dat[j][nm];    
              lcase2[1].dat[j][ns] =  stl*lcase2[0].dat[j][nm]+ctl*lcase2[1].dat[j][nm];    
	    }
          }
          /* transform the vector from the cylindrical system back into the cartesian system */
          rectcyl(-2, csab, anz->n, node, lcase2, 0, 'd');
          rectcyl(-2, csab, anz->n, node, lcase2, 1, 'd');

          /* calc amplitude and phase */
          for (j=0; j<3; j++)
          {
            for (i=0; i<copiedNodeSets.anz_n[set]; i++)
            {
              ns=copiedNodeSets.snod[set][i];
              lcase[lc1].dat[j][ns] =  sqrt(lcase2[0].dat[j][ns]*lcase2[0].dat[j][ns]+lcase2[1].dat[j][ns]*lcase2[1].dat[j][ns]);
              lcase[lc1].dat[j+3][ns]=  ((atan2(lcase2[1].dat[j][ns],lcase2[0].dat[j][ns]))*180./PI);    
       	    }
          }
          lcase[lc1].ncomps=6;

          for(i=0; i<6; i++) { free(lcase2[0].dat[i]); free(lcase2[1].dat[i]); }
          free(lcase2[0].dat);
          free(lcase2[1].dat);
          free(lcase2);

        }
        else if(lcase[lc1].ictype[0]!=1)
        {
          for (j=0; j<lcase[lc1].ncomps; j++)
	  {
            for (i=0; i<copiedNodeSets.anz_n[set]; i++)
            {
              ns=copiedNodeSets.snod[set][i];
              lcase[lc1].dat[j][ns]=0;
            }
          }
	}
        if(lc1!=lc2)
        {
          if(lc==lc1) i=lc2; else i=lc1;
          calcDatasets( i, anz, node, lcase );
          recompileEntitiesInMenu(i);
        }
      }
    }
    else
    {
      printf(" ERROR: transformation not known: %d\n", copiedNodeSets.type[set] );
      return;
    }
  }
  for(i=0; i<lcase[lc].ncomps; i++)
  {
    lcase[lc].max[i]=-MAX_FLOAT;
    lcase[lc].min[i]=MAX_FLOAT;
    for(j=0; j<anz->n; j++)
    {
      if(node[node[j].nr].pflag==-1) continue;
      if (lcase[lc].dat[i][node[j].nr] >  lcase[lc].max[i])
      {  lcase[lc].max[i]=lcase[lc].dat[i][node[j].nr]; lcase[lc].nmax[i]=node[j].nr;}
      if (lcase[lc].dat[i][node[j].nr] <  lcase[lc].min[i])
      {  lcase[lc].min[i]=lcase[lc].dat[i][node[j].nr]; lcase[lc].nmin[i]=node[j].nr;}
    }
  }
}

/*------------------------------------------------------------------*/
/* copy                                                             */
/*------------------------------------------------------------------*/

int copy_set( int settrgt, char *trans, int setNr, int *dep_n, int *dep_e, int *dep_p, int *dep_l, int *dep_c, int *dep_s, int *dep_se, int *dep_sh, int *dep_S, int mastersetNr, int appendSetFlag )
{
  static int *lines=NULL;

  int   i,j,k,p,l,s,b,p1,p2,p3,p4,p5,p6,p7, se,nr,S=-1, transtyp=-1;
  int elnr, n, en[26], lc, ipuf;
  static Nodes *bufn=NULL;
  char name[MAX_LINE_LENGTH];
  char setname[MAX_LINE_LENGTH];
  double csab[6], fi=0.;

  char  pkt1[MAX_LINE_LENGTH], pkt2[MAX_LINE_LENGTH];
  int   pnr1, pnr2;

  /* copy nodes */
  if(set[setNr].anz_n>0)
  {
    descalNodes ( anz->n, node, scale );
    /* free the additional midside-nodes for higher order elements */
    for(i=anz->orign; i<anz->n; i++) node[node[i].nr].pflag=-1;
    anz->n= anz->orign;
    anz->nmax=anz->orignmax;

    if ( (bufn = (Nodes *)realloc((Nodes *)bufn, set[setNr].anz_n * sizeof(Nodes))) == NULL )
    {  printf("\n\n ERROR: malloc failed in copy_set()\n\n") ; return(-1); }
    j=set[setNr].anz_n;

    for (i=0; i<j; i++)
    {
      p=set[setNr].node[i];
      if (p==-1)
      {
        errMsg(" node-nr:%d is undefined\n", set[setNr].node[i] );
        return(-1);
      }
      bufn[i].nx=node[p].nx;
      bufn[i].ny=node[p].ny;
      bufn[i].nz=node[p].nz;
    }
    transtyp=transform( trans, set[setNr].anz_n, bufn );
    if (transtyp==-1) return(-1);

    /* create new nodes */
    n=anz->orignmax+anz->noffs;
    for (i=0; i<j; i++)
    {
      n++;
      if(printFlag) printf(" create n:%d %lf %lf %lf from n:%d\n", n, bufn[i].nx, bufn[i].ny, bufn[i].nz, set[setNr].node[i] );
      b=nod( anz, &node, 1, n, bufn[i].nx, bufn[i].ny, bufn[i].nz, 0 );
      dep_n[set[setNr].node[i]] = node[b].nr; /* remember the index of the new node */
      seta( settrgt, "n", node[b].nr );

      /* add to all sets were the orig is a member */
      if(appendSetFlag) for(se=1; se<anz->sets; se++) if((se!=setNr)&&(se!=mastersetNr)&&(set[se].name!=(char *)NULL)&&(set[se].name[0]!='-'))  if(!set[se].type) if( getIndex(&set[se].node, set[se].anz_n, set[setNr].node[i]) >-1) { seta( se, "n", anz->nmax );  }
    }

    if(anz->l)
    {
      csab[0]=0.;
      csab[1]=0.;
      csab[2]=0.;
      csab[3]=0.;
      csab[4]=0.;
      csab[5]=0.;
      if((transtyp==3)||(transtyp==5))
      {
        i=sscanf( trans, "%*s%s%s%lf", pkt1, pkt2, &fi );
        if(i>2) fi=fi*PI/180.; else fi=0;
        if(i>1) pnr2=getPntNr( pkt2 ); else pnr2=-1;
    
        if(pkt1[0]=='x') { csab[3]=1.; if(i>1) fi=atof(pkt2)*PI/180.; }
        else if(pkt1[0]=='y') { csab[4]=1.; if(i>1) fi=atof(pkt2)*PI/180.; }
        else if(pkt1[0]=='z') { csab[5]=1.; if(i>1) fi=atof(pkt2)*PI/180.; }
        else
        {
          pnr1=getPntNr( pkt1 );
          pnr2=getPntNr( pkt2 );

          if (pnr1==-1)
          {
            errMsg(" ERROR: Point1 (%s) is undefined\n", pkt1 );
            return(-1);
          }
          else
          {
            csab[0] = point[pnr1].px*scale->w+scale->x;
            csab[1] = point[pnr1].py*scale->w+scale->y;
            csab[2] = point[pnr1].pz*scale->w+scale->z;
          }

          if (pnr2==-1)
          {
            csab[3] = point[pnr1].px*scale->w+scale->x;
            csab[4] = point[pnr1].py*scale->w+scale->y;
            csab[5] = point[pnr1].pz*scale->w+scale->z;
            if(pkt2[0]=='x') csab[3]+=1.;
            else if(pkt2[0]=='y') csab[4]+=1.;
            else if(pkt2[0]=='z') csab[5]+=1.;
            else
	    {
              errMsg(" ERROR: Point2 (%s) is undefined\n", pkt2 );
              return(-1);
	    }
          }
          else
          {
            csab[3] = point[pnr2].px*scale->w+scale->x;
            csab[4] = point[pnr2].py*scale->w+scale->y;
            csab[5] = point[pnr2].pz*scale->w+scale->z;
          }
        }
      }

      //printf("csab: %f %f %f   %f %f %f\n",csab[0],csab[1],csab[2],csab[3],csab[4],csab[5]);

      /* fill the structure for copying Datasets from master to slave on demand */
      n=copiedNodeSets->sets;
      copiedNodeSets->sets++;

      if ( (copiedNodeSets->type= (int *)realloc((int *)copiedNodeSets->type, (n+1) * sizeof(int))) == NULL )
      {  printf("\n\n ERROR: realloc failed in copy_set()\n\n") ; return(-1); }
      if ( (copiedNodeSets->anz_n= (int *)realloc((int *)copiedNodeSets->anz_n, (n+1) * sizeof(int))) == NULL )
      {  printf("\n\n ERROR: realloc failed in copy_set()\n\n") ; return(-1); }
      if ( (copiedNodeSets->axis= (int **)realloc((int **)copiedNodeSets->axis, (n+1) * sizeof(int *))) == NULL )
      {  printf("\n\n ERROR: realloc failed in copy_set()\n\n") ; return(-1); }
      if ( (copiedNodeSets->axis[n]= (int *)malloc( (6) * sizeof(int))) == NULL )
      {  printf("\n\n ERROR: realloc failed in copy_set()\n\n") ; return(-1); }
      if ( (copiedNodeSets->mnod= (int **)realloc((int **)copiedNodeSets->mnod, (n+1) * sizeof(int *))) == NULL )
      {  printf("\n\n ERROR: realloc failed in copy_set()\n\n") ; return(-1); }
      if ( (copiedNodeSets->mnod[n]= (int *)malloc((set[setNr].anz_n) * sizeof(int))) == NULL )
      {  printf("\n\n ERROR: realloc failed in copy_set()\n\n") ; return(-1); }
      if ( (copiedNodeSets->snod= (int **)realloc((int **)copiedNodeSets->snod, (n+1) * sizeof(int *))) == NULL )
      {  printf("\n\n ERROR: realloc failed in copy_set()\n\n") ; return(-1); }
      if ( (copiedNodeSets->snod[n]= (int *)malloc( (set[setNr].anz_n) * sizeof(int))) == NULL )
      {  printf("\n\n ERROR: realloc failed in copy_set()\n\n") ; return(-1); }
      if ( (copiedNodeSets->fi= (double *)realloc((double *)copiedNodeSets->fi, (n+1) * sizeof(double))) == NULL )
      {  printf("\n\n ERROR: realloc failed in copy_set()\n\n") ; return(-1); }

      copiedNodeSets->type[n]=transtyp;
      copiedNodeSets->anz_n[n]=set[setNr].anz_n;
      copiedNodeSets->fi[n]=fi;
      for(i=0; i<6; i++) copiedNodeSets->axis[n][i]= csab[i];
      for(i=0; i<set[setNr].anz_n; i++)
      {
        copiedNodeSets->mnod[n][i]=set[setNr].node[i];
        copiedNodeSets->snod[n][i]=dep_n[set[setNr].node[i]];
      }
    }

    /* expand all loaded Datasets by the new nodes */
    /* all datasets have to be read. Only then results can be mapped (later this should be done on demand) */
    for (lc=0; lc<anz->l; lc++)
    {
      if (lcase[lc].loaded)
      {
        copyDatasetToNodes(anz, node, lcase, lc, copiedNodeSets[0]);
        calcDatasets( lc, anz, node, lcase );
        recompileEntitiesInMenu(lc);
      }
    }

    /* new total number of mesh nodes */
    anz->orignmax = anz->nmax;
    anz->orign = anz->n;

    scalNodes ( anz->n, node, scale );

    /* new midnodes for drawing if no elements are to copy */
    adjustDrawNodes(1);
    getElemNormalen( e_enqire, node, anz->e );
    makeSurfaces();
    realloc_colNr();
    updateDispLists();
  }


  /* copy elements */
  if(set[setNr].anz_e>0)
  {
    /* free the additional midside-nodes for higher order elements */
    for(i=anz->orign; i<anz->n; i++) node[node[i].nr].pflag=-1;
    anz->n= anz->orign;
    anz->nmax=anz->orignmax;

    j=set[setNr].anz_e;
    for (i=0; i<j; i++)
    {
      elnr=set[setNr].elem[i];

      ipuf=0;
      if (e_enqire[ elnr ].type == 4) ipuf = 20; /* HEX20 */
      else if (e_enqire[ elnr ].type == 5) ipuf = 15; /* PENTA15 */
      else if (e_enqire[ elnr ].type == 6) ipuf = 10; /* TET10 */
      if(ipuf)
        for (n=0; n<ipuf; n++) en[n]=dep_n[e_enqire[ elnr ].nod[n]];
      else
      {
        if(transtyp==5)
	{
          ipuf=0;
          if (e_enqire[ elnr ].type == 1) ipuf = 8;  /* HEXA8 */
          else if (e_enqire[ elnr ].type == 2) ipuf = 6;  /* PENTA6 */
          else if (e_enqire[ elnr ].type == 3) ipuf = 4;  /* TET4 */
          else if ((e_enqire[ elnr ].type == 7)||(e_enqire[ elnr ].type == 8)) ipuf = 3;  /* TRI3  */
          else if ((e_enqire[ elnr ].type == 9)||(e_enqire[ elnr ].type == 10)) ipuf = 4;  /* QUAD4 */
          else if ((e_enqire[ elnr ].type == 11)||(e_enqire[ elnr ].type == 12)) ipuf = 2; /* BEAM */
          if(ipuf)
	  {
            k=ipuf-1;
            for (n=0; n<ipuf; n++) en[k--]=dep_n[e_enqire[ elnr ].nod[n]];
	  }

          if (e_enqire[ elnr ].type == 8)  /* TRI6  */
	  {
            en[3]=dep_n[e_enqire[ elnr ].nod[4]];
            en[4]=dep_n[e_enqire[ elnr ].nod[3]];
            en[5]=dep_n[e_enqire[ elnr ].nod[5]];
	  }
          else if (e_enqire[ elnr ].type == 10) /* QUAD8 */
	  {
            en[4]=dep_n[e_enqire[ elnr ].nod[6]];
            en[6]=dep_n[e_enqire[ elnr ].nod[4]];
            en[5]=dep_n[e_enqire[ elnr ].nod[5]];
            en[7]=dep_n[e_enqire[ elnr ].nod[7]];
	  }
          else if (e_enqire[ elnr ].type == 12) /* BEAM3 */
            en[2]=dep_n[e_enqire[ elnr ].nod[2]];
	}
	else
	{
          if (e_enqire[ elnr ].type == 1) ipuf = 8;  /* HEXA8 */
          else if (e_enqire[ elnr ].type == 2) ipuf = 6;  /* PENTA6 */
          else if (e_enqire[ elnr ].type == 3) ipuf = 4;  /* TET4 */
          //else if (e_enqire[ elnr ].type == 4) ipuf = 20; /* HEX20 */
          //else if (e_enqire[ elnr ].type == 5) ipuf = 15; /* PENTA15 */
          //else if (e_enqire[ elnr ].type == 6) ipuf = 10; /* TET10 */
          else if (e_enqire[ elnr ].type == 7) ipuf = 3;  /* TRI3  */
          else if (e_enqire[ elnr ].type == 8) ipuf = 6;  /* TRI6  */
          else if (e_enqire[ elnr ].type == 9) ipuf = 4;  /* QUAD4 */
          else if (e_enqire[ elnr ].type == 10) ipuf = 8; /* QUAD8 */
          else if (e_enqire[ elnr ].type == 11) ipuf = 2; /* BEAM */
          else if (e_enqire[ elnr ].type == 12) ipuf = 3; /* BEAM3 */
          if(ipuf)
            for (n=0; n<ipuf; n++) en[n]=dep_n[e_enqire[ elnr ].nod[n]];
	}
      }
      elem_define( anz->emax+1, e_enqire[ elnr ].type, en, 1, e_enqire[ elnr ].attr );

      dep_e[elnr] = anz->emax; /* remember the index of the new elem */
      seta( settrgt, "e", anz->emax );

      /* add to all sets were the orig is a member */
      if(appendSetFlag) for(se=1; se<anz->sets; se++) if((se!=setNr)&&(se!=mastersetNr)&&(set[se].name!=(char *)NULL)&&(set[se].name[0]!='-')) if(!set[se].type) if( getIndex(&set[se].elem, set[se].anz_e, set[setNr].elem[i]) >-1) { seta( se, "e", anz->emax );  }
    }
    /* new midnodes for drawing */
    if (transtyp==5) elemChecker( anz->emax+1, node, e_enqire);
    adjustDrawNodes(1);
    getElemNormalen( e_enqire, node, anz->e );
    makeSurfaces();
    realloc_colNr();
    updateDispLists();
  }


  /* copy points */
  if(set[setNr].anz_p>0)
  {
    if ( (bufn = (Nodes *)realloc((Nodes *)bufn, set[setNr].anz_p * sizeof(Nodes))) == NULL )
    {  printf("\n\n ERROR: malloc failed in copy_set()\n\n") ; return(-1); }
    j=set[setNr].anz_p;
    for (i=0; i<j; i++)
    {
      p=set[setNr].pnt[i];
      if (p==-1)
      {
        errMsg(" ERROR: Pointnr:%d is undefined\n", set[setNr].pnt[i] );
        return(-1);
      }
      bufn[i].nx=point[p].px*scale->w+scale->x;
      bufn[i].ny=point[p].py*scale->w+scale->y;
      bufn[i].nz=point[p].pz*scale->w+scale->z;
    }
    transtyp=transform( trans, set[setNr].anz_p, bufn );
    if (transtyp==-1) return(-1);

    for (i=0; i<j; i++)
    {
      p= getNewName( name, "p" );
      if ( p == -1 )
        { printf(" ERROR: in copy, could not create new point\n"); return(-1); }
      else if(printFlag) printf(" create p:%s %lf %lf %lf from p:%s\n", name, bufn[i].nx, bufn[i].ny, bufn[i].nz, point[set[setNr].pnt[i]].name );
      p=pnt( name, bufn[i].nx, bufn[i].ny, bufn[i].nz, 1 );
      if ( p <0 )
        { printf(" ERROR: in copy, could not create new point\n"); return(-1); }
      dep_p[set[setNr].pnt[i]] = p; /* remember the index of the new point */
      seta( settrgt, "p", p );

      /* add to all sets were the orig is a member */
      if(appendSetFlag) for(se=1; se<anz->sets; se++) if((se!=setNr)&&(se!=mastersetNr)&&(set[se].name!=(char *)NULL)&&(set[se].name[0]!='-'))  if(!set[se].type) if( getIndex(&set[se].pnt, set[se].anz_p, set[setNr].pnt[i]) >-1) { seta( se, "p", p );  }
    }
  }

  /* copy the lines */
  if(set[setNr].anz_l>0)
  {
    j=set[setNr].anz_l;
    for (i=0; i<j; i++)
    {
      l=set[setNr].line[i];
      if (l==-1)
      {
        errMsg(" Linenr:%d is undefined\n", set[setNr].line[i] );
        return(-1);
      }
      l= getNewName( name, "l" );
      if ( l == -1 )
        { printf("copy: could not create new line\n"); return(-1); }
      p1=line[set[setNr].line[i]].p1;
      p2=line[set[setNr].line[i]].p2;
      p3=line[set[setNr].line[i]].trk;
      if (line[set[setNr].line[i]].typ==' ')     
        l=line_i( name, dep_p[p1], dep_p[p2], 0, line[set[setNr].line[i]].div, line[set[setNr].line[i]].bias, 0 ); 
      else if (line[set[setNr].line[i]].typ=='a') 
        l=line_i( name, dep_p[p1], dep_p[p2], dep_p[p3], line[set[setNr].line[i]].div, line[set[setNr].line[i]].bias, 'a' );
      else if (line[set[setNr].line[i]].typ=='s')
      {
        /* copy set */
        se= getNewName( setname, "se" );
        if ( se == -1 )
          { printf("copy: could not create new set\n"); return(-1); }
        if(set[p3].type==1) se=pre_seta( setname, "is", 0 );
        else se=pre_seta( setname, "i", 0 );
        if ( se <0 )
          { printf("copy: could not create new set\n"); return(-1); }
  
        for (k=0; k<set[p3].anz_p; k++)
        {
          p=dep_p[set[p3].pnt[k]];
          if( getPntNr(point[p].name)<0 )
          { printf("copy: could not find dep_p:%s from p:%s (from set:%s)\n", point[p].name, point[set[p3].pnt[k]].name, set[p3].name ); return(-1); }
          seta( se, "p", p );
        }
        dep_se[p3] = se; /* remember the index of the new set */
        seta( settrgt, "r", se );

        l=line_i( name, dep_p[p1], dep_p[p2], dep_se[p3], line[set[setNr].line[i]].div, line[set[setNr].line[i]].bias, 's' );
      }
      if ( l <0 )
      { printf("copy: could not create new line\n"); return(-1); }
      line[l].etyp =line[set[setNr].line[i]].etyp;
      line[l].eattr=line[set[setNr].line[i]].eattr;
      dep_l[set[setNr].line[i]] = l; /* remember the index of the new line */
      seta( settrgt, "l", l );

      /* add to all sets were the orig is a member */
      if(appendSetFlag) for(se=1; se<anz->sets; se++) if((se!=setNr)&&(se!=mastersetNr)&&(set[se].name!=(char *)NULL)&&(set[se].name[0]!='-')) if(!set[se].type) if( getIndex(&set[se].line, set[se].anz_l, set[setNr].line[i]) >-1) { seta( se, "l", l );  }
    }
  }

  /* copy the lcmb */
  if(set[setNr].anz_c>0)
  {
    j=set[setNr].anz_c;
    for (i=0; i<j; i++)
    {
      p=set[setNr].lcmb[i];
      if (p==-1)
      {
        errMsg(" Lcmbnr:%d is undefined\n", set[setNr].lcmb[i] );
        return(-1);
      }
      p= getNewName( name, "c" );
      if ( p == -1 )
        { printf("copy: could not create new lcmb\n"); return(-1); }

      if ((lines = (int *)malloc( (lcmb[set[setNr].lcmb[i]].nl)*sizeof(int)) ) == NULL )
      { printf("ERROR: malloc failure in copySet()\n"); return(-1); }
      for (k=0; k<lcmb[set[setNr].lcmb[i]].nl; k++)
      {
        lines[k]= dep_l[lcmb[set[setNr].lcmb[i]].l[k]];
      }
      if ( getNewName( name, "c" ) == -1 )
      { printf("copy: lcmb could not be created\n"); }
      l=lcmb_i( name, 0, lcmb[set[setNr].lcmb[i]].nl, lcmb[set[setNr].lcmb[i]].o, lines );
      free(lines);
      if (l <0 )
        { printf("copy: lcmb could not be created\n"); }
      dep_c[set[setNr].lcmb[i]] = l; /* remember the index of the new lcmb */
      seta( settrgt, "c", l );

      /* add to all sets were the orig is a member */
      if(appendSetFlag) for(se=1; se<anz->sets; se++) if((se!=setNr)&&(se!=mastersetNr)&&(set[se].name!=(char *)NULL)&&(set[se].name[0]!='-')) if(!set[se].type) if( getIndex(&set[se].lcmb, set[se].anz_c, set[setNr].lcmb[i]) >-1) { seta( se, "c", l );  }
    }
  }

  /* copy nurs */
  if(set[setNr].anz_nurs>0)
  {
    b=set[setNr].anz_nurs;
    for (s=0; s<b; s++)
    {
      S= getNewName( name, "S" );
      if ( S == -1 )
        { printf("copy: could not create new nurs\n"); return(-1); }

      S=set[setNr].nurs[s];

      if ((nurbs = (Nurbs *)realloc( (Nurbs *)nurbs, (anzGeo->nurs+1)*sizeof(Nurbs)) ) == NULL )
      { printf("\n\nERROR: realloc failure in Nurs, nurbs:%s not installed\n\n", name); return(-1); }

      nr=anzGeo->nurs;
      hashNurs( sumAsci, name, nr );
      anzGeo->nurs++;
      if((nurbs[nr].name= (char *)malloc((strlen(name)+1)*sizeof(char))) == NULL )
      { printf("ERROR: malloc failed\n\n" ); return(-1); }
      strcpy(nurbs[nr].name, name);

      if(printFlag) printf("copy %s to %s\n",nurbs[S].name,nurbs[nr].name);

      nurbs[nr].u_exp = nurbs[S].u_exp;
      nurbs[nr].v_exp = nurbs[S].v_exp;
      nurbs[nr].u_npnt= nurbs[S].u_npnt;
      nurbs[nr].v_npnt= nurbs[S].v_npnt;
      nurbs[nr].u_nknt= nurbs[S].u_nknt;
      nurbs[nr].v_nknt= nurbs[S].v_nknt;
      nurbs[nr].u_stride= nurbs[S].u_stride;
      nurbs[nr].v_stride= nurbs[S].v_stride;

      if ( (nurbs[nr].uknt = (GLfloat *)malloc( (nurbs[S].u_nknt+1) * sizeof(GLfloat))) == NULL )
        printf("\n\n ERROR: realloc failed uknt\n\n");
      if ( (nurbs[nr].vknt = (GLfloat *)malloc( (nurbs[S].v_nknt+1) * sizeof(GLfloat))) == NULL )
        printf("\n\n ERROR: realloc failed vknt\n\n");
      for(i=0; i<nurbs[nr].u_nknt; i++) { nurbs[nr].uknt[i]=nurbs[S].uknt[i]; }
      for(i=0; i<nurbs[nr].v_nknt; i++) { nurbs[nr].vknt[i]=nurbs[S].vknt[i]; }

      if ( (nurbs[nr].ctlpnt =
        (int **)malloc(  (nurbs[nr].u_npnt+1) * sizeof(int *))) == NULL )
        printf("\n\n ERROR: malloc failed ctlpnt\n\n");
      for (i=0; i<nurbs[nr].u_npnt; i++)
      {
        if ( (nurbs[nr].ctlpnt[i] =
          (int *)malloc(  (nurbs[nr].v_npnt+1) * sizeof( int ))) == NULL )
          printf("\n\n ERROR: malloc failed ctlpnt[i]\n\n");
        for (j=0; j<nurbs[nr].v_npnt; j++)
          nurbs[nr].ctlpnt[i][j] = dep_p[nurbs[S].ctlpnt[i][j]];
      }

      if ( (nurbs[nr].weight =
        (float **)malloc(  (nurbs[nr].u_npnt+1) * sizeof(float *))) == NULL )
        printf("\n\n ERROR: malloc failed weight\n\n");
      for (i=0; i<nurbs[nr].u_npnt; i++)
      {
        if ( (nurbs[nr].weight[i] =
          (float *)malloc(  (nurbs[nr].v_npnt+1) * sizeof(float))) == NULL )
          printf("\n\n ERROR: malloc failed weight[i]\n\n");
        for (j=0; j<nurbs[nr].v_npnt; j++)
          nurbs[nr].weight[i][j] =nurbs[S].weight[i][j];
      }

      nurbs[nr].ctlarray=(GLfloat *)NULL;
      nurbs[nr].endFlag=1;       
      nurbs[nr].type=GL_MAP2_VERTEX_4;       
  
      /* additional variables for the trimming */
      nurbs[nr].trimFlag=0;
      nurbs[nr].patches=0;
      nurbs[nr].nc=NULL;
      nurbs[nr].uv=NULL;
      nurbs[nr].xyz=NULL;
      nurbs[nr].np=NULL;
      nurbs[nr].umax=NULL;
      nurbs[nr].vmax=NULL;
      nurbs[nr].vstep=NULL;
      nurbs[nr].ustep=NULL;
      nurbs[nr].Nurb = (GLUnurbsObj *)gluNewNurbsRenderer();
      nurbs[nr].nurbsType=1;
      nurbs[nr].uvflipped=NULL;
      nurbs[nr].sum_ambiguousPnts=NULL;
      for (i=0; i<anz->sets; i++)
      {
        if ( set[i].flag=='o') seta( i, "S", nr );
      }
      repNurs(nr);

      dep_S[S] = nr; 
      seta( settrgt, "S", nr );

      /* add to all sets were the orig is a member */
      if(appendSetFlag) for(se=1; se<anz->sets; se++) if((se!=setNr)&&(se!=mastersetNr)&&(set[se].name!=(char *)NULL)&&(set[se].name[0]!='-')) if(!set[se].type) if( getIndex(&set[se].nurs, set[se].anz_nurs, set[setNr].nurs[i]) >-1) { seta( se, "S", nr );  }
    }
  }

  /* copy the shapes */
  if(set[setNr].anz_sh>0)
  {
    j=set[setNr].anz_sh;
    for (i=0; i<j; i++)
    {
      if (set[setNr].shp[i]==-1)
      {
        errMsg(" Shapenr:%d is undefined\n", set[setNr].shp[i] );
        return(-1);
      }
      l=-1;

      if((shape[set[setNr].shp[i]].type==0)||(shape[set[setNr].shp[i]].type==1))
      {
        l= getNewName( name, "sh" );
        if ( l == -1 )
        { printf("copy: could not create new shape\n"); return(-1); }
        p1=shape[set[setNr].shp[i]].p[0];
        p2=shape[set[setNr].shp[i]].p[1];
        p3=shape[set[setNr].shp[i]].p[2];
        l=shape_i( name, shape[set[setNr].shp[i]].type, dep_p[p1], dep_p[p2], dep_p[p3], 0, 0, 0, 0 );
      } 
      else if(shape[set[setNr].shp[i]].type==2)
      {
        l= getNewName( name, "sh" );
        if ( l == -1 )
        { printf("copy: could not create new shape\n"); return(-1); }
        p1=shape[set[setNr].shp[i]].p[0];
        p2=shape[set[setNr].shp[i]].p[1];
        p3=shape[set[setNr].shp[i]].p[2];
        p4=shape[set[setNr].shp[i]].p[3];
        l=shape_i( name, shape[set[setNr].shp[i]].type, dep_p[p1], dep_p[p2], dep_p[p3], dep_p[p4], 0, 0, 0 );
      } 
      else if(shape[set[setNr].shp[i]].type==3)
      {
        l= getNewName( name, "sh" );
        if ( l == -1 )
        { printf("copy: could not create new shape\n"); return(-1); }
        p1=shape[set[setNr].shp[i]].p[0];
        p2=shape[set[setNr].shp[i]].p[1];
        p3=shape[set[setNr].shp[i]].p[2];
        p4=shape[set[setNr].shp[i]].p[3];
        p5=shape[set[setNr].shp[i]].p[4];
        p6=shape[set[setNr].shp[i]].p[5];
        p7=shape[set[setNr].shp[i]].p[6];
        l=shape_i( name, shape[set[setNr].shp[i]].type, dep_p[p1], dep_p[p2], dep_p[p3], dep_p[p4], dep_p[p5], dep_p[p6],  dep_p[p7] );
      } 
      else if(shape[set[setNr].shp[i]].type==4)
      {
        p1=shape[set[setNr].shp[i]].p[0];
        if(p1>-1)
          l=shape_i( nurbs[dep_S[p1]].name, shape[set[setNr].shp[i]].type, dep_S[p1], 0,0,0, 0,0,0);
      } 
      if ( l <0 )
      { printf("copy: could not create new shape\n"); return(-1); }
      dep_sh[set[setNr].shp[i]] = l; /* remember the index of the new shape */
      seta( settrgt, "sh", l );

      /* add to all sets were the orig is a member */
      if(appendSetFlag) for(se=1; se<anz->sets; se++) if((se!=setNr)&&(se!=mastersetNr)&&(set[se].name!=(char *)NULL)&&(set[se].name[0]!='-')) if(!set[se].type) if( getIndex(&set[se].shp, set[se].anz_sh, set[setNr].shp[i]) >-1) { seta( se, "sh", l );  }
    }
  }

  /* copy the surfs */
  if(set[setNr].anz_s>0)
  {
    j=set[setNr].anz_s;
    for (i=0; i<j; i++)
    {
      s=set[setNr].surf[i];
      if (s==-1)
      {
        errMsg(" surfnr:%d is undefined\n", set[setNr].surf[i] );
        return(-1);
      }
      
      if ((lines = (int *)malloc( (surf[set[setNr].surf[i]].nl)*sizeof(int)) ) == NULL )
      { printf("ERROR: malloc failure in copySet()\n"); return(-1); }
      for (k=0; k<surf[set[setNr].surf[i]].nl; k++)
      {
        if (surf[set[setNr].surf[i]].typ[k]=='l')  lines[k]=  dep_l[surf[set[setNr].surf[i]].l[k]];
        else                                       lines[k]=  dep_c[surf[set[setNr].surf[i]].l[k]];
      }
      if ( getNewName( name, "s" ) == -1 )
      { printf("Type s not known, surf can not be created\n"); }
      if(surf[s].sh==-1) S=-1; else if(surf[s].sh>-1) S=dep_sh[surf[s].sh];
      s=surface_i( name, surf[set[setNr].surf[i]].ori, S,
               surf[set[setNr].surf[i]].nl, surf[set[setNr].surf[i]].o, lines, surf[set[setNr].surf[i]].typ);
      free(lines);
      if ( s <0 )
        { printf("copy: could not create new surf\n"); return(-1); }
      surf[s].etyp =surf[set[setNr].surf[i]].etyp;
      surf[s].eattr=surf[set[setNr].surf[i]].eattr;
      dep_s[set[setNr].surf[i]] = s; /* remember the index of the new surf */
      seta( settrgt, "s", s );

      /* add to all sets were the orig is a member */
      if(appendSetFlag) for(se=1; se<anz->sets; se++) if((se!=setNr)&&(se!=mastersetNr)&&(set[se].name!=(char *)NULL)&&(set[se].name[0]!='-')) if(!set[se].type) if( getIndex(&set[se].surf, set[se].anz_s, set[setNr].surf[i]) >-1) { seta( se, "s", s );  }
    }
  }

  /* copy the bodys */
  if(set[setNr].anz_b>0)
  {
    j=set[setNr].anz_b;
    for (i=0; i<j; i++)
    {
      b=set[setNr].body[i];
      if (b==-1)
      {
        errMsg(" bodynr:%d is undefined\n", set[setNr].body[i] );
        return(-1);
      }
      
      if ((lines = (int *)malloc((body[set[setNr].body[i]].ns)*sizeof(int)) ) == NULL )
      { printf("ERROR: malloc failure in copySet()\n"); return(-1); }
      for (k=0; k<body[set[setNr].body[i]].ns; k++)
      {
        lines[k]= dep_s[body[set[setNr].body[i]].s[k]];
      }
      if ( getNewName( name, "b" ) == -1 )
      { printf("Type b not known, body can not be created\n"); }
      b=gbod_i( name, -1, body[set[setNr].body[i]].ns, body[set[setNr].body[i]].o, lines );
      free(lines);
      if ( b <0 )
        { printf("copy: could not create new body\n"); return(-1); }
      body[b].etyp =body[set[setNr].body[i]].etyp;
      body[b].eattr=body[set[setNr].body[i]].eattr;
      seta( settrgt, "b", b );

      /* add to all sets were the orig is a member */
      if(appendSetFlag) for(se=1; se<anz->sets; se++) if((se!=setNr)&&(se!=mastersetNr)&&(set[se].name!=(char *)NULL)&&(set[se].name[0]!='-')) if(!set[se].type) if( getIndex(&set[se].body, set[se].anz_b, set[setNr].body[i]) >-1) { seta( se, "b", b );  }
    }
  }
  if ( (bufn = (Nodes *)realloc((Nodes *)bufn, 1 * sizeof(Nodes))) == NULL )
    printf("\n\n ERROR: malloc failed in copy_set()\n\n")  ;

  return(transtyp);
}


void pre_swep( char *record )
{
  int  i,j,k,n,b,e,nn;
  int  length, se, lc, anz_e, setNr, setcopy, settrgt, mastersetNr, cnset=0;
  char setname[MAX_LINE_LENGTH], targetnam[MAX_LINE_LENGTH];
  int  l=0, p1=0,p2=0,p3,transtyp,ldiv=0;
  int  appendSetFlag=0, genNurbsFlag=0;
  int  setNtmp, setPtmp;
  char addDispFlagLocal=0;
  
  char  name[MAX_LINE_LENGTH], pkt1[MAX_LINE_LENGTH], pkt2[MAX_LINE_LENGTH], buf[MAX_LINE_LENGTH];
  char  p1name[MAX_LINE_LENGTH], p2name[MAX_LINE_LENGTH];
  static int   **dep_n, *dep_e, *dep_p, *dep_l, *dep_c, *dep_s, *dep_lp, *dep_sl, *dep_sc, *dep_se, *dep_sh, *dep_S;
  static char *lori=NULL, *types=NULL;
  static int *lines=NULL, *s=NULL;

  int ipuf,en[26];
  
  int sweepType=0; // 0==NoNurbs 1==rot, 2==tra 

  
  sscanf( record, "%s %s %s", setname, targetnam, name );

  /* evaluate the last argument */
  i=strlen(record);
  while((record[--i]!=' ')&&(i>-1));
  if(record[i+1]=='a') appendSetFlag=1;
  if(record[i+1]=='n') genNurbsFlag=1;
  if(strlen(&record[i+1])>1)
  {
    if(record[i+2]=='a') appendSetFlag=1;
    if(record[i+2]=='n') genNurbsFlag=1;
  }

  operateAlias( setname, "se" );
  setNr=getSetNr( setname );
  if (setNr<0)
  {
    errMsg(" Set (%s) is undefined\n", setname );
    goto no_swep;
  }
  operateAlias( targetnam, "se" );
  settrgt=pre_seta( targetnam, "i", 0 );
  if (settrgt<0)
  {
    errMsg(" Set (%s) is undefined\n", targetnam );
    goto no_swep;
  }

  /* determine the division of the new lines */
  if ((compare(name, "tra", 3) == 3)||(compare(name, "TRA", 3)))
  {
    /* This nurbs are not perfect plane for a plane situation, sometimes problematic.
       The user creates a nurbs better with command "nurs ! set" */
    //if(genNurbsFlag) sweepType = 2;
    sscanf( record, "%*s%*s%*s%*s%*s%*s%d", &ldiv );
  }
  else if ((compare(name, "sca", 3) == 3)||(compare(name, "SCA", 3)))
     sscanf( record, "%*s%*s%*s%*s%*s%*s%d", &ldiv );
  else if ((compare(name, "rot", 3) == 3)||(compare(name, "ROT", 3)))
  {
    if(genNurbsFlag) sweepType = 1;
    i=sscanf( record, "%*s%*s%*s%*s%s", pkt2 );
    if((getPntNr(pkt2)>-1)||(pkt2[0]=='x')||(pkt2[0]=='y')||(pkt2[0]=='z')) sscanf( record, "%*s%*s%*s%*s%*s%*s%d", &ldiv );
    else sscanf( record, "%*s%*s%*s%*s%*s%d", &ldiv );
  }
  else if ((compare(name, "rad", 3) == 3)||(compare(name, "RAD", 3)))
  {
    i=sscanf( record, "%*s%*s%*s%*s%s", pkt2 );
    if((getPntNr(pkt2)>-1)||(pkt2[0]=='x')||(pkt2[0]=='y')||(pkt2[0]=='z')) sscanf( record, "%*s%*s%*s%*s%*s%*s%d", &ldiv );
    else sscanf( record, "%*s%*s%*s%*s%*s%d", &ldiv );
  }
  else if ((compare(name, "mir", 3) == 3)||(compare(name, "MIR", 3)))
  {
    i=sscanf( record, "%*s%*s%*s%*s%s", pkt2 );
    if((getPntNr(pkt2)>-1)||(pkt2[0]=='x')||(pkt2[0]=='y')||(pkt2[0]=='z')) sscanf( record, "%*s%*s%*s%*s%*s%d", &ldiv );
    else sscanf( record, "%*s%*s%*s%*s%d", &ldiv );
  }
  else if ((compare(name, "nor", 3) == 3)||(compare(name, "NOR", 3)))
     sscanf( record, "%*s%*s%*s%*s%d", &ldiv );
  else printf("transformation:%s not known\n", name );
  if (ldiv<1) ldiv=ddiv;

  /* create a set with all lower entitys down to points */

  /* cycle through all entities and add them to the special set  */
  /* cycle through all bodys and add  */
  setcopy=pre_seta( specialset->copy, "i", 0);
  if (setcopy<0)
  {
    errMsg (" ERROR in pre_copy: special set:%s could not be created\n", specialset->copy );
    goto no_swep;
  }

    /* when node coordinates were changed to the deformed ones then switch back before they are copied and then switch again */ 
    if(addDispFlag)
    {
      addDispToCoordinates(node);
      // remember to switch back
      addDispFlagLocal=2;
    }

  for (i=0; i<set[setNr].anz_b; i++)
  {
    seta( setcopy, "b", set[setNr].body[i] );
  }
  /* cycle through all surfs and add  */
  for (i=0; i<set[setNr].anz_s; i++)
  {
    seta( setcopy, "s", set[setNr].surf[i] );
  }
  /* cycle through all lcmbs and add  */
  for (i=0; i<set[setNr].anz_c; i++)
  {
    seta( setcopy, "c", set[setNr].lcmb[i] );
  }
  /* cycle through all lines and add  */
  for (i=0; i<set[setNr].anz_l; i++)
  {
    seta( setcopy, "l", set[setNr].line[i] );
  }
  /*  add all points */
  for (i=0; i<set[setNr].anz_p; i++)
  {
    seta( setcopy, "p", set[setNr].pnt[i] );
  }
  /*  add all sets (seq) */
  for (i=0; i<set[setNr].anz_se; i++)
  {
    seta( setcopy, "r", set[setNr].set[i] );
  }
  /*  add all nurs */
  for (i=0; i<set[setNr].anz_nurs; i++)
  {
    seta( setcopy, "S", set[setNr].nurs[i] );
  }
  /*  add all shapes */
  for (i=0; i<set[setNr].anz_sh; i++)
  {
    seta( setcopy, "sh", set[setNr].shp[i] );
  }
  /*  add all nodes */
  for (i=0; i<set[setNr].anz_n; i++)
  {
    seta( setcopy, "n", set[setNr].node[i] );
  }
  /*  add all elements */
  for (i=0; i<set[setNr].anz_e; i++)
  {
    seta( setcopy, "e", set[setNr].elem[i] );
  }

  /* second cycle through all entities and add lower ones to the special set  */
  completeSet( specialset->copy, "do") ;

  if ( (dep_n = (int **)realloc((int **)dep_n, (ldiv) * sizeof(int *))) == NULL )
  {  printf("\n\n ERROR: malloc failed in pre_move() fuer anz->n %d\n\n",anz->n) ; goto no_swep; }
  for(i=0; i<ldiv; i++)
  {
    if ( (dep_n[i] = (int *)malloc((anz->nmax+1) * sizeof(int))) == NULL )
    {  printf("\n\n ERROR: malloc failed in pre_move() fuer anz->n %d\n\n",anz->n) ; goto no_swep; }
  }
  if ( (dep_e = (int *)realloc((int *)dep_e, (anz->emax+1) * sizeof(int))) == NULL )
  {  printf("\n\n ERROR: malloc failed in pre_move() fuer anz->e %d\n\n",anz->e) ; goto no_swep; }
  if ( (dep_p = (int *)realloc((int *)dep_p, (anzGeo->p+1) * sizeof(int))) == NULL )
  {  printf("\n\n ERROR: malloc failed in pre_move() fuer anzGeo->p %d\n\n",anzGeo->p) ; goto no_swep; }
  if ( (dep_l = (int *)realloc((int *)dep_l, (anzGeo->l+1) * sizeof(int))) == NULL )
  {  printf("\n\n ERROR: malloc failed in pre_move() fuer anzGeo->l %d\n\n",anzGeo->l) ; goto no_swep; }
  if ( (dep_c = (int *)realloc((int *)dep_c, (anzGeo->c+1) * sizeof(int))) == NULL )
  {  printf("\n\n ERROR: malloc failed in pre_move() fuer anzGeo->c %d\n\n",anzGeo->c) ; goto no_swep; }
  if ( (dep_s = (int *)realloc((int *)dep_s, (anzGeo->s+1) * sizeof(int))) == NULL )
  {  printf("\n\n ERROR: malloc failed in pre_move() fuer anzGeo->s %d\n\n",anzGeo->s) ; goto no_swep; }
  if ( (dep_lp = (int *)realloc((int *)dep_lp, (anzGeo->p+1) * sizeof(int))) == NULL )
  {  printf("\n\n ERROR: malloc failed in pre_move() fuer anzGeo->p %d\n\n",anzGeo->p) ; goto no_swep; }
  if ( (dep_sl = (int *)realloc((int *)dep_sl, (anzGeo->l+1) * sizeof(int))) == NULL )
  {  printf("\n\n ERROR: malloc failed in pre_move() fuer anzGeo->l %d\n\n",anzGeo->l) ; goto no_swep; }
  if ( (dep_sc = (int *)realloc((int *)dep_sc, (anzGeo->c+1) * sizeof(int))) == NULL )
  {  printf("\n\n ERROR: malloc failed in pre_move() fuer anzGeo->c %d\n\n",anzGeo->c) ; goto no_swep; }
  if ( (dep_se = (int *)realloc((int *)dep_se, (anz->sets+1) * sizeof(int))) == NULL )
  {  printf("\n\n ERROR: malloc failed in pre_move() fuer anz->sets %d\n\n", anz->sets); goto no_swep; }
  if ( (dep_sh = (int *)realloc((int *)dep_sh, (anzGeo->sh+1) * sizeof(int))) == NULL )
  {  printf("\n\n ERROR: malloc failed in pre_move() fuer anzGeo->sh %d\n\n", anzGeo->sh); goto no_swep; }
  if ( (dep_S = (int *)realloc((int *)dep_S, (anzGeo->nurs+1) * sizeof(int))) == NULL )
  {  printf("\n\n ERROR: malloc failed in pre_move() fuer anz->nurs %d\n\n", anzGeo->nurs); goto no_swep; }

  /* copy all entities */
  length=strlen(setname)+strlen(targetnam)+1; 
  transtyp=copy_set(settrgt, &record[length], setcopy, &dep_n[ldiv-1][0], dep_e, dep_p, dep_l, dep_c, dep_s, dep_se, dep_sh, dep_S, setNr, appendSetFlag);
  if (transtyp==-1) goto no_swep;

  mastersetNr=setNr;
  setNr=setcopy;

  /* create new elements between all ori- and new nodes, overwrite the old elem */
  anz_e=set[setNr].anz_e;
  if(set[setNr].anz_e>0)
  {
    /* free the additional midside-nodes for higher order elements */
    for(i=anz->orign; i<anz->n; i++) node[node[i].nr].pflag=-1;
    anz->n= anz->orign;
    anz->nmax=anz->orignmax;

    /* at first collect all nodes of the master side which belong to shells */
    /* this nodes are used to create additional nodes along the path to the copied nodes */
    delSet("-ntmp");    
    delSet("-ptmp");    
    setNtmp=pre_seta("-ntmp","i",0);
    setPtmp=pre_seta("-ptmp","i",0);
    for (i=0; i<anz_e; i++)
    {
      e=set[setNr].elem[i];
      if (e<0)
      {
        errMsg(" elem (%d) is undefined\n", e );
        goto no_swep;
      }
      ipuf=0;
      switch(e_enqire[e].type)
      {
        case 7:
          ipuf = 3;  /* TRI3 */
	  break;
        case 8:
          ipuf = 6;  /* TRI6  */
	  break;
        case 9:
          ipuf = 4;  /* QUAD4  */
	  break;
        case 10:
          ipuf = 8;  /* QUAD8  */
	  break;
      }
      if(ipuf>0)
      {
        /* nodes per elem-type */
        for(n=0; n<ipuf; n++)
        {
          seta(setNtmp,"n",e_enqire[e].nod[n]);
        }
      }
    }

    /* create the nodes along the path */
    if(anz->l) cnset=copiedNodeSets->sets-1;
    getNewName( name, "l" );
    getNewName( p1name, "p" );
    getNewName( p2name, "p" );
      nn=anz->orignmax+anz->noffs;
    for (i=0; i<set[setNtmp].anz_n; i++)
    {
      j=set[setNtmp].node[i];
      p1=pnt( p1name, node[j].nx, node[j].ny, node[j].nz, 0 );
      if (p1==-1)
      {
        errMsg(" point (%d) is undefined\n", p1name );
        goto no_swep;
      }
      n=dep_n[ldiv-1][j];
      p2=pnt( p2name, node[n].nx, node[n].ny, node[n].nz, 0 );
      if (p2==-1)
      {
        errMsg(" corresp. point to point (%d) is undefined\n", p2name );
        goto no_swep;
      }
      /* create centerpoint if transtyp=3 (rot) */
      if (transtyp==3)
      {
        sscanf( &record[length], "%*s%s%s%s", pkt1, pkt2, buf );
        p3=createCenterpnt( pkt1, pkt2, buf, p1name, 1 );
      }
      else p3=0;

      if (p3==0)     
        l=line_i( name, p1, p2, 0, ldiv, 1., 0 ); 
      else
      {
        if (p3>0)
        {
          seta(setPtmp,"p",p3);
          l=line_i( name, p1, p2, p3, ldiv, 1., 'a' );
	}
        else
        {
          seta(-p3,"p",p2);
          l=line_i( name, p1, p2, -p3, ldiv, 1., 's' );
	}
      }
 
      /* create nodes along the line */
      k=0;
      for (n=3; n<line[l].nip-3; n+=3)
      {
        nn++;
        b=nod(anz, &node, 1, nn, line[l].ip[n], line[l].ip[n+1], line[l].ip[n+2], 0);
        dep_n[k++][j]=node[b].nr;

        /* add to all sets were the orig is a member */
        if(appendSetFlag) for(se=1; se<anz->sets; se++) if((se!=setNr)&&(se!=mastersetNr)&&(set[se].name!=(char *)NULL)&&(set[se].name[0]!='-'))  if(!set[se].type) if( getIndex(&set[se].node, set[se].anz_n, j) >-1) { seta( se, "n", node[b].nr );  }

        /* extend the copiedNodeSets structure by the new nodes */
        if(anz->l)
	{
          copiedNodeSets->anz_n[cnset]++;
          if ( (copiedNodeSets->mnod[cnset]= (int *)realloc((int *)copiedNodeSets->mnod[cnset], (copiedNodeSets->anz_n[cnset]) * sizeof(int))) == NULL )
          {  printf("\n\n ERROR: realloc failed in copy_set()\n\n") ; return; }
          if ( (copiedNodeSets->snod[cnset]= (int *)realloc((int *)copiedNodeSets->snod[cnset], (copiedNodeSets->anz_n[cnset]) * sizeof(int))) == NULL )
          {  printf("\n\n ERROR: realloc failed in copy_set()\n\n") ; return; }
          copiedNodeSets->mnod[cnset][copiedNodeSets->anz_n[cnset]-1]=j;
          copiedNodeSets->snod[cnset][copiedNodeSets->anz_n[cnset]-1]=node[b].nr;
	}
      }
      if (p3<0) delPnt( set[-p3].anz_p, set[-p3].pnt );
    }
    seta(setPtmp,"p",p1);
    seta(setPtmp,"p",p2);
    seta(setPtmp,"l",l);
    zap("-ptmp");
    //if (p3<0) zap(set[-p3].name);

    /* expand all loaded Datasets by the new nodes */
    /* all datasets have to be read. Only then results can be mapped (later this should be done on demand) */
    for (lc=0; lc<anz->l; lc++)
    {
      if (lcase[lc].loaded)
      {
        descalNodes ( anz->n, node, scale );
        copyDatasetToNodes(anz, node, lcase, lc, copiedNodeSets[0]);
        scalNodes ( anz->n, node, scale );
        calcDatasets( lc, anz, node, lcase );
        recompileEntitiesInMenu(lc);
      }
    }
    delSet("-ntmp");    

    for (i=0; i<anz_e; i++)
    {
      e=set[setNr].elem[i];
      if (e==-1)
      {
        errMsg(" elem (%d) is undefined\n", e );
        goto no_swep;
      }

      /* sweep the elements */
      ipuf=0;
      if (e_enqire[e].type == 7) ipuf = 3;  /* TRI3 */
      else if (e_enqire[e].type == 9) ipuf = 4;  /* QUAD4  */
      if(ipuf>0)
      {
        for(k=0; k<ldiv-1; k++)
	{
          for(n=0; n<ipuf; n++)
          {
            en[n]=dep_n[k][e_enqire[e].nod[n]];
            en[n+ipuf]=dep_n[k+1][e_enqire[e].nod[n]];
          }
          elem_define( anz->emax+1, 5-ipuf, &en[0], 1, 0 );

          /* add to all sets were the orig is a member */
          if(appendSetFlag) for(se=1; se<anz->sets; se++) if((se!=setNr)&&(se!=mastersetNr)&&(set[se].name!=(char *)NULL)&&(set[se].name[0]!='-'))  if(!set[se].type) if( getIndex(&set[se].elem, set[se].anz_e, e) >-1) { seta( se, "e", anz->emax );  }
	}
        for(n=0; n<ipuf; n++)
        {
          en[n]=e_enqire[e].nod[n];
          en[n+ipuf]=dep_n[0][e_enqire[e].nod[n]];
        }
        elem_define( e, 5-ipuf, &en[0], 0, 0 );
      }
      ipuf=0;
      if (e_enqire[e].type == 8) ipuf = 6;  /* TRI6 */
      else if (e_enqire[e].type == 10) ipuf = 8;  /* QUAD8  */
      if(ipuf>0)
      {
        for(k=1; k<ldiv-2; k+=2)
	{
          for(n=0; n<ipuf/2; n++)
          {
            en[n]=dep_n[k][e_enqire[e].nod[n]];
            en[n+ipuf/2]=dep_n[k+2][e_enqire[e].nod[n]];
            en[n+ipuf+ipuf/2]=dep_n[k+1][e_enqire[e].nod[n]];
          }
          for(n=ipuf/2; n<ipuf; n++)
          {
            en[n+ipuf/2]=dep_n[k][e_enqire[e].nod[n]];
            en[n+ipuf+ipuf/2]=dep_n[k+2][e_enqire[e].nod[n]];
            //node[dep_n[k+1][e_enqire[e].nod[n]]].pflag=1;
            delNod(1,&dep_n[k+1][e_enqire[e].nod[n]]);
          }
          if(ipuf==6) elem_define( anz->emax+1, 5, &en[0], 1, 0 );
          if(ipuf==8) elem_define( anz->emax+1, 4, &en[0], 1, 0 );

          /* add to all sets were the orig is a member */
          if(appendSetFlag) for(se=1; se<anz->sets; se++) if((se!=setNr)&&(se!=mastersetNr)&&(set[se].name!=(char *)NULL)&&(set[se].name[0]!='-'))  if(!set[se].type) if( getIndex(&set[se].elem, set[se].anz_e, e) >-1) { seta( se, "e", anz->emax );  }
	}
        for(n=0; n<ipuf/2; n++)
        {
          en[n]=e_enqire[e].nod[n];
          en[n+ipuf/2]=dep_n[1][e_enqire[e].nod[n]];
          en[n+ipuf+ipuf/2]=dep_n[0][e_enqire[e].nod[n]];
        }
        for(n=ipuf/2; n<ipuf; n++)
        {
          en[n+ipuf/2]=e_enqire[e].nod[n];
          en[n+ipuf+ipuf/2]=dep_n[1][e_enqire[e].nod[n]];
          //node[dep_n[0][e_enqire[e].nod[n]]].pflag=1;
          delNod(1,&dep_n[0][e_enqire[e].nod[n]]);
        }
        if(ipuf==6) elem_define( e, 5, &en[0], 0, 0 );
        if(ipuf==8) elem_define( e, 4, &en[0], 0, 0 );
      }
    }
    /* the nodes numbers have to be stored before the nodes for drawing purposes are generated */
    anz->orignmax   = anz->nmax;
    anz->orign      = anz->n;

    //for(i=0; i<anz->n; i++) printf("1 i:%d n:%d p:%d\n", i,node[i].nr,node[node[i].nr].pflag);
    delElem( set[settrgt].anz_e, set[settrgt].elem ) ;
    //for(i=0; i<anz->n; i++) printf("2 i:%d n:%d p:%d\n", i,node[i].nr,node[node[i].nr].pflag);
    elemChecker( anz->emax+1, node, e_enqire);
    // midside nodes were already created in delElem by calling iniElements
    //adjustDrawNodes(1);
    getElemNormalen( e_enqire, node, anz->e );
    makeSurfaces();
    realloc_colNr();
    updateDispLists();
  }

  /* create lines between all ori- and new points */
  j=set[setNr].anz_p;
  if(set[setNr].anz_p>0)
  {
    for (i=0; i<j; i++)
    {
      p1=set[setNr].pnt[i];
      if (p1==-1)
      {
        errMsg(" point (%d) is undefined\n", set[setNr].pnt[i] );
        goto no_swep;
      }
      p2=dep_p[set[setNr].pnt[i]];
      if (p2==-1)
      {
        errMsg(" corresp. point to point (%d) is undefined\n", set[setNr].pnt[i] );
        goto no_swep;
      }
      /* create centerpoint if transtyp=3 (rot) */
      if (transtyp==3)
      {
        sscanf( &record[length], "%*s%s%s%s", pkt1, pkt2, buf );
        p3=createCenterpnt( pkt1, pkt2, buf, point[set[setNr].pnt[i]].name, 0 );
      }
      else p3=0;

      l= getNewName( name, "l" );
      if ( l == -1 )
        { printf("copy: could not create new line\n"); goto no_swep; }
      
      if (p3==0)     
        l=line_i( name, p1, p2, 0, ldiv, 1., 0 ); 
      else
      {
        if (p3>0) l=line_i( name, p1, p2, p3, ldiv, 1., 'a' );
        else
        {
          seta(-p3,"p",p2);
          l=line_i( name, p1, p2, -p3, ldiv, 1., 's' );
	}
      }
      p1=line[l].p1;
      p2=line[l].p2;
      p3=line[l].trk;
      if ( l <0 )
        { printf("copy: could not create new line\n"); goto no_swep; }
      dep_lp[p1] = l; /* remember the index of the new line based on the 1st point */

      /* add to all sets were the orig is a member */
      if(appendSetFlag) for(se=1; se<anz->sets; se++) if((se!=setNr)&&(se!=mastersetNr)&&(set[se].name!=(char *)NULL)&&(set[se].name[0]!='-'))  if(!set[se].type) if( getIndex(&set[se].pnt, set[se].anz_p, set[setNr].pnt[i]) >-1) { seta( se, "l", l );  }
    }
  }

  /* create surfs between all ori- and new lines */
  if ((lori = (char *)realloc((char *)lori, (4)*sizeof(char)) ) == NULL )
  { printf("ERROR: realloc failure in pre_swep()\n"); return; }
  if ((types = (char *)realloc((char *)types, (4)*sizeof(char)) ) == NULL )
  { printf("ERROR: realloc failure in pre_swep()\n"); return; }
  if ((lines = (int *)realloc((int *)lines, (4)*sizeof(int)) ) == NULL )
  { printf("ERROR: realloc failure in pre_swep()\n"); return; }
  for (k=0; k<4; k++) { lori[k]='+'; types[k]='l'; }

  j=set[setNr].anz_l;
  if(set[setNr].anz_l>0)
  {
    for (i=0; i<j; i++)
    {
      lines[0]=set[setNr].line[i];
      if (lines[0]==-1)
      {
        errMsg(" line (%d) is undefined\n", set[setNr].line[i] );
        goto no_swep;
      }
      lines[1]=dep_l[set[setNr].line[i]];
      if (lines[1]==-1)
      {
        errMsg(" dep line from line (%d) is undefined\n", set[setNr].line[i] );
        goto no_swep;
      }
      lines[2]=dep_lp[line[set[setNr].line[i]].p1];
      lines[3]=dep_lp[line[set[setNr].line[i]].p2];

      if ( getNewName( name, "s" ) == -1 )
      { printf("Type s not known, surf can not be created\n"); goto no_swep; }


      //================================================//
      // 		NURBS-Sweeping			//
      // a NURBS surface is generated from a B-Spline	//
      // the B-Spline will be calculated from        	//
      // the support points of a line (type independent)//
      // a rotative or translative sweep is performed	//
      //================================================//     
      int nurbsNumber;
      if(sweepType != 0)
      {
	int npC1,npC2;
	double pC1[3],pC2[3];

	if(sweepType == 1)//rotative sweeping
	{	  
	  //get two points on symmetry axis	  
	  if(pkt2[0]=='x' || pkt1[0]=='x'){
	    pC1[0] = (0 -(scale->x))/(scale->w);
	    pC1[1] = (0 -(scale->y))/(scale->w);
	    pC1[2] = (0 -(scale->z))/(scale->w);
	    pC2[0] = (1 -(scale->x))/(scale->w);
	    pC2[1] = (0 -(scale->y))/(scale->w);
	    pC2[2] = (0 -(scale->z))/(scale->w); 
	  }
	  else if(pkt2[0]=='y' || pkt1[0]=='y'){
	    pC1[0] = (0 -(scale->x))/(scale->w);
	    pC1[1] = (0 -(scale->y))/(scale->w);
	    pC1[2] = (0 -(scale->z))/(scale->w);
	    pC2[0] = (0 -(scale->x))/(scale->w);
	    pC2[1] = (1 -(scale->y))/(scale->w);
	    pC2[2] = (0 -(scale->z))/(scale->w); 
	  }
	  else if(pkt2[0]=='z' || pkt1[0]=='z'){
	    pC1[0] = (0 -(scale->x))/(scale->w);
	    pC1[1] = (0 -(scale->y))/(scale->w);
	    pC1[2] = (0 -(scale->z))/(scale->w);
	    pC2[0] = (0 -(scale->x))/(scale->w);
	    pC2[1] = (0 -(scale->y))/(scale->w);
	    pC2[2] = (1 -(scale->z))/(scale->w); 	    
	  }
	  else{   
	    printf("manual Points \n");
	    npC1 =getPntNr(pkt1);
	    npC2 =getPntNr(pkt2);	  
	  #if TESTSWEEP == 1
	    printf("Point P1 %s: %f %f %f\n",point[npC1].name,point[npC1].px* scale->w+scale->x,point[npC1].py* scale->w+scale->y,point[npC1].pz* scale->w+scale->z);
	    printf("Point P2 %s: %f %f %f\n",point[npC2].name,point[npC2].px* scale->w+scale->x,point[npC2].py* scale->w+scale->y,point[npC2].pz* scale->w+scale->z); 
	  #endif	
	    //store center points
	    pC1[0] = point[npC1].px;
	    pC1[1] = point[npC1].py;
	    pC1[2] = point[npC1].pz;     
	    pC2[0] = point[npC2].px;
	    pC2[1] = point[npC2].py;
	    pC2[2] = point[npC2].pz;      
	  }
	}
	if(sweepType == 2)//linear sweepig
	{
	  double transVector[3];
	  sscanf( record, "%*s%*s%*s%lf%lf%lf%*d",&transVector[0],&transVector[1],&transVector[2]);
	  pC1[0] = point[line[lines[0]].p1].px;
	  pC1[1] = point[line[lines[0]].p1].py;
	  pC1[2] = point[line[lines[0]].p1].pz;     
	  pC2[0] = pC1[0] + transVector[0]/(scale->w);
	  pC2[1] = pC1[1] + transVector[1]/(scale->w);
	  pC2[2] = pC1[2] + transVector[2]/(scale->w); 	
	}
	
	//************************************************//
	// get support points for the B-Spline curve	//
	//************************************************//
	int k,l,index=0;  
	int nPnt = line[lines[0]].nip/3; //number of points
	double pCloud[nPnt][3];//define point cloud
	//fill point cloud with support points
	for(l=0;l<nPnt;l++)
	{
	  pCloud[l][0] = line[lines[0]].ip[index];
	  pCloud[l][1] = line[lines[0]].ip[index+1];
	  pCloud[l][2] = line[lines[0]].ip[index+2];	
	#if TESTSWEEP == 1
	  printf("X: %f Y: %f Z: %f\n",line[lines[0]].ip[index],line[lines[0]].ip[index+1],line[lines[0]].ip[index+2]);
	#endif      
	  index+=3;
	}	
	//************************************************//
	
	//*****************************************************//
	// perform B-Spline fitting with an iterative algorithm//
	//*****************************************************//
	printf("fitting B-Spline...");
	BSplineCurve myCurve;     
	piaFitting(pCloud,nPnt,&myCurve,3,0.0001);//Convert the line lines[0] to a B-Spline Curve via PIA-Fitting
	printf(" --> Done\n");
	//*****************************************************//
	
	BSplineSurface mySurf;
	if(sweepType == 1)//rotative sweeping
	{
	  //*****************************************************//
	  // rotate the B-Spline to generate a NURBS surface     //
	  //*****************************************************//	  
	  rotateBSpline(pC1, pC2, &myCurve, &mySurf);
	  //u-Parameter -> profile curve
	  //v-Parameter -> path curve (here a circle)
	  //*****************************************************//
	}
	if(sweepType == 2)//linear sweepig
	{
	  //*****************************************************//
	  // extrude the B-Spline to generate a NURBS surface    //
	  //*****************************************************//	
	  translateBSpline(pC1, pC2, &myCurve, &mySurf);
	  //u-Parameter -> profile curve
	  //v-Parameter -> path curve (here a circle)
	  //*****************************************************//      
	}
	  
	//*****************************************************//
	// 	generate and initialise an nurs-object         //
	//*****************************************************//
	char lineBuffer[50];//char buffers
	char nurbsName[10];
	getNewName(nurbsName,"S");
	//set header line of NURBS surface
	//sprintf(name,"N%c%c%c",line[lines[0]].name[1],line[lines[0]].name[2],line[lines[0]].name[3]);
	sprintf(lineBuffer,"%s DEFINE COMPACT %d %d %d %d %d %d",nurbsName, mySurf.uDeg, mySurf.vDeg, mySurf.nUPol, mySurf.nVPol, mySurf.nUKnt, mySurf.nVKnt);
	
      #if TESTSWEEP == 1
	printf("%s\n",lineBuffer);
      #endif
	nurbsNumber =  nurs(lineBuffer, 0);//wire content to struct nurs  
  
	//set poles of NURBS surface
	for(k=0;k<mySurf.nUPol;k++)//iterate over u-Parameter k =^ u
	{
	  for(l=0;l<mySurf.nVPol;l++)//iterate over v-Parameter l =^ v
	  {    
	  sprintf(lineBuffer, "%s CONTROL %d %d %f %f %f %f",nurbsName,k+1,l+1,mySurf.cX[l+mySurf.nVPol*k], mySurf.cY[l+mySurf.nVPol*k], mySurf.cZ[l+mySurf.nVPol*k],mySurf.weights[l+mySurf.nVPol*k]);	 
	  #if TESTSWEEP == 1 
	  printf("%s\n",lineBuffer);
	  #endif	
	  nurbsNumber =  nurs(lineBuffer, 0);//wire content to struct nurs    
	  }
	}
	//set u-knots to NURBS-surface
	for(k=0;k<mySurf.nUKnt;k++)
	{    
	  sprintf(lineBuffer,"%s KNOT U %d %f",nurbsName, k+1, mySurf.uKnt[k]);
	#if TESTSWEEP == 1
	  printf("%s\n",lineBuffer);
	#endif
	  nurbsNumber =  nurs(lineBuffer, 0);//wire content to struct nurs    
	}
	//set v-knots to NURBS-surface
	for(k=0;k<mySurf.nVKnt;k++)
	{    
	  sprintf(lineBuffer,"%s KNOT V %d %f",nurbsName,k+1, mySurf.vKnt[k]);
	#if TESTSWEEP == 1
	  printf("%s\n",lineBuffer);
	#endif
	  nurbsNumber =  nurs(lineBuffer, 0);//wire content to struct nurs  
	}
	//write end of NURBS surface
	sprintf(lineBuffer,"%s END",nurbsName);
      #if TESTSWEEP == 1
	printf("%s\n",lineBuffer);
      #endif
	nurbsNumber =  nurs(lineBuffer, 0); //wire content to struct nurs      
	//*****************************************************//
      }     
      //generate a surface and set the new NURBS-surface as its shape
      l=surface_i( name, '+', -1, (int)4, lori, lines, types );
      if(sweepType != 0)
      {
	nurbsNumber = shape_i(nurbs[nurbsNumber].name, 4, nurbsNumber, 0, 0, 0, 0, 0, 0);
	if(nurbsNumber == -1) printf("sweeping error: shape coult not be created\n");
        surf[l].sh = nurbsNumber;  
      }
      
      //===================  NURBS-Sweeping  END  ==================//
      
      if ( l <0 )
        { printf("copy: could not create new surf based on line:%s\n", line[lines[0]].name); goto no_swep; }
      dep_sl[lines[0]] = l; /* remember the index of the new surf based on the 1st line */

      /* add to all sets were the orig is a member */
      if(appendSetFlag) for(se=1; se<anz->sets; se++) if((se!=setNr)&&(se!=mastersetNr)&&(set[se].name!=(char *)NULL)&&(set[se].name[0]!='-')) if(!set[se].type) if( getIndex(&set[se].line, set[se].anz_l, set[setNr].line[i]) >-1) { seta( se, "s", l );  }
    }
  }

  /* create surfs between all ori- and new lcmbs */
  j=set[setNr].anz_c;
  if(set[setNr].anz_c>0)
  {
    for (i=0; i<j; i++)
    {
      lines[0]=set[setNr].lcmb[i];
      types[0]='c';
      if (lines[0]==-1)
      {
        errMsg(" lcmb (%d) is undefined\n", set[setNr].lcmb[i] );
        goto no_swep;
      }
      lines[1]=dep_c[set[setNr].lcmb[i]];
      types[1]='c';
      if (lines[1]==-1)
      {
        errMsg(" dep lcmb from lcmb (%d) is undefined\n", set[setNr].lcmb[i] );
        goto no_swep;
      }
      lines[2]=dep_lp[lcmb[set[setNr].lcmb[i]].p1];
      lines[3]=dep_lp[lcmb[set[setNr].lcmb[i]].p2];
      
      if ( getNewName( name, "s" ) == -1 )
      { printf("Type s not known, surf can not be created\n"); goto no_swep; }
      
      //================================================//
      // 		NURBS-Sweeping			//
      // a NURBS surface is generated from a B-Spline	//
      // the B-Spline will be calculated from        	//
      // the support points of a line (type independent)//
      // a rotative or translative sweep is performed	//
      //================================================//     
      int nurbsNumber;
      if(sweepType != 0)
      {
	int npC1,npC2;
	double pC1[3],pC2[3];
	
	if(sweepType == 1)//rotative sweeping
	{
	  //get two points on symmetry axis
	  if(pkt2[0]=='x' || pkt1[0]=='x'){
	    pC1[0] = (0 -(scale->x))/(scale->w);
	    pC1[1] = (0 -(scale->y))/(scale->w);
	    pC1[2] = (0 -(scale->z))/(scale->w);
	    pC2[0] = (1 -(scale->x))/(scale->w);
	    pC2[1] = (0 -(scale->y))/(scale->w);
	    pC2[2] = (0 -(scale->z))/(scale->w); 
	  }
	  else if(pkt2[0]=='y' || pkt1[0]=='y'){
	    pC1[0] = (0 -(scale->x))/(scale->w);
	    pC1[1] = (0 -(scale->y))/(scale->w);
	    pC1[2] = (0 -(scale->z))/(scale->w);
	    pC2[0] = (0 -(scale->x))/(scale->w);
	    pC2[1] = (1 -(scale->y))/(scale->w);
	    pC2[2] = (0 -(scale->z))/(scale->w); 
	  }
	  else if(pkt2[0]=='z' || pkt1[0]=='z'){
	    pC1[0] = (0 -(scale->x))/(scale->w);
	    pC1[1] = (0 -(scale->y))/(scale->w);
	    pC1[2] = (0 -(scale->z))/(scale->w);
	    pC2[0] = (0 -(scale->x))/(scale->w);
	    pC2[1] = (0 -(scale->y))/(scale->w);
	    pC2[2] = (1 -(scale->z))/(scale->w); 	    
	  }
	  else{ 
	  npC1 =getPntNr(pkt1);
	  npC2 =getPntNr(pkt2);  
	  #if TESTSWEEP == 1
	  printf("manual Points \n");
	    printf("Point P1 %s: %f %f %f\n",point[npC1].name,point[npC1].px* scale->w+scale->x,point[npC1].py* scale->w+scale->y,point[npC1].pz* scale->w+scale->z);
	    printf("Point P2 %s: %f %f %f\n",point[npC2].name,point[npC2].px* scale->w+scale->x,point[npC2].py* scale->w+scale->y,point[npC2].pz* scale->w+scale->z); 
	  #endif	
	    //store center points
	    pC1[0] = point[npC1].px;
	    pC1[1] = point[npC1].py;
	    pC1[2] = point[npC1].pz;     
	    pC2[0] = point[npC2].px;
	    pC2[1] = point[npC2].py;
	    pC2[2] = point[npC2].pz;   
	  }
	}
	if(sweepType == 2)//linear sweepig
	{
	  double transVector[3];
	  sscanf( record, "%*s%*s%*s%lf%lf%lf%*d",&transVector[0],&transVector[1],&transVector[2]);
	  pC1[0] = point[line[lines[0]].p1].px;
	  pC1[1] = point[line[lines[0]].p1].py;
	  pC1[2] = point[line[lines[0]].p1].pz;     
	  pC2[0] = pC1[0] + transVector[0]/(scale->w);
	  pC2[1] = pC1[1] + transVector[1]/(scale->w);
	  pC2[2] = pC1[2] + transVector[2]/(scale->w); 	
	}
	
	//************************************************//
	// get support points for the B-Spline curve	//
	//************************************************//
	int k,l,index, pCount = 0;  
	int nPnt = 0, nPntLine;
	for (l=0; l<lcmb[lines[0]].nl; l++){
	  nPnt += line[lcmb[lines[0]].l[l]].nip/3;
	}
	nPnt -= (lcmb[lines[0]].nl-1); //use connection points only once
	
	double pCloud[nPnt][3];//define point cloud
	//fill point cloud with support points
	for (l=0; l<lcmb[lines[0]].nl; l++)
	{
	  index=0;	    
	  printf("Orientation: %c \n",lcmb[lines[0]].o[l]);  
	  if(lcmb[lines[0]].o[l] == '+') // forward orientation 
	  {
	    if(l == lcmb[lines[0]].nl -1)  nPntLine = (line[lcmb[lines[0]].l[l]].nip/3); //last line
	    else  nPntLine = (line[lcmb[lines[0]].l[l]].nip/3)-1; //not last line
	    for(k=0;k<nPntLine;k++)
	    {      
	      pCloud[pCount][0] = line[lcmb[lines[0]].l[l]].ip[index];
	      pCloud[pCount][1] = line[lcmb[lines[0]].l[l]].ip[index+1];
	      pCloud[pCount][2] = line[lcmb[lines[0]].l[l]].ip[index+2];	
	    #if TESTSWEEP == 1
	      printf("X: %f Y: %f Z: %f\n",pCloud[pCount][0],pCloud[pCount][1],pCloud[pCount][2]);
	    #endif    
	      pCount++;
	      index+=3;	    
	    }	  
	  }
	  else // reversed orientation 
	  {
	    if(l == lcmb[lines[0]].nl -1){	      
	      nPntLine = (line[lcmb[lines[0]].l[l]].nip/3); //last line
	      index = (nPntLine*3)-1;
	    }
	    else{
	     
	      nPntLine = (line[lcmb[lines[0]].l[l]].nip/3)-1; //not last line
	       index = ((nPntLine+1)*3)-1;
	    }
    
	    for(k=0;k<nPntLine;k++)
	    {      
	      pCloud[pCount][0] = line[lcmb[lines[0]].l[l]].ip[index-2];
	      pCloud[pCount][1] = line[lcmb[lines[0]].l[l]].ip[index-1];
	      pCloud[pCount][2] = line[lcmb[lines[0]].l[l]].ip[index];	
	    #if TESTSWEEP == 1
	      printf("X: %f Y: %f Z: %f\n",pCloud[pCount][0],pCloud[pCount][1],pCloud[pCount][2]);
	    #endif    
	      pCount++;
	      index-=3;	    
	    }	  
	  }
	  
	}		
	//************************************************//	
	
	//*****************************************************//
	// perform B-Spline fitting with an iterative algorithm//
	//*****************************************************//
	printf("fitting B-Spline...");
	BSplineCurve myCurve;     
	piaFitting(pCloud,nPnt,&myCurve,3,0.0001);//Convert the line lines[0] to a B-Spline Curve via PIA-Fitting
	printf(" --> Done\n");
	//*****************************************************//
	
	BSplineSurface mySurf;
	if(sweepType == 1)//rotative sweeping
	{
	  //*****************************************************//
	  // rotate the B-Spline to generate a NURBS surface     //
	  //*****************************************************//	  
	  rotateBSpline(pC1, pC2, &myCurve, &mySurf);
	  //u-Parameter -> profile curve
	  //v-Parameter -> path curve (here a circle)
	  //*****************************************************//
	}
	if(sweepType == 2)//linear sweepig
	{
	  //*****************************************************//
	  // extrude the B-Spline to generate a NURBS surface    //
	  //*****************************************************//	
	  translateBSpline(pC1, pC2, &myCurve, &mySurf);
	  //u-Parameter -> profile curve
	  //v-Parameter -> path curve (here a circle)
	  //*****************************************************//      
	}
	  
	//*****************************************************//
	// 	generate and initialise an nurs-object         //
	//*****************************************************//
	char lineBuffer[50];//char buffers
	char nurbsName[10];
	getNewName(nurbsName,"S");
	//set header line of NURBS surface
	//sprintf(name,"N%c%c%c",lcmb[lines[0]].name[1],lcmb[lines[0]].name[2],lcmb[lines[0]].name[3]);
	sprintf(lineBuffer,"%s DEFINE COMPACT %d %d %d %d %d %d",nurbsName, mySurf.uDeg, mySurf.vDeg, mySurf.nUPol, mySurf.nVPol, mySurf.nUKnt, mySurf.nVKnt);
	
      #if TESTSWEEP == 1
	printf("%s\n",lineBuffer);
      #endif
	nurbsNumber =  nurs(lineBuffer, 0);//wire content to struct nurs  
  
	//set poles of NURBS surface
	for(k=0;k<mySurf.nUPol;k++)//iterate over u-Parameter k =^ u
	{
	  for(l=0;l<mySurf.nVPol;l++)//iterate over v-Parameter l =^ v
	  {    
	  sprintf(lineBuffer, "%s CONTROL %d %d %f %f %f %f",nurbsName,k+1,l+1,mySurf.cX[l+mySurf.nVPol*k], mySurf.cY[l+mySurf.nVPol*k], mySurf.cZ[l+mySurf.nVPol*k],mySurf.weights[l+mySurf.nVPol*k]);	 
	  #if TESTSWEEP == 1 
	  printf("%s\n",lineBuffer);
	  #endif	
	  nurbsNumber =  nurs(lineBuffer, 0);//wire content to struct nurs    
	  }
	}
	//set u-knots to NURBS-surface
	for(k=0;k<mySurf.nUKnt;k++)
	{    
	  sprintf(lineBuffer,"%s KNOT U %d %f",nurbsName, k+1, mySurf.uKnt[k]);
	#if TESTSWEEP == 1
	  printf("%s\n",lineBuffer);
	#endif
	  nurbsNumber =  nurs(lineBuffer, 0);//wire content to struct nurs    
	}
	//set v-knots to NURBS-surface
	for(k=0;k<mySurf.nVKnt;k++)
	{    
	  sprintf(lineBuffer,"%s KNOT V %d %f",nurbsName,k+1, mySurf.vKnt[k]);
	#if TESTSWEEP == 1
	  printf("%s\n",lineBuffer);
	#endif
	  nurbsNumber =  nurs(lineBuffer, 0);//wire content to struct nurs  
	}
	//write end of NURBS surface
	sprintf(lineBuffer,"%s END",nurbsName);
      #if TESTSWEEP == 1
	printf("%s\n",lineBuffer);
      #endif
	nurbsNumber =  nurs(lineBuffer, 0); //wire content to struct nurs      
	//*****************************************************//
      }     
      //generate a surface and set the new NURBS-surface as its shape
      l=surface_i( name, '+', -1, (int)4, lori, lines, types );      
      if(sweepType != 0)
      {
	nurbsNumber = shape_i(nurbs[nurbsNumber].name, 4, nurbsNumber, 0, 0, 0, 0, 0, 0);
	if(nurbsNumber == -1) printf("sweeping error: shape coult not be created\n");
        surf[l].sh = nurbsNumber;  
      }
      
      //===================  NURBS-Sweeping  END  ==================//

      if ( l <0 )
        { printf("copy: could not create new surf based on lcmb:%s\n", lcmb[lines[0]].name); goto no_swep; }
      dep_sc[lines[0]] = l; /* remember the index of the new surf based on the 1st lcmb */

      /* add to all sets were the orig is a member */
      if(appendSetFlag) for(se=1; se<anz->sets; se++) if((se!=setNr)&&(se!=mastersetNr)&&(set[se].name!=(char *)NULL)&&(set[se].name[0]!='-')) if(!set[se].type) if( getIndex(&set[se].lcmb, set[se].anz_c, set[setNr].lcmb[i]) >-1) { seta( se, "s", l );  }
    }
  }

  /* create bodys between all ori- and new surfs */
  j=set[setNr].anz_s;
  if(set[setNr].anz_s>0)
  {
    for (i=0; i<j; i++)
    {
      if ((s = (int *)realloc((int *)s, (surf[set[setNr].surf[i]].nl+2)*sizeof(int)) ) == NULL )
      { printf("ERROR: malloc failure in pre_swep()\n"); return; }
      if ((lori = (char *)realloc((char *)lori, (surf[set[setNr].surf[i]].nl+2)*sizeof(char)) ) == NULL )
      { printf("ERROR: realloc failure in pre_swep()\n"); return; }

      s[0]=set[setNr].surf[i];
      if (s[0]==-1)
      {
        errMsg(" surf (%d) is undefined\n", set[setNr].surf[i] );
        goto no_swep;
      }

      /* remove the now volume elements from the surface (once shells) */
      surf[s[0]].ne=0;

      s[1]=dep_s[set[setNr].surf[i]];
      if (s[1]==-1)
      {
        errMsg(" dep surf from surf (%d) is undefined\n", set[setNr].surf[i] );
        goto no_swep;
      }
      for (k=0; k<surf[s[0]].nl; k++)
      {
        if ( surf[s[0]].typ[k]=='l')
          s[k+2]=dep_sl[surf[s[0]].l[k]];
        else s[k+2]=dep_sc[surf[s[0]].l[k]];    
      }
      for (k=0; k<surf[s[0]].nl+2; k++) lori[k]= '+';

      if ( getNewName( name, "b" ) == -1 )
	{ printf("Type b not known, body can not be created\n"); goto no_swep; }
      b=gbod_i( name, -1, surf[s[0]].nl+2, lori, s );
      if ( b <0 )
        { printf("copy: could not create new body based on surf:%s\n", surf[s[0]].name); goto no_swep; }

      /* add to all sets were the orig is a member */
      if(appendSetFlag) for(se=1; se<anz->sets; se++) if((se!=setNr)&&(se!=mastersetNr)&&(set[se].name!=(char *)NULL)&&(set[se].name[0]!='-')) if(!set[se].type) if( getIndex(&set[se].surf, set[se].anz_s, set[setNr].surf[i]) >-1) { seta( se, "b", b );  }
    }
  }

 no_swep:;
  for(i=0; i<ldiv; i++) free(dep_n[i]);
  free(dep_n);   dep_n =NULL;
  free(dep_e);   dep_e =NULL;
  free(dep_p);   dep_p =NULL;
  free(dep_l);   dep_l =NULL;
  free(dep_c);   dep_c =NULL;
  free(dep_s);   dep_s =NULL;
  free(dep_lp);  dep_lp=NULL;
  free(dep_sl);  dep_sl=NULL;
  free(dep_sc);  dep_sc=NULL;
  free(dep_se);  dep_se=NULL;
  free(dep_sh);  dep_sh=NULL;
  free(dep_S);   dep_S=NULL;
  free(lori);    lori  =NULL;
  free(types);   types =NULL;
  free(lines);   lines =NULL;
  free(s);       s     =NULL;

  /* clear special set  */
  delSet(specialset->copy );

  /* when node coordinates were changed to the deformed ones then switch back before they are copied and then switch again */ 
  if(addDispFlagLocal==2)
  {
    addDispToCoordinates(node);
  }
}


void pre_copy( char *record )
{
  int  length, i, setNr, settrgt, setcopy;
  char setname[MAX_LINE_LENGTH], targetnam[MAX_LINE_LENGTH];
  static int   *dep_n, *dep_e, *dep_p, *dep_l, *dep_c, *dep_s, *dep_se, *dep_sh, *dep_S;
  int  appendSetFlag=0;
  char addDispFlagLocal=0;

  sscanf( record, "%s %s", setname, targetnam);

  /* evaluate the last argument */
  i=strlen(record);
  while((record[--i]!=' ')&&(i>-1));
  if(compare(&record[i+1], "append", 1)==1) appendSetFlag=1;

  operateAlias( setname, "se" );
  setNr=getSetNr( setname );
  if (setNr<0)
  {
    errMsg(" Set (%s) is undefined\n", setname );
    goto no_copy;
  }
  settrgt=pre_seta( targetnam, "i", 0 );
  if (settrgt<0)
  {
    errMsg(" Set (%s) is undefined\n", targetnam );
    goto no_copy;
  }

  /* create a set with all lower entitys down to points */

  /* cycle through all entities and add them to the special set  */
  /* cyrcle through all bodys and add  */
  setcopy=pre_seta( specialset->copy, "i", 0);
  if (setcopy<0)
  {
    errMsg (" ERROR in pre_copy: special set:%s could not be created\n", specialset->copy );
    goto no_copy;
  }
    /* when node coordinates were changed to the deformed ones then switch back before they are copied and then switch again */ 
    if(addDispFlag)
    {
      addDispToCoordinates(node);
      // remember to switch back
      addDispFlagLocal=2;
    }

  for (i=0; i<set[setNr].anz_b; i++)
  {
    seta( setcopy, "b", set[setNr].body[i] );
  }
  /* cyrcle through all surfs and add  */
  for (i=0; i<set[setNr].anz_s; i++)
  {
    seta( setcopy, "s", set[setNr].surf[i] );
  }
  /* cyrcle through all lcmbs and add  */
  for (i=0; i<set[setNr].anz_c; i++)
  {
    seta( setcopy, "c", set[setNr].lcmb[i] );
  }
  /* cyrcle through all lines and add  */
  for (i=0; i<set[setNr].anz_l; i++)
  {
    seta( setcopy, "l", set[setNr].line[i] );
  }
  /*  add all points */
  for (i=0; i<set[setNr].anz_p; i++)
  {
    seta( setcopy, "p", set[setNr].pnt[i] );
  }
  /*  add all sets (seq) */
  for (i=0; i<set[setNr].anz_se; i++)
  {
    seta( setcopy, "r", set[setNr].set[i] );
  }
  /*  add all nurs */
  for (i=0; i<set[setNr].anz_nurs; i++)
  {
    seta( setcopy, "S", set[setNr].nurs[i] );
  }
  /*  add all shapes */
  for (i=0; i<set[setNr].anz_sh; i++)
  {
    seta( setcopy, "sh", set[setNr].shp[i] );
  }
  /*  add all nodes */
  for (i=0; i<set[setNr].anz_n; i++)
  {
    seta( setcopy, "n", set[setNr].node[i] );
  }
  /*  add all elements */
  for (i=0; i<set[setNr].anz_e; i++)
  {
    seta( setcopy, "e", set[setNr].elem[i] );
  }

  /* second cycle through all entities and add lower ones to the special set  */
  completeSet( specialset->copy, "do") ;

  if ( (dep_n = (int *)realloc((int *)dep_n, (anz->nmax+1) * sizeof(int))) == NULL )
  {  printf("\n\n ERROR: malloc failed in pre_move()\n\n") ; goto no_copy; }
  if ( (dep_e = (int *)realloc((int *)dep_e, (anz->emax+1) * sizeof(int))) == NULL )
  {  printf("\n\n ERROR: malloc failed in pre_move()\n\n") ; goto no_copy; }
  if ( (dep_p = (int *)realloc((int *)dep_p, (anzGeo->p+1) * sizeof(int))) == NULL )
  {  printf("\n\n ERROR: malloc failed in pre_move()\n\n") ; goto no_copy; }
  if ( (dep_l = (int *)realloc((int *)dep_l, (anzGeo->l+1) * sizeof(int))) == NULL )
  {  printf("\n\n ERROR: malloc failed in pre_move()\n\n") ; goto no_copy; }
  if ( (dep_c = (int *)realloc((int *)dep_c, (anzGeo->c+1) * sizeof(int))) == NULL )
  {  printf("\n\n ERROR: malloc failed in pre_move()\n\n") ; goto no_copy; }
  if ( (dep_s = (int *)realloc((int *)dep_s, (anzGeo->s+1) * sizeof(int))) == NULL )
  {  printf("\n\n ERROR: malloc failed in pre_move()\n\n") ; goto no_copy; }
  if ( (dep_se = (int *)realloc((int *)dep_se, (anz->sets+1) * sizeof(int))) == NULL )
  {  printf("\n\n ERROR: malloc failed in pre_move()\n\n") ; goto no_copy; }
  if ( (dep_sh = (int *)realloc((int *)dep_sh, (anzGeo->sh+1) * sizeof(int))) == NULL )
  {  printf("\n\n ERROR: malloc failed in pre_move() \n\n"); goto no_copy; }
  if ( (dep_S = (int *)realloc((int *)dep_S, (anzGeo->nurs+1) * sizeof(int))) == NULL )
  {  printf("\n\n ERROR: malloc failed in pre_move()\n\n") ; goto no_copy; }

  length=strlen(setname)+strlen(targetnam)+1; 
  copy_set(settrgt, &record[length], setcopy, dep_n, dep_e, dep_p, dep_l, dep_c, dep_s, dep_se, dep_sh, dep_S, setNr, appendSetFlag);

 no_copy:;
  if ( (dep_n = (int *)realloc((int *)dep_n, 1 * sizeof(int))) == NULL )
  {  printf("\n\n ERROR: malloc failed in pre_move()\n\n") ;  }
  if ( (dep_e = (int *)realloc((int *)dep_e, 1 * sizeof(int))) == NULL )
  {  printf("\n\n ERROR: malloc failed in pre_move()\n\n") ;  }
  if ( (dep_p = (int *)realloc((int *)dep_p, 1 * sizeof(int))) == NULL )
  {  printf("\n\n ERROR: malloc failed in pre_move()\n\n") ;  }
  if ( (dep_l = (int *)realloc((int *)dep_l, 1 * sizeof(int))) == NULL )
  {  printf("\n\n ERROR: malloc failed in pre_move()\n\n") ;  }
  if ( (dep_c = (int *)realloc((int *)dep_c, 1 * sizeof(int))) == NULL )
  {  printf("\n\n ERROR: malloc failed in pre_move()\n\n") ;  }
  if ( (dep_s = (int *)realloc((int *)dep_s, 1 * sizeof(int))) == NULL )
  {  printf("\n\n ERROR: malloc failed in pre_move()\n\n") ;  }
  if ( (dep_se = (int *)realloc((int *)dep_se, 1 * sizeof(int))) == NULL )
  {  printf("\n\n ERROR: malloc failed in pre_move()\n\n") ;  }
  if ( (dep_sh = (int *)realloc((int *)dep_sh, 1 * sizeof(int))) == NULL )
  {  printf("\n\n ERROR: malloc failed in pre_move()\n\n") ;  }
  if ( (dep_S = (int *)realloc((int *)dep_S, 1 * sizeof(int))) == NULL )
  {  printf("\n\n ERROR: malloc failed in pre_move()\n\n") ;  }

  /* clear special set  */
  delSet(specialset->copy );

  /* when node coordinates were changed to the deformed ones then switch back before they are copied and then switch again */ 
  if(addDispFlagLocal==2)
  {
    addDispToCoordinates(node);
  }
}
