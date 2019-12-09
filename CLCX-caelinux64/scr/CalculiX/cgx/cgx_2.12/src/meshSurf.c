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
/* TO DO  */
/* -in mesh_tr3u()                                                                          */
/*   near3d() is called two times. The second time can be removed if the node sequence      */
/*   is the same as the uv sequence (this is not done so far)                               */

#define TEST           0
#define TEST1          0
#define TEST2          0
#define TEST3          0
#define TEST4          0

#include <cgx.h>

extern double     gtol;

extern char  printFlag;                   /* printf 1:on 0:off */
extern int   nurbsflag;
extern char  fillSurfFlag;                /* 1: generate triangles for surface-rendering and projection */

extern Scale     scale[1];
extern Summen    anz[1];
extern NodeBlocks *nBlock;
extern Sets      *set;
extern Points    *point;
extern Lines     *line;
extern Lcmb      *lcmb;
extern Gsur      *surf;
extern Nurbs     *nurbs;
extern Shapes    *shape;
extern Nodes     *node;
extern Elements  *e_enqire;

/* additional entities from setFunktions */
extern SpecialSet specialset[1];


/* preliminary nodes, they will be deleded after the elements are created */
extern Summen    apre[1];
extern Nodes     *npre;

/* relation of node generated on entity to final node of the mesh, nbuf[npre][nr]=node */
/* for cfd more than one node of the mesh can exist based on one node of a certain entity */
extern int **nbuf;
extern int sum_nbuf;

/* for CFD-meshing */
extern int              anz_cfdSurfs;
extern int              writeCFDflag;

/* threading */
typedef struct {
  int tid, returnVal, surf, anz_s, meshflag, started;
}Thread_param;
extern int countOpenThreads;
extern sem_t   *sem;

/* semaphores:
countOpenThreads      0
surfToShape           1
surfToNurs            2
pre_seta              3
delNurs,delPnt,delSet 4
splitLineAtDivratio   5
getNewName,surface_i  6
setr                  7
elem_define           8
nblock (var)          9
nod                  10
addTwoLines          11
*/



void error(const char *string)
{
    printf("Error in:%s\n", string);
    exit(0);
}


typedef struct {
  int sum, *n2, *nm;
}N1nm;
N1nm *n1nm;

void genMidsideNodes(int nr, N1nm **ptr_n1nm, int **ptr_nm_i, int *ptr_sum_nm, double **ptr_pntm_u, double **ptr_pntm_v, int *nbuf, int *ibuf, double *pnt_u, double *pnt_v, double *tab_u, double *tab_lu, double *tab_bu, double *tab_v, double *tab_lv, double *tab_bv)
{
  int i,i1,i2,n,n1,n2,nm;
  double pnt_uscal[2], pnt_vscal[2], bu[2],bv[2],sumbu,sumbv;
  int sum_nm;
  int *nm_i;
  N1nm *n1nm;
  double *pntm_u, *pntm_v;
  sum_nm=*ptr_sum_nm;
  nm_i=*ptr_nm_i;
  n1nm = *ptr_n1nm; 
  pntm_u=*ptr_pntm_u;
  pntm_v=*ptr_pntm_v;
  
  for (n=0; n<3; n++)
  {
    i1=ibuf[n];
    if(n<2) i2=ibuf[n+1]; else i2=ibuf[0];
    n1=nbuf[n];
    if(n<2) n2=nbuf[n+1]; else n2=nbuf[0];

    /* check if the nm exists already */
    nm=-1;
    //printf("n:%d %d apre->nmax:%d\n",n1,n2,apre->nmax);
    for(i=0; i<n1nm[n1].sum; i++) if(n1nm[n1].n2[i]==n2) nm=n1nm[n1].nm[i];
    for(i=0; i<n1nm[n2].sum; i++) if(n1nm[n2].n2[i]==n1) nm=n1nm[n2].nm[i];

    if(nm==-1)
    {
      /* generate new node */
      if( (nm_i = (int *)realloc((int *)nm_i, (sum_nm+1)*sizeof(int) )) == NULL )
      { printf(" ERROR: realloc failure\n\n"); return; }
      nm_i[sum_nm]= nod( apre, &npre, 0, apre->nmax+1, 0., 0., 0., 0 );
      nm= apre->nmax;
      if( ( surf[nr].nod=(int *)realloc( (int *)surf[nr].nod, (surf[nr].nn+1)*sizeof(int)) )==NULL)
      { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[nr].name); return; }
      surf[nr].nod[surf[nr].nn++]=apre->nmax;
      if ( (n1nm = (N1nm *)realloc((N1nm *)n1nm, (apre->nmax+1) * sizeof(N1nm))) == NULL )
      { printf("\n\n ERROR in mids: malloc\n\n") ; exit(-1); }    
      n1nm[apre->nmax].sum=0;
      n1nm[apre->nmax].n2=(int *)NULL;
      n1nm[apre->nmax].nm=(int *)NULL;

      if ( (n1nm[n1].n2 = (int *)realloc( n1nm[n1].n2, (n1nm[n1].sum+1) * sizeof(int))) == NULL )
      { printf("\n\n ERROR in mids: realloc\n\n") ; exit(-1); }    
      if ( (n1nm[n1].nm = (int *)realloc( n1nm[n1].nm, (n1nm[n1].sum+1) * sizeof(int))) == NULL )
      { printf("\n\n ERROR in mids: realloc\n\n") ; exit(-1); }    
      n1nm[n1].n2[n1nm[n1].sum]=n2;
      n1nm[n1].nm[n1nm[n1].sum]=nm;
      n1nm[n1].sum++;


      // data for later coordinate calc.
      if( (pntm_u = (double *)realloc(pntm_u, (sum_nm+1)*sizeof(double) )) == NULL )
      { printf(" ERROR: realloc failure\n\n"); return; }
      if( (pntm_v = (double *)realloc(pntm_v, (sum_nm+1)*sizeof(double) )) == NULL )
      { printf(" ERROR: realloc failure\n\n"); return; }
      //printf(" %d  %d %d  %f %f  %f %f\n", sum_nm+1,n1,n2, pnt_u[i1], pnt_u[i2], pnt_v[i1], pnt_v[i2]);

      // change into scaled u,v-coordinates
      pnt_uscal[0]=intpol( tab_u, tab_lu, UV_STEPS, pnt_u[i1] );
      pnt_vscal[0]=intpol( tab_v, tab_lv, UV_STEPS, pnt_v[i1] );
      pnt_uscal[1]=intpol( tab_u, tab_lu, UV_STEPS, pnt_u[i2] );
      pnt_vscal[1]=intpol( tab_v, tab_lv, UV_STEPS, pnt_v[i2] );
      bu[0]=intpol( tab_u, tab_bu, UV_STEPS, pnt_u[i1] );
      bv[0]=intpol( tab_v, tab_bv, UV_STEPS, pnt_v[i1] );
      bu[1]=intpol( tab_u, tab_bu, UV_STEPS, pnt_u[i2] );
      bv[1]=intpol( tab_v, tab_bv, UV_STEPS, pnt_v[i2] );
      sumbu=bu[0]+bu[1];
      sumbv=bv[0]+bv[1];
      pntm_u[sum_nm]=pnt_uscal[0]*(bv[0]/sumbv)+pnt_uscal[1]*(bv[1]/sumbv);
      pntm_v[sum_nm]=pnt_vscal[0]*(bu[0]/sumbu)+pnt_vscal[1]*(bu[1]/sumbu);
      // change into real u,v-coordinates
      pntm_u[sum_nm]=intpol( tab_lu, tab_u, UV_STEPS, pntm_u[sum_nm]);
      pntm_v[sum_nm]=intpol( tab_lv, tab_v, UV_STEPS, pntm_v[sum_nm]);
      //printf(" # %f %f %f %f %f %f\n", bu[0],bv[0],bu[1],bv[1],sumbu,sumbv);
      //printf(" node %d %f %f %f\n", sum_nm+1+50000, pntm_u[sum_nm],pntm_v[sum_nm],0.);

      // alternatively w/o scaling:
      /*
      pntm_u[sum_nm]=(pnt_u[i1]+pnt_u[i2])*.5;
      pntm_v[sum_nm]=(pnt_v[i1]+pnt_v[i2])*.5;
      printf(" node %d %f %f %f\n", sum_nm+1+50000, pntm_u[sum_nm],pntm_v[sum_nm],0.);
      */

      sum_nm++;
    }

    nbuf[n+3]=nm;
  }

  *ptr_sum_nm=sum_nm;
  *ptr_nm_i  =nm_i  ;
  *ptr_n1nm  =n1nm  ; 
  *ptr_pntm_u=pntm_u;
  *ptr_pntm_v=pntm_v;
}



int mesh_tr3u(int nr )
{
  int i,j,k,l,m,n,p, oprod, ini_anzn,loop;
  int n1,n2,nm;
  int ebuf[20],ibuf[3];

  extern Meshp meshp;
  double p0[3], p0p1[3], p1[3];
  int   patch;

  int     e=0, np, np_bou=0, *npc=NULL, nurbsnr; 
  double *pnt_u=NULL, *pnt_v=NULL;
  double *pnt_ubou=NULL, *pnt_vbou=NULL;
  double *pnt_uscal=NULL, *pnt_vscal=NULL;
  int    *tri=NULL;
  Points *ppre=NULL, *pcg=NULL;
  double *cg_u=NULL, *cg_v=NULL;
  int    **edge_n2=NULL;
#if TEST1
  FILE   *handle;
#endif

  int *nm_i=NULL, sum_nm=0;
  double umin, umax,du,vmin,vmax,dv;
  double tab_u[UV_STEPS+1], tab_v[UV_STEPS+1], tab_lu[UV_STEPS+1], tab_lv[UV_STEPS+1], tab_bu[UV_STEPS+1], tab_bv[UV_STEPS+1];
  Points tab_p[UV_STEPS+1];
  double dtab_u[UV_STEPS+1], dtab_v[UV_STEPS+1], dtab_umean[UV_STEPS+1], dtab_vmean[UV_STEPS+1];

  int    anz_n, triSplitterFlag=0, *snods=NULL, *pnt_flag=NULL, *pnt_indx=NULL, *nod_mesh2d=NULL;
  double alpha, beta, qfactor=1., alphaFactor=ALPHAFACTOR;

  double *orig_x=NULL, *orig_y=NULL, *orig_z=NULL, *sort_x=NULL, *sort_y=NULL, *sort_z=NULL;
  int *sort_nx=NULL, *sort_ny=NULL, *sort_nz=NULL, near_node[10];
  Rsort *rsort=NULL;

  double *pntm_u=NULL, *pntm_v=NULL;
  double vl,v0[3],v1[3],u=0.5,dist,max_ratio;
  Points *pm=NULL, *nv=NULL, *pnt_bou=NULL;

  if(shape[surf[nr].sh].type!=4)
  {
    printf(" ERROR: tr3u requires a related nurbs. No nurbs found for surf:%s\n",surf[nr].name);  return(-1);
  }

  /* create the uv patch */
  repSurf(nr, 0);

  nurbsnr=shape[surf[nr].sh].p[0];
  patch=surf[nr].patch;
  {
    if(printFlag)
      printf(" mesh surf:%s\n", surf[nr].name);

    /* create a table where the u,v values correspond to real length in the middle of the nurbs */
    /* the triangulation will take place by using scaled u,v coordinates to avoid bad shaped elements */
    /* get the u and v range by looking into the knots */
    umin=nurbs[nurbsnr].uknt[0];
    umax=nurbs[nurbsnr].uknt[nurbs[nurbsnr].u_nknt-1];
    du=(umax-umin)/(UV_STEPS-1);
    vmin=nurbs[nurbsnr].vknt[0];
    vmax=nurbs[nurbsnr].vknt[nurbs[nurbsnr].v_nknt-1];
    dv=(vmax-vmin)/(UV_STEPS-1);
#if TEST
    printf("surf:%s nurbs:%s u:%lf-%lf v:%lf-%lf\n", surf[nr].name, nurbs[nurbsnr].name, umin,umax,vmin,vmax);
#endif
    for(i=0; i<UV_STEPS; i++)
    {
      tab_u[i]=dtab_u[i]=umin+du*i;
      tab_v[i]=dtab_v[i]=vmin+dv*i;
      dtab_umean[i]=(umax+umin)*.5;
      dtab_vmean[i]=(vmax+vmin)*.5;
    }

    tab_lu[0]=0.;
    evalNurbs( nurbs, nurbsnr, UV_STEPS, dtab_u, dtab_vmean, tab_p);
    for(i=1; i<UV_STEPS; i++)
    {
      p0[0]=tab_p[i-1].px;
      p0[1]=tab_p[i-1].py;
      p0[2]=tab_p[i-1].pz;
      p1[0]=tab_p[i].px;
      p1[1]=tab_p[i].py;
      p1[2]=tab_p[i].pz;
      v_result(p0,p1,p0p1);
      tab_lu[i]=tab_lu[i-1]+v_betrag(p0p1);
    }

    tab_lv[0]=0.;
    evalNurbs( nurbs, nurbsnr, UV_STEPS, dtab_umean, dtab_v, tab_p);
    for(i=1; i<UV_STEPS; i++)
    {
      p0[0]=tab_p[i-1].px;
      p0[1]=tab_p[i-1].py;
      p0[2]=tab_p[i-1].pz;
      p1[0]=tab_p[i].px;
      p1[1]=tab_p[i].py;
      p1[2]=tab_p[i].pz;
      v_result(p0,p1,p0p1);
      tab_lv[i]=tab_lv[i-1]+v_betrag(p0p1);
    }

    /* create a table where the u,v values correspond to the local width of the nurbs */
    /* this is needed for the generation of new nodes in triSplitter() and fixMidsideNodes_npre() */
    /* first determine the width along the u direction, then v */
    for(j=0; j<UV_STEPS; j++)
    {
      for(i=0; i<UV_STEPS; i++)
      {
        dtab_u[i]=umin+du*j;
        dtab_v[i]=vmin+dv*i;
      }
      tab_bu[j]=0.;
      evalNurbs( nurbs, nurbsnr, UV_STEPS, dtab_u, dtab_v, tab_p);
      for(i=1; i<UV_STEPS; i++)
      {
        p0[0]=tab_p[i-1].px;
        p0[1]=tab_p[i-1].py;
        p0[2]=tab_p[i-1].pz;
        p1[0]=tab_p[i].px;
        p1[1]=tab_p[i].py;
        p1[2]=tab_p[i].pz;
        v_result(p0,p1,p0p1);
        tab_bu[j]+=v_betrag(p0p1);
      }
      //printf("%d u:%f u:%f bu:%f\n",j,dtab_u[0],tab_u[j], tab_bu[j]);

      for(i=0; i<UV_STEPS; i++)
      {
        dtab_u[i]=umin+du*i;
        dtab_v[i]=vmin+dv*j;
      }
      tab_bv[j]=0.;
      evalNurbs( nurbs, nurbsnr, UV_STEPS, dtab_u, dtab_v, tab_p);
      for(i=1; i<UV_STEPS; i++)
      {
        p0[0]=tab_p[i-1].px;
        p0[1]=tab_p[i-1].py;
        p0[2]=tab_p[i-1].pz;
        p1[0]=tab_p[i].px;
        p1[1]=tab_p[i].py;
        p1[2]=tab_p[i].pz;
        v_result(p0,p1,p0p1);
        tab_bv[j]+=v_betrag(p0p1);
      }
      //printf("%d v:%f v:%f bv:%f\n",j,dtab_v[0],tab_v[j], tab_bv[j]);
    }

    /* get the coords of the trimming loops (uv-values) */
    /* first determine the amount of points over all trimming loops for that patch (surface) */
    np=0; for(i=0; i<nurbs[nurbsnr].nc[patch]; i++) np+=nurbs[nurbsnr].np[patch][i];
    
    if( (edge_n2 = (int **)malloc( (np+1)*sizeof(int *) )) == NULL )
    { printf(" ERROR: malloc failure\n\n");
      return(-1); }
    for(i=0; i<np+1; i++)
    {
      if( (edge_n2[i] = (int *)malloc( (2)*sizeof(int) )) == NULL )
      { printf(" ERROR: malloc failure\n\n");
        return(-1); }
    }
    if( (pnt_u = (double *)malloc( (np+1)*sizeof(double) )) == NULL )
    { printf(" ERROR: malloc failure\n\n");
      return(-1); }
    if( (pnt_v = (double *)malloc( (np+1)*sizeof(double) )) == NULL )
    { printf(" ERROR: malloc failure\n\n");
      return(-1); }
    if( (pnt_ubou = (double *)malloc( (np+1)*sizeof(double) )) == NULL )
    { printf(" ERROR: malloc failure\n\n");
      return(-1); }
    if( (pnt_vbou = (double *)malloc( (np+1)*sizeof(double) )) == NULL )
    { printf(" ERROR: malloc failure\n\n");
      return(-1); }
    if( (npc = (int *)malloc( (nurbs[nurbsnr].nc[patch]+1)*sizeof(int) )) == NULL )
    { printf(" ERROR: malloc failure\n\n");
      return(-1); }

    if( (pntm_u = (double *)malloc( (np+1)*sizeof(double) )) == NULL )
    { printf(" ERROR: malloc failure\n\n");
      return(-1); }
    if( (pntm_v = (double *)malloc( (np+1)*sizeof(double) )) == NULL )
    { printf(" ERROR: malloc failure\n\n");
      return(-1); }

    np=k=0;
    for(i=0; i<nurbs[nurbsnr].nc[patch]; i++)
    {
      if(surf[nr].etyp==8)
      {
        npc[k]=(nurbs[nurbsnr].np[patch][i]/2);

#if TEST
    printf(" in mesh_tr3u: surf:%s has %d ambiguous points.\n", surf[nr].name,nurbs[nurbsnr].sum_ambiguousPnts[patch][i]);
    printf(" uvflipped:%d\n", nurbs[nurbsnr].uvflipped[patch][i]);
#endif

        if(nurbs[nurbsnr].sum_ambiguousPnts[patch][i]==1)
	{
          if(nurbs[nurbsnr].uvflipped[patch][i])
	  {
            n=2;
            //npc[k]-=1;
	  }
          else n=0;
	}
        else if(nurbs[nurbsnr].sum_ambiguousPnts[patch][i]>1)
	{
          printf(" ERROR in mesh_tr3u: surf:%s has %d ambiguous points. This is not meshable with tr6u\n", surf[nr].name,nurbs[nurbsnr].sum_ambiguousPnts[patch][i]);
          return(-1);
        }
        else
	{
          n=0;
	}
      } 
      else
      {
        npc[k]=nurbs[nurbsnr].np[patch][i]-1;
        n=0;
      }

      /* get the uv-values of the single curves which trim a surface */
      /* and scale them into real world length */
      p=0;
      for(j=0; j<npc[k]; j++)
      {
        pnt_u[np]=nurbs[nurbsnr].uv[patch][i][n++];
        pnt_v[np]=nurbs[nurbsnr].uv[patch][i][n++];

#if TEST
        printf("patch:%d i:%d flip:%d j:%d n(2=flip):%d p:%d np:%d uv:%lf %lf xyz:%lf %lf %lf\n",patch,i,nurbs[nurbsnr].uvflipped[patch][i],j,n,p,np, pnt_u[np], pnt_v[np], nurbs[nurbsnr].xyz[patch][i][p]*scale->w+scale->x, nurbs[nurbsnr].xyz[patch][i][p+1]*scale->w+scale->y,  nurbs[nurbsnr].xyz[patch][i][p+2]*scale->w+scale->z); 
#endif

        //printf(" node %d %f %f %f\n", np+1, pnt_u[np],pnt_v[np],0.);
        pnt_ubou[np]=intpol( tab_u, tab_lu, UV_STEPS, pnt_u[np] );
        pnt_vbou[np]=intpol( tab_v, tab_lv, UV_STEPS, pnt_v[np] );
        //printf(" node %d %f %f %f\n", np+1, pnt_u[np],pnt_v[np],0.);

	if(surf[nr].etyp==8)
	{
          p+=3;
          pntm_u[np]=nurbs[nurbsnr].uv[patch][i][n++];
          pntm_v[np]=nurbs[nurbsnr].uv[patch][i][n++];
 	}
        else
	{
          if(j)
	  {
            l=np-1;
            pntm_u[l]=(pnt_ubou[np-1]+pnt_ubou[np])*.5;
            pntm_v[l]=(pnt_vbou[np-1]+pnt_vbou[np])*.5;
            pntm_u[l]=intpol( tab_lu, tab_u, UV_STEPS, pntm_u[l] );
            pntm_v[l]=intpol( tab_lv, tab_v, UV_STEPS, pntm_v[l] );
	  }
          if(j==npc[k]-1)
	  {
            pntm_u[np]=(pnt_ubou[0]+pnt_ubou[np])*.5;
            pntm_v[np]=(pnt_vbou[0]+pnt_vbou[np])*.5;
            pntm_u[np]=intpol( tab_lu, tab_u, UV_STEPS, pntm_u[np] );
            pntm_v[np]=intpol( tab_lv, tab_v, UV_STEPS, pntm_v[np] );
	  }
	}

        /* fill a field with related nodes at the edges */
        edge_n2[np][0]=np-1;
        edge_n2[np][1]=np+1;
        np++;
      }
      edge_n2[np-npc[k]][0]=np-1;
      edge_n2[np-1][1]=np-npc[k];
      //for(j=1; j<=npc[k]; j++) printf("%d edge_n2[%d][0]=%d    [1]=%d\n",j, np-j,edge_n2[np-j][0], edge_n2[np-j][1]);

      k++;
    }

    /* calculate the mesh density criterion based on the distance at the middle node of 3 nodes to the straight line n1n3 in normal dir */
    /*  */ 

    if( (pnt_bou = (Points *)malloc((np+1)*sizeof(Points) )) == NULL )
    { printf(" ERROR: malloc failure\n\n"); return(-1); }
    if( (pm = (Points *)realloc(pm, (np)*sizeof(Points) )) == NULL )
    { printf(" ERROR: malloc failure\n\n"); return(-1); }
    if( (nv = (Points *)realloc(nv, (np)*sizeof(Points) )) == NULL )
    { printf(" ERROR: malloc failure\n\n"); return(-1); }
    // nurbs[nurbsnr].xyz[patch][i][p] can not be used since uv can be flipped but nurbs.xyz was not. So pnt_bou must be calc from uv
    evalNurbs( nurbs, nurbsnr, np, pnt_u, pnt_v, pnt_bou);
    evalNurbsWithNormalVector( nurbs, nurbsnr, np, pntm_u, pntm_v, pm, nv);
    max_ratio=0.;
    for (n=0; n<np-1; n++)
    {
      v_result(&pnt_bou[n].px,&pnt_bou[n+1].px, v0);
      v_scal(&u,v0,v1);
      v_add(&pnt_bou[n].px,v1,p1);
      v_result(&pm[n].px,p1,p0p1);
      v_norm( &nv[n].px, p0);
      dist=dabs(v_sprod( p0p1, p0));
      vl=v_betrag(v0);
#if TEST
      printf(" pnt p%d %f %f %f\n", n, pnt_bou[n].px*scale->w+scale->x,pnt_bou[n].py*scale->w+scale->y,pnt_bou[n].pz*scale->w+scale->z);
      printf(" pnt px%d %f %f %f\n", n, p1[0]*scale->w+scale->x,p1[1]*scale->w+scale->y,p1[2]*scale->w+scale->z);
      printf(" pnt pm%d %f %f %f\n", n, pm[n].px*scale->w+scale->x,pm[n].py*scale->w+scale->y,pm[n].pz*scale->w+scale->z);
      printf(" pnt p%d %f %f %f\n", n+1, pnt_bou[n+1].px*scale->w+scale->x,pnt_bou[n+1].py*scale->w+scale->y,pnt_bou[n+1].pz*scale->w+scale->z);
      printf("# dist %f l %f max_ratio:%f\n", dist*scale->w, vl*scale->w, max_ratio);
#endif
      if(!vl) continue;
      vl=dist/vl;
      // save the max ratio
      if(vl>max_ratio) max_ratio=vl;
      // save the average ratio
      //max_ratio+=vl/np;
    }
    free(pnt_bou);
    free(pm);
    free(nv);
    free(pntm_u);
    free(pntm_v);
    pntm_u=NULL;
    pntm_v=NULL;
    /* a 1/3rd since in calcMeshDistance() the criterion is dist/circumlength (length of 3 lines!)*/ 
    max_ratio*=0.3333;
    printf("max_ratio:%f\n",max_ratio);
    if(max_ratio<MESH2D_QUALITY_THRESHOLD)
    {
      max_ratio=MESH2D_QUALITY_THRESHOLD;
      printf("max_ratio set to MESH2D_QUALITY_THRESHOLD:%f\n",max_ratio);
    }


    /* mesh the surface with triangles */

    if(surf[nr].eparm!=(char *)NULL) alpha=beta=(double)atof(surf[nr].eparm) * 0.8;
    else { alpha=meshp.alpha; beta=meshp.beta; }

    for (n=0; n<np; n++)
    {
        pnt_u[n]=pnt_ubou[n];
        pnt_v[n]=pnt_vbou[n];
    }

    /* loop until the triangles are close enough to the nurbs */
    /* arrays need a buffer */
    loop=0;
    np_bou=np;
    while(1)
    {
      loop++;
#if TEST
      printf("2D mesh loop:%d surf:%s alpha:%f\n", loop, surf[nr].name, alpha);
#endif
      //for(i=0; i<np; i++) printf(" node %d %f %f %f\n", i+1, pnt_u[i],pnt_v[i],0);
      e=mesh2d(&np, k, npc, &pnt_u, &pnt_v, &pnt_flag, &tri, alpha, beta, meshp.nadapt );
#if TEST
      printf(" elements:%d\n", e);
#endif
      if(e==0) goto no_elems;

      /* determine the coordinates of the triangles */
      if( (pnt_uscal = (double *)realloc(pnt_uscal, (np+1)*sizeof(double) )) == NULL )
      { printf(" ERROR: malloc failure\n\n");
        return(-1); }
      if( (pnt_vscal = (double *)realloc(pnt_vscal, (np+1)*sizeof(double) )) == NULL )
      { printf(" ERROR: malloc failure\n\n");
        return(-1); }
      for(i=0; i<np; i++)
      {
        //printf(" node %d %f %f %f\n", i+1, pnt_u[i],pnt_v[i],0.);

        // keep the scaled uv coords 
        pnt_uscal[i]=pnt_u[i];
        pnt_vscal[i]=pnt_v[i];

        // back into real u,v-coordinates
        pnt_u[i]=intpol( tab_lu, tab_u, UV_STEPS, pnt_u[i] );
        if(pnt_u[i]>umax) pnt_u[i]=umax;
        else if(pnt_u[i]<umin) pnt_u[i]=umin;
        pnt_v[i]=intpol( tab_lv, tab_v, UV_STEPS, pnt_v[i] );
        if(pnt_v[i]>vmax) pnt_v[i]=vmax;
        else if(pnt_v[i]<vmin) pnt_v[i]=vmin;
        //printf(" node %d %f %f %f\n", i+1, pnt_u[i],pnt_v[i],0.);
      }
      if( (ppre = (Points *)realloc(ppre, (np+1)*sizeof(Points) )) == NULL )
      { printf(" ERROR: malloc failure\n\n");
        return(-1); }
      evalNurbs( nurbs, nurbsnr, np, pnt_u, pnt_v, ppre);
      //for(i=0; i<np; i++) printf(" pnt P%d %f %f %f\n",i+1, ppre[i].px* scale->w+scale->x, ppre[i].py* scale->w+scale->y, ppre[i].pz* scale->w+scale->z);

      //if(e<4) break;

      if(loop>=MAX_MESH2D_LOOPS) break;

      /* if no alpha was pre-defined then search for bad elements and eventually increase the mesh density */
#if TEST
      if(surf[nr].eparm!=(char *)NULL) printf(" alpha:%f\n",alpha );
#endif
      if((surf[nr].eparm==(char *)NULL)&&(!checkMesh(np, ppre, e, tri)))
      {
        if(alpha>=MESH2D_MAX_APLHABETA)
	{
          /* change in mesh-density did not solve the problem, continue with bad elements for later manual correction */
	  break;
	}
	else
	{
          alpha=alpha*alphaFactor;
          beta=alpha;
          if(alpha<MESH2D_MIN_APLHABETA)
	  {
            /* a finer mesh could not solve the problem, try to coarsen the mesh */
            if(alphaFactor<1.)
	    {
              if(surf[nr].eparm!=(char *)NULL) alpha=beta=(double)atof(surf[nr].eparm) * 0.8;
              else { alpha=meshp.alpha; beta=meshp.beta; }
              alphaFactor=1./ALPHAFACTOR;
              alpha=alpha*alphaFactor;
              triSplitterFlag=1;
	    }
	  }
#if TEST
          printf(" bad elem, new alpha:%f\n", alpha);
#endif
          goto restoreFields; 
	}
      }
      // if the mesh-density was decreased do not increase again!
      if(alphaFactor>1.) break;
 
      /* check how close the mesh is to the nurbs and eventually increase the mesh density */

      /* determine the location of the tri-cg on the nurbs */
      if( (cg_u = (double *)realloc(cg_u, (e)*sizeof(double) )) == NULL )
      { printf(" ERROR: malloc failure\n\n");
      return(-1); }
      if( (cg_v = (double *)realloc(cg_v, (e)*sizeof(double) )) == NULL )
      { printf(" ERROR: malloc failure\n\n");
      return(-1); }

      //printf(" seto cg\n");
      /* decrement the pointer, tri[0] points to pnt_uscal[1] */
      pnt_uscal--;
      pnt_vscal--;
      j=0;
      for(i=0; i<e; i++)
      {
        cg_u[i]=0.;
        cg_v[i]=0.;
        for(n=0; n<3; n++)
        {
          cg_u[i]+=pnt_uscal[tri[j+n]];
          cg_v[i]+=pnt_vscal[tri[j+n]];
        }
        cg_u[i]/=3.;
        cg_v[i]/=3.;
        j+=3;

        //printf(" node %d %f %f %f\n", i+1+np, cg_u[i],cg_v[i],0.);

        // back into real u,v-coordinates
        cg_u[i]=intpol( tab_lu, tab_u, UV_STEPS, cg_u[i] );
        if(cg_u[i]>umax) cg_u[i]=umax;
        else if(cg_u[i]<umin) cg_u[i]=umin;
        cg_v[i]=intpol( tab_lv, tab_v, UV_STEPS, cg_v[i] );
        if(cg_v[i]>vmax) cg_v[i]=vmax;
        else if(cg_v[i]<vmin) cg_v[i]=vmin;

        //printf(" node %d %f %f %f\n", i+1+np, cg_u[i],cg_v[i],0.);
      }
      pnt_uscal++;
      pnt_vscal++;

      if( (pcg = (Points *)realloc(pcg, (e+1)*sizeof(Points) )) == NULL )
      { printf(" ERROR: malloc failure\n\n");
      return(-1); }
      evalNurbs( nurbs, nurbsnr, e, cg_u, cg_v, pcg);
      //for(i=0; i<e; i++) printf(" pnt P%d %f %f %f # %d %f %f\n",i+1+np, pcg[i].px* scale->w+scale->x, pcg[i].py* scale->w+scale->y, pcg[i].pz* scale->w+scale->z,  i, cg_u[i],cg_v[i]);
      qfactor=calcMeshDistance(nurbs, nurbsnr, np, ppre, e, pcg, tri);
#if TEST
      printf(" tri-cg distance to nurbs qfactor:%f\n", qfactor);
#endif

      if(qfactor<=max_ratio) break;
      if(alpha==MESH2D_MIN_APLHABETA) break;
      alpha=alpha-(qfactor*MESH2D_CURVATURE_FACTOR);
      beta=alpha;
      if(alpha<MESH2D_MIN_APLHABETA) { alpha=MESH2D_MIN_APLHABETA; triSplitterFlag=1; }

      /* if an individual alpha was pre-defined then do not re-mesh and break */
      if(surf[nr].eparm!=(char *)NULL) { break; }
#if TEST
      printf(" mesh too coarse, mesh with new alpha:%f\n", alpha);
#endif

      /* restore fields for meshing */
    restoreFields:;
      np=k=0;
      for(i=0; i<nurbs[nurbsnr].nc[patch]; i++)
      {
        for(j=0; j<npc[k]; j++)
        {
          pnt_u[np]=pnt_ubou[np];
          pnt_v[np]=pnt_vbou[np];
          np++;
	}
        k++;
      }
    }

    /* find the original point-index of the boundary nodes */

    /* memory for the boundary node-seach algorithm */
    /* stelle daten fuer near3d bereit */
    if ( (rsort = (Rsort *)realloc((Rsort *)rsort, (np_bou+1) * sizeof(Rsort))) == NULL )
      printf("ERROR: realloc failed: Rsort\n\n" ); 
    if ( (orig_x = (double *)malloc( (np_bou+1) * sizeof(double))) == NULL )
      printf("ERROR: realloc failed \n\n" ); 
    if ( (orig_y = (double *)malloc( (np_bou+1) * sizeof(double))) == NULL )
      printf("ERROR: realloc failed \n\n" ); 
    if ( (orig_z = (double *)malloc( (np_bou+1) * sizeof(double))) == NULL )
      printf("ERROR: realloc failed \n\n" ); 
    if ( (sort_x = (double *)malloc( (np_bou+1) * sizeof(double))) == NULL )
      printf("ERROR: realloc failed \n\n" ); 
    if ( (sort_y = (double *)malloc( (np_bou+1) * sizeof(double))) == NULL )
      printf("ERROR: realloc failed \n\n" ); 
    if ( (sort_z = (double *)malloc( (np_bou+1) * sizeof(double))) == NULL )
      printf("ERROR: realloc failed \n\n" ); 
    if ( (sort_nx = (int *)malloc( (np_bou+1) * sizeof(int))) == NULL )
      printf("ERROR: realloc failed \n\n" ); 
    if ( (sort_ny = (int *)malloc( (np_bou+1) * sizeof(int))) == NULL )
      printf("ERROR: realloc failed \n\n" ); 
    if ( (sort_nz = (int *)malloc( (np_bou+1) * sizeof(int))) == NULL )
      printf("ERROR: realloc failed \n\n" ); 

    if ( (pnt_indx = (int *)malloc( (np+1) * sizeof(int))) == NULL )
      printf("ERROR: realloc failed \n\n" ); 
  
    for(i=0; i<np_bou; i++)
    {
      rsort[i].r=orig_x[i]=pnt_ubou[i];
      rsort[i].i=i;
    }
    qsort( rsort, np_bou, sizeof(Rsort), (void *)compareRsort );
    for(i=0; i<np_bou; i++)
    {
      sort_x[i]=rsort[i].r;
      sort_nx[i]=rsort[i].i;
    }
    for(i=0; i<np_bou; i++)
    {
      rsort[i].r=orig_y[i]=pnt_vbou[i];
      rsort[i].i=i;
    }
    qsort( rsort, np_bou, sizeof(Rsort), (void *)compareRsort );
    for(i=0; i<np_bou; i++)
    {
      sort_y[i]=rsort[i].r;
      sort_ny[i]=rsort[i].i;
    }

    for(i=0; i<np_bou; i++)
    {
      sort_z[i]=orig_z[i]=0.;
      sort_nz[i]=i;
    }

    n=0; for(i=0; i<np; i++)
    {
      /* if its a boundary node (pnt_flag=1) search the original one */ 
      
      if(pnt_flag[i]!=0)
      {
        near3d(orig_x,orig_y,orig_z,sort_x,sort_y,sort_z,sort_nx,sort_ny,sort_nz, pnt_uscal[i], pnt_vscal[i], 0., np_bou, &near_node[0], (int)1);
        pnt_indx[i]=near_node[0];
        //printf("new_nodnr:%d bou_nod:%d \n", i,near_node[0] ); 
      }
    }
    free(orig_x);
    free(sort_x);
    free(sort_nx);
    free(orig_y);
    free(sort_y);
    free(sort_ny);
    free(orig_z);
    free(sort_z);
    free(sort_nz);

    /* the mesh needs further refinement if triSplitterFlag==1 (alpha did not solve the problem) */
    if(triSplitterFlag)
    {
#if TEST
      printf("np:%d e:%d\n", np,e);
#endif
      /* call the triSplitter for individual adjustments */
      triSplitter( nurbs, nurbsnr, &np, &ppre, &e, &tri, &pnt_flag, edge_n2, pnt_indx, &pnt_u, &pnt_v, max_ratio, tab_u, tab_lu, tab_bu, tab_v, tab_lv, tab_bv );
#if TEST
      printf("triSplitter, np:%d e:%d\n", np,e);
#endif
      if ( (pnt_indx = (int *)realloc(pnt_indx, (np+1) * sizeof(int))) == NULL )
        printf("ERROR: realloc failed \n\n" ); 
    }
    for(i=0; i<np_bou; i++) free(edge_n2[i]); free(edge_n2);

    /* create nodes */
#if TEST
    printf(" create nodes\n");
#endif

    /* store all node-numbers (not indexes) used by the surf in an array snods[j] */
    if( ( snods= (int *)malloc( (1)*sizeof(int) )) == NULL )
    { printf(" ERROR: malloc failure in mesh_tr3u(), nurbs:%s can not be shaped\n\n", nurbs[nurbsnr].name);
      return(-1); }

    if ( (n1nm = (N1nm *)malloc( (apre->nmax+1) * sizeof(N1nm))) == NULL )
    { printf("\n\n ERROR in mids: malloc\n\n") ; exit(-1); }    
    for (i=0; i<=apre->nmax; i++) n1nm[i].sum=0;
    for (i=0; i<=apre->nmax; i++) n1nm[i].n2=n1nm[i].nm=NULL;

    anz_n=0;
    for (n=0; n<surf[nr].nl; n++)
    {
      k=surf[nr].l[n];
      if( surf[nr].typ[n]=='c' )
      {
        for( l=0; l<lcmb[k].nl; l++ )
        {
          m=lcmb[k].l[l];
          if( ( snods= (int *)realloc((int *)snods, (anz_n+line[m].nn+2)*sizeof(int) )) == NULL )
          { printf(" ERROR: malloc failure in mesh_tr3u(), nurbs:%s can not be shaped\n\n", nurbs[nurbsnr].name); return(-1); }

          ini_anzn=anz_n;
          snods[anz_n++]=point[line[m].p1].nod[0];
          for(i=0; i<line[m].nn; i++) snods[anz_n++]=line[m].nod[i];
          snods[anz_n++]=point[line[m].p2].nod[0];

          /* in case of tr6u remember midside nodes on the boundary */
          if(surf[nr].etyp==8)
          {
            do{
              n1=snods[ini_anzn++];
              nm=snods[ini_anzn++];
              n2=snods[ini_anzn];
              if ( (n1nm[n1].n2 = (int *)realloc( n1nm[n1].n2, (n1nm[n1].sum+1) * sizeof(int))) == NULL )
              { printf("\n\n ERROR in mids: realloc\n\n") ; exit(-1); }    
              if ( (n1nm[n1].nm = (int *)realloc( n1nm[n1].nm, (n1nm[n1].sum+1) * sizeof(int))) == NULL )
              { printf("\n\n ERROR in mids: realloc\n\n") ; exit(-1); }    
              n1nm[n1].n2[n1nm[n1].sum]=n2;
              n1nm[n1].nm[n1nm[n1].sum]=nm;
              n1nm[n1].sum++;
	    }while(ini_anzn<anz_n-1);
          }

        }
      }
      else
      {
        if( ( snods= (int *)realloc((int *)snods,  (anz_n+line[k].nn+4)*sizeof(int) )) == NULL )
        { printf(" ERROR: malloc failure in mesh_tr3u(), nurbs:%s can not be shaped\n\n", nurbs[nurbsnr].name); return(-1); }

        ini_anzn=anz_n;
        snods[anz_n++]=point[line[k].p1].nod[0];
        for(i=0; i<line[k].nn; i++) snods[anz_n++]=line[k].nod[i];
        snods[anz_n++]=point[line[k].p2].nod[0];

        /* in case of tr6u remember midside nodes on the boundary */
        if(surf[nr].etyp==8)
        {
          do{
            n1=snods[ini_anzn++];
            nm=snods[ini_anzn++];
            n2=snods[ini_anzn];
            if ( (n1nm[n1].n2 = (int *)realloc( n1nm[n1].n2, (n1nm[n1].sum+1) * sizeof(int))) == NULL )
            { printf("\n\n ERROR in mids: realloc\n\n") ; exit(-1); }    
            if ( (n1nm[n1].nm = (int *)realloc( n1nm[n1].nm, (n1nm[n1].sum+1) * sizeof(int))) == NULL )
            { printf("\n\n ERROR in mids: realloc\n\n") ; exit(-1); }    
            n1nm[n1].n2[n1nm[n1].sum]=n2;
            n1nm[n1].nm[n1nm[n1].sum]=nm;
            n1nm[n1].sum++;
	  }while(ini_anzn<anz_n-2);
        }
      }
    }

    /* check if the node exists already (line, point) or create a new one */
    if( ( nod_mesh2d= (int *)malloc( (np+1)*sizeof(int) )) == NULL )
    { printf(" ERROR: malloc failure\n\n");
      return(-1); }

    /* allocate memory for embeded nodes */
    if( ( surf[nr].nod=(int *)realloc( (int *)surf[nr].nod, (np+1)*sizeof(int)) )==NULL)
    { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[nr].name);
      return(-1); }

    //replace later:
    /* memory for the boundary node-seach algorithm */
#if TEST
    printf(" boundary node-seach\n");
#endif

    /* stelle daten fuer near3d bereit */
    if ( (rsort = (Rsort *)realloc((Rsort *)rsort, (anz_n+1) * sizeof(Rsort))) == NULL )
      printf("ERROR: realloc failed: Rsort\n\n" ); 
    if ( (orig_x = (double *)malloc( (anz_n+1) * sizeof(double))) == NULL )
      printf("ERROR: realloc failed \n\n" ); 
    if ( (orig_y = (double *)malloc( (anz_n+1) * sizeof(double))) == NULL )
      printf("ERROR: realloc failed \n\n" ); 
    if ( (orig_z = (double *)malloc( (anz_n+1) * sizeof(double))) == NULL )
      printf("ERROR: realloc failed \n\n" ); 
    if ( (sort_x = (double *)malloc( (anz_n+1) * sizeof(double))) == NULL )
      printf("ERROR: realloc failed \n\n" ); 
    if ( (sort_y = (double *)malloc( (anz_n+1) * sizeof(double))) == NULL )
      printf("ERROR: realloc failed \n\n" ); 
    if ( (sort_z = (double *)malloc( (anz_n+1) * sizeof(double))) == NULL )
      printf("ERROR: realloc failed \n\n" ); 
    if ( (sort_nx = (int *)malloc( (anz_n+1) * sizeof(int))) == NULL )
      printf("ERROR: realloc failed \n\n" ); 
    if ( (sort_ny = (int *)malloc( (anz_n+1) * sizeof(int))) == NULL )
      printf("ERROR: realloc failed \n\n" ); 
    if ( (sort_nz = (int *)malloc( (anz_n+1) * sizeof(int))) == NULL )
      printf("ERROR: realloc failed \n\n" ); 
  
    for(i=0; i<anz_n; i++)
    {
      rsort[i].r=orig_x[i]=npre[snods[i]].nx;
      rsort[i].i=i;
    }
    qsort( rsort, anz_n, sizeof(Rsort), (void *)compareRsort );
    for(i=0; i<anz_n; i++)
    {
      sort_x[i]=rsort[i].r;
      sort_nx[i]=rsort[i].i;
    }
    for(i=0; i<anz_n; i++)
    {
      rsort[i].r=orig_y[i]=npre[snods[i]].ny;
      rsort[i].i=i;
    }
    qsort( rsort, anz_n, sizeof(Rsort), (void *)compareRsort );
    for(i=0; i<anz_n; i++)
    {
      sort_y[i]=rsort[i].r;
      sort_ny[i]=rsort[i].i;
    }
    for(i=0; i<anz_n; i++)
    {
      rsort[i].r=orig_z[i]=npre[snods[i]].nz;
      rsort[i].i=i;
    }
    qsort( rsort, anz_n, sizeof(Rsort), (void *)compareRsort );
    for(i=0; i<anz_n; i++)
    {
      sort_z[i]=rsort[i].r;
      sort_nz[i]=rsort[i].i;
    }

    // end replace later

    n=0; for(i=0; i<np; i++)
    {
      /* if ppre is a boundary node (pnt_flag=1) search the original one */ 
      if(pnt_flag[i]!=0)
      {
         near3d(orig_x,orig_y,orig_z,sort_x,sort_y,sort_z,sort_nx,sort_ny,sort_nz, ppre[i].px, ppre[i].py, ppre[i].pz, anz_n, &near_node[0], (int)1);

	 nod_mesh2d[i+1]=snods[near_node[0]];
	//printf("use bounod:%d from newnod:%d nodenr:%d\n", pnt_indx[i],i,snods[pnt_indx[i]]);
        //nod_mesh2d[i+1]=snods[pnt_indx[i]];
      }
      else
      {
        nod( apre, &npre, 0, apre->nmax+1,ppre[i].px, ppre[i].py, ppre[i].pz, 0 );
        nod_mesh2d[i+1]=apre->nmax;
        surf[nr].nod[n++]=apre->nmax;
        if ( (n1nm = (N1nm *)realloc((N1nm *)n1nm, (apre->nmax+1) * sizeof(N1nm))) == NULL )
        { printf("\n\n ERROR in mids: malloc\n\n") ; exit(-1); }    
        n1nm[apre->nmax].sum=0;
        n1nm[apre->nmax].n2=(int *)NULL;
        n1nm[apre->nmax].nm=(int *)NULL;
	// printf("apre->nmax:%d\n",apre->nmax);
      }
    }
    surf[nr].nn=n;

    /* create elements */
#if TEST
    printf(" create elements\n");
#endif

    /* allocate memory for embeded elements */
    if((surf[nr].elem=(int *)realloc((int *)surf[nr].elem, (e+1)*sizeof(int)) )==NULL)
    { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed, surf->elem\n\n", surf[nr].name); return(-1); }

    surf[nr].ne=0;
    /* in case of tr6u create midside nodes */
    if(surf[nr].etyp==8)
    {
      j=0; for(i=0; i<e; i++)
      {
        /* do not create elements which have 2 unique nodes */
        if((nod_mesh2d[tri[j+1]]!=nod_mesh2d[tri[j]])&&(nod_mesh2d[tri[j+2]]!=nod_mesh2d[tri[j]])&&(nod_mesh2d[tri[j+2]]!=nod_mesh2d[tri[j+1]]))
        {
          /* the triangles must be inverted if the product of the nurbs-ori and surf-ori is '-' */
          if(surf[nr].sho=='-') oprod=-1; else oprod=1;
          if(surf[nr].ori=='-') oprod*=-1; else oprod*=1;
          if(oprod==1)
          {
  	    ebuf[0]=nod_mesh2d[tri[j]];
  	    ebuf[1]=nod_mesh2d[tri[j+1]];
  	    ebuf[2]=nod_mesh2d[tri[j+2]];
            /* midside nodes */
            ibuf[0]=tri[j]-1;
            ibuf[1]=tri[j+1]-1;
            ibuf[2]=tri[j+2]-1;
            genMidsideNodes(nr, &n1nm, &nm_i, &sum_nm, &pntm_u, &pntm_v, ebuf,ibuf, pnt_u, pnt_v, tab_u, tab_lu, tab_bu, tab_v, tab_lv, tab_bv);
            elem_define( anz->emax+1, 8, ebuf, 0, surf[nr].eattr );
          }
          else
          {
  	    ebuf[0]=nod_mesh2d[tri[j+2]];
  	    ebuf[1]=nod_mesh2d[tri[j+1]];
  	    ebuf[2]=nod_mesh2d[tri[j]];
            /* midside nodes */
            ibuf[2]=tri[j]-1;
            ibuf[1]=tri[j+1]-1;
            ibuf[0]=tri[j+2]-1;
            genMidsideNodes(nr, &n1nm, &nm_i, &sum_nm, &pntm_u, &pntm_v, ebuf,ibuf, pnt_u, pnt_v, tab_u, tab_lu, tab_bu, tab_v, tab_lv, tab_bv);
            elem_define( anz->emax+1, 8, ebuf, 0, surf[nr].eattr );
          }
          surf[nr].elem[surf[nr].ne]=anz->emax;
          surf[nr].ne++;
        }
        j+=3;
      }

      /* calculate the mid-node coordinates */
      if( (ppre = (Points *)realloc(ppre, (sum_nm+1)*sizeof(Points) )) == NULL )
      { printf(" ERROR: malloc failure\n\n"); return(-1); }

      evalNurbs( nurbs, nurbsnr, sum_nm, pntm_u, pntm_v, ppre);
      free(pntm_u); free(pntm_v);
      pntm_u=pntm_v=NULL;
      //for(i=0; i<sum_nm; i++) printf(" pnt P%d %f %f %f\n",i+1, ppre[i].px* scale->w+scale->x, ppre[i].py* scale->w+scale->y, ppre[i].pz* scale->w+scale->z);
      for(i=0; i<sum_nm; i++)
      {
        npre[npre[nm_i[i]].nr].nx=ppre[i].px;
        npre[npre[nm_i[i]].nr].ny=ppre[i].py;
        npre[npre[nm_i[i]].nr].nz=ppre[i].pz;
      }
      free(nm_i);
      nm_i=NULL;

    }
    else
    {
      j=0; for(i=0; i<e; i++)
      {
        /* do not create elements which have 2 unique nodes */
        if((nod_mesh2d[tri[j+1]]!=nod_mesh2d[tri[j]])&&(nod_mesh2d[tri[j+2]]!=nod_mesh2d[tri[j]])&&(nod_mesh2d[tri[j+2]]!=nod_mesh2d[tri[j+1]]))
        {
          /* the triangles must be inverted if the product of the nurbs-ori and surf-ori is '-' */
          if(surf[nr].sho=='-') oprod=-1; else oprod=1;
          if(surf[nr].ori=='-') oprod*=-1; else oprod*=1;
          if(oprod==1)
          {
  	    ebuf[0]=nod_mesh2d[tri[j]];
  	    ebuf[1]=nod_mesh2d[tri[j+1]];
  	    ebuf[2]=nod_mesh2d[tri[j+2]];
            elem_define( anz->emax+1, 7, ebuf, 0, surf[nr].eattr );
          }
          else
          {
    	    ebuf[0]=nod_mesh2d[tri[j+2]];
  	    ebuf[1]=nod_mesh2d[tri[j+1]];
  	    ebuf[2]=nod_mesh2d[tri[j]];
            elem_define( anz->emax+1, 7, ebuf, 0, surf[nr].eattr );
          }
          surf[nr].elem[surf[nr].ne]=anz->emax;
          surf[nr].ne++;
        }
        j+=3;
      }
    }


#if TEST1
    handle = fopen ("all.msh", "w");
    fprintf(handle, "*NODE, NSET=Nall\n");
    for(j=0; j< np; j++)
      fprintf(handle, "%d, %lf, %lf, %lf\n", j+1, ppre[j].px, ppre[j].py, ppre[j].pz);
    fprintf(handle, "*ELEMENT, TYPE=S3R, ELSET=Eall\n");
    j=0;for(i=0; i<e; i++)
    {
    	fprintf(handle, "%d, %d, %d, %d\n", i+1, tri[j], tri[j+1], tri[j+2]); j+=3;
    }
    fprintf(handle, "*END STEP\n");
    fclose(handle);
#endif

    /* free the temporary space */
#if TEST
    printf(" free the temporary space\n");
#endif
    for (i=0; i<=apre->nmax; i++) {  free(n1nm[i].n2);  free(n1nm[i].nm); }
    free(n1nm);
    n1nm=NULL;
 no_elems:;
    free(pnt_ubou);
    free(pnt_vbou);
    free(pnt_uscal);
    free(pnt_vscal);
    free(npc); free(pnt_u); free(pnt_v);
    free(tri); free(ppre); free(pcg);
    free(cg_u); free(cg_v); free(snods);
    free(pnt_flag); free(pnt_indx); free(nod_mesh2d);

    pnt_ubou=NULL;
    pnt_vbou=NULL;
    pnt_uscal=NULL;
    pnt_vscal=NULL;
    npc=NULL;
    pnt_u=NULL; pnt_v=NULL; 
    tri=NULL;
    ppre=NULL;
    pcg=NULL;
    cg_u=NULL;
    cg_v=NULL;
    snods=NULL;
    pnt_flag=NULL;
    pnt_indx=NULL;
    nod_mesh2d=NULL;
  }
#if TEST
  printf(" leave\n");
#endif
  if(surf[nr].ne<=0) return(-1);
  else return(surf[nr].ne);
}



int mesh_tr3g( int s )
{
  int i,j,k=0,l,m,n,e,p, anz_n, anz_e;
  int min_nod=0;
  double min_dr, dr;
  Nodes *nlocal=NULL;
  Elements *elocal=NULL;
  Rsort *rsort=NULL;
  int *snods=NULL;
  int ebuf[20];

  if(shape[surf[s].sh].type!=4) return(-1);
  if(surf[s].npgn<1) return(-1);

  /* create the uv patch and render with GLU */
  repSurf(s, 1);

  /* allocate memory for local nodes */
  if ( (nlocal = (Nodes *)malloc( (surf[s].npgn+1) * sizeof(Nodes))) == NULL )
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[s].name);
    return(-1); }

  /* allocate memory for embeded nodes */
  if( ( surf[s].nod=(int *)realloc( (int *)surf[s].nod, (3*surf[s].npgn+1)*sizeof(int)) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[s].name);
    return(-1); }

  /* allocate memory for embeded elements */
  if((surf[s].elem=(int *)realloc((int *)surf[s].elem, (surf[s].npgn+1)*sizeof(int)) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed, surf->elem\n\n", surf[s].name); return(-1); }

  /* write the interiour of the surface */
  n=e=0;
  p=0;
  while((surf[s].npgn-n))
  {
    /* create temporary nodes */
    n++;
    j=surf[s].pgn[n++];
    n+=3;
    for(k=0; k<j; k++)
    {
      p++;
      nlocal[p].nx=surf[s].pgn[n];
      nlocal[p].ny=surf[s].pgn[n+1];
      nlocal[p].nz=surf[s].pgn[n+2];
      n+=3;
    }

    /* create the element with local nodes */
    if (surf[s].ori=='+')
    {
      if(j==3)
      {
        if((elocal=(Elements *)realloc( (Elements *)elocal,(e+2)*sizeof(Elements)))==NULL)
        { printf("ERROR: realloc failure\n\n"); return(-1); }
        elocal[e].group=1;
        elocal[e].mat=1;
        elocal[e].nod[0]=p-2;
        elocal[e].nod[1]=p-1;
        elocal[e].nod[2]=p;
      }
      else printf("wrong number of nodes, number not supported\n");
    }
    else
    {
      if(j==3)
      {
        if((elocal=(Elements *)realloc( (Elements *)elocal,(e+2)*sizeof(Elements)))==NULL)
        { printf("ERROR: realloc failure\n\n"); return(-1); }
        elocal[e].group=1;
        elocal[e].mat=1;
        elocal[e].nod[0]=p;
        elocal[e].nod[1]=p-1;
        elocal[e].nod[2]=p-2;
      }
      else printf("wrong number of nodes, number not supported\n");
    }
    surf[s].elem[e++]=anz->emax;
  }
  surf[s].ne=e;
  anz_e=e;

  /* merge local nodes */
  /* calculate all absolute r of all nodes and sort the indexes according to r */ 
  if ( (rsort = (Rsort *)malloc( (p+1) * sizeof(Rsort))) == NULL )
    printf("ERROR: realloc failed: Rsort\n\n" ); 
  for( i=0; i<p; i++) 
  { 
    nlocal[i+1].nr=0;
    rsort[i].r=sqrt(nlocal[i+1].nx*nlocal[i+1].nx+nlocal[i+1].ny*nlocal[i+1].ny+nlocal[i+1].nz*nlocal[i+1].nz);
    rsort[i].i=i+1;
  }
  qsort( rsort, p, sizeof(Rsort), (void *)compareRsort );
#if TEST
  for (i=0; i<p; i++)
    printf("node:%d n:%d r:%lf\n", i, rsort[i].i, rsort[i].r); 
#endif

  k=p;
  for(i=0; i<k; i++)
  {
    j = merge_nlocal( rsort, nlocal, elocal, e, k, i, 0, gtol/scale->w );
  }
#if TEST
  for (i=0; i<p; i++)
    printf("rsort-indx:%d node:%d r:%lf\n", i, rsort[i].i, rsort[i].r); 
#endif

  /* create real nodes from the remaining ones */
  /* store all node-numbers (not indexes) used by the surf-lines in an array snods[j] */
  if( ( snods= (int *)malloc( (p+1)*sizeof(int) )) == NULL )
  { printf(" ERROR: malloc failure in meshGLU: surf %s\n\n", surf[s].name);
    return(-1); }
  anz_n=0;
  for (n=0; n<surf[s].nl; n++)
  {
    k=surf[s].l[n];
    if( surf[s].typ[n]=='c' )
    {
      for( l=0; l<lcmb[k].nl; l++ )
      {
        m=lcmb[k].l[l];
        if(surf[s].o[n]=='+')
        {
          if(lcmb[k].o[l]=='+')
          {
            snods[anz_n++]=point[line[m].p1].nod[0];
            for(i=0; i<line[m].nn; i++) snods[anz_n++]=line[m].nod[i];
          }
          else
          {
            snods[anz_n++]=point[line[m].p2].nod[0];
            for(i=line[m].nn-1; i>=0; i--) snods[anz_n++]=line[m].nod[i];
          }
	}
        else
        {
          if(lcmb[k].o[l]=='-')
          {
            snods[anz_n++]=point[line[m].p1].nod[0];
            for(i=0; i<line[m].nn; i++) snods[anz_n++]=line[m].nod[i];
          }
          else
          {
            snods[anz_n++]=point[line[m].p2].nod[0];
            for(i=line[m].nn-1; i>=0; i--) snods[anz_n++]=line[m].nod[i];
          }
	}
      }
    }
    else
    {
      if(surf[s].o[n]=='+')
      {
        snods[anz_n++]=point[line[k].p1].nod[0];
        for(i=0; i<line[k].nn; i++) snods[anz_n++]=line[k].nod[i];
      }
      else
      {
        snods[anz_n++]=point[line[k].p2].nod[0];
        for(i=line[k].nn-1; i>=0; i--) snods[anz_n++]=line[k].nod[i];
      }
    }
  }

  /* if a node matches a node in the trimming lines or points (snods) then change to this node */
  /* else create a new one */
  for(i=0; i<anz_n; i++)
  {
    min_dr=MAX_INTEGER;
    for(j=0; j<p; j++)
    {
      if(rsort[j].i!=0)
      {
        dr=(npre[snods[i]].nx-nlocal[rsort[j].i].nx)*(npre[snods[i]].nx-nlocal[rsort[j].i].nx)+(npre[snods[i]].ny-nlocal[rsort[j].i].ny)*(npre[snods[i]].ny-nlocal[rsort[j].i].ny)+(npre[snods[i]].nz-nlocal[rsort[j].i].nz)*(npre[snods[i]].nz-nlocal[rsort[j].i].nz);
        if(dr<min_dr){ min_dr=dr; min_nod=j; }
      }
    }
    /* printf("%d snods:%d min_dr:%lf min_nod:%d n:%d\n", i+1,snods[i],min_dr,min_nod, rsort[min_nod].i ); */
    nlocal[rsort[min_nod].i].nr=snods[i];
    rsort[min_nod].i=0;
  }
  n=0;
  for(i=0; i<p; i++)
  {
    if(rsort[i].i!=0)
    {
  //NOTHREAD if(sem_wait(&sem[10])) error("sem_wait");
      nod( apre, &npre, 0, apre->nmax+1,nlocal[rsort[i].i].nx,nlocal[rsort[i].i].ny,nlocal[rsort[i].i].nz, 0 );
      nlocal[rsort[i].i].nr=apre->nmax;
  //NOTHREAD if(sem_post(&sem[10])) error("sem_post");
      surf[s].nod[n++]=nlocal[rsort[i].i].nr;
    }
  }
  surf[s].nn=n;

  /* Delete additional nodes on the lines of the surf and replace affected elems */
  /* Only the original nodes on the lines are allowed. */
  /* Then all remaining elements between surfs are connected */

  /* update the node-numbers in the elements */
  for(i=0; i<anz_e; i++)
  {
    for(n=0; n<3; n++)
    {
      elocal[i].nod[n]=nlocal[elocal[i].nod[n]].nr;
    }
  }

  anz_e=deleteFreeEdges(npre, snods,anz_n,elocal,anz_e );

  /* delete collapsed elements */
  for(i=0; i<anz_e; i++)
  {
    if(elocal[i].nod[0]==elocal[i].nod[1]) elocal[i].group=0;
    else if(elocal[i].nod[0]==elocal[i].nod[2]) elocal[i].group=0;
    else if(elocal[i].nod[1]==elocal[i].nod[2]) elocal[i].group=0;
  }

  /* create final elements only for not collapsed ones */
  surf[s].ne=0;
  for(i=0; i<anz_e; i++)
  {
    if((elocal[i].nod[0]!=elocal[i].nod[1])&&(elocal[i].nod[0]!=elocal[i].nod[2])&&(elocal[i].nod[1]!=elocal[i].nod[2]))
    {
      if (surf[s].ori=='+')
      {
        ebuf[0]=elocal[i].nod[0];
        ebuf[1]=elocal[i].nod[1];
        ebuf[2]=elocal[i].nod[2];
  //NOTHREAD if(sem_wait(&sem[8])) error("sem_wait");
        elem_define( anz->emax+1, 7, ebuf, 0, surf[s].eattr );
  //NOTHREAD if(sem_post(&sem[8])) error("sem_post");
      }
      else
      {
        ebuf[0]=elocal[i].nod[2];
        ebuf[1]=elocal[i].nod[1];
        ebuf[2]=elocal[i].nod[0];
  //NOTHREAD if(sem_wait(&sem[8])) error("sem_wait");
        elem_define( anz->emax+1, 7, ebuf, 0, surf[s].eattr );
  //NOTHREAD if(sem_post(&sem[8])) error("sem_post");
      }
      surf[s].elem[surf[s].ne++]=anz->emax;
    }
  }

  free(snods);
  free(rsort);
  free(nlocal);
  free(elocal);

  /* reallocate memory for embeded nodes */
  if( ( surf[s].nod=(int *)realloc( (int *)surf[s].nod, (surf[s].nn+1)*sizeof(int)) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[s].name);
    return(-1); }

  /* rellocate memory for embeded elements */
  if((surf[s].elem=(int *)realloc((int *)surf[s].elem, (surf[s].ne+1)*sizeof(int)) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed, surf->elem2\n\n", surf[s].name); return(-1); }

  return(1);
}



/**********************************************************************************/
/* Fuellt ein xyz-feld mit den koordinaten einer Flaeche bei der gegenueber       */
/* liegende seiten ungleiche divisions haben.                                     */
/* Die erforderlichen divisions muessen vorher mit newDivisions() best. werden.   */
/* Die xyz-Koordinaten gelten fuer ein Feld im uv-Raum bei dem die u-achse mit der*/
/* surf[].l[0] zusammenfaellt und die v-achse mit der surf[].l[3].                */
/*                                                                                */
/* in:                                                                            */
/* sur      surface-index                                                         */
/* div_l    feld mit den divisions der surface-edges                              */
/* div_a    ersatzdivision der 'a'-edges der surf.                                */
/* div_b    ersatzdivision der 'b'-edges der surf.                                */
/* sa1,sa2,sb1,sb2  Zuordnung der original-edges zu den a&b-edges                 */
/*                                                                                */
/* out:                                                                           */
/* n_uv        nodeNr im uv-feld, die zusaetzlichen ersatz-stuetzpunkte sind      */
/*             nicht mit nodes belegt da sie bei der Vernetzung rausfallen        */
/* umax, vmax  anzahl der ersatz-stuetzpunkte in 'u' und 'v' bzw entlang edge     */
/*             '0' und '3' ( umax = div_(a|b) +1 )                                */
/* n_ba        nodeNr im ba-feld, siehe n_uv                                      */
/* amax, bmax  anzahl der ersatz-stuetzpunkte in 'a' und 'b'                      */
/*                         ( amax = div_a +1 )                                    */
/* offs_sa1    div_a-div_l[sa1], zur elementerzeugung notwendig                   */
/* offs_sa2    div_a-div_l[sa2]                                                   */
/* x,y,z       f(u,v) aus dem surfmesher, alle u,v positionen sind belegt         */
/*                                                                                */
/*                                                                                */
/**********************************************************************************/
int  fillSurf2( int sur, int *div_l, int div_a, int div_b, int sa1, int sa2, int sb1, int sb2,
 int *n_uv, int *umax, int *vmax, int *n_ba, int *amax, int *bmax, int *offs_sa1, int *offs_sa2,
 double *x, double *y, double *z )
{
  int n,o,m, u,v, b=0,a=0;
  int nodnr;
  double *lx, *ly, *lz; /* line koordinates for linelength() */
  int flag;
  char buffer[MAX_LINE_LENGTH];

  int k;
  double xn, yn, zn;

      *bmax=div_b+1;
      *amax=div_a+1;
      if ((sa1==0)||(sa1==2))
      {
        *umax=*amax    ;
        *vmax=*bmax    ;
      }
      else
      {
        *umax=*bmax    ;
        *vmax=*amax    ;
      }
      edgeNodes( *vmax, *umax, sur, n_uv );

      /* die Randknoten sind nun bekannt. es muessen nun ersatzkoordinaten */
      /* mit dem spacing der ersatzdivissions berechnet werden. dazu wird  */
      /* in einem feld aus koordinate(x,y oder z) und der lauflaenge s int-*/
      /* erpoliert. Ausserdem werden die Randknoten im b,a system          */
      /* gespeichert, dazu ist auch ein offset erforderlich                */

      *offs_sa1=div_a-div_l[sa1];
      *offs_sa2=div_a-div_l[sa2];

      /* side 0 */
      v=0;
      n=div_l[0]+1;        /* anzahl von original-nodes auf der line */
      if (((lx = (double *)malloc( (n)*sizeof(double)) ) == NULL )||
          ((ly = (double *)malloc( (n)*sizeof(double)) ) == NULL )||
          ((lz = (double *)malloc( (n)*sizeof(double)) ) == NULL ))
      { errMsg(" ERROR: realloc failure in fillSurf2()\n");    return(-1); }
      for (u=0; u<n; u++)
      {
        nodnr=n_uv[u* *vmax +v];
        o=0;
        if ((sb1==0)&&(sa1==3)) { b=u                     ; a=0                    ;  o=1 ;}
        if ((sb1==3)&&(sa1==2)) { b=*vmax-1               ; a=u                    ;  o=2 ;}
        if ((sb1==2)&&(sa1==1)) { b=*umax-u-1-*offs_sa2   ; a=*vmax-1              ;  o=3 ;}
        if ((sb1==1)&&(sa1==0)) { b=0                     ; a=*umax-u-1-*offs_sa1  ;  o=4 ;}
        if ((sb1==0)&&(sa1==1)) { b=*umax-u-1             ; a=0                    ;  o=5 ;}
        if ((sb1==3)&&(sa1==0)) { b=0                     ; a=u                    ;  o=6 ;}
        if ((sb1==2)&&(sa1==3)) { b=u+*offs_sa1           ; a=*vmax-1              ;  o=7 ;}
        if ((sb1==1)&&(sa1==2)) { b=*vmax-1               ; a=*umax-u-1-*offs_sa2  ;  o=8 ;}
        n_ba[b* *amax +a]=nodnr;
        lx[u]=npre[nodnr].nx;
        ly[u]=npre[nodnr].ny;
        lz[u]=npre[nodnr].nz;
#if TEST
  printf(" u:%d v:%d umax:%d vmax:%d n:%d x:%lf y:%lf z:%lf \n", u,v,*umax,*vmax, nodnr,
    npre[nodnr].nx,npre[nodnr].ny,npre[nodnr].nz );
  printf(" Fall:%d b:%d a:%d\n", o, b, a);
#endif
      }
      /* nun lx,ly,lz auf die geaenderte division umrechnen */
      u=0;
      flag=0;
      if( newEdgePositions( n, u,v, *umax,*vmax, lx,ly,lz, x,y,z, flag) <0) return(-1);
      free(lx);free(ly);free(lz); 

      /* side 1 */
      u=*umax-1;
      n=div_l[1]+1;
      if (((lx = (double *)malloc( (n)*sizeof(double)) ) == NULL )||
          ((ly = (double *)malloc( (n)*sizeof(double)) ) == NULL )||
          ((lz = (double *)malloc( (n)*sizeof(double)) ) == NULL ))
      { errMsg(" ERROR: realloc failure in fillSurf2()\n");    return(-1); }
      for (v=0; v<n; v++)
      {
        nodnr=n_uv[u**vmax +v];
        o=0;
        if ((sb1==0)&&(sa1==3)) { b=*umax-1              ; a=v                    ;  o=1 ;}
        if ((sb1==3)&&(sa1==2)) { b=*vmax-v-1-*offs_sa2   ; a=*umax-1               ;  o=2 ;}
        if ((sb1==2)&&(sa1==1)) { b=0                   ; a=*vmax-v-1-*offs_sa1    ;  o=3 ;}
        if ((sb1==1)&&(sa1==0)) { b=v                   ; a=0                    ;  o=4 ;}
        if ((sb1==0)&&(sa1==1)) { b=0                   ; a=v                    ;  o=5 ;}
        if ((sb1==3)&&(sa1==0)) { b=v+*offs_sa1          ; a=*umax-1               ;  o=6 ;}
        if ((sb1==2)&&(sa1==3)) { b=*umax-1              ; a=*vmax-v-1-*offs_sa2    ;  o=7 ;}
        if ((sb1==1)&&(sa1==2)) { b=*vmax-v-1            ; a=0                    ;  o=8 ;}
        n_ba[b**amax +a]=nodnr;
        lx[v]=npre[nodnr].nx;
        ly[v]=npre[nodnr].ny;
        lz[v]=npre[nodnr].nz;
      }
      /* nun auf die geaenderte division umrechnen */
      v=0;
      flag=1;
      if( newEdgePositions( n, u,v, *umax,*vmax, lx,ly,lz, x,y,z, flag) <0) return(-1);
      free(lx);free(ly);free(lz); 


      /* side 2 */
      v=*vmax-1;
      n=div_l[2]+1;
      if (((lx = (double *)malloc( (n)*sizeof(double)) ) == NULL )||
          ((ly = (double *)malloc( (n)*sizeof(double)) ) == NULL )||
          ((lz = (double *)malloc( (n)*sizeof(double)) ) == NULL ))
      { errMsg(" ERROR: realloc failure in fillSurf2()\n");    return(-1); }
      m=*umax-n;
      for (u=*umax-1; u>=m; u--)
      {
        nodnr=n_uv[u**vmax +v];
        o=0;
        if ((sb1==0)&&(sa1==3)) { b=u-*offs_sa2          ; a=*vmax-1               ;  o=1 ;}
        if ((sb1==3)&&(sa1==2)) { b=0                   ; a=u-*offs_sa1           ;  o=2 ;}
        if ((sb1==2)&&(sa1==1)) { b=*umax-u-1            ; a=0                    ;  o=3 ;}
        if ((sb1==1)&&(sa1==0)) { b=*vmax-1              ; a=*umax-u-1             ;  o=4 ;}
        if ((sb1==0)&&(sa1==1)) { b=*umax-u-1+*offs_sa1   ; a=*vmax-1               ;  o=5 ;}
        if ((sb1==3)&&(sa1==0)) { b=*vmax-1              ; a=u-*offs_sa2           ;  o=6 ;}
        if ((sb1==2)&&(sa1==3)) { b=u                   ; a=0                    ;  o=7 ;}
        if ((sb1==1)&&(sa1==2)) { b=0                   ; a=*umax-u-1             ;  o=8 ;}
        n_ba[b**amax +a]=nodnr;
        lx[u-m]=npre[nodnr].nx;
        ly[u-m]=npre[nodnr].ny;
        lz[u-m]=npre[nodnr].nz;
      }
      /* nun auf die geaenderte division umrechnen */
      u=0;
      flag=0;
      if( newEdgePositions( n, u,v, *umax,*vmax, lx,ly,lz, x,y,z, flag) <0) return(-1);
      free(lx);free(ly);free(lz); 


      /* side 3 */
      u=0;
      n=div_l[3]+1;
      if (((lx = (double *)malloc( (n)*sizeof(double)) ) == NULL )||
          ((ly = (double *)malloc( (n)*sizeof(double)) ) == NULL )||
          ((lz = (double *)malloc( (n)*sizeof(double)) ) == NULL ))
      { errMsg(" ERROR: realloc failure in fillSurf2()\n");    return(-1); }
      m=*vmax-n;
      for (v=*vmax-1; v>=m; v--)
      {
        nodnr=n_uv[u**vmax +v];
        o=0;
        if ((sb1==0)&&(sa1==3)) { b=0                   ; a=v-*offs_sa1           ;  o=1 ;}
        if ((sb1==3)&&(sa1==2)) { b=*vmax-v-1            ; a=0                    ;  o=2 ;}
        if ((sb1==2)&&(sa1==1)) { b=*umax-1              ; a=*vmax-v-1             ;  o=3 ;}
        if ((sb1==1)&&(sa1==0)) { b=v-*offs_sa2          ; a=*umax-1               ;  o=4 ;}
        if ((sb1==0)&&(sa1==1)) { b=*umax-1              ; a=v-*offs_sa2           ;  o=5 ;}
        if ((sb1==3)&&(sa1==0)) { b=v                   ; a=0                    ;  o=6 ;}
        if ((sb1==2)&&(sa1==3)) { b=0                   ; a=*vmax-v-1             ;  o=7 ;}
        if ((sb1==1)&&(sa1==2)) { b=*vmax-v-1+*offs_sa1   ; a=*umax-1               ;  o=8 ;}
        n_ba[b**amax +a]=nodnr;
        lx[v-m]=npre[nodnr].nx;
        ly[v-m]=npre[nodnr].ny;
        lz[v-m]=npre[nodnr].nz;
      }
      /* nun auf die geaenderte division umrechnen */
      v=0;
      flag=1;
      if( newEdgePositions( n, u,v, *umax,*vmax, lx,ly,lz, x,y,z, flag) <0) return(-1);
      free(lx);free(ly);free(lz); 


      /* auffuellen der surface mit temporaeren-nodes */
      /* die vernetzung (nodes, elem generierung) erfolgt  */
      /* im ba system. Jedoch werden sie zur verwendung in */
      /* meshBodies() im uv system in der surf abgelegt     */
      k=0;
      surfMesh( vmax, umax, x, y, z);
      for (u=1; u< *umax-1; u++)
      {
        for (v=1; v< *vmax-1; v++)
        {
          xn=x[u* *vmax +v];
          yn=y[u* *vmax +v];
          zn=z[u* *vmax +v];
  //NOTHREAD if(sem_wait(&sem[10])) error("sem_wait");
          nod(  apre, &npre, 0, apre->nmax+1, xn, yn, zn, 0 );

          /* apre->nmax wird in nod um 1 erhoeht!  */
          n_uv[u* *vmax +v]=apre->nmax;
  //NOTHREAD if(sem_post(&sem[10])) error("sem_post");

          /* umspeichern der temp-nodes ins ba system */
          o=0;
          if ((sb1==0)&&(sa1==3)) { b=u                   ; a=v                    ;  o=1 ;}
          if ((sb1==3)&&(sa1==2)) { b=*vmax-v-1           ; a=u                    ;  o=2 ;}
          if ((sb1==2)&&(sa1==1)) { b=*umax-u-1           ; a=*vmax-v-1            ;  o=3 ;}
          if ((sb1==1)&&(sa1==0)) { b=v                   ; a=*umax-u-1            ;  o=4 ;}
          if ((sb1==0)&&(sa1==1)) { b=*umax-u-1           ; a=v                    ;  o=5 ;}
          if ((sb1==3)&&(sa1==0)) { b=v                   ; a=u                    ;  o=6 ;}
          if ((sb1==2)&&(sa1==3)) { b=u                   ; a=*vmax-v-1            ;  o=7 ;}
          if ((sb1==1)&&(sa1==2)) { b=*vmax-v-1           ; a=*umax-u-1            ;  o=8 ;}
          n_ba[b* *amax +a]=apre->nmax;
#if TEST
  printf(" Fall:%d b:%d a:%d\n", o, b, a);
#endif
          surf[sur].nod[k]=apre->nmax;
          sprintf( buffer,"%d ", surf[sur].nod[k] );
          k++;
        }
      }
      surf[sur].nn=k;

      /* randbereiche umspeichern (nodenr der diagonale den raendern der undefinierten luecken zuordnen) */
      a=div_l[sa1];
      for (b=1; b<*offs_sa1; b++)
      {
        n_ba[b* *amax +a]=n_ba[b* *amax +(div_a-b)];
#if TEST
  printf("1 node:%d b:%d a:%d (div_a-b):%d\n", n_ba[b* *amax +a], b, a,(div_a-b));
#endif
      }
      o=0;
      b=*offs_sa1;
      for (a=div_l[sa1]+1; a<div_a; a++)
      {
        o++;
        n_ba[b* *amax +a]=n_ba[(b-o)* *amax +a];
#if TEST
  printf("2 node:%d b:%d a:%d\n", n_ba[b* *amax +a], b, a);
#endif
      }
      o=0;
      a=div_l[sa2];
      for (b=div_b-1; b>div_b - *offs_sa2; b--)
      {
        o++;
        n_ba[b* *amax +a]=n_ba[b* *amax +(div_a-o)];
#if TEST
  printf("3 node:%d b:%d a:%d\n", n_ba[b* *amax +a], b, a);
#endif
     }
      o=0;
      b=div_b- *offs_sa2;
      for (a=div_l[sa2]+1; a<div_a; a++)
      {
        o++;
        n_ba[b* *amax +a]=n_ba[(b+o)* *amax +a];
#if TEST
  printf("4 node:%d b:%d a:%d\n", n_ba[b* *amax +a], b, a);
#endif
      }
  return(1);
}



/**********************************************************************************/
/* Fuellt ein xyz-feld mit den koordinaten einer Flaeche bei der gegenueber       */
/* liegende seiten gleiche divisions haben.                                       */
/* Die xyz-Koordinaten gelten fuer ein Feld im uv-Raum bei dem die u-achse mit der*/
/* surf[].l[0] zusammenfaellt und die v-achse mit der surf[].l[3].                */
/*                                                                                */
/*                                                                                */
/*                                                                                */
/* in:                                                                            */
/* j           surface-index                                                      */
/* umax, vmax  anzahl der ersatz-stuetzpunkte in 'u' und 'v' bzw entlang edge     */
/*             '0' und '3'                                                        */
/*                                                                                */
/* out:                                                                           */
/* n_uv        nodeNr im uv-feld                                                  */
/* x,y,z       f(u,v) aus dem surfmesher, alle u,v positionen sind belegt         */
/*                                                                                */
/*                                                                                */
/**********************************************************************************/
int  fillSurf( int j, int *n_uv, int umax, int vmax, double *x, double *y, double *z )
{
  int u, v, nodnr, k;
  double xn, yn, zn;
  char buffer[MAX_LINE_LENGTH];

      edgeNodes( vmax, umax, j, n_uv );
      /* auffuellen der Surface-randfelder */
      for (u=0; u<umax; u++)
      {
        for (v=0; v<vmax; v++)
        {
          nodnr=n_uv[u*vmax +v];
          if (nodnr>-1)
          {
            x[u*vmax +v]=npre[nodnr].nx;
            y[u*vmax +v]=npre[nodnr].ny;
            z[u*vmax +v]=npre[nodnr].nz;
#if TEST3
 printf(" u:%d v:%d umax:%d vmax:%d n:%d x:%lf y:%lf z:%lf x:%lf y:%lf z:%lf\n", u,v,umax,vmax, nodnr,
   x[u*vmax +v], y[u*vmax +v],z[u*vmax +v],npre[nodnr].nx,npre[nodnr].ny,npre[nodnr].nz );
#endif	  
          }	  
        }
      }

      /* auffuellen der surface mit nodes */
      k=0;
      surfMesh( &vmax, &umax, x, y, z);
      for (u=1; u<umax-1; u++)
      {
        for (v=1; v<vmax-1; v++)
        {
          xn=x[u*vmax +v];
          yn=y[u*vmax +v];
          zn=z[u*vmax +v];
    //NOTHREAD if(sem_wait(&sem[10])) error("sem_wait");
#if TEST4
          printf("hallo nod:%d %x\n", apre->nmax+1, npre);
#endif	  
          nodnr=nod( apre, &npre, 0, apre->nmax+1, xn, yn, zn, 0 );
          /* apre->nmax wird in nod um 1 erhoeht!  */
#if TEST4
	  printf("hallo nod end:%d %x\n", npre[nodnr].nr, npre);
#endif	  
   //NOTHREAD if(sem_post(&sem[10])) error("sem_post");
          n_uv[u*vmax +v]=npre[nodnr].nr;
          surf[j].nod[k]=npre[nodnr].nr;

          sprintf( buffer,"%d ", surf[j].nod[k] );
          k++;
        }
      }
      surf[j].nn=k;
  return(1);
}

void *meshSurf( void *arg)
{
  int    ii,jj,k,l,n,m, s=0, ke,u,v, b=0,a, amax,bmax;
  int *div_l=NULL;                 /* aufsummierte div der surf-edges */
  int sum_div, umax, vmax, imax, jmax;
  int   *n_uv=NULL, mapflag;
  double *x=NULL, *y=NULL, *z=NULL;

  int   div_a,div_b, sa1,sa2,sb1,sb2;
  int   *n_ba=NULL, offs_sa1, offs_sa2;

  /* variables for the mesh-improver */
  int   *n_indx=NULL, *n_ori=NULL, **e_nod=NULL, *n_type=NULL, **n_edge=NULL, **s_div=NULL;
  double **n_coord=NULL; 

  int  lnew[2], ps, cl[2];
  char typnew[2];
  int edge[MAX_EDGES_PER_SURF];                 /* lines/lcmb for the meshable substitute surface with 4 edges */
  char  ctyp[MAX_EDGES_PER_SURF];      /*   type: l=line c=lcmb */
  char  cori[MAX_EDGES_PER_SURF];      /*   l-orient +- */
  char name[MAX_LINE_LENGTH];
  char buffer[MAX_LINE_LENGTH];

  char surFlag;

  GLdouble *fptr;
  double v1[3], v2[3], vn[3];

  int transitionflag, ipuf=0, nori[2], eori[2], ebuf[26];

  Gsur surfbuf[1];

  int setTmp=-1, Stmp=-1, sh_buf=-1, jbuf;

  int anz_nmax;
  int j,anz_s,meshflag;
  Thread_param *thread_param;
  thread_param=(Thread_param *)arg;

  j=thread_param->surf;
  anz_s=thread_param->anz_s;
  meshflag=thread_param->meshflag;

  //NOTHREAD if(sem_wait(&sem[0])) error("sem_wait");
  countOpenThreads++; 
#if TEST2
  printf("hallo start s:%d countOpenThreads:%d\n", j, countOpenThreads);
#endif
  //NOTHREAD if(sem_post(&sem[0])) error("sem_post");

  jbuf=j;

    /* if the interiour of the surf should be filled (no mesh), save the mesh */
    if (fillSurfFlag)
    {
      meshflag=1;

      if(surf[j].ne>0)
      {
        if((surfbuf[0].elem=(int *)malloc((surf[j].ne)*sizeof(int)) )==NULL)
	  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed, surf->elem (5)\n\n", surf[j].name); thread_param->returnVal=-1; return(&thread_param->returnVal); }
        for(k=0; k<surf[j].ne; k++) surfbuf[0].elem[k]=surf[j].elem[k];
        surfbuf[0].ne=surf[j].ne;
      }
      else surfbuf[0].ne=0;
      if(surf[j].nn>0)
      {
        if((surfbuf[0].nod=(int *)malloc((surf[j].nn)*sizeof(int)) )==NULL)
        { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed, surf->nod (2)\n\n", surf[s].name); thread_param->returnVal=-1; return(&thread_param->returnVal); }
        for(k=0; k<surf[j].nn; k++) surfbuf[0].nod[k]=surf[j].nod[k];
        surfbuf[0].nn=surf[j].nn;
      }
      else surfbuf[0].nn=0;
    }

    if(((surf[j].etyp==7)||(surf[j].etyp==8))&&(surf[j].sh<0)&&(surf[j].eattr==-1))
    {
      /* if shape=BLEND and the surf is plane, generate shape */
  //NOTHREAD if(sem_wait(&sem[1])) error("sem_wait");
      if(surfToShape(j)>-1) { if(printFlag) printf (" define shape for surf:%s\n", surf[j].name); }
  //NOTHREAD if(sem_post(&sem[1])) error("sem_post");
    }

    if(((surf[j].etyp==7)||(surf[j].etyp==8))&&(surf[j].sh!=-1)&&(surf[j].eattr==-1))
    {
      if(printFlag)
        printf (" unstructured meshing of surf:%s type:%d\n", surf[j].name, shape[surf[j].sh].type);

      /* if shape, generate prelim nurbs */
      sh_buf=-1;
  //NOTHREAD if(sem_wait(&sem[2])) error("sem_wait");
      if(shape[surf[j].sh].type>-1) Stmp= surfToNurs(j);
  //NOTHREAD if(sem_post(&sem[2])) error("sem_post");
    
      if(Stmp>-1)
      {
        sprintf(buffer,"-Stmp%d", j);
  //NOTHREAD if(sem_wait(&sem[3])) error("sem_wait");
        setTmp=pre_seta( buffer, "S", nurbs[Stmp].name );
	if(setTmp<0)
	{
          printf(" ERROR: in meshSurf, temporary nurbs could not be added to set %s\n", buffer);
          return(-2);
	}
        completeSet( buffer, "do" );
  //NOTHREAD if(sem_post(&sem[3])) error("sem_post");
    
        sh_buf=surf[j].sh;
        surf[j].sh=shape_i( nurbs[Stmp].name, 4, Stmp, 0, 0, 0, 0, 0, 0);

        /* delete the interiour of the surface */
	/*
        if(surf[j].pgn!=NULL)
        {
          free(surf[j].pgn); surf[j].pgn=NULL; surf[j].npgn=0;
        }
	*/
        if(printFlag) printf (" interior changed to Nurbs: %s\n", nurbs[Stmp].name );
      }

      /* incl. semaphores there */
      ps=mesh_tr3u(j);

#if TEST2
  for (n=0; n<11; n++)
  {
    //NOTHREAD if(sem_getvalue(&sem[n], &k) < 0) printf("sem_init failed\n");
    printf("hallo sem:%d %d\n", n,k);
  }
#endif
      /* restore the pointer to the shape */
      //seta(0,"S",Stmp ); // activate if nurs should be kept
      if(sh_buf>-1)
	//if(sh_buf==9999) // activate if nurs should be kept
      {
        surf[j].sh=sh_buf;
  //NOTHREAD if(sem_wait(&sem[4])) error("sem_wait");
	
        delNurs( set[setTmp].anz_nurs, set[setTmp].nurs ); 
        delPnt( set[setTmp].anz_p, set[setTmp].pnt ); 
        delSet(buffer);
  //NOTHREAD if(sem_post(&sem[4])) error("sem_post");
      }

      if (ps==-1) { thread_param->returnVal=-2; return(&thread_param->returnVal); } 
      if ((nbuf = (int **)realloc((int **)nbuf, (apre->nmax+1)*sizeof(int *)) ) == NULL )
      { printf(" ERROR: realloc failure in meshSurf, nodes not installed\n\n"); thread_param->returnVal=-1; return(&thread_param->returnVal); }
      for (v=sum_nbuf; v<=apre->nmax; v++)
      {
        if ((nbuf[v] = (int *)malloc( (2)*sizeof(int)) ) == NULL )
        { printf(" ERROR: realloc failure in meshSurf, nodes not installed\n\n"); thread_param->returnVal=-1; return(&thread_param->returnVal); }
        nbuf[v][0]=0;
      }
      sum_nbuf=apre->nmax+1;
      goto checkSurf;
    }

    if((surf[j].etyp==7)&&(surf[j].sh>-1)&&(surf[j].eattr==-2))
    {
      if(printFlag) printf (" GLU based meshing of surf:%s\n", surf[j].name);

      /* incl. semaphores there */
      ps=mesh_tr3g(j);

      if (ps==-1) { thread_param->returnVal=-2; return(&thread_param->returnVal); }
      if ((nbuf = (int **)realloc((int **)nbuf, (apre->nmax+1)*sizeof(int *)) ) == NULL )
      { printf(" ERROR: realloc failure in meshSurf, nodes not installed\n\n"); thread_param->returnVal=-1; return(&thread_param->returnVal); }
      for (v=sum_nbuf; v<=apre->nmax; v++)
      {
        if ((nbuf[v] = (int *)malloc( (2)*sizeof(int)) ) == NULL )
        { printf(" ERROR: realloc failure in meshSurf, nodes not installed\n\n"); thread_param->returnVal=-1; return(&thread_param->returnVal); }
        nbuf[v][0]=0;
      }
      sum_nbuf=apre->nmax+1;
      goto checkSurf;
    }
    else if (((surf[j].etyp==7)||
        (surf[j].etyp==8)||
        (surf[j].etyp==9)||
	 (surf[j].etyp==10)||(meshflag))&&(surf[j].nc==1))
    {
      surf[j].fail=1;
      if(printFlag) printf (" meshing surf:%s\n", surf[j].name);
    }
    else
      goto nextSurf;


#if TEST2
  printf("hallo mesh:%d\n",jbuf);
#endif

    if (surf[j].nl!=MAX_EDGES_PER_SURF)
    {
      mapflag=1;
      if (surf[j].nl==2)
      {
  //NOTHREAD if(sem_wait(&sem[5])) error("sem_wait");
        ps=splitLineAtDivratio( surf[j].l[0], surf[j].typ[0], 0.5, edge, ctyp);
        if (ps==-1) { thread_param->returnVal=-2; return(&thread_param->returnVal); }
        if(surf[j].o[0]=='-')
        {
          n=edge[0];
          m=ctyp[0];
          edge[0]=edge[1];
          ctyp[0]=ctyp[1];
          edge[1]=n;
          ctyp[1]=m;
        }
        ps=splitLineAtDivratio( surf[j].l[1], surf[j].typ[1], 0.5, &edge[2], &ctyp[2]);
  //NOTHREAD if(sem_post(&sem[5])) error("sem_post");
        if (ps==-1) { thread_param->returnVal=-2; return(&thread_param->returnVal); }

        /* create a 4 sided surf */
        for (jj=0; jj<4; jj+=2) { cori[jj]=cori[jj+1]=surf[j].o[jj]; }
#if TEST2
        for (jj=0; jj<MAX_EDGES_PER_SURF; jj++)
        {
          if(ctyp[jj]=='l') printf("edge:%s cori:%c ctyp:%c\n", line[edge[jj]].name, cori[jj], ctyp[jj]);
          else if(ctyp[jj]=='c') printf("edge:%s cori:%c ctyp:%c\n", lcmb[edge[jj]].name, cori[jj], ctyp[jj]);
          else printf("error:%c\n",ctyp[jj]);
	}
#endif
  //NOTHREAD if(sem_wait(&sem[6])) error("sem_wait");
        getNewName( name, "s" );
        s=surface_i( name, surf[j].ori, surf[j].sh, (int)4, cori, edge, ctyp );
  //NOTHREAD if(sem_post(&sem[6])) error("sem_post");
        if( s<0)
        { printf("ERROR: surface could not be created\n"); thread_param->returnVal=-2; return(&thread_param->returnVal); }
  //NOTHREAD if(sem_wait(&sem[3])) error("sem_wait");
        pre_seta(specialset->zap, "s", name);
  //NOTHREAD if(sem_post(&sem[3])) error("sem_post");
/*
  //NOTHREAD if(sem_wait(&sem[7])) error("sem_wait");
        setr( 0, "s",s );        
  //NOTHREAD if(sem_post(&sem[7])) error("sem_post");
*/
        surf[s].etyp=surf[j].etyp;
        surf[s].eattr=surf[j].eattr;
      
        if(printFlag) printf("surf[%d]:%s is replaced by surf[%d]:%s\n",j, surf[j].name,s, surf[s].name);
        j=s;   
      }
      else if (surf[j].nl==3)
      {
        /* choose an edge with has no rest for the equation division/4 in case elements have quadratic formulation */
        for (ii=0; ii<3; ii++)
        {
          if(( surf[j].typ[ii]=='l')&&(!(line[surf[j].l[ii]].div%4))) break;
          else if( line[surf[j].l[ii]].typ=='c')
          {
            sum_div=0;
            for(jj=0; jj<lcmb[surf[j].l[ii]].nl; jj++) sum_div+=line[lcmb[surf[j].l[ii]].l[jj]].div;
            if (!(sum_div%4)) break;
	  }
        }
        /* no mesh if no suitable edge exists */
        if(ii==3) ii=2;
  //NOTHREAD if(sem_wait(&sem[5])) error("sem_wait");
        ps=splitLineAtDivratio( surf[j].l[ii], surf[j].typ[ii], 0.5, lnew, typnew);
  //NOTHREAD if(sem_post(&sem[5])) error("sem_post");
        if (ps==-1) {  thread_param->returnVal=-2; return(&thread_param->returnVal); }

        /* create a 4 sided surf */
        for (jj=0; jj<ii; jj++)
        {
          edge[jj]=surf[j].l[jj];
          cori[jj]=surf[j].o[jj];
          ctyp[jj]=surf[j].typ[jj];
        }
        if(surf[j].o[ii]=='+') { n=0; m=1; } else { n=1; m=0; }
        edge[jj]=lnew[n];
        cori[jj]=surf[j].o[ii];
        ctyp[jj]=typnew[n];
        jj++;
        edge[jj]=lnew[m];
        cori[jj]=surf[j].o[ii];
        ctyp[jj]=typnew[m];
        for (; jj<3; jj++) /* thats ok so */
        {
          edge[jj+1]=surf[j].l[jj];
          cori[jj+1]=surf[j].o[jj];
          ctyp[jj+1]=surf[j].typ[jj];
        }

#if TEST2
        for (jj=0; jj<MAX_EDGES_PER_SURF; jj++)
        {
          if(ctyp[jj]=='l') printf("edge:%s cori:%c ctyp:%c\n", line[edge[jj]].name, cori[jj], ctyp[jj]);
          else if(ctyp[jj]=='c') printf("edge:%s cori:%c ctyp:%c\n", lcmb[edge[jj]].name, cori[jj], ctyp[jj]);
          else printf("error:%c\n",ctyp[jj]);
	}
#endif
  //NOTHREAD if(sem_wait(&sem[6])) error("sem_wait");
        getNewName( name, "s" );
        s=surface_i( name, surf[j].ori, surf[j].sh, (int)4, cori, edge, ctyp );
  //NOTHREAD if(sem_post(&sem[6])) error("sem_post");
        if( s<0)
        { printf("ERROR: surface could not be created\n"); thread_param->returnVal=-2; return(&thread_param->returnVal); }
  //NOTHREAD if(sem_wait(&sem[3])) error("sem_wait");
        pre_seta(specialset->zap, "s", name);
  //NOTHREAD if(sem_post(&sem[3])) error("sem_post");
/*
  //NOTHREAD if(sem_wait(&sem[7])) error("sem_wait");
        setr( 0, "s",s );        
  //NOTHREAD if(sem_post(&sem[7])) error("sem_post");
*/
        surf[s].etyp=surf[j].etyp;
        surf[s].eattr=surf[j].eattr;

        /* allocate memory for the reference to the substitute-surface (s)  */
        if((surf[j].l=(int *)realloc((int *)surf[j].l, (surf[j].nl+7)*sizeof(int)) )==NULL)
        { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed, surf->l\n\n", surf[j].name); thread_param->returnVal=-1; return(&thread_param->returnVal); }
        surf[j].l[surf[j].nl]=ii;
        surf[j].l[surf[j].nl+1]=s;
        surf[j].l[surf[j].nl+2]=ps;
        surf[j].l[surf[j].nl+3]=lnew[0];
        surf[j].l[surf[j].nl+4]=typnew[0];
        surf[j].l[surf[j].nl+5]=lnew[1];
        surf[j].l[surf[j].nl+6]=typnew[1];
      
        if(printFlag) printf("surf[%d]:%s is replaced by surf[%d]:%s\n",j, surf[j].name,s, surf[s].name);
        j=s;   
      }

      /* to mesh a n-sided surf two corners will be combined to one lcmb until only 4 edges remain */
      else if (surf[j].nl>4)
      {
        /* create a temporary surface for the meshing */
  //NOTHREAD if(sem_wait(&sem[6])) error("sem_wait");
        getNewName( name, "s" );
        s=surface_i( name, surf[j].ori, surf[j].sh, surf[j].c[0], surf[j].o, surf[j].l, surf[j].typ );
  //NOTHREAD if(sem_post(&sem[6])) error("sem_post");
        if( s<0)
        { printf("ERROR: surface could not be created\n"); thread_param->returnVal=-2; return(&thread_param->returnVal); }
  //NOTHREAD if(sem_wait(&sem[3])) error("sem_wait");
        pre_seta(specialset->zap, "s", name);
  //NOTHREAD if(sem_post(&sem[3])) error("sem_post");
/*
  //NOTHREAD if(sem_wait(&sem[7])) error("sem_wait");
        setr( 0, "s",s );        
  //NOTHREAD if(sem_post(&sem[7])) error("sem_post");
*/
        surf[s].etyp=surf[j].etyp;
        surf[s].eattr=surf[j].eattr;

        for (ii=0; ii<surf[j].nl-MAX_EDGES_PER_SURF; ii++)
        {
          /* determine the best suited corner for the lcmb */
          determineBestCorners( s,cl);
#if TEST2
	  printf("edges:%d\n", surf[s].nl);
          for (jj=0; jj<2; jj++)
	  {          
	    if(surf[s].typ[cl[jj]]=='l') printf("add:%d %s %c\n",cl[jj], line[surf[s].l[cl[jj]]].name, surf[s].o[cl[jj]]);
            else  printf("add:%d %s %c\n",cl[jj], lcmb[surf[s].l[cl[jj]]].name, surf[s].o[cl[jj]]);
	  } 
#endif
          /* create a lcmb out of 2 edges */
  //NOTHREAD if(sem_wait(&sem[11])) error("sem_wait");
          lnew[0]=addTwoLines( surf[s].l[cl[0]], surf[s].o[cl[0]], surf[s].typ[cl[0]], surf[s].l[cl[1]] ,surf[s].o[cl[1]], surf[s].typ[cl[1]] );
  //NOTHREAD if(sem_post(&sem[11])) error("sem_post");
          if( lnew[0]==-1) { thread_param->returnVal=-2; return(&thread_param->returnVal); }

          /* replace the concatonated lines of the surface */
          if (cl[0]==surf[s].nl-1)
          {
            surf[s].l[0]=lnew[0];
            surf[s].o[0]='+';
            surf[s].typ[0]='c';
            surf[s].nl--;
          }
          else
          {
            surf[s].l[cl[0]]=lnew[0];
            surf[s].o[cl[0]]='+';
            surf[s].typ[cl[0]]='c';
            for (jj=cl[0]+1; jj<surf[s].nl-1; jj++)
            {
              surf[s].l[jj]=surf[s].l[jj+1];
              surf[s].o[jj]=surf[s].o[jj+1];
              surf[s].typ[jj]=surf[s].typ[jj+1];
            }
            surf[s].nl--;
          }
	}
        if(printFlag) printf("surf[%d]:%s is replaced by surf[%d]:%s\n",j, surf[j].name,s, surf[s].name);
#if TEST2
        for (jj=0; jj<MAX_EDGES_PER_SURF; jj++)
        {
          if(surf[s].typ[jj]=='l') printf("edge:%s cori:%c ctyp:%c\n", line[surf[s].l[jj]].name, surf[s].o[jj], surf[s].typ[jj]);
          else if(surf[s].typ[jj]=='c') printf("edge:%s cori:%c ctyp:%c\n", lcmb[surf[s].l[jj]].name, surf[s].o[jj], surf[s].typ[jj]);
          else printf("error:%c\n",surf[s].typ[jj]);
	}
#endif
        if (surf[j].nl==5)
        {
          /* allocate memory for the reference to the substitute-surface (s)  */
          if((surf[j].l=(int *)realloc((int *)surf[j].l, (surf[j].nl+4)*sizeof(int)) )==NULL)
          { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed, surf->l (2)\n\n", surf[j].name); thread_param->returnVal=-1; return(&thread_param->returnVal); }
          surf[j].l[surf[j].nl]=cl[0];    /* place of the line in the surf */
          surf[j].l[surf[j].nl+1]=cl[1];  /* place of the line in the surf */
          surf[j].l[surf[j].nl+2]=s;  /* slave surf */
          surf[j].l[surf[j].nl+3]=lnew[0]; /* slave lcmb */
	}
        j=s;   
      }
      else
      {
        printf(" ERROR: in meshSurfs Surf:%s has no valid number of edges (has:%d)\n", surf[j].name, surf[j].nl );
        thread_param->returnVal=-2; return(&thread_param->returnVal);
      }
    }
    else  mapflag=0;
       
#if TEST2
  printf("hallo1:%d\n",jbuf);
#endif


    /* determine the amount of nodes in the surf without the nodes of the borderlines and points */
    /* determine the div of each edge */
    /* and check if all lines of that surf are meshed (canceled:not nessesary up to now! )*/
    if( (div_l=(int *)realloc((int *)div_l, (surf[j].nl+1)*sizeof(int) ) )==NULL) 
    { printf(" ERROR: realloc failure in meshSurfs()\n"); thread_param->returnVal=-1; return(&thread_param->returnVal); }

    for (n=0; n<surf[j].nl; n++)
    {
      k=surf[j].l[n];
      div_l[n]=0;
      if( surf[j].typ[n]=='c' )
      {
        for( l=0; l<lcmb[k].nl; l++ )
        {
          m=lcmb[k].l[l];
          div_l[n]+=line[m].div;
        }
      }
      else
        div_l[n]=line[k].div;
    }

    /* look if the divisions are suited for the elemtype  */
    /*  sum_div%2=0 for linear elements */
    sum_div=0;
    if ((surf[j].etyp==7)||(surf[j].etyp==9))
    {
      for (n=0; n<4; n++) sum_div+=div_l[n];
      if((sum_div&(int)1))
      {
        printf (" WARNING: bad divisions in surf:%s\n", surf[j].name);
        printf ("  sum_div:%d sum_div&(int)1:%d \n", sum_div, (sum_div&(int)1));
        thread_param->returnVal=-2; return(&thread_param->returnVal);
      } 
    }
    /* check each div%2=0 for quadratic elems, and check if the sum_div%4=0 */
    if ((surf[j].etyp==8)||(surf[j].etyp==10))
    { 
      for (n=0; n<4; n++)
      {
        if((div_l[n]&(int)1))
        {
          printf (" WARNING: bad divisions in surf:%s\n", surf[j].name);
          thread_param->returnVal=-2; return(&thread_param->returnVal);
        } 
        sum_div+=div_l[n];
      } 
      if( ((sum_div&(int)1))||((sum_div&(int)2)) )
      {
        printf (" WARNING: bad divisions in surf:%s\n", surf[j].name);
        thread_param->returnVal=-2; return(&thread_param->returnVal);
      } 
    }


#if TEST2
  printf("hallo check if the surf has balanced edges:%d\n",jbuf);
#endif
    /* check if the surf has balanced edges */
    if ((div_l[0]!=div_l[2])||(div_l[1]!=div_l[3]))
    {
      transitionflag=1;

#if TEST2
  printf("hallo unbalanced:%d\n",jbuf);
#endif
      /* surf has unbalanced edges, check if it is meshable */
      /* and calculate the nessesary divisions of the surf    */

      n=newDivisions( div_l, &div_a, &div_b, &sa1, &sa2, &sb1, &sb2 );
      if (n==-1)
      {
        printf("ERROR: surf:%s has bad divisions\n\n",surf[j].name);
        thread_param->returnVal=-2; return(&thread_param->returnVal);
      }

      /* Fuelle ein xyz-feld mit den koordinaten einer Flaeche mit ungleichen divisions. */
      /* Die xyz-Koordinaten gelten fuer ein Feld im uv-Raum bei dem die u-achse mit der*/
      /* surf[j].l[0] zusammenfaellt und die v-achse mit der surf[j].l[3].              */
      /* ausserdem werden die     knotenNr im feld n_ba und n_uv abgelegt               */

      if( (n_uv=(int *)malloc( (div_a+1)*(div_b+1)*sizeof(int) ) )==NULL)
      { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
        thread_param->returnVal=-1; return(&thread_param->returnVal); }
      if( (n_ba=(int *)malloc( (div_a+1)*(div_b+1)*sizeof(int) ) )==NULL)
      { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
        thread_param->returnVal=-1; return(&thread_param->returnVal); }
      if( (x=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
      { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
        thread_param->returnVal=-1; return(&thread_param->returnVal); }
      if( (y=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
      { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
        thread_param->returnVal=-1; return(&thread_param->returnVal); }
      if( (z=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
      { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
        thread_param->returnVal=-1; return(&thread_param->returnVal); }

      /* allocate memory for embeded nodes */
      if((surf[j].nod=(int *)realloc((int *)surf[j].nod, ((div_a)*(div_b))*sizeof(int)) )==NULL)
      { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed, surf->nod\n\n", surf[j].name);
        thread_param->returnVal=-1; return(&thread_param->returnVal); }

#if TEST2
  printf("hallo1 unbalanced:%d\n",jbuf);
#endif
      /* incl. semaphores there */
      if( fillSurf2( j, div_l, div_a, div_b, sa1, sa2, sb1, sb2, n_uv, &umax, &vmax, n_ba, &amax, &bmax, &offs_sa1, &offs_sa2, x,y,z)<0) { thread_param->returnVal=-2; return(&thread_param->returnVal); }

      surf[j].fail=0;


      /* START of section mesh-improver, allocate memory for the mesh-improver */
      if ((surf[j].etyp>=7)&&(surf[j].etyp<=10))
      {
#if TEST2
  printf("hallo2 unbalanced:%d\n",jbuf);
#endif
        if((n_indx=(int *)realloc((int *)n_indx, (apre->nmax+1)*sizeof(int)) )==NULL)
        { printf(" ERROR: n_indx realloc failure in meshSurfs surf:%s can not be meshed\n\n"
          , surf[j].name); thread_param->returnVal=-1; return(&thread_param->returnVal); }
        if((n_ori=(int *)realloc((int *)n_ori, ((div_a+1)*(div_b+1))*sizeof(int)) )==NULL)
        { printf(" ERROR: n_ori realloc failure in meshSurfs surf:%s can not be meshed\n\n"
          , surf[j].name); thread_param->returnVal=-1; return(&thread_param->returnVal); }
        if((e_nod=(int **)realloc((int **)e_nod, ((div_a+1)*(div_b+1))*sizeof(int *)) )==NULL)
        { printf(" ERROR: e_nod realloc failure in meshSurfs surf:%s can not be meshed\n\n"
          , surf[j].name); thread_param->returnVal=-1; return(&thread_param->returnVal); }
        for (ii=0; ii<((div_a+1)*(div_b+1)); ii++)
          if((e_nod[ii]=(int *)malloc( (int)20*sizeof(int)) )==NULL)
          { printf(" ERROR: e_nod[%d] malloc failure in meshSurfs surf:%s can not be meshed\n\n"
           , ii, surf[j].name); thread_param->returnVal=-1; return(&thread_param->returnVal); }
        if((n_edge=(int **)realloc((int **)n_edge, (int)MAX_SURFS_PER_BODY*sizeof(int *)) )==NULL)
        { printf(" ERROR: n_edge realloc failure in meshSurfs surf:%s can not be meshed\n\n"
          , surf[j].name); thread_param->returnVal=-1; return(&thread_param->returnVal); }
        for (ii=0; ii<MAX_SURFS_PER_BODY; ii++)
          if((n_edge[ii]=(int *)malloc( (int)((div_a+1)*(div_b+1))*sizeof(int)) )==NULL)
          { printf(" ERROR: n_edge[%d] malloc failure in meshSurfs surf:%s can not be meshed\n\n"
            , ii, surf[j].name); thread_param->returnVal=-1; return(&thread_param->returnVal); }
        if((s_div=(int **)realloc((int **)s_div, (int)MAX_SURFS_PER_BODY*sizeof(int *)) )==NULL)
        { printf(" ERROR: s_div realloc failure in meshSurfs surf:%s can not be meshed\n\n"
          , surf[j].name); thread_param->returnVal=-1; return(&thread_param->returnVal); }
        for (ii=0; ii<MAX_SURFS_PER_BODY; ii++)
          if((s_div[ii]=(int *)malloc( (int)MAX_EDGES_PER_SURF*sizeof(int)) )==NULL)
          { printf(" ERROR: s_div[%d] malloc failure in meshSurfs surf:%s can not be meshed\n\n"
           , ii, surf[j].name); thread_param->returnVal=-1; return(&thread_param->returnVal); }
        if((n_type=(int *)realloc((int *)n_type, ((div_a+1)*(div_b+1)+1)*sizeof(int)) )==NULL)
        { printf(" ERROR: n_type realloc failure in meshSurfs surf:%s can not be meshed\n\n"
          , surf[j].name); thread_param->returnVal=-1; return(&thread_param->returnVal); }
        if((n_coord=(double **)realloc((double **)n_coord, (int)((div_a+1)*(div_b+1))*sizeof(double *)) )==NULL)
        { printf(" ERROR: n_coord realloc failure in meshSurfs surf:%s can not be meshed\n\n"
          , surf[j].name); thread_param->returnVal=-1; return(&thread_param->returnVal); }
        for (ii=0; ii<((div_a+1)*(div_b+1)); ii++)
          if((n_coord[ii]=(double *)malloc( (int)3*sizeof(double)) )==NULL)
          { printf(" ERROR: n_coord[%d] malloc failure in meshSurfs surf:%s can not be meshed\n\n"
            , ii, surf[j].name); thread_param->returnVal=-1; return(&thread_param->returnVal); }
  
        /* ini data for mesh-improver */
        jj=0;
        for(ii=0; ii<=apre->nmax; ii++) n_indx[ii]=-1;      /* initialized as unused */
        for(ii=0; ii<((div_a+1)*(div_b+1)); ii++) n_type[ii]=1; /* initialized as surface nodes */ 
      }
      /* INTERRUPT of section mesh-improver */

      /* erzeugen der elemente  */
      k=ke=0;

#if TEST2
  printf("hallo3 unbalanced:%d\n",jbuf);
#endif
      /* allocate memory for final-node-buffer nbuf and final nodes */
      if ((nbuf = (int **)realloc((int **)nbuf, (apre->nmax+1)*sizeof(int *)) ) == NULL )
      { printf(" ERROR: realloc failure in meshSurf, nodes not installed\n\n"); thread_param->returnVal=-1; return(&thread_param->returnVal); }
      for (v=sum_nbuf; v<=apre->nmax; v++)
      {
        if ((nbuf[v] = (int *)malloc( (2)*sizeof(int)) ) == NULL )
        { printf(" ERROR: realloc failure in meshSurf, nodes not installed\n\n"); thread_param->returnVal=-1; return(&thread_param->returnVal); }
        nbuf[v][0]=0;
      }
      sum_nbuf=apre->nmax+1;
          
      if (fillSurfFlag)
      {
        /* if we have a substitute surf, fill the original one */
        if(mapflag) s=j; j=jbuf;
        surf[j].npgn=0;
        if( (surf[j].pgn=fptr= (GLdouble *)realloc( (GLdouble *)surf[j].pgn, (div_a*div_b*28)*sizeof(GLdouble) )) == NULL )
        { printf(" ERROR1: realloc failure in meshSurf()\n\n"); thread_param->returnVal=-1; return(&thread_param->returnVal); }
        for (b=0; b<div_b  ; b++)
        {
          for (a=0; a<div_a; a++)
          {
            if ((a>=div_l[sa1])&&(b<offs_sa1)) goto nofill;
            if ((a>=div_l[sa2])&&(b>=div_b-offs_sa2)) goto nofill;
            if (surf[j].ori=='+')
            {
              *fptr=GL_POLYGON_TOKEN; fptr++;
              *fptr=3; fptr++;
              v_result(&npre[n_ba[(b  )*amax + a    ]].nx,&npre[n_ba[(b+1)*amax + a    ]].nx, v1); 
              v_result(&npre[n_ba[(b  )*amax + a    ]].nx,&npre[n_ba[(b  )*amax + (a+1)]].nx, v2);
              v_prod(v1,v2,vn);
              v_norm(vn,fptr); fptr+=3;
              *fptr=npre[n_ba[(b  )*amax + a    ]].nx; fptr++;
              *fptr=npre[n_ba[(b  )*amax + a    ]].ny; fptr++;
              *fptr=npre[n_ba[(b  )*amax + a    ]].nz; fptr++;

              *fptr=npre[n_ba[(b+1)*amax + a    ]].nx; fptr++;
              *fptr=npre[n_ba[(b+1)*amax + a    ]].ny; fptr++;
              *fptr=npre[n_ba[(b+1)*amax + a    ]].nz; fptr++;

              *fptr=npre[n_ba[(b  )*amax + (a+1)]].nx; fptr++;
              *fptr=npre[n_ba[(b  )*amax + (a+1)]].ny; fptr++;
              *fptr=npre[n_ba[(b  )*amax + (a+1)]].nz; fptr++;


              *fptr=GL_POLYGON_TOKEN; fptr++;
              *fptr=3; fptr++;
              v_result(&npre[n_ba[(b  )*amax + (a+1)]].nx,&npre[n_ba[(b+1)*amax + a    ]].nx, v1); 
              v_result(&npre[n_ba[(b  )*amax + (a+1)]].nx,&npre[n_ba[(b+1)*amax + (a+1)]].nx, v2);
              v_prod(v1,v2,vn);
              v_norm(vn,fptr); fptr+=3;
              *fptr=npre[n_ba[(b  )*amax + (a+1)]].nx; fptr++;
              *fptr=npre[n_ba[(b  )*amax + (a+1)]].ny; fptr++;
              *fptr=npre[n_ba[(b  )*amax + (a+1)]].nz; fptr++;
			 
              *fptr=npre[n_ba[(b+1)*amax + a    ]].nx; fptr++;
              *fptr=npre[n_ba[(b+1)*amax + a    ]].ny; fptr++;
              *fptr=npre[n_ba[(b+1)*amax + a    ]].nz; fptr++;
			 
              *fptr=npre[n_ba[(b+1)*amax + (a+1)]].nx; fptr++;
              *fptr=npre[n_ba[(b+1)*amax + (a+1)]].ny; fptr++;
              *fptr=npre[n_ba[(b+1)*amax + (a+1)]].nz; fptr++;
              surf[j].npgn+=28;
            }
            else
            {
              *fptr=GL_POLYGON_TOKEN; fptr++;
              *fptr=3; fptr++;
              v_result(&npre[n_ba[(b  )*amax + a    ]].nx,&npre[n_ba[(b  )*amax + (a+1)]].nx, v1); 
              v_result(&npre[n_ba[(b  )*amax + a    ]].nx,&npre[n_ba[(b+1)*amax + a    ]].nx, v2);
              v_prod(v1,v2,vn);
              v_norm(vn,fptr); fptr+=3;
              *fptr=npre[n_ba[(b  )*amax + a    ]].nx; fptr++;
              *fptr=npre[n_ba[(b  )*amax + a    ]].ny; fptr++;
              *fptr=npre[n_ba[(b  )*amax + a    ]].nz; fptr++;
			                         
              *fptr=npre[n_ba[(b  )*amax + (a+1)]].nx; fptr++;
              *fptr=npre[n_ba[(b  )*amax + (a+1)]].ny; fptr++;
              *fptr=npre[n_ba[(b  )*amax + (a+1)]].nz; fptr++;
			                         
              *fptr=npre[n_ba[(b+1)*amax + a    ]].nx; fptr++;
              *fptr=npre[n_ba[(b+1)*amax + a    ]].ny; fptr++;
              *fptr=npre[n_ba[(b+1)*amax + a    ]].nz; fptr++;

              *fptr=GL_POLYGON_TOKEN; fptr++;
              *fptr=3; fptr++;
              v_result(&npre[n_ba[(b  )*amax + (a+1)]].nx,&npre[n_ba[(b+1)*amax + (a+1)]].nx, v1); 
              v_result(&npre[n_ba[(b  )*amax + (a+1)]].nx,&npre[n_ba[(b+1)*amax + a    ]].nx, v2);
              v_prod(v1,v2,vn);
              v_norm(vn,fptr); fptr+=3;
              *fptr=npre[n_ba[(b  )*amax + (a+1)]].nx; fptr++;
              *fptr=npre[n_ba[(b  )*amax + (a+1)]].ny; fptr++;
              *fptr=npre[n_ba[(b  )*amax + (a+1)]].nz; fptr++;
			                         
              *fptr=npre[n_ba[(b+1)*amax + (a+1)]].nx; fptr++;
              *fptr=npre[n_ba[(b+1)*amax + (a+1)]].ny; fptr++;
              *fptr=npre[n_ba[(b+1)*amax + (a+1)]].nz; fptr++;
			                         
              *fptr=npre[n_ba[(b+1)*amax + a    ]].nx; fptr++;
              *fptr=npre[n_ba[(b+1)*amax + a    ]].ny; fptr++;
              *fptr=npre[n_ba[(b+1)*amax + a    ]].nz; fptr++;
              surf[j].npgn+=28;
            }
            nofill:;
          }
        }
        if(mapflag) j=s; /* change back to substitute surf */
      }
      else if (surf[j].etyp==7)
      {
        /* allocate memory for embeded elements */
        if((surf[j].elem=(int *)realloc((int *)surf[j].elem, (div_a*div_b*2)*sizeof(int)) )==NULL)
        { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed, surf->elem\n\n", surf[j].name);
          thread_param->returnVal=-1; return(&thread_param->returnVal); }
        for (b=0; b<div_b  ; b++)
        {
          for (a=0; a<div_a; a++)
          {
            if ((a>=div_l[sa1])&&(b<offs_sa1)) goto noelem7;
            if ((a>=div_l[sa2])&&(b>=div_b-offs_sa2)) goto noelem7;
            if (surf[j].ori=='+')
            {
              ebuf[0]=n_ba[(b  )*amax + a    ];
              ebuf[1]=n_ba[(b+1)*amax + a    ];
              ebuf[2]=n_ba[(b  )*amax + (a+1)];
  //NOTHREAD if(sem_wait(&sem[8])) error("sem_wait");
              elem_define( anz->emax+1, 7, ebuf, 0, surf[j].eattr );
              surf[j].elem[k]=anz->emax;
              k++;
              ebuf[0]=n_ba[(b  )*amax + (a+1)];
              ebuf[1]=n_ba[(b+1)*amax + a    ];
              ebuf[2]=n_ba[(b+1)*amax + (a+1)];
              elem_define( anz->emax+1, 7, ebuf, 0, surf[j].eattr );
  //NOTHREAD if(sem_post(&sem[8])) error("sem_post");
              surf[j].elem[k]=anz->emax;
              k++;
            }
            else
            {
              ebuf[0]=n_ba[(b  )*amax + a    ];
              ebuf[1]=n_ba[(b  )*amax + (a+1)];
              ebuf[2]=n_ba[(b+1)*amax + a    ];
  //NOTHREAD if(sem_wait(&sem[8])) error("sem_wait");
              elem_define( anz->emax+1, 7, ebuf, 0, surf[j].eattr );
              surf[j].elem[k]=anz->emax;
              k++;
              ebuf[0]=n_ba[(b  )*amax + (a+1)];
              ebuf[1]=n_ba[(b+1)*amax + (a+1)];
              ebuf[2]=n_ba[(b+1)*amax + a    ];
              elem_define( anz->emax+1, 7, ebuf, 0, surf[j].eattr );
  //NOTHREAD if(sem_post(&sem[8])) error("sem_post");
              surf[j].elem[k]=anz->emax;
              k++;
            }

              /* RESTART of section mesh-improver, describe variables for the mesh-improver */
            if (surf[j].ori=='+')
            {
              ebuf[0]=n_ba[(b  )*amax + a    ];
              ebuf[1]=n_ba[(b+1)*amax + a    ];
              ebuf[2]=n_ba[(b+1)*amax + (a+1)];
              ebuf[3]=n_ba[(b  )*amax + (a+1)];
            }
            else
            {
              ebuf[0]=n_ba[(b  )*amax + a    ];
              ebuf[1]=n_ba[(b  )*amax + (a+1)];
              ebuf[2]=n_ba[(b+1)*amax + (a+1)];
              ebuf[3]=n_ba[(b+1)*amax + a    ];
	    }

              for (ii=0; ii<4; ii++)
              {
                if( n_indx[ebuf[ii]]==-1) /* node not stored yet */
                {
                  n_ori[jj]=ebuf[ii];
                  n_coord[jj][0]=npre[n_ori[jj]].nx;
                  n_coord[jj][1]=npre[n_ori[jj]].ny;
                  n_coord[jj][2]=npre[n_ori[jj]].nz;
                  jj++;
                  n_indx[ebuf[ii]]=jj; /* first number is "1" to match the needs of the improver */
                }
#if TEST2
		printf("en[i]:%d n_indx[en[i]]:%d ke:%d i:%d\n", ebuf[ii],n_indx[ebuf[ii]],ke,ii);
#endif
                e_nod[ke][ii]=n_indx[ebuf[ii]];
              }
              ke++;
              /* INTERRUPT of section mesh-improver */

            noelem7:;
          }
        }
      }
      else if (surf[j].etyp==8)
      {
        /* allocate memory for embeded elements */
        if((surf[j].elem=(int *)realloc((int *)surf[j].elem, (div_a*div_b/2)*sizeof(int)) )==NULL)
        { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed, surf->elem (2)\n\n", surf[j].name);
          thread_param->returnVal=-1; return(&thread_param->returnVal); }
        for (b=0; b<div_b-1; b+=2)
        {
          for (a=0; a<div_a-1; a+=2)
          {
            if ((a>=div_l[sa1])&&(b<offs_sa1)) goto noelem8;
            if ((a>=div_l[sa2])&&(b>=div_b-offs_sa2)) goto noelem8;
            if (surf[j].ori=='+')
            {
              ebuf[0]=n_ba[(b  )*amax + a    ];
              ebuf[1]=n_ba[(b+2)*amax + a    ];
              ebuf[2]=n_ba[(b  )*amax + (a+2)];
              ebuf[3]=n_ba[(b+1)*amax + (a  )];
              ebuf[4]=n_ba[(b+1)*amax + (a+1)];
              ebuf[5]=n_ba[(b  )*amax + (a+1)];
  //NOTHREAD if(sem_wait(&sem[8])) error("sem_wait");
              elem_define( anz->emax+1, 8, ebuf, 0, surf[j].eattr );
              surf[j].elem[k]=anz->emax;
              k++;
              ebuf[0]=n_ba[(b  )*amax + (a+2)];
              ebuf[1]=n_ba[(b+2)*amax + a    ];
              ebuf[2]=n_ba[(b+2)*amax + (a+2)];
              ebuf[3]=n_ba[(b+1)*amax + (a+1)];
              ebuf[4]=n_ba[(b+2)*amax + (a+1)];
              ebuf[5]=n_ba[(b+1)*amax + (a+2)];
              elem_define( anz->emax+1, 8, ebuf, 0, surf[j].eattr );
  //NOTHREAD if(sem_post(&sem[8])) error("sem_post");
              surf[j].elem[k]=anz->emax;
              k++;
            }
            else
            {
              ebuf[0]=n_ba[(b  )*amax + a    ];
              ebuf[1]=n_ba[(b  )*amax + (a+2)];
              ebuf[2]=n_ba[(b+2)*amax + a    ];
              ebuf[3]=n_ba[(b  )*amax + (a+1)];
              ebuf[4]=n_ba[(b+1)*amax + (a+1)];
              ebuf[5]=n_ba[(b+1)*amax + (a  )];
  //NOTHREAD if(sem_wait(&sem[8])) error("sem_wait");
              elem_define( anz->emax+1, 8, ebuf, 0, surf[j].eattr );
              surf[j].elem[k]=anz->emax;
              k++;
              ebuf[0]=n_ba[(b  )*amax + (a+2)];
              ebuf[1]=n_ba[(b+2)*amax + (a+2)];
              ebuf[2]=n_ba[(b+2)*amax + a    ];
              ebuf[3]=n_ba[(b+1)*amax + (a+2)];
              ebuf[4]=n_ba[(b+2)*amax + (a+1)];
              ebuf[5]=n_ba[(b+1)*amax + (a+1)];
              elem_define( anz->emax+1, 8, ebuf, 0, surf[j].eattr );
  //NOTHREAD if(sem_post(&sem[8])) error("sem_post");
              surf[j].elem[k]=anz->emax;
              k++;
            }
            noelem8:;
          }
        }

        /* RESTART of section mesh-improver, describe variables for the mesh-improver */
        for (b=0; b<div_b  ; b++)
        {
          for (a=0; a<div_a; a++)
          {
            if ((a>=div_l[sa1])&&(b<offs_sa1)) goto noelem7i;
            if ((a>=div_l[sa2])&&(b>=div_b-offs_sa2)) goto noelem7i;
            if (surf[j].ori=='+')
            {
              ebuf[0]=n_ba[(b  )*amax + a    ];
              ebuf[1]=n_ba[(b+1)*amax + a    ];
              ebuf[2]=n_ba[(b+1)*amax + (a+1)];
              ebuf[3]=n_ba[(b  )*amax + (a+1)];
            }
            else
            {
              ebuf[0]=n_ba[(b  )*amax + a    ];
              ebuf[1]=n_ba[(b  )*amax + (a+1)];
              ebuf[2]=n_ba[(b+1)*amax + (a+1)];
              ebuf[3]=n_ba[(b+1)*amax + a    ];
	    }

            for (ii=0; ii<4; ii++)
            {
              if( n_indx[ebuf[ii]]==-1) /* node not stored yet */
              {
                n_ori[jj]=ebuf[ii];
                n_coord[jj][0]=npre[n_ori[jj]].nx;
                n_coord[jj][1]=npre[n_ori[jj]].ny;
                n_coord[jj][2]=npre[n_ori[jj]].nz;
                jj++;
                n_indx[ebuf[ii]]=jj; /* first number is "1" to match the needs of the improver */
              }
              e_nod[ke][ii]=n_indx[ebuf[ii]];
            }
            ke++;
	  noelem7i:;
	  }
	}
        /* INTERRUPT of section mesh-improver */

      }
      else if (surf[j].etyp==9)
      {
        /* allocate memory for embeded elements */
        if((surf[j].elem=(int *)realloc((int *)surf[j].elem, (div_a*div_b)*sizeof(int)) )==NULL)
        { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed, surf->elem (3)\n\n", surf[j].name);
          thread_param->returnVal=-1; return(&thread_param->returnVal); }
        for (b=0; b<div_b  ; b++)
        {
          for (a=0; a<div_a; a++)
          {
            if ((a>=div_l[sa1])&&(b<offs_sa1)) goto noelem9;
            if ((a>=div_l[sa2])&&(b>=div_b-offs_sa2)) goto noelem9;
            if (surf[j].ori=='+')
            {
              ebuf[0]=n_ba[(b  )*amax + a    ];
              ebuf[1]=n_ba[(b+1)*amax + a    ];
              ebuf[2]=n_ba[(b+1)*amax + (a+1)];
              ebuf[3]=n_ba[(b  )*amax + (a+1)];
  //NOTHREAD if(sem_wait(&sem[8])) error("sem_wait");
              elem_define( anz->emax+1, 9 , ebuf, 0, surf[j].eattr );
  //NOTHREAD if(sem_post(&sem[8])) error("sem_post");
              surf[j].elem[k]=anz->emax;
            }
            else
            {
              ebuf[0]=n_ba[(b  )*amax + a    ];
              ebuf[1]=n_ba[(b  )*amax + (a+1)];
              ebuf[2]=n_ba[(b+1)*amax + (a+1)];
              ebuf[3]=n_ba[(b+1)*amax + a    ];
  //NOTHREAD if(sem_wait(&sem[8])) error("sem_wait");
              elem_define( anz->emax+1, 9, ebuf, 0, surf[j].eattr );
  //NOTHREAD if(sem_post(&sem[8])) error("sem_post");
               surf[j].elem[k]=anz->emax;
            }

              /* RESTART of section mesh-improver, describe variables for the mesh-improver */
              for (ii=0; ii<4; ii++)
              {
                if( n_indx[e_enqire[anz->emax].nod[ii]]==-1) /* node not stored yet */
                {
                  n_ori[jj]=e_enqire[anz->emax].nod[ii];
                  n_coord[jj][0]=npre[n_ori[jj]].nx;
                  n_coord[jj][1]=npre[n_ori[jj]].ny;
                  n_coord[jj][2]=npre[n_ori[jj]].nz;
                  jj++;
                  n_indx[e_enqire[anz->emax].nod[ii]]=jj; /* first number is "1" to match the needs of the improver */
                }
#if TEST2
		printf("en[i]:%d n_indx[en[i]]:%d ke:%d i:%d\n", e_enqire[anz->emax].nod[ii],n_indx[e_enqire[anz->emax].nod[ii]],ke,ii);
#endif
                e_nod[ke][ii]=n_indx[e_enqire[anz->emax].nod[ii]];
              }
              ke++;
              /* INTERRUPT of section mesh-improver */

            k++;
            noelem9:;
          }
        }
      }      
      else if (surf[j].etyp==10)
      {
#if TEST2
  printf("hallo qu8 unbalanced:%d\n",jbuf);
#endif
        /* allocate memory for embeded elements */
        if((surf[j].elem=(int *)realloc((int *)surf[j].elem, (div_a*div_b/4)*sizeof(int)) )==NULL)
        { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed, surf->elem (4)\n\n", surf[j].name);
          thread_param->returnVal=-1; return(&thread_param->returnVal); }
        for (b=0; b<div_b  ; b+=2)
        {
          for (a=0; a<div_a; a+=2)
          {
            if ((a>=div_l[sa1])&&(b<offs_sa1)) goto noelem10;
            if ((a>=div_l[sa2])&&(b>=div_b-offs_sa2)) goto noelem10;
            if (surf[j].ori=='+')
            {
              ebuf[0]=n_ba[(b  )*amax + a    ];
              ebuf[4]=n_ba[(b+1)*amax + a    ];
              ebuf[1]=n_ba[(b+2)*amax + a    ];
              ebuf[5]=n_ba[(b+2)*amax + (a+1)];
              ebuf[2]=n_ba[(b+2)*amax + (a+2)];
              ebuf[6]=n_ba[(b+1)*amax + (a+2)];
              ebuf[3]=n_ba[(b)*amax   + (a+2)];
              ebuf[7]=n_ba[(b  )*amax + (a+1)];
  //NOTHREAD if(sem_wait(&sem[8])) error("sem_wait");
              elem_define( anz->emax+1, 10, ebuf, 0, surf[j].eattr );
  //NOTHREAD if(sem_post(&sem[8])) error("sem_post");
              surf[j].elem[k]=anz->emax;
            }
            else
            {
              ebuf[0]=n_ba[(b  )*amax + a    ];
              ebuf[4]=n_ba[(b  )*amax + (a+1)];
              ebuf[1]=n_ba[(b)*amax   + (a+2)];
              ebuf[5]=n_ba[(b+1)*amax + (a+2)];
              ebuf[2]=n_ba[(b+2)*amax + (a+2)];
              ebuf[6]=n_ba[(b+2)*amax + (a+1)];
              ebuf[3]=n_ba[(b+2)*amax + a    ];
              ebuf[7]=n_ba[(b+1)*amax + a    ];
  //NOTHREAD if(sem_wait(&sem[8])) error("sem_wait");
              elem_define( anz->emax+1, 10, ebuf, 0, surf[j].eattr );
  //NOTHREAD if(sem_post(&sem[8])) error("sem_post");
              surf[j].elem[k]=anz->emax;
            }

              /* RESTART of section mesh-improver, describe variables for the mesh-improver */
              for (ii=0; ii<8; ii++)
              {
                if( n_indx[e_enqire[anz->emax].nod[ii]]==-1) /* node not stored yet */
                {
                  n_ori[jj]=e_enqire[anz->emax].nod[ii];
                  n_coord[jj][0]=npre[n_ori[jj]].nx;
                  n_coord[jj][1]=npre[n_ori[jj]].ny;
                  n_coord[jj][2]=npre[n_ori[jj]].nz;
                  jj++;
                  n_indx[e_enqire[anz->emax].nod[ii]]=jj; /* first number is "1" to match the needs of the improver */
                }
#if TEST2
		printf("en[i]:%d n_indx[en[i]]:%d ke:%d i:%d\n", e_enqire[anz->emax].nod[ii],n_indx[e_enqire[anz->emax].nod[ii]],ke,ii);
#endif
                e_nod[ke][ii]=n_indx[e_enqire[anz->emax].nod[ii]];
              }
              ke++;
              /* INTERRUPT of section mesh-improver */

            k++;
            noelem10:;
          }
        }
      }
      
      surf[j].ne=k;

      /* RESTART of section mesh-improver: determine the edge nodes */
      if ((surf[j].etyp>=7)&&(surf[j].etyp<=10))
      {
        v=0;
        u=0;
        a=0; 
        for (b=0; b<div_b; b++) { n_edge[v][u]=n_indx[n_ba[b*amax +a]]; if(n_edge[v][u]>=0) n_type[n_edge[v][u++]]=-1; } 
        s_div[v][0]=b;
        b=div_b; 
        for (a=0; a<div_a-offs_sa2; a++) { n_edge[v][u]=n_indx[n_ba[b*amax +a]]; if(n_edge[v][u]>=0) n_type[n_edge[v][u++]]=-1; } 
        s_div[v][1]=a;
        a=div_a; 
        for (b=div_b-offs_sa2; b>offs_sa1; b--) { n_edge[v][u]=n_indx[n_ba[b*amax +a]]; if(n_edge[v][u]>=0) n_type[n_edge[v][u++]]=-1; } 
        s_div[v][2]=div_b-offs_sa2-offs_sa1;
        b=0; 
        for (a=div_a-offs_sa1; a>0; a--) { n_edge[v][u]=n_indx[n_ba[b*amax +a]]; if(n_edge[v][u]>=0) n_type[n_edge[v][u++]]=-1; } 
        s_div[v][3]=div_a-offs_sa1;

        if (meshImprover( &surf[j].etyp, &jj, &ke, n_indx, n_ori, n_coord, e_nod, n_type, n_edge, s_div )==0)
	{
          /* write the coordinates back */
          for (ii=0; ii<jj; ii++)
          {
            npre[n_ori[ii]].nx=n_coord[ii][0];
            npre[n_ori[ii]].ny=n_coord[ii][1];
            npre[n_ori[ii]].nz=n_coord[ii][2];
          }
	}
  
        /* free some space */
        for (ii=0; ii<((div_a+1)*(div_b+1)); ii++) free(n_coord[ii]);	
        for (ii=0; ii<((div_a+1)*(div_b+1)); ii++) free(e_nod[ii]); 
        for (ii=0; ii<MAX_SURFS_PER_BODY; ii++) free(n_edge[ii]);
        for (ii=0; ii<MAX_SURFS_PER_BODY; ii++) free(s_div[ii]);
        free(n_coord);
        free(e_nod);
        free(n_edge);
        free(s_div);

        free(n_indx);
        free(n_ori);
        free(n_type);
      }
      /* END of section mesh-improver */

      if(( nurbsflag )&&( surf[j].sh>-1 ))
      {
        if(shape[surf[j].sh].type==4) projSurfToNurbs( nurbs, shape[surf[j].sh].p[0], surf, j, npre );
      }
 
      free(n_ba);
      free(n_uv);
      free(x);
      free(y);
      free(z);
    }
    else /* surf has balanced edges */
    {
      transitionflag=0;
#if TEST2
  printf("hallo balanced:%d\n",jbuf);
#endif

      vmax=div_l[3]+1;
      umax=div_l[0]+1;

      if( (n_uv=(int *)malloc( (umax)*(vmax)*sizeof(int) ) )==NULL)
      { printf(" ERROR1: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
        thread_param->returnVal=-1; return(&thread_param->returnVal); }
      if( (x=(double *)malloc( (umax)*(vmax)*sizeof(double) ) )==NULL)
      { printf(" ERROR2: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
        thread_param->returnVal=-1; return(&thread_param->returnVal); }
      if( (y=(double *)malloc( (umax)*(vmax)*sizeof(double) ) )==NULL)
      { printf(" ERROR3: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
        thread_param->returnVal=-1; return(&thread_param->returnVal); }
      if( (z=(double *)malloc( (umax)*(vmax)*sizeof(double) ) )==NULL)
      { printf(" ERROR4: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
        thread_param->returnVal=-1; return(&thread_param->returnVal); }

      /* allocate memory for embeded nodes */
      if((surf[j].nod=(int *)realloc((int *)surf[j].nod, ((div_l[0]-1)*(div_l[3]-1)+1)*sizeof(int)) )==NULL)
      { printf(" ERROR5: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
        thread_param->returnVal=-1; return(&thread_param->returnVal); }

      /* knoten bestimmen  */
#if TEST2
  printf("hallo before:%d\n",jbuf);
#endif
      if( fillSurf( j, n_uv, umax, vmax, x,y,z) <1) { thread_param->returnVal=-2; return(&thread_param->returnVal); }
      surf[j].fail=0;
#if TEST2
  printf("hallo after:%d\n",jbuf);
#endif

      /* erzeugen der elemente   */
      k=0;

      /* allocate memory for final-node-buffer nbuf and final nodes */
      if ((nbuf = (int **)realloc((int **)nbuf, (apre->nmax+1)*sizeof(int *)) ) == NULL )
      { printf(" ERROR: realloc failure in meshSurf, nodes not installed\n\n"); thread_param->returnVal=-1; return(&thread_param->returnVal); }
      for (v=sum_nbuf; v<=apre->nmax; v++)
      {
        if ((nbuf[v] = (int *)malloc( (2)*sizeof(int)) ) == NULL )
        { printf(" ERROR: realloc failure in meshSurf, nodes not installed\n\n"); thread_param->returnVal=-1; return(&thread_param->returnVal); }
        nbuf[v][0]=0;
      }
      sum_nbuf=apre->nmax+1;

#if TEST2
      printf("hallo %d etyp:%d\n",jbuf, surf[j].etyp);
#endif
      if (fillSurfFlag)
      {
        surf[j].npgn=0;
        if( (surf[j].pgn=fptr= (GLdouble *)realloc( (GLdouble *)surf[j].pgn, (div_l[0]*div_l[3]*28)*sizeof(GLdouble) )) == NULL )
        { printf(" ERROR1: realloc failure in meshSurf()\n\n"); thread_param->returnVal=-1; return(&thread_param->returnVal); }
        for (u=0; u<div_l[0]; u++)
        {
          for (v=0; v<div_l[3]; v++)
          {
            if (surf[j].ori=='+')
            {
              *fptr=GL_POLYGON_TOKEN; fptr++;
              *fptr=3; fptr++;
              v_result(&npre[n_uv[(u  )*vmax + v    ]].nx,&npre[n_uv[(u+1)*vmax + v    ]].nx, v1); 
              v_result(&npre[n_uv[(u  )*vmax + v    ]].nx,&npre[n_uv[(u  )*vmax + (v+1)]].nx, v2);
              v_prod(v1,v2,vn);
              v_norm(vn,fptr); fptr+=3;
              *fptr=npre[n_uv[(u  )*vmax + v    ]].nx; fptr++;
              *fptr=npre[n_uv[(u  )*vmax + v    ]].ny; fptr++;
              *fptr=npre[n_uv[(u  )*vmax + v    ]].nz; fptr++;
			                         
              *fptr=npre[n_uv[(u+1)*vmax + v    ]].nx; fptr++;
              *fptr=npre[n_uv[(u+1)*vmax + v    ]].ny; fptr++;
              *fptr=npre[n_uv[(u+1)*vmax + v    ]].nz; fptr++;
			                         
              *fptr=npre[n_uv[(u  )*vmax + (v+1)]].nx; fptr++;
              *fptr=npre[n_uv[(u  )*vmax + (v+1)]].ny; fptr++;
              *fptr=npre[n_uv[(u  )*vmax + (v+1)]].nz; fptr++;


              *fptr=GL_POLYGON_TOKEN; fptr++;
              *fptr=3; fptr++;
              v_result(&npre[n_uv[(u  )*vmax + (v+1)]].nx,&npre[n_uv[(u+1)*vmax + v    ]].nx, v1); 
              v_result(&npre[n_uv[(u  )*vmax + (v+1)]].nx,&npre[n_uv[(u+1)*vmax + (v+1)]].nx, v2);
              v_prod(v1,v2,vn);
              v_norm(vn,fptr); fptr+=3;
              *fptr=npre[n_uv[(u  )*vmax + (v+1)]].nx; fptr++;
              *fptr=npre[n_uv[(u  )*vmax + (v+1)]].ny; fptr++;
              *fptr=npre[n_uv[(u  )*vmax + (v+1)]].nz; fptr++;
			                         
              *fptr=npre[n_uv[(u+1)*vmax + v    ]].nx; fptr++;
              *fptr=npre[n_uv[(u+1)*vmax + v    ]].ny; fptr++;
              *fptr=npre[n_uv[(u+1)*vmax + v    ]].nz; fptr++;
			                         
              *fptr=npre[n_uv[(u+1)*vmax + (v+1)]].nx; fptr++;
              *fptr=npre[n_uv[(u+1)*vmax + (v+1)]].ny; fptr++;
              *fptr=npre[n_uv[(u+1)*vmax + (v+1)]].nz; fptr++;
              surf[j].npgn+=28;
            }
            else
            {
              *fptr=GL_POLYGON_TOKEN; fptr++;
              *fptr=3; fptr++;
              v_result(&npre[n_uv[(u  )*vmax + v    ]].nx,&npre[n_uv[(u  )*vmax + (v+1)]].nx, v1); 
              v_result(&npre[n_uv[(u  )*vmax + v    ]].nx,&npre[n_uv[(u+1)*vmax + v    ]].nx, v2);
              v_prod(v1,v2,vn);
              v_norm(vn,fptr); fptr+=3;
              *fptr=npre[n_uv[(u  )*vmax + v    ]].nx; fptr++;
              *fptr=npre[n_uv[(u  )*vmax + v    ]].ny; fptr++;
              *fptr=npre[n_uv[(u  )*vmax + v    ]].nz; fptr++;
			                         
              *fptr=npre[n_uv[(u  )*vmax + (v+1)]].nx; fptr++;
              *fptr=npre[n_uv[(u  )*vmax + (v+1)]].ny; fptr++;
              *fptr=npre[n_uv[(u  )*vmax + (v+1)]].nz; fptr++;
			                         
              *fptr=npre[n_uv[(u+1)*vmax + v    ]].nx; fptr++;
              *fptr=npre[n_uv[(u+1)*vmax + v    ]].ny; fptr++;
              *fptr=npre[n_uv[(u+1)*vmax + v    ]].nz; fptr++;


              *fptr=GL_POLYGON_TOKEN; fptr++;
              *fptr=3; fptr++;
              v_result(&npre[n_uv[(u  )*vmax + (v+1)]].nx,&npre[n_uv[(u+1)*vmax + (v+1)]].nx, v1); 
              v_result(&npre[n_uv[(u  )*vmax + (v+1)]].nx,&npre[n_uv[(u+1)*vmax + v    ]].nx, v2);
              v_prod(v1,v2,vn);
              v_norm(vn,fptr); fptr+=3;
              *fptr=npre[n_uv[(u  )*vmax + (v+1)]].nx; fptr++;
              *fptr=npre[n_uv[(u  )*vmax + (v+1)]].ny; fptr++;
              *fptr=npre[n_uv[(u  )*vmax + (v+1)]].nz; fptr++;
			                         
              *fptr=npre[n_uv[(u+1)*vmax + (v+1)]].nx; fptr++;
              *fptr=npre[n_uv[(u+1)*vmax + (v+1)]].ny; fptr++;
              *fptr=npre[n_uv[(u+1)*vmax + (v+1)]].nz; fptr++;
			                         
              *fptr=npre[n_uv[(u+1)*vmax + v    ]].nx; fptr++;
              *fptr=npre[n_uv[(u+1)*vmax + v    ]].ny; fptr++;
              *fptr=npre[n_uv[(u+1)*vmax + v    ]].nz; fptr++;
              surf[j].npgn+=28;
            }
          }
        }
      }
      else if (surf[j].etyp==7)
      {
        /* allocate memory for embeded elements */
        if((surf[j].elem=(int *)realloc((int *)surf[j].elem, (div_l[0]*div_l[3]*2)*sizeof(int)) )==NULL)
        { printf(" ERROR6: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
          thread_param->returnVal=-1; return(&thread_param->returnVal); }
        for (u=0; u<div_l[0]; u++)
        {
          for (v=0; v<div_l[3]; v++)
          {
            if (surf[j].ori=='+')
            {
              ebuf[0]=n_uv[(u  )*vmax + v    ];
              ebuf[1]=n_uv[(u+1)*vmax + v    ];
              ebuf[2]=n_uv[(u  )*vmax + (v+1)];
  //NOTHREAD if(sem_wait(&sem[8])) error("sem_wait");
              elem_define( anz->emax+1, 7, ebuf, 0, surf[j].eattr );
              surf[j].elem[k]=anz->emax;
              k++;
              ebuf[0]=n_uv[(u  )*vmax + (v+1)];
              ebuf[1]=n_uv[(u+1)*vmax + v    ];
              ebuf[2]=n_uv[(u+1)*vmax + (v+1)];
              elem_define( anz->emax+1, 7, ebuf, 0, surf[j].eattr );
  //NOTHREAD if(sem_post(&sem[8])) error("sem_post");
              surf[j].elem[k]=anz->emax;
            }
            else
	    {
              ebuf[0]=n_uv[(u  )*vmax + v    ];
              ebuf[1]=n_uv[(u  )*vmax + (v+1)];
              ebuf[2]=n_uv[(u+1)*vmax + v    ];
  //NOTHREAD if(sem_wait(&sem[8])) error("sem_wait");
              elem_define( anz->emax+1, 7, ebuf, 0, surf[j].eattr );
              surf[j].elem[k]=anz->emax;
              k++;
              ebuf[0]=n_uv[(u  )*vmax + (v+1)];
              ebuf[1]=n_uv[(u+1)*vmax + (v+1)];
              ebuf[2]=n_uv[(u+1)*vmax + v    ];
              elem_define( anz->emax+1, 7, ebuf, 0, surf[j].eattr );
  //NOTHREAD if(sem_post(&sem[8])) error("sem_post");
              surf[j].elem[k]=anz->emax;
            }
            k++;
          }
        }
      }
      else if (surf[j].etyp==8)
      {
        /* allocate memory for embeded elements */
        if((surf[j].elem=(int *)realloc((int *)surf[j].elem, (div_l[0]*div_l[3]/2)*sizeof(int)) )==NULL)
        { printf(" ERROR7: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
          thread_param->returnVal=-1; return(&thread_param->returnVal); }
        for (u=0; u<div_l[0]-1; u+=2)
        {
          for (v=0; v<div_l[3]-1; v+=2)
          {
            if (surf[j].ori=='+')
            {
              ebuf[0]=n_uv[(u  )*vmax + v    ];
              ebuf[1]=n_uv[(u+2)*vmax + v    ];
              ebuf[2]=n_uv[(u  )*vmax + (v+2)];
              ebuf[3]=n_uv[(u+1)*vmax + v    ];
              ebuf[4]=n_uv[(u+1)*vmax + (v+1)];
              ebuf[5]=n_uv[(u  )*vmax + (v+1)];
  //NOTHREAD if(sem_wait(&sem[8])) error("sem_wait");
              elem_define( anz->emax+1, 8, ebuf, 0, surf[j].eattr );
              surf[j].elem[k]=anz->emax;
              k++;
              ebuf[0]=n_uv[(u  )*vmax + (v+2)];
              ebuf[1]=n_uv[(u+2)*vmax + (v  )];
              ebuf[2]=n_uv[(u+2)*vmax + (v+2)];
              ebuf[3]=n_uv[(u+1)*vmax + (v+1)];
              ebuf[4]=n_uv[(u+2)*vmax + (v+1)];
              ebuf[5]=n_uv[(u+1)*vmax + (v+2)];
              elem_define( anz->emax+1, 8, ebuf, 0, surf[j].eattr );
  //NOTHREAD if(sem_post(&sem[8])) error("sem_post");
              surf[j].elem[k]=anz->emax;
            }
            else
	    {
              ebuf[0]=n_uv[(u  )*vmax + v    ];
              ebuf[1]=n_uv[(u  )*vmax + (v+2)];
              ebuf[2]=n_uv[(u+2)*vmax + v    ];
              ebuf[3]=n_uv[(u  )*vmax + (v+1)];
              ebuf[4]=n_uv[(u+1)*vmax + (v+1)];
              ebuf[5]=n_uv[(u+1)*vmax + (v  )];
  //NOTHREAD if(sem_wait(&sem[8])) error("sem_wait");
              elem_define( anz->emax+1, 8, ebuf, 0, surf[j].eattr );
              surf[j].elem[k]=anz->emax;
              k++;
              ebuf[0]=n_uv[(u  )*vmax + (v+2)];
              ebuf[1]=n_uv[(u+2)*vmax + (v+2)];
              ebuf[2]=n_uv[(u+2)*vmax + v    ];
              ebuf[3]=n_uv[(u+1)*vmax + (v+2)];
              ebuf[4]=n_uv[(u+2)*vmax + (v+1)];
              ebuf[5]=n_uv[(u+1)*vmax + (v+1)];
              elem_define( anz->emax+1, 8, ebuf, 0, surf[j].eattr );
  //NOTHREAD if(sem_post(&sem[8])) error("sem_post");
              surf[j].elem[k]=anz->emax;
            }
            k++;
          }
        }
      }
      else if (surf[j].etyp==9)
      {
        /* allocate memory for embeded elements */
        if((surf[j].elem=(int *)realloc((int *)surf[j].elem, (div_l[0]*div_l[3])*sizeof(int)) )==NULL)
        { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
          thread_param->returnVal=-1; return(&thread_param->returnVal); }

        if(writeCFDflag==1)
        {       
  //NOTHREAD if(sem_wait(&sem[9])) error("sem_wait");
          /* store the node-indexes for block-structured cfd meshes */
          if(surf[j].ori=='+')
          {
            printf("surf:%s\n", surf[j].name);
            if ( (nBlock = (NodeBlocks *)realloc((NodeBlocks *)nBlock, (apre->b+1) * sizeof(NodeBlocks))) == NULL )
              printf("\n\n ERROR: realloc failed, NodeBlocks\n\n") ;
            if ( (nBlock[apre->b].nod = (int *)malloc( (umax*vmax+1) * sizeof(int))) == NULL )
              printf("\n\n ERROR: malloc failed, NodeBlocks\n\n") ;
            nBlock[apre->b].dim=2;
            nBlock[apre->b].geo=j;
            nBlock[apre->b].i=vmax;
            nBlock[apre->b].j=umax;
            nBlock[apre->b].k=1;
            n=0;
            for (u=0; u<umax; u++)
            {
              for (v=0; v<vmax; v++)
              {
                nBlock[apre->b].nod[n]=n_uv[u*vmax+v];
                n++;
    	      }
            }
  
            /* determine the connectivity for cfd-blocks */
            if(surf[j].nl!=4)
            {
              printf("PRG_ERROR: found surface with no 4 edges:%d, call the admin.\n",surf[j].nl);
              exit(1);
            }
            for (v=0; v<surf[j].nl; v++)
            {
              /* duns-edges are imin, imax, jmin, jmax, (kmin, kmax) */
              if(v==0) ii=0;
              else if(v==3) ii=1;
              else if(v==1) ii=2;
              else ii=3;
  
              nBlock[apre->b].neighbor[v]=-1;
              nBlock[apre->b].bcface[v]=-1;
              nBlock[apre->b].map[v][0]=-1;  nBlock[apre->b].map[v][1]=-1; nBlock[apre->b].map[v][2]=-1;
    	
              /* i==nBlock[].strt1[][0], j==nBlock[].strt1[][1] */
              imax= vmax; jmax= umax;
              if(ii==0)
              {
                nBlock[apre->b].strt1[v][0]=1;    nBlock[apre->b].strt1[v][1]=1;    nBlock[apre->b].strt1[v][2]=1; 
                nBlock[apre->b].end_1[v][0]=1;    nBlock[apre->b].end_1[v][1]=jmax; nBlock[apre->b].end_1[v][2]=2; 
              }
              if(ii==1)
              {
                nBlock[apre->b].strt1[v][0]=1;    nBlock[apre->b].strt1[v][1]=jmax; nBlock[apre->b].strt1[v][2]=1; 
                nBlock[apre->b].end_1[v][0]=imax; nBlock[apre->b].end_1[v][1]=jmax; nBlock[apre->b].end_1[v][2]=2; 
              }
              if(ii==2)
              {
                nBlock[apre->b].strt1[v][0]=imax; nBlock[apre->b].strt1[v][1]=jmax; nBlock[apre->b].strt1[v][2]=1; 
                nBlock[apre->b].end_1[v][0]=imax; nBlock[apre->b].end_1[v][1]=1;    nBlock[apre->b].end_1[v][2]=2; 
              }
              if(ii==3)
              {
                nBlock[apre->b].strt1[v][0]=imax; nBlock[apre->b].strt1[v][1]=1;    nBlock[apre->b].strt1[v][2]=1; 
                nBlock[apre->b].end_1[v][0]=1;    nBlock[apre->b].end_1[v][1]=1;    nBlock[apre->b].end_1[v][2]=2; 
              }
  
              surFlag=1;
              for (u=0; u<anz_s; u++)
              {
                if((u!=j)&&(surf[u].name != (char *)NULL))  for (jj=0; jj<surf[u].nl; jj++)
    	        {
                  if((surf[j].l[ii]==surf[u].l[jj])&&(surf[j].typ[ii]==surf[u].typ[jj]))
    	          {
                   if(surf[j].o[ii]!=surf[u].o[jj])
                   {
                    surFlag=0;
    
                    /* corresponding surface */
                    nBlock[apre->b].neighbor[v]=u;
  
                    if(surf[j].typ[ii]=='l') printf(" block:%d master-name:%s side[%d]:%s\n", j+1, surf[j].name, ii, line[surf[j].l[ii]].name );
                    if(surf[j].typ[ii]=='c') printf(" block:%d master-name:%s side[%d]:%s\n", j+1, surf[j].name, ii, line[surf[j].l[ii]].name );
  
                    if(surf[u].typ[jj]=='l') printf(" block:%d slave-name: %s side[%d]:%s\n", u+1, surf[u].name, jj, line[surf[u].l[jj]].name );
                    if(surf[u].typ[jj]=='c') printf(" block:%d slave-name: %s side[%d]:%s\n", u+1, surf[u].name, jj, line[surf[u].l[jj]].name );
  
                    if((ii==0)&&(jj==0))
                    {
                       nBlock[apre->b].map[v][0]=4; nBlock[apre->b].map[v][1]=5; nBlock[apre->b].map[v][2]=3;
                    }
                    if((ii==1)&&(jj==0))
                    {
                      nBlock[apre->b].map[v][0]=5;  nBlock[apre->b].map[v][1]=1; nBlock[apre->b].map[v][2]=3;
                    }
                    if((ii==2)&&(jj==0))
                    {
                      nBlock[apre->b].map[v][0]=1;  nBlock[apre->b].map[v][1]=2; nBlock[apre->b].map[v][2]=3;
                    }
                    if((ii==3)&&(jj==0))
                    {
                      nBlock[apre->b].map[v][0]=2;  nBlock[apre->b].map[v][1]=4; nBlock[apre->b].map[v][2]=3;
                    }
  
                    if((ii==0)&&(jj==1))
                    {
                      nBlock[apre->b].map[v][0]=2; nBlock[apre->b].map[v][1]=4; nBlock[apre->b].map[v][2]=3; 
                    }
                    if((ii==1)&&(jj==1))
                    {
                      nBlock[apre->b].map[v][0]=4;  nBlock[apre->b].map[v][1]=5; nBlock[apre->b].map[v][2]=3;
                    }
                    if((ii==2)&&(jj==1))
                    {
                      nBlock[apre->b].map[v][0]=5;  nBlock[apre->b].map[v][1]=1; nBlock[apre->b].map[v][2]=3;
                    }
                    if((ii==3)&&(jj==1))
                    {
                      nBlock[apre->b].map[v][0]=1;  nBlock[apre->b].map[v][1]=2; nBlock[apre->b].map[v][2]=3;
                    }
  
                    if((ii==0)&&(jj==2))
                    {
                      nBlock[apre->b].map[v][0]=1; nBlock[apre->b].map[v][1]=2; nBlock[apre->b].map[v][2]=3; 
                    }
                    if((ii==1)&&(jj==2))
                    {
                      nBlock[apre->b].map[v][0]=2;  nBlock[apre->b].map[v][1]=4; nBlock[apre->b].map[v][2]=3;
                    }
                    if((ii==2)&&(jj==2))
                    {
                      nBlock[apre->b].map[v][0]=4;  nBlock[apre->b].map[v][1]=5; nBlock[apre->b].map[v][2]=3;
                    }
                    if((ii==3)&&(jj==2))
                    {
                      nBlock[apre->b].map[v][0]=5;  nBlock[apre->b].map[v][1]=1; nBlock[apre->b].map[v][2]=3;
                    }
  
                    if((ii==0)&&(jj==3))
                    {
                      nBlock[apre->b].map[v][0]=5;  nBlock[apre->b].map[v][1]=1; nBlock[apre->b].map[v][2]=3;
                    }
                    if((ii==1)&&(jj==3))
                    {
                      nBlock[apre->b].map[v][0]=1;  nBlock[apre->b].map[v][1]=2; nBlock[apre->b].map[v][2]=3;
                    }
                    if((ii==2)&&(jj==3))
                    {
                      nBlock[apre->b].map[v][0]=2;  nBlock[apre->b].map[v][1]=4; nBlock[apre->b].map[v][2]=3;
                    }
                    if((ii==3)&&(jj==3))
                    {
                      nBlock[apre->b].map[v][0]=4;  nBlock[apre->b].map[v][1]=5; nBlock[apre->b].map[v][2]=3;
                    }
  
                    /* count the block-to-block interfaces for isaac */
                    apre->c++;
    
                    /* determine the amount of nodes in each direction of the surf */
                    if( surf[u].typ[3]=='c' )
                    {
                      imax=1;
                      for( l=0; l<lcmb[surf[u].l[3]].nl; l++ )
                      {
                        m=lcmb[surf[u].l[3]].l[l];
                        imax+=line[m].div;
                      }
                    }
                    else
                      imax=line[surf[u].l[3]].div+1;
  
                    if( surf[u].typ[0]=='c' )
                    {
                      jmax=1;
                      for( l=0; l<lcmb[surf[u].l[0]].nl; l++ )
                      {
                        m=lcmb[surf[u].l[0]].l[l];
                        jmax+=line[m].div;
                      }
                    }
                    else
                      jmax=line[surf[u].l[0]].div+1;
  
                    if(jj==0)
                    {
                      nBlock[apre->b].strt2[v][0]=1;    nBlock[apre->b].strt2[v][1]=jmax; nBlock[apre->b].strt2[v][2]=1; 
                      nBlock[apre->b].end_2[v][0]=1;    nBlock[apre->b].end_2[v][1]=1;    nBlock[apre->b].end_2[v][2]=2; 
                    }
                    if(jj==1)
                    {
                      nBlock[apre->b].strt2[v][0]=imax; nBlock[apre->b].strt2[v][1]=jmax; nBlock[apre->b].strt2[v][2]=1; 
                      nBlock[apre->b].end_2[v][0]=1;    nBlock[apre->b].end_2[v][1]=jmax; nBlock[apre->b].end_2[v][2]=2; 
                    }
                    if(jj==2)
                    {
                      nBlock[apre->b].strt2[v][0]=imax; nBlock[apre->b].strt2[v][1]=1;    nBlock[apre->b].strt2[v][2]=1; 
                      nBlock[apre->b].end_2[v][0]=imax; nBlock[apre->b].end_2[v][1]=jmax; nBlock[apre->b].end_2[v][2]=2; 
                    }
                    if(jj==3)
                    {
                      nBlock[apre->b].strt2[v][0]=1;    nBlock[apre->b].strt2[v][1]=1;    nBlock[apre->b].strt2[v][2]=1; 
                      nBlock[apre->b].end_2[v][0]=imax; nBlock[apre->b].end_2[v][1]=1;    nBlock[apre->b].end_2[v][2]=2; 
                    }
  
                   }
                   else
                   {
                     printf("ERROR surface orientation does not match, all surfs must be defined math-positive\n");
                   }
                  }
    	        }
      	      }
  
              if(surFlag)
              {
                /* found a free surface */
                nBlock[apre->b].neighbor[v]=anz_cfdSurfs+1;
                nBlock[apre->b].bcface[v]=ii;
                anz_cfdSurfs++;
  
                /* i==nBlock[].strt1[][0], j==nBlock[].strt1[][1], imin==1, imax==vmax, jmin==1, jmax==umax */
                /* i,j,k must be ascending, therefore they must be newly written! */
                if(ii==0)
                {
                  nBlock[apre->b].strt1[v][0]=1;    nBlock[apre->b].strt1[v][1]=1;    nBlock[apre->b].strt1[v][2]=1; 
                  nBlock[apre->b].end_1[v][0]=1;    nBlock[apre->b].end_1[v][1]=umax; nBlock[apre->b].end_1[v][2]=2; 
                }
                if(ii==1)
                {
                  nBlock[apre->b].strt1[v][0]=1;    nBlock[apre->b].strt1[v][1]=umax; nBlock[apre->b].strt1[v][2]=1; 
                  nBlock[apre->b].end_1[v][0]=vmax; nBlock[apre->b].end_1[v][1]=umax; nBlock[apre->b].end_1[v][2]=2; 
                }
                if(ii==2)
                {
                  /*
                  nBlock[apre->b].strt1[v][0]=vmax; nBlock[apre->b].strt1[v][1]=umax; nBlock[apre->b].strt1[v][2]=1; 
                  nBlock[apre->b].end_1[v][0]=vmax; nBlock[apre->b].end_1[v][1]=1;    nBlock[apre->b].end_1[v][2]=2; 
                  */
                  nBlock[apre->b].strt1[v][0]=vmax; nBlock[apre->b].strt1[v][1]=1;    nBlock[apre->b].strt1[v][2]=1; 
                  nBlock[apre->b].end_1[v][0]=vmax; nBlock[apre->b].end_1[v][1]=umax; nBlock[apre->b].end_1[v][2]=2; 
                }
                if(ii==3)
                {
                  /*
                  nBlock[apre->b].strt1[v][0]=vmax; nBlock[apre->b].strt1[v][1]=1;    nBlock[apre->b].strt1[v][2]=1; 
                  nBlock[apre->b].end_1[v][0]=1;    nBlock[apre->b].end_1[v][1]=1;    nBlock[apre->b].end_1[v][2]=2; 
                  */
                  nBlock[apre->b].strt1[v][0]=1;    nBlock[apre->b].strt1[v][1]=1;    nBlock[apre->b].strt1[v][2]=1; 
                  nBlock[apre->b].end_1[v][0]=vmax; nBlock[apre->b].end_1[v][1]=1;    nBlock[apre->b].end_1[v][2]=2; 
                }
    	      }
    	    }
  
            for (u=0; u<=div_l[0]; u++)
            {
              for (v=0; v<=div_l[3]; v++)
              {
  //NOTHREAD if(sem_wait(&sem[10])) error("sem_wait");
                nod( anz, &node, 0, anz->nmax+1, npre[n_uv[u*vmax+v]].nx, npre[n_uv[u*vmax+v]].ny, npre[n_uv[u*vmax+v]].nz, 0 );
                anz_nmax=anz->nmax;
  //NOTHREAD if(sem_post(&sem[10])) error("sem_post");
                if(nbuf[n_uv[u*vmax+v]][0]>0)
    	        {
                  if((nbuf[n_uv[u*vmax+v]] = (int *)realloc((int *)nbuf[n_uv[u*vmax+v]], (nbuf[n_uv[u*vmax+v]][0]+2)*sizeof(int)))==NULL )
                  { printf(" ERROR: realloc failure in meshSurf, nodes not installed\n\n"); thread_param->returnVal=-1; return(&thread_param->returnVal); }
    	        }
                nbuf[n_uv[u*vmax+v]][0]++; nbuf[n_uv[u*vmax+v]][nbuf[n_uv[u*vmax+v]][0]]=anz_nmax;
    	      }
            }
  
            apre->b++;
          }
          else
  	  {
            printf("ERROR: surface:%s must be positive oriented but is negative oriented.\nAlso all surfaces must have a unique orientation\n", surf[j].name);
          }
  //NOTHREAD if(sem_post(&sem[9])) error("sem_post");
	}
        /* end cfd */


        for (u=0; u<div_l[0]; u++)
        {
          for (v=0; v<div_l[3]; v++)
          {
            if (surf[j].ori=='+')
            {
              ebuf[0]=n_uv[(u  )*vmax + v    ];
              ebuf[1]=n_uv[(u+1)*vmax + v    ];
              ebuf[2]=n_uv[(u+1)*vmax + (v+1)];
              ebuf[3]=n_uv[(u  )*vmax + (v+1)];
  //NOTHREAD if(sem_wait(&sem[8])) error("sem_wait");
              elem_define( anz->emax+1, 9, ebuf, 0, surf[j].eattr );
  //NOTHREAD if(sem_post(&sem[8])) error("sem_post");
              surf[j].elem[k]=anz->emax;
            }
            else
	    {
              ebuf[0]=n_uv[(u  )*vmax + v    ];
              ebuf[1]=n_uv[(u  )*vmax + (v+1)];
              ebuf[2]=n_uv[(u+1)*vmax + (v+1)];
              ebuf[3]=n_uv[(u+1)*vmax + v    ];
  //NOTHREAD if(sem_wait(&sem[8])) error("sem_wait");
              elem_define( anz->emax+1, 9, ebuf, 0, surf[j].eattr );
  //NOTHREAD if(sem_post(&sem[8])) error("sem_post");
              surf[j].elem[k]=anz->emax;
            }
            k++;
          }
        }
      }
      else if (surf[j].etyp==10)
      {
#if TEST2
  printf("hallo etyp:%d\n",jbuf);
#endif
        /* allocate memory for embeded elements */
        if((surf[j].elem=(int *)realloc((int *)surf[j].elem, (div_l[0]*div_l[3]/4)*sizeof(int)) )==NULL)
        { printf(" ERROR9: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
          thread_param->returnVal=-1; return(&thread_param->returnVal); }
        for (u=0; u<div_l[0]; u+=2)
        {
          for (v=0; v<div_l[3]; v+=2)
          {
            if (surf[j].ori=='+')
            {
              ebuf[0]=n_uv[(u  )*vmax + v    ];
              ebuf[4]=n_uv[(u+1)*vmax +  v   ];
              ebuf[1]=n_uv[(u+2)*vmax +  v   ];
              ebuf[5]=n_uv[(u+2)*vmax + (v+1)];
              ebuf[2]=n_uv[(u+2)*vmax + (v+2)];
              ebuf[6]=n_uv[(u+1)*vmax + (v+2)];
              ebuf[3]=n_uv[(u)*vmax   + (v+2)];
              ebuf[7]=n_uv[(u  )*vmax + (v+1)];
  //NOTHREAD if(sem_wait(&sem[8])) error("sem_wait");
              elem_define( anz->emax+1, 10, ebuf, 0, surf[j].eattr );
  //NOTHREAD if(sem_post(&sem[8])) error("sem_post");
              surf[j].elem[k]=anz->emax;
            }
            else
	    {
              ebuf[0]=n_uv[(u  )*vmax + v    ];
              ebuf[4]=n_uv[(u  )*vmax + (v+1)];
              ebuf[1]=n_uv[(u)*vmax   + (v+2)];
              ebuf[5]=n_uv[(u+1)*vmax + (v+2)];
              ebuf[2]=n_uv[(u+2)*vmax + (v+2)];
              ebuf[6]=n_uv[(u+2)*vmax + (v+1)];
              ebuf[3]=n_uv[(u+2)*vmax +  v   ];
              ebuf[7]=n_uv[(u+1)*vmax +  v   ];
  //NOTHREAD if(sem_wait(&sem[8])) error("sem_wait");
              elem_define( anz->emax+1, 10, ebuf, 0, surf[j].eattr );
  //NOTHREAD if(sem_post(&sem[8])) error("sem_post");
              surf[j].elem[k]=anz->emax;
            }
            k++;
          }
        }
      }
      
      surf[j].ne=k;

      if(( nurbsflag )&&( surf[j].sh>-1 )) 
      {
        if(shape[surf[j].sh].type==4) projSurfToNurbs( nurbs, shape[surf[j].sh].p[0], surf, j, npre );
      }

      free(n_uv);
      free(x);
      free(y);
      free(z);
    } /* end check of divisions */


#if TEST2
  printf("hallo if a substitute surf was meshed then map the mesh onto the original one:%d\n",jbuf);
#endif
    /* if a substitute surf was meshed then map the mesh onto the original one */
    if(mapflag)
    {
      s=jbuf;

      if(surf[j].ne>0)
      {
        if((surf[s].elem=(int *)realloc((int *)surf[s].elem, (surf[j].ne)*sizeof(int)) )==NULL)
        { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed, surf->elem (5)\n\n", surf[s].name); thread_param->returnVal=-1; return(&thread_param->returnVal); }
        for(k=0; k<surf[j].ne; k++) surf[s].elem[k]=surf[j].elem[k];
        surf[s].ne=surf[j].ne;
      }
      if(surf[j].nn>0)
      {
        if((surf[s].nod=(int *)realloc((int *)surf[s].nod, (surf[j].nn)*sizeof(int)) )==NULL)
        { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed, surf->nod (2)\n\n", surf[s].name); thread_param->returnVal=-1; return(&thread_param->returnVal); }
        for(k=0; k<surf[j].nn; k++) surf[s].nod[k]=surf[j].nod[k];
        surf[s].nn=surf[j].nn;
      }
      if(surf[j].npgn>0)
      {
        if((surf[s].pgn=(GLdouble *)realloc((GLdouble *)surf[s].pgn, (surf[j].npgn)*sizeof(GLdouble)) )==NULL)
        { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed, surf->nod (2)\n\n", surf[s].name); thread_param->returnVal=-1; return(&thread_param->returnVal); }
        for(k=0; k<surf[j].npgn; k++) surf[s].pgn[k]=surf[j].pgn[k];
        surf[s].npgn=surf[j].npgn;
      }
      surf[s].fail=0;
    }

    j=jbuf;
    if((surf[j].etyp==7)&&(surf[j].ne>0)&&((mapflag)||(transitionflag)))
    {
      /* check the orientation of the first element which matches the first line of the surf */
      /* if it is inverse to the surface then change the orientation */
      /* of the surface temporarily */

      /* the first line in the surface definition defines the orientation */
      /* if the connected element is invers orientated then all elements are inverted */

      /* get the first two nodes of the surface-lines */
#if TEST2
      printf("get nodes in surf:%s\n",surf[j].name);
#endif
      if(surf[j].typ[0]=='l')
      {
#if TEST2
        printf("line:%s\n", line[surf[j].l[0]].name);
#endif
        if(surf[j].o[0]=='+')
        {
          nori[0]= point[line[surf[j].l[0]].p1].nod[0];
          if(line[surf[j].l[0]].nn>0) nori[1]= line[surf[j].l[0]].nod[0];
          else nori[1]= point[line[surf[j].l[0]].p2].nod[0];
        }
        else
        {
          nori[1]= point[line[surf[j].l[0]].p1].nod[0];
          if(line[surf[j].l[0]].nn>0) nori[0]= line[surf[j].l[0]].nod[0];
          else nori[0]= point[line[surf[j].l[0]].p2].nod[0];
        }
      }
      else
      {
#if TEST2
        printf("lcmb:%s\n", lcmb[surf[j].l[0]].name);
#endif
        if(lcmb[surf[j].l[0]].o[0]=='+')
        {
          nori[0]= point[line[lcmb[surf[j].l[0]].l[0]].p1].nod[0];
          if(line[lcmb[surf[j].l[0]].l[0]].nn>0) nori[1]= line[lcmb[surf[j].l[0]].l[0]].nod[0];
          else nori[1]= point[line[lcmb[surf[j].l[0]].l[0]].p2].nod[0];
        }
        else
        {
          nori[1]= point[line[lcmb[surf[j].l[0]].l[0]].p1].nod[0];
          if(line[lcmb[surf[j].l[0]].l[0]].nn>0) nori[0]= line[lcmb[surf[j].l[0]].l[0]].nod[0];
          else nori[0]= point[line[lcmb[surf[j].l[0]].l[0]].p2].nod[0];
        }
        if(surf[j].o[0]=='-') { k=nori[0]; nori[0]=nori[1]; nori[1]=k; }
      }
#if TEST2
      printf("nodes are :%d %d\n", nori[0],nori[1]);
#endif
 
      /* go over all elements and search the associated one */
      for(k=0; k<surf[j].ne; k++)
      {
#if TEST2
        printf("check el:%d from:%d\n",surf[j].elem[k], surf[j].ne); 
#endif
        a=0;
        if      (e_enqire[surf[j].elem[k]].type == 1) ipuf = 8;   /* HEXA8  */
        else if (e_enqire[surf[j].elem[k]].type == 2) ipuf = 6;   /* PE6   */
        else if (e_enqire[surf[j].elem[k]].type == 3) ipuf = 4;   /* TET4   */
        else if (e_enqire[surf[j].elem[k]].type == 4) ipuf = 20;  /* HEXA20 */
        else if (e_enqire[surf[j].elem[k]].type == 5) ipuf = 15;  /* PE15  */
        else if (e_enqire[surf[j].elem[k]].type == 6) ipuf = 10;  /* TET10  */
        else if (e_enqire[surf[j].elem[k]].type == 7) ipuf = 3;   /* TRI3   */
        else if (e_enqire[surf[j].elem[k]].type == 8) ipuf = 6;   /* TRI6   */
        else if (e_enqire[surf[j].elem[k]].type == 9) ipuf = 4;   /* QUAD4  */
        else if (e_enqire[surf[j].elem[k]].type == 10) ipuf = 10; /* QUAD8  */
        else if (e_enqire[surf[j].elem[k]].type == 11) ipuf = 2;  /* BEAM2   */
        else if (e_enqire[surf[j].elem[k]].type == 12) ipuf = 3;  /* BEAM3   */

	/* create an array with the last node in front of all nodes */
	ebuf[0]=e_enqire[surf[j].elem[k]].nod[ipuf-1];
        for(ii=0; ii<ipuf; ii++) ebuf[ii+1]=e_enqire[surf[j].elem[k]].nod[ii];

        for(ii=0; ii<=ipuf; ii++)
        {
#if TEST2
          printf("nod:%d\n", ebuf[ii]);
#endif
          for(n=0; n<2; n++)
          {
            if(nori[n]==ebuf[ii])
            {
              if(!a)
	      {
                b=ii;
                eori[a++]=n;
#if TEST2
                printf("a:%d found n:%d nod:%d\n",a,n,nori[n]);
#endif
	      }
	      else if((a)&&(n!=eori[0])&&(b+1==ii))
	      {
                eori[a++]=n;
#if TEST2
                printf("a:%d found n:%d nod:%d\n",a,n,nori[n]);
#endif
	      }
              else /* start again */
	      {
                b=ii;
                eori[0]=n;
#if TEST2
                printf("a:%d found n:%d nod:%d\n",a,n,nori[n]);
#endif
	      }
            }
            if(a==2) goto found_reference;
          }
        }
      }
      printf(" ERROR: could not check orientation for surf:%s\n",surf[j].name);
      exit(-1);
      goto nextSurf;
    found_reference:;

      /* if the elements have to be reoriented */
      if(((eori[0]==1)&&(surf[j].ori=='+'))||((eori[0]==0)&&(surf[j].ori=='-')))
      {
#if TEST2
        printf("rearrange:%s\n",surf[j].name); 
#endif
        for(k=0; k<surf[j].ne; k++)
        {
          for(ii=0; ii<ipuf; ii++) ebuf[ii]=e_enqire[surf[j].elem[k]].nod[ii];
          n=ii;
          for(ii=0; ii<ipuf; ii++)
          {
            e_enqire[surf[j].elem[k]].nod[ii]=ebuf[--n];
          }
        }
      }
    }

  checkSurf:;
#if TEST2
  printf(" if the interiour of the surf was filled restore the original surface def:%d\n",jbuf);
#endif
    /* if the interiour of the surf was filled restore the original surface def */
    if(fillSurfFlag)
    {
      if(surfbuf[0].ne>0)
      {
        if((surf[j].elem=(int *)realloc((int *)surf[j].elem, (surfbuf[0].ne)*sizeof(int)) )==NULL)
        { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed, surf->elem (5)\n\n", surf[j].name); thread_param->returnVal=-1; return(&thread_param->returnVal); }
        for(k=0; k<surfbuf[0].ne; k++) surf[j].elem[k]=surfbuf[0].elem[k];
        surf[j].ne=surfbuf[0].ne;
        free(surfbuf[0].elem);
      }
      if(surfbuf[0].nn>0)
      {
        if((surf[j].nod=(int *)realloc((int *)surf[j].nod, (surfbuf[0].nn)*sizeof(int)) )==NULL)
        { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed, surf->nod (2)\n\n", surf[j].name); thread_param->returnVal=-1; return(&thread_param->returnVal); }
        for(k=0; k<surfbuf[0].nn; k++) surf[j].nod[k]=surfbuf[0].nod[k];
        surf[j].nn=surfbuf[0].nn;
        free(surfbuf[0].nod);
      }
    }

  nextSurf:;


  free(div_l);

  //NOTHREAD if(sem_wait(&sem[0])) error("sem_wait");
  countOpenThreads--; 
  //NOTHREAD if(sem_post(&sem[0])) error("sem_post");
#if TEST2
  printf("hallo finish s:%d countOpenThreads:%d\n", j, countOpenThreads);
#endif

  thread_param->returnVal=0; return(&thread_param->returnVal);
}


