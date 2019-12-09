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
TODO:
  sendSliders() and sendSurfNormalen(): still working on node basis instead of using the faces
*/



#include <cgx.h>

#define     TEST            0     /* debugging */

#define     DOFX        1
#define     DOFY        2
#define     DOFZ        3
#define     DOFT        11
#define     DOFP        8
#define     MIN_VECTOR  0.0001

extern char  datin[MAX_LINE_LENGTH];         /* Input-data-file */

extern int       neqn;                 /* offset der equations fuer ansys, bzw. MPC ID fuer Nast. */

extern int   basCol[3];                     /* color indexes due to basic colormap */
extern int   width_ini, height_ini; /* Grafig-Fensterbreite/hoehe */
extern int   width_menu, height_menu;
extern int   w0, w1, w_index, w_rgb, activWindow;
extern int   width_w0, height_w0;
extern int   width_w1, height_w1;
extern int   MouseMode;                                   /* status maustasten */
extern double dtx, dty, drx, dry, drz, ds;                 /* Verschiebungen */
extern GLdouble R[4][4];                                   /* Rotationsmatrix */
extern char  surfFlag;                /* zeichne nur Oberflaechenelemente (1), sonst (0)*/
extern char  modelEdgeFlag;                /* zeichne mit Modell-Ecken (1), sonst (0)*/
extern char  frameFlag;               /* mit (1) oder ohne Rahmen um das Grafigfenster */
extern double dx ,dy;                                      /* Mauskoordinaten */
extern int   steps;                   /* Schrittweite der Farbscala, und NURB aufloesung */

extern Scale     scale[1];
extern Summen    anz[1];
extern Edges     *edge;
extern Nodes     *node;
extern Elements  *e_enqire;
extern Datasets *lcase;
extern Faces     *face;

extern Alias     *alias;
extern Sets      *set;
extern Points    *point;
extern Lines     *line;
extern Lcmb      *lcmb;
extern Gsur      *surf;
extern Gbod      *body;
extern Nurbs     *nurbs;
extern SumGeo    anzGeo[1];

extern int       offset;

/* for CFD-meshing */
extern int       anz_cfdSurfs;


/* Set Management */
extern char  printFlag;                     /* printf 1:on 0:off */

/* the copied node-sets which have to be filled with values from new loaded Datasets */
extern CopiedNodeSets copiedNodeSets[1];


/* search all surfaces who are fully described by the selected nodes and add them to the set */
int compSurfElems( int setNr, Elements **ptr)
{
  int      i, j, k, n=0, elem2nr=0, anz_n;
  int      *surnod=NULL;
  Elements *elem2;

  elem2=*ptr;

  /* mark the surface nodes for easy element identification */
  if( (surnod=(int *)realloc((int *)surnod, (anz->nmax+1)*sizeof(int) ) )==NULL) 
  { printf(" ERROR: malloc failure\n"); return(0); }
  for (i=0; i<=anz->nmax; i++) surnod[i]=0;
  for (i=0; i<set[setNr].anz_n; i++) surnod[set[setNr].node[i]]=1;


  for(i=0; i<anz->f; i++)
  {
    anz_n=n=0;
    if (face[i].type==7) anz_n=3;
    if (face[i].type==8) anz_n=6;
    if (face[i].type==9) anz_n=4;
    if (face[i].type==10) anz_n=8;

    if(anz_n)
    {
      for (k=0; k<anz_n; k++) if (surnod[face[i].nod[k]]) n++;
      if (n==anz_n)
      {
        if ( (elem2 = (Elements *)realloc((Elements *)elem2, (elem2nr+1) * sizeof(Elements))) == NULL )
        { printf("\n\nERROR: malloc failed in compSurfElems\n\n"); return(0); }
        seta( setNr, "e", face[i].elem_nr);
        elem2[elem2nr].nr      = elem2nr+1+anz->emax;
        elem2[elem2nr].type    = face[i].type;
        elem2[elem2nr].group   = face[i].nr;

        for (j=0; j<anz_n; j++)
        {
          elem2[elem2nr].nod[j]=face[i].nod[j];
        }
        elem2nr++;
      }
      else if(n>anz_n)
      {
        printf("ERROR: to much corners found on elem:%d\n", face[i].elem_nr);
      }
    }
  }


  *ptr=elem2;
  free(surnod);
  return( elem2nr );
}


typedef struct {
  int nod[4];
  double ncol[4][3];
  int elem_nr;
  int group;
  int mat;
} CQuad4;

typedef struct {
  double side[48][3];                 /* side[Nr.][x|y|z] */
} Normalen;


void calcNormale_quad4(int i, int n1, int n2, int n3, int n4, int f, Nodes *node, Normalen *norm)
{
  static double v1[3], v2[3], v3[3];

      /* Vereinfachte Normale auf Flaeche des i.Elements*/
      v_result( &node[n1].nx, &node[n3].nx, v1);
      v_result( &node[n2].nx, &node[n4].nx, v2);
      v_prod( v1, v2, v3 );
      v_norm( v3, norm[i].side[f] );

}

void getCQuad4Normalen( CQuad4 *cquad4, Nodes *node, int *num_etype, Normalen *norm )
{
  int i;

  for (i=0; i<*num_etype; i++ )
  {
    calcNormale_quad4(i,cquad4[i].nod[0], cquad4[i].nod[1], cquad4[i].nod[2],
                  cquad4[i].nod[3], 0, node, norm);
  }
}


void sendSurfNormalen( char *setname, char *format, Summen *anz, Nodes *node, Elements *e_enqire,
               Datasets *lcase , Scale *scale )
{
  int  setNr, i, j, n=0, ipuf, elem2nr=0, comp, faces;
  char prognam[MAX_LINE_LENGTH];
  static int   *isort=NULL;
  Summen    anz2[1];
  static Nodes     *node2=NULL;
  static Elements  *elem2=NULL;
  static Datasets *lcase2=NULL;
  static CQuad4    *face4=NULL;
  static Normalen  *norm2=NULL;
  extern int compareInt();

  strcpy ( prognam, setname);
  setNr=getSetNr(setname);
  if (setNr<0)
  {
    printf (" ERROR: set:%s does not exist\n", setname);
    return;
  }

  /* search all element-faces who are fully described by the selected nodes and add them to the set */
  printf("Please wait\n");
  elem2nr=compSurfElems( setNr, &elem2);

  /* calculate the normals on the new faces */
  faces=0;
  if ( (face4 = (CQuad4 *)malloc( (elem2nr+1) * sizeof(CQuad4))) == NULL )
    printf("\n\nERROR: malloc failed\n\n") ;
  for (i=0; i<elem2nr; i++ )
  {
    if(elem2[i].type==9)
    {
      for (n=0; n<4; n++)
      {
       face4[faces].nod[n]=elem2[i].nod[n];
      }
      face4[faces].elem_nr = elem2[i].nr;
      face4[faces].group   = elem2[i].group;
      face4[faces].mat     = elem2[i].mat;
      faces++;
    }
  }

  if(faces>0)
  {
    anz2->l = 1;
    if ( (norm2 = (Normalen *)malloc( (faces+1)*4 * sizeof(Normalen))) == NULL )
      printf("\n\nERROR: malloc failed in sendSurfNormalen\n\n") ;
    getCQuad4Normalen( face4, node, &faces, norm2);

    /* send the surfs and normals in the frd format */
    if ( (lcase2 = (Datasets *)malloc( 2 * sizeof(Datasets))) == NULL )
      printf("\n\n ERROR: malloc failed lcase2\n\n") ;
    
    sprintf( lcase2[0].name,"NORM");
    comp=0;
    lcase2[0].ncomps=3;
    lcase2[0].irtype=3;
    lcase2[0].npheader=0;    
    if ( (lcase2[0].nmax = (int *)malloc(lcase2[0].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase2[0].nmin = (int *)malloc(lcase2[0].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase2[0].max = (float *)malloc(lcase2[0].ncomps * sizeof(float))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase2[0].min = (float *)malloc(lcase2[0].ncomps * sizeof(float))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase2[0].compName = (char **)malloc(lcase2[0].ncomps * sizeof(char *))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase2[0].icname = (char **)malloc(lcase2[0].ncomps * sizeof(char *))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase2[0].menu = (int *)malloc(lcase2[0].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase2[0].ictype = (int *)malloc(lcase2[0].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase2[0].icind1 = (int *)malloc(lcase2[0].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase2[0].icind2 = (int *)malloc(lcase2[0].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase2[0].iexist = (int *)malloc(lcase2[0].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    
    for(i=comp; i<lcase2[0].ncomps; i++)
    {
      if ( (lcase2[0].compName[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
        printf("\n\n ERROR: malloc failed\n\n" );
      if ( (lcase2[0].icname[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
        printf("\n\n ERROR: malloc failed\n\n" );
      lcase2[0].max[i]=-MAX_INTEGER;
      lcase2[0].min[i]=MAX_INTEGER;
    
      lcase2[0].menu[i] = 1;
      lcase2[0].ictype[i] = 2;
      lcase2[0].icind1[i] = i+1;
      lcase2[0].icind2[i] = 0;
      lcase2[0].iexist[i] = 0;
    }
    
    strcpy ( lcase2[0].compName[0], "x ");
    strcpy ( lcase2[0].compName[1], "y ");
    strcpy ( lcase2[0].compName[2], "z ");
    
    if ( (lcase2[0].edat = (float ***)malloc(  (lcase2[0].ncomps)*sizeof(float **))) == NULL )
      printf("\n\n ERROR: malloc failed esxx\n\n" );
    if ( (lcase2[0].edat[0] = (float **)malloc(  (elem2nr+anz->emax+1)*sizeof(float *))) == NULL )
      printf("\n\n ERROR: malloc failed esxx\n\n" );
    if ( (lcase2[0].edat[1] = (float **)malloc(  (elem2nr+anz->emax+1)*sizeof(float *))) == NULL )
      printf("\n\n ERROR: malloc failed esyy\n\n" );
    if ( (lcase2[0].edat[2] = (float **)malloc(  (elem2nr+anz->emax+1)*sizeof(float *))) == NULL )
      printf("\n\n ERROR: malloc failed eszz\n\n" );
    
    for (i=0; i<elem2nr; i++ )
    {
      for (j=0; j<lcase2[0].ncomps; j++)
      {
        if ( (lcase2[0].edat[j][elem2[i].nr] = (float *)malloc( (4) * sizeof(float))) == NULL )
            printf("\n\n ERROR: malloc failed esxx[%d]\n\n", i );
      }
    }

    for (i=0; i<faces; i++ )
    {
      for (j=0; j<4; j++)
      {
        lcase2[0].edat[0][face4[i].elem_nr][j]=norm2[i].side[0][0];
        lcase2[0].edat[1][face4[i].elem_nr][j]=norm2[i].side[0][1];
        lcase2[0].edat[2][face4[i].elem_nr][j]=norm2[i].side[0][2];
      }
    }
  }
  else anz2->l = 0;

  anz2->emax=-MAX_INTEGER;
  anz2->emin=MAX_INTEGER;
  for (i=0; i<elem2nr; i++)
  {
    if (anz2->emax<elem2[i].nr ) anz2->emax = elem2[i].nr;
    if (anz2->emin>elem2[i].nr ) anz2->emin = elem2[i].nr;
    ipuf=0;
    if (elem2[i].type == 1) ipuf = 8;  /* CHEXA8 */
    else if (elem2[i].type == 3) ipuf = 4;  /* CTET4 */
    else if (elem2[i].type == 4) ipuf = 20;  /* CHEXA20 */
    else if (elem2[i].type == 6) ipuf = 10;  /* CTET10 */
    else if (elem2[i].type == 7) ipuf = 3;  /* CTRI3  */
    else if (elem2[i].type == 8) ipuf = 6;  /* CTRI6  */
    else if (elem2[i].type == 9) ipuf = 4;  /* CQUAD4 */
    else if (elem2[i].type == 10) ipuf = 8;  /* CQUAD8 */
    else if (elem2[i].type == 11) ipuf = 2;  /* CBEAM */
    else if (elem2[i].type == 12) ipuf = 3;  /* CBEAM3 */
    else
    {
      if(printFlag) printf (" elem(%d) not a known type (%d)\n", elem2[i].nr, elem2[i].type);
    }
    for (j=0; j<ipuf; j++)
    {
      seta( setNr, "n", elem2[i].nod[j]);
    }
  }
  /* nodes must follow the elements (seta n above!) */
  /* sort the nodenumbers */
  if ( (node2 = (Nodes *)malloc( (anz->nmax+1) * sizeof(Nodes))) == NULL )
    printf("\n\nERROR: malloc failed\n\n") ;

  anz2->nmax = anz2->nmin = 0;

  if ( (isort = (int *)realloc( isort, (set[setNr].anz_n+1) * sizeof(int))) == NULL )
    printf("ERROR: realloc failed: isort\n\n" ); 
  for( i=0; i<set[setNr].anz_n; i++) isort[i]=set[setNr].node[i];
  qsort( isort, set[setNr].anz_n, sizeof(int), (void *)compareInt );

  for (i=0; i<set[setNr].anz_n; i++)
  {
    node2[i].nr = isort[i];
    node2[isort[i]].nx = node[isort[i]].nx;
    node2[isort[i]].ny = node[isort[i]].ny;
    node2[isort[i]].nz = node[isort[i]].nz;
    if (anz2->nmax<node2[i].nr) anz2->nmax = node2[i].nr;
    if (anz2->nmin>node2[i].nr) anz2->nmin = node2[i].nr;
  }
  strcpy( anz2->model, setname);
  anz2->n = set[setNr].anz_n;
  anz2->e = elem2nr;
  anz2->u = 0;
  anz2->p = 0;

  descalNodes ( set[setNr].anz_n, node2, scale);
  write2frd( "surface.frd", anz2, node2, elem2, lcase2, 0);
  scalNodes ( set[setNr].anz_n, node2, scale );

  /* free temporary memory */
  if(faces)
  {
    for (i=0; i<lcase2[0].ncomps; i++)
    {
      for(j=0; j<elem2nr; j++)
      {
          free(lcase2[0].edat[i][elem2[j].nr]);
          lcase2[0].edat[i][elem2[j].nr]=NULL;
      }
      free(lcase2[0].edat[i]); 
      lcase2[0].edat[i]=NULL;
    }
    free(lcase2);
    lcase2=NULL;
  }
  free(node2);
  free(face4);
  free(elem2);
  free(norm2);
  node2=NULL;
  face4=NULL;
  elem2=NULL;
  norm2=NULL;

  printf (" ready\n");
}



void getNodeNormalen(int **sum_n_ptr, Nodes **norm_ptr, int setNr, Summen *anz, Faces *face)
{
  int  i,f, n,k;
  int *sum_n=NULL;
  Nodes *norm=NULL;

  if ( (norm = (Nodes *)realloc( (Nodes *)norm, (anz->nmax+1) * sizeof(Nodes))) == NULL )
  {
    printf("WARNING: realloc failed in sendSet\n");
    return;
  }
  if ( (sum_n = (int *)realloc( (int *)sum_n, (anz->nmax+1) * sizeof(int))) == NULL )
  {
    printf("WARNING: realloc failed in sendSet\n");
    return;
  }

  /* ini all = 0 */
  for (n=0; n<=anz->nmax; n++ )
  {
    sum_n[n]=0;
    norm[n].nx=norm[n].ny=norm[n].nz=0.;
  }

  /* - add the normals per node */
  for (k=0; k<set[setNr].anz_f; k++ )
  {
    f=set[setNr].face[k];
    switch(face[f].type)
    {
  	case 7:
        for(n=0; n<3; n++)
	  {
	    norm[face[f].nod[n]].nx+=face[f].side[0][0];
	    norm[face[f].nod[n]].ny+=face[f].side[0][1];
	    norm[face[f].nod[n]].nz+=face[f].side[0][2];
          sum_n[face[f].nod[n]]++;
	  }
      break;
  	case 8:
        i=0;
	    norm[face[f].nod[i]].nx+=face[f].side[0][0];
	    norm[face[f].nod[i]].ny+=face[f].side[0][1];
	    norm[face[f].nod[i]].nz+=face[f].side[0][2];
          sum_n[face[f].nod[i]]++;
        i=3;
	    norm[face[f].nod[i]].nx+=face[f].side[0][0];
	    norm[face[f].nod[i]].ny+=face[f].side[0][1];
	    norm[face[f].nod[i]].nz+=face[f].side[0][2];
          sum_n[face[f].nod[i]]++;
        i=5;
	    norm[face[f].nod[i]].nx+=face[f].side[0][0];
	    norm[face[f].nod[i]].ny+=face[f].side[0][1];
	    norm[face[f].nod[i]].nz+=face[f].side[0][2];
          sum_n[face[f].nod[i]]++;
        i=2;
	    norm[face[f].nod[i]].nx+=face[f].side[1][0];
	    norm[face[f].nod[i]].ny+=face[f].side[1][1];
	    norm[face[f].nod[i]].nz+=face[f].side[1][2];
          sum_n[face[f].nod[i]]++;
        i=5;
	    norm[face[f].nod[i]].nx+=face[f].side[1][0];
	    norm[face[f].nod[i]].ny+=face[f].side[1][1];
	    norm[face[f].nod[i]].nz+=face[f].side[1][2];
          sum_n[face[f].nod[i]]++;
        i=4;
	    norm[face[f].nod[i]].nx+=face[f].side[1][0];
	    norm[face[f].nod[i]].ny+=face[f].side[1][1];
	    norm[face[f].nod[i]].nz+=face[f].side[1][2];
          sum_n[face[f].nod[i]]++;
        i=4;
	    norm[face[f].nod[i]].nx+=face[f].side[2][0];
	    norm[face[f].nod[i]].ny+=face[f].side[2][1];
	    norm[face[f].nod[i]].nz+=face[f].side[2][2];
          sum_n[face[f].nod[i]]++;
        i=5;
	    norm[face[f].nod[i]].nx+=face[f].side[2][0];
	    norm[face[f].nod[i]].ny+=face[f].side[2][1];
	    norm[face[f].nod[i]].nz+=face[f].side[2][2];
          sum_n[face[f].nod[i]]++;
        i=3;
	    norm[face[f].nod[i]].nx+=face[f].side[2][0];
	    norm[face[f].nod[i]].ny+=face[f].side[2][1];
	    norm[face[f].nod[i]].nz+=face[f].side[2][2];
          sum_n[face[f].nod[i]]++;
        i=3;
	    norm[face[f].nod[i]].nx+=face[f].side[3][0];
	    norm[face[f].nod[i]].ny+=face[f].side[3][1];
	    norm[face[f].nod[i]].nz+=face[f].side[3][2];
          sum_n[face[f].nod[i]]++;
        i=1;
	    norm[face[f].nod[i]].nx+=face[f].side[3][0];
	    norm[face[f].nod[i]].ny+=face[f].side[3][1];
	    norm[face[f].nod[i]].nz+=face[f].side[3][2];
          sum_n[face[f].nod[i]]++;
        i=4;
	    norm[face[f].nod[i]].nx+=face[f].side[3][0];
	    norm[face[f].nod[i]].ny+=face[f].side[3][1];
	    norm[face[f].nod[i]].nz+=face[f].side[3][2];
          sum_n[face[f].nod[i]]++;
      break;
  	case 9:
        for(n=0; n<4; n++)
	  {
	    norm[face[f].nod[n]].nx+=face[f].side[0][0];
	    norm[face[f].nod[n]].ny+=face[f].side[0][1];
	    norm[face[f].nod[n]].nz+=face[f].side[0][2];
          sum_n[face[f].nod[n]]++;
	  }
      break;
  	case 10:
        i=0;
	    norm[face[f].nod[i]].nx+=face[f].side[0][0];
	    norm[face[f].nod[i]].ny+=face[f].side[0][1];
	    norm[face[f].nod[i]].nz+=face[f].side[0][2];
          sum_n[face[f].nod[i]]++;
        i=4;
	    norm[face[f].nod[i]].nx+=face[f].side[0][0];
	    norm[face[f].nod[i]].ny+=face[f].side[0][1];
	    norm[face[f].nod[i]].nz+=face[f].side[0][2];
          sum_n[face[f].nod[i]]++;
        i=4;
	    norm[face[f].nod[i]].nx+=face[f].side[1][0];
	    norm[face[f].nod[i]].ny+=face[f].side[1][1];
	    norm[face[f].nod[i]].nz+=face[f].side[1][2];
          sum_n[face[f].nod[i]]++;
        i=1;
	    norm[face[f].nod[i]].nx+=face[f].side[1][0];
	    norm[face[f].nod[i]].ny+=face[f].side[1][1];
	    norm[face[f].nod[i]].nz+=face[f].side[1][2];
          sum_n[face[f].nod[i]]++;
        i=1;
	    norm[face[f].nod[i]].nx+=face[f].side[2][0];
	    norm[face[f].nod[i]].ny+=face[f].side[2][1];
	    norm[face[f].nod[i]].nz+=face[f].side[2][2];
          sum_n[face[f].nod[i]]++;
        i=5;
	    norm[face[f].nod[i]].nx+=face[f].side[2][0];
	    norm[face[f].nod[i]].ny+=face[f].side[2][1];
	    norm[face[f].nod[i]].nz+=face[f].side[2][2];
          sum_n[face[f].nod[i]]++;
        i=5;
	    norm[face[f].nod[i]].nx+=face[f].side[3][0];
	    norm[face[f].nod[i]].ny+=face[f].side[3][1];
	    norm[face[f].nod[i]].nz+=face[f].side[3][2];
          sum_n[face[f].nod[i]]++;
        i=2;
	    norm[face[f].nod[i]].nx+=face[f].side[3][0];
	    norm[face[f].nod[i]].ny+=face[f].side[3][1];
	    norm[face[f].nod[i]].nz+=face[f].side[3][2];
          sum_n[face[f].nod[i]]++;
        i=2;
	    norm[face[f].nod[i]].nx+=face[f].side[4][0];
	    norm[face[f].nod[i]].ny+=face[f].side[4][1];
	    norm[face[f].nod[i]].nz+=face[f].side[4][2];
          sum_n[face[f].nod[i]]++;
        i=6;
	    norm[face[f].nod[i]].nx+=face[f].side[4][0];
	    norm[face[f].nod[i]].ny+=face[f].side[4][1];
	    norm[face[f].nod[i]].nz+=face[f].side[4][2];
          sum_n[face[f].nod[i]]++;
        i=6;
	    norm[face[f].nod[i]].nx+=face[f].side[5][0];
	    norm[face[f].nod[i]].ny+=face[f].side[5][1];
	    norm[face[f].nod[i]].nz+=face[f].side[5][2];
          sum_n[face[f].nod[i]]++;
        i=3;
	    norm[face[f].nod[i]].nx+=face[f].side[5][0];
	    norm[face[f].nod[i]].ny+=face[f].side[5][1];
	    norm[face[f].nod[i]].nz+=face[f].side[5][2];
          sum_n[face[f].nod[i]]++;
        i=3;
	    norm[face[f].nod[i]].nx+=face[f].side[6][0];
	    norm[face[f].nod[i]].ny+=face[f].side[6][1];
	    norm[face[f].nod[i]].nz+=face[f].side[6][2];
          sum_n[face[f].nod[i]]++;
        i=7;
	    norm[face[f].nod[i]].nx+=face[f].side[6][0];
	    norm[face[f].nod[i]].ny+=face[f].side[6][1];
	    norm[face[f].nod[i]].nz+=face[f].side[6][2];
          sum_n[face[f].nod[i]]++;
        i=7;
	    norm[face[f].nod[i]].nx+=face[f].side[7][0];
	    norm[face[f].nod[i]].ny+=face[f].side[7][1];
	    norm[face[f].nod[i]].nz+=face[f].side[7][2];
          sum_n[face[f].nod[i]]++;
        i=0;
	    norm[face[f].nod[i]].nx+=face[f].side[7][0];
	    norm[face[f].nod[i]].ny+=face[f].side[7][1];
	    norm[face[f].nod[i]].nz+=face[f].side[7][2];
          sum_n[face[f].nod[i]]++;
      break;
    }
  }

  for (n=0; n<=anz->nmax; n++ )
  {
    if( sum_n[n] >0 )
    {
      v_norm( &norm[n].nx, &norm[n].nx );
      //printf("sum:%d node:%d norm: %lf %lf %lf l:%lf \n", sum_n[n], n, norm[n].nx, norm[n].ny, norm[n].nz, sqrt(norm[n].nx*norm[n].nx+norm[n].ny*norm[n].ny+norm[n].nz*norm[n].nz));
    }
  }

  *sum_n_ptr = sum_n;
  *norm_ptr  = norm;
}



void sendSlidersf( char *setname, char *format, char *spcType )
{
  int   setNr;
  int  length, k,f,n;
  char prognam[MAX_LINE_LENGTH];

  double xx,yy,zz;
  Nodes *norm;           // stores the face normale


  FILE *handle;


  strcpy ( prognam, setname);
  length= strlen ( setname );
  setNr=getSetNr(setname);
  if (setNr<0)
  {
    printf (" ERROR: set:%s does not exist\n", setname);
    return;
  }
  strcpy(&prognam[length],".equf");

  /* ---- sliders ----- */
  if(!set[setNr].anz_f)
  {
    printf(" WARNING: Found no faces in set:%s\n",set[setNr].name); 
    return;
  }

  if( compare(spcType, "s", 1) == 1)
  {
    /* faces slide on their element surfaces */

    /* calculate the average normal on every node */
    // known, just average over all triangles of the face if necessary. face[f].side[0-7][0-2];
    if ( (norm = (Nodes *)malloc((set[setNr].anz_f+1) * sizeof(Nodes))) == NULL )
    {
      printf("WARNING: realloc failed in sendSet\n");
      return;
    }
    for (k=0; k<set[setNr].anz_f; k++ )
    {
      f=set[setNr].face[k];
      switch(face[f].type)
      {
  	case 7:
        for(n=0; n<3; n++)
	{
	  norm[k].nx=face[f].side[0][0];
	  norm[k].ny=face[f].side[0][1];
          norm[k].nz=face[f].side[0][2];
        }
        break;
  	case 8:
	  norm[k].nx=face[f].side[0][0];
	  norm[k].ny=face[f].side[0][1];
	  norm[k].nz=face[f].side[0][2];
	  norm[k].nx+=face[f].side[1][0];
	  norm[k].ny+=face[f].side[1][1];
	  norm[k].nz+=face[f].side[1][2];
	  norm[k].nx+=face[f].side[2][0];
	  norm[k].ny+=face[f].side[2][1];
	  norm[k].nz+=face[f].side[2][2];
	  norm[k].nx+=face[f].side[3][0];
	  norm[k].ny+=face[f].side[3][1];
	  norm[k].nz+=face[f].side[3][2];
        break;
  	case 9:
	  norm[k].nx=face[f].side[0][0];
	  norm[k].ny=face[f].side[0][1];
	  norm[k].nz=face[f].side[0][2];
        break;
  	case 10:
	  norm[k].nx=face[f].side[0][0];
	  norm[k].ny=face[f].side[0][1];
	  norm[k].nz=face[f].side[0][2];
	  norm[k].nx+=face[f].side[1][0];
	  norm[k].ny+=face[f].side[1][1];
	  norm[k].nz+=face[f].side[1][2];
	  norm[k].nx+=face[f].side[2][0];
	  norm[k].ny+=face[f].side[2][1];
	  norm[k].nz+=face[f].side[2][2];
	  norm[k].nx+=face[f].side[3][0];
	  norm[k].ny+=face[f].side[3][1];
	  norm[k].nz+=face[f].side[3][2];
	  norm[k].nx+=face[f].side[4][0];
	  norm[k].ny+=face[f].side[4][1];
	  norm[k].nz+=face[f].side[4][2];
	  norm[k].nx+=face[f].side[5][0];
	  norm[k].ny+=face[f].side[5][1];
	  norm[k].nz+=face[f].side[5][2];
	  norm[k].nx+=face[f].side[6][0];
	  norm[k].ny+=face[f].side[6][1];
	  norm[k].nz+=face[f].side[6][2];
	  norm[k].nx+=face[f].side[7][0];
	  norm[k].ny+=face[f].side[7][1];
	  norm[k].nz+=face[f].side[7][2];
        break;
      }
      v_norm( &norm[k].nx, &norm[k].nx );
    }
  }  
  else
  {
    errMsg ("\nERROR: format:%s not known\n", spcType);
    return;
  }  

  /* write the sliders in abaqus-format */
  if (compare( format, "abq", 3)== 3)
  {
    handle = fopen (prognam, "w");
    if ( handle== NULL )
    {
      printf ("\nThe input file %s could not be opened.\n\n", prognam); 
      return;
    }
    fprintf(handle, "** Sliders based on %s\n", setname);
    for (k=0; k<set[setNr].anz_f; k++ )
    {
      f=set[setNr].face[k];
      printf("elem %d face %d Norm:%lf %lf %lf\n"
               , face[f].elem_nr, face[f].nr+1
	       , norm[k].nx, norm[k].ny, norm[k].nz ); 

      xx=norm[k].nx*norm[k].nx;
      yy=norm[k].ny*norm[k].ny;
      zz=norm[k].nz*norm[k].nz;
      if((xx>MIN_VECTOR)
      &&(yy>MIN_VECTOR)&&(zz>MIN_VECTOR))
      {
        fprintf(handle, "*EQUATIONF\n");
        fprintf(handle, "%d\n", 3);
        fprintf(handle, "%d,S%d,%d,%lf, %d,S%d,%d,%lf, %d,S%d,%d,%lf \n"
          , face[f].elem_nr, face[f].nr+1, DOFX, norm[k].nx
          , face[f].elem_nr, face[f].nr+1, DOFY, norm[k].ny
	  , face[f].elem_nr, face[f].nr+1, DOFZ, norm[k].nz );
      }
      else if((xx<=MIN_VECTOR)
      &&(yy>MIN_VECTOR)&&(zz>MIN_VECTOR))
      {
        fprintf(handle, "*EQUATIONF\n");
        fprintf(handle, "%d\n", 2);
        fprintf(handle, "%d,S%d,%d,%lf, %d,S%d,%d,%lf \n"
        , face[f].elem_nr, face[f].nr+1, DOFY, norm[k].ny
	  , face[f].elem_nr, face[f].nr+1, DOFZ, norm[k].nz );
      }
      else if((yy<=MIN_VECTOR)
      &&(xx>MIN_VECTOR)&&(zz>MIN_VECTOR))
      {
        fprintf(handle, "*EQUATIONF\n");
        fprintf(handle, "%d\n", 2);
        fprintf(handle, "%d,S%d,%d,%lf, %d,S%d,%d,%lf \n"
        , face[f].elem_nr, face[f].nr+1, DOFX, norm[k].nx
	  , face[f].elem_nr, face[f].nr+1, DOFZ, norm[k].nz );
      }
      else if((zz<=MIN_VECTOR)
      &&(xx>MIN_VECTOR)&&(yy>MIN_VECTOR))
      {
        fprintf(handle, "*EQUATIONF\n");
        fprintf(handle, "%d\n", 2);
        fprintf(handle, "%d,S%d,%d,%lf, %d,S%d,%d,%lf \n"
        , face[f].elem_nr, face[f].nr+1, DOFX, norm[k].nx
	  , face[f].elem_nr, face[f].nr+1, DOFY, norm[k].ny );
      }

      else if((xx>MIN_VECTOR)
      &&(yy<=MIN_VECTOR)&&(zz<=MIN_VECTOR))
      {
        fprintf(handle, "*BOUNDARYF\n");
        fprintf(handle, "%d,S%d,%d,,0. \n"
		, face[f].elem_nr, face[f].nr+1, DOFX );
      }
      else if((yy>MIN_VECTOR)
      &&(xx<=MIN_VECTOR)&&(zz<=MIN_VECTOR))
      {
        fprintf(handle, "*BOUNDARYF\n");
        fprintf(handle, "%d,S%d,%d,,0. \n"
        , face[f].elem_nr, face[f].nr+1, DOFY );
      }
      else if((zz>MIN_VECTOR)
      &&(xx<=MIN_VECTOR)&&(yy<=MIN_VECTOR))
      {
        fprintf(handle, "*BOUNDARYF\n");
        fprintf(handle, "%d,S%d,%d,,0. \n"
        , face[f].elem_nr, face[f].nr+1, DOFZ );
      }
      else printf("ERROR; no slider created for node:%d, S%d\n", face[f].elem_nr, face[f].nr+1);
    }
    fclose(handle);
  }
  else
  {
    errMsg(" ERROR: format %s not yet supported\n", format );
  }

  free(norm);

  printf (" ready\n");
}



void sendSliders( char *setname, char *format, Summen *anz, Nodes *node, Elements *e_enqire, char *spcType )
{
  int   setNr;
  int  length, i,n, anz_n;
  char prognam[MAX_LINE_LENGTH];

  double xx,yy,zz;
  Nodes *norm;
  int   *sum_n;


  int n0, n1, n2;
  double p0[3], p1[3], p2[3], p3[3], N[3], p0p1[3], p0p2[3];

  FILE *handle;


  strcpy ( prognam, setname);
  length= strlen ( setname );
  setNr=getSetNr(setname);
  if (setNr<0)
  {
    printf (" ERROR: set:%s does not exist\n", setname);
    return;
  }

  /* cycsym sliding-mpcs */
  /* the nodes slide on copies of them. This copy-nodes can be used for cycsym or spc etc. */
  /* the 1st face in the set using the 1st node is used to define the siding-plane for all nodes in the set */
  if( compare(spcType, "cyc", 1) == 1)
  {
    /* check if at least one face is in the set */
    if(set[setNr].anz_f<1)
    {
      printf(" ERROR: set:%s does not contain a face which defines the sliding plane.\n", setname); 
      return;
    }
    i=set[setNr].face[0];
    anz_n=n=0;
    if (face[i].type==7) anz_n=3;
    if (face[i].type==8) anz_n=6;
    if (face[i].type==9) anz_n=4;
    if (face[i].type==10) anz_n=8;
    
    if(anz_n)
    {
      n0=face[i].nod[0];
      n1=face[i].nod[1];
      n2=face[i].nod[2];
      if(printFlag) printf("n0:%d n1:%d n2:%d\n", n0, n1, n2);
      p0[0]=node[n0].nx*scale->w+scale->x;
      p0[1]=node[n0].ny*scale->w+scale->y;
      p0[2]=node[n0].nz*scale->w+scale->z;
      p1[0]=node[n1].nx*scale->w+scale->x;
      p1[1]=node[n1].ny*scale->w+scale->y;
      p1[2]=node[n1].nz*scale->w+scale->z;
      p2[0]=node[n2].nx*scale->w+scale->x;
      p2[1]=node[n2].ny*scale->w+scale->y;
      p2[2]=node[n2].nz*scale->w+scale->z;
      v_result( p0, p1, p0p1 );
      v_result( p0, p2, p0p2 );
      
      if (compare( format, "abq", 3)== 3)
      {
        sprintf(prognam, "%s.mpc", setname);
        handle = fopen (prognam, "w");
        if ( handle== NULL )
        {
          printf ("\nThe input file %s could not be opened.\n\n", prognam); 
          return;
        }
        fprintf(handle, "** Sliders based on %s\n", setname );

        /* copy nodes */
        fprintf (handle, "*NODE, NSET=N%s_COPY\n", setname );
        for (n=0; n<set[setNr].anz_n; n++)
        {
          i=set[setNr].node[n];
          fprintf( handle, "%8d,%.12e,%.12e,%.12e\n", anz->nmax+n+1, node[i].nx*scale->w+scale->x, node[i].ny*scale->w+scale->y, node[i].nz*scale->w+scale->z );
        }

        /* write name-set for the tramsformation into the sliding plane */
        fprintf (handle, "*NSET, NSET=N%s\n", setname );
        for (n=0; n<set[setNr].anz_n; n++)
        {
          i=set[setNr].node[n];
          fprintf( handle, "%8d,\n%8d,\n", i, anz->nmax+n+1 );
        }

        /* coordinate system for the sliding plane */
        fprintf(handle, "*TRANSFORM,NSET=N%s\n",set[setNr].name );
        fprintf(handle, "%lf, %lf, %lf, %lf, %lf, %lf\n", p0p1[0],p0p1[1],p0p1[2], p0p2[0],p0p2[1],p0p2[2] );

        /* write equations normal to the sliding plane */
        for (n=0; n<set[setNr].anz_n; n++)
        {
          i=set[setNr].node[n];
          fprintf(handle, "*EQUATION\n");
          fprintf(handle, "%d\n", 2);
          fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf \n", i, 3, 1., anz->nmax+n+1, 3, -1. );
        }

        fclose(handle);
      }
      return;
    }
    else
    {
      printf(" ERROR: face-type:%d unknown, no siding plane could be defined.\n", face[i].type); 
      return;
    }
  }

  /* ---- sliders ----- */
  if(!set[setNr].anz_f)
  {
    printf(" WARNING: Found no faces in set:%s\n",set[setNr].name); 
    return;
  }

  if( compare(spcType, "s", 1) == 1)
  {
    /* nodes slide on the element surfaces */

    /* calculate the average normal on every node */
    getNodeNormalen(&sum_n, &norm, setNr, anz, face);
  }  
  else if( compare(spcType, "rx", 2) == 2)
  {
    /* calculate the average normal on every node */
    p2[0]=1.;
    p2[1]=0.;
    p2[2]=0.;
    for (i=0; i<set[setNr].anz_n; i++)
    {
      n= set[setNr].node[i];
      sum_n[n]=1;
      p1[0]=node[n].nx* scale->w+scale->x;
      p1[1]=node[n].ny* scale->w+scale->y;
      p1[2]=node[n].nz* scale->w+scale->z;
      v_prod( p1, p2, p3);
      v_norm( p3, N ); 
      norm[n].nx=N[0];
      norm[n].ny=N[1];
      norm[n].nz=N[2];
    }
  }  
  else if( compare(spcType, "ry", 2) == 2)
  {
    /* calculate the average normal on every node */
    p2[0]=0.;
    p2[1]=1.;
    p2[2]=0.;
    for (i=0; i<set[setNr].anz_n; i++)
    {
      n= set[setNr].node[i];
      sum_n[n]=1;
      p1[0]=node[n].nx* scale->w+scale->x;
      p1[1]=node[n].ny* scale->w+scale->y;
      p1[2]=node[n].nz* scale->w+scale->z;
      v_prod( p1, p2, p3);
      v_norm( p3, N ); 
      norm[n].nx=N[0];
      norm[n].ny=N[1];
      norm[n].nz=N[2];
    }
  }  
  else if( compare(spcType, "rz", 2) == 2)
  {
    /* calculate the average normal on every node */
    p2[0]=0.;
    p2[1]=0.;
    p2[2]=1.;
    for (i=0; i<set[setNr].anz_n; i++)
    {
      n= set[setNr].node[i];
      sum_n[n]=1;
      p1[0]=node[n].nx* scale->w+scale->x;
      p1[1]=node[n].ny* scale->w+scale->y;
      p1[2]=node[n].nz* scale->w+scale->z;
      v_prod( p1, p2, p3);
      v_norm( p3, N ); 
      norm[n].nx=N[0];
      norm[n].ny=N[1];
      norm[n].nz=N[2];
    }
  }  
  else if( compare(spcType, "tx", 2) == 2)
  {
    /* calculate the average normal on every node */
    for (i=0; i<set[setNr].anz_n; i++)
    {
      n= set[setNr].node[i];
      sum_n[n]=1;
      norm[n].nx=0.;
      norm[n].ny=node[n].ny* scale->w+scale->y;
      norm[n].nz=node[n].nz* scale->w+scale->z;
    }
  }  
  else if( compare(spcType, "ty", 2) == 2)
  {
    /* calculate the average normal on every node */
    for (i=0; i<set[setNr].anz_n; i++)
    {
      n= set[setNr].node[i];
      sum_n[n]=1;
      norm[n].nx=node[n].nx* scale->w+scale->x;
      norm[n].ny=0.;
      norm[n].nz=node[n].nz* scale->w+scale->z;
    }
  }  
  else if( compare(spcType, "tz", 2) == 2)
  {
    /* calculate the average normal on every node */
    for (i=0; i<set[setNr].anz_n; i++)
    {
      n= set[setNr].node[i];
      sum_n[n]=1;
      norm[n].nx=node[n].nx* scale->w+scale->x;
      norm[n].ny=node[n].ny* scale->w+scale->y;
      norm[n].nz=0.;
    }
  }  
  else
  {
    errMsg ("\nERROR: format:%s not known\n", spcType);
    return;
  }  

  strcpy ( prognam, setname);
  length= strlen ( setname );
  strcpy (&prognam[length], ".equ");

  /* write the sliders in nastran-format */
  /* write the sliders in abaqus-format */
  if (compare( format, "abq", 3)== 3)
  {
    handle = fopen (prognam, "w");
    if ( handle== NULL )
    {
      printf ("\nThe input file %s could not be opened.\n\n", prognam); 
      return;
    }
    fprintf(handle, "** Sliders based on %s\n", setname);
    for (i=0; i<anz->n; i++ )
    {
      if( sum_n[node[i].nr] >0 )
      {
        printf("node %d Norm:%lf %lf %lf Koord:%lf %lf %lf\n"
               , node[i].nr
	       , norm[node[i].nr].nx, norm[node[i].nr].ny, norm[node[i].nr].nz
               ,node[node[i].nr].nx,node[node[i].nr].ny,node[node[i].nr].nz ); 

        xx=norm[node[i].nr].nx*norm[node[i].nr].nx;
        yy=norm[node[i].nr].ny*norm[node[i].nr].ny;
        zz=norm[node[i].nr].nz*norm[node[i].nr].nz;
        if((xx>MIN_VECTOR)&&(yy>MIN_VECTOR)
        &&(zz>MIN_VECTOR))
        {
          fprintf(handle, "*EQUATION\n");
          fprintf(handle, "%d\n", 3);
          fprintf(handle, "%d,%d,%lf, %d,%d,%lf, %d,%d,%lf \n"
          , node[i].nr, DOFX, norm[node[i].nr].nx
          , node[i].nr, DOFY, norm[node[i].nr].ny
	  , node[i].nr, DOFZ, norm[node[i].nr].nz );
        }
        else if((xx<=MIN_VECTOR)
        &&(yy>MIN_VECTOR)&&(zz>MIN_VECTOR))
        {
          fprintf(handle, "*EQUATION\n");
          fprintf(handle, "%d\n", 2);
          fprintf(handle, "%d,%d,%lf, %d,%d,%lf \n"
          , node[i].nr, DOFY, norm[node[i].nr].ny
	  , node[i].nr, DOFZ, norm[node[i].nr].nz );
        }
        else if((yy<=MIN_VECTOR)
        &&(xx>MIN_VECTOR)&&(zz>MIN_VECTOR))
        {
          fprintf(handle, "*EQUATION\n");
          fprintf(handle, "%d\n", 2);
          fprintf(handle, "%d,%d,%lf, %d,%d,%lf \n"
          , node[i].nr, DOFX, norm[node[i].nr].nx
	  , node[i].nr, DOFZ, norm[node[i].nr].nz );
        }
        else if((zz<=MIN_VECTOR)
        &&(xx>MIN_VECTOR)&&(yy>MIN_VECTOR))
        {
          fprintf(handle, "*EQUATION\n");
          fprintf(handle, "%d\n", 2);
          fprintf(handle, "%d,%d,%lf, %d,%d,%lf \n"
          , node[i].nr, DOFX, norm[node[i].nr].nx
	  , node[i].nr, DOFY, norm[node[i].nr].ny );
        }
        else if((xx>MIN_VECTOR)
        &&(yy<=MIN_VECTOR)&&(zz<=MIN_VECTOR))
        {
          fprintf(handle, "*BOUNDARY\n");
          fprintf(handle, "%d,%d \n"
          , node[i].nr, DOFX );
        }
        else if((yy>MIN_VECTOR)
        &&(xx<=MIN_VECTOR)&&(zz<=MIN_VECTOR))
        {
          fprintf(handle, "*BOUNDARY\n");
          fprintf(handle, "%d,%d \n"
          , node[i].nr, DOFY );
        }
        else if((zz>MIN_VECTOR)
        &&(xx<=MIN_VECTOR)&&(yy<=MIN_VECTOR))
        {
          fprintf(handle, "*BOUNDARY\n");
          fprintf(handle, "%d,%d \n"
          , node[i].nr, DOFZ );
        }
        else printf("ERROR; no slider created for node:%d\n", node[i].nr);
      }
    }
    fclose(handle);
  }
  /* write the sliders in ansys-format */
  else if (compare( format, "ans", 3)== 3)
  {
    handle = fopen (prognam, "w");
    if ( handle== NULL )
    {
      printf ("\nThe input file %s could not be opened.\n\n", prognam); 
      return;
    }
    fprintf(handle, "! Sliders based on %s\n", setname);
    for (i=0; i<anz->n; i++ )
    {
      if( sum_n[node[i].nr] >0 )
      {
        printf("node %d Norm:%lf %lf %lf Koord:%lf %lf %lf\n"
               , node[i].nr
	       , norm[node[i].nr].nx, norm[node[i].nr].ny, norm[node[i].nr].nz
               ,node[node[i].nr].nx,node[node[i].nr].ny,node[node[i].nr].nz ); 

        xx=norm[node[i].nr].nx*norm[node[i].nr].nx;
        yy=norm[node[i].nr].ny*norm[node[i].nr].ny;
        zz=norm[node[i].nr].nz*norm[node[i].nr].nz;
        if((xx>MIN_VECTOR)&&(yy>MIN_VECTOR)
        &&(zz>MIN_VECTOR))
        {
          neqn++;
          fprintf(handle, "CE,%d,0,%d,%s,%lf, %d,%s,%lf,  %d,%s,%lf\n", neqn
          , node[i].nr, "UX", norm[node[i].nr].nx 
          , node[i].nr, "UY", norm[node[i].nr].ny
          , node[i].nr, "UZ", norm[node[i].nr].nz );
        }
        else if((xx<=MIN_VECTOR)
        &&(yy>MIN_VECTOR)&&(zz>MIN_VECTOR))
        {
          neqn++;
          fprintf(handle, "CE,%d,0,%d,%s,%lf, %d,%s,%lf,  %d,%s,%lf\n", neqn
          , -node[i].nr, "UX", norm[node[i].nr].nx 
          , node[i].nr, "UY", norm[node[i].nr].ny
          , node[i].nr, "UZ", norm[node[i].nr].nz );
        }
        else if((yy<=MIN_VECTOR)
        &&(xx>MIN_VECTOR)&&(zz>MIN_VECTOR))
        {
          neqn++;
          fprintf(handle, "CE,%d,0,%d,%s,%lf, %d,%s,%lf,  %d,%s,%lf\n", neqn
          , node[i].nr, "UX", norm[node[i].nr].nx 
          , -node[i].nr, "UY", norm[node[i].nr].ny
          , node[i].nr, "UZ", norm[node[i].nr].nz );
        }
        else if((zz<=MIN_VECTOR)
        &&(xx>MIN_VECTOR)&&(yy>MIN_VECTOR))
        {
          neqn++;
          fprintf(handle, "CE,%d,0,%d,%s,%lf, %d,%s,%lf,  %d,%s,%lf\n", neqn
          , node[i].nr, "UX", norm[node[i].nr].nx 
          , node[i].nr, "UY", norm[node[i].nr].ny
          , -node[i].nr, "UZ", norm[node[i].nr].nz );
        }
        else if((xx>MIN_VECTOR)
        &&(yy<=MIN_VECTOR)&&(zz<=MIN_VECTOR))
        {
          fprintf(handle, "D, %d, UX, 0. \n"
          , node[i].nr );
        }
        else if((yy>MIN_VECTOR)
        &&(xx<=MIN_VECTOR)&&(zz<=MIN_VECTOR))
        {
          fprintf(handle, "D, %d, UY, 0. \n"
          , node[i].nr );
        }
        else if((zz>MIN_VECTOR)
        &&(xx<=MIN_VECTOR)&&(yy<=MIN_VECTOR))
        {
          fprintf(handle, "D, %d, UZ, 0. \n"
          , node[i].nr );
        }
        else printf("ERROR; no slider created for node:%d\n", node[i].nr);
      }
    }
    fclose(handle);
  }
  else
  {
    errMsg(" ERROR: format %s not yet supported\n", format );
  }

  free(norm);
  free(sum_n);

  printf (" ready\n");
}


void sendQuadLin( char *setname, char *format, Summen *anz, Nodes *nod, Elements *e_enqire )
{
  int  setNr, anz_n=0;
  int  i,j,e=0,k,n,ipuf=0;
  char prognam[MAX_LINE_LENGTH];
  FILE *handle=NULL;
  int  anz_nmax, enr=1;

  Nodes *node=NULL;

  /* 8*4 */
  int tet10tet[]={4,1,5,8, 5,2,6,9, 6,0,4,7, 7,8,9,3,  4,5,6,7, 4,5,7,8, 6,7,5,9, 8,9,5,7 };
  /* 8*6 incl midside nodes */
  int pe15pe[]=  {0,6,8,9,15,17, 9,15,17,3,12,14, 8,7,2,17,16,11, 17,16,11,14,13,5, 6,7,8,15,16,17, 15,16,17,12,13,14, 1,7,6,10,16,15, 10,16,15,4,13,12 }; 
  /* 8*8 incl midside and midvolume nodes */
  int hex20hex[]={0,8,24,11, 12,20,26,23,  11,24,10,3, 23,26,22,15,  8,1,9,24, 20,13,21,26,  24,9,2,10, 26,21,14,22,
		  12,20,26,23, 4,16,25,19,  23,26,22,15, 19,25,18,7,  20,13,21,26, 16,5,17,25,  26,21,14,22, 25,17,6,18 };

  static Rsort *rsort=NULL;

  static double *orig_x=NULL, *orig_y=NULL, *orig_z=NULL, *sort_x=NULL, *sort_y=NULL, *sort_z=NULL;
  static int *sort_nx=NULL, *sort_ny=NULL, *sort_nz=NULL, near_node[10];
  double  dr, dx, dy, dz,tol;

  double pcoords[3]={0.5,0.5,0.5}, weights[20];

  setNr=getSetNr(setname);
  if (setNr<0)
  {
    printf (" ERROR: set:%s does not exist\n", setname);
    return;
  }

  tol=0.00001;
  tol*=tol;

  enr=set[setNr].elem[0];

  sprintf(prognam,"%s_lin.msh",setname);
  printf (" write file: %s\n", prognam );

  /* copy all nodes */

  if ( (node = (Nodes *)realloc( (Nodes *)node, (anz->nmax+1) * sizeof(Nodes))) == NULL )
    printf("WARNING: realloc error\n");

  for (i=0; i<anz->n; i++)
  {
    node[i].nr=nod[i].nr;
    j=node[i].nr;
    node[j].indx=-1;
    node[j].pflag=0;
    node[j].nx=nod[j].nx;
    node[j].ny=nod[j].ny;
    node[j].nz=nod[j].nz;
  }
  descalNodes ( anz->n, node, scale);
  anz_nmax=anz->nmax;

  /* merge nodes */
  /* search the closest node */
  if ( (rsort = (Rsort *)malloc( (anz->n+1) * sizeof(Rsort))) == NULL )
    printf("ERROR: realloc failed: Rsort\n\n" ); 
  if ( (orig_x = (double *)malloc( (anz->n+1) * sizeof(double))) == NULL )
    printf("ERROR: realloc failed in areampc\n\n" ); 
  if ( (orig_y = (double *)malloc( (anz->n+1) * sizeof(double))) == NULL )
    printf("ERROR: realloc failed in areampc\n\n" ); 
  if ( (orig_z = (double *)malloc( (anz->n+1) * sizeof(double))) == NULL )
    printf("ERROR: realloc failed in areampc\n\n" ); 
  if ( (sort_x = (double *)malloc( (anz->n+1) * sizeof(double))) == NULL )
    printf("ERROR: realloc failed in areampc\n\n" ); 
  if ( (sort_y = (double *)malloc( (anz->n+1) * sizeof(double))) == NULL )
    printf("ERROR: realloc failed in areampc\n\n" ); 
  if ( (sort_z = (double *)malloc( (anz->n+1) * sizeof(double))) == NULL )
    printf("ERROR: realloc failed in areampc\n\n" ); 
  if ( (sort_nx = (int *)malloc( (anz->n+1) * sizeof(int))) == NULL )
    printf("ERROR: realloc failed in areampc\n\n" ); 
  if ( (sort_ny = (int *)malloc( (anz->n+1) * sizeof(int))) == NULL )
    printf("ERROR: realloc failed in areampc\n\n" ); 
  if ( (sort_nz = (int *)malloc( (anz->n+1) * sizeof(int))) == NULL )
    printf("ERROR: realloc failed in areampc\n\n" ); 
  for(i=0; i<anz->n; i++)
  {
    rsort[i].r=orig_x[i]=node[node[i].nr].nx;
    rsort[i].i=i;
  }
  qsort( rsort, anz->n, sizeof(Rsort), (void *)compareRsort );
  for(i=0; i<anz->n; i++)
  {
    sort_x[i]=rsort[i].r;
    sort_nx[i]=rsort[i].i;
  }
  for(i=0; i<anz->n; i++)
  {
    rsort[i].r=orig_y[i]=node[node[i].nr].ny;
    rsort[i].i=i;
  }
  qsort( rsort, anz->n, sizeof(Rsort), (void *)compareRsort );
  for(i=0; i<anz->n; i++)
  {
    sort_y[i]=rsort[i].r;
    sort_ny[i]=rsort[i].i;
  }
  for(i=0; i<anz->n; i++)
  {
    rsort[i].r=orig_z[i]=node[node[i].nr].nz;
    rsort[i].i=i;
  }
  qsort( rsort, anz->n, sizeof(Rsort), (void *)compareRsort );
  for(i=0; i<anz->n; i++)
  {
    sort_z[i]=rsort[i].r;
    sort_nz[i]=rsort[i].i;
  }

  /* aktivate nodes and generate midvolume nodes for hex20 */
  anz_n=anz->n;
  for (i=0; i<set[setNr].anz_e; i++ )
  {
    e=set[setNr].elem[i];
    switch(e_enqire[e].type)
    {
      case 1:
      ipuf = 8;   
      break;
      case 2:
      ipuf = 6; 
      break;
      case 3:
      ipuf = 4;   
      break;
      case 4:
      ipuf = 27;

        /* generate mid-volume node */
        anz_nmax++;
        if ( (node = (Nodes *)realloc( (Nodes *)node, (anz_nmax+1) * sizeof(Nodes))) == NULL )
          printf("\n\n ERROR: realloc failed node\n\n") ;

	shapeHe20(pcoords[0],pcoords[1],pcoords[2], (double (*)[3])NULL, NULL, weights, 1);
        node[anz_nmax].nx =node[anz_nmax].ny =node[anz_nmax].nz = 0.;
        for(j=0; j<20; j++)
	{
          node[anz_nmax].nx+=node[e_enqire[e].nod[j]].nx * weights[j];
          node[anz_nmax].ny+=node[e_enqire[e].nod[j]].ny * weights[j];
          node[anz_nmax].nz+=node[e_enqire[e].nod[j]].nz * weights[j];
	}
        node[anz_n].nr=anz_nmax;
        anz_n++;
        node[anz_nmax].indx=-1;
        node[anz_nmax].pflag=1;
        e_enqire[ e ].nod[26]=anz_nmax;

        /* use common nodes on the mid-face positions (merge) */
        /* first, search for a close node */
        for(j=20; j<26; j++)
	{
          n=e_enqire[ e ].nod[j];
          near3d(orig_x,orig_y,orig_z,sort_x,sort_y,sort_z,sort_nx,sort_ny,sort_nz, node[n].nx,node[n].ny,
            node[n].nz, anz->n, &near_node[0], 2);
          for(k=0;k<2; k++)
	  {
            if(node[near_node[k]].nr!=n)
	    {
              // printf("node:%d near node:%d \n",n, node[near_node[k]].nr);
              dx= node[node[near_node[k]].nr].nx - node[n].nx;
              dy= node[node[near_node[k]].nr].ny - node[n].ny;
              dz= node[node[near_node[k]].nr].nz - node[n].nz;      
              dr=dx*dx + dy*dy + dz*dz;      
              if(dr<tol )
              {
                if(printFlag) printf("node:%d, found equal node:%d with dr:%lf\n", n, node[near_node[k]].nr, sqrt(dr));
                if(node[n].indx==-1)
	        { node[n].pflag=0; node[n].indx=node[near_node[k]].nr; node[node[near_node[k]].nr].indx=node[n].indx; }
	      }
              break;
	    }
	  }
	}
      break;
      case 5:
      ipuf = 18;

        /* use common nodes on the mid-face positions (merge) */
        /* first, search for a close node */
        for(j=15; j<18; j++)
	{
          n=e_enqire[ e ].nod[j];
          near3d(orig_x,orig_y,orig_z,sort_x,sort_y,sort_z,sort_nx,sort_ny,sort_nz, node[n].nx,node[n].ny,
            node[n].nz, anz->n, &near_node[0], 2);
          for(k=0;k<2; k++)
	  {
            if(node[near_node[k]].nr!=n)
	    {
              // printf("node:%d near node:%d \n",n, node[near_node[k]].nr);
              dx= node[node[near_node[k]].nr].nx - node[n].nx;
              dy= node[node[near_node[k]].nr].ny - node[n].ny;
              dz= node[node[near_node[k]].nr].nz - node[n].nz;      
              dr=dx*dx + dy*dy + dz*dz;      
              if(dr<tol )
              {
                if(printFlag) printf("node:%d, found equal node:%d with dr:%lf\n", n, node[near_node[k]].nr, sqrt(dr));
                if(node[n].indx==-1)
	        { node[n].pflag=0; node[n].indx=node[near_node[k]].nr; node[node[near_node[k]].nr].indx=node[n].indx; }
	      }
              break;
	    }
	  }
	}
      break;
      case 6:
      ipuf = 10;  
      break;
      case 7:
      ipuf = 3;   
      break;
      case 8:
      ipuf = 6;   
      break;
      case 9:
      ipuf = 4;   
      break;
      case 10:
      ipuf = 8;   
      break;
      case 11:
      ipuf = 2;   
      break;
      case 12:
      ipuf = 3;   
      break;      
    }
    for (n=0; n<ipuf; n++)
    {
      node[e_enqire[e].nod[n]].pflag=1;
    }
  }
  for (n=0; n<anz_n; n++) if(node[node[n].nr].indx==-1) node[node[n].nr].indx=node[n].nr;

  /* write abaqus-format */
  if (compare( format, "abq", 3)== 3)
  {
    handle = fopen (prognam, "w");
    if ( handle== NULL )
    { printf ("\nThe input file %s could not be opened.\n\n", prognam); return; }
    fprintf(handle, "** Linear elements based on %s\n", setname);

    fprintf(handle,"*NODE, NSET=Nall\n");
    for (i=0; i<anz_n; i++ )
    {
      j=node[i].nr;
      if(node[j].pflag==1) fprintf(handle," %d,%f,%f,%f\n", j,node[j].nx,node[j].ny,node[j].nz  );
    }
    for (i=0; i<set[setNr].anz_e; i++ )
    {
      e=set[setNr].elem[i];
      switch(e_enqire[e].type)
      {
        case 1:
        fprintf(handle,"** elem:%d\n*ELEMENTS, TYPE=C3D8\n", e);
        fprintf(handle,"%d", enr++);
        for (k=0; k<8; k++)
        {
          fprintf(handle,",%d", e_enqire[e].nod[k]);
        }
        fprintf(handle,"\n");
        break;
        case 2:
        fprintf(handle,"** elem:%d\n*ELEMENTS, TYPE=C3D6\n", e);
        fprintf(handle,"%d", enr++);
        for (k=0; k<6; k++)
        {
          fprintf(handle,",%d", e_enqire[e].nod[k]);
        }
        fprintf(handle,"\n");
        break;
        case 3:
        fprintf(handle,"** elem:%d\n*ELEMENTS, TYPE=C3D4\n", e);
        fprintf(handle,"%d", enr++);
        for (k=0; k<4; k++)
        {
          fprintf(handle,",%d", e_enqire[e].nod[k]);
        }
        fprintf(handle,"\n");
        break;
        case 4:
        fprintf(handle,"** elem:%d\n*ELEMENTS, TYPE=C3D8\n", e);
        for (j=0; j<8; j++)
        {
          fprintf(handle,"%d", enr++);
          for (k=0; k<8; k++)
          {
            fprintf(handle,",%d", node[ e_enqire[e].nod[hex20hex[8*j+k]] ].indx);
          }
          fprintf(handle,"\n");
        }
        break;
        case 5:
        fprintf(handle,"** elem:%d\n*ELEMENTS, TYPE=C3D6\n", e);
        for (j=0; j<8; j++)
        {
          fprintf(handle,"%d", enr++);
          for (k=0; k<6; k++)
          {
            fprintf(handle,",%d", node[ e_enqire[e].nod[pe15pe[6*j+k]] ].indx);
          }
          fprintf(handle,"\n");
        }
        break;
        case 6:
        fprintf(handle,"** elem:%d\n*ELEMENTS, TYPE=C3D4\n", e);
        for (j=0; j<8; j++)
        {
          fprintf(handle,"%d", enr++);
          for (k=0; k<4; k++)
          {
            fprintf(handle,",%d", e_enqire[e].nod[tet10tet[4*j+k]]);
          }
          fprintf(handle,"\n");
        }
        break;
        default:
	  printf(" ERROR: Elem:%d type:%d not supported\n",e, e_enqire[e].type);
        break;     
      }
    }
  }
  else
  {
    errMsg(" ERROR: format %s not yet supported\n", format );
  }

  fclose(handle);
  scalNodes ( anz->n, node, scale); // necessary to reset the scale flag in scalNodes
  free(node);
  printf (" ready\n");
}



/* val4 holds the dofs when this function is used for *writing BOUNDARYF commands */
void sendPressure( char *setname, char *format, Summen *anz, Nodes *node, Elements *e_enqire, char *val1, char *val2, char *val3, char *val4 )
{
  int   setNr;
  int  i,j;
  char prognam[MAX_LINE_LENGTH];
  double *pressure;
  FILE *handle;
  int  n, lc, entity;

  setNr=getSetNr(setname);
  if (setNr<0)
  {
    printf (" ERROR: set:%s does not exist\n", setname);
    return;
  }

  if(compare(val1, "ds", 2)==2)  sprintf(prognam,"%s_%s%s",setname,val1,val2);
  else sprintf(prognam,"%s",setname);
  if(val4) sprintf(&prognam[strlen(prognam)],"_%s.bouf",val4);
  else     sprintf(&prognam[strlen(prognam)],".dlo");
  printf (" write file: %s\n", prognam );

  if((pressure = (double *)malloc((set[setNr].anz_f+1) * sizeof(double))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );

  /* check if a constant value or a dataset has to be used */
  if(compare(val1, "ds", 2)==2)
  {
    lc=atoi(&val1[2])-1;
    entity=atoi(&val2[1])-1;

    /* check if this lc and entity are valid */
    if((lc>=anz->l)||(lcase[lc].ncomps<entity))
    { printf("ERROR: ds:%d or entity:%d not defined\n",lc,entity); return; }

    /* check if the data of the specified lcase (Dataset) are already available */
    if (!lcase[lc].loaded)
    {
     if( pre_readfrdblock(copiedNodeSets , lc, anz, node, lcase )==-1) 
     {
       printf(" ERROR in sendSet: Could not read data for Dataset:%d\n", lc+1); 
       return;
     }
     calcDatasets( lc, anz, node, lcase );
     recompileEntitiesInMenu(lc);
    }

    /* calculate the average load on that face */
    for (j=0; j<set[setNr].anz_f; j++ )
    {
      i=set[setNr].face[j];
      pressure[j]=0.;
      if(face[i].type==7)
      {
        for(n=0; n<3; n++) pressure[j]+=lcase[lc].dat[entity][face[i].nod[n]];
        pressure[j]/=3.;
      }
      else if(face[i].type==8)
      {
        for(n=0; n<6; n++) pressure[j]+=lcase[lc].dat[entity][face[i].nod[n]];
        pressure[j]/=6.;
      }
      else if(face[i].type==9)
      {
        for(n=0; n<4; n++) pressure[j]+=lcase[lc].dat[entity][face[i].nod[n]];
        pressure[j]/=4.;
      }
      else if(face[i].type==10)
      {
        for(n=0; n<8; n++) pressure[j]+=lcase[lc].dat[entity][face[i].nod[n]];
        pressure[j]/=8.;
      }
      else
      {
        printf(" Not supported for this element type, load set to 0.\n");
        pressure[i]=0.;
      }
    }
  }
  else
  {
    for (j=0; j<set[setNr].anz_f; j++ ) pressure[j]=atof(val1);
  }

  /* write the pressure in nastran-format */
  if (compare( format, "nas", 3)== 3)
  {
    handle = fopen (prognam, "w");
    if ( handle== NULL )
    { printf ("\nThe input file %s could not be opened.\n\n", prognam); return; }
    fprintf(handle, "$ Pressure based on %s\n", setname);
    for (j=0; j<set[setNr].anz_f; j++ )
    {
      i=set[setNr].face[j];
      if (face[i].type==9)
      {
        fprintf(handle, "PLOAD4,%8d,%8d,%12.5e, , , ,%8d,%8d\n",
          1, face[i].elem_nr, pressure[j], face[i].nod[0], face[i].nod[2] );
      }
      else
        printf(" Pressure not supported for this element type:%d\n", face[i].type);
    }
    fclose(handle);
  }
  /* write the pressure in abaqus-format */
  else if (compare( format, "abq", 3)== 3)
  {
    handle = fopen (prognam, "w");
    if ( handle== NULL )
    { printf ("\nThe input file %s could not be opened.\n\n", prognam); return; }
    if(val4) fprintf(handle, "** BOUNDARYF based on %s\n", setname);
    else fprintf(handle, "** Pressure based on %s\n", setname);
    for (j=0; j<set[setNr].anz_f; j++ )
    {
      i=set[setNr].face[j];
      if(e_enqire[face[i].elem_nr].type>6)  // shell
      {
        if(e_enqire[face[i].elem_nr].type>10)  // beam
	{
          if(val4) fprintf( handle, "%d, S?, %s,, %e \n", face[i].elem_nr, val4, pressure[j]);
          else fprintf( handle, "%d, P?, %e \n", face[i].elem_nr, pressure[j]);
	}
        else if(e_enqire[face[i].elem_nr].attr>3)  // plain strain,stress or axissym
        {
          /* only the surrounding faces are applicable */
          if(val4) fprintf( handle, "%d, S%d, %s,, %e\n", face[i].elem_nr, face[i].nr-1, val4, pressure[j]);
          else fprintf( handle, "%d, P%d, %e\n", face[i].elem_nr, face[i].nr-1, pressure[j]);
        }
        else
        {
          if(face[i].nr==1)
	  {
            if(val4) fprintf( handle, "%d, S, %s,, %e\n", face[i].elem_nr, val4, pressure[j]);
            else fprintf( handle, "%d, P, %e\n", face[i].elem_nr, pressure[j]);
	  }
          else
	  {
            if(val4) fprintf( handle, "%d, EDNOR%d, %s,,%e\n", face[i].elem_nr, face[i].nr-1, val4, pressure[j]);
            else fprintf( handle, "%d, EDNOR%d, %e\n", face[i].elem_nr, face[i].nr-1, pressure[j]);
	  }
        }
      }
      else
      {
        if(val4) fprintf( handle, "%d, S%d, %s,, %e\n", face[i].elem_nr, face[i].nr+1, val4, pressure[j]);
        else fprintf( handle, "%d, P%d, %e\n", face[i].elem_nr, face[i].nr+1, pressure[j]);
      }
    }
    fclose(handle);
  }
  /* write the pressure in ansys-format */
  else if (compare( format, "ans", 3)== 3)
  {
    handle = fopen (prognam, "w");
    if ( handle== NULL )
    { printf ("\nThe input file %s could not be opened.\n\n", prognam); return; }
    fprintf(handle, "! Pressure based on %s\n", setname);
    for (j=0; j<set[setNr].anz_f; j++ )
    {
      i=set[setNr].face[j];
      if (face[i].nr==0)
        fprintf(handle, "SFE, %d, 1, PRES, ,%e \n",
        face[i].elem_nr,  pressure[j] );
      else if (face[i].nr==1)
        fprintf(handle, "SFE, %d, 6, PRES, ,%e \n",
        face[i].elem_nr,  pressure[j] );
      else if (face[i].nr==2)
        fprintf(handle, "SFE, %d, 2, PRES, ,%e \n",
        face[i].elem_nr,  pressure[j] );
      else if (face[i].nr==3)
        fprintf(handle, "SFE, %d, 3, PRES, ,%e \n",
        face[i].elem_nr,  pressure[j] );
      else if (face[i].nr==4)
        fprintf(handle, "SFE, %d, 4, PRES, ,%e \n",
        face[i].elem_nr,  pressure[j] );
      else if (face[i].nr==5)
        fprintf(handle, "SFE, %d, 5, PRES, ,%e \n",
        face[i].elem_nr,  pressure[j] );
      else
        printf(" Not supported for this element type\n");
    }
    fclose(handle);
  }
  else
  {
    errMsg(" ERROR: format %s not yet supported\n", format );
  }
  free(pressure);
  printf (" ready\n");
}


void sendFilm( char *setname, char *format, Summen *anz, Nodes *node, Elements *e_enqire, char *val1, char *val2, char *val3, char *val4, char *val5 )
{
  int   setNr;
  int  i,j, ipuf;
  char prognam[MAX_LINE_LENGTH];
  char typelabel[MAX_LINE_LENGTH];
  char **amp_t=NULL, **amp_a=NULL;
  double *temp, *alpha, value=0;
  FILE *handle;
  int  n, lc=0, entity;
  int lcmin=0, lcmax=0;
  int *dsNr=NULL, anz_lc=0;
  int orif=1;

  setNr=getSetNr(setname);
  if (setNr<0)
  {
    printf (" ERROR: set:%s does not exist\n", setname);
    return;
  }

  if( ((val1!=NULL)&&(val1[0]=='-'))||((val2!=NULL)&&(val2[0]=='-'))||((val3!=NULL)&&(val3[0]=='-'))||((val4!=NULL)&&(val4[0]=='-'))||((val5!=NULL)&&(val5[0]=='-')) ) orif=0; 

  if((temp = (double *)malloc((set[setNr].anz_f+1) * sizeof(double))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );
  if((alpha = (double *)malloc((set[setNr].anz_f+1) * sizeof(double))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );


  printf("film: %s %s %s %s\n",val1,val2,val3,val4);

  /* check if a node, constant value or a dataset has to be used */
  typelabel[0]=0;
  if(compare(val1, "n", 1)==1)
  {
    strcpy(typelabel,"FC");
    for (j=0; j<set[setNr].anz_f; j++ ) temp[j]=(float)atoi(&val1[1]);
  }    
  else if(compare(val1, "ds", 2)==2)
  {
    lc=atoi(&val1[2])-1;
    entity=atoi(&val2[1])-1;

    /* check if this lc and entity are valid */
    if((lc>=anz->l)||(lcase[lc].ncomps<entity))
    { printf("ERROR: ds:%d or entity:%d not defined\n",lc,entity); return; }

    /* check if the data of the specified lcase (Dataset) are already available */
    if (!lcase[lc].loaded)
    {
     if( pre_readfrdblock(copiedNodeSets , lc, anz, node, lcase )==-1) 
     {
       printf(" ERROR in sendSet: Could not read data for Dataset:%d\n", lc+1); 
       return;
     }
     calcDatasets( lc, anz, node, lcase );
     recompileEntitiesInMenu(lc);
    }

    /* calculate the average load on that face */
    for (j=0; j<set[setNr].anz_f; j++ )
    {
      i=set[setNr].face[j];
      temp[j]=0.;
      ipuf=0;
      if(face[i].type==7) ipuf=3;
      else if(face[i].type==8) ipuf=6;
      else if(face[i].type==9) ipuf=4;
      else if(face[i].type==10) ipuf=8;
      else printf(" Not supported for this element type:%d, alpha set to 0.\n",face[i].type);
      if(ipuf)
      {
        for(n=0; n<ipuf; n++) temp[j]+=lcase[lc].dat[entity][face[i].nod[n]];
        temp[j]/=ipuf;
      }
    }
  }
  else if(compare(val1, "sq", 2)==2)
  {
    /* get the range of datasets */
    sscanf(&val1[2],"%d-%d", &lcmin, &lcmax);
    entity=atoi(&val2[1])-1;
    lcmin--;
    lcmax--;

    /* check if this lc and entity are valid */
    if((lcmin>anz->l)||(lcmax>=anz->l)||(lcmax<=lcmin)||(lcase[lc].ncomps<entity))
    { printf("ERROR: ds:%d, %d or entity:%d in error\n",lcmin+1, lcmax+1, entity+1); return; }

    printf("use lc from %d to %d of type:%s and entity:%s\n", lcmin+1, lcmax+1,  lcase[lcmin].name, lcase[lcmin].compName[entity]);

    /* compile the lc-data based on a dataset-name */
    anz_lc=0;
    for(lc=lcmin; lc<=lcmax; lc++)
    {
      if( compare( lcase[lc].name, lcase[lcmin].name, strlen(lcase[lcmin].name)) == strlen(lcase[lcmin].name) )
    	  //if(compareStrings(lcase[lc].name, lcase[lcmin].name)>0)
      {
        anz_lc++; 
        if((dsNr=(int *)realloc((int *)dsNr, (anz_lc+2) *sizeof(int)))==NULL )
          printf("\n\n ERROR: malloc failed \n\n") ;
        dsNr[anz_lc]=lc;
      }
    }
    if(!anz_lc)
    {
      printf(" ERROR: found no matching Dataset for string:%s\n", lcase[lcmin].name);
      return;
    }
    dsNr[0]=anz_lc;

    /* check if the data of the specified lcase (Dataset) are already available */
    printf (" please wait, loading data\n");
    for(i=1; i<=dsNr[0]; i++)
    {
      lc=dsNr[i];
      if (!lcase[lc].loaded)
      {
        if( pre_readfrdblock(copiedNodeSets , lc, anz, node, lcase )==-1) 
        {
          printf("ERROR in sendFilm: Could not read data for Dataset:%d\n", lc+1); 
          return;
        }
        calcDatasets( lc, anz, node, lcase );
        recompileEntitiesInMenu(lc);
      }
    }


    /* store the amplitudes for all faces */
    if ( (amp_t = (char **)malloc( (set[setNr].anz_f+1) * sizeof(char *))) == NULL )
          printf("\n\n ERROR: malloc failed \n\n") ;

    /* write in abaqus-format */
    if (compare( format, "abq", 3)== 3)
    {
      sprintf(prognam,"%s_temp.amp",setname);
      printf (" write file: %s\n", prognam );
      handle = fopen (prognam, "w");
      if ( handle== NULL )
      { printf ("\nThe input file %s could not be opened.\n\n", prognam); return; }
      fprintf(handle, "** Film based on %s\n", setname);

      for (j=0; j<set[setNr].anz_f; j++ )
      {
        i=set[setNr].face[j];
        if ( (amp_t[j] = (char *)malloc( (MAX_LINE_LENGTH) * sizeof(char))) == NULL )
            printf("\n\n ERROR: malloc failed \n\n") ;
        sprintf(amp_t[j],"ampt_%d_%d",face[i].elem_nr, face[i].nr+1);
        fprintf(handle, "*AMPLITUDE,NAME=%s\n",amp_t[j]);
        temp[j]=1.;
  
        for(lc=1; lc<=dsNr[0]; lc++)
        {
          /* calculate the average load on that face */
          i=set[setNr].face[j];
  	  ipuf=0;
          if(face[i].type==7) ipuf=3;
          else if(face[i].type==8) ipuf=6;
          else if(face[i].type==9) ipuf=4;
          else if(face[i].type==10) ipuf=8;
          else printf(" Not supported for this element type:%d, alpha set to 0.\n",face[i].type);
          if(ipuf)
  	  {
            value=0.;
            for(n=0; n<ipuf; n++) value+=lcase[dsNr[lc]].dat[entity][face[i].nod[n]];
            value/=ipuf;
  	  }
          fprintf(handle, "%f, %f,\n",lcase[dsNr[lc]].value, value);
        }
      }
      free(dsNr);
      fclose(handle);
    }
  }
  else
  {
    for (j=0; j<set[setNr].anz_f; j++ ) temp[j]=atof(val1);
  }


  if((compare(val2, "ds", 2)==2)||(compare(val3, "ds", 2)==2))
  {
    if(compare(val2, "ds", 2))
    {
      lc=atoi(&val2[2])-1;
      entity=atoi(&val3[1])-1;
    }
    else
    {
      lc=atoi(&val3[2])-1;
      entity=atoi(&val4[1])-1;
    }

    /* check if this lc and entity are valid */
    if((lc>=anz->l)||(lcase[lc].ncomps<entity))
    { printf("ERROR: ds:%d or entity:%d not defined\n",lc,entity); return; }

    /* calculate the average load on that face */
    for (j=0; j<set[setNr].anz_f; j++ )
    {
      i=set[setNr].face[j];
      alpha[j]=0.;
      ipuf=0;
      if(face[i].type==7) ipuf=3;
      else if(face[i].type==8) ipuf=6;
      else if(face[i].type==9) ipuf=4;
      else if(face[i].type==10) ipuf=8;
      else printf(" Not supported for this element type:%d, alpha set to 0.\n",face[i].type);
      if(ipuf)
      {
        for(n=0; n<ipuf; n++) alpha[j]+=lcase[lc].dat[entity][face[i].nod[n]];
        alpha[j]/=ipuf;
      }
    }
  }
  else if((compare(val2, "sq", 2)==2)||(compare(val3, "sq", 2)==2))
  {
    /* get the range of datasets */
    if(compare(val2, "sq", 2))
    {
      sscanf(&val2[2],"%d-%d", &lcmin, &lcmax);
      entity=atoi(&val3[1])-1;
    }
    else
    {
      sscanf(&val3[2],"%d-%d", &lcmin, &lcmax);
      entity=atoi(&val4[1])-1;
    }
    lcmin--;
    lcmax--;

    /* check if this lc and entity are valid */
    if((lcmin>=anz->l)||(lcmax>=anz->l)||(lcmax<=lcmin)||(lcase[lc].ncomps<entity))
    { printf("ERROR: ds:%d, %d or entity:%d in error\n",lcmin+1, lcmax+1, entity+1); return; }

    printf("use lc from %d to %d of type:%s and entity:%s\n", lcmin+1, lcmax+1,  lcase[lcmin].name, lcase[lcmin].compName[entity]);


    /* compile the lc-data based on a dataset-name */
    anz_lc=0;
    for(lc=lcmin; lc<=lcmax; lc++)
    {
      if( compare( lcase[lc].name, lcase[lcmin].name, strlen(lcase[lcmin].name)) == strlen(lcase[lcmin].name) )
    	  //if(compareStrings(lcase[lc].name, lcase[lcmin].name)>0)
      {
        anz_lc++; 
        if((dsNr=(int *)realloc((int *)dsNr, (anz_lc+2) *sizeof(int)))==NULL )
          printf("\n\n ERROR: malloc failed \n\n") ;
        dsNr[anz_lc]=lc;
      }
    }
    if(!anz_lc)
    {
      printf(" ERROR: found no matching Dataset for string:%s\n", lcase[lcmin].name);
      return;
    }
    dsNr[0]=anz_lc;

    /* check if the data of the specified lcase (Dataset) are already available */
    printf (" please wait, loading data\n");
    for(i=1; i<=dsNr[0]; i++)
    {
      lc=dsNr[i];
      if (!lcase[lc].loaded)
      {
        if( pre_readfrdblock(copiedNodeSets , lc, anz, node, lcase )==-1) 
        {
          printf("ERROR in sendFilm: Could not read data for Dataset:%d\n", lc+1); 
          return;
        }
        calcDatasets( lc, anz, node, lcase );
        recompileEntitiesInMenu(lc);
      }
    }

    /* store the amplitudes for all faces */
    if ( (amp_a = (char **)malloc( (set[setNr].anz_f+1) * sizeof(char *))) == NULL )
          printf("\n\n ERROR: malloc failed \n\n") ;

    /* write in abaqus-format */
    if (compare( format, "abq", 3)== 3)
    {
      sprintf(prognam,"%s_alfa.amp",setname);
      printf (" write file: %s\n", prognam );
      handle = fopen (prognam, "w");
      if ( handle== NULL )
      { printf ("\nThe input file %s could not be opened.\n\n", prognam); return; }
      fprintf(handle, "** Film based on %s\n", setname);

      for (j=0; j<set[setNr].anz_f; j++ )
      {
        i=set[setNr].face[j];
        if ( (amp_a[j] = (char *)malloc( (MAX_LINE_LENGTH) * sizeof(char))) == NULL )
            printf("\n\n ERROR: malloc failed \n\n") ;
        sprintf(amp_a[j],"ampf_%d_%d",face[i].elem_nr, face[i].nr+1);
        fprintf(handle, "*AMPLITUDE,NAME=%s\n",amp_a[j]);
        alpha[j]=1.;
  
        for(lc=1; lc<=dsNr[0]; lc++)
        {
          /* calculate the average load on that face */
          i=set[setNr].face[j];
  	  ipuf=0;
          if(face[i].type==7) ipuf=3;
          else if(face[i].type==8) ipuf=6;
          else if(face[i].type==9) ipuf=4;
          else if(face[i].type==10) ipuf=8;
          else printf(" Not supported for this element type:%d, alpha set to 0.\n",face[i].type);
          if(ipuf)
  	  {
            value=0.;
            for(n=0; n<ipuf; n++) value+=lcase[dsNr[lc]].dat[entity][face[i].nod[n]];
            value/=ipuf;
  	  }
          fprintf(handle, "%f, %f,\n",lcase[dsNr[lc]].value, value);
        }
      }
      free(dsNr);
      fclose(handle);
    }
    else
    {
      errMsg(" ERROR: format %s not yet supported\n", format );
    }
  }
  else
  {
    if((compare(val1, "ds", 2)==2)||(compare(val1, "sq", 2)==2))
      for (j=0; j<set[setNr].anz_f; j++ ) alpha[j]=atof(val3);
    else
      for (j=0; j<set[setNr].anz_f; j++ ) alpha[j]=atof(val2);
  }

  /* write in abaqus-format */
  if (compare( format, "abq", 3)== 3)
  {
    if((compare(val1, "ds", 2)==2)&&(compare(val3, "ds", 2)==2))  sprintf(prognam,"%s_%s%s%s%s.flm",setname,val1,val2,val3,val4);
    else sprintf(prognam,"%s.flm",setname);
    printf (" write file: %s\n", prognam );
    handle = fopen (prognam, "w");
    if ( handle== NULL )
    { printf ("\nThe input file %s could not be opened.\n\n", prognam); return; }
    fprintf(handle, "** Film based on %s\n", setname);

    for (j=0; j<set[setNr].anz_f; j++ )
    {
      i=set[setNr].face[j];
  
      /* are amplitudes defined? Then we have individual values for all faces */
      if(amp_t!=NULL)
      {
        fprintf(handle, "*FILM, AMPLITUDE=%s, FILM AMPLITUDE=replace_with_editor\n", amp_t[j]);
      }
      else if(amp_a!=NULL)
      {
        fprintf(handle, "*FILM, AMPLITUDE=replace_with_editor, FILM AMPLITUDE=%s\n", amp_a[j]);
      }
      else if((amp_t!=NULL)&&(amp_a!=NULL))
      {
        fprintf(handle, "*FILM, AMPLITUDE=%s, FILM AMPLITUDE=%s\n", amp_t[j], amp_a[j]);
      }

     if(e_enqire[face[i].elem_nr].type>6)  // shell
      {
        if(e_enqire[face[i].elem_nr].type>10)  // beam
	{
          if(typelabel[0]) fprintf( handle, "%d, F?%s, %d, %e\n", face[i].elem_nr, typelabel, (int)temp[j], alpha[j]);
          else fprintf( handle, "%d, F?%s, %lf, %e\n", face[i].elem_nr, typelabel, temp[j], alpha[j]);
	}
        else if(e_enqire[face[i].elem_nr].attr>3)  // plain strain,stress or axissym
        {
          if(typelabel[0])
	  {
            if((face[i].nr==1)&&(orif==1)) fprintf( handle, "%d, FP%s, %d, %e\n", face[i].elem_nr, typelabel, (int)temp[j], alpha[j]);
            else if((face[i].nr==1)&&(orif==0)) fprintf( handle, "%d, FN%s, %d, %e\n", face[i].elem_nr, typelabel, (int)temp[j], alpha[j]);
            else fprintf( handle, "%d, F%d%s, %d, %e\n", face[i].elem_nr, face[i].nr-1, typelabel, (int)temp[j], alpha[j]);
	  }
          else
	  {
            if((face[i].nr==1)&&(orif==1)) fprintf( handle, "%d, FP%s, %lf, %e\n", face[i].elem_nr, typelabel, temp[j], alpha[j]);
            else if((face[i].nr==1)&&(orif==0)) fprintf( handle, "%d, FN%s, %lf, %e\n", face[i].elem_nr, typelabel, temp[j], alpha[j]);
            else fprintf( handle, "%d, F%d%s, %lf, %e\n", face[i].elem_nr, face[i].nr-1, typelabel, temp[j], alpha[j]);
	  }
        }
        else
        {
          if(typelabel[0])
	  {
            if((face[i].nr==1)&&(orif==1)) fprintf( handle, "%d, FP%s, %d, %e\n", face[i].elem_nr, typelabel, (int)temp[j], alpha[j]);
            else if((face[i].nr==1)&&(orif==0)) fprintf( handle, "%d, FN%s, %d, %e\n", face[i].elem_nr, typelabel, (int)temp[j], alpha[j]);
            else fprintf( handle, "%d, F%d%s, %d, %e\n", face[i].elem_nr, face[i].nr+1, typelabel, (int)temp[j], alpha[j]);
	  }
          else
	  {
            if((face[i].nr==1)&&(orif==1)) fprintf( handle, "%d, FP%s, %lf, %e\n", face[i].elem_nr, typelabel, temp[j], alpha[j]);
            else if((face[i].nr==1)&&(orif==0)) fprintf( handle, "%d, FN%s, %lf, %e\n", face[i].elem_nr, typelabel, temp[j], alpha[j]);
            else fprintf( handle, "%d, F%d%s, %lf, %e\n", face[i].elem_nr, face[i].nr+1, typelabel, temp[j], alpha[j]);
	  }
        }
      }
      else
      {
         if(typelabel[0]) fprintf( handle, "%d, F%d%s, %d, %e\n", face[i].elem_nr, face[i].nr+1, typelabel, (int)temp[j], alpha[j]);
         else fprintf( handle, "%d, F%d%s, %lf, %e\n", face[i].elem_nr, face[i].nr+1, typelabel, temp[j], alpha[j]);
      }
    }
    fclose(handle);
  }
  else
  {
    errMsg(" ERROR: format %s not yet supported\n", format );
  }
  free(temp);
  free(alpha);
  printf (" ready\n");
}


void sendRadiate( char *setname, char *format, Summen *anz, Nodes *node, Elements *e_enqire, char *val1, char *val2, char *val3, char *val4, char *val5 )
{
  int   setNr;
  int  i,j;
  char prognam[MAX_LINE_LENGTH];
  char faceExtention[MAX_LINE_LENGTH];
  double *temp, *emisivity;
  FILE *handle;
  int  n, lc, entity;
  int orif=1;

  setNr=getSetNr(setname);
  if (setNr<0)
  {
    printf (" ERROR: set:%s does not exist\n", setname);
    return;
  }

  faceExtention[0]=0;
  if( ((val1!=NULL)&&(val1[0]=='-'))||((val2!=NULL)&&(val2[0]=='-'))||((val3!=NULL)&&(val3[0]=='-'))||((val4!=NULL)&&(val4[0]=='-'))||((val5!=NULL)&&(val5[0]=='-')) ) orif=0; 

  if((compare(val1, "ds", 2)==2)&&(compare(val3, "ds", 2)==2))  sprintf(prognam,"%s_%s%s%s%s.rad",setname,val1,val2,val3,val4);
  else sprintf(prognam,"%s.rad",setname);
  printf (" write file: %s\n", prognam );

  if((temp = (double *)malloc((set[setNr].anz_f+1) * sizeof(double))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );
  if((emisivity = (double *)malloc((set[setNr].anz_f+1) * sizeof(double))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );

  /* check if a constant value or a dataset has to be used */
  if(compare(val1, "ds", 2)==2)
  {
    lc=atoi(&val1[2])-1;
    entity=atoi(&val2[1])-1;

    /* check if this lc and entity are valid */
    if((lc>=anz->l)||(lcase[lc].ncomps<entity))
    { printf("ERROR: ds:%d or entity:%d not defined\n",lc,entity); return; }

    /* check if the data of the specified lcase (Dataset) are already available */
    if (!lcase[lc].loaded)
    {
     if( pre_readfrdblock(copiedNodeSets , lc, anz, node, lcase )==-1) 
     {
       printf(" ERROR in sendSet: Could not read data for Dataset:%d\n", lc+1); 
       return;
     }
     calcDatasets( lc, anz, node, lcase );
     recompileEntitiesInMenu(lc);
    }

    /* calculate the average load on that face */
    for (j=0; j<set[setNr].anz_f; j++ )
    {
      i=set[setNr].face[j];
      temp[j]=0.;
      if(face[i].type==7)
      {
        for(n=0; n<4; n++) temp[j]+=lcase[lc].dat[entity][face[i].nod[n]];
        temp[j]/=4.;
      }
      else if(face[i].type==10)
      {
        for(n=0; n<8; n++) temp[j]+=lcase[lc].dat[entity][face[i].nod[n]];
        temp[j]/=8.;
      }
      else
      {
        printf(" Not supported for this element type, temp set to 0.\n");
        temp[i]=0.;
      }
    }
  }
  else if(compare(val1, "cr", 2)==2)
  {
    strcpy(faceExtention,"CR");
    for (j=0; j<set[setNr].anz_f; j++ ) temp[j]=atof(&val1[2]);
  }
  else
  {
    for (j=0; j<set[setNr].anz_f; j++ ) temp[j]=atof(val1);
  }

  if((compare(val2, "ds", 2)==2)||(compare(val3, "ds", 2)==2))
  {
    if(compare(val2, "ds", 2))
    {
      lc=atoi(&val2[2])-1;
      entity=atoi(&val3[1])-1;
    }
    else
    {
      lc=atoi(&val3[2])-1;
      entity=atoi(&val4[1])-1;
    }

    /* check if this lc and entity are valid */
    if((lc>=anz->l)||(lcase[lc].ncomps<entity))
    { printf("ERROR: ds:%d or entity:%d not defined\n",lc,entity); return; }

    /* calculate the average load on that face */
    for (j=0; j<set[setNr].anz_f; j++ )
    {
      i=set[setNr].face[j];
      emisivity[j]=0.;
      if(face[i].type==7)
      {
        for(n=0; n<4; n++) temp[j]+=lcase[lc].dat[entity][face[i].nod[n]];
        emisivity[j]/=4.;
      }
      else if(face[i].type==10)
      {
        for(n=0; n<8; n++) emisivity[j]+=lcase[lc].dat[entity][face[i].nod[n]];
        emisivity[j]/=8.;
      }
      else
      {
        printf(" Not supported for this element type, emisivity set to 0.\n");
        emisivity[i]=0.;
      }
    }
  }
  else
  {
    if(compare(val1, "ds", 2)==2)
      for (j=0; j<set[setNr].anz_f; j++ ) emisivity[j]=atof(val3);
    else
      for (j=0; j<set[setNr].anz_f; j++ ) emisivity[j]=atof(val2);
  }

  /* write in abaqus-format */
  if (compare( format, "abq", 3)== 3)
  {
    handle = fopen (prognam, "w");
    if ( handle== NULL )
    { printf ("\nThe input file %s could not be opened.\n\n", prognam); return; }
    fprintf(handle, "** Radiate based on %s\n", setname);
    for (j=0; j<set[setNr].anz_f; j++ )
    {
      i=set[setNr].face[j];
      if(e_enqire[face[i].elem_nr].type>6)  // shell
      {
        if(e_enqire[face[i].elem_nr].type>10)  // beam
          fprintf( handle, "%d, R?, %lf, %e \n", face[i].elem_nr, temp[j], emisivity[j]);
        else if(e_enqire[face[i].elem_nr].attr>3)  // plain strain,stress or axissym
        {
          if((face[i].nr==1)&&(orif==1)) fprintf( handle, "%d, RP%s, %lf, %e\n", face[i].elem_nr, faceExtention, temp[j], emisivity[j]);
          else if((face[i].nr==1)&&(orif==0)) fprintf( handle, "%d, RN%s, %lf, %e\n", face[i].elem_nr, faceExtention, temp[j], emisivity[j]);
          else fprintf( handle, "%d, R%d%s, %lf, %e\n", face[i].elem_nr, face[i].nr-1, faceExtention, temp[j], emisivity[j]);
        }
        else
        {
          if((face[i].nr==1)&&(orif==1)) fprintf( handle, "%d, RPOS%s, %lf, %e\n", face[i].elem_nr, faceExtention, temp[j], emisivity[j]);
          else if((face[i].nr==1)&&(orif==0)) fprintf( handle, "%d, RNEG%s, %lf, %e\n", face[i].elem_nr, faceExtention, temp[j], emisivity[j]);
          else fprintf( handle, "%d, R%d%s, %lf, %e\n", face[i].elem_nr, face[i].nr+1, faceExtention, temp[j], emisivity[j]);
        }
      }
      else fprintf( handle, "%d, R%d%s, %lf, %e\n", face[i].elem_nr, face[i].nr+1, faceExtention, temp[j], emisivity[j]);
    }
    fclose(handle);
  }
  else
  {
    errMsg(" ERROR: format %s not yet supported\n", format );
  }
  free(temp);
  free(emisivity);
  printf (" ready\n");
}


void sendDflux( char *setname, char *format, Summen *anz, Nodes *node, Elements *e_enqire, char *val1, char *val2, char *val3, char type )
{
  int   setNr;
  int  i,j;
  char prognam[MAX_LINE_LENGTH], label;
  double *load;
  FILE *handle;
  int  n, lc, entity;
  int orif=1;

  setNr=getSetNr(setname);
  if (setNr<0)
  {
    printf (" ERROR: set:%s does not exist\n", setname);
    return;
  }

  if( ((val1!=NULL)&&(val1[0]=='-'))||((val2!=NULL)&&(val2[0]=='-'))||((val3!=NULL)&&(val3[0]=='-')) ) orif=0; 

  if(type==0)
  {
    if(compare(val1, "ds", 2)==2)  sprintf(prognam,"%s_%s%s.dfl",setname,val1,val2);
    else sprintf(prognam,"%s.dfl",setname);
  }
  if(type==1)
  {
    if(compare(val1, "ds", 2)==2)  sprintf(prognam,"%s_%s%s.mfl",setname,val1,val2);
    else sprintf(prognam,"%s.mfl",setname);
  }
  printf (" write file: %s\n", prognam );

  if((load = (double *)malloc((set[setNr].anz_f+1) * sizeof(double))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );

  /* check if a constant value or a dataset has to be used */
  if(compare(val1, "ds", 2)==2)
  {
    lc=atoi(&val1[2])-1;
    entity=atoi(&val2[1])-1;

    /* check if this lc and entity are valid */
    if((lc>=anz->l)||(lcase[lc].ncomps<entity))
    { printf("ERROR: ds:%d or entity:%d not defined\n",lc,entity); return; }

    /* check if the data of the specified lcase (Dataset) are already available */
    if (!lcase[lc].loaded)
    {
     if( pre_readfrdblock(copiedNodeSets , lc, anz, node, lcase )==-1) 
     {
       printf(" ERROR in sendSet: Could not read data for Dataset:%d\n", lc+1); 
       return;
     }
     calcDatasets( lc, anz, node, lcase );
     recompileEntitiesInMenu(lc);
    }

    /* calculate the average load on that face */
    for (j=0; j<set[setNr].anz_f; j++ )
    {
      i=set[setNr].face[j];
      load[j]=0.;
      if(face[i].type==7)
      {
        for(n=0; n<4; n++) load[j]+=lcase[lc].dat[entity][face[i].nod[n]];
        load[j]/=4.;
      }
      else if(face[i].type==10)
      {
        for(n=0; n<8; n++) load[j]+=lcase[lc].dat[entity][face[i].nod[n]];
        load[j]/=8.;
      }
      else
      {
        printf(" Not supported for this element type, load set to 0.\n");
        load[i]=0.;
      }
    }
  }
  else
  {
    for (j=0; j<set[setNr].anz_f; j++ ) load[j]=atof(val1);
  }

  /* write in abaqus-format */
  if (compare( format, "abq", 3)== 3)
  {
    handle = fopen (prognam, "w");
    if ( handle== NULL )
    { printf ("\nThe input file %s could not be opened.\n\n", prognam); return; }
    if(type==0) { fprintf(handle, "** DFlux based on %s\n", setname); label='S'; }
    if(type==1) { fprintf(handle, "** MASSFLOW based on %s\n", setname); label='M'; }
    for (j=0; j<set[setNr].anz_f; j++ )
    {
      i=set[setNr].face[j];
      if(e_enqire[face[i].elem_nr].type>6)  // shell
      {
        if(e_enqire[face[i].elem_nr].type>10)  // beam
          fprintf( handle, "%d, %c?, %e\n", face[i].elem_nr, label, load[j]);
        else if(e_enqire[face[i].elem_nr].attr>3)  // plain strain,stress or axissym
        {
          if((face[i].nr==1)&&(orif==1)) fprintf( handle, "%d, %cP, %e\n", face[i].elem_nr, label, load[j]);
          else if((face[i].nr==1)&&(orif==0)) fprintf( handle, "%d, %cN, %e\n", face[i].elem_nr, label, load[j]);
          else fprintf( handle, "%d, %c%d, %e\n", face[i].elem_nr, label, face[i].nr-1, load[j]);
        }
        else
        {
          if((face[i].nr==1)&&(orif==1)) fprintf( handle, "%d, %cPOS, %e\n", face[i].elem_nr, label, load[j]);
          else if((face[i].nr==1)&&(orif==0)) fprintf( handle, "%d, %cNEG, %e\n", face[i].elem_nr, label, load[j]);
          else fprintf( handle, "%d, %c%d, %e\n", face[i].elem_nr, label, face[i].nr-1, load[j]);
        }
      }
      else fprintf( handle, "%d, %c%d, %e\n", face[i].elem_nr, label, face[i].nr+1, load[j]);
    }
    fclose(handle);
  }
  else
  {
    errMsg(" ERROR: format %s not yet supported\n", format );
  }
  free(load);
  printf (" ready\n");
}


void sendSPCF( char *setname, char *format, Summen *anz, Nodes *node, Elements *e_enqire, char *dofstring, char *val1, char *val2, char *val3 )
{
  int  length,i, dofi;
  char dofa[2]={" \0"};
  char buffer[MAX_LINE_LENGTH];

  if(val1==NULL)
  {
    printf (" ERROR: no value specified\n");
    return;
  }
  if(strlen(val1)==0)
  {
    printf (" ERROR: no value specified\n");
    return;
  }

  length=strlen( dofstring );
  for (i=0; i<length; i++)
  {
    dofa[0]=dofstring[i];
    if(checkIfNumber(dofa)) dofi=atoi(dofa); else dofi=-1;
    if ((dofstring[i]!='t')&&(dofstring[i]!='p')&&((dofi<0)||(dofi>6))) { errMsg ("ERROR dof:%s not known\n", dofa); return; }
    else 
    {
      if(dofstring[i]=='t') sprintf(buffer,"%d",DOFT);
      else if(dofstring[i]=='p') sprintf(buffer,"%d",DOFP);
      else sprintf(buffer,"%c",dofstring[i]);
      sendPressure( setname, format, anz, node, e_enqire, val1, val2, val3, buffer );
    }
  }
}


void sendSPC( char *setname, char *format, Summen *anz, Nodes *node, Elements *e_enqire, char *dofstring, char *val1, char *val2, char *val3 )
{
  int   setNr;
  int  length,i,j,n, dofi, lc, entity;
  char prognam[MAX_LINE_LENGTH], dofa[2]={" \0"};
  Nodes *norm;
  int   *sum_n;
  double fx=0.,fy=0.,fz=0.;
  FILE *handle;


  setNr=getSetNr(setname);
  if (setNr<0)
  {
    printf (" ERROR: set:%s does not exist\n", setname);
    return;
  }

  sprintf(prognam,"%s_%s%s%s%s.bou",setname,dofstring,val1,val2,val3);
  printf (" write file: %s\n", prognam );

  if (compare( format, "abq", 3)== 3)
  {
    handle = fopen (prognam, "w");
    if ( handle== NULL )
    { printf ("\nThe input file %s could not be opened.\n\n", prognam); return; }
    fprintf(handle, "** BOUNDARY based on %s\n", setname);

    /* move the nodes normal to the surface (xyz can have individual factors) */
    if(compare(dofstring, "nor", 2)==2)
    {
      /* ---- add the nodes and faces ----- */
      if(!set[setNr].anz_f)
      {
        printf(" WARNING: Found no faces in set:%s. A 'comp %s down' was performed\n",set[setNr].name,set[setNr].name); 
        completeSet( set[setNr].name, "do" );
      }

      /* calculate the average normal on every node */
      getNodeNormalen(&sum_n, &norm, setNr, anz, face);

      /* check if a constant dofstring or a dataset has to be used */
      if(compare(val1, "ds", 2)==2)
      {
        if((!val1[2])&&(!val2[1])) { printf("ERROR: ds and/or entity not defined\n"); return; } 
        lc=atoi(&val1[2])-1;
        entity=atoi(&val2[1])-1;

        /* check if this lc and entity are valid */
        if((lc>=anz->l)||(lcase[lc].ncomps<entity))
        { printf("ERROR: ds:%d or entity:%d not defined\n",lc,entity); return; }

        /* check if the data of the specified lcase (Dataset) are already available */
        if (!lcase[lc].loaded)
        {
          if( pre_readfrdblock(copiedNodeSets , lc, anz, node, lcase )==-1) 
          {
            printf(" ERROR in sendSet: Could not read data for Dataset:%d\n", lc+1); 
            return;
          }
          calcDatasets( lc, anz, node, lcase );
          recompileEntitiesInMenu(lc);
        }

        for (n=1; n<=anz->nmax; n++ )
        {
          if( sum_n[n] >0 )
          {
            fx=lcase[lc].dat[entity][n];
            fprintf(handle, "%d, 1, 1, %f\n",n, fx* norm[n].nx );
            fprintf(handle, "%d, 2, 2, %f\n",n, fx* norm[n].ny );
            fprintf(handle, "%d, 3, 3, %f\n",n, fx* norm[n].nz );
          }
        }
      }
      else
      {
        if((strlen(val2)==0)&&(strlen(val3)==0)) fx=fy=fz=atof(val1);
        else
        {
          fx=atof(val1);
          fy=atof(val2);
          fz=atof(val3);
        }
        //printf("normal deflection:%f %f %f\n",fx,fy,fz); 
        for (n=1; n<=anz->nmax; n++ )
        {
          if( sum_n[n] >0 )
          {
            fprintf(handle, "%d, 1, 1, %f\n",n, fx* norm[n].nx );
            fprintf(handle, "%d, 2, 2, %f\n",n, fy* norm[n].ny );
            fprintf(handle, "%d, 3, 3, %f\n",n, fz* norm[n].nz );
          }
        }
      }
    }
    else
    {
     length=strlen( dofstring );
     for (i=0; i<length; i++)
     {
      dofa[0]=dofstring[i];
      if(checkIfNumber(dofa)) dofi=atoi(dofa); else dofi=-1;
      if ((dofstring[i]!='t')&&(dofstring[i]!='p')&&((dofi<0)||(dofi>6))) { errMsg ("ERROR dof:%s not known\n", dofa); return; }
      else 
      {
        /* check if a constant dofstring or a dataset has to be used */
        if(compare(val1, "ds", 2)==2)
	{
          if(!val1[2]) { printf("ERROR: ds not defined\n"); return; } 
          lc=atoi(&val1[2])-1;
          if(val2[1]) entity=atoi(&val2[1])-1;
          else entity=0;

          /* check if this lc and entity are valid */
          if((lc>=anz->l)||(lcase[lc].ncomps<entity))
          { printf("ERROR: ds:%d or entity:%d not defined\n",lc,entity); return; }

          /* check if the data of the specified lcase (Dataset) are already available */
          if (!lcase[lc].loaded)
          {
           if( pre_readfrdblock(copiedNodeSets , lc, anz, node, lcase )==-1) 
           {
             printf(" ERROR in sendSet: Could not read data for Dataset:%d\n", lc+1); 
             return;
           }
           calcDatasets( lc, anz, node, lcase );
           recompileEntitiesInMenu(lc);
          }

          for (j=0; j<set[setNr].anz_n; j++ )
          {
            if( dofstring[i]=='t') fprintf(handle, "%d, %d, , %e\n",set[setNr].node[j], DOFT,  lcase[lc].dat[entity][set[setNr].node[j]] );
            else if( dofstring[i]=='p') fprintf(handle, "%d, %d, , %e\n",set[setNr].node[j], DOFP,  lcase[lc].dat[entity][set[setNr].node[j]] );
            else fprintf(handle, "%d, %c, , %e\n",set[setNr].node[j], dofstring[i],  lcase[lc].dat[entity][set[setNr].node[j]] );
          }
	}
        else
	{
          for (j=0; j<set[setNr].anz_n; j++ )
          {
            if( dofstring[i]=='t') fprintf(handle, "%d, %d, , %s\n",set[setNr].node[j], DOFT, val1  );
            else if( dofstring[i]=='p') fprintf(handle, "%d, %d, , %s\n",set[setNr].node[j], DOFP, val1  );
            else fprintf(handle, "%d, %c, , %s\n",set[setNr].node[j], dofstring[i], val1  );
          }
	}
      }
     }
    }
    fclose(handle);
  }
  else if (compare( format, "ans", 3)== 3)
  {
    handle = fopen (prognam, "w");
    if ( handle== NULL )
    { printf ("\nThe input file %s could not be opened.\n\n", prognam); return; }
    fprintf(handle, "! BOUNDARY based on %s\n", setname);
    length=strlen( dofstring );
    dofa[1]='\0';
    for (i=0; i<length; i++)
    {
      dofa[0]=dofstring[i];
      dofi=atoi(dofa);
      if ((dofi<1)||(dofi>6))  errMsg ("ERROR dof:%d not known\n", dofi);
      else {
      for (j=0; j<set[setNr].anz_n; j++ )
      {
        if (dofi==1) fprintf(handle, "D,%d, UX, 0.  \n",set[setNr].node[j]);
        if (dofi==2) fprintf(handle, "D,%d, UY, 0.  \n",set[setNr].node[j]);
        if (dofi==3) fprintf(handle, "D,%d, UZ, 0.  \n",set[setNr].node[j]);
        if (dofi==4) fprintf(handle, "D,%d, ROTX, 0. \n",set[setNr].node[j]);
        if (dofi==5) fprintf(handle, "D,%d, ROTY, 0. \n",set[setNr].node[j]);
        if (dofi==6) fprintf(handle, "D,%d, ROTZ, 0. \n",set[setNr].node[j]);
      }
      }
    }
    fclose(handle);
  }
  else if (compare( format, "nas", 3)== 3)
  {
    handle = fopen (prognam, "w");
    if ( handle== NULL )
    { printf ("\nThe input file %s could not be opened.\n\n", prognam); return; }
    fprintf(handle, "$ BOUNDARY based on %s\n", setname);
    length=strlen( dofstring );
    dofa[1]='\0';
    for (i=0; i<length; i++)
    {
      dofa[0]=dofstring[i];
      dofi=atoi(dofa);
      if ((dofi<1)||(dofi>6))  errMsg ("ERROR dof:%d not known\n", dofi);
      else {
       for (j=0; j<set[setNr].anz_n; j++ )
       {
        fprintf(handle, "SPC1, 1,%8d,%8d\n", dofi, set[setNr].node[j]);
       }
      }
    }
    fclose(handle);
  }
  else
  {
    errMsg(" ERROR: format %s not yet supported\n", format );
  }
  printf (" ready\n");
}


void sendCflux( char *setname, char *format, Summen *anz, Nodes *node, Elements *e_enqire, char *dofstring, char *val2 )
{
  int   setNr;
  int  length, i,j, dofi=0;
  char prognam[MAX_LINE_LENGTH];

  FILE *handle;


  setNr=getSetNr(setname);
  if (setNr<0)
  {
    printf (" ERROR: set:%s does not exist\n", setname);
    return;
  }

  sprintf(prognam,"%s.cfl",setname);
  printf (" write file: %s\n", prognam );

  if (compare( format, "abq", 3)== 3)
  {
    handle = fopen (prognam, "w");
    if ( handle== NULL )
    { printf ("\nThe input file %s could not be opened.\n\n", prognam); return; }
    fprintf(handle, "** Cflux based on %s\n", setname);
    length=strlen( dofstring );
    for (i=0; i<length; i++)
    {
      if (dofstring[i]!='t') errMsg ("ERROR dof:%d not known\n", dofi);
      else 
      {
        for (j=0; j<set[setNr].anz_n; j++ )
        {
          fprintf(handle, "%d, %d, %s\n",set[setNr].node[j], DOFT, val2  );
	}
      }
    }
    fclose(handle);
  }
  else
  {
    errMsg(" ERROR: format %s not yet supported\n", format );
  }
  printf (" ready\n");
}


void sendForce( char *setname, char *format, Summen *anz, Nodes *node, Elements *e_enqire, double *f )
{
  int   setNr;
  int  i,j;
  char prognam[MAX_LINE_LENGTH];

  FILE *handle;


  setNr=getSetNr(setname);
  if (setNr<0)
  {
    printf (" ERROR: set:%s does not exist\n", setname);
    return;
  }

  sprintf (prognam, "%s.frc",setname);
  printf (" write file: %s\n", prognam );

  if (compare( format, "abq", 3)== 3)
  {
    handle = fopen (prognam, "w");
    if ( handle== NULL )
    { printf ("\nThe input file %s could not be opened.\n\n", prognam); return; }
    fprintf(handle, "** Forces based on %s\n", setname );
    for (j=0; j<set[setNr].anz_n; j++ )
    {
      for (i=0; i<3; i++)
      {
        if(f[i]) fprintf(handle, "%d, %d, %lf\n",set[setNr].node[j], i+1, f[i]  );
      }
    }
    fclose(handle);
  }
  else if (compare( format, "ans", 3)== 3)
  {
    handle = fopen (prognam, "w");
    if ( handle== NULL )
    { printf ("\nThe input file %s could not be opened.\n\n", prognam); return; }
    fprintf(handle, "! Forces based on %s\n", setname );
    for (j=0; j<set[setNr].anz_n; j++ )
    {
      if(f[0]) fprintf(handle, "F, %d, FX, %lf\n",set[setNr].node[j], f[0] );
      if(f[1]) fprintf(handle, "F, %d, FY, %lf\n",set[setNr].node[j], f[1] );
      if(f[2]) fprintf(handle, "F, %d, FZ, %lf\n",set[setNr].node[j], f[2] );
    }
    fclose(handle);
  }
  else if (compare( format, "nas", 3)== 3)
  {
    handle = fopen (prognam, "w");
    if ( handle== NULL )
    { printf ("\nThe input file %s could not be opened.\n\n", prognam); return; }
    fprintf(handle, "$ Forces based on %s\n", setname );
    for (j=0; j<set[setNr].anz_n; j++ )
    {
      fprintf(handle, "FORCE, 1,%8d,%8d,%12.5e,%12.5e,%12.5e,%12.5e\n", set[setNr].node[j],0,1.,f[0],f[1],f[2]);
    }
    fclose(handle);
  }
  else
  {
    errMsg(" ERROR: format %s not yet supported\n", format );
  }
  printf (" ready\n");
}


void sendNames( char *setname, char *format, Summen *anz, Nodes *node, Elements *e_enqire )
{
  int   i,j=0,e,incr=0;
  int   setNr, nf=0;
  char prognam[MAX_LINE_LENGTH];

  FILE *handle;
  FILE *handle_pat;


  setNr=getSetNr(setname);
  if (setNr<0)
  {
    printf (" ERROR: set:%s does not exist\n", setname);
    return;
  }

  sprintf(prognam, "%s.nam", setname);
  printf (" write file: %s\n", prognam );

  if (compare( format, "abq", 3)== 3)
  {
    handle = fopen (prognam, "w");
    if ( handle== NULL )
    { printf ("\nThe input file %s could not be opened.\n\n", prognam); return; }
    fprintf(handle, "** Names based on %s\n", setname );
    if (set[setNr].anz_n>0)
    {
      fprintf(handle, "*NSET,NSET=N%s \n",set[setNr].name  );
      for (j=0; j<set[setNr].anz_n; j++ )
      {
        fprintf(handle, "%d, \n",set[setNr].node[j]  );
      }
    }
    if (set[setNr].anz_e>0)
    {
      fprintf(handle, "*ELSET,ELSET=E%s \n",set[setNr].name  );
      for (j=0; j<set[setNr].anz_e; j++ )
      {
        fprintf(handle, "%d, \n",set[setNr].elem[j]  );
      }
    }
    fclose(handle);
  }
  else if (compare( format, "ans", 3)== 3)
  {
    handle = fopen (prognam, "w");
    if ( handle== NULL )
    { printf ("\nThe input file %s could not be opened.\n\n", prognam); return; }
    fprintf(handle, "! Names based on %s\n", setname );
    if (set[setNr].anz_n>0)
    {
      fprintf(handle, "NSEL, NONE \n"); 
      fprintf(handle, "NSEL, S, NODE, , %d \n",set[setNr].node[0]);
      for (j=1; j<set[setNr].anz_n; j++ )
      {
        fprintf(handle, "NSEL, A, NODE, , %d \n",set[setNr].node[j]);
      }
      fprintf(handle, "CM, %s, NODE \n",set[setNr].name); 
      fprintf(handle, "NSEL, ALL \n"); 
    }
    if (set[setNr].anz_e>0)
    {
      fprintf(handle, "ESEL, NONE \n"); 
      fprintf(handle, "ESEL, S, ELEM, , %d \n",set[setNr].elem[0]);
      for (j=1; j<set[setNr].anz_e; j++ )
      {
        fprintf(handle, "ESEL, A, ELEM, , %d \n",set[setNr].elem[j]);
      }
      fprintf(handle, "CM, %s, ELEM \n",set[setNr].name); 
      fprintf(handle, "ESEL, ALL \n"); 
    }
    fclose(handle);
  }
  /* interface to code_aster from Paul CARRICO */
  else if (compare( format, "ast", 3)== 3)
  {
    handle = fopen (prognam, "w");
    if ( handle== NULL ) 
    {
       printf ("\nThe input file %s could not be opened.\n\n", prognam); 
       return;
    }
    if (set[setNr].anz_n>0)
    {
      fprintf(handle, "GROUP_NO\n%s\n",set[setNr].name);
      for (j=0; j<set[setNr].anz_n; j++ )
      {
        fprintf(handle, "N%d ",set[setNr].node[j]);
        incr++;
        if (incr == 8)
        {
          fprintf(handle, "\n");
          incr = 0;
        }
      }
      if (incr != 0) fprintf(handle, "\n");
      fprintf(handle, "FINSF\n");
    }
    if (set[setNr].anz_e>0)
    {
      fprintf(handle, "GROUP_MA\n");
      fprintf(handle, "%s\n",set[setNr].name  );
      incr = 0;
      for (j=0; j<set[setNr].anz_e; j++ )
      {
        fprintf(handle, "M%d ",set[setNr].elem[j]  );
        incr++;
        if (incr == 8) 
	{
          fprintf(handle, "\n");
	  incr = 0;
        }
      }
      if (incr != 0) fprintf(handle, "\n");
      fprintf(handle, "FINSF\n");
    }
    fclose(handle);
  }
  /* interface to Samcef/Bacon from Paul CARRICO */
  else if (compare( format, "sam", 3)== 3)
  {
    handle = fopen (prognam, "w");
    if ( handle== NULL ) 
    {
       printf ("\nThe input file %s could not be opened.\n\n", prognam); 
       return;
    }
    if (set[setNr].anz_n>0)
    {
      fprintf(handle, ".SEL GROUPE \"%s\" NOEUDS\n",set[setNr].name);
      for (j=0; j<set[setNr].anz_n; j++ )
      {
        fprintf(handle, " I %d\n",set[setNr].node[j]);
      }
    }
    if (set[setNr].anz_e>0)
    {
      fprintf(handle, ".SEL GROUPE \"%s\" MAILLES\n",set[setNr].name);
      for (j=0; j<set[setNr].anz_e; j++ )
      {
        fprintf(handle, " I %d\n",set[setNr].elem[j]  );
      }
    }
    fclose(handle);
  }
  /* interface to gagemap */
  else if (compare( format, "gmp", 3)== 3)
  {
    handle = fopen (prognam, "w");
    if ( handle== NULL ) 
    {
       printf ("\nThe input file %s could not be opened.\n\n", prognam); 
       return;
    }
    if (set[setNr].anz_n>0)
    {
      fprintf(handle, "\n**Group_Start**\n");
      fprintf(handle, "Name=  %s\n",set[setNr].name);
      fprintf(handle, "Opcode= 0\n");
      fprintf(handle, "Type= 0\n");
      fprintf(handle, "Color= 255 0 0\n");
      fprintf(handle, "Nodetype= 0\n");
      fprintf(handle, "Nodes= \n");
      for (j=0; j<set[setNr].anz_n; j++ )
      {
        fprintf(handle, "%d\n",set[setNr].node[j]);
      }
    }
    fclose(handle);
  }
  /* patran neutral file */
  else if (compare( format, "pat", 3)== 3)
  {
    handle = fopen (prognam, "w");
    if ( handle== NULL ) 
    {
       printf ("\nThe input file %s could not be opened.\n\n", prognam); 
       return;
    }
    handle_pat = fopen ("trailer.out", "w");
    if ( handle_pat== NULL ) 
    {
       printf ("\nThe trailer file could not be opened.\n\n"); 
       return;
    }
    printf(" patran neutral file for set:%s will be written, bundle all .nam to .out before reading and add the 'trailer.out'\n", set[setNr].name);

    fprintf(handle, "21%8d%8d%8d\n", setNr+2, (set[setNr].anz_n+set[setNr].anz_e)*2, (int)(1+((set[setNr].anz_n+set[setNr].anz_e)*2+9)/10));
    //if(strlen(set[setNr].name)<=12) j=strlen(set[setNr].name); else j=12;
    //for(i=0; i<j; i++) buffer[i]=set[setNr].name[i]; 
    //for( ; i<12; i++) buffer[i]=' '; buffer[i]=0; 
    //fprintf(handle, "%s\n", buffer);
    fprintf(handle, "%s\n", set[setNr].name);
    j=0;
    if (set[setNr].anz_n>0)
    {
      for (i=0; i<set[setNr].anz_n; i++ )
      {
        fprintf(handle, "%8d%8d",(int)5,set[setNr].node[i]);
        j++;
        if(!(j%5)) fprintf(handle, "\n");
      }
    }
    if (set[setNr].anz_e>0)
    {
      for (i=0; i<set[setNr].anz_e; i++ )
      {
        e=set[setNr].elem[i];
        if (e_enqire[e].type == 1)       nf=12; /* HEXA8 */
        else if (e_enqire[e].type == 2)  nf=11; /* PENTA6 */
        else if (e_enqire[e].type == 3)  nf=9;  /* TET4 */
        else if (e_enqire[e].type == 4)  nf=12; /* HEXA20 */
        else if (e_enqire[e].type == 5)  nf=11; /* PENTA15 */
        else if (e_enqire[e].type == 6)  nf= 9; /* TET10 */
        else if (e_enqire[e].type == 7)  nf=7;  /* TRI3  */
        else if (e_enqire[e].type == 8)  nf=7; /* TRI6  */
        else if (e_enqire[e].type == 9)  nf=8; /* QUAD4 */
        else if (e_enqire[e].type == 10) nf=8; /* QUAD8 */
        else if (e_enqire[e].type == 11) nf=6; /* BEAM */
        else if (e_enqire[e].type == 12) nf=6; /* BEAM3 */
        
        fprintf(handle, "%8d%8d",nf,set[setNr].elem[i]  );
        j++;
        if(!(j%5)) fprintf(handle, "\n");
      }
    }
    if(j%5) fprintf(handle, "\n");
    fprintf(handle_pat, "99       0       0       1       0       0       0       0       0\n");
    fclose(handle_pat);
    fclose(handle);
  }

  else
  {
    errMsg(" ERROR: format %s not yet supported\n", format );
  }
  printf (" ready\n");
}



void sendSurfaces( char *setname, char *format, Summen *anz, Nodes *node, Elements *e_enqire, char *val )
{
  int   i,j;
  int   setNr;
  char prognam[MAX_LINE_LENGTH];
  int orif=1;

  FILE *handle;


  setNr=getSetNr(setname);
  if (setNr<0)
  {
    printf (" ERROR: set:%s does not exist\n", setname);
    return;
  }

  if((val!=NULL)&&(val[0]=='-')) orif=0; 

  sprintf(prognam, "%s.sur", setname);
  printf (" write file: %s\n", prognam );

  if (compare( format, "abq", 3)== 3)
  {
    handle = fopen (prognam, "w");
    if ( handle== NULL )
    { printf ("\nThe input file %s could not be opened.\n\n", prognam); return; }
    fprintf(handle, "** Surfaces based on %s\n", setname );
    fprintf(handle, "*SURFACE, NAME=S%s\n", setname );
    if (set[setNr].anz_f>0)
    {
      for (j=0; j<set[setNr].anz_f; j++ )
      {
        i=set[setNr].face[j];
        if(e_enqire[face[i].elem_nr].type>6)  // shell
	{
          if(e_enqire[face[i].elem_nr].type>10)  // beam
            fprintf( handle, "%d, S?\n", face[i].elem_nr);
	  else if(e_enqire[face[i].elem_nr].attr>3)  // plain strain,stress or axissym
	  {
            //if(face[i].nr==0) fprintf( handle, "%d, SN\n", face[i].elem_nr);
            //else if(face[i].nr==1) fprintf( handle, "%d, SP\n", face[i].elem_nr);
            if((face[i].nr==1)&&(orif==1)) fprintf( handle, "%d, SP \n", face[i].elem_nr);
            else if((face[i].nr==1)&&(orif==0)) fprintf( handle, "%d, SN \n", face[i].elem_nr);
            else fprintf( handle, "%d, S%d\n", face[i].elem_nr, face[i].nr-1);
	  }
	  else
	  {
            //if(face[i].nr==0) fprintf( handle, "%d, SNEG\n", face[i].elem_nr);
            //else if(face[i].nr==1) fprintf( handle, "%d, SPOS\n", face[i].elem_nr);
            if((face[i].nr==1)&&(orif==1)) fprintf( handle, "%d, SPOS \n", face[i].elem_nr);
            else if((face[i].nr==1)&&(orif==0)) fprintf( handle, "%d, SNEG \n", face[i].elem_nr);
            else fprintf( handle, "%d, S%d\n", face[i].elem_nr, face[i].nr+1);
	  }
	}
        else fprintf( handle, "%d, S%d\n", face[i].elem_nr, face[i].nr+1);
      }
    }
    fclose(handle);
  }
  else
  {
    errMsg(" ERROR: format %s not yet supported\n", format );
  }
  printf (" ready\n");
}



