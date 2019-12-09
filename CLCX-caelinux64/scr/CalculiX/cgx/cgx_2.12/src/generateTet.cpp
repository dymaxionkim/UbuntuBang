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

#ifndef CGXCDSM_H
#define CGXCDSM_H


#ifdef __cplusplus
extern "C" {
#endif
#include <cgx.h>
#ifdef __cplusplus
}
#endif

#endif

extern double     gtol;

extern char  printFlag;                   /* printf 1:on 0:off */

extern Scale     scale[1];
extern Summen    anz[1];
extern Nodes     *node;
extern Elements  *e_enqire;

extern SumGeo    anzGeo[1];
extern Gbod      *body;
extern Gsur      *surf;
extern Sets      *set;
extern Sets      *setx;

using namespace std;

/*
namespace nglib {
#include <nglib.h>
}
*/

int generateTetFromSet(int setNr, double teth )
{
  //using namespace nglib;

  int e,i,j,k,n;
  int snodSet, np, ne, ngtet[10], cgxtet[10], sumtri3=0, sumtri6=0,n1,n2,nm;
  //double ngpnt[3];
  static int *cgxnode=NULL;
  static int *ngnode=NULL;
  char buffer[1000];

  typedef struct {
    int sum, *n2, *nm;
  }N1nm;
  N1nm *n1nm=NULL;

  int nodseq_tr6[]={0,3,1,1,4,2,2,5,0};
  int nodseq_te10[]={0,4,1,1,5,2,2,6,0, 0,7,3,1,8,3,2,9,3};

  Summen    anz_ng[1];
  Nodes     *node_ng=NULL;
  Elements  *elem_ng=NULL;

  //Ng_Mesh * mesh;

  anz_ng->orign=0;
  anz_ng->n=0;
  anz_ng->e=0;
  anz_ng->f=0;
  anz_ng->g=0;
  anz_ng->t=0;
  anz_ng->l=0;
  anz_ng->olc=0;
  anz_ng->orignmax=0;
  anz_ng->nmax=0;
  anz_ng->nmin=MAX_INTEGER;
  anz_ng->emax=0;
  anz_ng->emin=MAX_INTEGER;
  anz_ng->sets=0;
  anz_ng->mats=0;
  anz_ng->amps=0;
  anz_ng->noffs=0;
  anz_ng->eoffs=0;

  // the address stored in setx could still be in use (ie. set)
  // therefore a fresh start is needed
  setx=NULL;

  //Ng_Meshing_Parameters mp;
  //mp.maxh = teth/scale->w;
  //mp.fineness = 1.;

  // check the consistency of the surface mesh. Either all tr6 or tr3
  // generate a list of surface nodes which will be passed to NG

  delSet("+snodSet");
  delSet("+velemSet");
  snodSet=pre_seta("+snodSet","i",0);
  for (i = 0; i < set[setNr].anz_e; i++)
  {
    if (e_enqire[set[setNr].elem[i]].type == 7) sumtri3++;
    else if (e_enqire[set[setNr].elem[i]].type == 8) sumtri6++;
    else
    {
      printf("ERROR: mesh in set %s contains not only triangle elements:%d\n",set[setNr].name, set[setNr].elem[i]);
      return(0);
    }
    for(k=0; k<3; k++) seta(snodSet,"n",e_enqire[set[setNr].elem[i]].nod[k]);
  }
  if((sumtri3)&&(sumtri3!=set[setNr].anz_e))
  {
    printf("ERROR: mesh in set %s contains not only tr3 elements\n",set[setNr].name);
    return(0);
  }
  if((sumtri6)&&(sumtri6!=set[setNr].anz_e))
  {
    printf("ERROR: mesh in set %s contains not only tr6 elements\n",set[setNr].name);
    return(0);
  }

  // creates mesh structure
  //Ng_Init();
  //mesh = Ng_NewMesh ();

  // add the nodes and elements to mesh
  if ((ngnode = (int *)realloc((int *)ngnode, (anz->nmax+1)*sizeof(int)) ) == NULL )
    { errMsg("ERROR: realloc failure in generateTet\n"); return(0); }

  FILE *handle = fopen ("nodnr_ng_cgx.out", "w");
  FILE *handle_ng = fopen ("mesh.ng", "w");

  fprintf (handle_ng, "%d\n", set[snodSet].anz_n);
  for (i = 0; i < set[snodSet].anz_n; i++)
  {
    //Ng_AddPoint (mesh, &node[set[snodSet].node[i]].nx);                             
    ngnode[set[snodSet].node[i]]=i+1;
    fprintf(handle,"ng %d cgx %d\n", i+1,set[snodSet].node[i]);
    fprintf (handle_ng, "%.12e %.12e %.12e\n", node[set[snodSet].node[i]].nx, node[set[snodSet].node[i]].ny, node[set[snodSet].node[i]].nz);
  }
  fclose(handle);

  handle = fopen ("elemnr_ng_cgx.out", "w");

  fprintf (handle_ng, "%d\n", set[setNr].anz_e);
  for (i = 0; i < set[setNr].anz_e; i++)
  {
    for(k=0; k<3; k++) ngtet[k]=ngnode[e_enqire[set[setNr].elem[i]].nod[k]];
    //Ng_AddSurfaceElement (mesh, NG_TRIG, ngtet);
    fprintf(handle,"ng %d cgx %d\n", i+1,set[setNr].elem[i]);
    for(k=0; k<3; k++) fprintf (handle_ng, " %d",ngnode[e_enqire[set[setNr].elem[i]].nod[k]]); fprintf (handle_ng, "\n");
  }
  fclose(handle);
  fclose(handle_ng);

  // mesh
  // either linked:
  //Ng_GenerateVolumeMesh (mesh, &mp);
  // or as stand alone mesher:
  sprintf(buffer, "ng_vol mesh.ng %f%d", teth/scale->w, 0);
  system(buffer);
  printf (" Try to read NG file\n\n");
  if ( (  readNG( "test.vol", anz_ng, &setx, &node_ng, &elem_ng, NULL)) == -1) 
  {
    printf("ERROR: No mesh-file found\n\n");
    return(0);
  }
  for(j=0; j<anz_ng->sets; j++) {  delSetx(setx[j].name); }
  free(setx); setx=NULL;
  if(anz_ng->e>0)
  {
    system("rm -rf mesh.ng");
    system("rm -rf test.vol");
    system("rm -rf test.out");
    system("rm -rf netgen.prof");
    system("rm -rf nodnr_ng_cgx.out elemnr_ng_cgx.out");
  }
  else
  {
    printf("ERROR: No valid mesh found, temporary files still exists for debugging\n\n");
    return(0);
  }

  // define new nodes and tets
  //np = Ng_GetNP(mesh);
  np = anz_ng->n;
  if ((cgxnode = (int *)realloc((int *)cgxnode, (np+1)*sizeof(int)) ) == NULL )
    { errMsg("ERROR: realloc failure in generateTet\n"); return(0); }
  for (i = 0; i < np; i++)
  {
    // store the new nodenr
    if(i<set[snodSet].anz_n) cgxnode[i+1]=set[snodSet].node[i];
    else
    {
      cgxnode[i+1]=anz->nmax+1;
      //Ng_GetPoint (mesh, i+1, ngpnt);
      //nod( anz, &node, 1, cgxnode[i+1], ngpnt[0], ngpnt[1], ngpnt[2], 0 );     
      nod( anz, &node, 1, cgxnode[i+1], node_ng[i+1].nx, node_ng[i+1].ny, node_ng[i+1].nz, 0 );     
    } 
  }
  delSet( set[snodSet].name );

  /* create a table for all nodes which points to already created midside nodes, surface-elements must still exist */
  if(sumtri6)
  {
    if ( (n1nm = (N1nm *)malloc( (anz->nmax+1) * sizeof(N1nm))) == NULL )
    { printf("\n\n ERROR in mids: malloc\n\n") ; exit(-1); }    
    for (i=0; i<=anz->nmax; i++) n1nm[i].sum=0;
    for (i=0; i<=anz->nmax; i++) n1nm[i].n2=n1nm[i].nm=NULL;
    for (k = 0; k < set[setNr].anz_e; k++)
    {
      for (n=0; n<3; n++)
      {
        n1=e_enqire[set[setNr].elem[k]].nod[nodseq_tr6[n*3]];
        n2=e_enqire[set[setNr].elem[k]].nod[nodseq_tr6[n*3+2]];

        /* check if the nm exists already */
        nm=-1;
        for(i=0; i<n1nm[n1].sum; i++) if(n1nm[n1].n2[i]==n2) nm=n1nm[n1].nm[i];
        for(i=0; i<n1nm[n2].sum; i++) if(n1nm[n2].n2[i]==n1) nm=n1nm[n2].nm[i];

        if(nm==-1)
        {
          nm=e_enqire[set[setNr].elem[k]].nod[nodseq_tr6[n*3+1]];

          if ( (n1nm[n1].n2 = (int *)realloc( n1nm[n1].n2, (n1nm[n1].sum+1) * sizeof(int))) == NULL )
          { printf("\n\n ERROR in mids: realloc\n\n") ; exit(-1); }    
          if ( (n1nm[n1].nm = (int *)realloc( n1nm[n1].nm, (n1nm[n1].sum+1) * sizeof(int))) == NULL )
          { printf("\n\n ERROR in mids: realloc\n\n") ; exit(-1); }    
          n1nm[n1].n2[n1nm[n1].sum]=n2;
          n1nm[n1].nm[n1nm[n1].sum]=nm;
          n1nm[n1].sum++;
        }
      }
    }
  }

  // remove surface elements from surfaces and sets except setNr
  for (k=0; k<anzGeo->s; k++)
  {  
    for (n=0; n<surf[k].ne; n++)
    {
      for (j=0; j<set[setNr].anz_e; j++)
      {
        if(surf[k].elem[n]==set[setNr].elem[j]) surf[k].elem[n]=0;
  	}
    }
    e=0;
    for(n=0; n<surf[k].ne; n++) if(surf[k].elem[n]>0) surf[k].elem[e++]=surf[k].elem[n];
    surf[k].ne=e; 
  }

  for (k=0; k<anz->sets; k++)
  {
    if(( set[k].name != (char *)NULL )&&( k != setNr))
    {
      for (j=0; j<set[setNr].anz_e; j++) setr( k, "e", set[setNr].elem[j]);
    }
  }

  //ne = Ng_GetNE(mesh);
  ne = anz_ng->e;
  //for (i = 0; i < ne; i++)
  i=0; for (j = 0; j < ne; j++)
  {
    //if(Ng_GetVolumeElement (mesh, i+1, ngtet)!=NG_TET) printf("ERROR: e:%d not tet\n",i+1);
    //printf("e:%d type:tet \n",i+1);
    if(elem_ng[j].type != 3) continue;
    ngtet[0]=elem_ng[j].nod[0];
    ngtet[2]=elem_ng[j].nod[1];
    ngtet[1]=elem_ng[j].nod[2];
    ngtet[3]=elem_ng[j].nod[3];
    //printf("  n:%d %d %d %d\n", ngtet[0],ngtet[1],ngtet[2],ngtet[3]);
    
    cgxtet[0]=cgxnode[ngtet[0]];
    cgxtet[1]=cgxnode[ngtet[2]];
    cgxtet[2]=cgxnode[ngtet[1]];
    cgxtet[3]=cgxnode[ngtet[3]];
    if(i<set[setNr].anz_e) elem_define( set[setNr].elem[i], 3, cgxtet, 1, 0 );
    else { elem_define( anz->emax+1, 3, cgxtet, 1, 0 ); seta( setNr, "e", anz->emax ); }
    i++;
  }
  ne = i;
  free(node_ng);
  free(elem_ng);
  //printf("set[].flag:%c i:%d e:%d e-i:%d\n", set[setNr].flag, i,set[setNr].anz_e, set[setNr].anz_e-i);

  // delete remaining surface elements
  if (i<set[setNr].anz_e) delElem( set[setNr].anz_e-i, &set[setNr].elem[i] ) ;

  /* generate midside nodes */
  if(sumtri6)
  {
    fixMidsideNodes( set[setNr].name, "gen" );

    /* change coords of surface-midside nodes */
    snodSet=pre_seta("+snodSet","i",0);
    for (k = 0; k < set[setNr].anz_e; k++)
    {
        for (n=0; n<6; n++)
        {
          n1=e_enqire[set[setNr].elem[k]].nod[nodseq_te10[n*3]];
          n2=e_enqire[set[setNr].elem[k]].nod[nodseq_te10[n*3+2]];

          /* check if the nm is known */
          nm=-1;
          for(i=0; i<n1nm[n1].sum; i++) if(n1nm[n1].n2[i]==n2) nm=n1nm[n1].nm[i];
          for(i=0; i<n1nm[n2].sum; i++) if(n1nm[n2].n2[i]==n1) nm=n1nm[n2].nm[i];

          if(nm!=-1)
	  {
            /* change node */
            //printf(" change node:%d to %d\n", e_enqire[set[setNr].elem[k]].nod[nodseq_te10[n*3+1]],nm);
            seta(snodSet,"n",e_enqire[set[setNr].elem[k]].nod[nodseq_te10[n*3+1]]); 
            e_enqire[set[setNr].elem[k]].nod[nodseq_te10[n*3+1]]=nm;
	  }
        }
    }
    zap( set[snodSet].name );
    fixMidsideNodes( set[setNr].name, "" );
  }

  //Ng_Exit();
  printf("tet-mesh done with h:%f\n",teth);
  return(ne);
}



int generateTetFromBody(int nr, double teth )
{
  //using namespace nglib;

  int i,j,k,n,s,sb;
  int setNr, snodSet, snodSet2, mnodSet, np, ne, ngtet[10], cgxtet[10], sumtri=0, sumtri3=0, sumtri6=0,n1,n2,nm;
  //double ngpnt[3];
  static int *cgxnode=NULL;
  static int *ngnode=NULL;
  char buffer[1000];
  int tryToFlipBody=0;

  typedef struct {
    int sum, *n2, *nm;
  }N1nm;
  N1nm *n1nm=NULL;

  int nodseq_tr6[]={0,3,1,1,4,2,2,5,0};
  int nodseq_te10[]={0,4,1,1,5,2,2,6,0, 0,7,3,1,8,3,2,9,3};

  Summen    anz_ng[1];
  Nodes     *node_ng=NULL;
  Elements  *elem_ng=NULL;

  //Ng_Mesh * mesh;

  anz_ng->orign=0;
  anz_ng->n=0;
  anz_ng->e=0;
  anz_ng->f=0;
  anz_ng->g=0;
  anz_ng->t=0;
  anz_ng->l=0;
  anz_ng->olc=0;
  anz_ng->orignmax=0;
  anz_ng->nmax=0;
  anz_ng->nmin=MAX_INTEGER;
  anz_ng->emax=0;
  anz_ng->emin=MAX_INTEGER;
  anz_ng->sets=0;
  anz_ng->mats=0;
  anz_ng->amps=0;
  anz_ng->noffs=0;
  anz_ng->eoffs=0;

  // the address stored in setx could still be in use (ie. set)
  // therefore a fresh start is needed
  setx=NULL;

  //Ng_Meshing_Parameters mp;
  //mp.maxh = teth/scale->w;
  //mp.fineness = 1.;

  // check the consistency of the surface mesh. Either all tr6 or tr3
  // generate a list of surface nodes which will be passed to NG

  
  delSet("+snodSet");
  delSet("+mnodSet");
  delSet("+velemSet");
  snodSet=pre_seta("+snodSet","i",0);
  mnodSet=pre_seta("+mnodSet","i",0);
  setNr=pre_seta("+velemSet","i",0);
  for(sb=0; sb<body[nr].ns; sb++)
  {
    s=body[nr].s[sb];
    for (i = 0; i < surf[s].ne; i++)
    {
      sumtri++;
      if (e_enqire[surf[s].elem[i]].type == 7) sumtri3++;
      else if (e_enqire[surf[s].elem[i]].type == 8)
      {
        sumtri6++;
        for(k=0; k<3; k++) seta(mnodSet,"n",e_enqire[surf[s].elem[i]].nod[k+3]);
      }
      else
      {
        printf("ERROR: mesh in surf %s contains not only triangle elements:%d\n",surf[s].name, surf[s].elem[i]);
        return(0);
      }
      for(k=0; k<3; k++) seta(snodSet,"n",e_enqire[surf[s].elem[i]].nod[k]);
    }
  }
  if((sumtri3)&&(sumtri3!=sumtri))
  {
    printf("ERROR: surface-mesh in body %s contains not only tr3 elements\n",body[nr].name);
    return(0);
  }
  if((sumtri6)&&(sumtri6!=sumtri))
  {
    printf("ERROR: surface-mesh in body %s contains not only tr6 elements\n",body[nr].name);
    return(0);
  }

  // creates mesh structure
  //Ng_Init();
  //mesh = Ng_NewMesh ();

  // add the nodes and elements to mesh
  if ((ngnode = (int *)realloc((int *)ngnode, (anz->nmax+1)*sizeof(int)) ) == NULL )
    { errMsg("ERROR: realloc failure in generateTet\n"); return(0); }

 tryToFlipBodyMark:;
  FILE *handle = fopen ("nodnr_ng_cgx.out", "w");
  FILE *handle_ng = fopen ("mesh.ng", "w");

  fprintf (handle_ng, "%d\n", set[snodSet].anz_n);
  for (i = 0; i < set[snodSet].anz_n; i++)
  {
    //Ng_AddPoint (mesh, &node[set[snodSet].node[i]].nx);                             
    ngnode[set[snodSet].node[i]]=i+1;
    fprintf(handle,"ng %d cgx %d\n", i+1,set[snodSet].node[i]);
    fprintf (handle_ng, "%.12e %.12e %.12e\n", node[set[snodSet].node[i]].nx, node[set[snodSet].node[i]].ny, node[set[snodSet].node[i]].nz);
  }
  fclose(handle);

  handle = fopen ("elemnr_ng_cgx.out", "w");

  //printf("body:%s\n", body[nr].name);
  n=0; for(sb=0; sb<body[nr].ns; sb++) n+=surf[body[nr].s[sb]].ne;
  fprintf (handle_ng, "%d\n", n);
  for(sb=0; sb<body[nr].ns; sb++)
  {
    s=body[nr].s[sb];
    n=1;
    if(body[nr].o[sb]=='-') n*=-1;
    if(body[nr].ori=='-') n*=-1;
    if(tryToFlipBody) n*=-1;
    if(n==-1)
    {
      printf("-surf:%s\n", surf[s].name);
      for (i = 0; i < surf[s].ne; i++)
      {
        j=2; for(k=0; k<3; k++) ngtet[k]=ngnode[e_enqire[surf[s].elem[i]].nod[j--]];
        //Ng_AddSurfaceElement (mesh, NG_TRIG, ngtet);
        fprintf(handle,"ng %d cgx %d\n", i+1,surf[s].elem[i] );
        for(k=0; k<3; k++) fprintf (handle_ng, " %d",ngtet[k]); fprintf (handle_ng, "\n");
      }
    }
    else
    {
      printf("+surf:%s\n", surf[s].name);
      for (i = 0; i < surf[s].ne; i++)
      {
        for(k=0; k<3; k++) ngtet[k]=ngnode[e_enqire[surf[s].elem[i]].nod[k]];
        //Ng_AddSurfaceElement (mesh, NG_TRIG, ngtet);
        fprintf(handle,"ng %d cgx %d\n", i+1,surf[s].elem[i] );
        for(k=0; k<3; k++) fprintf (handle_ng, " %d",ngtet[k]); fprintf (handle_ng, "\n");
      }
    }
  }
  fclose(handle);
  fclose(handle_ng);

  // mesh
  // either linked:
  //Ng_GenerateVolumeMesh (mesh, &mp);
  // or as stand alone mesher:
  sprintf(buffer, "ng_vol mesh.ng %f%d", teth/scale->w, 0);
  system(buffer);
  printf (" Try to read NG file\n\n");
  if ( (  readNG( "test.vol", anz_ng, &setx, &node_ng, &elem_ng, NULL)) == -1) 
  {
    if(tryToFlipBody)
    {
      printf("ERROR: No mesh-file found\n\n");
      return(0);
    }
    else { tryToFlipBody=1; goto tryToFlipBodyMark; }
  }
  for(j=0; j<anz_ng->sets; j++) {  delSetx(setx[j].name); }
  free(setx); setx=NULL;
  if(anz_ng->e>0)
  {
    system("rm -rf mesh.ng");
    system("rm -rf test.vol");
    system("rm -rf test.out");
    system("rm -rf netgen.prof");
    system("rm -rf nodnr_ng_cgx.out elemnr_ng_cgx.out");
  }
  else
  {
    printf("ERROR: No valid mesh found, temporary files still exists for debugging\n\n");
    return(0);
  }

  // define new nodes and tets
  //np = Ng_GetNP(mesh);
  np = anz_ng->n;
  if ((cgxnode = (int *)realloc((int *)cgxnode, (np+1)*sizeof(int)) ) == NULL )
    { errMsg("ERROR: realloc failure in generateTet\n"); return(0); }
  for (i = 0; i < np; i++)
  {
    // store the new nodenr
    if(i<set[snodSet].anz_n) cgxnode[i+1]=set[snodSet].node[i];
    else
    {
      cgxnode[i+1]=anz->nmax+1;
      //Ng_GetPoint (mesh, i+1, ngpnt);
      //nod( anz, &node, 1, cgxnode[i+1], ngpnt[0], ngpnt[1], ngpnt[2], 0 );     
      nod( anz, &node, 1, cgxnode[i+1], node_ng[i+1].nx, node_ng[i+1].ny, node_ng[i+1].nz, 0 );     
    } 
  }

  /* create a table for all nodes which points to already created midside nodes, surface-elements must still exist */
  if(sumtri6)
  {
    if ( (n1nm = (N1nm *)malloc( (anz->nmax+1) * sizeof(N1nm))) == NULL )
    { printf("\n\n ERROR in mids: malloc\n\n") ; exit(-1); }    
    for (i=0; i<=anz->nmax; i++) n1nm[i].sum=0;
    for (i=0; i<=anz->nmax; i++) n1nm[i].n2=n1nm[i].nm=NULL;
    for(sb=0; sb<body[nr].ns; sb++)
    {
      s=body[nr].s[sb];
      for (k = 0; k < surf[s].ne; k++)
      {
        for (n=0; n<3; n++)
        {
          n1=e_enqire[surf[s].elem[k]].nod[nodseq_tr6[n*3]];
          n2=e_enqire[surf[s].elem[k]].nod[nodseq_tr6[n*3+2]];
    
          /* check if the nm exists already */
          nm=-1;
          for(i=0; i<n1nm[n1].sum; i++) if(n1nm[n1].n2[i]==n2) nm=n1nm[n1].nm[i];
          for(i=0; i<n1nm[n2].sum; i++) if(n1nm[n2].n2[i]==n1) nm=n1nm[n2].nm[i];
    
          if(nm==-1)
          {
            nm=e_enqire[surf[s].elem[k]].nod[nodseq_tr6[n*3+1]];
    
            if ( (n1nm[n1].n2 = (int *)realloc( n1nm[n1].n2, (n1nm[n1].sum+1) * sizeof(int))) == NULL )
            { printf("\n\n ERROR in mids: realloc\n\n") ; exit(-1); }    
            if ( (n1nm[n1].nm = (int *)realloc( n1nm[n1].nm, (n1nm[n1].sum+1) * sizeof(int))) == NULL )
            { printf("\n\n ERROR in mids: realloc\n\n") ; exit(-1); }    
            n1nm[n1].n2[n1nm[n1].sum]=n2;
            n1nm[n1].nm[n1nm[n1].sum]=nm;
            n1nm[n1].sum++;
          }
        }
      }
    }
  }

  //ne = Ng_GetNE(mesh);
  ne = anz_ng->e;

  /* allocate memory for embeded elements */
  if((body[nr].elem=(int *)realloc((int *)body[nr].elem, (ne)*sizeof(int)) )==NULL)
  { printf(" ERROR: realloc failure in generateTet body:%s can not be meshed\n\n" , body[nr].name); return(0); }
  if((body[nr].nod=(int *)realloc((int *)body[nr].nod, (ne)*sizeof(int)) )==NULL)
  { printf(" ERROR: realloc failure in generateTet body:%s can not be meshed\n\n" , body[nr].name); return(0); }

  //for (i = 0; i < ne; i++)
  i=0; for (j = 0; j < ne; j++)
  {
    //if(Ng_GetVolumeElement (mesh, i+1, ngtet)!=NG_TET) printf("ERROR: e:%d not tet\n",i+1);
    //printf("e:%d type:tet \n",i+1);
    if(elem_ng[j].type != 3) continue;
    ngtet[0]=elem_ng[j].nod[0];
    ngtet[2]=elem_ng[j].nod[1];
    ngtet[1]=elem_ng[j].nod[2];
    ngtet[3]=elem_ng[j].nod[3];
    //printf("  n:%d %d %d %d\n", ngtet[0],ngtet[1],ngtet[2],ngtet[3]);
    
    cgxtet[0]=cgxnode[ngtet[0]];
    cgxtet[1]=cgxnode[ngtet[2]];
    cgxtet[2]=cgxnode[ngtet[1]];
    cgxtet[3]=cgxnode[ngtet[3]];
    elem_define( anz->emax+1, 3, cgxtet, 1, 0 ); seta( setNr, "e", anz->emax );
    body[nr].elem[i]=anz->emax;
    i++;
  }
  ne = i;
  free(node_ng);
  free(elem_ng);
  body[nr].ne=ne;

  /* generate midside nodes */
  if(sumtri6)
  {
    fixMidsideNodes( set[setNr].name, "gen" );

    /* change coords of surface-midside nodes */
    snodSet2=pre_seta("+snodSet2","i",0);

    for (k = 0; k < set[setNr].anz_e; k++)
    {
      for (n=0; n<6; n++)
      {
        n1=e_enqire[set[setNr].elem[k]].nod[nodseq_te10[n*3]];
        n2=e_enqire[set[setNr].elem[k]].nod[nodseq_te10[n*3+2]];

        /* check if the nm is known */
        nm=-1;
        for(i=0; i<n1nm[n1].sum; i++) if(n1nm[n1].n2[i]==n2) nm=n1nm[n1].nm[i];
        for(i=0; i<n1nm[n2].sum; i++) if(n1nm[n2].n2[i]==n1) nm=n1nm[n2].nm[i];

        if(nm!=-1)
    	{
          /* change node */
          //printf(" change node:%d to %d\n", e_enqire[surf[s].elem[k]].nod[nodseq_te10[n*3+1]],nm);
          seta(snodSet2,"n",e_enqire[set[setNr].elem[k]].nod[nodseq_te10[n*3+1]]); 
          e_enqire[set[setNr].elem[k]].nod[nodseq_te10[n*3+1]]=nm;
    	}
      }
    }

    /* add body-nodes */
    for (k = 0; k < set[setNr].anz_e; k++)
      for (n=0; n<10; n++) seta( setNr, "n", e_enqire[set[setNr].elem[k]].nod[n] );

    zap( set[snodSet2].name );
    fixMidsideNodes( set[setNr].name, "" );
  }
  else
  {
    /* add body-nodes */
    for (k = 0; k < set[setNr].anz_e; k++)
      for (n=0; n<4; n++) seta( setNr, "n", e_enqire[set[setNr].elem[k]].nod[n] );
  }

  /* remove surface nodes */
  for (k = 0; k < set[snodSet].anz_n; k++) setr(setNr,"n",set[snodSet].node[k]);
  for (k = 0; k < set[mnodSet].anz_n; k++) setr(setNr,"n",set[mnodSet].node[k]);

  /* add to body */
  if((body[nr].nod=(int *)realloc((int *)body[nr].nod, (set[setNr].anz_n)*sizeof(int)) )==NULL)
  { printf(" ERROR: realloc failure in generateTet body:%s can not be meshed\n\n" , body[nr].name); return(0); }
  for (k = 0; k < set[setNr].anz_n; k++) body[nr].nod[k]=set[setNr].node[k];
  body[nr].nn=set[setNr].anz_n;

  delSet("+snodSet");
  delSet("+mnodSet");
  delSet("+velemSet");


  //Ng_Exit();
  printf("tet-mesh done with h:%f\n",teth);
  return(ne);
}
