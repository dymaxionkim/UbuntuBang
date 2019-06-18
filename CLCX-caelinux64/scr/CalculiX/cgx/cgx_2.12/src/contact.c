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

#define TEST 0
#define     N_CLOSEST_NODES 10

extern SpecialSet specialset[1];
extern char  printFlag;                     /* printf 1:on 0:off */
extern Sets      *set;                               /* must! else pre_seta will change the address */


int mergeCluster(Cluster *cluster, int c1, int c2)
{
  int i,n;
  int sum_n=0, *cnode=NULL, mergFlag=0;

  /* add both node-lists */
  if((cnode =(int *)malloc((cluster[c1].nn + cluster[c2].nn+1) *sizeof(int)))==NULL)
  { errMsg("\n\n ERROR: malloc failed for cluster\n" ); return(-1); }
  for(i=0; i<cluster[c1].nn; i++) cnode[sum_n++]=cluster[c1].node[i];
  for(i=0; i<cluster[c2].nn; i++) cnode[sum_n++]=cluster[c2].node[i];

  /* sort both node-lists */
  qsort( cnode, sum_n, sizeof(int), (void *)compareInt );
#if TEST
  for (i=0; i<sum_n; i++)
    printf("1 %d n:%d\n", i, cnode[i]); 
#endif


  /* go over the node-list and look for double nodenr */
  /* if yes, delete second node from list */
  for (i=0; i<sum_n-1; i++)
  {
    if(cnode[i]==cnode[i+1])
    {
      mergFlag=1;
      cnode[i]=0;
    }
  }

  if(mergFlag)
  {
    n=0;
    for (i=0; i<sum_n; i++)
    {
      if(cnode[i])
      {
        cnode[n]=cnode[i];
        n++;
      }
    }
    sum_n=n;
    /* the first original cluster gets the new address of the node-list */
    free(cluster[c1].node);
    cluster[c1].node=cnode;
    cluster[c1].nn=sum_n;

    /* and all faces of the second cluster in addition */
    if((cluster[c1].face =(int *)realloc((int *)cluster[c1].face, (cluster[c1].nf+cluster[c2].nf) *sizeof(int)))==NULL)
    { errMsg("\n\n ERROR: malloc failed for cluster\n" ); return(-1); }
    for(i=0; i<cluster[c2].nf; i++) cluster[c1].face[cluster[c1].nf++]=cluster[c2].face[i];

    /* delete the second original cluster */
    free(cluster[c2].node);
    free(cluster[c2].face);
    cluster[c2].nn=0;
    cluster[c2].nf=0;
    return(1);
  }

  /* if no, return(0) without action */
  free(cnode);
  return(0);
}


int getMeshSections(int setNr, Faces *face, Nodes *node)
{
  int e,i,f,n=0, c1,c2;
  int sum_c=0, mergFlag;
  static Cluster *cluster=NULL;
  char buffer[MAX_LINE_LENGTH], setname[MAX_LINE_LENGTH];


  /* initialize cluster of faces with just one face */
  for (f=0; f<set[setNr].anz_f; f++)
  {
    if((cluster =(Cluster *)realloc((Cluster *)cluster, (sum_c+1) *sizeof(Cluster)))==NULL)
    {
      errMsg("\n\n ERROR: realloc failed for cluster\n" );
      return(-1);
    }

    if (face[set[setNr].face[f]].type == 7)       n = 3;  /* TRI3  */
    else if (face[set[setNr].face[f]].type == 8)  n = 6;  /* TRI6  */
    else if (face[set[setNr].face[f]].type == 9)  n = 4;  /* QUAD4 */
    else if (face[set[setNr].face[f]].type == 10) n = 8;  /* QUAD8 */
    else n=0;
    
    if((cluster[sum_c].node =(int *)malloc((n) *sizeof(int)))==NULL)
    {
      errMsg("\n\n ERROR: malloc failed for cluster\n" );
      return(-1);
    }
    if((cluster[sum_c].face =(int *)malloc((1) *sizeof(int)))==NULL)
    {
      errMsg("\n\n ERROR: malloc failed for cluster\n" );
      return(-1);
    }

    for(i=0; i<n; i++) cluster[sum_c].node[i]=face[set[setNr].face[f]].nod[i];
    cluster[sum_c].nn=n;
    cluster[sum_c].face[0]=set[setNr].face[f];
    cluster[sum_c].nf=1;
    sum_c++;
  }

  /* go over all clusters and merge cluster which have at least one common node */
  do
  {
    mergFlag=0;
    for (c1=0; c1<sum_c-1; c1++) if(cluster[c1].nn>0)
    {
      for (c2=c1+1; c2<sum_c; c2++) if(cluster[c2].nn>0)
      {
        i=mergeCluster(cluster, c1, c2 );
        if(i>0) mergFlag=1;
      }
    }
  }
  while (mergFlag==1);

  /* stop if no cluster found another cluster with a common node */
  /* define a set per remaining cluster */
  e=sum_c;
  sum_c=0;
  for (c1=0; c1<e; c1++) if(cluster[c1].nn>0)
  {
    sum_c++;
    sprintf( setname, "%s%d", specialset->cf,sum_c); 
    for (n=0; n<cluster[c1].nn; n++)
    {
      sprintf( buffer, "%d", cluster[c1].node[n]); 
      pre_seta(setname, "n", buffer);
    }
    for (n=0; n<cluster[c1].nf; n++)
    {
      sprintf( buffer, "%d", cluster[c1].face[n]); 
      pre_seta(setname, "f", buffer);
    }
  }
  return(sum_c);
}



/* search dependent nodes of unconnected sets of faces */
/* return depnodes */


int getDepNodes( Summen *anz, int set1, int set2, Elements *e_enqire, Faces *face, Nodes *node, double tol)
{
  int  i, j, n, e, f;
  int  n_closest_nodes, depset, eset;

  char setname[MAX_LINE_LENGTH];

  int    elbuf=0;
  double dx,dy,dz;
  double xmin,ymin,zmin,xmax,ymax,zmax;
  double offset=0., min_offset, offsetbuf=0.;

  Rsort *rsort=NULL;

  int *dep=NULL, *ind=NULL;
  int sum_dep=0, sum_ind=0;

  CTri3     *ctri3=NULL;             /* triangulierte indep-nodes flaeche */
  static int **tri3_index;           /* am jeweiligen node haengenden dreiecke */
  int       *ntri_nodes=NULL;        /* anzahl der anhaengenden dreiecke       */
  int       sum_tri3;                /* anzahl der ctri3 */

  /* get elem-faces and a reference from nodes to faces */
  delSet("-eset");
  eset=pre_seta("-eset", "i", 0);
  for (f=0; f<set[set2].anz_f; f++)
  {
    i=set[set2].face[f];
    seta(eset, "e", face[i].elem_nr);
  }

  sum_tri3 = makeTriFromElems(set2, eset, anz->nmax, set, e_enqire, &ctri3, &tri3_index, &ntri_nodes);
  delSet("-eset");

  if(printFlag) printf (" %d ctri3 created from elements\n\n", sum_tri3 );
  if(!sum_tri3)
  {
    errMsg ("ERROR: found no valid element\n\n" );
    return(-1);
  }


  /* get the dimensions of a cubus in which the set2-nodes are located */ 
  xmin=ymin=zmin=MAX_FLOAT;
  xmax=ymax=zmax=-MAX_FLOAT;
  for (j=0; j<set[set2].anz_n; j++)
  {
    if(xmin > node[set[set2].node[j]].nx) xmin = node[set[set2].node[j]].nx;
    if(xmax < node[set[set2].node[j]].nx) xmax = node[set[set2].node[j]].nx;
    if(ymin > node[set[set2].node[j]].ny) ymin = node[set[set2].node[j]].ny;
    if(ymax < node[set[set2].node[j]].ny) ymax = node[set[set2].node[j]].ny;
    if(zmin > node[set[set2].node[j]].nz) zmin = node[set[set2].node[j]].nz;
    if(zmax < node[set[set2].node[j]].nz) zmax = node[set[set2].node[j]].nz;
  }

  xmax+=tol;
  xmin-=tol;
  ymax+=tol;
  ymin-=tol;
  zmax+=tol;
  zmin-=tol;

  if ( (rsort = (Rsort *)malloc( (set[set2].anz_n+1) * sizeof(Rsort))) == NULL )
  {  printf("ERROR: realloc failed: Rsort\n\n" );     return(-1); }

  if((int)N_CLOSEST_NODES<set[set2].anz_n) n_closest_nodes=(int)N_CLOSEST_NODES; else n_closest_nodes=set[set2].anz_n;

  for (i=0; i<set[set1].anz_n; i++ )
  {
    if(node[set[set1].node[i]].nx > xmax) goto next_node;
    if(node[set[set1].node[i]].nx < xmin) goto next_node;
    if(node[set[set1].node[i]].ny > ymax) goto next_node;
    if(node[set[set1].node[i]].ny < ymin) goto next_node;
    if(node[set[set1].node[i]].nz > zmax) goto next_node;
    if(node[set[set1].node[i]].nz < zmin) goto next_node;

    /* suche die naechst-liegenden indep-nodes  */
    for (j=0; j<set[set2].anz_n; j++)
    {
      dx= node[set[set2].node[j]].nx - node[set[set1].node[i]].nx;
      dy= node[set[set2].node[j]].ny - node[set[set1].node[i]].ny;
      dz= node[set[set2].node[j]].nz - node[set[set1].node[i]].nz;      
      rsort[j].i=set[set2].node[j];
      rsort[j].r=dx*dx + dy*dy + dz*dz;
    }
    qsort( rsort, set[set2].anz_n, sizeof(Rsort), (void *)compareRsort );
#if TEST
    for (j=0; j<n_closest_nodes; j++)
      printf("%d n:%d r:%lf\n", j, rsort[j].i, sqrt(rsort[j].r)); 
#endif
    if(printFlag)
      printf("                              node:%d, found close node:%d at dist:%lf\n", set[set1].node[i], rsort[0].i, sqrt(rsort[0].r));

    /* knoten liegt zwischen den indep-nodes, suche das passende element.       */
    /* fange mit den elementen des naechstliegenden nodes an, und kontrolliere   */
    /* mit dem vectorprodukt welche elementflaeche den node umschliest          */

    min_offset=MAX_INTEGER;
    for (n=0; n<n_closest_nodes; n++)
    {
      for (j=0; j<ntri_nodes[ rsort[n].i ]; j++)
      {
        e=tri3_index[ rsort[n].i ][j];
    	    
        if(printFlag)
          printf("checke n:%d von e:%d  nod:%d %d %d\n",rsort[n].i, e+1,ctri3[e].nod[0],ctri3[e].nod[1],ctri3[e].nod[2] );
    	    
        offset=find_tri3( node, set[set1].node[i], ctri3, e);
        if(printFlag) 
	  printf("offset:%f\n", offset);
        if( offset != MAX_INTEGER )
        {
          
          if(printFlag) 
            printf(" found enclosing tri3:%d for node:%d, offset:%lf\n", e+1, set[set1].node[i], offset);
          
          /* merke dir das element und den offset */
          if (min_offset > offset*offset ) { min_offset=offset*offset;  elbuf=e; offsetbuf=offset;}
        }
      }
    }
    if(printFlag) 
      printf("min_offset!=(double)MAX_INTEGER:%d tol*tol>=offsetbuf*offsetbuf:%d\n", min_offset!=(double)MAX_INTEGER,tol*tol>=offsetbuf*offsetbuf);
    if((min_offset!=(double)MAX_INTEGER)&&(tol*tol>=offsetbuf*offsetbuf))
    {
      if(printFlag) 
        printf("closest tri3:%d (%d %d %d)for node:%d, offset:%lf\n", elbuf+1, ctri3[elbuf].nod[0],ctri3[elbuf].nod[1],ctri3[elbuf].nod[2], set[set1].node[i], offsetbuf);
      if((dep =(int *)realloc((int *)dep, (sum_dep+2) *sizeof(int)))==NULL)
      { errMsg("\n\n ERROR: realloc failed\n" ); return(-1); }
      dep[sum_dep]=set[set1].node[i];
      sum_dep++;
      if((ind =(int *)realloc((int *)ind, (sum_ind+4) *sizeof(int)))==NULL)
      { errMsg("\n\n ERROR: realloc failed\n" ); return(-1); }
      ind[sum_ind]=ctri3[elbuf].nod[0];
      sum_ind++;
      ind[sum_ind]=ctri3[elbuf].nod[1];
      sum_ind++;
      ind[sum_ind]=ctri3[elbuf].nod[2];
      sum_ind++;
    }
    next_node:;
  }
  
  depset=-1;
  if (sum_dep)
  {
    /* check if all nodes of a face connected to this node are dep-nodes */
    for(f=0; f<set[set1].anz_f; f++)
    {
      if (face[set[set1].face[f]].type == 7)       j = 3;  /* TRI3  */
      else if (face[set[set1].face[f]].type == 8)  j = 6;  /* TRI6  */
      else if (face[set[set1].face[f]].type == 9)  j = 4;  /* QUAD4 */
      else if (face[set[set1].face[f]].type == 10) j = 8;  /* QUAD8 */
      /* add the nodes only if the complete face is defined */
      e=0;
      for (n=0; n<sum_dep; n++)
      {
        for(i=0; i<j; i++)
	{
          if(face[set[set1].face[f]].nod[i]==dep[n]) e++;
        }
        if(e==j)
        {
          if(depset==-1)
	  {
            sprintf( setname, "%s_%s", &set[set1].name[1], &set[set2].name[1]); 
            delSet(setname);
            depset=pre_seta(setname, "i", 0);
	  }
          for(i=0; i<j; i++) seta(depset, "n", face[set[set1].face[f]].nod[i]);
	}
      }
    }

    for(f=0; f<set[set1].anz_f; f++)
    {
      if (face[set[set1].face[f]].type == 7)       j = 3;  /* TRI3  */
      else if (face[set[set1].face[f]].type == 8)  j = 6;  /* TRI6  */
      else if (face[set[set1].face[f]].type == 9)  j = 4;  /* QUAD4 */
      else if (face[set[set1].face[f]].type == 10) j = 8;  /* QUAD8 */

      /* add all touched faces */
      if(depset>-1) for (n=0; n<sum_dep; n++)
      {
        for(i=0; i<j; i++)
	{
          if(face[set[set1].face[f]].nod[i]==dep[n]) { seta(depset, "f", set[set1].face[f]); break; }
        }
      }
    }
  }
  
  if(ntri_nodes) free(ntri_nodes);
  if(ctri3) free(ctri3);
  if(rsort) free(rsort);
  if(dep) free(dep);
  if(ind) free(ind);
  if(depset>-1) return(depset);
  else return(-1);
}




/* search face-pairs of unconnected sets of faces */
/* deside which side is the dep side */
/* remove all dep-nodes which are not inside the ind-faces */

int getFacePair( Summen *anz, int set1, int set2, Elements *e_enqire, Faces *face, Nodes *node, double tol, int *mpcset)
{
  int  i,j=0,f;

  printf ("getFacePair of %s %s\n", set[set1].name, set[set2].name);

  mpcset[0]=getDepNodes( anz, set1, set2, e_enqire, face, node, tol);
  if(mpcset[0]>-1) mpcset[1]=getDepNodes( anz, set2, set1, e_enqire, face, node, tol);
  else return(0);

  if(mpcset[1]<0)
  {
    printf(" ERROR: could not detect dependent nodes from set:%s, connection not done\n", set[set2].name);
    return(0);
  }

  if(set[mpcset[0]].anz_n < set[mpcset[1]].anz_n)
  {
    /* switch dep and ind */
    i=mpcset[0];
    mpcset[0]=mpcset[1];
    mpcset[1]=i;
  }

  /* add all face-nodes of the ind-set */
  for(f=0; f<set[mpcset[1]].anz_f; f++)
  {
    if (face[set[mpcset[1]].face[f]].type == 7)       j = 3;  /* TRI3  */
    else if (face[set[mpcset[1]].face[f]].type == 8)  j = 6;  /* TRI6  */
    else if (face[set[mpcset[1]].face[f]].type == 9)  j = 4;  /* QUAD4 */
    else if (face[set[mpcset[1]].face[f]].type == 10) j = 8;  /* QUAD8 */
    for(i=0; i<j; i++) seta(mpcset[1], "n", face[set[mpcset[1]].face[f]].nod[i]);
  }
  printf(" fond %d dependent nodes and %d independent nodes\n", set[mpcset[0]].anz_n,set[mpcset[1]].anz_n);

  return(1);

}
