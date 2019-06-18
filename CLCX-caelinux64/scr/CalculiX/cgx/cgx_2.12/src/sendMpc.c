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

/* BUGS
 search for "bug:"
 */

#include <cgx.h>
#define TEST     0
#define MIN_NODE_DIST 1.e-12    /* minimum gap between ind and dep for normal-vector calcs */ 
#define MAXLOOPS 10             /* loops in areampc to adjust the node-pos */

/* WARNING: Never save the model after the following switches were activated */
#define STORE_CONNECTIVITY 0    /* connectivity.txt stores the PIDs of the parts connected with RBEs (mata has to be used after mesh! */

extern int       neqn;                 /* offset der equations fuer ansys, bzw. MPC ID or elemnr for RBEs fuer Nast. */

extern double gtol;

extern char  delPntFlag;                    /* 1: deleted points exists */
extern char  delLineFlag;                   /* 1: deleted lines exists */
extern char  delLcmbFlag;                   /* 1: deleted lcmbs exists */
extern char  delSurfFlag;                   /* 1: deleted surfs exists */
extern char  delBodyFlag;                   /* 1: deleted bodys exists */

extern Scale     scale[1];
extern Summen    anz[1];
extern Edges     *edge;
extern Nodes     *node;
extern Elements  *e_enqire;
extern Datasets *lcase;
extern Faces     *face;

extern Alias     *alias;
extern Sets      *set;
extern Psets     *pset;
extern Points    *point;
extern Lines     *line;
extern Lcmb      *lcmb;
extern Gsur      *surf;
extern Gbod      *body;
extern Nurbs     *nurbs;
extern SumGeo    anzGeo[1];
extern SumAsci   sumAsci[1];

extern SpecialSet specialset[1];

extern int       setall;                /* setNr of the default set "all" */
extern char  printFlag;                     /* printf 1:on 0:off */
extern CopiedNodeSets copiedNodeSets[1];

extern int       nasMpc;                                       /* 1: areampc generates mpcs; 0: rbes with optional heat-expansion-coefficient */
extern double    nasRbeHec; 

#define     DOFX        1
#define     DOFY        2
#define     DOFZ        3
#define     DOFP        8
#define     DOFT        11
#define     DPHI_TOL    0.017453      /* 1grd */
#define     MIN_VECTOR  0.0001

#define     N_CLOSEST_NODES 10

double   MIN_C={0.5};   /* zulaessiger flaechenfehler in calcCoefficientsTri() */
double   MAX_C={2.};   /* wird durch verschieben der nodes korrigiert      */

void sendMpc( char *setname, char *format, char *rotation , double *vector)
{
  int   i,j;
  int   length, setNr, indeql;
  char prognam[MAX_LINE_LENGTH], boundary[MAX_LINE_LENGTH];

  FILE *handle, *handle_boundary;
  double sum[3]={0.,0.,0.};

#if STORE_CONNECTIVITY
  FILE *handle_pid;
#endif

  if(neqn<anz->emax) neqn=anz->emax;

  strcpy ( prognam, setname);
  length= strlen ( setname );
  setNr=getSetNr(setname);
  if (setNr<0)
  {
    printf (" ERROR: set:%s does not exist\n", setname);
    return;
  }

  strcpy ( prognam, setname);
  length= strlen ( setname );
  strcpy (&prognam[length], ".mpc");

  handle = fopen (prognam, "w");
  if ( handle== NULL )
  {
    printf ("\nThe input file %s could not be opened.\n\n", prognam); 
    return;
  }

  /* write the mpcs in nastran-format as RBE2 */
  if (compare( format, "nas", 3)== 3)
  {
    if((rotation[0]=='v')||(rotation[0]=='n'))
    {
      /* free the additional midside-nodes for higher order elements */
      for(i=anz->orign; i<anz->n; i++) node[node[i].nr].pflag=-1;
      anz->n= anz->orign;
      anz->nmax=anz->orignmax;
  
      /* define a node in the center of the dependend nodes if no coordinates are specified */
      indeql=anz->nmax+1;
      if(rotation[0]=='v')
      {
        if(strlen(rotation)>1) indeql=atoi(&rotation[1]);
        nod( anz, &node, 1, indeql, vector[0], vector[1], vector[2], 1 );
      }
      else 
      {
        if(rotation[0]=='n') if(strlen(rotation)>1) indeql=atoi(&rotation[1]);
        for (i=0; i<set[setNr].anz_n; i++ )
        {
          sum[0]+=node[set[setNr].node[i]].nx;
          sum[1]+=node[set[setNr].node[i]].ny;
          sum[2]+=node[set[setNr].node[i]].nz;
        }
        for (i=0; i<3; i++) sum[i]/=set[setNr].anz_n;
        nod( anz, &node, 1, indeql, sum[0], sum[1], sum[2], 0 );
      }
      seta(setall, "n", indeql);
      
      /* define the rbe */
      //fprintf(handle, "RBE2,%8d,%8d,  123456", ++neqn, indeql);
      fprintf(handle, "RBE2    %8d%8d  123456", ++neqn, indeql);
      j=0; for (i=0; i<set[setNr].anz_n; i++ )
      {
        //fprintf(handle, ",%8d", set[setNr].node[i] );
        fprintf(handle, "%8d", set[setNr].node[i] );
        if(i!=set[setNr].anz_n-1)
        {  if( i==4 ) { fprintf(handle, "\n        "); j++; }
           if( (i-4)==8*j ) { fprintf(handle, "\n        "); j++; }
        }
      }
      if(nasRbeHec>0.) fprintf(handle, "%5.2fE-5", nasRbeHec*1e5 );
      fprintf(handle, "\n");
      fclose(handle);

#if STORE_CONNECTIVITY
    /* generate a list of connection pid and first neqn (only use with care, dep and ind sets are changed) */
    handle_pid = fopen ("connectivity.txt", "a");
    if (handle==NULL) { printf ("\nThe output file \"connectivity.txt\" could not be opened.\n\n"); return; }
    else  printf (" connectivity.txt opened\n" );
   
    completeSet(set[setNr].name,"up");
    if(set[setNr].anz_e)
    {
      printf("Set %s PID %d connected by elem %d to_node %d\n", set[setNr].name, e_enqire[set[setNr].elem[0]].mat, neqn, indeql);
      fprintf(handle_pid,"Set %s PID %d connected by elem %d to_node %d ", set[setNr].name, e_enqire[set[setNr].elem[0]].mat, neqn, indeql);
      if(nasRbeHec>0.) fprintf(handle_pid, "HC %5.2fE-5 ", nasRbeHec*1e5 );
      fprintf (handle_pid,"DOFs: 123456\n");
    }
    else
    {
      printf("Set %s w/o_elems connected by elem %d to_node %d\n", set[setNr].name, neqn, indeql);
      fprintf(handle_pid,"Set %s PID %d connected by elem %d to_node %d ", set[setNr].name, neqn, indeql);
      if(nasRbeHec>0.) fprintf(handle_pid, "HC %5.2fE-5 ", nasRbeHec*1e5 );
      fprintf (handle_pid,"DOFs: 123456\n");
    }

    fclose(handle_pid);
    printf("\nWARNING: sets %s were upwards completed, do not save.\n\n", set[setNr].name);
#endif
  
      /* new number of original nodes */
      anz->orignmax = anz->nmax;
      anz->orign = anz->n;
  
      /* new midnodes */
      adjustDrawNodes(1);
  
      makeSurfaces();
      getElemNormalen( e_enqire, node, anz->e );
      realloc_colNr();
      updateDispLists(); 
    }
    else
    {
      errMsg(" ERROR: format %s does not yet support %s \n", format, rotation);
    }
  }

  /* write the mpcs in abaqus-format */
  else if (compare( format, "abq", 3)== 3)
  {
    if((rotation[0]=='v')||(rotation[0]=='n'))
    {
      /* free the additional midside-nodes for higher order elements */
      for(i=anz->orign; i<anz->n; i++) node[node[i].nr].pflag=-1;
      anz->n= anz->orign;
      anz->nmax=anz->orignmax;
  
      /* define a node in the center of the dependend nodes if no coordinates are specified */
      indeql=anz->nmax+1;
      if(rotation[0]=='v')
      {
        if(strlen(rotation)>1) indeql=atoi(&rotation[1]);
        nod( anz, &node, 1, indeql, vector[0], vector[1], vector[2], 1 );
      }
      else 
      {
        if(rotation[0]=='n') if(strlen(rotation)>1) indeql=atoi(&rotation[1]);
        for (i=0; i<set[setNr].anz_n; i++ )
        {
          sum[0]+=node[set[setNr].node[i]].nx;
          sum[1]+=node[set[setNr].node[i]].ny;
          sum[2]+=node[set[setNr].node[i]].nz;
        }
        for (i=0; i<3; i++) sum[i]/=set[setNr].anz_n;
        nod( anz, &node, 1, indeql, sum[0], sum[1], sum[2], 0 );
      }
      seta(setall, "n", indeql);
      
      /* define the rbe */
      neqn++;
      fprintf(handle, "*NSET, NSET=NRB%d\n", neqn );
      j=0; for (i=0; i<set[setNr].anz_n; i++ )
      {
        fprintf(handle, "%d,\n", set[setNr].node[i] );
      }
      fprintf(handle, "*RIGID BODY,NSET=NRB%d,REF NODE=%d\n", neqn, indeql );
      fclose(handle);
  
      /* new number of original nodes */
      anz->orignmax = anz->nmax;
      anz->orign = anz->n;
  
      /* new midnodes */
      adjustDrawNodes(1);
  
      makeSurfaces();
      getElemNormalen( e_enqire, node, anz->e );
      realloc_colNr();
      updateDispLists(); 
    }
    else 
    {
      strcpy ( boundary, setname);
      length= strlen ( setname );
      strcpy (&boundary[length], ".bou");
      handle_boundary = fopen (boundary, "w");
      if ( handle_boundary== NULL )
      {
        printf ("\nThe input file %s could not be opened.\n\n", boundary); 
        return;
      }
  
      fprintf(handle, "** WARNING: THE USE OF THE ORIGINAL COORDINATE SYSTEM IS MANDATORY\n");
      fprintf(handle, "** INCLUDE THE FOLLOWING LINES IN THE MODEL-DEFINITION-SECTION:\n");
      fprintf(handle, "**   THE LOCATION OF THE INDEPENDENT NODE FOR PRETWIST\n");
      fprintf(handle, "**   IS USED TO DEFINE THE DIRECTION OF THE ROTATIONAL VECTOR\n");
      fprintf(handle, "**     AROUND X: 1.,0.,0.\n");
      fprintf(handle, "**     AROUND Y: 0.,1.,0.\n");
      fprintf(handle, "**     AROUND Z: 0.,0.,1. (DEFAULT)\n");
      fprintf(handle, "*NODE, NSET=Ntwist\n");
      fprintf(handle, "%d,%lf,%lf,%lf\n",anz->orignmax+1,vector[0],vector[1],vector[2] );
      fprintf(handle, "*MPC, USER\n1,");
      j=0; for (i=0; i<set[setNr].anz_n; i++ )
      {
        if( j>12 )
        {
          j=0;
          fprintf(handle, "\n0,");
        }
        fprintf(handle, "%d,",set[setNr].node[i]);
        j++;
        fprintf(handle, "%d,",set[setNr].node[i]);
        j++;
        fprintf(handle, "%d,",set[setNr].node[i]);
        j++;
      }
      if(j>14) fprintf(handle, "\n0,%d\n", anz->orignmax+1);
      else fprintf(handle, "%d\n", anz->orignmax+1);
      fclose(handle);
  
      fprintf(handle_boundary, "** INCLUDE THE FOLLOWING LINES IN A STEP:\n");
      fprintf(handle_boundary, "**  Pretwist of %s degree\n*BOUNDARY\n", rotation);
      fprintf(handle_boundary, "%d, 1, 1, %lf\n", anz->orignmax+1, atof(rotation)*PI/180.);
      fclose(handle_boundary);
    }
  }
  else
  {
    errMsg(" ERROR: format %s not yet supported\n", format );
  }
  printf (" ready\n");
}


void cycmpc(int set1, int set2, char *format, char *value, char *corr)
{
  int i,j,s,n;
  int firstrun;
  int ni,nj;
  char  datout[MAX_LINE_LENGTH], buffer[MAX_LINE_LENGTH];
  double dphi, phi_teilung, phi_vorgabe=0.;
  char  csys, axis;
  double *nx1=0, *nx2=0;
  double *nt1=0, *nt2=0;
  double *nr1=0, *nr2=0;
  double *phi=0;
  FILE  *handle, *handle2=NULL;

  double rmin,dx,dr,R,f;
  int   node2j=0, *nodbuf=NULL;
  int   lastNode=-1;
  double nv[3];


  if ( (compare( format, "abq", 3)!= 3)&&
       (compare( format, "ans", 3)!= 3)&&
       (compare( format, "ids", 3)!= 3)&&
       (compare( format, "nas", 3)!= 3) ) 
  {
    errMsg ("ERROR: format:%s not known\n\n", format);
    return;
  }

  /* ---- clear special sets ----- */
  delSet(specialset->mpc );
  delSet(specialset->nompc );


  /* Open the files and check to see that it was opened correctly */
  sprintf(datout, "%s.equ", set[set1].name);
  if(printFlag) printf ("datout:%s set1:%s set2:%s\n",datout, set[set1].name, set[set2].name);
  handle = fopen (datout, "w");
  if (handle==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n", datout); return; }
  else  printf (" %s opened\n", datout);
  if (compare( format, "abq", 3)== 3)      fprintf(handle, "** cycmpc based on set %s %s\n",set[set1].name, set[set2].name );
  else if (compare( format, "ans", 3)== 3) fprintf(handle, "! cycmpc based on set %s %s\n",set[set1].name, set[set2].name );
  else if (compare( format, "nas", 3)== 3) fprintf(handle, "$ cycmpc based on set %s %s\n",set[set1].name, set[set2].name );
  else if (compare( format, "ids", 3)== 3)
  {
    sprintf(datout, "%s2", datout);
    handle2 = fopen (datout, "w");
    if (handle2==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n", datout); return; }
    else  printf (" %s opened\n", datout);
  }
  csys=value[0];
  axis=value[1];
  i=atoi(&corr[1]);    /* startnummer der equations in abaqus, oder id in nastran */
  if(i>0) neqn=i-1;
  f=atof(&value[2]);
  if(f!=0.) phi_vorgabe=2.*PI /f;  /* teilungswinkel, berechnet aus der segmentzahl */


  /* ------- node-koordinaten berechnen und am ende wieder scalieren ------ */
  descalNodes ( anz->n, node, scale );

  /* for cfd equations the faces have to be translated to nodes */
  if(compare( format, "abqf", 4)== 4)
  {
    if ( (nodbuf = (int *)malloc( set[set1].anz_n * sizeof(int))) == NULL )
    {
      errMsg("\nERROR: malloc failed in cycmpc() \n\n");
      return;
    }
    for(i=0; i<set[set1].anz_n; i++) nodbuf[i]=set[set1].node[i];
    n=set[set1].anz_n;
    for(i=0; i<n; i++) setr(set1,"n",nodbuf[i]);
    free(nodbuf);
    if ( (nodbuf = (int *)malloc( set[set2].anz_n * sizeof(int))) == NULL )
    {
      errMsg("\nERROR: malloc failed in cycmpc() \n\n");
      return;
    }
    for(i=0; i<set[set2].anz_n; i++) nodbuf[i]=set[set2].node[i];
    n=set[set2].anz_n;
    for(i=0; i<n; i++) setr(set2,"n",nodbuf[i]);
    free(nodbuf);

    // remember the last regular node
    lastNode=anz->nmax;

    // create a node in the center of the faces
    nodbuf=NULL;
    for(i=0; i<set[set1].anz_f; i++)
    {
      s=set[set1].face[i];
      if (face[s].type == 7) n = 3;  /* TRI3  */
      else if (face[s].type == 8) n = 6;  /* TRI6  */
      else if (face[s].type == 9) n = 4;  /* QUAD4 */
      else if (face[s].type == 10) n = 8; /* QUAD8 */
      else if (face[s].type == 11) n = 2; /* beam2 */
      else if (face[s].type == 12) n = 3; /* beam3 */
      for(j=0; j<3; j++) nv[j]=0;
      for(j=0; j<n; j++)
      {
        nv[0]+=node[face[s].nod[j]].nx;
        nv[1]+=node[face[s].nod[j]].ny;
        nv[2]+=node[face[s].nod[j]].nz;
      }
      for(j=0; j<3; j++) nv[j]/=n;
      
      nod( anz, &node, 0, anz->nmax+1, nv[0], nv[1], nv[2], 0 );
      seta(set1,"n",anz->nmax);
      if ( (nodbuf = (int *)realloc((int *)nodbuf, (anz->nmax+1) * sizeof(int))) == NULL )
      {
        errMsg("\nERROR: malloc failed in cycmpc() \n\n");
        return;
      }
      nodbuf[anz->nmax]=s;
    }
    for(i=0; i<set[set2].anz_f; i++)
    {
      s=set[set2].face[i];
      if (face[s].type == 7) n = 3;  /* TRI3  */
      else if (face[s].type == 8) n = 6;  /* TRI6  */
      else if (face[s].type == 9) n = 4;  /* QUAD4 */
      else if (face[s].type == 10) n = 8; /* QUAD8 */
      else if (face[s].type == 11) n = 2; /* beam2 */
      else if (face[s].type == 12) n = 3; /* beam3 */
      for(j=0; j<3; j++) nv[j]=0;
      for(j=0; j<n; j++)
      {
        nv[0]+=node[face[s].nod[j]].nx;
        nv[1]+=node[face[s].nod[j]].ny;
        nv[2]+=node[face[s].nod[j]].nz;
      }
      for(j=0; j<3; j++) nv[j]/=n;
      
      nod( anz, &node, 0, anz->nmax+1, nv[0], nv[1], nv[2], 0 );
      seta(set2,"n",anz->nmax);
      if ( (nodbuf = (int *)realloc((int *)nodbuf, (anz->nmax+1) * sizeof(int))) == NULL )
      {
        errMsg("\nERROR: malloc failed in cycmpc() \n\n");
        return;
      }
      nodbuf[anz->nmax]=s;
    }
  }

  /* --------- Seitenflaeche einlesen und Daten aufbereiten --------------- */

  if ( (nx1 = (double *)malloc( set[set1].anz_n * sizeof(double))) == NULL )
  {
    errMsg("\nERROR: malloc failed in cycmpc() \n\n");
    goto end;
  }
  if ( (nx2 = (double *)malloc( set[set2].anz_n * sizeof(double))) == NULL )
  {
    errMsg("\nERROR: malloc failed in cycmpc() \n\n");
    goto end;
  }
  if ( (nt1 = (double *)malloc( set[set1].anz_n * sizeof(double))) == NULL )
  {
    errMsg("\nERROR: malloc failed in cycmpc() \n\n");
    goto end;
  }
  if ( (nt2 = (double *)malloc( set[set2].anz_n * sizeof(double))) == NULL )
  {
    errMsg("\nERROR: malloc failed in cycmpc() \n\n");
    goto end;
  }
  if ( (nr1 = (double *)malloc( set[set1].anz_n * sizeof(double))) == NULL )
  {
    errMsg("\nERROR: malloc failed in cycmpc() \n\n");
    goto end;
  }
  if ( (nr2 = (double *)malloc( set[set2].anz_n * sizeof(double))) == NULL )
  {
    errMsg("\nERROR: malloc failed in cycmpc() \n\n");
    goto end;
  }
  if ( (phi = (double *)malloc( set[set1].anz_n * sizeof(double))) == NULL )
  {
    errMsg("\nERROR: malloc failed in cycmpc() \n\n");
    goto end;
  }


  /* --------- R,Theta,x Koordinaten berechnen  */

  if (set[set1].anz_n > anz->n) {printf(" ERROR: side has more nodes than the model"); exit(-1);}


 if(axis=='x')
 {
  if(printFlag) printf(" x-axis specified\n");
  for (i=0; i<set[set1].anz_n; i++ )
  {
    ni=set[set1].node[i];
    nx1[i]= node[ni].nx;
    nr1[i]= sqrt( node[ni].nz*node[ni].nz + node[ni].ny*node[ni].ny );
    if ( nr1[i] )
    {
      nt1[i]= p_angle( node[ni].nz, node[ni].ny ); 
    }
  }
  for (i=0; i<set[set2].anz_n; i++ )
  {
    ni=set[set2].node[i];
    nx2[i]= node[ni].nx;
    nr2[i]= sqrt( node[ni].nz*node[ni].nz + node[ni].ny*node[ni].ny );
    if ( nr2[i] )
    {
      nt2[i]= p_angle( node[ni].nz, node[ni].ny );
    }
  }



  /* suche nodepaare, berechne differenzwinkel und schreibe EQUATION's  */
  for (i=0; i<set[set1].anz_n; i++ )
  {
   ni=set[set1].node[i];
   if (nr1[i]>0)
   {
    /* suche gegenueber liegenden naechsten node  */
    rmin=MAX_INTEGER;
    for (j=0; j<set[set2].anz_n; j++ )
    {
      nj=set[set2].node[j];
      if ((nr2[j]>0)&&( nj!= ni))
      {
        dx= nx2[j]-nx1[i];
        dr= nr2[j]-nr1[i];
        R=sqrt( dx*dx + dr*dr );
        if (R<rmin)
        {
          rmin=R;
          node2j=j;
        }
      }
    }
    nj=set[set2].node[node2j];

    /* berechne den differenzwinkel  */
    phi[i]= nt2[node2j]-nt1[i];

    /* definiere den Zielwinkel, entweder vorgabe oder wie berechnet */
    if(phi_vorgabe==0.) phi_teilung=phi[i];
    else phi_teilung=phi_vorgabe;

    firstrun=1;
   nexttryx:;
    /* kontrolliere den differenzwinkel mit der teilung */
    dphi = phi_teilung - phi[i];
    if(dphi>2.*PI-DPHI_TOL) while (dphi>2.*PI-DPHI_TOL) dphi-=2.*PI;
    if(dphi<-2.*PI+DPHI_TOL) while (dphi<-2.*PI+DPHI_TOL) dphi+=2.*PI;
    if ((dphi*dphi > DPHI_TOL)&&(firstrun))
    {
      firstrun=0;
      phi_teilung=-phi_vorgabe;
      goto nexttryx;
    }
    else if ((dphi*dphi > DPHI_TOL)&&(!firstrun))
    {
      errMsg ("ERROR: korresponding node too far away:%lf grd, see set %s\n",dphi*180./PI, specialset->nompc);
      sprintf( buffer, "%d", ni); 
      pre_seta(specialset->nompc, "n", buffer);
    }
    else 
    {
    if(printFlag) printf(" theta(%d):%lf theta(%d):%lf  dtheta:%lf dtheta_corr:%lf rmin:%lf\n",
      ni, nt1[i]*180./PI, set[set2].node[node2j], nt2[node2j]*180./PI,
      phi[i]*180./PI, phi_teilung*180./PI, rmin);

    if (corr[0]=='c')
    { 
      /* versetze den gefundenen Knoten auf die korrespondierende pos. */
      /* der dependent node wird auf die pos des indep gesetzt */
      node[ni].nx = node[set[set2].node[node2j]].nx;
      if ( nr1[i] )
      {
        node[ni].ny = node[set[set2].node[node2j]].ny*cos(phi_teilung)-node[set[set2].node[node2j]].nz*sin(phi_teilung);
        node[ni].nz = node[set[set2].node[node2j]].ny*sin(phi_teilung)+node[set[set2].node[node2j]].nz*cos(phi_teilung);
      }  
    }


   if ((csys=='t')||(csys=='p'))
   {
    if (compare( format, "abqf", 4)== 4)
    {
      /* schreibe die EQUATIOS im Abaqusformat */
      fprintf(handle, "*EQUATIONF\n");
        fprintf(handle, "%d\n", 2);
        if(csys=='t'){ 
          fprintf(handle, "%d,S%d,%d,%.12lf, %d,S%d,%d,%.12lf \n", face[nodbuf[ni]].elem_nr, face[nodbuf[ni]].nr+1, DOFT, 1.,
		  face[nodbuf[nj]].elem_nr, face[nodbuf[nj]].nr+1, DOFT, -1. );}
        else{ 
          fprintf(handle, "%d,S%d,%d,%.12lf, %d,S%d,%d,%.12lf \n", face[nodbuf[ni]].elem_nr, face[nodbuf[ni]].nr+1, DOFP, 1.,
		  face[nodbuf[nj]].elem_nr, face[nodbuf[nj]].nr+1, DOFP, -1. );}
    }
    else if (compare( format, "abq", 3)== 3)
    {
      /* schreibe die EQUATIOS im Abaqusformat */
      fprintf(handle, "*EQUATION\n");
        fprintf(handle, "%d\n", 2);
        if(csys=='t'){ 
          fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf \n", ni, DOFT, 1.,
		set[set2].node[node2j], DOFT, -1. ); }
        else{ 
          fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf \n", ni, DOFP, 1.,
		set[set2].node[node2j], DOFP, -1. ); }
    }
    else if (compare( format, "ids", 3)== 3)
    {
      /* schreibe die EQUATIOS als knotenliste */
      fprintf(handle, "%d\n", ni);
      fprintf(handle2, "%d\n", set[set2].node[node2j]);
    }
   }
   if (csys=='r')
   {
    if(compare( format, "abqf", 4)== 4)
    {
      /* schreibe die EQUATIOS im Abaqusformat */
      fprintf(handle, "*EQUATIONF\n");
        fprintf(handle, "%d\n", 2);
        fprintf(handle, "%d,S%d,%d,%.12lf, %d,S%d,%d,%.12lf \n", face[nodbuf[ni]].elem_nr, face[nodbuf[ni]].nr+1, DOFX, 1.,
          face[nodbuf[nj]].elem_nr, face[nodbuf[nj]].nr+1, DOFX, -1. );
      fprintf(handle, "*EQUATIONF\n");
        fprintf(handle, "%d\n", 3);
        fprintf(handle, "%d,S%d,%d,%.12lf, %d,S%d,%d,%.12lf, %d,S%d,%d,%.12lf\n", face[nodbuf[ni]].elem_nr, face[nodbuf[ni]].nr+1, DOFY, 1.,
          face[nodbuf[nj]].elem_nr, face[nodbuf[nj]].nr+1, DOFY, -cos(phi_teilung),  face[nodbuf[nj]].elem_nr, face[nodbuf[nj]].nr+1, DOFZ, sin(phi_teilung) );
      fprintf(handle, "*EQUATIONF\n");
        fprintf(handle, "%d\n", 3);
        fprintf(handle, "%d,S%d,%d,%.12lf, %d,S%d,%d,%.12lf, %d,S%d,%d,%.12lf\n", face[nodbuf[ni]].elem_nr, face[nodbuf[ni]].nr+1, DOFZ, 1.,
          face[nodbuf[nj]].elem_nr, face[nodbuf[nj]].nr+1, DOFY, -sin(phi_teilung),  face[nodbuf[nj]].elem_nr, face[nodbuf[nj]].nr+1, DOFZ, -cos(phi_teilung) );
    }
    else if (compare( format, "abq", 3)== 3)
    {
      /* schreibe die EQUATIOS im Abaqusformat */
      fprintf(handle, "*EQUATION\n");
        fprintf(handle, "%d\n", 2);
        fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf \n", ni, DOFX, 1.,
          set[set2].node[node2j], DOFX, -1. );
      fprintf(handle, "*EQUATION\n");
        fprintf(handle, "%d\n", 3);
        fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf, %d,%d,%.12lf\n", ni, DOFY, 1.,
          set[set2].node[node2j], DOFY, -cos(phi_teilung),  set[set2].node[node2j], DOFZ, sin(phi_teilung) );
      fprintf(handle, "*EQUATION\n");
        fprintf(handle, "%d\n", 3);
        fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf, %d,%d,%.12lf\n", ni, DOFZ, 1.,
          set[set2].node[node2j], DOFY, -sin(phi_teilung),  set[set2].node[node2j], DOFZ, -cos(phi_teilung) );
    }
    else if (compare( format, "ans", 3)== 3)
    {
      /* schreibe die EQUATIOS im Ansysformat */
      neqn++;
      fprintf(handle, "CE,%d,0,%d,%s,%.12lf, %d,%s,%.12lf,  %d,%s,%.12lf\n",
        neqn, -1, "UX", 1.,
              ni, "UX", 1., set[set2].node[node2j], "UX", -1. );
      neqn++;
      fprintf(handle, "CE,%d,0,%d,%s,%.12lf, %d,%s,%.12lf,  %d,%s,%.12lf\n",
        neqn, ni, "UY", 1.,
              set[set2].node[node2j], "UY", -cos(phi_teilung), set[set2].node[node2j], "UZ", sin(phi_teilung) );
      neqn++;
      fprintf(handle, "CE,%d,0,%d,%s,%.12lf, %d,%s,%.12lf,  %d,%s,%.12lf\n",
        neqn, ni, "UZ", 1.,
              set[set2].node[node2j], "UY", -sin(phi_teilung), set[set2].node[node2j], "UZ", -cos(phi_teilung) );
    }
    else if (compare( format, "nas", 3)== 3)
    {
      /* schreibe die EQUATIOS im Nastranformat */
      fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n", neqn+1, ni, DOFX, 1.,
          set[set2].node[node2j], DOFX, -1. );
      fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n", neqn+1, ni, DOFY, 1.,
                                                          set[set2].node[node2j], DOFY, -cos(phi_teilung));
      fprintf(handle, ", ,%8d,%8d,%.12lf\n", set[set2].node[node2j], DOFZ, sin(phi_teilung) );
      fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n", neqn+1, ni, DOFZ, 1.,
                                                          set[set2].node[node2j], DOFY, -sin(phi_teilung));
      fprintf(handle, ", ,%8d,%8d,%.12lf\n", set[set2].node[node2j], DOFZ, -cos(phi_teilung) );
    }
   }
   if (csys=='c')
   {
    if (compare( format, "abq", 3)== 3)
    {
      /* schreibe die EQUATIOS im Abaqusformat */
      fprintf(handle, "*EQUATION\n");
        fprintf(handle, "%d\n", 2);
        fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf \n", ni, DOFX, 1.,
          set[set2].node[node2j], DOFX, -1. );
      fprintf(handle, "*EQUATION\n");
        fprintf(handle, "%d\n", 2);
        fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf \n", ni, DOFY, 1.,
          set[set2].node[node2j], DOFY, -1. );
      fprintf(handle, "*EQUATION\n");
        fprintf(handle, "%d\n", 2);
        fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf \n", ni, DOFZ, 1.,
          set[set2].node[node2j], DOFZ, -1. );
    }
    else if (compare( format, "ans", 3)== 3)
    {
      /* schreibe die EQUATIOS im Ansysformat */
      neqn++;
      fprintf(handle, "CE,%d,0,%d,%s,%.12lf, %d,%s,%.12lf,  %d,%s,%.12lf\n",
        neqn, -1, "UX", 1.,
              ni, "UX", 1., set[set2].node[node2j], "UX", -1. );
      neqn++;
      fprintf(handle, "CE,%d,0,%d,%s,%.12lf, %d,%s,%.12lf,  %d,%s,%.12lf\n",
        neqn, -1, "UY", 1.,
              ni, "UY", 1., set[set2].node[node2j], "UY", -1. );
      neqn++;
      fprintf(handle, "CE,%d,0,%d,%s,%.12lf, %d,%s,%.12lf,  %d,%s,%.12lf\n",
        neqn, -1, "UZ", 1.,
              ni, "UZ", 1., set[set2].node[node2j], "UZ", -1. );
    }
    else if (compare( format, "nas", 3)== 3)
    {
      /* schreibe die EQUATIOS im Nastranformat */
      fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n", neqn+1, ni, DOFX, 1.,
          set[set2].node[node2j], DOFX, -1. );
      fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n", neqn+1, ni, DOFY, 1.,
          set[set2].node[node2j], DOFY, -1. );
      fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n", neqn+1, ni, DOFZ, 1.,
          set[set2].node[node2j], DOFZ, -1. );
    }
   }
   sprintf( buffer, "%d", ni); 
   pre_seta(specialset->mpc, "n", buffer);
   }
   }
  }
 }
 else if(axis=='y')
 {
  if(printFlag) printf("y-axis specified\n");
  for (i=0; i<set[set1].anz_n; i++ )
  {
    ni=set[set1].node[i];
    nx1[i]= node[ni].ny;
    nr1[i]= sqrt( node[ni].nz*node[ni].nz + node[ni].nx*node[ni].nx );
    if ( nr1[i] )
    {
      nt1[i]= p_angle( node[ni].nx, node[ni].nz ); 
    }
  }
  for (i=0; i<set[set2].anz_n; i++ )
  {
    ni=set[set2].node[i];
    nx2[i]= node[ni].ny;
    nr2[i]= sqrt( node[ni].nz*node[ni].nz + node[ni].nx*node[ni].nx );
    if ( nr2[i] )
    {
      nt2[i]= p_angle( node[ni].nx, node[ni].nz ); 
    }
  }



  /* suche nodepaare, berechne differenzwinkel und schreibe EQUATION's  */
  for (i=0; i<set[set1].anz_n; i++ )
  {
   ni=set[set1].node[i];
   if (nr1[i]>0)
   {
    /* suche gegenueber liegenden naechsten node  */
    rmin=MAX_INTEGER;
    for (j=0; j<set[set2].anz_n; j++ )
    {
      nj=set[set2].node[j];
      if ((nr2[j]>0)&&( nj!= ni))
      {
        dx= nx2[j]-nx1[i];
        dr= nr2[j]-nr1[i];
        R=sqrt( dx*dx + dr*dr );
        if (R<rmin)
        {
          rmin=R;
          node2j=j;
        }
      }
    }
    nj=set[set2].node[node2j];

    /* berechne den differenzwinkel  */
    phi[i]= nt2[node2j]-nt1[i];

    /* definiere den Zielwinkel, entweder vorgabe oder wie berechnet */
    if(phi_vorgabe==0.) phi_teilung=phi[i];
    else phi_teilung=phi_vorgabe;

    firstrun=1;
   nexttryy:;
    /* kontrolliere den differenzwinkel mit der teilung */
    dphi = phi_teilung - phi[i];
    if(dphi>2.*PI-DPHI_TOL) while (dphi>2.*PI-DPHI_TOL) dphi-=2.*PI;
    if(dphi<-2.*PI+DPHI_TOL) while (dphi<-2.*PI+DPHI_TOL) dphi+=2.*PI;
    if ((dphi*dphi > DPHI_TOL)&&(firstrun))
    {
      firstrun=0;
      phi_teilung=-phi_vorgabe;
      goto nexttryy;
    }
    else if ((dphi*dphi > DPHI_TOL)&&(!firstrun))
    {
      errMsg ("ERROR: korresponding node too far away:%lf grd, see set %s\n",dphi*180./PI, specialset->nompc);
      sprintf( buffer, "%d", ni); 
      pre_seta(specialset->nompc, "n", buffer);
    }
    else 
    {

    if(printFlag) printf(" theta(%d):%lf theta(%d):%lf  dtheta:%lf dtheta_corr:%lf rmin:%lf\n",
      ni, nt1[i]*180./PI, set[set2].node[node2j], nt2[node2j]*180./PI,
      phi[i]*180./PI, phi_teilung*180./PI, rmin);

    if (corr[0]=='c')
    { 
      /* versetze den gefundenen Knoten auf die korrespondierende pos. */
      /* der dependent node wird auf die pos des indep gesetzt */
      node[ni].ny = node[set[set2].node[node2j]].ny;
      if ( nr1[i] )
      {
        node[ni].nx = node[set[set2].node[node2j]].nz*sin(phi_teilung) + node[set[set2].node[node2j]].nx*cos(phi_teilung);
        node[ni].nz = node[set[set2].node[node2j]].nz*cos(phi_teilung) - node[set[set2].node[node2j]].nx*sin(phi_teilung);
      }  
    }  


   if ((csys=='t')||(csys=='p'))
   {
    if (compare( format, "abqf", 4)== 4)
    {
      /* schreibe die EQUATIOS im Abaqusformat */
      fprintf(handle, "*EQUATIONF\n");
        fprintf(handle, "%d\n", 2);
        if(csys=='t'){ 
          fprintf(handle, "%d,S%d,%d,%.12lf, %d,S%d,%d,%.12lf \n", face[nodbuf[ni]].elem_nr, face[nodbuf[ni]].nr+1, DOFT, 1.,
		  face[nodbuf[nj]].elem_nr, face[nodbuf[nj]].nr+1, DOFT, -1. );}
        else{ 
          fprintf(handle, "%d,S%d,%d,%.12lf, %d,S%d,%d,%.12lf \n", face[nodbuf[ni]].elem_nr, face[nodbuf[ni]].nr+1, DOFP, 1.,
		  face[nodbuf[nj]].elem_nr, face[nodbuf[nj]].nr+1, DOFP, -1. );}
    }
    else if (compare( format, "abq", 3)== 3)
    {
      /* schreibe die EQUATIOS im Abaqusformat */
      fprintf(handle, "*EQUATION\n");
        fprintf(handle, "%d\n", 2);
        if(csys=='t'){ 
          fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf \n", ni, DOFT, 1.,
          set[set2].node[node2j], DOFT, -1. ); }
        else{ 
          fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf \n", ni, DOFP, 1.,
          set[set2].node[node2j], DOFP, -1. ); }
    }
    else if (compare( format, "ids", 3)== 3)
    {
      /* schreibe die EQUATIOS als knotenliste */
      fprintf(handle, "%d\n", ni);
      fprintf(handle2, "%d\n", set[set2].node[node2j]);
    }
   }
   if (csys=='r')
    {
    if (compare( format, "abqf", 4)== 4)
      {
      /* schreibe die EQUATIOS im Abaqusformat */
      fprintf(handle, "*EQUATIONF\n");
        fprintf(handle, "%d\n", 2);
        fprintf(handle, "%d,S%d,%d,%.12lf, %d,S%d,%d,%.12lf \n", face[nodbuf[ni]].elem_nr, face[nodbuf[ni]].nr+1, DOFY, 1.,
          face[nodbuf[nj]].elem_nr, face[nodbuf[nj]].nr+1, DOFY, -1. );
      fprintf(handle, "*EQUATIONF\n");
        fprintf(handle, "%d\n", 3);
        fprintf(handle, "%d,S%d,%d,%.12lf, %d,S%d,%d,%.12lf, %d,S%d,%d,%.12lf\n", face[nodbuf[ni]].elem_nr, face[nodbuf[ni]].nr+1, DOFZ, 1.,
          face[nodbuf[nj]].elem_nr, face[nodbuf[nj]].nr+1, DOFZ, -cos(phi_teilung), face[nodbuf[nj]].elem_nr, face[nodbuf[nj]].nr+1, DOFX, sin(phi_teilung) );
      fprintf(handle, "*EQUATIONF\n");
        fprintf(handle, "%d\n", 3);
        fprintf(handle, "%d,S%d,%d,%.12lf, %d,S%d,%d,%.12lf, %d,S%d,%d,%.12lf\n", face[nodbuf[ni]].elem_nr, face[nodbuf[ni]].nr+1, DOFX, 1.,
          face[nodbuf[nj]].elem_nr, face[nodbuf[nj]].nr+1, DOFZ, -sin(phi_teilung), face[nodbuf[nj]].elem_nr, face[nodbuf[nj]].nr+1, DOFX, -cos(phi_teilung) );
      }
    else if (compare( format, "abq", 3)== 3)
      {
      /* schreibe die EQUATIOS im Abaqusformat */
      fprintf(handle, "*EQUATION\n");
        fprintf(handle, "%d\n", 2);
        fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf \n", ni, DOFY, 1.,
          set[set2].node[node2j], DOFY, -1. );
      fprintf(handle, "*EQUATION\n");
        fprintf(handle, "%d\n", 3);
        fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf, %d,%d,%.12lf\n", ni, DOFZ, 1.,
          set[set2].node[node2j], DOFZ, -cos(phi_teilung),  set[set2].node[node2j], DOFX, sin(phi_teilung) );
      fprintf(handle, "*EQUATION\n");
        fprintf(handle, "%d\n", 3);
        fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf, %d,%d,%.12lf\n", ni, DOFX, 1.,
          set[set2].node[node2j], DOFZ, -sin(phi_teilung),  set[set2].node[node2j], DOFX, -cos(phi_teilung) );
      }
    else if (compare( format, "ans", 3)== 3)
      {
      /* schreibe die EQUATIOS im Ansysformat */
      neqn++;
      fprintf(handle, "CE,%d,0,%d,%s,%.12lf, %d,%s,%.12lf,  %d,%s,%.12lf\n",
        neqn, -1, "UY", 1.,
              ni, "UY", 1., set[set2].node[node2j], "UY", -1. );
      neqn++;
      fprintf(handle, "CE,%d,0,%d,%s,%.12lf, %d,%s,%.12lf,  %d,%s,%.12lf\n",
        neqn, ni, "UZ", 1.,
              set[set2].node[node2j], "UZ", -cos(phi_teilung), set[set2].node[node2j], "UX", sin(phi_teilung) );
      neqn++;
      fprintf(handle, "CE,%d,0,%d,%s,%.12lf, %d,%s,%.12lf,  %d,%s,%.12lf\n",
        neqn, ni, "UX", 1.,
              set[set2].node[node2j], "UZ", -sin(phi_teilung), set[set2].node[node2j], "UX", -cos(phi_teilung) );
      }
    else if (compare( format, "nas", 3)== 3)
      {
      /* schreibe die EQUATIOS im Nastranformat */
      fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n", neqn+1, ni, DOFY, 1.,
          set[set2].node[node2j], DOFY, -1. );
      fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n", neqn+1, ni, DOFZ, 1.,
                                                          set[set2].node[node2j], DOFZ, -cos(phi_teilung));
      fprintf(handle, ", ,%8d,%8d,%.12lf\n", set[set2].node[node2j], DOFX, sin(phi_teilung) );
      fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n", neqn+1, ni, DOFX, 1.,
                                                          set[set2].node[node2j], DOFZ, -sin(phi_teilung));
      fprintf(handle, ", ,%8d,%8d,%.12lf\n", set[set2].node[node2j], DOFX, -cos(phi_teilung) );
      }
    }
   if (csys=='c')
    {
    if (compare( format, "abq", 3)== 3)
      {
      /* schreibe die EQUATIOS im Abaqusformat */
      fprintf(handle, "*EQUATION\n");
        fprintf(handle, "%d\n", 2);
        fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf \n", ni, DOFX, 1.,
          set[set2].node[node2j], DOFX, -1. );
      fprintf(handle, "*EQUATION\n");
        fprintf(handle, "%d\n", 2);
        fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf \n", ni, DOFY, 1.,
          set[set2].node[node2j], DOFY, -1. );
      fprintf(handle, "*EQUATION\n");
        fprintf(handle, "%d\n", 2);
        fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf \n", ni, DOFZ, 1.,
          set[set2].node[node2j], DOFZ, -1. );
      }
    else if (compare( format, "ans", 3)== 3)
      {
      /* schreibe die EQUATIOS im Ansysformat */
      neqn++;
      fprintf(handle, "CE,%d,0,%d,%s,%.12lf, %d,%s,%.12lf,  %d,%s,%.12lf\n",
        neqn, -1, "UX", 1.,
              ni, "UX", 1., set[set2].node[node2j], "UX", -1. );
      neqn++;
      fprintf(handle, "CE,%d,0,%d,%s,%.12lf, %d,%s,%.12lf,  %d,%s,%.12lf\n",
        neqn, -1, "UY", 1.,
              ni, "UY", 1., set[set2].node[node2j], "UY", -1. );
      neqn++;
      fprintf(handle, "CE,%d,0,%d,%s,%.12lf, %d,%s,%.12lf,  %d,%s,%.12lf\n",
        neqn, -1, "UZ", 1.,
              ni, "UZ", 1., set[set2].node[node2j], "UZ", -1. );
      }
    else if (compare( format, "nas", 3)== 3)
      {
      /* schreibe die EQUATIOS im Nastranformat */
      fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n", neqn+1, ni, DOFX, 1.,
          set[set2].node[node2j], DOFX, -1. );
      fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n", neqn+1, ni, DOFY, 1.,
          set[set2].node[node2j], DOFY, -1. );
      fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n", neqn+1, ni, DOFZ, 1.,
          set[set2].node[node2j], DOFZ, -1. );
      }
    }
   sprintf( buffer, "%d", ni); 
   pre_seta(specialset->mpc, "n", buffer);
   }
   }
  }
 }
 else if(axis=='z')
 {
  if(printFlag) printf("z-axis specified\n");
  for (i=0; i<set[set1].anz_n; i++ )
  {
    ni=set[set1].node[i];
    nx1[i]= node[ni].nz;
    nr1[i]= sqrt( node[ni].nx*node[ni].nx + node[ni].ny*node[ni].ny );
    if ( nr1[i] )
    {
      nt1[i]= p_angle( node[ni].ny, node[ni].nx );
    }
  }
  for (i=0; i<set[set2].anz_n; i++ )
  {
    ni=set[set2].node[i];
    nx2[i]= node[ni].nz;
    nr2[i]= sqrt( node[ni].nx*node[ni].nx + node[ni].ny*node[ni].ny );
    if ( nr2[i] )
    {
      nt2[i]= p_angle( node[ni].ny, node[ni].nx );
    }
  }



  /* suche nodepaare, berechne differenzwinkel und schreibe EQUATION's  */
  for (i=0; i<set[set1].anz_n; i++ )
  {
   ni=set[set1].node[i];
   if (nr1[i]>0)
   {
    /* suche gegenueber liegenden naechsten node  */
    rmin=MAX_INTEGER;
    for (j=0; j<set[set2].anz_n; j++ )
    {
      nj=set[set2].node[j];
      if ((nr2[j]>0)&&( nj!= ni))
      {
        dx= nx2[j]-nx1[i];
        dr= nr2[j]-nr1[i];
        R=sqrt( dx*dx + dr*dr );
        if (R<rmin)
        {
          rmin=R;
          node2j=j;
        }
      }
    }
    nj=set[set2].node[node2j];

    /* berechne den differenzwinkel  */
    phi[i]= nt2[node2j]-nt1[i];

    /* definiere den Zielwinkel, entweder vorgabe oder wie berechnet */
    if(phi_vorgabe==0.) phi_teilung=phi[i];
    else phi_teilung=phi_vorgabe;

    firstrun=1;
   nexttryz:;
    /* kontrolliere den differenzwinkel mit der teilung */
    dphi = phi_teilung - phi[i];
    if(dphi>2.*PI-DPHI_TOL) while (dphi>2.*PI-DPHI_TOL) dphi-=2.*PI;
    if(dphi<-2.*PI+DPHI_TOL) while (dphi<-2.*PI+DPHI_TOL) dphi+=2.*PI;
    if ((dphi*dphi > DPHI_TOL)&&(firstrun))
    {
      firstrun=0;
      phi_teilung=-phi_vorgabe;
      goto nexttryz;
    }
    else if ((dphi*dphi > DPHI_TOL)&&(!firstrun))
    {
      errMsg ("ERROR: korresponding node too far away:%lf grd, see set %s\n",dphi*180./PI, specialset->nompc);
      sprintf( buffer, "%d", ni); 
      pre_seta(specialset->nompc, "n", buffer);
    }
    else 
    {
    if(printFlag) printf(" theta(%d):%lf theta(%d):%lf  dtheta:%lf dtheta_corr:%lf rmin:%lf\n",
      ni, nt1[i]*180./PI, set[set2].node[node2j], nt2[node2j]*180./PI,
      phi[i]*180./PI, phi_teilung*180./PI, rmin);

    if (corr[0]=='c')
    { 
      /* versetze den gefundenen Knoten auf die korrespondierende pos. */
      /* der dependent node wird auf die pos des indep gesetzt */
      node[ni].nz = node[set[set2].node[node2j]].nz;
      if ( nr1[i] )
      {
        node[ni].nx = node[set[set2].node[node2j]].nx*cos(phi_teilung)-node[set[set2].node[node2j]].ny*sin(phi_teilung);
        node[ni].ny = node[set[set2].node[node2j]].nx*sin(phi_teilung)+node[set[set2].node[node2j]].ny*cos(phi_teilung);
      }  
    }  


   if ((csys=='t')||(csys=='p'))
   {
    if (compare( format, "abqf", 4)== 4)
    {
      /* schreibe die EQUATIOS im Abaqusformat */
      fprintf(handle, "*EQUATIONF\n");
        fprintf(handle, "%d\n", 2);
        if(csys=='t'){ 
          fprintf(handle, "%d,S%d,%d,%.12lf, %d,S%d,%d,%.12lf \n", face[nodbuf[ni]].elem_nr, face[nodbuf[ni]].nr+1, DOFT, 1.,
		  face[nodbuf[nj]].elem_nr, face[nodbuf[nj]].nr+1, DOFT, -1. );}
        else{ 
          fprintf(handle, "%d,S%d,%d,%.12lf, %d,S%d,%d,%.12lf \n", face[nodbuf[ni]].elem_nr, face[nodbuf[ni]].nr+1, DOFP, 1.,
		  face[nodbuf[nj]].elem_nr, face[nodbuf[nj]].nr+1, DOFP, -1. );}
    }
    else if (compare( format, "abq", 3)== 3)
    {
      /* schreibe die EQUATIOS im Abaqusformat */
      fprintf(handle, "*EQUATION\n");
        fprintf(handle, "%d\n", 2);
        if(csys=='t'){ 
          fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf \n", ni, DOFT, 1.,
		  set[set2].node[node2j], DOFT, -1. );}
        else{ 
          fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf \n", ni, DOFP, 1.,
		  set[set2].node[node2j], DOFP, -1. );}
    }
    else if (compare( format, "ids", 3)== 3)
    {
      /* schreibe die EQUATIOS als knotenliste */
      fprintf(handle, "%d\n", ni);
      fprintf(handle2, "%d\n", set[set2].node[node2j]);
    }
   }
   if (csys=='r')
    {
    if (compare( format, "abqf", 4)== 4)
      {
      /* schreibe die EQUATIOS im Abaqusformat */
      fprintf(handle, "*EQUATIONF\n");
        fprintf(handle, "%d\n", 2);
        fprintf(handle, "%d,S%d,%d,%.12lf, %d,S%d,%d,%.12lf \n", face[nodbuf[ni]].elem_nr, face[nodbuf[ni]].nr+1, DOFZ, 1.,
          face[nodbuf[nj]].elem_nr, face[nodbuf[nj]].nr+1, DOFZ, -1. );
      fprintf(handle, "*EQUATIONF\n");
        fprintf(handle, "%d\n", 3);
        fprintf(handle, "%d,S%d,%d,%.12lf, %d,S%d,%d,%.12lf, %d,S%d,%d,%.12lf\n", face[nodbuf[ni]].elem_nr, face[nodbuf[ni]].nr+1, DOFX, 1.,
          face[nodbuf[nj]].elem_nr, face[nodbuf[nj]].nr+1, DOFX, -cos(phi_teilung), face[nodbuf[nj]].elem_nr, face[nodbuf[nj]].nr+1, DOFY, sin(phi_teilung) );
      fprintf(handle, "*EQUATIONF\n");
        fprintf(handle, "%d\n", 3);
        fprintf(handle, "%d,S%d,%d,%.12lf, %d,S%d,%d,%.12lf, %d,S%d,%d,%.12lf\n", face[nodbuf[ni]].elem_nr, face[nodbuf[ni]].nr+1, DOFY, 1.,
          face[nodbuf[nj]].elem_nr, face[nodbuf[nj]].nr+1, DOFX, -sin(phi_teilung), face[nodbuf[nj]].elem_nr, face[nodbuf[nj]].nr+1, DOFY, -cos(phi_teilung) );
      }
    else if (compare( format, "abq", 3)== 3)
      {
      /* schreibe die EQUATIOS im Abaqusformat */
      fprintf(handle, "*EQUATION\n");
        fprintf(handle, "%d\n", 2);
        fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf \n", ni, DOFZ, 1.,
          set[set2].node[node2j], DOFZ, -1. );
      fprintf(handle, "*EQUATION\n");
        fprintf(handle, "%d\n", 3);
        fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf, %d,%d,%.12lf\n", ni, DOFX, 1.,
          set[set2].node[node2j], DOFX, -cos(phi_teilung),  set[set2].node[node2j], DOFY, sin(phi_teilung) );
      fprintf(handle, "*EQUATION\n");
        fprintf(handle, "%d\n", 3);
        fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf, %d,%d,%.12lf\n", ni, DOFY, 1.,
          set[set2].node[node2j], DOFX, -sin(phi_teilung),  set[set2].node[node2j], DOFY, -cos(phi_teilung) );
      }
    else if (compare( format, "ans", 3)== 3)
      {
      /* schreibe die EQUATIOS im Ansysformat */
      neqn++;
      fprintf(handle, "CE,%d,0,%d,%s,%.12lf, %d,%s,%.12lf,  %d,%s,%.12lf\n",
        neqn, -1, "UZ", 1.,
              ni, "UZ", 1., set[set2].node[node2j], "UZ", -1. );
      neqn++;
      fprintf(handle, "CE,%d,0,%d,%s,%.12lf, %d,%s,%.12lf,  %d,%s,%.12lf\n",
        neqn, ni, "UX", 1.,
              set[set2].node[node2j], "UX", -cos(phi_teilung), set[set2].node[node2j], "UY", sin(phi_teilung) );
      neqn++;
      fprintf(handle, "CE,%d,0,%d,%s,%.12lf, %d,%s,%.12lf,  %d,%s,%.12lf\n",
        neqn, ni, "UY", 1.,
              set[set2].node[node2j], "UX", -sin(phi_teilung), set[set2].node[node2j], "UY", -cos(phi_teilung) );
      }
    else if (compare( format, "nas", 3)== 3)
      {
      /* schreibe die EQUATIOS im Nastranformat */
      fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n", neqn+1, ni, DOFZ, 1.,
          set[set2].node[node2j], DOFZ, -1. );
      fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n", neqn+1, ni, DOFX, 1.,
                                                          set[set2].node[node2j], DOFX, -cos(phi_teilung));
      fprintf(handle, ", ,%8d,%8d,%.12lf\n", set[set2].node[node2j], DOFY, sin(phi_teilung) );
      fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n", neqn+1, ni, DOFY, 1.,
                                                          set[set2].node[node2j], DOFX, -sin(phi_teilung));
      fprintf(handle, ", ,%8d,%8d,%.12lf\n", set[set2].node[node2j], DOFY, -cos(phi_teilung) );
      }
    }
   if (csys=='c')
    {
    if (compare( format, "abq", 3)== 3)
      {
      /* schreibe die EQUATIOS im Abaqusformat */
      fprintf(handle, "*EQUATION\n");
        fprintf(handle, "%d\n", 2);
        fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf \n", ni, DOFX, 1.,
          set[set2].node[node2j], DOFX, -1. );
      fprintf(handle, "*EQUATION\n");
        fprintf(handle, "%d\n", 2);
        fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf \n", ni, DOFY, 1.,
          set[set2].node[node2j], DOFY, -1. );
      fprintf(handle, "*EQUATION\n");
        fprintf(handle, "%d\n", 2);
        fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf \n", ni, DOFZ, 1.,
          set[set2].node[node2j], DOFZ, -1. );
      }
    else if (compare( format, "ans", 3)== 3)
      {
      /* schreibe die EQUATIOS im Ansysformat */
      neqn++;
      fprintf(handle, "CE,%d,0,%d,%s,%.12lf, %d,%s,%.12lf,  %d,%s,%.12lf\n",
        neqn, -1, "UX", 1.,
              ni, "UX", 1., set[set2].node[node2j], "UX", -1. );
      neqn++;
      fprintf(handle, "CE,%d,0,%d,%s,%.12lf, %d,%s,%.12lf,  %d,%s,%.12lf\n",
        neqn, -1, "UY", 1.,
              ni, "UY", 1., set[set2].node[node2j], "UY", -1. );
      neqn++;
      fprintf(handle, "CE,%d,0,%d,%s,%.12lf, %d,%s,%.12lf,  %d,%s,%.12lf\n",
        neqn, -1, "UZ", 1.,
              ni, "UZ", 1., set[set2].node[node2j], "UZ", -1. );
      }
    else if (compare( format, "nas", 3)== 3)
      {
      /* schreibe die EQUATIOS im Nastranformat */
      fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n", neqn+1, ni, DOFX, 1.,
          set[set2].node[node2j], DOFX, -1. );
      fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n", neqn+1, ni, DOFY, 1.,
          set[set2].node[node2j], DOFY, -1. );
      fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n", neqn+1, ni, DOFZ, 1.,
          set[set2].node[node2j], DOFZ, -1. );
      }
    }
   sprintf( buffer, "%d", ni); 
   pre_seta(specialset->mpc, "n", buffer);
    }
   }
  }
 }
 else errMsg("ERROR: no axis of symmetry was specified:%c \n\n", (char)axis);

 end:;

  fclose(handle);
  if (compare( format, "ids", 3)== 3) fclose(handle2);

  if(nx1) free(nx1);
  if(nx2) free(nx2);
  if(nt1) free(nt1);
  if(nt2) free(nt2);
  if(nr1) free(nr1);
  if(nr2) free(nr2);
  if(phi) free(phi);
  scalNodes ( anz->n, node, scale );


  i=getSetNr(specialset->nompc);
  if (i>-1) if (set[i].anz_n>0)
  {
    printf(" WARNING:%d dependent nodes could not be connected, check set:%s\n", set[i].anz_n, set[i].name);
    return;
  }

  /* for cfd equations the nodes in the middle of the faces have to be deleted */
  if(compare( format, "abqf", 4)== 4)
  {
    for(i=lastNode+1; i<=anz->nmax; i++) node[i].pflag=-1;

    if ( (nodbuf = (int *)malloc( set[set1].anz_n * sizeof(int))) == NULL )
    {
      errMsg("\nERROR: malloc failed in cycmpc() \n\n");
      return;
    }
    for(i=0; i<set[set1].anz_n; i++) nodbuf[i]=set[set1].node[i];
    n=set[set1].anz_n;
    for(i=0; i<n; i++) setr(set1,"n",nodbuf[i]);
    free(nodbuf);
    if ( (nodbuf = (int *)malloc( set[set2].anz_n * sizeof(int))) == NULL )
    {
      errMsg("\nERROR: malloc failed in cycmpc() \n\n");
      return;
    }
    for(i=0; i<set[set2].anz_n; i++) nodbuf[i]=set[set2].node[i];
    n=set[set2].anz_n;
    for(i=0; i<n; i++) setr(set2,"n",nodbuf[i]);
    free(nodbuf);
  }    


}




/* die koeffizienten der MPCs ergeben sich aus den Flaechenanteilen des Dreiecks */
int calcCoefficientsTri(  Nodes *node, int n0, CTri3 *ctri3, int  e, double *dist, double *c, double *conode)
{
  int n1,n2,n3;
  double vn0[3],v01[3],v10[3],v02[3],v03[3],v12[3],v13[3],vNorm012[3],vNorm023[3],vNorm031[3],vNorm123[3],eNorm012[3],eNorm023[3],eNorm031[3],eNorm123[3];
  double ag,a1,a2,a3, sum_c, lproj;

  n1=ctri3[e].nod[0];
  n2=ctri3[e].nod[1];
  n3=ctri3[e].nod[2];
  v_result( &node[n1].nx, &node[n2].nx, v12);
  v_result( &node[n1].nx, &node[n3].nx, v13);

  /* flaeche des tri3 *2 */
  v_prod( v12, v13, vNorm123 );
  v_norm( vNorm123, eNorm123 );
  ag=v_betrag(vNorm123);

  /* normal dist between tri and node */
  v_result( &node[n1].nx, &node[n0].nx, v10);
  lproj=v_sprod(v10,eNorm123);
  *dist=lproj;

  /* projektion des nodes auf das dreieck */
  if(conode!=0)
  {
    conode[0]=vn0[0]=node[n0].nx-lproj*eNorm123[0];
    conode[1]=vn0[1]=node[n0].ny-lproj*eNorm123[1];
    conode[2]=vn0[2]=node[n0].nz-lproj*eNorm123[2];
    v_result( vn0, &node[n1].nx, v01);
    v_result( vn0, &node[n2].nx, v02);
    v_result( vn0, &node[n3].nx, v03);
  /*
    node[n0].nx=vn0[0];
    node[n0].ny=vn0[1];
    node[n0].nz=vn0[2];
  */
  }
  else
  {
    v_result( &node[n0].nx, &node[n1].nx, v01);
    v_result( &node[n0].nx, &node[n2].nx, v02);
    v_result( &node[n0].nx, &node[n3].nx, v03);
  }

  /* teilflaechen *2 */
  v_prod( v02, v03, vNorm023 ); v_norm( vNorm023, eNorm023 );
  v_prod( v03, v01, vNorm031 ); v_norm( vNorm031, eNorm031 );
  v_prod( v01, v02, vNorm012 ); v_norm( vNorm012, eNorm012 );
  a1=v_betrag(vNorm023);
  if(v_sprod(eNorm023, eNorm123) <0.) a1*=-1;
  a2=v_betrag(vNorm031);
  if(v_sprod(eNorm031, eNorm123) <0.) a2*=-1;
  a3=v_betrag(vNorm012);
  if(v_sprod(eNorm012, eNorm123) <0.) a3*=-1;

  /* coefficients */
  c[0]= -1;
  c[1]= a1/ag;
  c[2]= a2/ag;
  c[3]= a3/ag;
  sum_c= c[1]+c[2]+c[3];

  /*
  printf(" eNorm123:%lf %lf %lf \n", eNorm123[0],eNorm123[1],eNorm123[2] );
  printf(" eNorm023:%lf %lf %lf \n", eNorm023[0],eNorm023[1],eNorm023[2] );
  printf(" eNorm031:%lf %lf %lf \n", eNorm031[0],eNorm031[1],eNorm031[2] );
  printf(" eNorm012:%lf %lf %lf \n", eNorm012[0],eNorm012[1],eNorm012[2] );
  printf(" v_sprod(eNorm023, eNorm123):%lf\n",  v_sprod(eNorm023, eNorm123)); 
  printf(" v_sprod(eNorm031, eNorm123):%lf\n",  v_sprod(eNorm031, eNorm123));
  printf(" v_sprod(eNorm012, eNorm123):%lf\n", v_sprod(eNorm012, eNorm123));
  printf(" sum_c:%lf c:%lf %lf %lf\n", sum_c,c[1],c[2],c[3] );
  */
  if ((sum_c<MIN_C)||(sum_c>MAX_C)) return(-1);
  return(1);
}


/* liefert MAX_INTEGER wenn der node ausserhalb liegt, */
/* ansonsten den kuerzesten Abstand zur ebene.     */
double find_tri3( Nodes *node, int n, CTri3 *ctri3, int e)
{
   int i;
   int n0,n1,n2, inull, iplus, iminus;
   double  vN0N1[3], vN1N2[3], vN2N0[3], vN0N[3], vN1N[3],  vN2N[3],
          vprod0[3], vprod1[3], vprod2[3], vsprod[3], vnorm[3];
   double en[3], ev[3], eu[3], vNN0[3], g;

   /* der node liegt im Dreieck wenn: */
   /* - berechne die Vektorprodukte zwischen den Kantenvektoren und dem Vektor von den    */
   /*   Kantenpunkten zu dem node (v_prod= vP1P2 x vP1Pn)                                 */
   /* - berechne die Scalarprtodukte v_sprod zwischen den v_prod und dem Normalenvektor   */
   /*   des Dreiecks. Wenn alle v_sprod positiv sind, dann liegt der node innerhalb und   */
   /*   davor, wenn alle negativ sind dann dahinter. Wenn uneinheitlich, dann ausserhalb. */

   n0=ctri3[e].nod[0];
   n1=ctri3[e].nod[1];
   n2=ctri3[e].nod[2];
   v_result( &node[n0].nx, &node[n1].nx, vN0N1);
   v_result( &node[n1].nx, &node[n2].nx, vN1N2);
   v_result( &node[n2].nx, &node[n0].nx, vN2N0);
   v_result( &node[n0].nx, &node[n].nx, vN0N);
   v_result( &node[n1].nx, &node[n].nx, vN1N);
   v_result( &node[n2].nx, &node[n].nx, vN2N);

   /* berechne den Normalenvektor auf der flaeche */
   v_prod( vN0N1, vN1N2, vnorm );
   v_norm( vnorm, en );
   /* berechne die einheitsvektoren der ebenengleichung des tri3 */
   v_norm( vN0N1, eu );
   v_norm( vN1N2, ev );
   /* bestimme den Abstand zwischen den Aufpunkten der Normalen und in n verschobenen Ebene  */
   v_result( &node[n].nx, &node[n0].nx, vNN0 );

   /* berechne die Konstante g (Abstand)  pn_neu=pn-en*g  */
   g = AsplitL( vNN0, eu, ev, en );
   v_prod( vN0N1, vN0N, vprod0 );
   v_prod( vN1N2, vN1N, vprod1 );
   v_prod( vN2N0, vN2N, vprod2 );
   vsprod[0]= v_sprod( vnorm, vprod0);
   vsprod[1]= v_sprod( vnorm, vprod1);
   vsprod[2]= v_sprod( vnorm, vprod2);

   /*   kontrolle ob ein vsprod=0 ist. Dann liegt naemlich                */
   /*   der zu kontrollierende node genau ueber einer der umrandungen     */
   /*   der kontroll-flaeche (vprod ist dann senkrecht auf der normalen). */
   /*   Dann werden vsprod mit den gleichen vorzeichen zusammen-          */
   /*   gezaelt, dabei gelten die nuller als joker                        */

   /* setze sehr kleine zahlen zu NULL */
   //for (i=0; i<3; i++) { a=vsprod[i]*vsprod[i]; if(a<1.e-40)  vsprod[i]=0.; }


   inull=0;
   for (i=0; i<3; i++) if (vsprod[i] == 0.) inull++;

   iplus=inull;
   for (i=0; i<3; i++) if (vsprod[i] > 0.) iplus++;

   iminus=inull;
   for (i=0; i<3; i++) if (vsprod[i] < 0.) iminus++;
   /*
   printf (" vsprod: %le %le %le inull:%d iplus:%d iminus:%d offs:%lf\n",
       vsprod[0],vsprod[1],vsprod[2], inull, iplus, iminus, g );
   */
   /*           * wenn alle vectorprodukte das gleiche vorzeichen haben    *   */
   /*           * dann ist der node innerhalb der flaeche                  *   */

   if (iplus == 3) return(g);
   else if (iminus == 3) return(g);
   else return(MAX_INTEGER);
}



/* flag=1: clear temporary sets */
/* extrapolflag=0: do not extrapolate */
void areampc(int set1, int set2, char *format, char *type, char *value, char *corr, int flag, Nodes *nptr, int extrapolflag)
{
  int  i, j, k=0, l, n, nf=0, e, f;

  int    DOF[7]={0,1,1,1,0,0,0}, ds=0, allds=0;
  char    ansy_dofs[6][5]={"UX","UY","UZ","ROTX","ROTY","ROTZ"}; /* SHORT-CUTS FOR ANSYS DOFS */
  char     **dat=NULL;
  int      args=0;

  int     sflag={0};             /* 0:abq, 1:ans, 2:nas equations, 3:interpolate data */
  int     n_closest_nodes;
  int     terms;

  char   datout[MAX_LINE_LENGTH];
  char   buffer[MAX_LINE_LENGTH];
  char   dofbuf[8];

  CTri3     *ctri3=NULL;                   /* triangulierte indep-nodes flaeche */
  static int **tri3_index;                 /* am jeweiligen node haengenden dreiecke */
  int       *ntri_nodes=NULL;              /* anzahl der anhaengenden dreiecke       */
  int       sum_tri3;                      /* anzahl der ctri3 */
  int       ini_corrFlag=1;                /* move dep node to indep face (stress free), 1:correction on ("c") */
  int       corrFlag;                      /* temporary ini_corrFlag */
  int       forcedDispFlag=0;              /* moves the dep-nodes relative to the ind-nodes by a certain value or as calc in a previous step. */
  int       displSlideFlag=0;              /* generates additional solver statements to force the dep-node to the indep face during solving */  
  int       displStickFlag=0;              /* - and the dep node is "glued" to the indep face */ 
  int       cylsysFlag=0;                  /* specified dof's reference a cylindric system */

  int loops=0;

  FILE  *handle=NULL, *handle_boundary=NULL;

#if STORE_CONNECTIVITY
  neqn_ini=0;
#endif
  int n0, n1, n2;
  int   indeql, elbuf=0, anz_n;
  double dx,dy,dz, xx,yy,zz;
  double  offset=0., min_offset,offsetbuf=0., tol, dv;
  double  ve[3];
  /* nod_edir[dof][node][xyz]: radial, tangential, axial vector-components at the nodes of the mpc-connection */
  double  nod_edir[3][9][3];

  int   nface[8];
  double sum_c, c[9],  coords[24], ratio[8];
  double dist, conode[3], ndist[3], ndist_sqr=0., elemcoords[2];
  double p0[3], p1[3], p2[3], p0p1[3], p0p2[3], norm[3], norm_av[3], forcedDisp=0.;
  double pax[3],vax[3];
  int    settmp,  *surnod=NULL;
  int displNode=0;

  double *orig_x=NULL, *orig_y=NULL, *orig_z=NULL, *sort_x=NULL, *sort_y=NULL, *sort_z=NULL;
  int *sort_nx=NULL, *sort_ny=NULL, *sort_nz=NULL, near_node[N_CLOSEST_NODES];
  Rsort *rsort=NULL;

  static Nodes *node=NULL;
  Nodes *norm_dep;
  int   *sum_n_dep;
  int lines=0;

  tol=gtol;
  tol*=tol;

  /* ------ check and set the format ---- */
  sprintf(datout, "%s.equ", set[set1].name);
  if (compare(format,"abq", 3) == 3)
  {
    sflag=0; 
    handle = fopen (datout, "w");
    if (handle==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n", datout); return; }
    else  printf (" %s opened\n", datout);
    fprintf(handle, "** areampc based on set %s %s\n",set[set1].name, set[set2].name );
  }
  else if (compare(format,"ans", 3) == 3)
  {
    sflag=1;
    handle = fopen (datout, "w");
    if (handle==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n", datout); return; }
    else  printf (" %s opened\n", datout);
    fprintf(handle, "! areampc based on set %s %s\n",set[set1].name, set[set2].name );
  }
  else if (compare(format,"nas", 3) == 3)
  {
    sflag=2;
    handle = fopen (datout, "w");
    if (handle==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n", datout); return; }
    else  printf (" %s opened\n", datout);
    fprintf(handle, "$ areampc based on set %s %s\n",set[set1].name, set[set2].name );
    if((!neqn)&&(!nasMpc)) neqn=anz->emax;
#if STORE_CONNECTIVITY
    neqn_ini=neqn;
#endif
  }
  else if (compare(format,"map", 3) == 3)
  {
    sflag=3;
  }
  else 
  {
    errMsg("ERROR, format:%s not known \n\n", format);
    return;
  }


  /* ------- node-koordinaten berechnen und am ende wieder scalieren ------ */
  descalNodes ( anz->n, nptr, scale );


  /* remove all dependent nodes (set1) from the independent set (set2) */
  for(i=0; i<set[set1].anz_n; i++) setr(set2,"n",set[set1].node[i]);

  /* if flag==0: clear special sets */
  if(!flag)
  {
    delSet(specialset->impc );
    delSet(specialset->mpc );
    delSet(specialset->nompc );
    delSet(specialset->noel );
  }
  /* else: remove all already used dep-nodes from the set1 (dep-set) and also from set2 (ind-set) */
  /*       and  remove all already used ind-nodes from the set1 (dep-set) */
  else
  {
    k=getSetNr(specialset->mpc);
    if(k>-1) for(i=0; i<set[k].anz_n; i++) { setr(set1,"n",set[k].node[i]); setr(set2,"n",set[k].node[i]); }
    k=getSetNr(specialset->impc);
    if(k>-1) for(i=0; i<set[k].anz_n; i++) setr(set1,"n",set[k].node[i]);
  }      


  /* mapping only */
  if(sflag==3)
  {
    ini_corrFlag=0;
    if(value[0]=='d')
    {
      if(compareStrings( value, "ds" )>0)
      {
        /* all datasets should be interpolated */
        for(ds=0; ds<anz->l; ds++)
        {
          for(e=0; e<lcase[ds].ncomps; e++)
            printf(" interpol:%s entity:%s\n", lcase[ds].name, lcase[ds].compName[e]);
          /* check if the data of the specified lcase (Dataset) are already available */
          if (!lcase[ds].loaded)
          {
            if( pre_readfrdblock(copiedNodeSets , ds, anz, node, lcase )==-1) 
            {
              printf("ERROR in nodalDataset: Could not read data for Dataset:%d\n", ds+1); 
      	    return;
            }
            calcDatasets( ds, anz, node, lcase );
            recompileEntitiesInMenu(ds);
          }
        }
        allds=1;
      }
      else
      {
        ds=atoi(&value[2])-1; 
        if((ds<0)||(ds>anz->l-1)) { printf(" specified Dataset:%d not available\n",ds); return; }
        for(e=0; e<lcase[ds].ncomps; e++)
          printf(" interpol:%s entity:%s\n", lcase[ds].name, lcase[ds].compName[e]);
        /* check if the data of the specified lcase (Dataset) are already available */
        if (!lcase[ds].loaded)
        {
          if( pre_readfrdblock(copiedNodeSets , ds, anz, node, lcase )==-1) 
          {
            printf("ERROR in nodalDataset: Could not read data for Dataset:%d\n", ds+1); 
            return;
          }
          calcDatasets( ds, anz, node, lcase );
          recompileEntitiesInMenu(ds);
        }
      }
    }
    else { printf(" Dataset not given for interpolation of data\n"); return; }

    /* determine the dimension and type of mapping */
    /* map from volume to volume (ie. temperatures) */
    if(type[0]=='v')
    {
      /* not implemented so far but a quick solution is in the moment to use the 2D-mapper */
      node=nptr;
    }
    /* map to surfaces (ie. pressure) */
    else if(type[0]=='s')
    {
      node=nptr;
    }
    /* map2D rotational (rx,ry,rz) */
    else if(type[0]=='r')
    {
      /* transform all coords into rx coordinates */
      if ( (node = (Nodes *)realloc( (Nodes *)node, (anz->nmax+1) * sizeof(Nodes))) == NULL )
      { printf(" ERROR: realloc failure in areampc()\n\n"); return; }

      if(type[1]=='x')
      {
        for (i=0; i<anz->n; i++ )
        {
	  n=node[i].nr=nptr[i].nr;
	  node[n].indx=i;
          node[n].nx=sqrt(nptr[n].ny*nptr[n].ny+nptr[n].nz*nptr[n].nz);
          node[n].ny=0;
          node[n].nz=nptr[n].nx;
	  //printf("%d rtx: %f %f %f\n", n,node[n].nx, node[n].ny, node[n].nz); 
	}
      }
      else if(type[1]=='y')
      {
        for (i=0; i<anz->n; i++ )
        {
	  n=node[i].nr=nptr[i].nr;
	  node[n].indx=i;
          node[n].nx=sqrt(nptr[n].nx*nptr[n].nx+nptr[n].nz*nptr[n].nz);
          node[n].ny=0;
          node[n].nz=nptr[n].ny;
	  //printf("%d rtx: %f %f %f\n", n,node[n].nx, node[n].ny, node[n].nz); 
	}
      }
      else if(type[1]=='z')
      {
        for (i=0; i<anz->n; i++ )
        {
	  n=node[i].nr=nptr[i].nr;
	  node[n].indx=i;
          node[n].nx=sqrt(nptr[n].nx*nptr[n].nx+nptr[n].ny*nptr[n].ny);
          node[n].ny=0;
          node[n].nz=nptr[n].nz;
	  //printf("%d rtx: %f %f %f\n", n,node[n].nx, node[n].ny, node[n].nz); 
	}
      }
      else
      {
        printf(" ERROR: axis:%c not known\n", type[1]);
        return;
      }
    }
    else /* map2D translatoric */
    {
      /* transform all coords into rx coordinates */
      if ( (node = (Nodes *)realloc( (Nodes *)node, (anz->nmax+1) * sizeof(Nodes))) == NULL )
      { printf(" ERROR: realloc failure in areampc()\n\n"); return; }

      if(type[0]=='x')
      {
        for (i=0; i<anz->n; i++ )
        {
	  n=node[i].nr=nptr[i].nr;
	  node[n].indx=i;
          node[n].nx=0.;
          node[n].ny=nptr[n].ny;
          node[n].nz=nptr[n].nz;
	}
      }
      else if(type[0]=='y')
      {
        for (i=0; i<anz->n; i++ )
        {
	  n=node[i].nr=nptr[i].nr;
	  node[n].indx=i;
          node[n].nx=nptr[n].nx;
          node[n].ny=0.;
          node[n].nz=nptr[n].nz;
	}
      }
      else if(type[0]=='z')
      {
        for (i=0; i<anz->n; i++ )
        {
	  n=node[i].nr=nptr[i].nr;
	  node[n].indx=i;
          node[n].nx=nptr[n].nx;
          node[n].ny=nptr[n].ny;
          node[n].nz=0.;
	}
      }
      else
      {
        printf(" ERROR: direction:%c not known\n", type[0]);
        return;
      }
    }
  }

  /* only if no mapping */
  else
  {
    /* if no new nodes were generated (for mapping): */
    node=nptr;

    /* determine the starting nr of ansys-equations or for nastran-rbe's (if selected with the "asgn rbe" command) or the nastran-mpc-identifier */
    if ((corr[0]=='c')||(corr[0]=='u'))
    { if (atoi(&corr[1])>0) neqn=atoi(&corr[1])-1; }
    if (corr[0]=='f')
    {
      /* dep and ind should move relative to each other */
      forcedDispFlag=1;
      if (strlen(corr)>1) forcedDisp=atof(&corr[1]);
      displNode=anz->orignmax+1;
    }
  
    /* determine the degrees of freedom */
    /* first clear them */
    buffer[1]=0;
    for (i=0;i<7;i++)  DOF[i]=0;
  
    /* if nummerical dof */
    if (atoi(value)>0)
    { 
      if (forcedDispFlag)
      {
        sprintf(datout, "%s.bou", set[set1].name);
        handle_boundary = fopen (datout, "w");
        if (handle_boundary==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n", datout); return; }
        else  printf (" %s opened\n", datout);
      } 
  
      switch (sflag)
      {
        /* ABAQUS */
        case 0:
          fprintf(handle, "** WARNING: THE USE OF THE ORIGINAL COORDINATE SYSTEM IS MANDATORY\n");
          fprintf(handle, "** INCLUDE THE FOLLOWING LINES IN THE MODEL-DEFINITION-SECTION:\n");
          if (forcedDispFlag)
          {
            fprintf(handle_boundary, "** WARNING: THE USE OF THE ORIGINAL COORDINATE SYSTEM IS MANDATORY\n");
            fprintf(handle_boundary, "** INCLUDE THE FOLLOWING LINES IN A STEP:\n");
            if (strlen(corr)>1) fprintf(handle_boundary, "** FORCED DISPLACEMENT OF:%f REQUESTED\n", forcedDisp);
            else  fprintf(handle_boundary, "** FORCED DISPLACEMENT REQUESTED\n");
            if (forcedDisp!=0) fprintf(handle_boundary, "*BOUNDARY, OP=MOD\n");
            else fprintf(handle_boundary, "*BOUNDARY, FIXED\n");
  	}
        break;
      }

      /* get the dofs and eventually further parameters (separated by ',') */

      if((dat = (char **)realloc((char **)dat, (10)*sizeof(char*)))==NULL)
      errMsg("\n\n ERROR: realloc failed for **dat\n" );
      for (i=0; i<10; i++)
      {
        if((dat[i] = (char *)malloc((MAX_LINE_LENGTH)*sizeof(char)))==NULL)
          errMsg("\n\n ERROR: realloc failed for *dat\n" );
      }

      args=crecord(value, dat);
      for (i=0; i<strlen(dat[0]); i++)
      {
        buffer[0]=value[i];
        if (atoi(buffer)-1 >= 0) DOF[atoi(buffer)]=1;
      }
      /* further parameters? */  
      if(args==7) /* axis of cylsys, dofs are valid in this cylinder system */
      {
        cylsysFlag=1;
        for (i=0; i<3; i++) { pax[i]=atof(dat[i+1]); vax[i]=atof(dat[i+4]); }
        v_norm(vax, vax);
	printf(" dofs:%s in cylsys with axis pax: %f %f %f vax: %f %f %f\n",dat[0], pax[0],pax[1],pax[2], vax[0], vax[1], vax[2]);
      }
      else if(args!=1)  printf("ERROR: nr of args:%d in dofs must be 1 or 7\n",args);
      for(i=0; i<args; i++) free(dat[i]); free(dat);

      /* set the correction flag to correct the node position (in a first run). After the first run it is set to 0 again and then in a second run the equs are written */
      if (corr[0]=='u')
      {
        ini_corrFlag=0;
      }
      if (corr[0]=='c')
      {
        ini_corrFlag=1;
      }
    }
  
    /* if presfit
    force a deflection of the dep-node during solving to the indep faces */
    else if( compare( value, "presfit", 1)==1)
    { 
      /* do not correct the node position! */
      ini_corrFlag=0;
 
      /* calculate the average normal on every node */
      delSet("-presfit");
      if( (settmp=pre_seta( "-presfit", "i", 0 )) <0 ) return;
      for (k=0; k<set[set1].anz_n; k++)  seta( settmp, "n", set[set1].node[k]  );
      completeSet( "-presfit", "f");
      getNodeNormalen(&sum_n_dep, &norm_dep, settmp, anz, face);
  
  
      sprintf(datout, "%s.bou", set[set1].name);
      handle_boundary = fopen (datout, "w");
      if (handle_boundary==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n", datout); return; }
      else  printf (" %s opened\n", datout);
  
      DOF[1]=DOF[2]=DOF[3]=1;
      displNode=anz->orignmax+1;
      /* set the flag to force a deflection of the dep-node during solving only in normal direction */
      if (corr[0]=='s') displSlideFlag=1;
      else displStickFlag=1;
      if (strlen(corr)>1) forcedDisp=atof(&corr[1]);
  
      switch (sflag)
      {
        /* ABAQUS */
        case 0:
          fprintf(handle_boundary, "** WARNING: THE USE OF THE ORIGINAL COORDINATE SYSTEM IS MANDATORY\n");
          fprintf(handle_boundary, "** INCLUDE THE FOLLOWING LINES IN A STEP:\n");
          if (strlen(corr)>1) fprintf(handle_boundary, "** PRESS-FIT OF:%f REQUESTED\n", forcedDisp);
          else  fprintf(handle_boundary, "** PRESS-FIT OF MESH-OVERLAPPING REQUESTED\n");
          fprintf(handle_boundary, "*BOUNDARY\n");
          fprintf(handle, "** WARNING: THE USE OF THE ORIGINAL COORDINATE SYSTEM IS MANDATORY\n");
          fprintf(handle, "** INCLUDE THE FOLLOWING LINES IN THE MODEL-DEFINITION-SECTION:\n");
          if (strlen(corr)>1) fprintf(handle, "** PRESS-FIT OF:%f REQUESTED:\n", forcedDisp);
          else  fprintf(handle, "** PRESS-FIT OF MESH-OVERLAPPING REQUESTED:\n");
        break;
          printf("ERROR: format not supported for parameter=%s\n", value);
        return;
      }
    }
  
    /* "slide" is defined, mpcs for sliding in a plane defined by the dependent nodes */
    else if( compare( value, "slide", 1)==1)
    {
      DOF[3]=1;
      /* define a set of nodes which define the slide-condition */
      switch (sflag)
      {
        /* ABAQUS */
        case 0:
          fprintf(handle, "** WARNING: INCLUDES A NEW COORDINATE SYSTEM \n");
          fprintf(handle, "** INCLUDE THE FOLLOWING LINES IN THE MODEL-DEFINITION-SECTION:\n");
    
          /* determine the sliding plane */
          /* - search an element-face defined by dep-nodes (set1) */
          /* - get all dep-faces */
          delSet(specialset->tmp);
          if( (settmp=pre_seta( specialset->tmp, "i", 0 )) <0 ) return;
          for (k=0; k<set[set1].anz_n; k++)  seta( settmp, "n", set[set1].node[k]  );
          completeSet( specialset->tmp, "do");
    
          /* mark the surface nodes for easy element identification */
          if( (surnod=(int *)realloc((int *)surnod, (anz->nmax+1)*sizeof(int) ) )==NULL)
          { printf(" ERROR: malloc failure\n"); return; }
          for (i=0; i<=anz->nmax; i++) surnod[i]=0;
          for (i=0; i<set[set1].anz_n; i++) surnod[set[set1].node[i]]=1;    
          /* go over all dep-faces and average all normals */
          norm_av[0]=norm_av[1]=norm_av[2]=0.;
          if(set[settmp].anz_f<1)
	  {
            printf("ERROR: no sliding faces could be found. Check if at least all nodes of one element-face on the dependent side are included\n\n");
            return;
	  }
          for(f=0; f<set[settmp].anz_f; f++)
          {
            i=set[settmp].face[f];
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
                v_prod(p0p1, p0p2, norm );
                norm_av[0]+=norm[0];
                norm_av[1]+=norm[1];
                norm_av[2]+=norm[2];
              }
            }
          }
          v_norm(norm_av,norm);
          p0[0]=norm[2];
          p0[1]=norm[0];
          p0[2]=norm[1];
          v_prod(norm, p0, p1 );
          v_prod(norm, p1, p2 );
    
          /* define a set for abaqus which will be referenced by a transform command */
          fprintf(handle, "*NSET,NSET=%s%s\n",set[set1].name, set[set2].name );
          for (i=0; i<set[set1].anz_n; i++ ) fprintf(handle, "%d,\n", set[set1].node[i]);  
          for (i=0; i<set[set2].anz_n; i++ ) fprintf(handle, "%d,\n", set[set2].node[i]);  
    
          fprintf(handle, "*TRANSFORM,NSET=%s%s\n",set[set1].name, set[set2].name );
          fprintf(handle, "%lf, %lf, %lf, %lf, %lf, %lf\n", p1[0],p1[1],p1[2], p2[0],p2[1],p2[2] );
    
          // intf(handle, "%lf, %lf, %lf, %lf, %lf, %lf\n", p0p1[0],p0p1[1],p0p1[2], p0p2[0],p0p2[1],p0p2[2] );
        break;
          printf("ERROR: format not supported for parameter=%s\n", value);
        return;
      }
    }
  
    /* if thermal dof */
    else if (value[0]=='t') DOF[0]=1;
  
    /* if pressure dof */
    else if (value[0]=='p') DOF[0]=2;

    else 
    {
      errMsg("ERROR, parameter:%s not known \n\n", value);
      return;
    }

    if(printFlag) printf ("DOFs:"); for(i=0;i<7;i++) if(printFlag) printf("%d", DOF[i]);
    if(printFlag) printf ("\n1st Equ.Nr| ID   -e:%d\n", neqn);
  }

  if(printFlag) printf ("set1:%s nodes:%d set2:%s nodes:%d\n", set[set1].name, set[set1].anz_n, set[set2].name, set[set2].anz_n);
  if(printFlag) printf ("Tolerance for equal node -dr:%lf\n", gtol);
  if(printFlag) printf ("Tolerance for gap        -dA:%lf\n", 1.-MIN_C);
  /* if(printFlag) printf ("Tol for mismatch   -bail:%lf\n", BAILOUT); */
  printf(" please wait\n");


  /* Suche die elementflaechen, die durch die independent nodes beschrieben werden,       */
  /* und zerlege diese in dreiecke. Und speichere die zu jedem node gehoerenden dreiecke. */
  if(setall>-1)
    sum_tri3 = makeTriFromElems(set2, setall, anz->nmax, set, e_enqire, &ctri3, &tri3_index, &ntri_nodes);
  else { printf("ERROR: set all dose not exist\n"); goto end; }

  if(!sum_tri3)
  {
    errMsg ("ERROR: found no valid element\n\n" );
    goto end;
  }
  if(printFlag) printf (" %d ctri3 created from %d elements\n\n", sum_tri3, set[setall].anz_e );

#if TEST  
  for(i=0; i<sum_tri3; i++)
  {
    printf("%d e:%d s:%d nods: %d %d %d\n",i,
	   ctri3[i].elem_nr,
	   ctri3[i].group,  
	   ctri3[i].nod[0],
	   ctri3[i].nod[1],
	   ctri3[i].nod[2]);
  }
  for(i=0; i<set[set2].anz_n; i++)
  {
    printf("%d n:%d sum:%d", i,  set[set2].node[i], ntri_nodes[set[set2].node[i]]);
    for(j=0; j<ntri_nodes[set[set2].node[i]]; j++)  printf(" %d", tri3_index[set[set2].node[i]][j]);
    printf("\n");
  }
#endif


  /* for NASTRAN:  write the set-names, nastran-pids and 1st equation-nr for documentation purposes only (finds only the first indep set! tbu!) */
  if(printFlag)
  {
    /* search one dep element */
    for(j=0; j<anz->e; j++)
    {
      if (e_enqire[e_enqire[j].nr].type == 1)       nf=8;  /* HEXA8 */	
      else if (e_enqire[e_enqire[j].nr].type == 2)  nf=6;  /* PENTA6 */	
      else if (e_enqire[e_enqire[j].nr].type == 3)  nf=4;  /* TET4 */	
      else if (e_enqire[e_enqire[j].nr].type == 4)  nf=26; /* HEX20 */	
      else if (e_enqire[e_enqire[j].nr].type == 5)  nf=20; /* PENTA15 */	
      else if (e_enqire[e_enqire[j].nr].type == 6)  nf=10; /* TET10 */	
      else if (e_enqire[e_enqire[j].nr].type == 7)  nf=3;  /* TRI3  */	
      else if (e_enqire[e_enqire[j].nr].type == 8)  nf=6;  /* TRI6  */	
      else if (e_enqire[e_enqire[j].nr].type == 9)  nf=4;  /* QUAD4 */	
      else if (e_enqire[e_enqire[j].nr].type == 10) nf= 8; /* QUAD8 */	
      else if (e_enqire[e_enqire[j].nr].type == 11) nf= 2; /* BEAM */	
      else if (e_enqire[e_enqire[j].nr].type == 12) nf= 3; /* BEAM3 */       
      for(k=0; k<nf; k++) 
      { 
           if (e_enqire[e_enqire[j].nr].nod[k]==set[set1].node[0]) goto printtable;
      }
    }
  printtable:;
    if(j<anz->e) printf("areampc: %s %s  %d %d  %d\n", set[set1].name, set[set2].name, e_enqire[e_enqire[j].nr].mat, e_enqire[ctri3[0].elem_nr].mat, neqn+1);
    else printf("areampc: Warning: found no dep elem from nodes in set:%s\n", set[set1].name);
  }

  /* suche nodes, berechne und schreibe EQUATION's  */

  if((int)N_CLOSEST_NODES<set[set2].anz_n) n_closest_nodes=(int)N_CLOSEST_NODES; else n_closest_nodes=set[set2].anz_n;

  /* stelle daten fuer near3d bereit */
  if ( (rsort = (Rsort *)malloc( (set[set2].anz_n+1) * sizeof(Rsort))) == NULL )
    printf("ERROR: realloc failed: Rsort\n\n" ); 
  if ( (orig_x = (double *)malloc( (set[set2].anz_n+1) * sizeof(double))) == NULL )
    printf("ERROR: realloc failed in areampc\n\n" ); 
  if ( (orig_y = (double *)malloc( (set[set2].anz_n+1) * sizeof(double))) == NULL )
    printf("ERROR: realloc failed in areampc\n\n" ); 
  if ( (orig_z = (double *)malloc( (set[set2].anz_n+1) * sizeof(double))) == NULL )
    printf("ERROR: realloc failed in areampc\n\n" ); 
  if ( (sort_x = (double *)malloc( (set[set2].anz_n+1) * sizeof(double))) == NULL )
    printf("ERROR: realloc failed in areampc\n\n" ); 
  if ( (sort_y = (double *)malloc( (set[set2].anz_n+1) * sizeof(double))) == NULL )
    printf("ERROR: realloc failed in areampc\n\n" ); 
  if ( (sort_z = (double *)malloc( (set[set2].anz_n+1) * sizeof(double))) == NULL )
    printf("ERROR: realloc failed in areampc\n\n" ); 
  if ( (sort_nx = (int *)malloc( (set[set2].anz_n+1) * sizeof(int))) == NULL )
    printf("ERROR: realloc failed in areampc\n\n" ); 
  if ( (sort_ny = (int *)malloc( (set[set2].anz_n+1) * sizeof(int))) == NULL )
    printf("ERROR: realloc failed in areampc\n\n" ); 
  if ( (sort_nz = (int *)malloc( (set[set2].anz_n+1) * sizeof(int))) == NULL )
    printf("ERROR: realloc failed in areampc\n\n" ); 

  for(i=0; i<set[set2].anz_n; i++)
  {
    rsort[i].r=orig_x[i]=node[set[set2].node[i]].nx;
    rsort[i].i=i;
  }
  qsort( rsort, set[set2].anz_n, sizeof(Rsort), (void *)compareRsort );
  for(i=0; i<set[set2].anz_n; i++)
  {
    sort_x[i]=rsort[i].r;
    sort_nx[i]=rsort[i].i;
  }
  for(i=0; i<set[set2].anz_n; i++)
  {
    rsort[i].r=orig_y[i]=node[set[set2].node[i]].ny;
    rsort[i].i=i;
  }
  qsort( rsort, set[set2].anz_n, sizeof(Rsort), (void *)compareRsort );
  for(i=0; i<set[set2].anz_n; i++)
  {
    sort_y[i]=rsort[i].r;
    sort_ny[i]=rsort[i].i;
  }
  for(i=0; i<set[set2].anz_n; i++)
  {
    rsort[i].r=orig_z[i]=node[set[set2].node[i]].nz;
    rsort[i].i=i;
  }
  qsort( rsort, set[set2].anz_n, sizeof(Rsort), (void *)compareRsort );
  for(i=0; i<set[set2].anz_n; i++)
  {
    sort_z[i]=rsort[i].r;
    sort_nz[i]=rsort[i].i;
  }


  corrFlag=ini_corrFlag;

  secondRun:;

  /* to get the maximum quality of the mpc-connection it is necessary to keep only the digits of the node-coordinates */
  /* which can be written in the several solver formats */
  /*
  switch (sflag)
  {
    // ABAQUS
    case 0:
      for (i=0; i<anz->n; i++ )
      {
        node[node[i].nr].nx=(long long)(node[node[i].nr].nx*1e11)/1e11;
        node[node[i].nr].ny=(long long)(node[node[i].nr].ny*1e11)/1e11;
        node[node[i].nr].nz=(long long)(node[node[i].nr].nz*1e11)/1e11;
      }
    break;
    // ANSYS
    case 1:
      for (i=0; i<anz->n; i++ )
      {
        node[node[i].nr].nx=(long long)(node[node[i].nr].nx*1e11)/1e11;
        node[node[i].nr].ny=(long long)(node[node[i].nr].ny*1e11)/1e11;
        node[node[i].nr].nz=(long long)(node[node[i].nr].nz*1e11)/1e11;
      }
    break;
    // NASTRAN 16 digit format
    case 2:
      for (i=0; i<anz->n; i++ )
      {
        node[node[i].nr].nx=(long long)(node[node[i].nr].nx*1e8)/1e8;
        node[node[i].nr].ny=(long long)(node[node[i].nr].ny*1e8)/1e8;
        node[node[i].nr].nz=(long long)(node[node[i].nr].nz*1e8)/1e8;
      }
    break;
  }
  */

  for (i=0; i<set[set1].anz_n; i++ )
  {
    /* suche die naechst-liegenden indep-nodes  */

    near3d(orig_x,orig_y,orig_z,sort_x,sort_y,sort_z,sort_nx,sort_ny,sort_nz, node[set[set1].node[i]].nx,node[set[set1].node[i]].ny,
           node[set[set1].node[i]].nz, set[set2].anz_n, &near_node[0], n_closest_nodes);
    for (j=0; j<n_closest_nodes; j++) rsort[j].i=set[set2].node[near_node[j]];
    dx= node[rsort[0].i].nx - node[set[set1].node[i]].nx;
    dy= node[rsort[0].i].ny - node[set[set1].node[i]].ny;
    dz= node[rsort[0].i].nz - node[set[set1].node[i]].nz;      
    rsort[0].r=dx*dx + dy*dy + dz*dz;      

#if TEST
    if(set[set1].node[i]==2887) for (j=0; j<n_closest_nodes; j++)  printf("node:%d  n[%d]:%d \n", set[set1].node[i], j, set[set2].node[near_node[j]]); 
#endif

    indeql=0;
    //if(0)
    if(( rsort[0].r<tol )&&(!displSlideFlag)&&(forcedDisp==0.)&&(!corrFlag)) /* IF 2 NODES ARE EQUAL and no sliding and no forced displacement, corrFlag is set to 0 in the second run */
    {
      if(printFlag) printf("node:%d, found equal node:%d with dr:%lf\n"
      , set[set1].node[i], rsort[0].i, sqrt(rsort[0].r));
      indeql=rsort[0].i;
  
      /* if the dof's work in a cylindrical system then calc new coefficients in the basic system */
      if(cylsysFlag)
      {
        /* calculate the radial, tangential vectors from axis origin to dep- and indep-nodes, normal on axis to node*/
        v_result( pax, &node[set[set1].node[i]].nx, nod_edir[0][0]);
        v_prod( vax, nod_edir[0][0], nod_edir[1][0] );
        v_prod( nod_edir[1][0], vax, nod_edir[0][0] );
        v_norm( nod_edir[0][0], nod_edir[0][0] );
        v_norm( nod_edir[1][0], nod_edir[1][0] );
        v_norm( vax, nod_edir[2][0] );
      }

      /* schreibe die EQUATIONS */
      switch (sflag)
      {
        case 0:
        /* schreibe die EQUATIONS im Abaqusformat */
        if(forcedDispFlag)
        {
          fprintf(handle, "*NODE, NSET=%s%s\n", set[set1].name, set[set2].name);
          fprintf(handle, "%d,%lf,%lf,%lf\n",displNode, node[set[set1].node[i]].nx, node[set[set1].node[i]].ny, node[set[set1].node[i]].nz );
          for (e=0;e<7;e++)
          {
            if(DOF[e])
            {
	      if(e)
              {
		if(forcedDisp!=0) fprintf(handle_boundary, "%d, %d,%d,%lf\n", displNode,e,e, forcedDisp);
		else fprintf(handle_boundary, "%d, %d,%d\n", displNode,e,e);
                fprintf(handle, "*EQUATION\n");
                fprintf(handle, "%d\n", 3);
                fprintf(handle, "%d,%d,%.12lf,%d,%d,%.12lf,%d,%d,%.12lf \n", set[set1].node[i], e, -1., indeql, e, 1., displNode, e, 1. );
	      }
	    }
          }
          displNode++;
        }
        else if(cylsysFlag)
        {
          for (e=0;e<3;e++)
          {
            if(DOF[e+1])
	    {
              xx=nod_edir[e][0][0]*nod_edir[e][0][0];
              yy=nod_edir[e][0][1]*nod_edir[e][0][1];
              zz=nod_edir[e][0][2]*nod_edir[e][0][2];
              if((xx>MIN_VECTOR)&&(yy>MIN_VECTOR)
              &&(zz>MIN_VECTOR))
              {
                /* linking the dofx to the tangential direction */
                fprintf(handle, "*EQUATION\n");
                fprintf(handle, "%d\n", 6);
                fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf, %d,%d,%.12lf,\n %d,%d,%.12lf, %d,%d,%.12lf, %d,%d,%.12lf\n"
                , set[set1].node[i], DOFX, nod_edir[e][0][0]
                , set[set1].node[i], DOFY, nod_edir[e][0][1]
      	        , set[set1].node[i], DOFZ, nod_edir[e][0][2]
                , indeql, DOFX, -nod_edir[e][0][0]
                , indeql, DOFY, -nod_edir[e][0][1]
      	        , indeql, DOFZ, -nod_edir[e][0][2] );
              }
              else if((xx<=MIN_VECTOR)
              &&(yy>MIN_VECTOR)&&(zz>MIN_VECTOR))
              {
                 /* linking the dofy to the tangential direction */
                fprintf(handle, "*EQUATION\n");
                fprintf(handle, "%d\n", 4);
                fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf, %d,%d,%.12lf, %d,%d,%.12lf\n"
                , set[set1].node[i], DOFY, nod_edir[e][0][1]
      	        , set[set1].node[i], DOFZ, nod_edir[e][0][2]
                , indeql, DOFY, -nod_edir[e][0][1]
      	        , indeql, DOFZ, -nod_edir[e][0][2] );
              }
              else if((yy<=MIN_VECTOR)
              &&(xx>MIN_VECTOR)&&(zz>MIN_VECTOR))
              {
                /* linking the dofx to the tangential direction */
                fprintf(handle, "*EQUATION\n");
                fprintf(handle, "%d\n", 4);
                fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf, %d,%d,%.12lf, %d,%d,%.12lf\n"
                , set[set1].node[i], DOFX, nod_edir[e][0][0]
      	        , set[set1].node[i], DOFZ, nod_edir[e][0][2]
                , indeql, DOFX, -nod_edir[e][0][0]
      	        , indeql, DOFZ, -nod_edir[e][0][2] );
              }
              else if((zz<=MIN_VECTOR)
              &&(xx>MIN_VECTOR)&&(yy>MIN_VECTOR))
              {
                /* linking the dofx to the tangential direction */
                fprintf(handle, "*EQUATION\n");
                fprintf(handle, "%d\n", 4);
                fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf, %d,%d,%.12lf, %d,%d,%.12lf\n"
                , set[set1].node[i], DOFX, nod_edir[e][0][0]
      	        , set[set1].node[i], DOFY, nod_edir[e][0][1]
                , indeql, DOFX, -nod_edir[e][0][0]
      	        , indeql, DOFY, -nod_edir[e][0][1] );
              }
              else if((xx>MIN_VECTOR)
              &&(yy<=MIN_VECTOR)&&(zz<=MIN_VECTOR))
              {
                fprintf(handle, "*EQUATION\n");
                fprintf(handle, "%d\n", 2);
                fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf\n"
                , set[set1].node[i], DOFX, -1.
      	        , indeql, DOFX, 1.);
              }
              else if((yy>MIN_VECTOR)
              &&(xx<=MIN_VECTOR)&&(zz<=MIN_VECTOR))
              {
                fprintf(handle, "*EQUATION\n");
                fprintf(handle, "%d\n", 2);
                fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf\n"
                , set[set1].node[i], DOFY, -1.
      	        , indeql, DOFY, 1.);
              }
              else if((zz>MIN_VECTOR)
              &&(xx<=MIN_VECTOR)&&(yy<=MIN_VECTOR))
              {
                fprintf(handle, "*EQUATION\n");
                fprintf(handle, "%d\n", 2);
                fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf\n"
                , set[set1].node[i], DOFZ, -1.
      	        , indeql, DOFZ, 1.);
              }

            }
          }
        }
        else
        {
          for (e=0;e<7;e++)
          {
            if(DOF[e])
	    {
              fprintf(handle, "*EQUATION\n");
              fprintf(handle, "%d\n", 2);
              if(e) fprintf(handle, "%d,%d,%.12lf,%d,%d,%.12lf \n", set[set1].node[i], e, -1., indeql, e, 1. );
              else
	      {
                if(DOF[e]==1) fprintf(handle, "%d,%d,%.12lf,%d,%d,%.12lf \n", set[set1].node[i], DOFT, -1., indeql, DOFT, 1. );
                if(DOF[e]==2) fprintf(handle, "%d,%d,%.12lf,%d,%d,%.12lf \n", set[set1].node[i], DOFP, -1., indeql, DOFP, 1. );
	      }
	    }
          }
        }
        break;
        case 1:
        /* schreibe die EQUATIOS im Ansysformat */
        for (e=0;e<6;e++)
        {
          if(DOF[e+1])
          {
            neqn++;
	    /*
            fprintf(handle, "CE,%d,0,%d,%s,%.12lf, %d,%s,%.12lf,  %d,%s,%.12lf\n",
            neqn, set[set1].node[i], ansy_dofs[e], 1., indeql, ansy_dofs[e], -1., -1, ansy_dofs[e], 1. );
	    */
            fprintf(handle, "CE,%d,0,%d,%s,%.12lf, %d,%s,%.12d\n", neqn, set[set1].node[i], ansy_dofs[e], 1., indeql, ansy_dofs[e], -1);
          }
        }
        break;
        case 2:
	if(nasMpc)
        {
          /* schreibe die EQUATIOS im NASTRANformat als MPCs */
          if(cylsysFlag)
          {
            for (e=0;e<3;e++)
            {
              if(DOF[e+1])
              {
                xx=nod_edir[e][0][0]*nod_edir[e][0][0];
                yy=nod_edir[e][0][1]*nod_edir[e][0][1];
                zz=nod_edir[e][0][2]*nod_edir[e][0][2];
                if((xx>MIN_VECTOR)&&(yy>MIN_VECTOR)
                &&(zz>MIN_VECTOR))
                {
                  /* linking the dofx to the tangential direction */
		  /*
                  fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n", neqn+1
                  , set[set1].node[i], DOFX, nod_edir[e][0][0]
                  , set[set1].node[i], DOFY, nod_edir[e][0][1] );
                  fprintf(handle, ", ,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n"
        	  , set[set1].node[i], DOFZ, nod_edir[e][0][2]
                  , indeql, DOFX, -nod_edir[e][0][0] );
                  fprintf(handle, ", ,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n"
                  , indeql, DOFY, -nod_edir[e][0][1]
        	  , indeql, DOFZ, -nod_edir[e][0][2] );
		  */		  
                  fprintf(handle, "MPC*    %-16d%-16d%-16d%-.13lf\n", neqn+1
                  , set[set1].node[i], DOFX, nod_edir[e][0][0] );
                  fprintf(handle, "*       %-16d%-16d%-.13lf\n"
                  , set[set1].node[i], DOFY, nod_edir[e][0][1] );
                  fprintf(handle, "*                       %-16d%-16d%-.13lf\n"
       	          , set[set1].node[i], DOFZ, nod_edir[e][0][2] );
                  fprintf(handle, "*       %16d%16d%.13lf\n"
                  , indeql, DOFX, -nod_edir[e][0][0] );
                  fprintf(handle, "*                       %-16d%-16d%-.13lf\n"
                  , indeql, DOFY, -nod_edir[e][0][1] );
                  fprintf(handle, "*       %-16d%-16d%-.13lf\n"
        	  , indeql, DOFZ, -nod_edir[e][0][2] );
		  
                }
                else if((xx<=MIN_VECTOR)
                &&(yy>MIN_VECTOR)&&(zz>MIN_VECTOR))
                {
                   /* linking the dofy to the tangential direction */
		  /*
                  fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n", neqn+1
                  , set[set1].node[i], DOFY, nod_edir[e][0][1]
        	  , set[set1].node[i], DOFZ, nod_edir[e][0][2] );
                  fprintf(handle, ", ,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n"
                  , indeql, DOFY, -nod_edir[e][0][1]
                  , indeql, DOFZ, -nod_edir[e][0][2] );
		  */
                  fprintf(handle, "MPC*    %-16d%-16d%-16d%-.13lf\n", neqn+1
                  , set[set1].node[i], DOFY, nod_edir[e][0][1] );
                  fprintf(handle, "*       %-16d%-16d%-.13lf\n"
        	  , set[set1].node[i], DOFZ, nod_edir[e][0][2] );
                  fprintf(handle, "*                       %-16d%-16d%-.13lf\n"
                  , indeql, DOFY, -nod_edir[e][0][1] );
                  fprintf(handle, "*       %-16d%-16d%-.13lf\n"
                  , indeql, DOFZ, -nod_edir[e][0][2] );
                }
                else if((yy<=MIN_VECTOR)
                &&(xx>MIN_VECTOR)&&(zz>MIN_VECTOR))
                {
                  /* linking the dofx to the tangential direction */
		  /*
                  fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n", neqn+1
                  , set[set1].node[i], DOFX, nod_edir[e][0][0]
        	  , set[set1].node[i], DOFZ, nod_edir[e][0][2] );
                  fprintf(handle, ", ,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n"
                  , indeql, DOFX, -nod_edir[e][0][0]
        	  , indeql, DOFZ, -nod_edir[e][0][2] );
		  */
                  fprintf(handle, "MPC*    %-16d%-16d%-16d%-.13lf\n", neqn+1
                  , set[set1].node[i], DOFX, nod_edir[e][0][0] );
                  fprintf(handle, "*       %-16d%-16d%-.13lf\n"
        	  , set[set1].node[i], DOFZ, nod_edir[e][0][2] );
                  fprintf(handle, "*                       %-16d%-16d%-.13lf\n"
                  , indeql, DOFX, -nod_edir[e][0][0] );
                  fprintf(handle, "*       %-16d%-16d%-.13lf\n"
        	  , indeql, DOFZ, -nod_edir[e][0][2] );
                }
                else if((zz<=MIN_VECTOR)
                &&(xx>MIN_VECTOR)&&(yy>MIN_VECTOR))
                {
                  /* linking the dofx to the tangential direction */
		  /*
                  fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n", neqn+1
                  , set[set1].node[i], DOFX, nod_edir[e][0][0]
        	  , set[set1].node[i], DOFY, nod_edir[e][0][1] );
                  fprintf(handle, ", ,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n"
                  , indeql, DOFX, -nod_edir[e][0][0]
        	  , indeql, DOFY, -nod_edir[e][0][1] );
                  */
                  fprintf(handle, "MPC*    %-16d%-16d%-16d%-.13lf\n", neqn+1
                  , set[set1].node[i], DOFX, nod_edir[e][0][0] );
                  fprintf(handle, "*       %-16d%-16d%-.13lf\n"
        	  , set[set1].node[i], DOFY, nod_edir[e][0][1] );
                  fprintf(handle, "*                       %-16d%-16d%-.13lf\n"
                  , indeql, DOFX, -nod_edir[e][0][0] );
                  fprintf(handle, "*       %-16d%-16d%-.13lf\n"
        	  , indeql, DOFY, -nod_edir[e][0][1] );
                }
                else if((xx>MIN_VECTOR)
                &&(yy<=MIN_VECTOR)&&(zz<=MIN_VECTOR))
                {
		  /*
                  fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n", neqn+1
                  , set[set1].node[i], DOFX, -1.
        	  , indeql, DOFX, 1.);
		  */
                  fprintf(handle, "MPC*    %-16d%-16d%-16d%-.13lf\n", neqn+1
                  , set[set1].node[i], DOFX, -1. );
                  fprintf(handle, "*       %-16d%-16d%-.13lf\n"
        	  , indeql, DOFX, 1. );
                }
                else if((yy>MIN_VECTOR)
                &&(xx<=MIN_VECTOR)&&(zz<=MIN_VECTOR))
                {
		  /*
                  fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n", neqn+1
                  , set[set1].node[i], DOFY, -1.
        	  , indeql, DOFY, 1.);
		  */
                  fprintf(handle, "MPC*    %-16d%-16d%-16d%-.13lf\n", neqn+1
                  , set[set1].node[i], DOFY, -1. );
                  fprintf(handle, "*       %-16d%-16d%-.13lf\n"
        	  , indeql, DOFY, 1. );
                }
                else if((zz>MIN_VECTOR)
                &&(xx<=MIN_VECTOR)&&(yy<=MIN_VECTOR))
                {
		  /*
                  fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n", neqn+1
                  , set[set1].node[i], DOFZ, -1.
                  , indeql, DOFZ, 1.);
		  */
                  fprintf(handle, "MPC*    %-16d%-16d%-16d%-.13lf\n", neqn+1
                  , set[set1].node[i], DOFZ, -1. );
                  fprintf(handle, "*       %-16d%-16d%-.13lf\n"
                  , indeql, DOFZ, 1. );
                }
  
              }
            }
          }
          else
          {
            for (e=1;e<7;e++)
            {
              if(DOF[e])
              {
                //fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n"
                //, neqn+1, set[set1].node[i],e,-1.,indeql,e,1.);
                //fprintf(handle, "MPC*    %16d%16d%16d%.13lf   \n*       %16d%16d%.13lf\n*       \n"
                fprintf(handle, "MPC*    %16d%16d%16d%.13lf   \n*       %16d%16d%.13lf\n"
                , neqn+1, set[set1].node[i],e,-1.,indeql,e,1.);
              }
            }
          }
        }
        else
	{
          for(j=0; j<8; j++) dofbuf[j]=0;
          j=0; for(e=1; e<7; e++) { if(DOF[e]) { sprintf(&dofbuf[j],"%1d",e); j++; } }

          /* schreibe die EQUATIOS im Nastranformat */
          /* equivalence both nodes in any case, otherwise rbe2 might not work */
          node[set[set1].node[i]].nx=node[indeql].nx;
          node[set[set1].node[i]].ny=node[indeql].ny;
          node[set[set1].node[i]].nz=node[indeql].nz;
          //if(nasRbeHec>0.)  fprintf(handle, "RBE2,%8d,%8d,%8s,%8d,%.1e\n", ++neqn, set[set1].node[i], dofbuf, indeql, nasRbeHec );
          //else fprintf(handle, "RBE2,%8d,%8d,%8s,%8d\n", ++neqn, set[set1].node[i], dofbuf, indeql );
          //if(nasRbeHec>0.)  fprintf(handle, "RBE2    %8d%8d%8s%8d%.1e\n", ++neqn, set[set1].node[i], dofbuf, indeql, nasRbeHec );
          //if(nasRbeHec>0.)  fprintf(handle, "RBE2*   %16d%16d%16s%16d\n*       %.9e \n", ++neqn, set[set1].node[i], dofbuf, indeql, nasRbeHec );
          //else fprintf(handle, "RBE2    %8d%8d%8s%8d\n", ++neqn, set[set1].node[i], dofbuf, indeql );

          /* to avoid rbe2-shortcommings use mpc, mpc-id is 1, to be activated in input-file with mpc=1 */ 
          for (e=1;e<7;e++)
          {
            if(DOF[e])
            {
              fprintf(handle, "MPC,1,%8d,%8d, -1.,%8d,%8d, 1.\n", set[set1].node[i],e,indeql,e);
            }
          }
	}
        break;

        /* a loadcase is specified for interpolation */
        case 3:
        if(allds)
        {
          /* all datasets should be interpolated */
          for(ds=0; ds<anz->l; ds++)
          {
            for(e=0; e<lcase[ds].ncomps; e++)
            {
              lcase[ds].dat[e][set[set1].node[i]]=lcase[ds].dat[e][indeql];
	    }
	  }
	}
        else
        {
          for(e=0; e<lcase[ds].ncomps; e++)
          {
            lcase[ds].dat[e][set[set1].node[i]]=lcase[ds].dat[e][indeql];
	  }
	}
        break;
      }
    }
    else
    {
      if(printFlag)
        printf("node:%d, found non equal node:%d at dist:%lf\n", set[set1].node[i], rsort[0].i, sqrt(rsort[0].r));

      /* knoten liegt zwischen den indep-nodes, suche das passende element.       */
      /* fange mit den elementen des naechstliegenden nodes an, und kontrolliere   */
      /* mit dem vectorprodukt welche elementflaeche den node umschliest          */

      min_offset=MAX_INTEGER;
      for (n=0; n<n_closest_nodes; n++)
      {
        for (j=0; j<ntri_nodes[ rsort[n].i ]; j++)
        {
          e=tri3_index[ rsort[n].i ][j];
          offset=find_tri3( node, set[set1].node[i], ctri3, e);
          if( offset != MAX_INTEGER )
          {
            
	    //if( set[set1].node[i]==3539) 
            //  printf(" found enclosing tri3:%d for node:%d, offset:%lf\n", e+1, set[set1].node[i], offset);
	    
            /* merke dir das element und den offset */
            if (min_offset > offset*offset ) { min_offset=offset*offset;  elbuf=e; offsetbuf=offset;}
          }
        }
      }

      if(!extrapolflag)
      {
        if (min_offset>tol)
	{
          if(printFlag)
            printf("WARNING: no coefficients could be calculated, no MPC created for node:%d, see set %s\n", set[set1].node[i], specialset->nompc);
          goto create_no_mpc;
	}
      }

      if (min_offset==MAX_INTEGER)
      {
        //printf("WARNING: no enclosing element for dep-node:%d found, see set %s\n", set[set1].node[i], specialset->noel );
        sprintf( buffer, "%d", set[set1].node[i]); 
        pre_seta(specialset->noel, "n", buffer);

        /* kontrolliere welches zum naechsten node gehoerende dreieck */
        /* relativ am naechsten liegt und ob der grenzwert unterschritten ist */
        /* kriterium: flaechenfehler aller tri-set[set1].node[i] poligone */
        for (n=0; n<n_closest_nodes; n++) if(ntri_nodes[ rsort[n].i ]>0) break;
        if(n==n_closest_nodes)
        {
          printf("WARNING: no coefficients could be calculated, no MPC created for node:%d, see set %s\n", set[set1].node[i], specialset->nompc);
          goto create_no_mpc;
        }
        for (j=0; j<ntri_nodes[ rsort[n].i ]; j++)
        {
          e=tri3_index[ rsort[n].i ][j];
#if TEST    	    
          printf("suche fuer n:%d das naechste Tri3 e:%d  nod:%d %d %d \n",
            rsort[0].i, e+1,ctri3[e].nod[0],ctri3[e].nod[1],ctri3[e].nod[2] );
#endif	    
          if ( calcCoefficientsTri( node, set[set1].node[i], ctri3, e, &dist, c, 0) != -1 )
          {
            offset=c[1]*c[1]+c[2]*c[2]+c[3]*c[3];
            if(offset<min_offset){ min_offset=offset; elbuf=e; }
#if TEST    
            printf(" qadsum:%lf coef:%lf %lf %lf\n",
              offset, c[3], c[1], c[2] );
#endif	    
          }    
	}
        if(min_offset==MAX_INTEGER)
        {
          printf("WARNING: no coefficients could be calculated, no MPC created for node:%d, see set %s\n", set[set1].node[i], specialset->nompc);
          goto create_no_mpc;
        }
	/*
        else    
        {
          printf(" but found a close element:%d\n", ctri3[elbuf].elem_nr );
        }
	*/
      }
      else
      {
        offset=offsetbuf;
        if(printFlag)
          printf("closest tri3:%d elemnr:%d (%d %d %d)for node:%d, offset:%lf\n", elbuf+1, ctri3[elbuf].elem_nr, ctri3[elbuf].nod[0],ctri3[elbuf].nod[1],ctri3[elbuf].nod[2], set[set1].node[i], offset);
      }    

      /* das umschliessende Tri3 ist nun bekannt, wenn die korrekte shapefunction des */
      /* Mutterelements noch nicht eingebaut ist, haenge den node direkt an das Tri3  */
      if ( e_enqire[ ctri3[elbuf].elem_nr ].type ==  1 )
      {
        /* berechnung der MPC-Koeffizienten fuer he8  */

        /* knoten der betroffenen elementseite filtern */
        switch(ctri3[elbuf].group)
        {
          case 1:
          nface[0]=e_enqire[ ctri3[elbuf].elem_nr ].nod[0];
          nface[1]=e_enqire[ ctri3[elbuf].elem_nr ].nod[1];
          nface[2]=e_enqire[ ctri3[elbuf].elem_nr ].nod[2];
          nface[3]=e_enqire[ ctri3[elbuf].elem_nr ].nod[3];
          break;
          case 2:
          nface[0]=e_enqire[ ctri3[elbuf].elem_nr ].nod[0];
          nface[1]=e_enqire[ ctri3[elbuf].elem_nr ].nod[1];
          nface[2]=e_enqire[ ctri3[elbuf].elem_nr ].nod[5];
          nface[3]=e_enqire[ ctri3[elbuf].elem_nr ].nod[4];
          break;
          case 3:
          nface[0]=e_enqire[ ctri3[elbuf].elem_nr ].nod[1];
          nface[1]=e_enqire[ ctri3[elbuf].elem_nr ].nod[2];
          nface[2]=e_enqire[ ctri3[elbuf].elem_nr ].nod[6];
          nface[3]=e_enqire[ ctri3[elbuf].elem_nr ].nod[5];
          break;
          case 4:
          nface[0]=e_enqire[ ctri3[elbuf].elem_nr ].nod[2];
          nface[1]=e_enqire[ ctri3[elbuf].elem_nr ].nod[3];
          nface[2]=e_enqire[ ctri3[elbuf].elem_nr ].nod[7];
          nface[3]=e_enqire[ ctri3[elbuf].elem_nr ].nod[6];
          break;
          case 5:
          nface[0]=e_enqire[ ctri3[elbuf].elem_nr ].nod[3];
          nface[1]=e_enqire[ ctri3[elbuf].elem_nr ].nod[0];
          nface[2]=e_enqire[ ctri3[elbuf].elem_nr ].nod[4];
          nface[3]=e_enqire[ ctri3[elbuf].elem_nr ].nod[7];
          break;
          case 6:
          nface[0]=e_enqire[ ctri3[elbuf].elem_nr ].nod[4];
          nface[1]=e_enqire[ ctri3[elbuf].elem_nr ].nod[5];
          nface[2]=e_enqire[ ctri3[elbuf].elem_nr ].nod[6];
          nface[3]=e_enqire[ ctri3[elbuf].elem_nr ].nod[7];
          break;
        }

        /*
        attaches node with coordinates in "conode" to the face containing 
        "nterms" nodes with coordinates in field "coords" (nterms < 9).
        */
        n=4;

        for (j=0;j<n;j++)
        {
          coords[j*3]=  node[nface[j]].nx; 
          coords[j*3+1]=node[nface[j]].ny; 
          coords[j*3+2]=node[nface[j]].nz;
          /*
          printf (" nface[%d]:%d x:%lf y:%lf z:%lf\n", n, nface[j],
          coords[j*3], coords[j*3+1], coords[j*3+2] );
          */
        }
        conode[0]=node[set[set1].node[i]].nx;
        conode[1]=node[set[set1].node[i]].ny;
        conode[2]=node[set[set1].node[i]].nz;
        attach_new(coords,conode,&n,ratio,&dist, elemcoords);

        c[0]=0.;
        for(j=0; j<n; j++) c[0]+=c[j+1]=ratio[j];
        c[0]*=-1.;
        k=5;
      }
      else if ( e_enqire[ ctri3[elbuf].elem_nr ].type == 2 )
      {
        /* berechnung der MPC-Koeffizienten fuer pe6 */

        /* knoten der betroffenen elementseite filtern */
        switch(ctri3[elbuf].group)
        {
          case 1:
          nface[0]=e_enqire[ ctri3[elbuf].elem_nr ].nod[0];
          nface[1]=e_enqire[ ctri3[elbuf].elem_nr ].nod[1];
          nface[2]=e_enqire[ ctri3[elbuf].elem_nr ].nod[2];
          n=3;
          break;
          case 2:
          nface[0]=e_enqire[ ctri3[elbuf].elem_nr ].nod[3];
          nface[1]=e_enqire[ ctri3[elbuf].elem_nr ].nod[4];
          nface[2]=e_enqire[ ctri3[elbuf].elem_nr ].nod[5];
          n=3;
          break;
          case 3:
          nface[0]=e_enqire[ ctri3[elbuf].elem_nr ].nod[0];
          nface[1]=e_enqire[ ctri3[elbuf].elem_nr ].nod[1];
          nface[2]=e_enqire[ ctri3[elbuf].elem_nr ].nod[4];
          nface[3]=e_enqire[ ctri3[elbuf].elem_nr ].nod[3];
          n=4;
          break;
          case 4:
          nface[0]=e_enqire[ ctri3[elbuf].elem_nr ].nod[1];
          nface[1]=e_enqire[ ctri3[elbuf].elem_nr ].nod[2];
          nface[2]=e_enqire[ ctri3[elbuf].elem_nr ].nod[5];
          nface[3]=e_enqire[ ctri3[elbuf].elem_nr ].nod[4];
          n=4;
          break;
          case 5:
          nface[0]=e_enqire[ ctri3[elbuf].elem_nr ].nod[2];
          nface[1]=e_enqire[ ctri3[elbuf].elem_nr ].nod[0];
          nface[2]=e_enqire[ ctri3[elbuf].elem_nr ].nod[3];
          nface[3]=e_enqire[ ctri3[elbuf].elem_nr ].nod[5];
          n=4;
          break;
        }

        /*
        attaches node with coordinates in "conode" to the face containing 
        "nterms" nodes with coordinates in field "coords" (nterms < 9).
        */

        for (j=0;j<n;j++)
        {
          coords[j*3]=  node[nface[j]].nx; 
          coords[j*3+1]=node[nface[j]].ny; 
          coords[j*3+2]=node[nface[j]].nz;
          /*
          printf (" nface[%d]:%d x:%lf y:%lf z:%lf\n", n, nface[j],
          coords[j*3], coords[j*3+1], coords[j*3+2] );
          */
        }
        conode[0]=node[set[set1].node[i]].nx;
        conode[1]=node[set[set1].node[i]].ny;
        conode[2]=node[set[set1].node[i]].nz;
        attach_new(coords,conode,&n,ratio,&dist, elemcoords);

        c[0]=0.;
        for(j=0; j<n; j++) c[0]+=c[j+1]=ratio[j];
        c[0]*=-1.;
        k=n+1;
      }
      else if ( e_enqire[ ctri3[elbuf].elem_nr ].type == 4 )
      {
        /* berechnung der MPC-Koeffizienten fuer he20 */

        /* knoten der betroffenen elementseite filtern */
        switch(ctri3[elbuf].group)
        {
          case 1:
          nface[0]=e_enqire[ ctri3[elbuf].elem_nr ].nod[0];
          nface[1]=e_enqire[ ctri3[elbuf].elem_nr ].nod[1];
          nface[2]=e_enqire[ ctri3[elbuf].elem_nr ].nod[2];
          nface[3]=e_enqire[ ctri3[elbuf].elem_nr ].nod[3];
          nface[4]=e_enqire[ ctri3[elbuf].elem_nr ].nod[8];
          nface[5]=e_enqire[ ctri3[elbuf].elem_nr ].nod[9];
          nface[6]=e_enqire[ ctri3[elbuf].elem_nr ].nod[10];
          nface[7]=e_enqire[ ctri3[elbuf].elem_nr ].nod[11];
          break;
          case 2:
          nface[0]=e_enqire[ ctri3[elbuf].elem_nr ].nod[0];
          nface[1]=e_enqire[ ctri3[elbuf].elem_nr ].nod[1];
          nface[2]=e_enqire[ ctri3[elbuf].elem_nr ].nod[5];
          nface[3]=e_enqire[ ctri3[elbuf].elem_nr ].nod[4];
          nface[4]=e_enqire[ ctri3[elbuf].elem_nr ].nod[8];
          nface[5]=e_enqire[ ctri3[elbuf].elem_nr ].nod[13];
          nface[6]=e_enqire[ ctri3[elbuf].elem_nr ].nod[16];
          nface[7]=e_enqire[ ctri3[elbuf].elem_nr ].nod[12];
          break;
          case 3:
          nface[0]=e_enqire[ ctri3[elbuf].elem_nr ].nod[1];
          nface[1]=e_enqire[ ctri3[elbuf].elem_nr ].nod[2];
          nface[2]=e_enqire[ ctri3[elbuf].elem_nr ].nod[6];
          nface[3]=e_enqire[ ctri3[elbuf].elem_nr ].nod[5];
          nface[4]=e_enqire[ ctri3[elbuf].elem_nr ].nod[9];
          nface[5]=e_enqire[ ctri3[elbuf].elem_nr ].nod[14];
          nface[6]=e_enqire[ ctri3[elbuf].elem_nr ].nod[17];
          nface[7]=e_enqire[ ctri3[elbuf].elem_nr ].nod[13];
          break;
          case 4:
          nface[0]=e_enqire[ ctri3[elbuf].elem_nr ].nod[2];
          nface[1]=e_enqire[ ctri3[elbuf].elem_nr ].nod[3];
          nface[2]=e_enqire[ ctri3[elbuf].elem_nr ].nod[7];
          nface[3]=e_enqire[ ctri3[elbuf].elem_nr ].nod[6];
          nface[4]=e_enqire[ ctri3[elbuf].elem_nr ].nod[10];
          nface[5]=e_enqire[ ctri3[elbuf].elem_nr ].nod[15];
          nface[6]=e_enqire[ ctri3[elbuf].elem_nr ].nod[18];
          nface[7]=e_enqire[ ctri3[elbuf].elem_nr ].nod[14];
          break;
          case 5:
          nface[0]=e_enqire[ ctri3[elbuf].elem_nr ].nod[3];
          nface[1]=e_enqire[ ctri3[elbuf].elem_nr ].nod[0];
          nface[2]=e_enqire[ ctri3[elbuf].elem_nr ].nod[4];
          nface[3]=e_enqire[ ctri3[elbuf].elem_nr ].nod[7];
          nface[4]=e_enqire[ ctri3[elbuf].elem_nr ].nod[11];
          nface[5]=e_enqire[ ctri3[elbuf].elem_nr ].nod[12];
          nface[6]=e_enqire[ ctri3[elbuf].elem_nr ].nod[19];
          nface[7]=e_enqire[ ctri3[elbuf].elem_nr ].nod[15];
          break;
          case 6:
          nface[0]=e_enqire[ ctri3[elbuf].elem_nr ].nod[4];
          nface[1]=e_enqire[ ctri3[elbuf].elem_nr ].nod[5];
          nface[2]=e_enqire[ ctri3[elbuf].elem_nr ].nod[6];
          nface[3]=e_enqire[ ctri3[elbuf].elem_nr ].nod[7];
          nface[4]=e_enqire[ ctri3[elbuf].elem_nr ].nod[16];
          nface[5]=e_enqire[ ctri3[elbuf].elem_nr ].nod[17];
          nface[6]=e_enqire[ ctri3[elbuf].elem_nr ].nod[18];
          nface[7]=e_enqire[ ctri3[elbuf].elem_nr ].nod[19];
          break;
        }

        /*
        attaches node with coordinates in "conode" to the face containing 
        "nterms" nodes with coordinates in field "coords" (nterms < 9).
        */
        n=8;

        for (j=0;j<n;j++)
        {
          coords[j*3]=  node[nface[j]].nx; 
          coords[j*3+1]=node[nface[j]].ny; 
          coords[j*3+2]=node[nface[j]].nz;
          /*
          printf (" nface[%d]:%d x:%lf y:%lf z:%lf\n", n, nface[j],
          coords[j*3], coords[j*3+1], coords[j*3+2] );
          */
        }
        conode[0]=node[set[set1].node[i]].nx;
        conode[1]=node[set[set1].node[i]].ny;
        conode[2]=node[set[set1].node[i]].nz;
        attach_new(coords,conode,&n,ratio,&dist, elemcoords);

        c[0]=0.;
        for(j=0; j<n; j++) c[0]+=c[j+1]=ratio[j];
        c[0]*=-1.;
        k=9;
      }
      else if ( e_enqire[ ctri3[elbuf].elem_nr ].type == 5 )
      {
        /* berechnung der MPC-Koeffizienten fuer pe15 */

        /* knoten der betroffenen elementseite filtern */
        switch(ctri3[elbuf].group)
        {
          case 1:
          nface[0]=e_enqire[ ctri3[elbuf].elem_nr ].nod[0];
          nface[1]=e_enqire[ ctri3[elbuf].elem_nr ].nod[1];
          nface[2]=e_enqire[ ctri3[elbuf].elem_nr ].nod[2];
          nface[3]=e_enqire[ ctri3[elbuf].elem_nr ].nod[6];
          nface[4]=e_enqire[ ctri3[elbuf].elem_nr ].nod[7];
          nface[5]=e_enqire[ ctri3[elbuf].elem_nr ].nod[8];
          n=6;
          break;
          case 2:
          nface[0]=e_enqire[ ctri3[elbuf].elem_nr ].nod[3];
          nface[1]=e_enqire[ ctri3[elbuf].elem_nr ].nod[4];
          nface[2]=e_enqire[ ctri3[elbuf].elem_nr ].nod[5];
          nface[3]=e_enqire[ ctri3[elbuf].elem_nr ].nod[12];
          nface[4]=e_enqire[ ctri3[elbuf].elem_nr ].nod[13];
          nface[5]=e_enqire[ ctri3[elbuf].elem_nr ].nod[14];
          n=6;
          break;
          case 3:
          nface[0]=e_enqire[ ctri3[elbuf].elem_nr ].nod[0];
          nface[1]=e_enqire[ ctri3[elbuf].elem_nr ].nod[1];
          nface[2]=e_enqire[ ctri3[elbuf].elem_nr ].nod[4];
          nface[3]=e_enqire[ ctri3[elbuf].elem_nr ].nod[3];
          nface[4]=e_enqire[ ctri3[elbuf].elem_nr ].nod[6];
          nface[5]=e_enqire[ ctri3[elbuf].elem_nr ].nod[10];
          nface[6]=e_enqire[ ctri3[elbuf].elem_nr ].nod[12];
          nface[7]=e_enqire[ ctri3[elbuf].elem_nr ].nod[9];
          n=8;
          break;
          case 4:
          nface[0]=e_enqire[ ctri3[elbuf].elem_nr ].nod[1];
          nface[1]=e_enqire[ ctri3[elbuf].elem_nr ].nod[2];
          nface[2]=e_enqire[ ctri3[elbuf].elem_nr ].nod[5];
          nface[3]=e_enqire[ ctri3[elbuf].elem_nr ].nod[4];
          nface[4]=e_enqire[ ctri3[elbuf].elem_nr ].nod[7];
          nface[5]=e_enqire[ ctri3[elbuf].elem_nr ].nod[11];
          nface[6]=e_enqire[ ctri3[elbuf].elem_nr ].nod[13];
          nface[7]=e_enqire[ ctri3[elbuf].elem_nr ].nod[10];
          n=8;
          break;
          case 5:
          nface[0]=e_enqire[ ctri3[elbuf].elem_nr ].nod[2];
          nface[1]=e_enqire[ ctri3[elbuf].elem_nr ].nod[0];
          nface[2]=e_enqire[ ctri3[elbuf].elem_nr ].nod[3];
          nface[3]=e_enqire[ ctri3[elbuf].elem_nr ].nod[5];
          nface[4]=e_enqire[ ctri3[elbuf].elem_nr ].nod[8];
          nface[5]=e_enqire[ ctri3[elbuf].elem_nr ].nod[9];
          nface[6]=e_enqire[ ctri3[elbuf].elem_nr ].nod[14];
          nface[7]=e_enqire[ ctri3[elbuf].elem_nr ].nod[11];
          n=8;
          break;
        }

        /*
        attaches node with coordinates in "conode" to the face containing 
        "nterms" nodes with coordinates in field "coords" (nterms < 9).
        */

        for (j=0;j<n;j++)
        {
          coords[j*3]=  node[nface[j]].nx; 
          coords[j*3+1]=node[nface[j]].ny; 
          coords[j*3+2]=node[nface[j]].nz;
          /*
          printf (" nface[%d]:%d x:%lf y:%lf z:%lf\n", n, nface[j],
          coords[j*3], coords[j*3+1], coords[j*3+2] );
          */
        }
        conode[0]=node[set[set1].node[i]].nx;
        conode[1]=node[set[set1].node[i]].ny;
        conode[2]=node[set[set1].node[i]].nz;
        attach_new(coords,conode,&n,ratio,&dist, elemcoords);

        c[0]=0.;
        for(j=0; j<n; j++) c[0]+=c[j+1]=ratio[j];
        c[0]*=-1.;
        k=n+1;
      }
      else if ( e_enqire[ ctri3[elbuf].elem_nr ].type == 6 )
      {
        /* berechnung der MPC-Koeffizienten fuer Tet10 */

        /* knoten der betroffenen elementseite filtern */
        switch(ctri3[elbuf].group)
        {
          case 1:
          nface[0]=e_enqire[ ctri3[elbuf].elem_nr ].nod[0];
          nface[1]=e_enqire[ ctri3[elbuf].elem_nr ].nod[1];
          nface[2]=e_enqire[ ctri3[elbuf].elem_nr ].nod[3];
          nface[3]=e_enqire[ ctri3[elbuf].elem_nr ].nod[4];
          nface[4]=e_enqire[ ctri3[elbuf].elem_nr ].nod[8];
          nface[5]=e_enqire[ ctri3[elbuf].elem_nr ].nod[7];
          break;
          case 2:
          nface[0]=e_enqire[ ctri3[elbuf].elem_nr ].nod[1];
          nface[1]=e_enqire[ ctri3[elbuf].elem_nr ].nod[2];
          nface[2]=e_enqire[ ctri3[elbuf].elem_nr ].nod[3];
          nface[3]=e_enqire[ ctri3[elbuf].elem_nr ].nod[5];
          nface[4]=e_enqire[ ctri3[elbuf].elem_nr ].nod[9];
          nface[5]=e_enqire[ ctri3[elbuf].elem_nr ].nod[8];
          break;
          case 3:
          nface[0]=e_enqire[ ctri3[elbuf].elem_nr ].nod[2];
          nface[1]=e_enqire[ ctri3[elbuf].elem_nr ].nod[0];
          nface[2]=e_enqire[ ctri3[elbuf].elem_nr ].nod[3];
          nface[3]=e_enqire[ ctri3[elbuf].elem_nr ].nod[6];
          nface[4]=e_enqire[ ctri3[elbuf].elem_nr ].nod[7];
          nface[5]=e_enqire[ ctri3[elbuf].elem_nr ].nod[9];
          break;
          case 4:
          nface[0]=e_enqire[ ctri3[elbuf].elem_nr ].nod[0];
          nface[1]=e_enqire[ ctri3[elbuf].elem_nr ].nod[2];
          nface[2]=e_enqire[ ctri3[elbuf].elem_nr ].nod[1];
          nface[3]=e_enqire[ ctri3[elbuf].elem_nr ].nod[6];
          nface[4]=e_enqire[ ctri3[elbuf].elem_nr ].nod[5];
          nface[5]=e_enqire[ ctri3[elbuf].elem_nr ].nod[4];
          break;
        }

        /*
        attaches node with coordinates in "conode" to the face containing 
        "nterms" nodes with coordinates in field "coords" (nterms < 9).
        */
        n=6;

        for (j=0;j<n;j++)
        {
          coords[j*3]=  node[nface[j]].nx; 
          coords[j*3+1]=node[nface[j]].ny; 
          coords[j*3+2]=node[nface[j]].nz;
          /*
          printf (" ind_n[%d]:%d x:%lf y:%lf z:%lf\n", j, nface[j], coords[j*3], coords[j*3+1], coords[j*3+2] );
          */
        }
        conode[0]=node[set[set1].node[i]].nx;
        conode[1]=node[set[set1].node[i]].ny;
        conode[2]=node[set[set1].node[i]].nz;
	// printf("dep:%d\n", set[set1].node[i]);
        attach_new(coords,conode,&n,ratio,&dist, elemcoords);

        /* calculate the normal-vector on the ind-face at the pos of the dep-node (norm_ind) */
	/*
        shape6tri(elemcoords[0],elemcoords[1], coords, norm);
        v_norm( norm, norm_ind );
	//printf("1 norm:%f %f %f\n", norm_ind[0], norm_ind[1], norm_ind[2]);
        */

        c[0]=0.;
        for(j=0; j<n; j++) c[0]+=c[j+1]=ratio[j];
        c[0]*=-1.;
        k=7;
      }
      else if ( e_enqire[ ctri3[elbuf].elem_nr ].type == 8 )
      {
        /* berechnung der MPC-Koeffizienten fuer tr6  */

        nface[0]=e_enqire[ ctri3[elbuf].elem_nr ].nod[0];
        nface[1]=e_enqire[ ctri3[elbuf].elem_nr ].nod[1];
        nface[2]=e_enqire[ ctri3[elbuf].elem_nr ].nod[2];
        nface[3]=e_enqire[ ctri3[elbuf].elem_nr ].nod[3];
        nface[4]=e_enqire[ ctri3[elbuf].elem_nr ].nod[4];
        nface[5]=e_enqire[ ctri3[elbuf].elem_nr ].nod[5];

        /*
        attaches node with coordinates in "conode" to the face containing 
        "nterms" nodes with coordinates in field "coords" (nterms < 9).
        */
        n=6;

        for (j=0;j<n;j++)
        {
          coords[j*3]=  node[nface[j]].nx; 
          coords[j*3+1]=node[nface[j]].ny; 
          coords[j*3+2]=node[nface[j]].nz;
          /*
          printf (" nface[%d]:%d x:%lf y:%lf z:%lf\n", n, nface[j],
          coords[j*3], coords[j*3+1], coords[j*3+2] );
          */
        }
        conode[0]=node[set[set1].node[i]].nx;
        conode[1]=node[set[set1].node[i]].ny;
        conode[2]=node[set[set1].node[i]].nz;
        attach_new(coords,conode,&n,ratio,&dist, elemcoords);

        /* calculate the normal-vector on the ind-face at the pos of the dep-node (norm_ind) */
	/*
        shape6tri(elemcoords[0],elemcoords[1], coords, norm);
        v_norm( norm, norm_ind );
	//printf("1 norm:%f %f %f\n", norm_ind[0], norm_ind[1], norm_ind[2]);
	*/

        c[0]=0.;
        for(j=0; j<n; j++) c[0]+=c[j+1]=ratio[j];
        c[0]*=-1.;
        k=7;
      }
      else if ( e_enqire[ ctri3[elbuf].elem_nr ].type == 10 )
      {
        /* berechnung der MPC-Koeffizienten fuer qu8  */

        nface[0]=e_enqire[ ctri3[elbuf].elem_nr ].nod[0];
        nface[1]=e_enqire[ ctri3[elbuf].elem_nr ].nod[1];
        nface[2]=e_enqire[ ctri3[elbuf].elem_nr ].nod[2];
        nface[3]=e_enqire[ ctri3[elbuf].elem_nr ].nod[3];
        nface[4]=e_enqire[ ctri3[elbuf].elem_nr ].nod[4];
        nface[5]=e_enqire[ ctri3[elbuf].elem_nr ].nod[5];
        nface[6]=e_enqire[ ctri3[elbuf].elem_nr ].nod[6];
        nface[7]=e_enqire[ ctri3[elbuf].elem_nr ].nod[7];

        /*
        attaches node with coordinates in "conode" to the face containing 
        "nterms" nodes with coordinates in field "coords" (nterms < 9).
        */
        n=8;

        for (j=0;j<n;j++)
        {
          coords[j*3]=  node[nface[j]].nx; 
          coords[j*3+1]=node[nface[j]].ny; 
          coords[j*3+2]=node[nface[j]].nz;
          /*
          printf (" nface[%d]:%d x:%lf y:%lf z:%lf\n", n, nface[j],
          coords[j*3], coords[j*3+1], coords[j*3+2] );
          */
        }
        conode[0]=node[set[set1].node[i]].nx;
        conode[1]=node[set[set1].node[i]].ny;
        conode[2]=node[set[set1].node[i]].nz;
        attach_new(coords,conode,&n,ratio,&dist, elemcoords);

        /* calculate the normal-vector on the ind-face at the pos of the dep-node (norm_ind) */
	/*
        shape8q(elemcoords[0],elemcoords[1], coords, norm);
        v_norm( norm, norm_ind );
	//printf("1 norm:%f %f %f\n", norm_ind[0], norm_ind[1], norm_ind[2]);
	*/

        c[0]=0.;
        for(j=0; j<n; j++) c[0]+=c[j+1]=ratio[j];
        c[0]*=-1.;
        k=9;
      }
      else
      {
        /* Alle anderen Elemente: Berechnung der MPC-Koeffizienten fuer Tri3 */

        nface[0]=ctri3[elbuf].nod[0];
        nface[1]=ctri3[elbuf].nod[1];
        nface[2]=ctri3[elbuf].nod[2];
        if(printFlag) printf("   enclosing nodes:%d %d %d\n", nface[0],nface[1],nface[2] );

        /* bestimmung der coeffizienten */
        /* und werte fuer die ev. spaetere verschiebung des dep nodes ins tri3-element ermitteln (conode) */
        if ( calcCoefficientsTri( node, set[set1].node[i], ctri3, elbuf, &dist, c, conode) == -1 )
        {
          printf("WARNING: no coefficients could be calculated, no MPC created for node:%d, see set %s\n", set[set1].node[i], specialset->nompc);
          goto create_no_mpc;
        }  

        /* the c-coefficients (area) have to sum up to 1 (should already but you never know) */
        sum_c= c[1]+c[2]+c[3];
        c[1]/=sum_c;
        c[2]/=sum_c;
        c[3]/=sum_c;
	
        k=4;
      }

      dv=(node[set[set1].node[i]].nx-conode[0])*(node[set[set1].node[i]].nx-conode[0])+(node[set[set1].node[i]].ny-conode[1])*(node[set[set1].node[i]].ny-conode[1])+(node[set[set1].node[i]].nz-conode[2])*(node[set[set1].node[i]].nz-conode[2]);

      /* correct the node possition (adjust to the indep face) if requested */
      if(corrFlag==1)
      {
        if(dv>tol) { ; /* printf("loop:%d tol:%f < dv:%f n:%d\n", loops, tol, dv, set[set1].node[i]); */ }
        else { loops=MAXLOOPS; /* printf("loop:%d tol:%f dv:%f n:%d ok\n", loops, tol, dv, set[set1].node[i]); */ }
        node[set[set1].node[i]].nx=conode[0];
        node[set[set1].node[i]].ny=conode[1];
        node[set[set1].node[i]].nz=conode[2];
      }
      else /* write equations only in a run w/o node adjustments */
      {
        if((ini_corrFlag==1)&&(dv>tol))
        {
          printf(" WARNING: no mpc for n:%d, loop:%d deviation:%f > tol:%f\n", set[set1].node[i], loops, sqrt(dv), sqrt(tol));
          goto create_no_mpc;
        }

        /* Die Koeffizienten c sind nun bekannt */
        /* setze den dep-koeffizienten gleich der summe der indep-koeffizienten */
        c[0]=0.;
        for (n=1;n<k;n++) c[0]-= c[n];
        if(printFlag) printf ("sum_c=%lf (should be 1)\n", -c[0]);
        //if(c[0]!=-1.) printf("n:%d c:%e\n", set[set1].node[i], c[0]+1.);
  
        /* if the dof's work in a cylindrical system then calc new coefficients in the basic system */
        if(cylsysFlag)
        {
          /* calculate the radial vectors nod_edir[0] from axis origin to dep-node, normal on axis to node and the tangential vector nod_edir[1] */
          v_result( pax, &node[set[set1].node[i]].nx, nod_edir[0][0]);
          v_prod( vax, nod_edir[0][0], nod_edir[1][0] );
          v_prod( nod_edir[1][0], vax, nod_edir[0][0] );
          v_norm( nod_edir[0][0], nod_edir[0][0] );
          v_norm( nod_edir[1][0], nod_edir[1][0] );
          v_norm( vax, nod_edir[2][0] );
          for(n=1;n<k;n++)
	  {
            v_result( pax, &node[nface[n-1]].nx, nod_edir[0][n]);
            v_prod( vax, nod_edir[0][n], nod_edir[1][n] );
            v_prod( nod_edir[1][n], vax, nod_edir[0][n] );
            v_norm( nod_edir[0][n], nod_edir[0][n] );
            v_norm( nod_edir[1][n], nod_edir[1][n] );
            v_norm( vax, nod_edir[2][n] );
	  }

	}

        /* force the dep nodes during solving to the indep-faces */
        if(displSlideFlag)
        {
          /* if a user-defined displacement is requested, use the normals */
          if(strlen(corr)>1)
          {
            ve[0]=-norm_dep[set[set1].node[i]].nx;
            ve[1]=-norm_dep[set[set1].node[i]].ny;
            ve[2]=-norm_dep[set[set1].node[i]].nz;
            v_scal( &forcedDisp, ve,ndist);
            ndist_sqr=ndist[0]*ndist[0]+ndist[1]*ndist[1]+ndist[2]*ndist[2];
          }
          else
          {
            if(dist*dist<MIN_NODE_DIST)
            {
              ndist[0]=-norm_dep[set[set1].node[i]].nx;
              ndist[1]=-norm_dep[set[set1].node[i]].ny;
              ndist[2]=-norm_dep[set[set1].node[i]].nz;
              ndist_sqr=0.;
            }
            else
            {
              /* projected node pos - node pos of dependent node */
              ndist[0]=conode[0]-node[set[set1].node[i]].nx;
              ndist[1]=conode[1]-node[set[set1].node[i]].ny;
              ndist[2]=conode[2]-node[set[set1].node[i]].nz;
              ndist_sqr=ndist[0]*ndist[0]+ndist[1]*ndist[1]+ndist[2]*ndist[2];
            }
          }
        }
  
        if(displStickFlag)
        {
          /* if a user-defined displacement is requested, use the normals  */
          if(strlen(corr)>1)
          {
            ve[0]=-norm_dep[set[set1].node[i]].nx;
            ve[1]=-norm_dep[set[set1].node[i]].ny;
            ve[2]=-norm_dep[set[set1].node[i]].nz;
            v_scal( &forcedDisp, ve,ndist);
          }
          else
          {
            ndist[0]=conode[0]-node[set[set1].node[i]].nx;
            ndist[1]=conode[1]-node[set[set1].node[i]].ny;
            ndist[2]=conode[2]-node[set[set1].node[i]].nz;
          }
          /* increase the nr-of-therms by 1 additional node for the displ */
          nface[k-1]=displNode;
          c[k]=1.;
          k++;
        } 
        
  
        /* schreibe die EQUATIONS */
        switch (sflag)
        {
  	/* ABAQUS */
          case 0:
          /* force the dep nodes during solving to the indep-faces (stick) */
          if(displStickFlag)
          {
            fprintf(handle_boundary, "%d, 1,1,%lf\n", displNode, ndist[0]);
            fprintf(handle_boundary, "%d, 2,2,%lf\n", displNode, ndist[1]);
            fprintf(handle_boundary, "%d, 3,3,%lf\n", displNode, ndist[2]);
  
            fprintf(handle, "*NODE, NSET=%s%s\n", set[set1].name, set[set2].name);
            fprintf(handle, "%d,%lf,%lf,%lf\n",displNode, node[set[set1].node[i]].nx, node[set[set1].node[i]].ny, node[set[set1].node[i]].nz );
            displNode++;
          }
    
          /* force the dep nodes during solving to the indep-faces (slide) */
          if(displSlideFlag)
          {
            /* additional node, only dof 1 */
            fprintf(handle_boundary, "%d, 1,1,%lf\n", displNode, ndist_sqr);
  
            fprintf(handle, "*NODE, NSET=%s%s\n", set[set1].name, set[set2].name);
            fprintf(handle, "%d,%lf,%lf,%lf\n",displNode, node[set[set1].node[i]].nx, node[set[set1].node[i]].ny, node[set[set1].node[i]].nz );
  
            terms=0;
            for (e=1;e<4;e++)
            {
              if((c[0]*ndist[e-1])*(c[0]*ndist[e-1])>=1e-11) terms++;
              for (n=1;n<k;n++) if((c[n]*ndist[e-1])*(c[n]*ndist[e-1])>=1e-11) terms++;
            }
            fprintf(handle, "*EQUATION\n");
            fprintf(handle, "%d\n", terms+1);
            for (e=1;e<4;e++)
            {
              if((c[0]*ndist[e-1])*(c[0]*ndist[e-1])>=1e-11)
                fprintf(handle, "%d,%d,%.12lf\n", set[set1].node[i], e, c[0]*ndist[e-1]);
              for (n=1;n<k;n++)
                if((c[n]*ndist[e-1])*(c[n]*ndist[e-1])>=1e-11)
                  fprintf(handle, "%d,%d,%.12lf\n", nface[n-1], e, c[n]*ndist[e-1] );
            }
            /* additional node, only dof 1 */
            fprintf(handle, "%d,%d,%.12lf\n", displNode, 1, 1.);
            displNode++;
          }
          else if(cylsysFlag)
          {
            for (e=0;e<3;e++)
            {
              if(DOF[e+1])
	      {
                xx=nod_edir[e][0][0]*nod_edir[e][0][0];
                yy=nod_edir[e][0][1]*nod_edir[e][0][1];
                zz=nod_edir[e][0][2]*nod_edir[e][0][2];
                if((xx>MIN_VECTOR)&&(yy>MIN_VECTOR)
                &&(zz>MIN_VECTOR))
                {
                  /* linking the dofx to the tangential direction */
                  fprintf(handle, "*EQUATION\n");
                  fprintf(handle, "%d\n", 3+(k-1)*3);
                  fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf, %d,%d,%.12lf, \n"
                  , set[set1].node[i], DOFX, nod_edir[e][0][0]
                  , set[set1].node[i], DOFY, nod_edir[e][0][1]
        	  , set[set1].node[i], DOFZ, nod_edir[e][0][2] );
                }
                else if((xx<=MIN_VECTOR)
                &&(yy>MIN_VECTOR)&&(zz>MIN_VECTOR))
                {
                   /* linking the dofy to the tangential direction */
                  fprintf(handle, "*EQUATION\n");
                  fprintf(handle, "%d\n", 2+(k-1)*3);
                  fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf, \n"
                  , set[set1].node[i], DOFY, nod_edir[e][0][1]
        	  , set[set1].node[i], DOFZ, nod_edir[e][0][2] );
                }
                else if((yy<=MIN_VECTOR)
                &&(xx>MIN_VECTOR)&&(zz>MIN_VECTOR))
                {
                  /* linking the dofx to the tangential direction */
                  fprintf(handle, "*EQUATION\n");
                  fprintf(handle, "%d\n", 2+(k-1)*3);
                  fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf,\n"
                  , set[set1].node[i], DOFX, nod_edir[e][0][0]
                  , set[set1].node[i], DOFZ, nod_edir[e][0][2]);
                }
                else if((zz<=MIN_VECTOR)
                &&(xx>MIN_VECTOR)&&(yy>MIN_VECTOR))
                {
                  /* linking the dofx to the tangential direction */
                  fprintf(handle, "*EQUATION\n");
                  fprintf(handle, "%d\n", 2+(k-1)*3);
                  fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf,\n"
                  , set[set1].node[i], DOFX, nod_edir[e][0][0]
                  , set[set1].node[i], DOFY, nod_edir[e][0][1]);
                }
                else if((xx>MIN_VECTOR)
                &&(yy<=MIN_VECTOR)&&(zz<=MIN_VECTOR))
                {
                  fprintf(handle, "*EQUATION\n");
                  fprintf(handle, "%d\n", 1+(k-1)*3);
                  fprintf(handle, "%d,%d,%.12lf,\n"
                  , set[set1].node[i], DOFX, nod_edir[e][0][0] );
                }
                else if((yy>MIN_VECTOR)
                &&(xx<=MIN_VECTOR)&&(zz<=MIN_VECTOR))
                {
                  fprintf(handle, "*EQUATION\n");
                  fprintf(handle, "%d\n", 1+(k-1)*3);
                  fprintf(handle, "%d,%d,%.12lf,\n"
                  , set[set1].node[i], DOFY, nod_edir[e][0][1] );
                }
                else if((zz>MIN_VECTOR)
                &&(xx<=MIN_VECTOR)&&(yy<=MIN_VECTOR))
                {
                  fprintf(handle, "*EQUATION\n");
                  fprintf(handle, "%d\n", 1+(k-1)*3);
                  fprintf(handle, "%d,%d,%.12lf,\n"
      	          , set[set1].node[i], DOFZ, nod_edir[e][0][2] );
                }

                /* add the DOF terms of the indep nodes */
                for (n=1;n<k;n++)
	        {
                  fprintf(handle, "%d,%d,%.12lf, %d,%d,%.12lf, %d,%d,%.12lf,\n"
		  , nface[n-1], DOFX, -(nod_edir[e][n][0]*c[n])
  	          , nface[n-1], DOFY, -(nod_edir[e][n][1]*c[n])
  	          , nface[n-1], DOFZ, -(nod_edir[e][n][2]*c[n]));
                }

              }
            }
          }
          else
          {  
            if(forcedDispFlag)
            {      
              fprintf(handle, "*NODE, NSET=%s%s\n", set[set1].name, set[set2].name);
              fprintf(handle, "%d,%lf,%lf,%lf\n",displNode, node[set[set1].node[i]].nx, node[set[set1].node[i]].ny, node[set[set1].node[i]].nz );
  	      
              for (e=0;e<7;e++)
              {
                if(DOF[e])
                {
                  if(e)
                  {
                    if(forcedDisp!=0) fprintf(handle_boundary, "%d, %d,%d,%lf\n", displNode,e,e, forcedDisp);
                    else fprintf(handle_boundary, "%d, %d,%d\n", displNode,e,e);
                    fprintf(handle, "*EQUATION\n");
                    fprintf(handle, "%d\n", k+1);
                    fprintf(handle, "%d,%d,%.12lf\n", set[set1].node[i], e, c[0]);
                    for (n=1;n<k;n++)
                      fprintf(handle, "%d,%d,%.12lf\n", nface[n-1], e, c[n] );
                    fprintf(handle, "%d,%d,%.12lf \n", displNode, e, 1. );
                  }
                }
              }
              displNode++;
  	    }
            else
            {
              for (e=0;e<7;e++)
              {
                if(DOF[e])
                {
                  fprintf(handle, "*EQUATION\n");
                  fprintf(handle, "%d\n", k);
                  if(e)
                  {
                    fprintf(handle, "%d,%d,%.12lf\n", set[set1].node[i], e, c[0]);
                    for (n=1;n<k;n++)
                      fprintf(handle, "%d,%d,%.12lf\n", nface[n-1], e, c[n] );
                  }
                  else
                  {
                    if(DOF[e]==1)
		    {
                      fprintf(handle, "%d,%d,%.12lf\n", set[set1].node[i], DOFT, c[0]);
                      for (n=1;n<k;n++)
                        fprintf(handle, "%d,%d,%.12lf\n", nface[n-1], DOFT, c[n] );
		    }
                    if(DOF[e]==2)
		    {
                      fprintf(handle, "%d,%d,%.12lf\n", set[set1].node[i], DOFP, c[0]);
                      for (n=1;n<k;n++)
                        fprintf(handle, "%d,%d,%.12lf\n", nface[n-1], DOFP, c[n] );
		    }
                  }
  	        }
              }
            }
          }
          break;
  
          /* ANSYS */
          case 1:
          for (e=0;e<6;e++)
          {
            if(DOF[e+1])
            {
              neqn++;
              fprintf(handle, "CE,%d,0,%d,%s,%.12lf\n", neqn, set[set1].node[i], ansy_dofs[e], c[0]);
              for (n=1;n<k;n++)
                fprintf(handle, "CE,%d,0,%d,%s,%.12lf\n", neqn, nface[n-1], ansy_dofs[e], c[n]);
            }
          }
          break;
  
          /* NASTRAN */
          case 2:
          /* schreibe die EQUATIOS im NASTRANformat */
	  if(nasMpc)
	  {
            if(cylsysFlag)
            {
              for (e=0;e<3;e++)
              {
                if(DOF[e+1])
  	        {
                  xx=nod_edir[e][0][0]*nod_edir[e][0][0];
                  yy=nod_edir[e][0][1]*nod_edir[e][0][1];
                  zz=nod_edir[e][0][2]*nod_edir[e][0][2];
                  if((xx>MIN_VECTOR)&&(yy>MIN_VECTOR)
                  &&(zz>MIN_VECTOR))
                  {
                    /* linking the dofx to the tangential direction */
		    /*
                    fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n", neqn+1
                    , set[set1].node[i], DOFX, nod_edir[e][0][0]
                    , set[set1].node[i], DOFY, nod_edir[e][0][1] );
                    fprintf(handle, ", ,%8d,%8d,%.12lf\n"
          	    , set[set1].node[i], DOFZ, nod_edir[e][0][2] );
		    */
                    fprintf(handle, "MPC*    %-16d%-16d%-16d%-.13lf\n", neqn+1
                    , set[set1].node[i], DOFX, nod_edir[e][0][0] );
                    fprintf(handle, "*       %-16d%-16d%-.13lf\n"
                    , set[set1].node[i], DOFY, nod_edir[e][0][1] );
                    fprintf(handle, "*                       %-16d%-16d%-.13lf\n"
          	    , set[set1].node[i], DOFZ, nod_edir[e][0][2] );
                    lines=1; 
                  }
                  else if((xx<=MIN_VECTOR)
                  &&(yy>MIN_VECTOR)&&(zz>MIN_VECTOR))
                  {
                     /* linking the dofy to the tangential direction */
		    /*
                    fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n", neqn+1
                    , set[set1].node[i], DOFY, nod_edir[e][0][1]
          	    , set[set1].node[i], DOFZ, nod_edir[e][0][2] );
		    */
                    fprintf(handle, "MPC*    %-16d%-16d%-16d%-.13lf\n", neqn+1
                    , set[set1].node[i], DOFY, nod_edir[e][0][1] );
                    fprintf(handle, "*       %-16d%-16d%-.13lf\n"
          	    , set[set1].node[i], DOFZ, nod_edir[e][0][2] );
                    lines=0; 
                  }
                  else if((yy<=MIN_VECTOR)
                  &&(xx>MIN_VECTOR)&&(zz>MIN_VECTOR))
                  {
                    /* linking the dofx to the tangential direction */
		    /*
                    fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n", neqn+1
                    , set[set1].node[i], DOFX, nod_edir[e][0][0]
                    , set[set1].node[i], DOFZ, nod_edir[e][0][2]);
		    */
                    fprintf(handle, "MPC*    %-16d%-16d%-16d%-.13lf\n", neqn+1
                    , set[set1].node[i], DOFX, nod_edir[e][0][0]);
                    fprintf(handle, "*       %-16d%-16d%-.13lf\n"
                    , set[set1].node[i], DOFZ, nod_edir[e][0][2]);
                    lines=0; 
                  }
                  else if((zz<=MIN_VECTOR)
                  &&(xx>MIN_VECTOR)&&(yy>MIN_VECTOR))
                  {
                    /* linking the dofx to the tangential direction */
		    /*
                    fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n", neqn+1
                    , set[set1].node[i], DOFX, nod_edir[e][0][0]
                    , set[set1].node[i], DOFY, nod_edir[e][0][1]);
		    */
                    fprintf(handle, "MPC*    %-16d%-16d%-16d%-.13lf\n", neqn+1
                    , set[set1].node[i], DOFX, nod_edir[e][0][0]);
                    fprintf(handle, "*       %-16d%-16d%-.13lf\n"
                    , set[set1].node[i], DOFY, nod_edir[e][0][1]);
                    lines=0; 
                  }
                  else if((xx>MIN_VECTOR)
                  &&(yy<=MIN_VECTOR)&&(zz<=MIN_VECTOR))
                  {
 		    /*
                   fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf\n", neqn+1
                    , set[set1].node[i], DOFX, nod_edir[e][0][0] );
		    */
                    fprintf(handle, "MPC*    %-16d%-16d%-16d%-.13lf\n", neqn+1
                    , set[set1].node[i], DOFX, nod_edir[e][0][0] );
                    lines=1; 
                  }
                  else if((yy>MIN_VECTOR)
                  &&(xx<=MIN_VECTOR)&&(zz<=MIN_VECTOR))
                  {
		    /*
                    fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf\n", neqn+1
                    , set[set1].node[i], DOFY, nod_edir[e][0][1] );
		    */
                    fprintf(handle, "MPC*    %-16d%-16d%-16d%-.13lf\n", neqn+1
                    , set[set1].node[i], DOFY, nod_edir[e][0][1] );
                    lines=1; 
                  }
                  else if((zz>MIN_VECTOR)
                  &&(xx<=MIN_VECTOR)&&(yy<=MIN_VECTOR))
                  {
		    /*
                    fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf\n", neqn+1
        	    , set[set1].node[i], DOFZ, nod_edir[e][0][2] );
		    */
                    fprintf(handle, "MPC*    %-16d%-16d%-16d%-.13lf\n", neqn+1
        	    , set[set1].node[i], DOFZ, nod_edir[e][0][2] );
                    lines=1; 
                  }
  
                  /* add the DOF terms of the indep nodes */
                  for (n=1;n<k;n++)
  	          {
		    /*
                    fprintf(handle, ", ,%8d,%8d,%.12lf,%8d,%8d,%.12lf\n"
  		    , nface[n-1], DOFX, -(nod_edir[e][n][0]*c[n])
    	            , nface[n-1], DOFY, -(nod_edir[e][n][1]*c[n]));
                    fprintf(handle, ", ,%8d,%8d,%.12lf\n"
		    , nface[n-1], DOFZ, -(nod_edir[e][n][2]*c[n]));
		    */
                    if(!lines)
		    {
                      fprintf(handle, "*                       %-16d%-16d%-.13lf\n"
  		      , nface[n-1], DOFX, -(nod_edir[e][n][0]*c[n]));
                      fprintf(handle, "*       %-16d%-16d%-.13lf\n"
    	              , nface[n-1], DOFY, -(nod_edir[e][n][1]*c[n]));
                      fprintf(handle, "*                       %-16d%-16d%-.13lf\n"
		      , nface[n-1], DOFZ, -(nod_edir[e][n][2]*c[n]));
		    }
                    else
		    { 
                      fprintf(handle, "*       %-16d%-16d%-.13lf\n"
  		      , nface[n-1], DOFX, -(nod_edir[e][n][0]*c[n]));
                      fprintf(handle, "*                       %-16d%-16d%-.13lf\n"
    	              , nface[n-1], DOFY, -(nod_edir[e][n][1]*c[n]));
                      fprintf(handle, "*       %-16d%-16d%-.13lf\n"
		      , nface[n-1], DOFZ, -(nod_edir[e][n][2]*c[n]));
		    }
                    lines=!lines;
                  }
                  if(lines) fprintf(handle, "*       \n");
                }
              }
            }
            else
            {  
              for (e=1;e<7;e++)
              {
                if(DOF[e])
                {
                  //fprintf(handle, "MPC,%8d,%8d,%8d,%.12lf\n", neqn+1, set[set1].node[i], e, c[0]);
                  //for (n=1;n<k;n++)
                  //  fprintf(handle, ", ,%8d,%8d,%.12lf\n", nface[n-1], e, c[n] );
                  fprintf(handle, "MPC*    %16d%16d%16d%.13lf\n", neqn+1, set[set1].node[i], e, c[0]);
                  l=0;
                  for (n=1;n<k;n++)
		  {
                    if(l) fprintf(handle, "*                       %16d%16d%.13lf\n", nface[n-1], e, c[n] );
                    else fprintf(handle, "*       %16d%16d%.13lf\n", nface[n-1], e, c[n] );
                    l=!l;
		  }
                  fprintf(handle, "*       \n");
                }
              }
	    }
	  }
          else
	  {
            /* schreibe die EQUATIOS im Nastranformat als RBE3 */
            /* RBE3     12051           111693  123     1.      123     111584  111659 */
            /*         111685  111694 */
            for(j=0; j<8; j++) dofbuf[j]=0;
            j=0; for(e=1; e<7; e++) { if(DOF[e]) { sprintf(&dofbuf[j],"%1d",e); j++; } }
 
            // RBEs with "," seem not to work with ALPHA?? 
            //fprintf(handle, "RBE3    ,%8d,        ,%8d,%8s,      1.,%8s,%8d,%8d\n", ++neqn, set[set1].node[i], dofbuf, dofbuf, nface[0], nface[1]);
            //fprintf(handle, "RBE3    %8d        %8d%8s      1.%8s%8d%8d\n", ++neqn, set[set1].node[i], dofbuf, dofbuf, nface[0], nface[1]);
            // for volume elements the indep dofs must not contain 456
            fprintf(handle, "RBE3    %8d        %8d%8s      1.%8s%8d%8d\n", ++neqn, set[set1].node[i], dofbuf, "123",nface[0], nface[1]);
            /* second line */
            //for (n=2;n<k-1;n++) fprintf(handle, ",%8d",nface[n]);
            fprintf(handle, "        "); for (n=2;n<k-1;n++) fprintf(handle, "%8d",nface[n]);
            fprintf(handle,"\n");
            if(nasRbeHec>0.) fprintf(handle,"*       ALPHA           %.9e \n*       \n", nasRbeHec);
	  }
          break;
  
          /* INTERPOLATION */
          /* a loadcase is specified for interpolation */
          case 3:
          if(allds)
          {
            /* all datasets should be interpolated */
            for(ds=0; ds<anz->l; ds++)
            {
              for(e=0; e<lcase[ds].ncomps; e++)
              {
                c[0]=0.;
                for (n=1;n<k;n++) c[0]+=lcase[ds].dat[e][nface[n-1]]*c[n];
                lcase[ds].dat[e][set[set1].node[i]] =   c[0];   
	      }
	    }
	  }
          else
          {
            for(e=0; e<lcase[ds].ncomps; e++)
            {
              c[0]=0.;
              for (n=1;n<k;n++) c[0]+=lcase[ds].dat[e][nface[n-1]]*c[n];
              lcase[ds].dat[e][set[set1].node[i]] =   c[0];   
	    }     
	  }
          break;
        }
      }
    }
    goto create_mpc;
    create_no_mpc:;
        sprintf( buffer, "%d", set[set1].node[i]); 
        pre_seta(specialset->nompc, "n", buffer);
    goto cont;
    create_mpc:;  
      if(corrFlag==0)
      {
        sprintf( buffer, "%d", set[set1].node[i]); 
        pre_seta(specialset->mpc, "n", buffer);
        if(displStickFlag) k--;
        if(indeql)
	{
          sprintf( buffer, "%d", indeql); 
          pre_seta(specialset->impc, "n", buffer);
	}
        else for (n=1;n<k;n++)
	{
          sprintf( buffer, "%d", nface[n-1]); 
          pre_seta(specialset->impc, "n", buffer);
	}
      }
    cont:;
  }

  /* correct also all midside nodes of the dependent side elements */
  if(corrFlag==1)
  {
    loops++;
    if(loops>MAXLOOPS) corrFlag=0;
    else{
     if(set[set1].anz_n>0)
     {
      /* add all dep nodes to set tmp */
      delSet(specialset->tmp);
      if( (settmp=pre_seta( specialset->tmp, "i", 0 )) <0 ) return;
      for (k=0; k<set[set1].anz_n; k++)  seta( settmp, "n", set[set1].node[k]  );
      completeSet( specialset->tmp, "up");
  
      fixMidsideNodes(specialset->tmp, "");
  
      /* clear special set  */
      delSet(specialset->ori);
     } 
    }
    goto secondRun;
  }

 end:;
  if (compare(format,"nas", 3) == 3)
  {
#if STORE_CONNECTIVITY
    /* generate a list of connection pid and first neqn (only use with care, dep and ind sets are changed) */
    handle_pid = fopen ("connectivity.txt", "a");
    if (handle==NULL) { printf ("\nThe output file \"connectivity.txt\" could not be opened.\n\n"); return; }
    else  printf (" connectivity.txt opened\n" );
   
    completeSet(set[set1].name,"up");
    completeSet(set[set2].name,"up");
    printf("Set %s %s PID %d %d connected by connection %d - %d\n", set[set1].name,set[set2].name, e_enqire[set[set1].elem[0]].mat, e_enqire[set[set2].elem[0]].mat, neqn_ini+1, neqn);
    fprintf(handle_pid,"Set %s %s PID %d %d connected by connection %d - %d ", set[set1].name,set[set2].name, e_enqire[set[set1].elem[0]].mat, e_enqire[set[set2].elem[0]].mat, neqn_ini+1, neqn);
    if(nasRbeHec>0.) fprintf(handle_pid, "HC %5.2fE-5 ", nasRbeHec*1e5 );
    fprintf (handle_pid,"DOFs: "); for(i=0;i<7;i++) { if(DOF[i]) fprintf(handle_pid,"%d", i); } fprintf(handle_pid,"\n");
    fclose(handle_pid);
    printf("\nWARNING: sets %s %s were upwards completed, do not save.\n\n", set[set1].name,set[set2].name);
#endif
  }


  if(rsort) free(rsort);
  if(orig_x) free(orig_x);
  if(orig_y) free(orig_y);
  if(orig_z) free(orig_z);
  if(sort_x) free(sort_x);
  if(sort_y) free(sort_y);
  if(sort_z) free(sort_z);
  if(sort_nx) free(sort_nx);
  if(sort_ny) free(sort_ny);
  if(sort_nz) free(sort_nz);

  i=getSetNr(specialset->nompc);
  if (i>-1) if (set[i].anz_n>0) printf(" WARNING:%d dependent nodes could not be connected, check set:%s\n", set[i].anz_n, set[i].name);

  if(ntri_nodes) free(ntri_nodes);
  /* if(tri3_index) free(tri3_index); do not free, otherwise subfields must be freed as well */
  if(ctri3) free(ctri3);
  scalNodes ( anz->n, nptr, scale );

  if(sflag==3)
  {
    if( node!=nptr ) { free(node); node=NULL; }
  } 
  else
  {
    fclose(handle);
    if((displSlideFlag)||(displStickFlag)||(forcedDispFlag))  fclose(handle_boundary);
  }
}



void gap(char *record)
{
  int  i,j, length;
  int setNr1, setNr2, enr, nn[2]={0,0}, nmin, *n_used, jmin;
  double offset, vdir[3], tol=0.17, edir[3], en1n2[3], n1n2[3], varea[3], area, min_area;
  char format[MAX_LINE_LENGTH], type[MAX_LINE_LENGTH];
  char  set1[MAX_LINE_LENGTH], set2[MAX_LINE_LENGTH], prognam[MAX_LINE_LENGTH];
  FILE *handle;

  length = sscanf(record, "%s%s%s%s%lf%lf%lf%lf", set1, set2, format, type, &vdir[0], &vdir[1], &vdir[2], &tol);
  if (compare( format, "abq", 3)!= 3)
  {
    errMsg(" ERROR: format:%s not yet supported\n", format );
  }
  setNr1=getSetNr(set1);
  setNr2=getSetNr(set2);
  
  strcpy ( prognam, set1);
  length= strlen ( set1 );
  strcpy (&prognam[length], ".gap");
  handle = fopen (prognam, "w");
  if ( handle== NULL )
  {
    printf ("\nThe input file %s could not be opened.\n\n", prognam); 
    return;
  }

  v_norm(vdir,edir); 
  if((n_used = (int *)malloc((set[setNr2].anz_n+2) * sizeof(int))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );
  for(j=0; j<set[setNr2].anz_n; j++) n_used[j]=0;
  

  fprintf(handle, "** gap based on %s\n", set1);
  for(i=0; i<set[setNr1].anz_n; i++)
  {
    nn[0]=set[setNr1].node[i];

    /* search the correct second node for each gap-element */
    /* go over all nodes of set2 and calculate the cross-prodiuct between the normalized vectors edir and en1n2 */
    /* the n2 which gives min-cross-product is the closest */
    min_area=MAX_INTEGER;
    jmin=nmin=-1;
    
    for(j=0; j<set[setNr2].anz_n; j++) if(n_used[j]==0)
    {
      nn[1]=set[setNr2].node[j];
      v_result( &node[nn[0]].nx, &node[nn[1]].nx, n1n2);
      v_norm(n1n2, en1n2);
      v_prod(edir, en1n2, varea);
      area=v_betrag(varea);
      if(area<min_area) { min_area=area; nmin=nn[1]; jmin=j; }
    }
    if((jmin>-1) && (nn[1]!=nn[0]) && (min_area<tol) )
    {
      n_used[jmin]=1;
      nn[1]=nmin;
      enr=anz->emax+1;
      fprintf(handle, "*ELEMENT, TYPE=GAPUNI, ELSET=E%s%d\n", set1, i);   
      fprintf(handle, "%d, %d, %d\n",enr+i,nn[0],nn[1]);
/*
      elem_define( enr, 11, nn, 1, 0 );
      fprintf(handle, "%d, %d, %d\n",enr,nn[0],nn[1]);
*/

      /* offset is the initial distance in vdir direction */
      v_result( &node[nn[0]].nx, &node[nn[1]].nx, n1n2);
      offset=v_sprod(n1n2,edir);
      offset*=scale->w;
      fprintf(handle, "*GAP,ELSET=E%s%d\n", set1, i);
      fprintf(handle, "%lf, %lf, %lf, %lf\n",offset, vdir[0], vdir[1], vdir[2]);
      printf(" nn0:%d  nn1:%d minarea:%lf offset:%lf\n", nn[0], nn[1], min_area, offset);
    }
    else
    {
      printf(" nn0:%d  nn1:%d minarea:%lf not connected\n", nn[0], nn[1], min_area);
    }
  }

  fclose(handle);
  free(n_used);
}

