/* ---------------------------------------------------------------------------  */
/* readIsaac                                       22.09.2005 Wittig            */
/* Text from io.F:SUBROUTINE PLOT3D                                             */
/* C Routine to write PLOT3D output. The mean flow variables are output in      */
/* C the PLOT3D solution (Q) file in 'conserved' form (neglecting TKE in the    */
/* C equation of state) and the turbulence variables and properties (if NQ > 5) */
/* C are output in the function file. The turbulence quantities are output as   */
/* C they are calculated and are not converted to 'conserved' form.             */
/* ---------------------------------------------------------------------------  */


#include <extUtil.h>
#include <sys/utsname.h>

#define MAXIDENTIFIER  12 
#define MAXINTEGER 2147483647
#define MAXFLOAT   1.e32
#define MAX_BLOCKS  100000



#define kappa    1.4 /* in ISAAC defined */
#define RHOINF 1.    /* in ISAAC defined */

#define _R_GAS  287.1 /* real Gas-Constant */
#define _PINF   1.e5; /* real ambient static pressure */
#define _TINF   288.  /* real ambient static temperature */

int calcAddEntitiesIsaac( Summen *anz, Nodes *node, Elements *elem, Datasets *lcase, int elem_type, double PINF, double TINF, double R_GAS )
{
  int lc, n, i, j, ncomps;

  double p0, t0, rho, ma, mau, mav, maw, rhoE, pcalc, ps, pt, ts, tt, a, vu, vv, vw, v, cpg;
  double rho0, rhocalc;

  p0=PINF;
  t0=TINF;
  rho0=p0/R_GAS/t0;

  cpg=R_GAS/(1.-(1./kappa));

  for (lc=0; lc<anz->l; lc++)
  {
    n=0; while(n<lcase[lc].ncomps)
    {
      /* search U entities */
      if(compare(lcase[lc].compName[n], "rho*u ", 6)==6)
      {
        ncomps=lcase[lc].ncomps;
        lcase[lc].ncomps+=9;

        if ( (lcase[lc].nmax = (int *)realloc(lcase[lc].nmax , lcase[lc].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: realloc failure\n\n" );
        if ( (lcase[lc].nmin = (int *)realloc(lcase[lc].nmin , lcase[lc].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: realloc failure\n\n" );
        if ( (lcase[lc].max = (float *)realloc(lcase[lc].max, lcase[lc].ncomps * sizeof(float))) == NULL )
          printf("\n\n ERROR: realloc failure\n\n" );
        if ( (lcase[lc].min = (float *)realloc( lcase[lc].min,lcase[lc].ncomps * sizeof(float))) == NULL )
          printf("\n\n ERROR: realloc failure\n\n" );
        if ( (lcase[lc].dat = (float **)realloc(lcase[lc].dat, lcase[lc].ncomps * sizeof(float *))) == NULL )
          printf("\n\n ERROR: realloc failure\n\n" );
        if ( (lcase[lc].icname = (char **)realloc(lcase[lc].icname, lcase[lc].ncomps * sizeof(char *))) == NULL )
          printf("\n\n ERROR: realloc failure\n\n" );
        if ( (lcase[lc].compName = (char **)realloc(lcase[lc].compName, lcase[lc].ncomps * sizeof(char *))) == NULL )
          printf("\n\n ERROR: realloc failure\n\n" );

        for(i=ncomps; i<lcase[lc].ncomps; i++)
	{
          if ( (lcase[lc].compName[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
            printf("\n\n ERROR: malloc failed\n\n" );
          if ( (lcase[lc].dat[i]      = (float *)malloc( (anz->nmax+1) * sizeof(float))) == NULL )
            printf("\n\n ERROR: malloc failed\n\n" );	               
          if ( (lcase[lc].icname[i]   = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
            printf("\n\n ERROR: malloc failed\n\n" );
          lcase[lc].max[i]=-MAXFLOAT;
          lcase[lc].min[i]=MAXFLOAT;
	}
        
        if ( (lcase[lc].menu = (int *)realloc(lcase[lc].menu, lcase[lc].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: realloc failure\n\n" );
        if ( (lcase[lc].ictype = (int *)realloc(lcase[lc].ictype, lcase[lc].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: realloc failure\n\n" );
        if ( (lcase[lc].icind1 = (int *)realloc(lcase[lc].icind1, lcase[lc].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: realloc failure\n\n" );
        if ( (lcase[lc].icind2 = (int *)realloc( lcase[lc].icind2,lcase[lc].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: realloc failure\n\n" );
        if ( (lcase[lc].iexist = (int *)realloc(lcase[lc].iexist, lcase[lc].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: realloc failure\n\n" );
        
        for(i=ncomps; i<lcase[lc].ncomps; i++)
	{
          lcase[lc].menu[i] = 1;
          lcase[lc].ictype[i] = 1;
          lcase[lc].icind1[i] = 0;
          lcase[lc].icind2[i] = 0;
          lcase[lc].iexist[i] = 0;
	}

        /* calculate the velocity */
        sprintf(lcase[lc].compName[ncomps+0],"ma "); 
        sprintf(lcase[lc].compName[ncomps+1],"v  "); 
        sprintf(lcase[lc].compName[ncomps+2],"vu "); 
        sprintf(lcase[lc].compName[ncomps+3],"vv "); 
        sprintf(lcase[lc].compName[ncomps+4],"vw "); 
        sprintf(lcase[lc].compName[ncomps+5],"ps "); 
        sprintf(lcase[lc].compName[ncomps+6],"ts ");
        sprintf(lcase[lc].compName[ncomps+7],"pt "); 
        sprintf(lcase[lc].compName[ncomps+8],"tt ");


	/*
C in ISAAC io.F:
C 
C 3.  Convert mean flow from 'conserved' to primitive variables
C 
      DO 330 I = 1, IDIM+1 
         DO 320 K = 1, KDIM+1 
            DO 310 J = 1, JDIM+1                     !RHO
                U      =Q(J,K,I,2) = Q(J,K,I,2) / Q(J,K,I,1)
                V      =Q(J,K,I,3) = Q(J,K,I,3) / Q(J,K,I,1)
                W      = Q(J,K,I,4) = Q(J,K,I,4) / Q(J,K,I,1)
C 3.  Convert from (rho*Etotal) to pressure 
               P= Q(J,K,I,5) = GAMM1*( Q(J,K,I,5) - 0.5E0*Q(J,K,I,1)*
     1                               ( Q(J,K,I,2)*Q(J,K,I,2)
     2                               + Q(J,K,I,3)*Q(J,K,I,3)
     3                               + Q(J,K,I,4)*Q(J,K,I,4) ) )
  310       CONTINUE 
  320    CONTINUE 
  330 CONTINUE 
C
C     Finished converting conserved variables to primitive variables
C
	*/

        for(i=0; i<anz->n; i++)
	{
          rhocalc=lcase[lc].dat[n-1][node[i].nr];
          vu=lcase[lc].dat[n][node[i].nr]  /rhocalc;
          vv=lcase[lc].dat[n+1][node[i].nr]/rhocalc;

          if(elem_type==1) /* 3D */
	  {
            vw=lcase[lc].dat[n+2][node[i].nr]/rhocalc;
            rhoE=lcase[lc].dat[n+3][node[i].nr];
	  }
          else
	  {
            vw=0.;
            rhoE=lcase[lc].dat[n+2][node[i].nr];
	  }
          v=sqrt(vu*vu+vv*vv+vw*vw);
          pcalc= (kappa-1.)*( rhoE -0.5* rhocalc* v*v );

          /* in ISAAC : (1./kappa)==PREF;  1.=RHOINF; ps=pcalc; */
          ts=pcalc/R_GAS/rhocalc;
          a=sqrt(kappa*R_GAS*ts);
          mau=vu/a;
          mav=vv/a;
          maw=vw/a;
          ma=v/a;

          /* transform into real world */
          ps=pcalc*p0/(1./kappa);
          rho=rhocalc*rho0/RHOINF;
          ts=ps/R_GAS/rho;
          a=sqrt(kappa*R_GAS*ts);
          vu=mau*a;
          vv=mav*a;
          vw=maw*a;
          v =ma *a;
          tt=v*v*.5/cpg + ts;
	  pt = ps/pow( (ts/tt), (kappa/(kappa-1)) );

          lcase[lc].dat[ncomps+0][node[i].nr]=ma;
          lcase[lc].dat[ncomps+1][node[i].nr]=v;
          lcase[lc].dat[ncomps+2][node[i].nr]=vu;
          lcase[lc].dat[ncomps+3][node[i].nr]=vv;
          lcase[lc].dat[ncomps+4][node[i].nr]=vw;
          lcase[lc].dat[ncomps+5][node[i].nr]=ps;
          lcase[lc].dat[ncomps+6][node[i].nr]=ts;
          lcase[lc].dat[ncomps+7][node[i].nr]=pt;
          lcase[lc].dat[ncomps+8][node[i].nr]=tt;            

          for(j=ncomps; j<lcase[lc].ncomps; j++)
	  {
            if(lcase[lc].dat[j][node[i].nr] > lcase[lc].max[j])
            {
              lcase[lc].max[j]=lcase[lc].dat[j][node[i].nr];
              lcase[lc].nmax[j]=node[i].nr;
            }
            if(lcase[lc].dat[j][node[i].nr] < lcase[lc].min[j])
            {
              lcase[lc].min[j]=lcase[lc].dat[j][node[i].nr];
              lcase[lc].nmin[j]=node[i].nr;
            }
	  }
	}
      }
      n++;
    }

  }
  return (1);
}


int readIsaac(char *datin, Summen *anz, Nodes **nptr, Elements **eptr, Datasets **lptr, NodeBlocks **bptr, int elem_type, double PINF, double TINF, double R_GAS )
{
  FILE *handle1=NULL, *handle2=NULL, *handle3=NULL, *handle4=NULL;
  int i, j, k, n, ld100, lfile;
  char meshfile[MAX_LINE_LENGTH];
  char buffer[MAX_LINE_LENGTH];
  int  anz_n;
  int  e_nmax=1, e_nmin=1;
  int  length;
  double fval[1];

  
  char identifier[MAXIDENTIFIER][MAX_LINE_LENGTH];
  int mnx[MAX_BLOCKS],mny[MAX_BLOCKS],mnz[MAX_BLOCKS];
  int u,v,w,umax,vmax,wmax,nsz,nblock,nident=0;
  int *n_uv=NULL, *n_uvw=NULL;

  static Nodes     *node=NULL;
  static Elements  *elem=NULL;
  static Datasets *lcase=NULL;
  static NodeBlocks *block=NULL;

  double fsmach, alpha, Re, time;


  if ( (lcase = (Datasets *)malloc( 1 * sizeof(Datasets))) == NULL )
    printf("\n\n ERROR: malloc failed, lcase\n\n") ;

  anz->n=-1;
  anz->e=-1;
  anz->l=-1;
  ld100=0;
  length = 1;

  /* Open the files and check to see that it was opened correctly */

  /* mesh */
  /* check if it is an input mesh (with a "." in the name) or an output mesh */
  lfile=strlen(datin);
  for(i=0; i<lfile; i++) if(datin[i]=='.') break;
  if(i<strlen(datin))
  {
    handle1 = fopen (datin, "r");
    lfile=-1;
  }
  else
  {
    sprintf(&datin[lfile], ".g.fmt");
    handle1 = fopen (datin, "r");
  }
  if ( handle1== NULL )
  {
    printf ("\nThe input file \"%s\" could not be opened.\n", datin);
    return (-1);
  }
  else printf ("\n%s opened\n",datin);
  strcpy(meshfile,datin);


  strcpy(anz->model, "isaac");
  printf (" MODEL NAME:  %s\n", anz->model);

  anz->n=anz->e=0;
  anz->nmax=0;  anz->nmin=1;
  anz->emax=0;  anz->emin=1;
  e_nmax=0;  e_nmin=MAXINTEGER;

  /* ------------------------------------------------------------------------- */
  /* read the geometry                                                         */
  /* ------------------------------------------------------------------------- */
  printf ("\n Read the Grid  \n");

  /* read the number of blocks if stated */
  length = frecord( handle1, buffer);
  length= sscanf(buffer,"%d %d",&nblock, &i);
  if(length==1)
  {
    printf("nblocks:%d\n",nblock);
  }
  else
  {
    fclose(handle1);
    handle1 = fopen (meshfile, "r");
    nblock=1;
  }
  anz->b=nblock;
  if ( (block = (NodeBlocks *)malloc( anz->b * sizeof(NodeBlocks))) == NULL )
    printf("\n\n ERROR: malloc failed, NodeBlocks\n\n") ;

  /* read the dimension of blocks  */
  for(i=0; i<nblock; i++)
  {
    mnx[i]=mny[i]=mnz[i]=1;
    length= fscanf(handle1,"%d",&mnx[i]);
    length+=fscanf(handle1,"%d",&mny[i]);
    if(elem_type==1) /* 3D */
      length+=fscanf(handle1,"%d",&mnz[i]);
    nsz = mnx[i]*mny[i]*mnz[i];
    printf("dim:%d block:%d i:%d j:%d nodes:%d\n",length,i+1,mnx[i],mny[i],nsz);

    block[i].i=mnx[i];
    block[i].j=mny[i];
    block[i].k=mnz[i];
    if ( (block[i].nod = (int *)malloc( nsz * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failed, NodeBlocks\n\n") ;
    
  }

  /* read the blocks  */
  for(i=0; i<nblock; i++)
  {
    printf(" block:%d i:%d j:%d k:%d\n",i+1,mnx[i],mny[i],mnz[i]);
    if(elem_type==1) /* 3D */
    {
      wmax  = mnx[i];
      vmax  = mny[i];
      umax  = mnz[i];
    }
    else
    {
      wmax  = mnz[i];
      vmax  = mnx[i];
      umax  = mny[i];
    }
    nsz = umax*vmax*wmax;

    /* continuous node-numbers */
    if ( (node = (Nodes *)realloc((Nodes *)node, (nsz+anz->n+2) * sizeof(Nodes))) == NULL )
      printf("\n\n ERROR: realloc failed, node\n\n") ;
    k=0;
    for(j=anz->n; j<nsz+anz->n; j++)
    { node[j].nr=j+1; node[node[j].nr].indx=j; block[i].nod[k++]=node[j].nr; }

    /* coordinates are stored in three rows, each holds either x, y or z */
    for(j=anz->n; j<nsz+anz->n; j++)
    {
      length=fscanf(handle1,"%lf",&fval[0]);
      if(length<1) break;
      node[node[j].nr].nx = fval[0];
    }
    for(j=anz->n; j<nsz+anz->n; j++)
    {
      length=fscanf(handle1,"%lf",&fval[0]);
      if(length<1) break;
      node[node[j].nr].ny = fval[0];
    }
    for(j=anz->n; j<nsz+anz->n; j++)
    {
      if(elem_type==1) /* 3D */
      {  length=fscanf(handle1,"%lf",&fval[0]); if(length<1) break; }
      else fval[0]=0.;
      node[node[j].nr].nz = fval[0];
    }


    for(j=anz->n; j<anz->n+5; j++)
    {
      printf("xyz[%d]: %lf %lf %lf %d\n",j, node[node[j].nr].nx, node[node[j].nr].ny, node[node[j].nr].nz, node[j].nr);
    }
    printf("\n");

    /* check */
    for(j=anz->n+nsz-5; j<anz->n+nsz; j++)
    {
      printf("xyz[%d]: %lf %lf %lf %d\n",j, node[node[j].nr].nx, node[node[j].nr].ny, node[node[j].nr].nz, node[j].nr);
    }
  
    if(elem_type==1) /* 3D */
    {
      /* generate the elements (3D) */
      if ( (n_uvw = (int *)realloc((int *)n_uvw,  nsz * sizeof(int))) == NULL )
        printf("\n\n ERROR: malloc failed, n_uvw\n\n") ;
      j=anz->n;
      for (u=0; u<umax; u++)
      {
        for (v=0; v<vmax; v++)
        {
          for (w=0; w<wmax; w++)
          {
            n_uvw[u*vmax*wmax+v*wmax+w]=++j;
          }
        }
      }
      j=0;
      for (u=0; u<umax-1; u++)
      {
        for (v=0; v<vmax-1; v++)
        {
          for (w=0; w<wmax-1; w++)
          {
            if ( (elem = (Elements *)realloc((Elements *)elem, (anz->e+2)  * sizeof(Elements))) == NULL )
              printf("\n\n ERROR: malloc failed, elem\n\n") ;
            elem[anz->e].nod[0]=n_uvw[ u*vmax*wmax     + v*wmax     + w      ];
            elem[anz->e].nod[1]=n_uvw[ u*vmax*wmax     + v*wmax     + (w+1)  ];
            elem[anz->e].nod[2]=n_uvw[ (u+1)*vmax*wmax + v*wmax     + (w+1)  ];
            elem[anz->e].nod[3]=n_uvw[ (u+1)*vmax*wmax + v*wmax     + w      ];
            elem[anz->e].nod[4]=n_uvw[ u*vmax*wmax     + (v+1)*wmax + w      ];
            elem[anz->e].nod[5]=n_uvw[ u*vmax*wmax     + (v+1)*wmax + (w+1)  ];
            elem[anz->e].nod[6]=n_uvw[ (u+1)*vmax*wmax + (v+1)*wmax + (w+1)  ];
            elem[anz->e].nod[7]=n_uvw[ (u+1)*vmax*wmax + (v+1)*wmax + w      ];
            elem[anz->e].type= 1 ;
            elem[anz->e].group= i;
            elem[anz->e].mat = 1;
            anz->etype[elem[anz->e].type]++;
            elem[anz->e].nr  = ++anz->e;
          }
        }
      }
    }
    else
    {
      /* generate the elements (2D) */
      if ( (n_uv = (int *)realloc((int *)n_uv,  nsz * sizeof(int))) == NULL )
        printf("\n\n ERROR: malloc failed, n_uv\n\n") ;
      j=anz->n;
      for (u=0; u<umax; u++)
      {
        for (v=0; v<vmax; v++)
        {
          n_uv[u*vmax+v]=++j;
        }
      }
      j=0;
      for (u=0; u<umax-1; u++)
      {
        for (v=0; v<vmax-1; v++)
        {
          if ( (elem = (Elements *)realloc((Elements *)elem, (anz->e+2)  * sizeof(Elements))) == NULL )
            printf("\n\n ERROR: malloc failed, elem\n\n") ;
          /* if 2D change the orientation of the elements so that the normals point in z */
          elem[anz->e].nod[0] = n_uv[(u  )*vmax + v    ];
          elem[anz->e].nod[3] = n_uv[(u+1)*vmax + v    ];
          elem[anz->e].nod[2] = n_uv[(u+1)*vmax + (v+1)];
          elem[anz->e].nod[1] = n_uv[(u  )*vmax + (v+1)];
          elem[anz->e].type= 9 ;
          elem[anz->e].group= i;
          elem[anz->e].mat = 1;
          anz->etype[elem[anz->e].type]++;
          elem[anz->e].nr  = ++anz->e;
        }
      }
    }
    anz->n+=nsz;
  }
  fclose (handle1);

  anz->nmax=anz->n;
  if(anz->nmax>0) anz->nmin=1;
  else anz->nmin=0;
  anz->emax=anz->e;
  if(anz->emax>0) anz->emin=1;
  else anz->emin=0;

  if(lfile==-1) goto endMark;

  /* ------------------------------------------------------------------------- */
  /* read the results                                                          */
  /* ------------------------------------------------------------------------- */
  printf ("\n Read the Results  \n");

  /* results */
  sprintf(&datin[lfile], ".q.fmt");
  handle2 = fopen (datin, "r");
  if ( handle2== NULL )
  {
    printf ("\nThe input file \"%s\" could not be opened.\n", datin);
  }
  else printf ("\n%s opened\n",datin);

  /* more results */
  /* description of results */
  sprintf(&datin[lfile], ".qt.nam");
  handle3 = fopen (datin, "r");
  if ( handle3== NULL )
  {
    printf ("\nThe input file \"%s\" could not be opened.\n-> no turbulence quantities available\n", datin);
  }
  /* results */
  else
  {
    printf ("\n%s opened\n",datin);
    sprintf(&datin[lfile], ".qt.fmt");
    handle4 = fopen (datin, "r");
    if ( handle4== NULL )
    {
      printf ("\nThe input file \"%s\" could not be opened.\n", datin);
      return (-1);
    }
    printf ("\n%s opened\n",datin);
  }

  /* read the names of the stored entities */
  if(handle3!=NULL)
  {
    for(i=0; i<MAXIDENTIFIER; i++) strcpy(identifier[i], "        ");
    nident=-1;
    do
    {
      nident++;
      length = fscanf(handle3,"%s",buffer);
      if(length<1) break;
      if(nident<MAXIDENTIFIER)
      {
        sprintf(identifier[nident],"%s", buffer);
        identifier[nident][strlen(buffer)]=' ';
        printf("i:%d ident[%d]:|%s|\n",i, nident,identifier[nident]);
      }
      else
      {
        nident--;
        break;
      }
    }while(length==1);
    fclose (handle3);


    /* read the resuts of the turbulence model */
    length= fscanf(handle4,"%d",&nblock);
    printf("nblocks:%d\n",nblock);
    for(i=0; i<nblock; i++)
    {
      mnx[i]=mny[i]=mnz[i]=1;
      length=fscanf(handle4,"%d",&mnx[i]);
      length+=fscanf(handle4,"%d",&mny[i]);
      if(elem_type==1) /* 3D */
        length+=fscanf(handle4,"%d",&mnz[i]);
      printf("dim:%d block:%d i:%d j:%d k:%d\n",length,i+1,mnx[i],mny[i],mnz[i]);
      length+=fscanf(handle4,"%d",&j);
      if(nident!=j) { printf("ERROR: number of turb.entities not correct! %d %d \n", j, nident); exit(1); }
    }
  
    /* define the lcases */
    anz_n=0; 
    anz->l++;
    ld100++;
    if ( (lcase = (Datasets *)realloc((Datasets *)lcase, (anz->l+2) * sizeof(Datasets))) == NULL )
    { printf("\n\n ERROR: malloc failure\n\n" ); exit(1); }

    lcase[anz->l].value=ld100;
    lcase[anz->l].ncomps = nident;
    lcase[anz->l].irtype = 1;
    sprintf( lcase[anz->l].name,"%s", "turb----");
    printf ("lcase.name[%d]= %s\n", anz->l, lcase[anz->l].name);

    lcase[anz->l].npheader=0;
    strcpy(lcase[anz->l].analysis_name,"");
    sprintf(lcase[anz->l].dataset_name,"CFD");
    strcpy(lcase[anz->l].dataset_text,"INITIAL");
    lcase[anz->l].step_number=1;
    lcase[anz->l].analysis_type=1;

    if ( (lcase[anz->l].nmax = (int *)malloc( lcase[anz->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase[anz->l].nmin = (int *)malloc( lcase[anz->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase[anz->l].max = (float *)malloc( lcase[anz->l].ncomps * sizeof(float))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase[anz->l].min = (float *)malloc( lcase[anz->l].ncomps * sizeof(float))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase[anz->l].dat = (float **)malloc( lcase[anz->l].ncomps * sizeof(float *))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase[anz->l].compName = (char **)malloc( lcase[anz->l].ncomps * sizeof(char *))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase[anz->l].icname = (char **)malloc( lcase[anz->l].ncomps * sizeof(char *))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    for(i=0; i<lcase[anz->l].ncomps; i++)
    {
      if ( (lcase[anz->l].dat[i] = (float *)malloc( (anz->nmax+1) * sizeof(float))) == NULL )
        printf("\n\n ERROR: malloc failure\n\n" );	               
      if ( (lcase[anz->l].compName[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
         printf("\n\n ERROR: malloc failed\n\n" );
      if ( (lcase[anz->l].icname[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
         printf("\n\n ERROR: malloc failed\n\n" );
      lcase[anz->l].max[i]=-MAXFLOAT;
      lcase[anz->l].min[i]=MAXFLOAT;
    }
    if ( (lcase[anz->l].menu = (int *)malloc( lcase[anz->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase[anz->l].ictype = (int *)malloc( lcase[anz->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase[anz->l].icind1 = (int *)malloc( lcase[anz->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase[anz->l].icind2 = (int *)malloc( lcase[anz->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase[anz->l].iexist = (int *)malloc( lcase[anz->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );

    for(i=0; i<lcase[anz->l].ncomps; i++)
    {
      lcase[anz->l].menu[i] = 1;
      lcase[anz->l].ictype[i] = 1;
      lcase[anz->l].icind1[i] = 0;
      lcase[anz->l].icind2[i] = 0;
      lcase[anz->l].iexist[i] = 0;
    }

    for(n=0; n<lcase[anz->l].ncomps; n++) strcpy( lcase[anz->l].compName[n], identifier[n]);
  
    for(i=0; i<nblock; i++)
    {
      printf(" block:%d i:%d j:%d k:%d\n",i+1,mnx[i],mny[i],mnz[i]);

      if(elem_type==1) /* 3D */
      {
        wmax  = mnx[i];
        vmax  = mny[i];
        umax  = mnz[i];
      }
      else /* 2D */
      {
        wmax  = mnz[i];
        vmax  = mnx[i];
        umax  = mny[i];
      }
      nsz = umax*vmax*wmax;
      
      for(n=0; n<nident; n++)
      {
        for(j=anz_n; j<anz_n+nsz; j++)
        {
          length=fscanf(handle4,"%lf",&fval[0]);
          if(length<1) break;
          lcase[anz->l].dat[n][j+1]       = fval[0];
          if(lcase[anz->l].dat[n][j+1] > lcase[anz->l].max[n])
          {
            lcase[anz->l].max[n]=lcase[anz->l].dat[n][j+1];
            lcase[anz->l].nmax[n]=j+1;
          }
          if(lcase[anz->l].dat[n][j+1] < lcase[anz->l].min[n])
          {
            lcase[anz->l].min[n]=lcase[anz->l].dat[n][j+1];
            lcase[anz->l].nmin[n]=j+1;
          }
        }
      }
      anz_n+=nsz;
    }
    fclose (handle4);
  }

  /* read the basic resuts  */
  if(handle2!=NULL)
  {
    length= fscanf(handle2,"%d",&nblock);
    printf("nblocks:%d\n",nblock);
    for(i=0; i<nblock; i++)
    {
      mnx[i]=mny[i]=mnz[i]=1;
      length=fscanf(handle2,"%d",&mnx[i]);
      length+=fscanf(handle2,"%d",&mny[i]);
      if(elem_type==1) /* 3D */
        length+=fscanf(handle2,"%d",&mnz[i]);
      printf("dim:%d block:%d i:%d j:%d k:%d\n",length,i+1,mnx[i],mny[i],mnz[i]);
    }
  
    /* define the lcases */
    anz_n=0; 
    anz->l++;
    ld100++;
    if ( (lcase = (Datasets *)realloc((Datasets *)lcase, (anz->l+2) * sizeof(Datasets))) == NULL )
    { printf("\n\n ERROR: malloc failure\n\n" ); exit(1); }

    lcase[anz->l].value=ld100;
    if(elem_type==1) lcase[anz->l].ncomps = 5; /* 3D */
    else  lcase[anz->l].ncomps = 4;
    lcase[anz->l].irtype = 1;
    sprintf( lcase[anz->l].name,"%s", "basic---");
    printf ("lcase.name[%d]= %s\n", anz->l, lcase[anz->l].name);

    lcase[anz->l].npheader=0;
    strcpy(lcase[anz->l].analysis_name,"");
    sprintf(lcase[anz->l].dataset_name,"CFD");
    strcpy(lcase[anz->l].dataset_text,"INITIAL");
    lcase[anz->l].step_number=1;
    lcase[anz->l].analysis_type=1;

    if ( (lcase[anz->l].nmax = (int *)malloc( lcase[anz->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase[anz->l].nmin = (int *)malloc( lcase[anz->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase[anz->l].max = (float *)malloc( lcase[anz->l].ncomps * sizeof(float))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase[anz->l].min = (float *)malloc( lcase[anz->l].ncomps * sizeof(float))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase[anz->l].dat = (float **)malloc( lcase[anz->l].ncomps * sizeof(float *))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase[anz->l].compName = (char **)malloc( lcase[anz->l].ncomps * sizeof(char *))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase[anz->l].icname = (char **)malloc( lcase[anz->l].ncomps * sizeof(char *))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    for(i=0; i<lcase[anz->l].ncomps; i++)
    {
      if ( (lcase[anz->l].dat[i] = (float *)malloc( (anz->nmax+1) * sizeof(float))) == NULL )
        printf("\n\n ERROR: malloc failure\n\n" );	               
      if ( (lcase[anz->l].compName[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
         printf("\n\n ERROR: malloc failed\n\n" );
      if ( (lcase[anz->l].icname[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
         printf("\n\n ERROR: malloc failed\n\n" );
      lcase[anz->l].max[i]=-MAXFLOAT;
      lcase[anz->l].min[i]=MAXFLOAT;
    }
    if ( (lcase[anz->l].menu = (int *)malloc( lcase[anz->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase[anz->l].ictype = (int *)malloc( lcase[anz->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase[anz->l].icind1 = (int *)malloc( lcase[anz->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase[anz->l].icind2 = (int *)malloc( lcase[anz->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase[anz->l].iexist = (int *)malloc( lcase[anz->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );

    for(i=0; i<lcase[anz->l].ncomps; i++)
    {
      lcase[anz->l].menu[i] = 1;
      lcase[anz->l].ictype[i] = 1;
      lcase[anz->l].icind1[i] = 0;
      lcase[anz->l].icind2[i] = 0;
      lcase[anz->l].iexist[i] = 0;
    }

    strcpy( lcase[anz->l].compName[0], "rho ");
    strcpy( lcase[anz->l].compName[1], "rho*u ");
    strcpy( lcase[anz->l].compName[2], "rho*v ");
    if(elem_type==1)
    {
      strcpy( lcase[anz->l].compName[3], "rho*w ");
      strcpy( lcase[anz->l].compName[4], "rho*E ");
    }
    else strcpy( lcase[anz->l].compName[3], "rho*E ");
  
    for(i=0; i<nblock; i++)
    {
      length=fscanf(handle2,"%lf",&fsmach);
      length=fscanf(handle2,"%lf",&alpha);
      length=fscanf(handle2,"%lf",&Re);
      length=fscanf(handle2,"%lf",&time);
      printf(" block:%d i:%d j:%d k:%d  fsmach:%f alpha:%f Re:%f time:%f\n",i+1,mnx[i],mny[i],mnz[i], fsmach,alpha,Re,time);

      if(elem_type==1) /* 3D */
      {
        wmax  = mnx[i];
        vmax  = mny[i];
        umax  = mnz[i];
      }
      else /* 2D */
      {
        wmax  = mnz[i];
        vmax  = mnx[i];
        umax  = mny[i];
      }
      nsz = umax*vmax*wmax;

      for(n=0; n<lcase[anz->l].ncomps; n++)
      {
        for(j=anz_n; j<anz_n+nsz; j++)
        {
          length=fscanf(handle2,"%lf",&fval[0]);
          if(length<1) break;
          lcase[anz->l].dat[n][j+1] = fval[0];
          if(lcase[anz->l].dat[n][j+1] > lcase[anz->l].max[n])
          {
            lcase[anz->l].max[n]=lcase[anz->l].dat[n][j+1];
            lcase[anz->l].nmax[n]=j+1;
          }
          if(lcase[anz->l].dat[n][j+1] < lcase[anz->l].min[n])
          {
            lcase[anz->l].min[n]=lcase[anz->l].dat[n][j+1];
            lcase[anz->l].nmin[n]=j+1;
          }
        }
      }
      anz_n+=nsz;
    }
    fclose (handle2);
  }

 endMark:;

  anz->l++;

  if ( e_nmax > (anz->nmax) )
  {
    printf ("\n WARNING: element requestes a nodename higher than allocated\n\n");
    printf (" e_nmax=%d e_nmin=%d \n", e_nmax, e_nmin );
  }
  if ( e_nmin < 1 )
  {
    printf ("\n WARNING: element requestes a nodename lower than allocated\n\n");
    printf (" e_nmax=%d e_nmin=%d \n", e_nmax, e_nmin );
  }

  /* calculate additional entities */
  calcAddEntitiesIsaac( anz, node, elem, lcase, elem_type, PINF, TINF, R_GAS);

  /* set .loaded to 1 to indicate that the data are available */
  for (i=0; i<anz->l; i++) { lcase[i].loaded=1; lcase[i].fileptr=NULL; }

  *nptr =  node; *eptr = elem; *lptr = lcase; *bptr = block;
  strcpy(datin, meshfile);
  return (1);
}





