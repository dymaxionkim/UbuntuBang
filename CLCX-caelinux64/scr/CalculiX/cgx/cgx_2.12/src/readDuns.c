/* ----------------------------------------------------------------  */
/* readDuns                                       22.09.2002 Wittig  */
/* ----------------------------------------------------------------  */


#include <extUtil.h>
#include <sys/utsname.h>

#define     MAXIDENTIFIER  12        /* maximum in duns is 12 */
#define MAXINTEGER 2147483647
#define MAXFLOAT   1.e32
#define MAX_BLOCKS  100000


#define R_GAS 287.1
#define kappa    1.4
#define cpg  1003.


int calcAddEntitiesDuns( Summen *anz, Nodes *node, Elements *elem, Datasets *lcase)
{
  int lc, n, i, j;
  int T_indx=-1, U_indx=-1, p_indx=-1;

  double ps, us, ts, pt, tt, rho;

  for (lc=0; lc<anz->l; lc++)
  {
    n=0; while(n<lcase[lc].ncomps)
    {
      /* search U entities */
      if(compare(lcase[lc].compName[n], "u ", 2)==2)
      {
        lcase[lc].ncomps++;

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
        if ( (lcase[lc].compName[lcase[lc].ncomps-1] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
          printf("\n\n ERROR: malloc failed\n\n" );
        if ( (lcase[lc].dat[lcase[lc].ncomps-1]      = (float *)malloc( (anz->nmax+1) * sizeof(float))) == NULL )
          printf("\n\n ERROR: malloc failed\n\n" );	               
        if ( (lcase[lc].icname[lcase[lc].ncomps-1]   = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
          printf("\n\n ERROR: malloc failed\n\n" );
        lcase[lc].max[lcase[lc].ncomps-1]=-MAXFLOAT;
        lcase[lc].min[lcase[lc].ncomps-1]=MAXFLOAT;
        
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
        
        lcase[lc].menu[lcase[lc].ncomps-1] = 1;
        lcase[lc].ictype[lcase[lc].ncomps-1] = 1;
        lcase[lc].icind1[lcase[lc].ncomps-1] = 0;
        lcase[lc].icind2[lcase[lc].ncomps-1] = 0;
        lcase[lc].iexist[lcase[lc].ncomps-1] = 0;

        /* calculate the velocity */
        sprintf(lcase[lc].compName[lcase[lc].ncomps-1],"|U| ");
        for(i=0; i<anz->n; i++)
	{
          lcase[lc].dat[lcase[lc].ncomps-1][node[i].nr]=sqrt( lcase[lc].dat[n][node[i].nr]*lcase[lc].dat[n][node[i].nr]+lcase[lc].dat[n+1][node[i].nr]*lcase[lc].dat[n+1][node[i].nr]+lcase[lc].dat[n+2][node[i].nr]*lcase[lc].dat[n+2][node[i].nr]);
          if(lcase[lc].dat[lcase[lc].ncomps-1][node[i].nr] > lcase[lc].max[lcase[lc].ncomps-1])
          {
             lcase[lc].max[lcase[lc].ncomps-1]=lcase[lc].dat[lcase[lc].ncomps-1][node[i].nr];
             lcase[lc].nmax[lcase[lc].ncomps-1]=node[i].nr;
          }
          if(lcase[lc].dat[lcase[lc].ncomps-1][node[i].nr] < lcase[lc].min[lcase[lc].ncomps-1])
          {
             lcase[lc].min[lcase[lc].ncomps-1]=lcase[lc].dat[lcase[lc].ncomps-1][node[i].nr];
             lcase[lc].nmin[lcase[lc].ncomps-1]=node[i].nr;
          }
	}
      }
      n++;
    }

    n=0; while(n<lcase[lc].ncomps)
    {
      /* search T entity */
      if(compare(lcase[lc].compName[n], "t ", 2)==2)
      {
        /* determine the location of T, U and p */
        T_indx=n;
        U_indx=p_indx=-1;
        for(i=0; i<lcase[lc].ncomps; i++)
        {
          if(compare(lcase[lc].compName[i], "vel", 3)==3) U_indx=i;
          if(compare(lcase[lc].compName[i], "p ", 2)==2) p_indx=i;
	}

       if((U_indx!=-1)||(p_indx!=-1))
       { 

        lcase[lc].ncomps+=3;

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

        for(i=lcase[lc].ncomps-3; i<lcase[lc].ncomps; i++)
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
        
        for(i=lcase[lc].ncomps-3; i<lcase[lc].ncomps; i++)
	{
          lcase[lc].menu[i] = 1;
          lcase[lc].ictype[i] = 1;
          lcase[lc].icind1[i] = 0;
          lcase[lc].icind2[i] = 0;
          lcase[lc].iexist[i] = 0;
	}

        /* calculate the velocity */
        sprintf(lcase[lc].compName[lcase[lc].ncomps-3],"TT ");
        sprintf(lcase[lc].compName[lcase[lc].ncomps-2],"PT ");
        sprintf(lcase[lc].compName[lcase[lc].ncomps-1],"rho ");

        for(i=0; i<anz->n; i++)
	{
          ps=lcase[lc].dat[p_indx][node[i].nr];
          us=lcase[lc].dat[U_indx][node[i].nr];
          ts=lcase[lc].dat[T_indx][node[i].nr];
          rho=ps/R_GAS/ts;
          tt=us*us*.5/cpg + ts;
	  pt = ps/pow( (ts/tt), (kappa/(kappa-1)) );
          lcase[lc].dat[lcase[lc].ncomps-3][node[i].nr]=tt;
          lcase[lc].dat[lcase[lc].ncomps-2][node[i].nr]=pt;
          lcase[lc].dat[lcase[lc].ncomps-1][node[i].nr]=rho;

          for(j=lcase[lc].ncomps-3; j<lcase[lc].ncomps; j++)
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
      }
      n++;
    }

  }
  return (1);
}


int readDuns(char *datin, Summen *anz, Nodes **nptr, Elements **eptr, Datasets **lptr, int elem_type )
{
  FILE *handle1, *handle2, *handle3;
  int i, j, n, ld100, lfile;
  char rec_str[MAX_LINE_LENGTH];
  char buffer[MAX_LINE_LENGTH];
  int  anz_n, time_step=0;
  int  e_nmax=1, e_nmin=1;
  int  length;
  float fval[1];

  
  char identifier[MAXIDENTIFIER][MAX_LINE_LENGTH];
  int mnx[MAX_BLOCKS],mny[MAX_BLOCKS],mnz[MAX_BLOCKS];
  int u,v,w,umax,vmax,wmax,nsz,nblock,nident;
  int *n_uv=NULL, *n_uvw=NULL;

  static Nodes     *node=NULL;
  static Elements  *elem=NULL;
  static Datasets *lcase=NULL;

  int  HEAD_CHARS, MID_CHARS;
  struct utsname  cursys[1];

  uname (cursys);
  if(compareStrings(cursys->machine, "x86_64")>0) { HEAD_CHARS=8; MID_CHARS=16; }
  else  { HEAD_CHARS=4; MID_CHARS=8; }
  // in case the file does not read correctly (for example wrong nr of blocks are listed in the xterm) it might help to try the chars manually:
  //HEAD_CHARS=4; MID_CHARS=8;

  if ( (lcase = (Datasets *)malloc( 1 * sizeof(Datasets))) == NULL )
    printf("\n\n ERROR: malloc failed, lcase\n\n") ;

  anz->n=-1;
  anz->e=-1;
  anz->l=-1;
  ld100=0;
  length = 1;

  /* Open the files and check to see that it was opened correctly */

  /* mesh */
  lfile=strlen(datin);
  sprintf(&datin[lfile], ".g");
  handle1 = fopen (datin, "rb");
  if ( handle1== NULL )
  {
    printf ("\nThe input file \"%s\" could not be opened.\n", datin);
    printf (" trying to use the default mesh file \"dunsout.g\"\n");
    handle1 = fopen ("dunsout.g", "rb");
    if ( handle1== NULL )
    { printf ("\nThe input file \"dunsout.g\" could also not be opened.\n\n");
      return (-1); }
    printf ("\ndunsout.g opened\n");
  }
  else printf ("\n%s opened\n",datin);

  /* results */
  sprintf(&datin[lfile], ".q");
  handle2 = fopen (datin, "rb");
  if ( handle2== NULL )
  {
    printf ("\nThe input file \"%s\" could not be opened.\n", datin);
    printf (" trying to use the file-extension \".1\"\n");
    sprintf(&datin[lfile], ".1");
    handle2 = fopen (datin, "rb");
    if ( handle2== NULL )
    { printf ("\nThe input file \"%s\" could also not be opened.\n\n",datin);
      return (-1); }
    printf ("\n%s opened\n",datin);
    time_step++;
  }
  else printf ("\n%s opened\n",datin);

  /* description of results */
  sprintf(&datin[lfile], ".v");
  handle3 = fopen (datin, "rb");
  if ( handle3== NULL )
  { printf ("\nThe input file \"%s\" could not be opened.\n\n", datin); return (-1); }
  else printf ("\n%s opened\n",datin);

  strcpy(anz->model, "duns");
  printf (" MODEL NAME:  %s\n", anz->model);

  anz->n=anz->e=0;
  anz->nmax=-MAXINTEGER;  anz->nmin=1;
  anz->emax=-MAXINTEGER;  anz->emin=1;
  e_nmax=-MAXINTEGER;  e_nmin=MAXINTEGER;

  /* ------------------------------------------------------------------------- */
  /* read the geometry                                                         */
  /* ------------------------------------------------------------------------- */
  printf ("\n Read the Grid  \n");

  /* four chars at the beginning of the binary file */
  length=fread(buffer,sizeof(char),HEAD_CHARS,handle1);
  length=fread(&nblock,sizeof(int),1,handle1);
  /* eight char after each write statement in the fortran code */
  length=fread(buffer,sizeof(char),MID_CHARS,handle1);

  printf("nblocks:%d\n",nblock);

  if(elem_type==1) /* 3D */
  {
    for(i=0; i<nblock; i++)
    {
      mnx[i]=mny[i]=mnz[i]=0;
      length=fread(&mnx[i],sizeof (int),1,handle1);
      length+=fread(&mny[i],sizeof(int),1,handle1);
      length+=fread(&mnz[i],sizeof(int),1,handle1);
      printf("dim:%d block:%d i:%d j:%d k:%d\n",length,i+1,mnx[i],mny[i],mnz[i]);
    }
    length=fread(buffer,sizeof(char),MID_CHARS,handle1);
  
    for(i=0; i<nblock; i++)
    {
      printf(" block:%d i:%d j:%d k:%d\n",i,mnx[i],mny[i],mnz[i]);
      wmax  = mnx[i];
      vmax  = mny[i];
      umax  = mnz[i];
      nsz = umax*vmax*wmax;
  
      /* continuous node-numbers */
      if ( (node = (Nodes *)realloc((Nodes *)node, (nsz+anz->n+2) * sizeof(Nodes))) == NULL )
        printf("\n\n ERROR: realloc failed, node\n\n") ;
      for(j=anz->n; j<nsz+anz->n; j++) { node[j].nr=j+1; node[node[j].nr].indx=j; }
  
      /* coordinates are stored in three rows, each holds either x, y or z */
      for(j=anz->n; j<anz->n+nsz; j++)
      {
        length=fread(&fval, sizeof(float), 1, handle1);
        if(length<1) break;
        node[node[j].nr].nx = fval[0];
      }
      for(j=anz->n; j<anz->n+nsz; j++)
      {
        length=fread(&fval, sizeof(float), 1, handle1);
        if(length<1) break;
        node[node[j].nr].ny = fval[0];
      }
      for(j=anz->n; j<anz->n+nsz; j++)
      {
        length=fread(&fval, sizeof(float), 1, handle1);
        if(length<1) break;
        node[node[j].nr].nz = fval[0];
      }
      length=fread(buffer,sizeof(char),MID_CHARS,handle1);
  
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
      anz->n+=nsz;
    }
  }
  else
  {
    for(i=0; i<nblock; i++)
    {
      mnx[i]=mny[i]=0;
      length=fread(&mnx[i],sizeof (int),1,handle1);
      length+=fread(&mny[i],sizeof(int),1,handle1);
      printf("dim:%d block:%d i:%d j:%d\n",length,i+1,mnx[i],mny[i]);
    }
    length=fread(buffer,sizeof(char),MID_CHARS,handle1);
  
    for(i=0; i<nblock; i++)
    {
      printf(" block:%d i:%d j:%d\n",i,mnx[i],mny[i]);
      vmax  = mnx[i];
      umax  = mny[i];
      nsz = umax*vmax;
  
      /* continuous node-numbers */
      if ( (node = (Nodes *)realloc((Nodes *)node, (nsz+anz->n+2) * sizeof(Nodes))) == NULL )
        printf("\n\n ERROR: realloc failed, node\n\n") ;
      for(j=anz->n; j<nsz+anz->n; j++) { node[j].nr=j+1; node[node[j].nr].indx=j; node[node[j].nr].nz=0.; }
  
      /* coordinates are stored in three rows, each holds either x, y or z */
      for(j=anz->n; j<anz->n+nsz; j++)
      {
        length=fread(&fval, sizeof(float), 1, handle1);
        if(length<1) break;
        node[node[j].nr].nx = fval[0];
      }
      for(j=anz->n; j<anz->n+nsz; j++)
      {
        length=fread(&fval, sizeof(float), 1, handle1);
        if(length<1) break;
        node[node[j].nr].ny = fval[0];
      }
      length=fread(buffer,sizeof(char),MID_CHARS,handle1);
  
      for(j=anz->n; j<anz->n+5; j++)
      {
        printf("xyz[%d]: %lf %lf %d\n",j, node[node[j].nr].nx, node[node[j].nr].ny, node[j].nr);
      }
      printf("\n");
  
      /* check */
      for(j=anz->n+nsz-5; j<anz->n+nsz; j++)
      {
        printf("xyz[%d]: %lf %lf %d\n",j, node[node[j].nr].nx, node[node[j].nr].ny, node[j].nr);
      }
  
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
          elem[anz->e].nod[0] = n_uv[(u  )*vmax + v    ];
          elem[anz->e].nod[1] = n_uv[(u+1)*vmax + v    ];
          elem[anz->e].nod[2] = n_uv[(u+1)*vmax + (v+1)];
          elem[anz->e].nod[3] = n_uv[(u  )*vmax + (v+1)];
          elem[anz->e].type= 9 ;
          elem[anz->e].group= i;
          elem[anz->e].mat = 1;
          anz->etype[elem[anz->e].type]++;
          elem[anz->e].nr  = ++anz->e;
        }
      }  
      anz->n+=nsz;
    }
  }
  anz->nmax=anz->n;
  if(anz->nmax>0) anz->nmin=1;
  else anz->nmin=0;
  anz->emax=anz->e;
  if(anz->emax>0) anz->emin=1;
  else anz->emin=0;
 
  /* ------------------------------------------------------------------------- */
  /* read the results                                                          */
  /* ------------------------------------------------------------------------- */
  printf ("\n Read the Results  \n");

  /* read x.v with the names of the stored entities */
  for(i=0; i<MAXIDENTIFIER; i++) strcpy(identifier[i], "        ");
  length = frecord( handle3, rec_str);
    printf("string:%s\n",rec_str);
  i=0;
  nident=-1;
  do
  {
    nident++;
    length = sscanf(&rec_str[i],"%s",buffer);
    if(length<1) break;
    i+=strlen(buffer)+1;
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


  /* read the resuts of each time-step */

  do
  {
    /* four chars at the beginning of the binary file */
    length=fread(buffer,sizeof(char),HEAD_CHARS,handle2);
    length=fread(&nblock,sizeof(int),1,handle2);
    /* eight char after each write statement in the fortran code */
    length=fread(buffer,sizeof(char),MID_CHARS,handle2);
    printf("nblocks:%d\n",nblock);
  
    if(elem_type==1) /* 3D */
    {
      for(i=0; i<nblock; i++)
      {
        mnx[i]=mny[i]=mnz[i]=0;
        length=fread(&mnx[i],sizeof (int),1,handle2);
        length+=fread(&mny[i],sizeof(int),1,handle2);
        length+=fread(&mnz[i],sizeof(int),1,handle2);
        printf("dim:%d block:%d i:%d j:%d k:%d\n",length,i+1,mnx[i],mny[i],mnz[i]);
      }
      length=fread(buffer,sizeof(char),MID_CHARS,handle2);
    
      /* define the lcases */
      anz_n=0; 
      anz->l++;
      ld100++;
      if ( (lcase = (Datasets *)realloc((Datasets *)lcase, (anz->l+2) * sizeof(Datasets))) == NULL )
      { printf("\n\n ERROR: malloc failure\n\n" ); exit(1); }

      lcase[anz->l].value=ld100;
      lcase[anz->l].ncomps = nident;
      lcase[anz->l].irtype = 1;
      sprintf( lcase[anz->l].name,"%s", "duns");
      printf ("lcase.name[%d]= %s\n", anz->l, lcase[anz->l].name);

      lcase[anz->l].npheader=0;
      strcpy(lcase[anz->l].analysis_name,"");
      sprintf(lcase[anz->l].dataset_name,"CFD");
      strcpy(lcase[anz->l].dataset_text,"INITIAL");
      lcase[anz->l].step_number=time_step;
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
        printf(" block:%d i:%d j:%d k:%d\n",i,mnx[i],mny[i],mnz[i]);
        wmax  = mnx[i];
        vmax  = mny[i];
        umax  = mnz[i];
        nsz = umax*vmax*wmax;
      
        /* dummy doubles lesen 4*4 + 8 trailing bytes */
        length=fread(buffer,sizeof(int),4,handle2);
        length=fread(buffer,sizeof(char),MID_CHARS,handle2);
  
        for(n=0; n<nident; n++)
        {
          for(j=anz_n; j<anz_n+nsz; j++)
          {
            length=fread(&fval, sizeof(float), 1, handle2);
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
  
        length=fread(buffer,sizeof(char),MID_CHARS,handle2);
        anz_n+=nsz;
      }
    }
  
    else /* 2D */
    {
      for(i=0; i<nblock; i++)
      {
        mnx[i]=mny[i]=0;
        length=fread(&mnx[i],sizeof (int),1,handle2);
        length+=fread(&mny[i],sizeof(int),1,handle2);
        printf("dim:%d block:%d i:%d j:%d\n",length,i+1,mnx[i],mny[i]);
      }
      length=fread(buffer,sizeof(char),MID_CHARS,handle2);
    
      /* define the lcases */
      anz_n=0; 
      anz->l++;
      ld100++;
      if ( (lcase = (Datasets *)realloc((Datasets *)lcase, (anz->l+2) * sizeof(Datasets))) == NULL )
      { printf("\n\n ERROR: malloc failure\n\n" ); exit(1); }

      lcase[anz->l].value=ld100;
      lcase[anz->l].ncomps = nident;
      lcase[anz->l].irtype = 1;
      sprintf( lcase[anz->l].name,"%s", datin);
      printf ("lcase.name[%d]= %s\n", anz->l, lcase[anz->l].name);

      lcase[anz->l].npheader=0;
      strcpy(lcase[anz->l].analysis_name,"");
      sprintf(lcase[anz->l].dataset_name,"CFD");
      strcpy(lcase[anz->l].dataset_text,"INITIAL");
      lcase[anz->l].step_number=time_step;
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

      for(n=0; n<lcase[anz->l].ncomps; n++)
        strcpy( lcase[anz->l].compName[n], identifier[n]);
    
      for(i=0; i<nblock; i++)
      {
        printf(" block:%d i:%d j:%d\n",i,mnx[i],mny[i]);
        vmax  = mnx[i];
        umax  = mny[i];
        nsz = umax*vmax;

        /* dummy doubles lesen 4*4 + 8 trailing bytes */
        length=fread(buffer,sizeof(int),4,handle2);
        length=fread(buffer,sizeof(char),MID_CHARS,handle2);

        for(n=0; n<nident; n++)
        {
          for(j=anz_n; j<anz_n+nsz; j++)
          {
            length=fread(&fval, sizeof(float), 1, handle2);
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
        length=fread(buffer,sizeof(char),MID_CHARS,handle2);
        anz_n+=nsz;
      }
    }
    fclose (handle2);

    /* open the file of the next time-step if any */
    time_step++;
    sprintf(&datin[lfile], ".%d", time_step);
    handle2 = fopen (datin, "rb");
    if ( handle2== NULL )
    { printf ("\nThe input file \"%s\" could not be opened.\n\n", datin); break; }
    else  printf ("\n%s opened",datin);

  }while(1);

  anz->l++;
  fclose (handle1);
  fclose (handle3);

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
  elemChecker( anz->e, node, elem);

  /* calculate additional entities */
  /* calcAddEntitiesDuns( anz, node, elem, lcase); */

  /* set .loaded to 1 to indicate that the data are available */
  for (i=0; i<anz->l; i++) { lcase[i].loaded=1; lcase[i].fileptr=NULL; }

  *nptr =  node; *eptr = elem; *lptr = lcase;
  return (1);
}





