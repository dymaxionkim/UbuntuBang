
#include <extUtil.h>

int write2isaac( char *name, Summen *anz, Nodes *node, Elements *elem, Datasets *lcase, NodeBlocks *nBlock )
{
  FILE *handle1, *handle2, *handle3;
  int  i,j,k,l,n,b=0, edges=0, m[3];
  char datout[MAX_LINE_LENGTH];
  int ic,jc,kc, **buf;
  int        pn=0;

  /* change the name of files from <setname> to isaac */
  strcpy(name, "isaac");

  /* Open the files and check to see that it was opened correctly */
  sprintf(datout, "%s.grd", name);
  handle1 = fopen (datout, "w+b");
  if (handle1==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n",
     datout); return(-1);}
  else  printf (" file %s opened\n",datout);

  sprintf(datout, "%s.dat", name);
  handle2 = fopen (datout, "w+b");
  if (handle2==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n",
     datout); return(-1);}
  else  printf (" file %s opened\n",datout);

  sprintf(datout, "%s.fbd", name);
  handle3 = fopen (datout, "w+b");
  if (handle3==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n",
     datout); return(-1);}
  else  printf (" file %s opened\n",datout);


  printf (" write isaac mesh \n");

  /* grid file */
  if(anz->b>1) fprintf (handle1, "%d\n", anz->b);
  for (b=0; b<anz->b; b++)
  {
    /* if 2D switch i,j (face normals must point against z direction, in duns in z direction) */
    if(nBlock[b].dim==2) fprintf (handle1, "%d %d\n", nBlock[b].j, nBlock[b].i);
    else                 fprintf (handle1, "%d %d %d\n", nBlock[b].i, nBlock[b].j, nBlock[b].k);
  }

  for (b=0; b<anz->b; b++)
  {
    /* if 2D switch i,j */
    if(nBlock[b].dim==2)
    {  
      if ( (buf = (int **)malloc( ( nBlock[b].j) * sizeof(int *))) == NULL ) printf(" ERROR: malloc failed\n\n");
      for (j=0; j<nBlock[b].j; j++)
        if ( (buf[j] = (int *)malloc(( nBlock[b].i) * sizeof(int))) == NULL ) printf(" ERROR: malloc failed\n\n");

      n=0;
      for (j=0; j<nBlock[b].j; j++)
      {
        for (i=0; i<nBlock[b].i; i++)
        {
          buf[j][i]=nBlock[b].nod[n];
          n++;
        }
      }

      for (l=0; l<nBlock[b].dim; l++)
      {
        n=0;
        for (i=0; i<nBlock[b].i; i++)
        {
          for (j=0; j<nBlock[b].j; j++)
          {
            if(l==0) fprintf (handle1, "%le\n", node[buf[j][i]].nx);
            if(l==1) fprintf (handle1, "%le\n", node[buf[j][i]].ny);
            if(l==2) fprintf (handle1, "%le\n", node[buf[j][i]].nz);
            n++;
          }
        }
      }
      for (j=0; j<nBlock[b].j; j++) free(buf[j]); free(buf);
    }
    else
    {
      for (l=0; l<nBlock[b].dim; l++)
      {
        n=0;
        for (k=0; k<nBlock[b].k; k++)
        {
          for (j=0; j<nBlock[b].j; j++)
          {
            for (i=0; i<nBlock[b].i; i++)
            {
              if(l==0) fprintf (handle1, "%le\n", node[nBlock[b].nod[n]].nx);
              if(l==1) fprintf (handle1, "%le\n", node[nBlock[b].nod[n]].ny);
              if(l==2) fprintf (handle1, "%le\n", node[nBlock[b].nod[n]].nz);
              n++;
            }
  	  }
        }
      }

      /* fbd file */
      fprintf (handle3, "seto b%d\n",b+1); 
      n=0;
      for (k=0; k<nBlock[b].k; k++)
      {
        for (j=0; j<nBlock[b].j; j++)
        {
          for (i=0; i<nBlock[b].i; i++)
          {
            fprintf (handle3, "pnt %d %le %le %le\n",pn, node[nBlock[b].nod[n]].nx, node[nBlock[b].nod[n]].ny, node[nBlock[b].nod[n]].nz);
            pn++; n++;
          }
        }
      }
      fprintf (handle3, "setc b%d\n",b+1); 
    }
  }
  fclose(handle1);
  fclose(handle3);


  /* input file */
  if(nBlock[0].dim==2)
  {
    /* if 2D switch i,j */
    ic=1;
    jc=0;
    kc=2;
    fprintf (handle2, "'TWOD'       1\n" );
  }
  else
  {
    ic=0;
    jc=1;
    kc=2;
  }
  fprintf (handle2, "'GRID FORMATTED'       1\n" );
  fprintf (handle2, "'GRID'       1\n" );
  fprintf (handle2, "  %s.grd\n", name );

  /* connectivity */
  fprintf (handle2, "'CUT'    %d\n", anz->c/2);
  for (b=0; b<anz->b; b++)
  {
    if(nBlock[b].dim==2) edges=4;
    if(nBlock[b].dim==3) edges=6;
    for(i=0; i<edges; i++) if(nBlock[b].map[i][0]>-1)
    {
      /* block-borders appear twice, write only the couple were the neighbour-nr is higher */
      if( nBlock[b].neighbor[i]> nBlock[b].geo)
      {
        for(j=0; j<3; j++) if( nBlock[b].map[i][j]>3) m[j]=nBlock[b].map[i][j]-3; else m[j]=nBlock[b].map[i][j];

        fprintf(handle2,"  'b%db%d'   %4d   %4d %4d %4d   %4d %4d %4d\n", nBlock[b].geo+1, nBlock[b].neighbor[i]+1, nBlock[b].geo+1, nBlock[b].strt1[i][ic], nBlock[b].strt1[i][jc], nBlock[b].strt1[i][kc], nBlock[b].end_1[i][ic], nBlock[b].end_1[i][jc], nBlock[b].end_1[i][kc]);
        fprintf(handle2,"  'b%db%d'   %4d   %4d %4d %4d   %4d %4d %4d", nBlock[b].geo+1, nBlock[b].neighbor[i]+1, nBlock[b].neighbor[i]+1, nBlock[b].strt2[i][ic], nBlock[b].strt2[i][jc], nBlock[b].strt2[i][kc], nBlock[b].end_2[i][ic], nBlock[b].end_2[i][jc], nBlock[b].end_2[i][kc]);
        fprintf(handle2,"   %d %d %d\n", m[0],m[1],m[2]);
      }
    }
  }

  /* boundaries */
  for (b=0; b<anz->b; b++)
  {
    n=0; for(i=0; i<edges; i++) if(nBlock[b].map[i][0]<0 ) n++;

    if(1)
    {
      fprintf (handle2, "'BLOCK'    %d\n", nBlock[b].geo+1);
      fprintf (handle2, "  'DIMENSIONS'    1\n" );
      if(nBlock[b].dim==2) fprintf (handle2, "    %d %d 2\n", nBlock[b].j, nBlock[b].i);
      else                 fprintf (handle2, "    %d %d %d\n", nBlock[b].i, nBlock[b].j, nBlock[b].k);
      fprintf (handle2, "  'FLUX'    1\n" );
      fprintf (handle2, "    'ROE'    1\n" );
      if(nBlock[b].dim==2) fprintf (handle2, "  'VISCOUS'    2\n    1 2  \n" );
      else                 fprintf (handle2, "  'VISCOUS'    3\n    1 2 3\n" );
      if(nBlock[b].dim==2) fprintf (handle2, "  'BC'    %d\n", n+2 );
      else                 fprintf (handle2, "  'BC'    %d\n", n );  
      if(nBlock[b].dim==2) edges=4;
      else                 edges=6;
      for(i=0; i<edges; i++) if(nBlock[b].map[i][0]<0 )
      {
        fprintf(handle2,"    'BC'    %4d %4d %4d   %4d %4d %4d  '%s'\n", nBlock[b].strt1[i][ic], nBlock[b].strt1[i][jc], nBlock[b].strt1[i][kc], nBlock[b].end_1[i][ic], nBlock[b].end_1[i][jc], nBlock[b].end_1[i][kc], nBlock[b].bctype[i]);
      }

      /* if dim==2 set the top and bottom face */
      if(nBlock[b].dim==2)
      {
        fprintf(handle2,"    'BC'    %4d %4d %4d   %4d %4d %4d  'EXTRAPOLATE'\n",1,1,1,nBlock[b].j, nBlock[b].i, 1);
        fprintf(handle2,"    'BC'    %4d %4d %4d   %4d %4d %4d  'EXTRAPOLATE'\n",1,1,2,nBlock[b].j, nBlock[b].i, 2);
      }
      fprintf (handle2, "'END BLOCK'    %d\n", nBlock[b].geo+1 );
    }
  }

  fprintf (handle2, "'END'       0\n" );
  fclose(handle2);
  return (1);
}

