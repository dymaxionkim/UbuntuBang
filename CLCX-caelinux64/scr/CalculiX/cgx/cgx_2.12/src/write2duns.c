
#include <extUtil.h>

int write2duns( char *datout, Summen *anz, Nodes *node, Elements *elem, Datasets *lcase, NodeBlocks *nBlock, int bouNr, Dunsbou *dunsbou )
{
  FILE *handle1, *handle2, *handle3;
  int  i,j,k,n,b=0,edges=0;

  /* Open the files and check to see that it was opened correctly */
  i=strlen(datout);
  strcpy (datout, "duns.grid");
  handle1 = fopen (datout, "w+b");
  if (handle1==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n",
     datout); return(-1);}
  else  printf (" file %s opened\n",datout);

  strcpy (datout, "duns.conn");
  handle2 = fopen (datout, "w+b");
  if (handle2==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n",
     datout); return(-1);}
  else  printf (" file %s opened\n",datout);

  strcpy (datout, "duns.bou");
  handle3 = fopen (datout, "w+b");
  if (handle3==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n",
     datout); return(-1);}
  else  printf (" file %s opened\n",datout);

  printf (" write duns mesh \n");

  /* grid file */
  fprintf (handle1, "%d\n", anz->b);
  for (b=0; b<anz->b; b++)
  {
    if(nBlock[b].dim==2) fprintf (handle1, "%d %d\n", nBlock[b].i, nBlock[b].j);
    else                 fprintf (handle1, "%d %d %d\n", nBlock[b].i, nBlock[b].j, nBlock[b].k);
    n=0;
    for (k=0; k<nBlock[b].k; k++)
    {
      for (j=0; j<nBlock[b].j; j++)
      {
        for (i=0; i<nBlock[b].i; i++)
        {
          if(nBlock[b].dim==2) fprintf (handle1, "%le %le\n", node[nBlock[b].nod[n]].nx, node[nBlock[b].nod[n]].ny);
          else                 fprintf (handle1, "%le %le %le\n", node[nBlock[b].nod[n]].nx, node[nBlock[b].nod[n]].ny, node[nBlock[b].nod[n]].nz);
          n++;
	}
      }
    }
  }
  fclose(handle1);

  /* connectivity file */
  fprintf (handle2, "%3d       blocks\n", anz->b);
  for (b=0; b<anz->b; b++)
  {
    fprintf (handle2,"B %3d",nBlock[b].geo+1 );
    if(nBlock[b].dim==2) edges=4;
    if(nBlock[b].dim==3) edges=6;
    for(i=0; i<edges; i++)
    {
      if(nBlock[b].map[i][0]>-1) fprintf(handle2," b  0%3d %1d%1d%1d",  nBlock[b].neighbor[i]+1, nBlock[b].map[i][0], nBlock[b].map[i][1], nBlock[b].map[i][2]);
      else  fprintf(handle2," s %2d  0 000",  nBlock[b].neighbor[i]);
    }
    fprintf (handle2,"\n" );
  }
  fclose(handle2);

  /* boundary file */
  fprintf(handle3,"  boundary-conditions \n");
  for (b=0; b<bouNr; b++)
  {
    fprintf(handle3,"#\n# set:%s\n", dunsbou[b].name);
    fprintf(handle3,"  surface ");
    for(i=0; i<dunsbou[b].surfs; i++) fprintf(handle3,"%d ", dunsbou[b].surf[i]);
    fprintf(handle3,"\n    %s\n", dunsbou[b].bctype);
  }
  fclose(handle3);

  return (1);
}

