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
 */

#include <cgx.h>
#include <time.h>

extern char **environ;


extern int   basCol[3];                     /* color indexes due to basic colormap */
extern int   width_ini, height_ini; /* Grafik-Fensterbreite/hoehe */
extern int   width_menu, height_menu;
extern int   w0, w1, w_index, w_rgb, activWindow;
extern int   width_w0, height_w0;
extern int   width_w1, height_w1;
extern int   MouseMode;                                   /* status maustasten */
extern double dtx, dty, dtz, drx, dry, drz, ds;                 /* Verschiebungen */
extern double vmem[4];                                     /* kor. bis auswahl eines neuen drehpkts */
extern GLdouble R[4][4];                                   /* Rotationsmatrix */
extern char  surfFlag;                /* zeichne nur Oberflaechenelemente (1), sonst (0)*/
extern char  modelEdgeFlag;                /* zeichne mit Modell-Ecken (1), sonst (0)*/
extern char  frameFlag;                   /* mit (1) oder ohne Rahmen um das Grafikfenster */
extern char  filenamFlag;                   /* mit (1) oder ohne filename im Menufenster */
extern char  textFlag;                   /* mit (1) oder ohne text im Menufenster */
extern char  printFlag;                     /* printf 1:on 0:off */
extern char  frameFlag;               /* mit (1) oder ohne Rahmen um das Grafigfenster */
extern char  scalaFlag;                 /* mit (1) oder ohne scala und wertetexte */ 
extern double dx ,dy;                                      /* Mauskoordinaten */
extern int   steps;                   /* Schrittweite der Farbscala, und NURB aufloesung */
extern double gtol;
extern char  picture_text[MAX_LINE_LENGTH], datin[MAX_LINE_LENGTH];
extern char  psviewer[MAX_LINE_LENGTH];
extern char  delPntFlag;                    /* 1: deleted points exists */
extern char  delLineFlag;                   /* 1: deleted lines exists */
extern char  delLcmbFlag;                   /* 1: deleted lcmbs exists */
extern char  delSurfFlag;                   /* 1: deleted surfs exists */
extern char  delBodyFlag;                   /* 1: deleted bodys exists */
extern char  inpformat;                     /* defines the start-up mode of cgx */

extern Scale     scale[1];
extern Summen    anz[1];
extern Edges     *edge;
extern Nodes     *node;
extern Elements  *e_enqire;
extern Datasets *lcase;

extern Alias     *alias;
extern Sets      *set;
extern Psets     *pset;
extern Points    *point;
extern Lines     *line;
extern Lcmb      *lcmb;
extern Gsur      *surf;
extern Gbod      *body;
extern Nurbs     *nurbs;
extern Materials *material; 
extern Amplitudes *amplitude; 
extern SumGeo    anzGeo[1];
extern SumAsci   sumAsci[1];

/* for the sequence */
extern char  sequenceFlag;                  /* 1: play a sequence of datasets */
extern int   cur_entity;                               /* aktiv value */
extern char  entityName[13][MAX_LINE_LENGTH];              /* Bezeichnung des aktiven values */
extern int       pre_lc;                                      /* pre-selected Dataset, active after entity is selected */
extern DsSequence dsSequence;                 /* datasets (ds) for sequence of results */

/* the copied node-sets which have to be filled with values from new loaded Datasets */
extern CopiedNodeSets copiedNodeSets[1];

extern char  addDispFlag;                    /* 0: original node-coordinates, 1: node-coordinates+displacements */
extern SpecialSet specialset[1];

int graph_Nr=-1;


void length2D(int setNr, int dsNr, int entity )
{
  FILE *handle_out, *handle_gnu;
  int    n;
  double dx,dy,dz, sum_l=0.;

  char  buffer[MAX_LINE_LENGTH];
  graph_Nr++;

  printf("ds:%d entity:%d\n", dsNr, entity);


  /* create special purpose plotfile, one line per node */

  sprintf(buffer, "graph_%d.out", graph_Nr);
  handle_out = fopen (buffer, "w+b" );
  if (handle_out==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n", buffer); return;}
  fprintf (handle_out, "# node length dat(ds)\n");

  fprintf ( handle_out, "%-10d %-12.5e %-12.5e\n", set[setNr].node[0], sum_l* scale->w, lcase[dsNr].dat[entity][set[setNr].node[0]]);
  for (n=1; n<set[setNr].anz_n; n++)
  {
    dx=node[set[setNr].node[n]].nx - node[set[setNr].node[n-1]].nx;
    dy=node[set[setNr].node[n]].ny - node[set[setNr].node[n-1]].ny;
    dz=node[set[setNr].node[n]].nz - node[set[setNr].node[n-1]].nz;
    sum_l+=sqrt(dx*dx+dy*dy+dz*dz);

    fprintf ( handle_out, "%-10d %-12.5e %-12.5e\n", set[setNr].node[n], sum_l* scale->w, lcase[dsNr].dat[entity][set[setNr].node[n]] );
  }
  fclose(handle_out);

  /* gnuplot-command-file  */

  sprintf(buffer, "graph_%d.gnu", graph_Nr);
  handle_gnu = fopen (buffer, "w+b" );
  if (handle_gnu==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n",buffer); return;}
#ifdef WIN32
  fprintf(handle_gnu, "set term png\n");
  sprintf(buffer, "graph_%d.png", graph_Nr);
  fprintf(handle_gnu, "set out \"%s\"\n", buffer);
  fprintf(handle_gnu, "set grid\n");
  fprintf(handle_gnu, "set title \"Values at Nodes\"\n");
#else
  fprintf(handle_gnu, "set term postscript landscape monochrom  \n");
  fprintf(handle_gnu, "#set term x11 \n");
  sprintf(buffer, "graph_%d.ps", graph_Nr);
  fprintf(handle_gnu, "set out \"%s\"\n", buffer);
  fprintf(handle_gnu, "set grid\n");
  fprintf(handle_gnu, "set title \"Values at Nodes (%s)\"\n", datin);
#endif
  fprintf(handle_gnu, "set ylabel \" %s \"\n", lcase[dsNr].compName[entity]);

  sprintf(buffer, "graph_%d.out", graph_Nr);
  fprintf(handle_gnu, "set xlabel \" Length \"\n");
  fprintf(handle_gnu, "plot ");
  fprintf(handle_gnu, "\"%s\" using 2:3 title 'Setname %s' with linespoints\n", buffer, set[setNr].name );

  fclose(handle_gnu);


#ifdef WIN32
  sprintf(buffer, "graph_%d.png", graph_Nr);
  printf ("write %s \n", buffer);
  sprintf(buffer, "gnuplot graph_%d.gnu", graph_Nr );
  system (buffer);
// if(inpformat) sprintf(buffer, "%s graph_%d.png &", psviewer, graph_Nr );
  if(inpformat) sprintf(buffer, "graph_%d.png &", graph_Nr );
  system (buffer);
#else
  sprintf(buffer, "graph_%d.ps", graph_Nr);
  printf ("write %s \n", buffer);
  sprintf(buffer, "gnuplot graph_%d.gnu", graph_Nr );
  system (buffer);
  if(inpformat) sprintf(buffer, "%s graph_%d.ps &", psviewer, graph_Nr );
  system (buffer);
#endif
  printf ("ready \n");
}



void param2D(char *par1, int *dsNr, char *par2 )
{
  FILE *handle_out, *handle_gnu;
  int i,j, lc, anz_lc=0, anz_val=0;
  static double *value1=NULL, *value[6]={NULL,NULL,NULL,NULL,NULL,NULL};
  char  buffer[MAX_LINE_LENGTH];

  graph_Nr++;

  printf (" please wait, loading data\n");
  for(i=1; i<=dsNr[0]; i++)
  {
    lc=dsNr[i];
    printf("ds[%d]:%d\n",i, dsNr[i]+1);

    if((value1=(double *)realloc(value1, (anz_lc+2) *sizeof(double)))==NULL )
      printf("\n\n ERROR: realloc failed \n\n") ;
    for(j=0; j<6; j++)
      if((value[j]=(double *)realloc(value[j], (anz_lc+2) *sizeof(double)))==NULL )
        printf("\n\n ERROR: realloc failed \n\n") ;

    /* determine par1 */
    for(j=0; j<lcase[lc].npheader; j++)
    {
      sscanf(lcase[lc].pheader[j],"%s %lf",buffer, &value1[anz_lc]); 
      if(compareStrings(&buffer[2],par1)>0) break;
    }
    if(j<lcase[lc].npheader);
    else if(compare(par1, "time",1)==1) value1[anz_lc]    =  lcase[lc].value;
    else if(compare(par1, "desc",1)==1) value1[anz_lc]    =  atof(lcase[lc].dataset_text);
    else if(compare(par1, "step",1)==1) value1[anz_lc]    =  lc+1;
    else if(compare(par1, "nr",1)==1) value1[anz_lc]    =  lc+1;
    else
    {
      printf(" ERROR: could not find parameter:%s\n", par1);
      return;
    }

    /* determine par2 */
    for(j=0; j<lcase[lc].npheader; j++)
    {
      anz_val=sscanf(lcase[lc].pheader[j],"%s %lf %lf %lf %lf %lf %lf",buffer, &value[0][anz_lc], &value[1][anz_lc], &value[2][anz_lc], &value[3][anz_lc], &value[4][anz_lc], &value[5][anz_lc]);
      if(compareStrings(&buffer[2],par2)>0) break;
    }
    if(j<lcase[lc].npheader) anz_val--;
    else if(compare(par2, "time",1)==1) { value[0][anz_lc]    =  lcase[lc].value; anz_val=1; }
    else if(compare(par2, "desc",1)==1) { value[0][anz_lc]    =  atof(lcase[lc].dataset_text); anz_val=1; }
    else if(compare(par2, "step",1)==1) { value[0][anz_lc]    =  lc+1; anz_val=1; }
    else if(compare(par2, "nr",1)==1) { value[0][anz_lc]    =  lc+1; anz_val=1; }
    else
    {
      printf(" ERROR: could not find parameter:%s\n", par2);
      return;
    }
 
    anz_lc++;
  }

  /* create plotfile, one line per Dataset */

  sprintf(buffer, "graph_%d.out", graph_Nr);
  handle_out = fopen (buffer, "w+b" );
  if (handle_out==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n",
     buffer); return;}
  for (lc=0; lc<anz_lc; lc++)
  {
    fprintf (handle_out, "%12.5e", value1[lc]);
    for(i=0; i<anz_val; i++)  fprintf (handle_out, " %12.5e", value[i][lc] );
    fprintf (handle_out, "\n");
  }
  fclose(handle_out);


  /* gnuplot-command-file  */

  sprintf(buffer, "graph_%d.gnu", graph_Nr);
  handle_gnu = fopen (buffer, "w+b" );
  if (handle_gnu==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n",
     buffer); return;}
#ifdef WIN32
  fprintf(handle_gnu, "set term png\n");
  sprintf(buffer, "graph_%d.png", graph_Nr);
  fprintf(handle_gnu, "set out \"%s\"\n", buffer);
  fprintf(handle_gnu, "set grid\n");
//  fprintf(handle_gnu, "set title \"File:%s\"\n", datin);
#else
  fprintf(handle_gnu, "set term postscript landscape monochrom  \n");
  fprintf(handle_gnu, "#set term x11 \n");
  sprintf(buffer, "graph_%d.ps", graph_Nr);
  fprintf(handle_gnu, "set out \"%s\"\n", buffer);
  fprintf(handle_gnu, "set grid\n");
  fprintf(handle_gnu, "set title \"File:%s\"\n", datin);
#endif
  fprintf(handle_gnu, "set xlabel \" %s \"\n", par1);
  fprintf(handle_gnu, "set ylabel \" %s \"\n", par2);

  sprintf(buffer, "graph_%d.out", graph_Nr);

  fprintf(handle_gnu, "plot ");
  for (i=0; i<anz_val-1; i++)
  {
    fprintf(handle_gnu, "\"%s\" using 1:%d title 'val%d' with linespoints, ", buffer, i+2, i+1);
  }
  fprintf(handle_gnu, "\"%s\" using 1:%d title 'val%d' with linespoints\n", buffer, i+2, i+1);
  fprintf(handle_gnu, "\n");
  fclose(handle_gnu);

#ifdef WIN32
  sprintf(buffer, "graph_%d.png", graph_Nr);
  printf ("write %s \n", buffer);
  sprintf(buffer, "gnuplot graph_%d.gnu", graph_Nr );
  system (buffer);
// if(inpformat)  sprintf(buffer, "%s graph_%d.png &", psviewer, graph_Nr );
  if(inpformat) sprintf(buffer, "graph_%d.png &", graph_Nr );
  system (buffer);
#else
  sprintf(buffer, "graph_%d.ps", graph_Nr);
  printf ("write %s \n", buffer);
  sprintf(buffer, "gnuplot graph_%d.gnu", graph_Nr );
  system (buffer);
  if(inpformat) sprintf(buffer, "%s graph_%d.ps &", psviewer, graph_Nr );
  system (buffer);
#endif

  printf ("ready \n");
}


void plot2D(char *type, int setNr, int *dsNr, int entity )
{
  FILE *handle_out, *handle_gnu;
  int i,j, anz_lc=0, n, lc, settmp, setcut, ncomps=0;
  time_t seconds;
  struct tm *date;

  double **dat, *lc_time=NULL, *vptr;
  long *offset=NULL;
  char  buffer[MAX_LINE_LENGTH], **lc_description=NULL;
  char  path[MAX_LINE_LENGTH];
  char  name[MAX_LINE_LENGTH];
  char  comp[MAX_LINE_LENGTH];
  extern Qcut_nodes *qcut_nod;
  extern SpecialSet specialset[1];

  graph_Nr++;

  if((dat=(double **)malloc((int)set[setNr].anz_n *sizeof(double *)))==NULL )
  {
    printf("\n\n ERROR: malloc failed \n\n") ;
  }
  for (i=0; i<set[setNr].anz_n; i++)
  {
    if((dat[i]=(double *)malloc((int)anz->l *sizeof(double)))==NULL )
      printf("\n\n ERROR: malloc failed \n\n") ;
  }
  
  /* compile the lc-data */
  anz_lc=0;
  setcut=getSetNr("-qcut");
  printf (" please wait, loading data\n");
  for(i=1; i<=dsNr[0]; i++)
  {
    lc=dsNr[i];
    printf("ds[%d]:%d\n",i, dsNr[i]+1);

    if((lc_time=(double *)realloc(lc_time, (anz_lc+2) *sizeof(double)))==NULL )
      printf("\n\n ERROR: realloc failed \n\n") ;

    /* determine type */
    for(j=0; j<lcase[lc].npheader; j++)
    {
      sscanf(lcase[lc].pheader[j],"%s %lf",buffer, &lc_time[anz_lc]);
      if(compareStrings(&buffer[2],type)>0) break;
    }
    if(j<lcase[lc].npheader);
    else if((type[0]=='s')||(type[0]=='n')||(type[0]=='t')||(type[0]=='d')) lc_time[anz_lc]    =  lcase[lc].value;
    else
    {
      printf(" ERROR: could not find parameter:%s\n", type);
      return;
    }

    if ( ( lc_description = (char **)realloc(lc_description, (anz_lc+2) * sizeof(char *))) == NULL )
      printf("\n\n ERROR: realloc failure\n\n" );
    if ( ( lc_description[anz_lc] = (char *)malloc((MAX_LINE_LENGTH) * sizeof(char))) == NULL )
      printf("\n\n ERROR: realloc failure\n\n" );
    strcpy(lc_description[anz_lc], lcase[lc].dataset_text);

    /* check if the data of the specified lcase (Dataset) are already available */
    if (!lcase[lc].loaded)
    {
      if ( (lcase[lc].dat = (float **)malloc( (lcase[lc].ncomps) * sizeof(float *))) == NULL )
        printf("\n\n ERROR: malloc failure\n\n" );
      for(j=0; j<(lcase[lc].ncomps); j++)
      {
        if ( (lcase[lc].dat[j] = (float *)malloc( (anz->nmax+1) * sizeof(float))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );	               
        for(n=0; n<=anz->nmax; n++) lcase[lc].dat[j][n]=0.;
      }
      lcase[lc].loaded=1;

      if(setcut>-1)
      {
        delSet(specialset->tmp);
        if( (settmp=pre_seta( specialset->tmp, "i", 0 )) <0 ) return;

        for ( n=0; n<set[setNr].anz_n; n++) seta( settmp, "n", set[setNr].node[n]  );
        for ( n=0; n<set[setcut].anz_n; n++) { setr( settmp, "n", qcut_nod[n].nr  ); }
        for ( n=0; n<set[setcut].anz_n; n++) { seta( settmp, "n", qcut_nod[n].n1  ); seta( settmp, "n", qcut_nod[n].n2  ); }
        if(offset==NULL)
        {
          if((offset=(long *)calloc((long)set[settmp].anz_n+2, sizeof(long)))==NULL )
          {
            printf("\n\n ERROR: calloc failed \n\n") ;
          }
        }
        for ( n=0; n<set[settmp].anz_n; n++)
        {
          if(readOneNode( lc, anz, lcase, set[settmp].node[n], &vptr, &offset[n] )==-1)
            printf(" ERROR: readOneNode could not find node:%d\n", set[settmp].node[n]);
          else
	  {
            for(j=0; j<lcase[lc].ncomps; j++)  lcase[lc].dat[j][set[settmp].node[n]] = vptr[j] ;
            free(vptr);
	  }
          if(offset[n+1]==0) offset[n+1]=offset[n];
        }
        ncomps=lcase[lc].ncomps;
        calcDatasets( lc, anz, node, lcase );
    
        /* if currently a section (qcut) is in use realloc the lcase and generate the necessary values */
        updLcase(lc, setcut);
      }
      else
      {
        if(offset==NULL)
        {
          if((offset=(long *)calloc((long)set[setNr].anz_n+2, sizeof(long)))==NULL )
          {
            printf("\n\n ERROR: calloc failed \n\n") ;
          }
        }
        for ( n=0; n<set[setNr].anz_n; n++)
        {
          if(readOneNode( lc, anz, lcase, set[setNr].node[n], &vptr, &offset[n] )==-1)
            printf(" ERROR: readOneNode could not find node:%d\n", set[setNr].node[n]);
          else
	  {
            for(j=0; j<lcase[lc].ncomps; j++)  lcase[lc].dat[j][set[setNr].node[n]] = vptr[j] ;
            free(vptr);
	  }
          if(offset[n+1]==0) offset[n+1]=offset[n];
        }
        ncomps=lcase[lc].ncomps;
        calcDatasets( lc, anz, node, lcase );
      }
      if(entity>lcase[lc].ncomps) { printf(" ERROR: entity-nr:%d not known in ds:%d\n", entity+1,lc+1 ); return; }
      for ( n=0; n<set[setNr].anz_n; n++) dat[n][anz_lc] =  lcase[lc].dat[entity][set[setNr].node[n]] ;
/*
      for ( n=0; n<set[setNr].anz_n; n++)
      {   
        printf("lcb %d, node:%d, val:%f  ", anz_lc, set[setNr].node[n], dat[n][anz_lc]);
      } printf("\n");
*/    
      /* free the datasets which were just additionally loaded to save space */
      if(printFlag) printf("free :%d\n", lc+1);
      for(j=0; j<(lcase[lc].ncomps); j++) free(lcase[lc].dat[j]);
      free(lcase[lc].dat);
      lcase[lc].dat=NULL;
      lcase[lc].loaded=0;
      lcase[lc].ncomps=ncomps;
    }
    else
    {
      /* if currently a section (qcut) is in use realloc the lcase and generate the necessary values */
      if(setcut>-1)  updLcase(lc, setcut);
    
      if(entity>lcase[lc].ncomps) { printf(" ERROR: entity-nr:%d not known in ds:%d\n", entity+1,lc+1 ); return; }
      for ( n=0; n<set[setNr].anz_n; n++) dat[n][anz_lc] =  lcase[lc].dat[entity][set[setNr].node[n]] ;
/*
      for ( n=0; n<set[setNr].anz_n; n++)
      {
        printf("lca %d, node:%d, val:%f  ", anz_lc, set[setNr].node[n], dat[n][anz_lc]);
      } printf("\n");
*/
    }    
 
    anz_lc++;
  }
  if(offset!=NULL) free(offset);


  /* create special purpose plotfile1 for the cad department */

  /* skip the trailing blanks */
  j=0;
  while((lcase[dsNr[1]].name[j]!=' ')&&(lcase[dsNr[1]].name[j]!='\0'))
  {  name[j]=lcase[dsNr[1]].name[j]; j++; }
  name[j]='\0';
  j=0;
  while((lcase[dsNr[1]].compName[entity][j]!=' ')&&(lcase[dsNr[1]].compName[entity][j]!='/')&&(lcase[dsNr[1]].compName[entity][j]!='\0'))
  {  comp[j]=lcase[dsNr[1]].compName[entity][j]; j++; }
  comp[j]='\0';
  sprintf(buffer, "graph_%s_%s_%s.out", set[setNr].name, name, comp);
  handle_out = fopen (buffer, "w+b" );
  if (handle_out==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n",
     buffer); return;}

  /* get the actual path */
  path[0]='\0';
  i=0; while(environ[i]!=0)
  {
    if(compare(environ[i],"PWD",3)==3) { strcpy(path,&environ[i][4]); break; }
    i++;
  }

  /* get the date */
  time(&seconds);
  date = gmtime(&seconds);

  /* write the header and data */
  fprintf (handle_out, "HEADER\n");
  fprintf (handle_out, "FILENAME:      %s/%s\n", path, datin);
  fprintf (handle_out, "DATE:          %d.%d.%d\n", date->tm_mday, date->tm_mon+1, date->tm_year+1900);
  fprintf (handle_out, "DATASET-NAME:  %s\n", lcase[dsNr[1]].name);
  fprintf (handle_out, "DATASET-ENTITY:%s\n", lcase[dsNr[1]].compName[entity]);
  fprintf (handle_out, "--------------------------------------------------------\n\n");
  if((type[0]=='N')||(type[0]=='n'))
    fprintf (handle_out, "DATASET INCREMENT / NODE\n");
  else if((type[0]=='T')||(type[0]=='t'))
    fprintf (handle_out, "TIME INCREMENT / NODE\n");
  else if((type[0]=='D')||(type[0]=='d'))
    fprintf (handle_out, "DESCRIPTION INCREMENT / NODE\n");
  else
  {
    fprintf (handle_out, "PARAMETER:%s / NODE\n", type);
  }
  fprintf (handle_out, "              ");
  for ( n=0; n<set[setNr].anz_n; n++)
    fprintf (handle_out, "; %13d ", set[setNr].node[n]);
  fprintf (handle_out, "\n");
  for (lc=0; lc<anz_lc; lc++)
  {
    fprintf (handle_out, "% -.6e ", lc_time[lc]);
    for ( n=0; n<set[setNr].anz_n; n++)
      fprintf (handle_out, "; % -.6e ", dat[n][lc]);
    fprintf (handle_out, "\n");
  }
  fclose(handle_out);


  /* create plotfile, one line per Dataset */

  sprintf(buffer, "graph_%d.out", graph_Nr);
  handle_out = fopen (buffer, "w+b" );
  if (handle_out==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n",
     buffer); return;}
  for (lc=0; lc<anz_lc; lc++)
  {
    //fprintf (handle_out, "%-5d %-12.5e %f ", lc+1, lc_time[lc], atof(lc_description[lc]));
    fprintf (handle_out, "%-5d %-5d %-12.5e %f ", lc+1, dsNr[lc+1], lc_time[lc], atof(lc_description[lc]));
    for ( n=0; n<set[setNr].anz_n; n++)
      fprintf (handle_out, "%-12.5e ", dat[n][lc]);
    fprintf (handle_out, "\n");
  }
  fclose(handle_out);


  /* create special purpose plotfile2, one line per node */

  sprintf(buffer, "graph_%d.out2", graph_Nr);
  handle_out = fopen (buffer, "w+b" );
  if (handle_out==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n",
     buffer); return;}
  fprintf (handle_out, "# node    x   y   z   dat(ds1)  dat(ds2) .. for the same entity-nr\n");
  for (n=0; n<set[setNr].anz_n; n++)
  {
    fprintf (handle_out, "%-10d %-12.5e %-12.5e %-12.5e", set[setNr].node[n], node[set[setNr].node[n]].nx* scale->w+scale->x, node[set[setNr].node[n]].ny* scale->w+scale->y, node[set[setNr].node[n]].nz* scale->w+scale->z );
    for (lc=0; lc<anz_lc; lc++)
      fprintf ( handle_out, " %-12.5e", dat[n][lc]);
    fprintf (handle_out, "\n");
  }
  fclose(handle_out);

  /* gnuplot-command-file  */

  sprintf(buffer, "graph_%d.gnu", graph_Nr);
  handle_gnu = fopen (buffer, "w+b" );
  if (handle_gnu==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n",
     buffer); return;}
#ifdef WIN32
  fprintf(handle_gnu, "set term png\n");
  sprintf(buffer, "graph_%d.png", graph_Nr);
  fprintf(handle_gnu, "set out \"%s\"\n", buffer);
  fprintf(handle_gnu, "set title \"Values at Nodes\"\n");
#else
  fprintf(handle_gnu, "set term postscript landscape monochrom  \n");
  fprintf(handle_gnu, "#set term x11 \n");
  sprintf(buffer, "graph_%d.ps", graph_Nr);
  fprintf(handle_gnu, "set out \"%s\"\n", buffer);
  fprintf(handle_gnu, "set title \"Values at Nodes (%s)\"\n", datin);
#endif
  fprintf(handle_gnu, "set grid\n");
  fprintf(handle_gnu, "set ylabel \" %s \"\n", lcase[dsNr[1]].compName[entity]);

  sprintf(buffer, "graph_%d.out", graph_Nr);
  if((type[0]=='S')||(type[0]=='s'))
  {
    fprintf(handle_gnu, "set xlabel \" Dataset \"\n");
    fprintf(handle_gnu, "plot ");
    for (n=0; n<set[setNr].anz_n-1; n++)
    {
      fprintf(handle_gnu, "\"%s\" using 1:%d title 'Node=%d' with linespoints, ",
            buffer, n+5, set[setNr].node[n]);
    }
    fprintf(handle_gnu, "\"%s\" using 1:%d title 'Node=%d' with linespoints\n",
            buffer, n+5, set[setNr].node[n]);
  }
  else if((type[0]=='N')||(type[0]=='n'))
  {
    fprintf(handle_gnu, "set xlabel \" Dataset \"\n");
    fprintf(handle_gnu, "plot ");
    for (n=0; n<set[setNr].anz_n-1; n++)
    {
      fprintf(handle_gnu, "\"%s\" using 2:%d title 'Node=%d' with linespoints, ",
            buffer, n+5, set[setNr].node[n]);
    }
    fprintf(handle_gnu, "\"%s\" using 2:%d title 'Node=%d' with linespoints\n",
            buffer, n+5, set[setNr].node[n]);
  }
  else if((type[0]=='T')||(type[0]=='t'))
  {
    fprintf(handle_gnu, "set xlabel \" Time \"\n");
    fprintf(handle_gnu, "plot ");
    for (n=0; n<set[setNr].anz_n-1; n++)
    {
      fprintf(handle_gnu, "\"%s\" using 3:%d title 'Node=%d' with linespoints, ",
            buffer, n+5, set[setNr].node[n]);
    }
    fprintf(handle_gnu, "\"%s\" using 3:%d title 'Node=%d' with linespoints\n ",
            buffer, n+5, set[setNr].node[n]);
  }
  else if((type[0]=='D')||(type[0]=='d'))
  {
    fprintf(handle_gnu, "set xlabel \" Description \"\n");
    fprintf(handle_gnu, "plot ");
    for (n=0; n<set[setNr].anz_n-1; n++)
    {
      fprintf(handle_gnu, "\"%s\" using 4:%d title 'Node=%d' with linespoints, ",
            buffer, n+5, set[setNr].node[n]);
    }
    fprintf(handle_gnu, "\"%s\" using 4:%d title 'Node=%d' with linespoints\n ",
            buffer, n+5, set[setNr].node[n]);
  }
  else
  {
    fprintf(handle_gnu, "set xlabel \" %s \"\n", type);
    fprintf(handle_gnu, "plot ");
    for (n=0; n<set[setNr].anz_n-1; n++)
    {
      fprintf(handle_gnu, "\"%s\" using 3:%d title 'Node=%d' with linespoints, ",
            buffer, n+5, set[setNr].node[n]);
    }
    fprintf(handle_gnu, "\"%s\" using 3:%d title 'Node=%d' with linespoints\n ",
            buffer, n+5, set[setNr].node[n]);
  }

  fclose(handle_gnu);

#ifdef WIN32
  sprintf(buffer, "graph_%d.png", graph_Nr);
  printf ("write %s \n", buffer);
  sprintf(buffer, "gnuplot graph_%d.gnu", graph_Nr );
  system (buffer);
// if(inpformat)  sprintf(buffer, "%s graph_%d.png &", psviewer, graph_Nr );
  if(inpformat) sprintf(buffer, "graph_%d.png &", graph_Nr );
  system (buffer);
#else
  sprintf(buffer, "graph_%d.ps", graph_Nr);
  printf ("write %s \n", buffer);
  sprintf(buffer, "gnuplot graph_%d.gnu", graph_Nr );
  system (buffer);
  if(inpformat) sprintf(buffer, "%s graph_%d.ps &", psviewer, graph_Nr );
  system (buffer);
#endif

  for (i=0; i<set[setNr].anz_n; i++)  free(dat[i]);
  free(dat);

  printf ("ready \n");
}



void graph( char *record)
{
  int  i,j,n,c,l,length=0, indx=0, nr, sum;
  int lc, *dsNr=NULL, anz_lc=0, eNr=-1, seq=-1;
  int lcmin=0, lcmax=0;
  char name[MAX_LINE_LENGTH], type[MAX_LINE_LENGTH];
  char dataset[MAX_LINE_LENGTH], entity[MAX_LINE_LENGTH];
  char  buffer[MAX_LINE_LENGTH], **dat;
  char addDispFlagLocal=0, zapFlag=0;

  int   copyset, copyseq, shellset, maxn=0, minn=0, jj;
  double nref[3], vnrefn[3], dist, maxdist, mindist;
  double vn1[3],vn2[3]; 
  
  FILE *handle_out, *handle_gnu;

  dataset[0]=entity[0]=0;
  length=sscanf( record, "%s%s%s%s%d%d", name, type, dataset, entity, &lcmin, &lcmax );
  lcmin--; lcmax--;
  if(lcmax<0) lcmax=anz->olc-1;
  if(lcmax>anz->olc) lcmax=anz->olc-1;
  if(lcmin<0) lcmin=0;
  if(lcmin>lcmax) lcmin=lcmax;

  /* check if a sequence with mapped values has to be generated */
  if (compare(type, "length", 1)==1)
  {
    if(!dataset[0]) type[0]='+';
    else strcpy(type, dataset);
    operateAlias( name, "se" );
    indx=getSetNr( name );
    if (indx<0)
    {
      errMsg(" ERROR: Set (%s) is undefined\n", name );
      return;
    }
    strcpy(buffer,name);

    /* when node coordinates were changed to the deformed ones then switch back before they are copied and then switch again */ 
    if(addDispFlag)
    {
      addDispToCoordinates(node);
      // remember to switch back
      addDispFlagLocal=2;
    }

    // when a lcmb is referenced, use this, else use the 1st line
    if(set[indx].anz_c>0)
    {
      /* open a node sequence */
      /* generate a name based on the setname */
      sprintf(name,"N%s",buffer);
      seq=getSetNr(name);
      if(seq>-1) getNewName(name,"se");
      seq=pre_seta(name,"is",0);
      zapFlag=1;
      c= set[indx].lcmb[0];
      for (i=0; i<lcmb[c].nl; i++)
      {
        if(type[0]=='+') j=i; else j=lcmb[c].nl-1-i;
        l=lcmb[c].l[j];
        if(lcmb[c].o[j]==type[0])
	{
          for (n=0; n<line[l].nip; n+=3)
          {
            nod(anz, &node, 0, anz->nmax+1, line[l].ip[n], line[l].ip[n+1], line[l].ip[n+2], 0);
            seta(seq,"n",anz->nmax);
	  }
        }
        else
	{
          for (n=line[l].nip-3; n>=0; n-=3)
          {
            nod(anz, &node, 0, anz->nmax+1, line[l].ip[n], line[l].ip[n+1], line[l].ip[n+2], 0);
            seta(seq,"n",anz->nmax);
	  }
        }
      }

      /* when node coordinates were changed to the deformed ones then switch back before they are copied and then switch again */ 
      if(addDispFlagLocal==2)
      {
        addDispToCoordinates(node);
      }
      // map dataset on new node
      sprintf(buffer,"ds%d",pre_lc+1); 
      j=getSetNr( "all");
      if(j<0) printf("ERROR:set all does not exist\n");
      n=interpol3d(seq, j, "vol", buffer, 0);
      if(n>0)
      {
        /* use only shell elements */
        delSet("-shellelems");
        shellset=pre_seta("-shellelems","i",0);
        for(i=0; i<anz->e; i++)
	{
	  if((e_enqire[e_enqire[i].nr].type == 7 )||
	     (e_enqire[e_enqire[i].nr].type == 8 )||
	     (e_enqire[e_enqire[i].nr].type == 9 )||
	     (e_enqire[e_enqire[i].nr].type == 10)) seta(shellset,"e",e_enqire[i].nr);
	}
        if(set[shellset].anz_e)
	{
          areampc(n, j, "map", "surf", buffer, 0, 0, node, 0);
          zap(specialset->nompc);
	}
        else zap(set[n].name);
	delSet("-shellelems");
      }
    }
    else if (set[indx].anz_l>0)
    {
      /* open a node sequence */
      /* generate a name based on the setname */
      sprintf(name,"N%s",buffer);
      seq=getSetNr(name);
      if(seq>-1) getNewName(name,"se");
      seq=pre_seta(name,"is",0);
      zapFlag=1;
      l=set[indx].line[0];
      if(type[0]=='+')
      {
        for (n=0; n<line[l].nip; n+=3)
        {
          nod(anz, &node, 0, anz->nmax+1, line[l].ip[n], line[l].ip[n+1], line[l].ip[n+2], 0);
          seta(seq,"n",anz->nmax);
        }
      }
      else
      {
        for (n=line[l].nip-3; n>=0; n-=3)
        {
          nod(anz, &node, 0, anz->nmax+1, line[l].ip[n], line[l].ip[n+1], line[l].ip[n+2], 0);
          seta(seq,"n",anz->nmax);
        }
      }

      /* when node coordinates were changed to the deformed ones then switch back before they are copied and then switch again */ 
      if(addDispFlagLocal==2)
      {
        addDispToCoordinates(node);
      }
      // map dataset on new node
      sprintf(buffer,"ds%d",pre_lc+1); 
      j=getSetNr( "all");
      if(j<0) printf("ERROR:set all does not exist\n");
      n=interpol3d(seq, j, "vol", buffer, 0);
      if(n>0)
      {
        /* use only shell elements */
        delSet("-shellelems");
        shellset=pre_seta("-shellelems","i",0);
        for(i=0; i<anz->e; i++)
	{
	  if((e_enqire[e_enqire[i].nr].type == 7 )||
	     (e_enqire[e_enqire[i].nr].type == 8 )||
	     (e_enqire[e_enqire[i].nr].type == 9 )||
	     (e_enqire[e_enqire[i].nr].type == 10)) seta(shellset,"e",e_enqire[i].nr);
	}
        if(set[shellset].anz_e)
	{
          areampc(n, j, "map", "surf", buffer, 0, 0, node, 0);
          zap(specialset->nompc);
	}
        else zap(set[n].name);
	delSet("-shellelems");
      }
    }
    else if (set[indx].anz_n>0)
    {
      /* open a node set */
      /* generate a name based on the setname */
      sprintf(name,"N%s",buffer);
      seq=getSetNr(name);
      if(seq>-1) getNewName(name,"se");
      seq=pre_seta(name,"is",0);

      /* create a copy of the nodeset */
      delSet("-tmp");
      copyset=pre_seta("-tmp","i",0);
      delSet("-seq");
      copyseq=pre_seta("-seq","is",0);

      /* order the nodes */
      /* the first node is the node with the greatest dist to its CG */
      /* calc CG */
      for (j=0; j<3; j++)  nref[j]=0;
      for (j=0; j<set[indx].anz_n; j++)
      {
        n=set[indx].node[j];
        seta(copyset,"n",n);
        nref[0]+=node[n].nx;
        nref[1]+=node[n].ny;
        nref[2]+=node[n].nz;
      }
      nref[0]/=set[indx].anz_n;
      nref[1]/=set[indx].anz_n;
      nref[2]/=set[indx].anz_n;
      maxdist=0.;
      for (j=0; j<set[indx].anz_n; j++)
      {
        n=set[indx].node[j];
        v_result(nref,&node[n].nx,vnrefn);
        dist=v_betrag(vnrefn);
        if(dist>maxdist) { maxdist=dist; maxn=n; }
      }
      //printf(" maxn:%d\n", maxn);
      n=maxn;
      nref[0]=node[n].nx;
      nref[1]=node[n].ny;
      nref[2]=node[n].nz;
      setr(copyset,"n",n);
      seta(copyseq,"n",n);

      for (j=1; j<set[indx].anz_n; j++)
      {
        /* search the closest to maxn */
        mindist=MAX_FLOAT;
        for (jj=0; jj<set[copyset].anz_n; jj++)
        {
          n=set[copyset].node[jj];
          v_result(nref,&node[n].nx,vnrefn);
          dist=v_betrag(vnrefn);
          if(dist<mindist) { mindist=dist; minn=n; }
        }
        //printf("j:%d minn:%d set[indx].anz_n:%d\n",j, minn,set[indx].anz_n);
        n=minn;
        nref[0]=node[n].nx;
        nref[1]=node[n].ny;
        nref[2]=node[n].nz;
        setr(copyset,"n",n);
        seta(copyseq,"n",n);
      }
      delSet("-tmp");

      /* check which of the end-points is closer to the origin. If its the last one then switch type[0] */
      vn1[0] = (node[set[copyseq].node[0]].nx* scale->w+scale->x);
      vn1[1] = (node[set[copyseq].node[0]].ny* scale->w+scale->y);
      vn1[2] = (node[set[copyseq].node[0]].nz* scale->w+scale->z);
      vn2[0] = (node[set[copyseq].node[set[copyseq].anz_n-1]].nx* scale->w+scale->x);
      vn2[1] = (node[set[copyseq].node[set[copyseq].anz_n-1]].ny* scale->w+scale->y);
      vn2[2] = (node[set[copyseq].node[set[copyseq].anz_n-1]].nz* scale->w+scale->z);
      if(v_betrag(vn1) > v_betrag(vn2))
      {
        if(type[0]=='+') type[0]='-'; else type[0]='+';
      }

      /* load the final seq */
      if(type[0]=='+')
      {
        for (j=0; j<set[copyseq].anz_n; j++)
        {
          //n=set[copyseq].node[j];
          //nod(anz, &node, 0, anz->nmax+1, node[n].nx,node[n].ny,node[n].nz, 0);
          //seta(seq,"n",anz->nmax);
          seta(seq,"n",set[copyseq].node[j]);
        }
      }
      else
      {
        for (j=set[copyseq].anz_n-1; j>=0; j--)
        {
          //n=set[copyseq].node[j];
          //nod(anz, &node, 0, anz->nmax+1, node[n].nx,node[n].ny,node[n].nz, 0);
          //seta(seq,"n",anz->nmax);
          seta(seq,"n",set[copyseq].node[j]);
        }
      }
      delSet("-seq");

      /* when node coordinates were changed to the deformed ones then switch back before they are copied and then switch again */ 
      if(addDispFlagLocal==2)
      {
        addDispToCoordinates(node);
      }
      /*
      // map dataset on new node
      sprintf(buffer,"ds%d",pre_lc+1); 
      j=getSetNr( "all");
      if(j<0) printf("ERROR:set all does not exist\n");
      n=interpol3d(seq, j, "vol", buffer, 0);
      if(n>0) areampc(n, j, "map", "surf", buffer, 0, 0, node, 0);
      zap(specialset->nompc);
      */
    }
    else if (set[indx].type>0)
    {
      /* open a node sequence */
      /* generate a name based on the setname */
      sprintf(name,"N%s",buffer);
      seq=getSetNr(name);
      if(seq>-1) getNewName(name,"se");
      seq=pre_seta(name,"is",0);
      if(type[0]=='+')
      {
        for (j=0; j<set[indx].anz_n; j++)
        {
          //n=set[indx].node[j];
          //nod(anz, &node, 0, anz->nmax+1, node[n].nx,node[n].ny,node[n].nz, 0);
          //seta(seq,"n",anz->nmax);
          seta(seq,"n",set[indx].node[j]);
        }
      }
      else
      {
        for (j=set[indx].anz_n-1; j>=0; j--)
        {
          //n=set[indx].node[j];
          //nod(anz, &node, 0, anz->nmax+1, node[n].nx,node[n].ny,node[n].nz, 0);
          //seta(seq,"n",anz->nmax);
          seta(seq,"n",set[indx].node[j]);
        }
      }

      /* when node coordinates were changed to the deformed ones then switch back before they are copied and then switch again */ 
      if(addDispFlagLocal==2)
      {
        addDispToCoordinates(node);
      }
      /*
      // map dataset on new node
      sprintf(buffer,"ds%d",pre_lc+1); 
      j=getSetNr( "all");
      if(j<0) printf("ERROR:set all does not exist\n");
      n=interpol3d(seq, j, "vol", buffer, 0);
      if(n>0) areampc(n, j, "map", "surf", buffer, 0, 0, node, 0);
      zap(specialset->nompc);
      */
    }
    else
    {
      /* when node coordinates were changed to the deformed ones then switch back before they are copied and then switch again */ 
      if(addDispFlagLocal==2)
      {
        addDispToCoordinates(node);
      }
      errMsg(" ERROR: Set (%s) contains no lcmb or line and is no sequence of nodes\n", name );
      return;
    }

    /* compile the lc-data based on a previous lc-selection */
    lc = pre_lc;
    eNr         =  cur_entity;
    printf("ds:%d e:%d\n", lc, eNr);
    length2D( seq, lc, eNr);

    /* delete a temporary sequence created for the line mapping */ 
    if (zapFlag) zap(set[seq].name);
    else delSet(set[seq].name);
  }
  else if (compare(type, "amp", 1)==1)
  {
    if(compareStrings(name, "all")>0) length=0;
    else length= strsplt( name, '*', &dat);
    for (nr=0; nr<anz->amps; nr++)
    {
      indx=-1;
      if(!length) indx=nr;
      else
      {
        for(i=0; i<length; i++)
        {
          if(strstr(amplitude[nr].name, dat[i]) !=NULL)
          {
            if( amplitude[nr].name != (char *)NULL ) indx=nr;
          }
	}
      }
      if(indx==-1) continue;
      sprintf(buffer, "%s.out",amplitude[nr].name);
      handle_out = fopen (buffer, "w+b" );
      if (handle_out==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n", buffer); return;}
      for (i=0; i<amplitude[indx].n; i++)
      {
        fprintf (handle_out, "%d %e %e\n", i+1, amplitude[indx].x[i],amplitude[indx].y[i]);
      }

      if((dsNr=(int *)realloc((int *)dsNr, (anz_lc+1) *sizeof(int)))==NULL )
        printf("\n\n ERROR: malloc failed \n\n") ;
      dsNr[anz_lc]=nr;
      anz_lc++; 

      fclose(handle_out);
    }

    sprintf(buffer, "amplitude.gnu");
    handle_gnu = fopen (buffer, "w+b" );
    if (handle_gnu==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n",buffer); return;}
#ifdef WIN32
    fprintf(handle_gnu, "set term png\n");
    sprintf(buffer, "amplitude.png", graph_Nr);
    fprintf(handle_gnu, "set out \"%s\"\n", buffer);
#else
    fprintf(handle_gnu, "set term postscript landscape monochrom  \n");
    fprintf(handle_gnu, "#set term x11 \n");
    sprintf(buffer, "amplitude.ps");
    fprintf(handle_gnu, "set out \"%s\"\n", buffer);
#endif
    fprintf(handle_gnu, "set grid\n");
    fprintf(handle_gnu, "set title \"Amplitude\"\n");
    fprintf(handle_gnu, "set ylabel \" y \"\n");
    fprintf(handle_gnu, "set xlabel \" x \"\n");
    for (nr=0; nr<anz_lc; nr++)
    {
      sprintf(buffer, "%s.out", amplitude[dsNr[nr]].name);
      if((dataset[0]=='l')&&(nr)) fprintf(handle_gnu, ", \"%s\" using 2:3 title '%s' with linespoints\\\n", buffer, amplitude[dsNr[nr]].name);
      else if(dataset[0]=='l') fprintf(handle_gnu, "plot \"%s\" using 2:3 title '%s' with linespoints\\\n", buffer, amplitude[dsNr[nr]].name);
      else fprintf(handle_gnu, "plot \"%s\" using 2:3 title '%s' with linespoints\n", buffer, amplitude[dsNr[nr]].name);
    }
    if(dataset[0]=='l') fprintf(handle_gnu, "\n");
    fclose(handle_gnu);

#ifdef WIN32
    sprintf(buffer, "amplitude.png");
    printf ("write %s \n", buffer);
    sprintf(buffer, "gnuplot amplitude.gnu" );
    system (buffer);
//   if(inpformat)  sprintf(buffer, "%s amplitude.png &", psviewer );
    if(inpformat) sprintf(buffer, "amplitude.png &");
    system (buffer);
#else
    sprintf(buffer, "amplitude.ps");
    printf ("write %s \n", buffer);
    sprintf(buffer, "gnuplot amplitude.gnu" );
    system (buffer);
    if(inpformat) sprintf(buffer, "%s amplitude.ps &", psviewer );
    system (buffer);
#endif
    free(dsNr);
  } 

  else if (compare(type, "mat", 1)==1)
  {
    if(compareStrings(name, "all")>0) length=0;
    else length= strsplt( name, '*', &dat);
    for (nr=0; nr<anz->mats; nr++)
    {
      indx=-1;
      if(!length) indx=nr;
      else
      {
        for(i=0; i<length; i++)
        {
          if(strstr(material[nr].name, dat[i]) !=NULL)
          {
            if( material[nr].name != (char *)NULL ) indx=nr;
          }
	}
      }
      if(indx==-1) continue;

      if(material[indx].nela)
      {
        sprintf(buffer, "%s_elastic.out",material[nr].name);
        handle_out = fopen (buffer, "w+b" );
        if (handle_out==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n", buffer); return;}
        for (i=0; i<material[indx].nela; i++)
        {
          fprintf (handle_out, "%d %e %e %e\n", i+1,material[indx].ela[i],material[indx].nue[i], material[indx].tela[i]);
        }
        fclose(handle_out);
      }

      if(material[indx].nexp)
      {
        sprintf(buffer, "%s_expansion.out",material[nr].name);
        handle_out = fopen (buffer, "w+b" );
        if (handle_out==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n", buffer); return;}
        for (i=0; i<material[indx].nexp; i++)
        {
          fprintf (handle_out, "%d %e %e\n", i+1,material[indx].exp[i],material[indx].texp[i]);
        }
        fclose(handle_out);
      }

      if(material[indx].ncon)
      {
        sprintf(buffer, "%s_conductivity.out",material[nr].name);
        handle_out = fopen (buffer, "w+b" );
        if (handle_out==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n", buffer); return;}
        for (i=0; i<material[indx].ncon; i++)
        {
          fprintf (handle_out, "%d %e %e\n", i+1,material[indx].con[i],material[indx].tcon[i]);
        }
        fclose(handle_out);
      }

      if(material[indx].nsph)
      {
        sprintf(buffer, "%s_spec_heat.out",material[nr].name);
        handle_out = fopen (buffer, "w+b" );
        if (handle_out==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n", buffer); return;}
        for (i=0; i<material[indx].nsph; i++)
        {
          fprintf (handle_out, "%d %e %e\n", i+1,material[indx].sph[i],material[indx].tsph[i]);
        }
        fclose(handle_out);
      }
      if((dsNr=(int *)realloc((int *)dsNr, (anz_lc+1) *sizeof(int)))==NULL )
        printf("\n\n ERROR: malloc failed \n\n") ;
      dsNr[anz_lc]=nr;
      anz_lc++; 
    }

    sprintf(buffer, "material.gnu");
    handle_gnu = fopen (buffer, "w+b" );
    if (handle_gnu==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n",buffer); return;}
#ifdef WIN32
    fprintf(handle_gnu, "set term png\n");
    sprintf(buffer, "material.png", graph_Nr);
    fprintf(handle_gnu, "set out \"%s\"\n", buffer);
#else
    fprintf(handle_gnu, "set term postscript landscape monochrom  \n");
    fprintf(handle_gnu, "#set term x11 \n");
    sprintf(buffer, "material.ps");
    fprintf(handle_gnu, "set out \"%s\"\n", buffer);
#endif
    fprintf(handle_gnu, "set grid\n");
    fprintf(handle_gnu, "set title \"Material\"\n");

    fprintf(handle_gnu, "set ylabel \" Elastic Modulus \"\n");
    fprintf(handle_gnu, "set xlabel \" Temperature \"\n");
    sum=0;
    for (nr=0; nr<anz_lc; nr++)
    {
      if(material[dsNr[nr]].nela)
      {
        sprintf(buffer, "%s_elastic.out", material[dsNr[nr]].name);
        if((dataset[0]=='l')&&(sum)) fprintf(handle_gnu, ", \"%s\" using 4:2 title '%s' with linespoints\\\n", buffer, material[dsNr[nr]].name);
        else if(dataset[0]=='l') fprintf(handle_gnu, "plot \"%s\" using 4:2 title '%s' with linespoints\\\n", buffer, material[dsNr[nr]].name);
        else fprintf(handle_gnu, "plot \"%s\" using 4:2 title '%s' with linespoints\n", buffer, material[dsNr[nr]].name);
        sum++;
      }
    }
    if((sum)&&(dataset[0]=='l')) fprintf(handle_gnu, "\n");

    fprintf(handle_gnu, "set ylabel \" Expansion Coefficient \"\n");
    fprintf(handle_gnu, "set xlabel \" Temperature \"\n");
    sum=0;
    for (nr=0; nr<anz_lc; nr++)
    {
      if(material[dsNr[nr]].nexp)
      {
        sprintf(buffer, "%s_expansion.out", material[dsNr[nr]].name);
        if((dataset[0]=='l')&&(sum)) fprintf(handle_gnu, ", \"%s\" using 3:2 title '%s' with linespoints\\\n", buffer, material[dsNr[nr]].name);
        else if(dataset[0]=='l') fprintf(handle_gnu, "plot \"%s\" using 3:2 title '%s' with linespoints\\\n", buffer, material[dsNr[nr]].name);
        else fprintf(handle_gnu, "plot \"%s\" using 3:2 title '%s' with linespoints\n", buffer, material[dsNr[nr]].name);
        sum++;
      }
    }
    if((sum)&&(dataset[0]=='l')) fprintf(handle_gnu, "\n");

    fprintf(handle_gnu, "set ylabel \" Conductivity \"\n");
    fprintf(handle_gnu, "set xlabel \" Temperature \"\n");
    sum=0;
    for (nr=0; nr<anz_lc; nr++)
    {
      if(material[dsNr[nr]].ncon)
      {
        sprintf(buffer, "%s_conductivity.out", material[dsNr[nr]].name);
        if((dataset[0]=='l')&&(sum)) fprintf(handle_gnu, ", \"%s\" using 3:2 title '%s' with linespoints\\\n", buffer, material[dsNr[nr]].name);
        else if(dataset[0]=='l') fprintf(handle_gnu, "plot \"%s\" using 3:2 title '%s' with linespoints\\\n", buffer, material[dsNr[nr]].name);
        else fprintf(handle_gnu, "plot \"%s\" using 3:2 title '%s' with linespoints\n", buffer, material[dsNr[nr]].name);
        sum++;
      }
    }
    if((sum)&&(dataset[0]=='l')) fprintf(handle_gnu, "\n");

    fprintf(handle_gnu, "set ylabel \" Specific Heat \"\n");
    fprintf(handle_gnu, "set xlabel \" Temperature \"\n");
    sum=0;
    for (nr=0; nr<anz_lc; nr++)
    {
      if(material[dsNr[nr]].nsph)
      {
        sprintf(buffer, "%s_spec_heat.out", material[dsNr[nr]].name);
        if((dataset[0]=='l')&&(sum)) fprintf(handle_gnu, ", \"%s\" using 3:2 title '%s' with linespoints\\\n", buffer, material[dsNr[nr]].name);
        else if(dataset[0]=='l') fprintf(handle_gnu, "plot \"%s\" using 3:2 title '%s' with linespoints\\\n", buffer, material[dsNr[nr]].name);
        else fprintf(handle_gnu, "plot \"%s\" using 3:2 title '%s' with linespoints\n", buffer, material[dsNr[nr]].name);
        sum++;
      }
    }
    if((sum)&&(dataset[0]=='l')) fprintf(handle_gnu, "\n");
    fclose(handle_gnu);

#ifdef WIN32
    sprintf(buffer, "material.png");
    printf ("write %s \n", buffer);
    sprintf(buffer, "gnuplot material.gnu" );
    system (buffer);
//   if(inpformat)  sprintf(buffer, "%s material.png &", psviewer );
    if(inpformat) sprintf(buffer, "material.png &");
    system (buffer);
#else
    sprintf(buffer, "material.ps");
    printf ("write %s \n", buffer);
    sprintf(buffer, "gnuplot material.gnu" );
    system (buffer);
    if(inpformat) sprintf(buffer, "%s material.ps &", psviewer );
    system (buffer);
#endif
    free(dsNr);
  } 
  else
  {
    /* get the list of datasets */
    if(length>2)
    {
      /* compile the lc-data based on a dataset-name */
      anz_lc=0;
      for(lc=lcmin; lc<=lcmax; lc++)
      {
        if( compare( lcase[lc].name, dataset, strlen(dataset)) == strlen(dataset) )
	  //if(compareStrings(lcase[lc].name, dataset)>0)
        {
          anz_lc++; 
          if((dsNr=(int *)realloc((int *)dsNr, (anz_lc+2) *sizeof(int)))==NULL )
            printf("\n\n ERROR: malloc failed \n\n") ;
          dsNr[anz_lc]=lc;
	}
      }
      if(!anz_lc)
      {
        printf(" ERROR: found no matching Dataset for string:%s\n", dataset);
        return;
      }
      dsNr[0]=anz_lc;

      /* determine the entity_nr */
      if(compareStrings(name,"-p")<1)
      {
       if(length<4) eNr = cur_entity;
       else
       {
        /* check if the data of the specified lcase (Dataset) are already available */
        if (!lcase[dsNr[1]].loaded)
        {
          if( pre_readfrdblock(copiedNodeSets , dsNr[1], anz, node, lcase )==-1) 
          {
            printf("ERROR in graph: Could not read data for Dataset:%d\n", dsNr[1]+1); 
            return;
          }
          calcDatasets( dsNr[1], anz, node, lcase );
          recompileEntitiesInMenu(dsNr[1]);
        }
        for(i=0; i<lcase[dsNr[1]].ncomps; i++)
        {
          j=0;
          while((lcase[dsNr[1]].compName[i][j]!=' ')&&(lcase[dsNr[1]].compName[i][j]!='\0'))
	  {  buffer[j]=lcase[dsNr[1]].compName[i][j]; j++; }
          buffer[j]='\0';
	  //printf("|%s|%s|\n",buffer,entity);
          if(compareStrings(buffer, entity)>0) { eNr =i; break; } 
        }
       }
       if(eNr==-1)
       {
        printf(" ERROR: found no matching entity for string:%s\n", entity);
        return;
       }
      }
    }
    else
    {
      /* compile the lc-data based on a previous lc-selection */
      if (dsSequence.nds<1)
      {
        errMsg(" ERROR: no sequence is defined, choose the menu-entry \"Animate->Toggle Dataset Sequence\" and define a sequence\n\n");
        return;
      }
      anz_lc=0;
      for(i=0; i<dsSequence.nds; i++)
      {
        lc=dsSequence.ds[i];
        anz_lc++; 
        if((dsNr=(int *)realloc((int *)dsNr, (anz_lc+2) *sizeof(int)))==NULL )
          printf("\n\n ERROR: malloc failed \n\n") ;
        dsNr[anz_lc]=lc;
      }
      dsNr[0]=anz_lc;
      if(compareStrings(name,"-p")<1) eNr         =  cur_entity;
    }

    if(compareStrings(name,"-p")>0)
    {
      param2D( type, dsNr, entity);
    }
    else
    {
      operateAlias( name, "se" );
      indx=getSetNr( name );
      if (indx<0)
      {
        errMsg(" ERROR: Set (%s) is undefined", name );
        return;
      }
      plot2D( type, indx, dsNr, eNr);
    }
    free(dsNr);
  } 
}
