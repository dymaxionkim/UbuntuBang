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

#include <extUtil.h>

#define TEST     0

#define INI_FIELD_SIZE 100000
#define BLOCKLENGTH 6                 /* lines between blocks of data */


int readNastran( char *datin, Summen *anz, Nodes **nptr, Elements **eptr, Datasets **lptr )
{
  int debug;

  FILE *handle;
  int i=0,j,n, line,skip;

  char rec_str[MAX_LINE_LENGTH];
  char buffer[MAX_LINE_LENGTH];
  char rec_str_bak[MAX_LINE_LENGTH];
  int  node_field_size, elem_field_size;
  int  e_nmax=1, e_nmin=1;
  int  length, idum;

  int step=0,step_stress=0,analysis_type=0;
  double value[INI_FIELD_SIZE], eigenvalue[INI_FIELD_SIZE];
  char dataset_name[MAX_LINE_LENGTH];

  Nodes     *node=NULL;
  Elements  *elem=NULL;
  Datasets  *lcase=NULL;


  debug=TEST;
  anz->u=anz->n=anz->e=anz->l=-1;

  node_field_size=INI_FIELD_SIZE;
  do
  {
    if ( (node = (Nodes *)realloc( (Nodes *)node, (node_field_size+1) * sizeof(Nodes))) == NULL )
    {
      printf("WARNING: in readfrd() is INI_FIELD_SIZE:%d to large and is reduced\n", node_field_size );
      node_field_size/=2;
    }
    if(node_field_size<100)
    {
      printf("\n\n ERROR: not enough memory in readfrd()\n\n");
      exit(-1);
    }
  }while(!node);

  elem_field_size=INI_FIELD_SIZE;
  do
  {
    if((elem = (Elements *)realloc( (Elements *)elem, (elem_field_size+1) * sizeof(Elements))) == NULL )
    {
      printf("WARNING: in readfrd() is INI_FIELD_SIZE:%d to large and is reduced\n", elem_field_size );
      elem_field_size/=2;
    }
    if(elem_field_size<100)
    {
      printf("\n\n ERROR: not enough memory in readfrd()\n\n");
      exit(-1);
    }
  }while(!elem);

 
  /* Open the files and check to see that it was opened correctly */
  handle = fopen (datin, "r");
  if ( handle== NULL )  { printf ("ERROR: The input file \"%s\" could not be opened.\n\n", datin); return(-1); }
  else  printf (" file:%s opened\n", datin);

  printf (" reading Nastran format\n");

  strcpy(anz->model, datin);
  printf (" MODEL NAME:  %s\n\n", anz->model);

  anz->emax=0;  anz->emin=MAX_INTEGER;
  anz->nmax=0;  anz->nmin=MAX_INTEGER;

  do
  {
    strcpy(rec_str_bak, rec_str);
    length = frecord( handle, rec_str);
    if (rec_str[length] == (char)EOF) break;
    if(compare(&rec_str[50],"S O R T E D   B U L K   D A T A   E C H O",12)==12)
    {
      length = frecord( handle, rec_str);
      length = frecord( handle, rec_str);
      while(1)
      {
        length = frecord( handle, rec_str);
        if (rec_str[length] == (char)EOF) break;
        if (rec_str[0] == '1') break;

        /* search beginning of statement */
        skip=0; while(rec_str[skip++]!='-');
	// printf("%s\nskip:%d\n", rec_str,skip);

        /* elems */
        if(compare(&rec_str[skip+8],"CHEXA   ",8)==8)
        {
	  if(debug) printf("%d %s",i,rec_str);
          if (rec_str[length] == (char)EOF) break;

          anz->e++;
          if (anz->e>=elem_field_size)
          {
            elem_field_size=anz->e+100;
            if((elem=(Elements *)realloc((Elements *)elem,(elem_field_size+1)*sizeof(Elements)))==NULL)
            {
              printf("\n\n ERROR: realloc failed, elem-index:%d\n\n", anz->e);
              return(-1);
            }
          }
          sscanf(&rec_str[skip+16], "%8d%8d%8d%8d%8d%8d%8d%8d", &elem[anz->e].nr,&elem[anz->e].mat, &elem[anz->e].nod[0], &elem[anz->e].nod[1], &elem[anz->e].nod[2], &elem[anz->e].nod[3], &elem[anz->e].nod[4], &elem[anz->e].nod[5] );

          length = frecord( handle, rec_str);
          if (rec_str[length] == (char)EOF) break;
          if (rec_str[0] == '1')
          {
            for(line=0; line<BLOCKLENGTH; line++) length = frecord( handle, rec_str);
          }
          skip=0; while(rec_str[skip++]!='-');
          // printf("%s\nskip:%d\n", rec_str,skip);
          length = sscanf(&rec_str[skip+16],"%8d%8d%8d%8d%8d%8d%8d%8d",  &elem[anz->e].nod[6], &elem[anz->e].nod[7], &elem[anz->e].nod[8], &elem[anz->e].nod[9], &elem[anz->e].nod[10], &elem[anz->e].nod[11], &elem[anz->e].nod[12], &elem[anz->e].nod[13]);

          /* check if its hex20 */
          if(length>2)
          {
            length = frecord( handle, rec_str);
            if (rec_str[length] == (char)EOF) break;
            if (rec_str[0] == '1')
	    {
              for(line=0; line<BLOCKLENGTH; line++) length = frecord( handle, rec_str);
	    }
            skip=0; while(rec_str[skip++]!='-');
            // printf("%s\nskip:%d\n", rec_str,skip);

            length = sscanf(&rec_str[skip+16],"%8d%8d%8d%8d%8d%8d", &elem[anz->e].nod[14], &elem[anz->e].nod[15], &elem[anz->e].nod[16], &elem[anz->e].nod[17], &elem[anz->e].nod[18], &elem[anz->e].nod[19]);
            //elem[anz->e].type  = 4;
            elem[anz->e].type  = 1;
          }
          else
          {
            elem[anz->e].type  = 1;
          }
          elem[anz->e].group  = 0;

          if (elem[anz->e].nr >  anz->emax)  anz->emax=elem[anz->e].nr;
          if (elem[anz->e].nr <  anz->emin)  anz->emin=elem[anz->e].nr;

          anz->etype[elem[anz->e].type]++;
	  if(debug) { printf(" %d %d", anz->e, elem[anz->e].nr);
	    for(j=0;j<8;j++) printf(" %d", elem[anz->e].nod[j]); printf("\n"); }
	}

        /* nodes */
        if(compare(&rec_str[skip+8],"GRID   *",8)==8)
        {
          anz->n++;
          node[anz->n].nr = atoi(&rec_str[skip+16]);
          if (node[anz->n].nr>=node_field_size)
          {
            node_field_size=node[anz->n].nr+100;
            if ( (node = (Nodes *)realloc((Nodes *)node, (node_field_size+1) * sizeof(Nodes))) == NULL )
            {
              printf("\n\n ERROR: realloc failed, nodenr:%d\n\n", node[anz->n].nr) ;
              return(-1);
            }
          }
          node[node[anz->n].nr].indx=anz->n;

          sscanf(&rec_str[skip+32],"%16d%16lf%16lf", &idum, &node[node[anz->n].nr].nx,&node[node[anz->n].nr].ny);

          length = frecord( handle, rec_str);
          if (rec_str[length] == (char)EOF) break;
          if (rec_str[0] == '1')
	  {
            for(line=0; line<BLOCKLENGTH; line++) length = frecord( handle, rec_str);
	  }
          skip=0; while(rec_str[skip++]!='-');
          // printf("%s\nskip:%d\n", rec_str,skip);
          sscanf(&rec_str[skip+16],"%16lf%16d", &node[node[anz->n].nr].nz, &idum);

          if (node[anz->n].nr >  anz->nmax)  anz->nmax=node[anz->n].nr;
          if (node[anz->n].nr <  anz->nmin)  anz->nmin=node[anz->n].nr;
	}
      }
    }

    idum=0;
    if(compare(&rec_str[41],"R E A L   E I G E N V E C T O R   N O",12)==12)
    {
      strcpy(dataset_name,"EIGV");
      analysis_type=2; 
      sscanf(&rec_str[80],"%d", &step);
      sscanf(&rec_str[18],"%lf", &value[step]);
      sscanf(&rec_str_bak[18],"%lf", &eigenvalue[step]);
      idum=1;
    }
    if((compare(&rec_str[45],"D I S P L A C E M E N T   V E C T O R",12)==12)||(idum))
    {
      if(!idum)
      {
        strcpy(dataset_name,"STAT");
        analysis_type=0; 
        step++;
      }
      length = frecord( handle, rec_str);
      length = frecord( handle, rec_str);
      while(1)
      {
        length = frecord( handle, rec_str);
        if (rec_str[length] == (char)EOF) break;
        if (rec_str[0] == '1') break;
	if(debug) printf("%d %d %s",line,anz->l,rec_str);

        //n=atoi(rec_str);
        sscanf(rec_str,"%d %s",&n, buffer);
        if (buffer[0] != 'G') continue;
        if (n > anz->nmax) continue;
        if (n == anz->nmin)
	{
	  if(debug) printf(" %d new ds:%d n:%d\n",line,anz->l,n);
          anz->l++;

          /* create a new dataset */
          if ( (lcase = (Datasets *)realloc((Datasets *)lcase, (anz->l+2) * sizeof(Datasets))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          lcase[anz->l].ncomps = 3;
          lcase[anz->l].irtype = 1;
          lcase[anz->l].npheader  = 0 ;
          lcase[anz->l].value  = value[step] ;
          strcpy(lcase[anz->l].name,"DISP") ;
          sprintf( lcase[anz->l].dataset_name,"%s", dataset_name);
          strcpy(lcase[anz->l].dataset_text,"");
          strcpy(lcase[anz->l].analysis_name,"analysis_name");
          lcase[anz->l].step_number=step;
          lcase[anz->l].analysis_type=analysis_type;
	  lcase[anz->l].loaded = 1;
          lcase[anz->l].fileptr = NULL;
   
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
            lcase[anz->l].max[i]=-MAX_INTEGER;
            lcase[anz->l].min[i]=MAX_INTEGER;
            for(j=0; j<=anz->nmax; j++)
              lcase[anz->l].dat[i][j]=0.;
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
            lcase[anz->l].menu[i]   = 1;
            lcase[anz->l].ictype[i] = 2;
            lcase[anz->l].icind1[i] = i+1;
            lcase[anz->l].icind2[i] = 0;
            lcase[anz->l].iexist[i] = 0;
          }
          strcpy( lcase[anz->l].compName[0], "D1");
          strcpy( lcase[anz->l].compName[1], "D2");
          strcpy( lcase[anz->l].compName[2], "D3");
        }

        sscanf(rec_str,"%*d %*s %f %f %f",&lcase[anz->l].dat[0][n],&lcase[anz->l].dat[1][n],&lcase[anz->l].dat[2][n]);
        if(debug) printf("nod:%d %f\n",n,lcase[anz->l].dat[0][n]);

        if (n == anz->nmax)
	{
          for(j=0; j<anz->n; j++)
          {
            for(i=0; i<lcase[anz->l].ncomps; i++)
            {
              if(lcase[anz->l].dat[i][node[j].nr] > lcase[anz->l].max[i])
              {
                lcase[anz->l].max[i]=lcase[anz->l].dat[i][node[j].nr];
                lcase[anz->l].nmax[i]=node[j].nr;
              }
              if(lcase[anz->l].dat[i][node[j].nr] < lcase[anz->l].min[i])
              {
                lcase[anz->l].min[i]=lcase[anz->l].dat[i][node[j].nr];
                lcase[anz->l].nmin[i]=node[j].nr;
              }
            }
          }
	  break;
	}
      }
    }
    if(compare(&rec_str[20],"D I R E C T   S T R E S S E S   A T   G R I D   P O I N T S",12)==12)
    {
      if(compare(&rec_str_bak[6],"EIGENVALUE",8)==8)
      {
        strcpy(dataset_name,"EIGV");
        analysis_type=2; 
        sscanf(&rec_str_bak[18],"%lf", &eigenvalue[0]);
        for(i=1; i<INI_FIELD_SIZE; i++) if(eigenvalue[0]==eigenvalue[i]) break;
        if(i<INI_FIELD_SIZE) step=i; else { printf("ERROR, step not known\n"); exit(0); }
      }
      else
      {
        strcpy(dataset_name,"STAT");
        analysis_type=0; 
        step_stress++;
        step=step_stress;
      }
      length = frecord( handle, rec_str);
      length = frecord( handle, rec_str);
      length = frecord( handle, rec_str);
      while(1)
      {
        length = frecord( handle, rec_str);
        if (rec_str[length] == (char)EOF) break;
        if (rec_str[0] == '1') break;
	if(debug) printf("%d %d %s",line,anz->l,rec_str);

        n=atoi(rec_str);
        if (n > anz->nmax) continue;
        if (n == anz->nmin)
	{
	  if(debug) printf(" %d new ds:%d n:%d\n",line,anz->l,n);
          anz->l++;

          /* create a new dataset */
          if ( (lcase = (Datasets *)realloc((Datasets *)lcase, (anz->l+2) * sizeof(Datasets))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          lcase[anz->l].ncomps = 6;
          lcase[anz->l].irtype = 1;
          lcase[anz->l].npheader  = 0 ;
          lcase[anz->l].value  = value[step] ;
          strcpy(lcase[anz->l].name,"STRESS") ;
          sprintf( lcase[anz->l].dataset_name,"%s", dataset_name);
          strcpy(lcase[anz->l].dataset_text,"");
          strcpy(lcase[anz->l].analysis_name,"analysis_name");
          lcase[anz->l].step_number=step;
          lcase[anz->l].analysis_type=analysis_type;
	  lcase[anz->l].loaded = 1;
          lcase[anz->l].fileptr = NULL;
   
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
            lcase[anz->l].max[i]=-MAX_INTEGER;
            lcase[anz->l].min[i]=MAX_INTEGER;
            for(j=0; j<=anz->nmax; j++)
              lcase[anz->l].dat[i][j]=0.;
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
            lcase[anz->l].menu[i]   = 1;
            lcase[anz->l].ictype[i] = 4;
            if(i<3)
	    {
              lcase[anz->l].icind1[i] = i+1;
              lcase[anz->l].icind2[i] = i+1;
	    }
            else
	    {
              lcase[anz->l].icind1[i] = i+1;
              if(i<6)
                lcase[anz->l].icind2[i] = i+2;
              else lcase[anz->l].icind2[i] = 1;
	    }
            lcase[anz->l].iexist[i] = 0;
          }
          strcpy( lcase[anz->l].compName[0], "XX");
          strcpy( lcase[anz->l].compName[1], "YY");
          strcpy( lcase[anz->l].compName[2], "ZZ");
          strcpy( lcase[anz->l].compName[3], "XY");
          strcpy( lcase[anz->l].compName[4], "YZ");
          strcpy( lcase[anz->l].compName[5], "ZX");
        }

        sscanf(rec_str,"%*d %f %f %f %f %f %f",&lcase[anz->l].dat[0][n],&lcase[anz->l].dat[1][n],&lcase[anz->l].dat[2][n],&lcase[anz->l].dat[3][n],&lcase[anz->l].dat[4][n],&lcase[anz->l].dat[5][n]);
        if(debug) printf("nod:%d %f\n",n,lcase[anz->l].dat[0][n]);

        if (n == anz->nmax)
	{
          for(j=0; j<anz->n; j++)
          {
            for(i=0; i<lcase[anz->l].ncomps; i++)
            {
              if(lcase[anz->l].dat[i][node[j].nr] > lcase[anz->l].max[i])
              {
                lcase[anz->l].max[i]=lcase[anz->l].dat[i][node[j].nr];
                lcase[anz->l].nmax[i]=node[j].nr;
              }
              if(lcase[anz->l].dat[i][node[j].nr] < lcase[anz->l].min[i])
              {
                lcase[anz->l].min[i]=lcase[anz->l].dat[i][node[j].nr];
                lcase[anz->l].nmin[i]=node[j].nr;
              }
            }
          }
	  break;
	}
      }
    }
  }while(1);


  anz->u++;
  anz->p++;
  anz->n++;
  anz->e++;
  anz->l++;
  fclose(handle);

  node_field_size=anz->nmax+1;
  if((node =(Nodes *)realloc( (Nodes *)node, node_field_size*sizeof(Nodes)))==NULL)
    printf("\n\n ERROR: realloc failed\n\n") ;
  else
    printf ("\n %d nodes reallocated \n",anz->nmax);

  elem_field_size=anz->e+1;
  if ( (elem = (Elements *)realloc((Elements *)elem, elem_field_size * sizeof(Elements))) == NULL )
    printf("\n\n ERROR: in readfrd realloc failed\n\n") ;
  else
    printf ("\n %d elements reallocated \n", anz->e);

  if ( e_nmax > (anz->nmax) )
  {
    printf ("\nWARNING: element requestes a nodename higher than allocated\n\n");
    printf (" e_nmax=%d e_nmin=%d\n", e_nmax, e_nmin );
  }
  if ( e_nmin < 1 )
  {
    printf ("\nWARNING: element requestes a nodename lower than allocated\n\n");
    printf (" e_nmax=%d e_nmin=%d\n", e_nmax, e_nmin );
  }


  anz->orign    = anz->n;
  anz->orignmax = anz->nmax;
  anz->olc = anz->l;

  *nptr = node; *eptr = elem; *lptr = lcase;
  return(1);
}

