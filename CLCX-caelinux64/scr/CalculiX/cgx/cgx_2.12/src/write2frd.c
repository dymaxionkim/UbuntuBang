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

int write2frd(char *datout, Summen *anz, Nodes *node, Elements *elem, Datasets *lcase, int controlFlag )
{
  FILE *handle1;
  int  i, j, n, k, ipuf;
  int nlc, cnam, formatFlag=0;
  char buffer[MAX_LINE_LENGTH], format[10][MAX_LINE_LENGTH];
  char rec_str[MAX_LINE_LENGTH];
  static float *value=NULL;

  /* Open the files and check to see that it was opened correctly */
  handle1 = fopen (datout, "w+b");
  if (handle1==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n",
     datout); return(-1);}
  else  printf ("\n%s opened\n",datout);

  printf ("n:%d nmax:%d nmin:%d e:%d emax:%d emin:%d ds:%d\n", anz->n, anz->nmax, anz->nmin, anz->e, anz->emax, anz->emin, anz->l ); 

  printf ("\n write frd file\n");
  fprintf (handle1, "    1C%s\n", anz->model);

  /* leading user headers */
  for (i=0; i<anz->u; i++) fprintf( handle1, "%s\n", anz->uheader[i]);

  /* leading project headers */
  for (i=0; i<anz->p; i++) fprintf( handle1, "%s\n", anz->pheader[i]);

  /* determine if long or short format can be used */
  if (anz->n > 0)
  {
    if (anz->nmax>99999) formatFlag=1;
  }
  if (anz->e > 0)
  {
    if(anz->emax>99999) formatFlag=1;
  }
  /* binary format requested? */
  if(controlFlag==1)
  {
    formatFlag=2;
  }

  if (anz->n > 0)
  {
    if (!formatFlag)
    {
      fprintf (handle1
      , "    2C                                                                   0\n");
      for (i=0; i<anz->n; i++)
      {
#ifdef WIN32
		  fprintf (handle1, " -1%5d%12.4e%12.4e%12.4e\n"
        , node[i].nr, node[node[i].nr].nx, node[node[i].nr].ny, node[node[i].nr].nz);
#else
		  fprintf (handle1, " -1%5d%12.5e%12.5e%12.5e\n"
        , node[i].nr, node[node[i].nr].nx, node[node[i].nr].ny, node[node[i].nr].nz);
#endif
      }
      fprintf (handle1, " -3\n");
    }
    else if (formatFlag==1)
    {
      fprintf (handle1
      , "    2C                                                                   1\n");
      for (i=0; i<anz->n; i++)
      {
#ifdef WIN32
        fprintf (handle1, " -1%10d%12.4e%12.4e%12.4e\n"
        , node[i].nr, node[node[i].nr].nx, node[node[i].nr].ny, node[node[i].nr].nz);
#else
        fprintf (handle1, " -1%10d%12.5e%12.5e%12.5e\n"
        , node[i].nr, node[node[i].nr].nx, node[node[i].nr].ny, node[node[i].nr].nz);
#endif
      }
      fprintf (handle1, " -3\n");
    }
    /* bin mode */
    else
    {
      sprintf(rec_str,"    2C                                                                     ");
      sprintf(buffer,"%10i",anz->n);
      sins(buffer, 25, 36, rec_str);
      rec_str[73]='3';
      rec_str[76]=0;
      fprintf(handle1,"%s\n",rec_str);
      printf("%s\n",rec_str);
      //if ( (value = (float *)realloc((float *)value, (3) * sizeof(float))) == NULL )
      //  printf("\n\n ERROR: realloc failed, value\n\n") ;
      for (i=0; i<anz->n; i++)
      {
        fwrite(&node[i].nr, sizeof(int), 1, handle1);
        //value[0]=node[node[i].nr].nx;
        //value[1]=node[node[i].nr].ny;
        //value[2]=node[node[i].nr].nz;
        //fwrite(value, sizeof(float), 3, handle1);
        fwrite(&node[node[i].nr].nx, sizeof(double), 3, handle1);
      }
      //n=-3;
      //fwrite(&n, sizeof(int), 1, handle1);
    }
  }

  if (anz->e > 0)
  {
    if (!formatFlag)
      fprintf (handle1, "    3C                                                                   0\n");
    else if (formatFlag==1)
      fprintf (handle1, "    3C                                                                   1\n");
    else
    {
      sprintf(rec_str,"    3C                                                                     ");
      sprintf(buffer,"%10i",anz->e);
      sins(buffer, 25, 36, rec_str);
      rec_str[73]='2';
      rec_str[76]=0;
      fprintf(handle1,"%s\n",rec_str);
      printf("%s\n",rec_str);
    }
    for (i=0; i<anz->e; i++)
    {
      ipuf=0;
      if (elem[i].type == 1) ipuf = 8;  /* HEXA8  */
      else if (elem[i].type == 2) ipuf = 6;  /* PENTA6   */
      else if (elem[i].type == 3) ipuf = 4;  /* TET4   */
      else if (elem[i].type == 4) ipuf = 20; /* HEXA20 */
      else if (elem[i].type == 5) ipuf = 15; /* PENTA15  */
      else if (elem[i].type == 6) ipuf = 10; /* TET10  */
      else if (elem[i].type == 7) ipuf = 3;  /* TRI3   */
      else if (elem[i].type == 8) ipuf = 6;  /* TRI6   */
      else if (elem[i].type == 9) ipuf = 4;  /* QUAD4  */
      else if (elem[i].type == 10) ipuf = 8; /* QUAD8  */
      else if (elem[i].type == 11) ipuf = 2;  /* BEAM2  */
      else if (elem[i].type == 12) ipuf = 3;  /* BEAM3  */
      if (ipuf==0)
      {
        printf (" elem(%d) not a known type (%d)\n", elem[i].nr, elem[i].type);
      }
      else
      {
        if (!formatFlag)
        {
          fprintf (handle1, " -1%5d%5d%5d%5d\n", elem[i].nr, elem[i].type, elem[i].group, elem[i].mat);
          fprintf (handle1, " -2");
          j=0;
          for (n=0; n<ipuf; n++)
          {
            fprintf (handle1, "%5d", elem[i].nod[n] );
            if (j<14) j++;
            else
            {
              if (n<ipuf-1) fprintf (handle1, "\n -2"); j=0;
            }
          }
          fprintf (handle1, "\n");
        }
        else if (formatFlag==1)
        {
          fprintf (handle1, " -1%10d%5d%5d%5d\n", elem[i].nr, elem[i].type, elem[i].group, elem[i].mat);
          fprintf (handle1, " -2");
          j=0;
          for (n=0; n<ipuf; n++)
          {
            fprintf (handle1, "%10d", elem[i].nod[n] );
            if (j<9) j++;
            else
            {
              if (n<ipuf-1) fprintf (handle1, "\n -2"); j=0;
            }
          }
          fprintf (handle1, "\n");
        }
        else
        {
          fwrite(&elem[i].nr, sizeof(int), 1, handle1);
          fwrite(&elem[i].type, sizeof(int), 1, handle1);
          fwrite(&elem[i].group, sizeof(int), 1, handle1);
          fwrite(&elem[i].mat, sizeof(int), 1, handle1);
          fwrite(elem[i].nod, sizeof(int), ipuf, handle1);
	}
      }
    }
    if (formatFlag<2) fprintf (handle1, " -3\n");
  }

  if (anz->l > 0)
  {
    for (nlc=0; nlc< anz->l; nlc++)
    {
      /* leading project headers */
      for (i=0; i<lcase[nlc].npheader; i++) fprintf( handle1, "%s\n", lcase[nlc].pheader[i]);

      /* special purpose block, for element-normals only (sendSurfNormalen()) */
      if (lcase[nlc].irtype == 3)
      {
#ifdef WIN32
        printf ("   21C%s%12.4e\n", lcase[nlc].name, lcase[nlc].value);
#else
        printf ("   21C%s%12.5e\n", lcase[nlc].name, lcase[nlc].value);
#endif
    	if (!formatFlag)
        {
#ifdef WIN32
          j=sprintf( buffer, "   21CL%-5d%12.4e", nlc+1, lcase[nlc].value); 
#else
          j=sprintf( buffer, "   21CL%-5d%12.5e", nlc+1, lcase[nlc].value); 
#endif
          for (i=j; i<MAX_LINE_LENGTH; i++) buffer[i]=' '; 
          buffer[73]='0'; buffer[75]='\n'; buffer[76]='\0';
          for (i=0; i<76; i++) fprintf( handle1, "%c", buffer[i]);
          strcpy( format[0]," -1%5d%5d%5d    1    1\n"); 
#ifdef WIN32
          strcpy( format[1]," -2%5d%12.4e%12.4e%12.4e%12.4e%12.4e%12.4e\n"); 
#else
          strcpy( format[1]," -2%5d%12.5e%12.5e%12.5e%12.5e%12.5e%12.5e\n"); 
#endif
        }
        else 
        {
#ifdef WIN32
          j=sprintf( buffer, "   21CL%-5d%12.4e", nlc+1, lcase[nlc].value); 
#else
          j=sprintf( buffer, "   21CL%-5d%12.5e", nlc+1, lcase[nlc].value); 
#endif
          for (i=j; i<MAX_LINE_LENGTH; i++) buffer[i]=' '; 
          buffer[73]='1'; buffer[75]='\n'; buffer[76]='\0';
          for (i=0; i<76; i++) fprintf( handle1, "%c", buffer[i]);
          strcpy( format[0]," -1%10d%5d%5d    1    1\n"); 
#ifdef WIN32
          strcpy( format[1]," -2%10d%12.4e%12.4e%12.4e%12.4e%12.4e%12.4e\n"); 
#else
          strcpy( format[1]," -2%10d%12.5e%12.5e%12.5e%12.5e%12.5e%12.5e\n"); 
#endif
        }
        for (i=0; i<anz->e; i++)
        {
          n=0;
          if (elem[i].type == 1) n=8;
          if (elem[i].type == 4) n=20;
          if (elem[i].type == 7) n=3;
          if (elem[i].type == 9) n=4;
          if (elem[i].type == 11) n=2;
          if (n>0)
          {
            fprintf (handle1, format[0], elem[i].nr, elem[i].type, elem[i].group );
            for (j=0; j<n; j++)
  	    fprintf (handle1, format[1], elem[i].nod[j], 
            lcase[nlc].edat[0][elem[i].nr][j], lcase[nlc].edat[1][elem[i].nr][j], lcase[nlc].edat[2][elem[i].nr][j],0.,0.,0. );
          }
        }
        fprintf (handle1, " -3\n");
      }
      else if (lcase[nlc].irtype == 1)
      {
        for(i=0; i<MAX_LINE_LENGTH; i++) rec_str[i]=' ';        
        sprintf(rec_str,"  100C");
        sins(lcase[nlc].dataset_name, 7, 12, rec_str);
#ifdef WIN32
        sprintf(buffer,"%-12.4e",lcase[nlc].value);
#else
        sprintf(buffer,"%-12.5e",lcase[nlc].value);
#endif
        sins(buffer, 13, 24, rec_str);
        sprintf(buffer,"%10i",anz->n);
        sins(buffer, 25, 36, rec_str);
        sins( lcase[nlc].dataset_text, 37, 56, rec_str );
        sprintf(buffer,"%-2d",lcase[nlc].analysis_type);
        sins(buffer , 57, 58, rec_str );
        sprintf(buffer,"%-5d",lcase[nlc].step_number);
        sins(buffer , 59, 63, rec_str );

        sins(lcase[nlc].analysis_name, 64, 73, rec_str);
        if (!formatFlag)
        {
          rec_str[73]='0';
          strcpy( format[0]," -1%5d");
        }
        else if (formatFlag==1)
        {
          rec_str[73]='1';
          strcpy( format[0]," -1%10d");
        }
        else
        {
          rec_str[73]='2';
          strcpy( format[0]," -1%10d");
        }
        /* rec_str[75]='\n'; */
        rec_str[76]=0;

        printf ("%s\n",rec_str);
        fprintf(handle1,"%s\n",rec_str);

        /* get rid of the additional columns which where calc in cgx (iexist=1) except the user has requested them */
        if(controlFlag==2) cnam=lcase[nlc].ncomps;
        else
	{
          cnam=0;
          for(i=0; i<lcase[nlc].ncomps; i++ ) if(lcase[nlc].iexist[i]!=1) cnam++;
	}
        sprintf(buffer," -4          ");
        sins(lcase[nlc].name, 6, 13, buffer );
        sprintf(&buffer[13],"%5d%5d", cnam, lcase[nlc].irtype);
        fprintf (handle1, "%s\n", buffer );

        for (i=0; i<lcase[nlc].ncomps; i++ )
	{
          if(controlFlag!=2) if(lcase[nlc].iexist[i]==1) continue;
          sprintf(buffer," -5          ");
          sins(lcase[nlc].compName[i], 6, 13, buffer );
	  if(lcase[nlc].iexist[i]==1) sprintf (&buffer[13], "%5d%5d%5d%5d%5d", 1, lcase[nlc].ictype[i], lcase[nlc].icind1[i], lcase[nlc].icind2[i], 0);
          else                        sprintf (&buffer[13], "%5d%5d%5d%5d%5d", 1, lcase[nlc].ictype[i], lcase[nlc].icind1[i], lcase[nlc].icind2[i], lcase[nlc].iexist[i]);
          fprintf (handle1, "%s\n", buffer );
        }
        if (formatFlag<2)
        {
         for (i=0; i<anz->n; i++)
         {
	  fprintf (handle1, format[0], node[i].nr);
          k=0; for (j=0; j<lcase[nlc].ncomps; j++ )
          {
            if(controlFlag!=2) if(lcase[nlc].iexist[j]==1) continue;
            if((k%6==0)&&(k>0))
	      { fprintf (handle1,"\n"); if (!formatFlag) fprintf (handle1," -2     " ); else fprintf (handle1," -2          " ); }
#ifdef WIN32
            fprintf (handle1,"%12.4e", lcase[nlc].dat[j][node[i].nr]);
#else
            fprintf (handle1,"%12.5e", lcase[nlc].dat[j][node[i].nr]);
#endif
            k++;
	  }
          fprintf (handle1, "\n");
         }
         fprintf (handle1, " -3\n");
	}
        /* bin mode */
        else 
        {
         if ( (value = (float *)realloc((float *)value, (lcase[nlc].ncomps) * sizeof(float))) == NULL )
           printf("\n\n ERROR: realloc failed, value\n\n") ;
         for (i=0; i<anz->n; i++)
         {
          fwrite(&node[i].nr, sizeof(int), 1, handle1);
          k=0;
          for (j=0; j<lcase[nlc].ncomps; j++)
	  {
            if(controlFlag!=2) if(lcase[nlc].iexist[j]==1) continue;
            value[k]=lcase[nlc].dat[j][node[i].nr];
            k++;
	  }
          fwrite(value, sizeof(float), k, handle1);
         }
	}
      }
      else
      {
        printf("type of Dataset not known:%d\n", lcase[nlc].irtype);
      }

    }
  }

  fprintf (handle1, " 9999\n");
  fclose(handle1);
  return (1);
}

