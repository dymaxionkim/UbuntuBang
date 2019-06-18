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
/*
Jan 2016 Peter Heppel added an optional parameter to read <file> ng 
 new parameter is ndsb (NoDeleteShellsorBeams) if  you want to keep them.
 (It would be better if this parameter contained vsb  for read volume ,
 shell or beams)

Jan 2016 Peter Heppel added a reader for quad8 elements - 
although I'm guessing that .mat should be 10
*/

#include <cgx.h>

#define TEST     0

#define INI_FIELD_SIZE 100000

extern Sets      *setx;
extern Summen    *anzx;
extern Alias     *alias;
extern SumGeo    anzGeo[1];
extern SumAsci   sumAsci[1];

int readNG( char *datin, Summen *apre, Sets **sptr, Nodes **nptr, Elements **eptr, Datasets **lptr )
{
  FILE *handle;
  int i=0;

  char rec_str[MAX_LINE_LENGTH], buffer[MAX_LINE_LENGTH], name[MAX_LINE_LENGTH];
  int  node_field_size, elem_field_size;
  int  e_nmax=1, e_nmin=1;
  int  length, sum,n;

  Nodes     *node=NULL;
  Elements  *elem=NULL;

  anzx=apre;
  setx=*sptr;

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

  printf (" reading NG format\n");

  strcpy(anzx->model, datin);
  printf (" MODEL NAME:  %s", anzx->model);

  do
  {
    length = frecord( handle, rec_str);
    if(compare(rec_str,"surfaceelements",6)==6) break;
    if (rec_str[length] == (char)EOF) break;
  }while(1);

  /* jump the nr of elems */
  length = frecord( handle, rec_str);
  sscanf(rec_str, "%d", &sum);
  for(i=0; i<sum; i++)
  {
    do { length = frecord( handle, rec_str); }while(!length); 
    //printf("surf:%s\n",rec_str);
    if (rec_str[length] == (char)EOF) break;

    if (anzx->e>=elem_field_size)
    {
      elem_field_size=anzx->e+100;
      if((elem=(Elements *)realloc((Elements *)elem,(elem_field_size+1)*sizeof(Elements)))==NULL)
      {
        printf("\n\n ERROR: realloc failed, elem-index:%d\n\n", anzx->e);
        return(-1);
      }
    }
    elem[anzx->e].nr    = anzx->e+1;
    elem[anzx->e].group = 1;
    elem[anzx->e].type  = 0;
    if (elem[anzx->e].nr >  anzx->emax)  anzx->emax=elem[anzx->e].nr;
    if (elem[anzx->e].nr <  anzx->emin)  anzx->emin=elem[anzx->e].nr;
    sscanf(rec_str, "%*s %*s %*s %*s %d", &n );
    if(n==3)
    {
      elem[anzx->e].type  = 7;
      elem[anzx->e].mat   = 7;
      sscanf(rec_str, "%*s %d %*s %*s %*s %d %d %d", &elem[anzx->e].mat, &elem[anzx->e].nod[0], &elem[anzx->e].nod[1], &elem[anzx->e].nod[2] );
    }
    else if(n==6)
    {
      elem[anzx->e].type  = 8;
      elem[anzx->e].mat   = 8;
      sscanf(rec_str, "%*s %d %*s %*s %*s %d %d %d %d %d %d", &elem[anzx->e].mat, &elem[anzx->e].nod[0], &elem[anzx->e].nod[1], &elem[anzx->e].nod[2], &elem[anzx->e].nod[4], &elem[anzx->e].nod[5], &elem[anzx->e].nod[3] );
    }
    else if(n==8)   //Peter qu8
    {
      elem[anzx->e].type  = 10;
      elem[anzx->e].mat   = 10; // (peter) I'm guessing here
      sscanf(rec_str, "%*s %d %*s %*s %*s %d %d %d %d %d %d %d %d", &elem[anzx->e].mat,
              &elem[anzx->e].nod[0],
              &elem[anzx->e].nod[1],
              &elem[anzx->e].nod[2],
              &elem[anzx->e].nod[3],
              &elem[anzx->e].nod[4],
              &elem[anzx->e].nod[6],
              &elem[anzx->e].nod[7],
              &elem[anzx->e].nod[5]

              );
    }
    else printf("elem-type not known, nr of nodes:%d\n", n);
    anzx->etype[elem[anzx->e].type]++;
    anzx->e++;
  }

  do
  {
    length = frecord( handle, rec_str);
    if(compare(rec_str,"volumeelements",6)==6) break;
    if (rec_str[length] == (char)EOF) break;
  }while(1);

  /* jump the nr of elems */
  length = frecord( handle, rec_str);
  sscanf(rec_str, "%d", &sum);
  for(i=0; i<sum; i++)
  {
    length = frecord( handle, rec_str);
    //printf("volu:%s\n",rec_str);
    if (rec_str[length] == (char)EOF) break;

    if (anzx->e>=elem_field_size)
    {
      elem_field_size=anzx->e+100;
      if((elem=(Elements *)realloc((Elements *)elem,(elem_field_size+1)*sizeof(Elements)))==NULL)
      {
        printf("\n\n ERROR: realloc failed, elem-index:%d\n\n", anzx->e);
        return(-1);
      }
    }
    elem[anzx->e].nr    = anzx->e+1;
    elem[anzx->e].group = 1;
    elem[anzx->e].type  = 0;
    if (elem[anzx->e].nr >  anzx->emax)  anzx->emax=elem[anzx->e].nr;
    if (elem[anzx->e].nr <  anzx->emin)  anzx->emin=elem[anzx->e].nr;
    sscanf(rec_str, "%*s %d", &n );
    if(n==4)
    {
      elem[anzx->e].mat   = 3;
      elem[anzx->e].type  = 3;
      sscanf(rec_str, "%d %*s %d %d %d %d", &elem[anzx->e].mat, &elem[anzx->e].nod[0], &elem[anzx->e].nod[2], &elem[anzx->e].nod[1], &elem[anzx->e].nod[3] );
    }
    else if(n==10)
    {
      elem[anzx->e].mat   = 6;
      elem[anzx->e].type  = 6;
      sscanf(rec_str, "%d %*s  %d %d %d %d %d %d %d %d %d %d", &elem[anzx->e].mat, &elem[anzx->e].nod[0], &elem[anzx->e].nod[2], &elem[anzx->e].nod[1],
      &elem[anzx->e].nod[3], &elem[anzx->e].nod[6], &elem[anzx->e].nod[4], &elem[anzx->e].nod[7], &elem[anzx->e].nod[5], &elem[anzx->e].nod[9], &elem[anzx->e].nod[8] );
    }
    else printf("elem-type not known, nr of nodes:%d\n", n);
    anzx->etype[elem[anzx->e].type]++;
    anzx->e++;
  }


  do
  {
    length = frecord( handle, rec_str);
    if(compare(rec_str,"edgesegments",6)==6) break;
    if (rec_str[length] == (char)EOF) break;
  }while(1);

  /* jump the nr of elems */
  length = frecord( handle, rec_str);
  sscanf(rec_str, "%d", &sum);
  for(i=0; i<sum; i++)
  {
    length = frecord( handle, rec_str);
    //printf("edge:%s\n",rec_str);
    if (rec_str[length] == (char)EOF) break;

    if (anzx->e>=elem_field_size)
    {
      elem_field_size=anzx->e+100;
      if((elem=(Elements *)realloc((Elements *)elem,(elem_field_size+1)*sizeof(Elements)))==NULL)
      {
        printf("\n\n ERROR: realloc failed, elem-index:%d\n\n", anzx->e);
        return(-1);
      }
    }
    elem[anzx->e].nr    = anzx->e+1;
    elem[anzx->e].group = 1;
    elem[anzx->e].mat   = 1;
    elem[anzx->e].type  = 11;
    if (elem[anzx->e].nr >  anzx->emax)  anzx->emax=elem[anzx->e].nr;
    if (elem[anzx->e].nr <  anzx->emin)  anzx->emin=elem[anzx->e].nr;
    sscanf(rec_str, "%d %*s %d %d", &elem[anzx->e].mat, &elem[anzx->e].nod[0], &elem[anzx->e].nod[1] );
    anzx->etype[elem[anzx->e].type]++;
    anzx->e++;
  }

  do
  {
    length = frecord( handle, rec_str);
    if (rec_str[length] == (char)EOF) break;
    if(compare(rec_str,"points",4)==4) break;
  }while(1);

  /* jump the nr of points */
  length = frecord( handle, rec_str);
  sscanf(rec_str, "%d", &sum);
  for(i=0; i<sum; i++)
  {
    length = frecord( handle, rec_str);
    if (rec_str[length] == (char)EOF) break;
    //printf("node:%s\n",rec_str);
    //node[anzx->n].nr = anzx->n;
    node[anzx->n].nr = anzx->n+1;
    if (node[anzx->n].nr>=node_field_size)
    {
      node_field_size=node[anzx->n].nr+100;
      if ( (node = (Nodes *)realloc((Nodes *)node, (node_field_size+1) * sizeof(Nodes))) == NULL )
      {
        printf("\n\n ERROR: realloc failed, nodenr:%d\n\n", node[anzx->n].nr) ;
        return(-1);
      }
    }
    node[node[anzx->n].nr].indx=anzx->n;

    sscanf(rec_str,"%lf %lf %lf", &node[node[anzx->n].nr].nx,&node[node[anzx->n].nr].ny,&node[node[anzx->n].nr].nz);
    if (node[anzx->n].nr >  anzx->nmax)  anzx->nmax=node[anzx->n].nr;
    if (node[anzx->n].nr <  anzx->nmin)  anzx->nmin=node[anzx->n].nr;
    anzx->n++;
  }

  fclose(handle);

  node_field_size=anzx->nmax+1;
  if((node =(Nodes *)realloc( (Nodes *)node, node_field_size*sizeof(Nodes)))==NULL)
    printf("\n\n ERROR: realloc failed\n\n") ;
  else
    printf ("\n %d nodes reallocated \n",anzx->nmax);

  elem_field_size=anzx->e+1;
  if ( (elem = (Elements *)realloc((Elements *)elem, elem_field_size * sizeof(Elements))) == NULL )
    printf("\n\n ERROR: in readfrd realloc failed\n\n") ;
  else
    printf ("\n %d elements reallocated \n", anzx->e);

  if ( e_nmax > (anzx->nmax) )
  {
    printf ("\nWARNING: element requestes a nodename higher than allocated\n\n");
    printf (" e_nmax=%d e_nmin=%d\n", e_nmax, e_nmin );
  }
  if ( e_nmin < 1 )
  {
    printf ("\nWARNING: element requestes a nodename lower than allocated\n\n");
    printf (" e_nmax=%d e_nmin=%d\n", e_nmax, e_nmin );
  }


  for (i=0; i<anzx->e; i++)
  {
    sprintf(name,"%d",  elem[i].nr);
    sprintf(buffer, "+set%d", elem[i].mat );
    pre_setax( buffer, "e", name );
  }
  for (i=0; i<anzx->e; i++)
  {
    sprintf(name,"%d", elem[i].nr);
    sprintf(buffer, "+typ%d", elem[i].type );
    pre_setax( buffer, "e", name );
  }

  *sptr = setx;
  *nptr = node;
  *eptr = elem;
  return(1);
}

