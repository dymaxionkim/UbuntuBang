/* ---------------------------------------------------------------------------------------------  */
/* write2nas schreibt das frd-file aus einer struktur  20.03.1998 Wittig   */
/* ---------------------------------------------------------------------------------------------  */

#include <extUtil.h>



int write2nas(char *datout, Summen *anz, Nodes *node, Elements *elem, Datasets *lcase )
{
  FILE *handle1, *handle2;
  int  i, lc;

  /* Open the files and check to see that it was opened correctly */
  i=strlen(datout);
  strcpy (&datout[i], ".bdf");
  handle1 = fopen (datout, "w+b");
  if (handle1==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n",
     datout); return(-1);}
  else  printf (" file %s opened\n",datout);

  strcpy (&datout[i], ".temp");
  handle2 = fopen (datout, "w+b");
  if (handle2==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n",
     datout); return(-1);}
  else  printf (" file %s opened\n",datout);


  printf ("\n write nastran file \n");
  for (i=0; i<anz->e; i++) if(elem[i].mat<1)
  { printf(" ERROR: element:%d has no valid PID:%d, please use 'mata' to assign PIDs to sets.\n", elem[i].nr,elem[i].mat); return(0); }

  for (i=0; i<anz->n; i++)
  {

    fprintf (handle1, "GRID*   %-16d%-16d", node[i].nr, 0);
    fprintf (handle1, "% 16.9e% 16.9e\n", node[node[i].nr].nx, node[node[i].nr].ny);
    fprintf (handle1, "*       % 16.9e%16d\n", node[node[i].nr].nz, node[node[i].nr].pflag);

    /*
    fprintf (handle1, "GRID    %8d%8d", node[i].nr, 0);

    betr= dabs(node[node[i].nr].nx)+1.e-1;
    if (betr >= 100000.) fprintf (handle1, "%8e", node[node[i].nr].nx);
    else if (betr >= 10000.) fprintf (handle1, "%8.1lf", node[node[i].nr].nx);
    else if (betr >= 1000.) fprintf (handle1, "%8.2lf", node[node[i].nr].nx);
    else if (betr >= 100.) fprintf (handle1, "%8.3lf", node[node[i].nr].nx);
    else if (betr >= 10.) fprintf (handle1, "%8.4lf", node[node[i].nr].nx);
    else  fprintf (handle1, "%8.5lf", node[node[i].nr].nx);

    betr= dabs(node[node[i].nr].ny)+1.e-1;
    if (betr >= 100000.) fprintf (handle1, "%8e", node[node[i].nr].ny);
    else if (betr >= 10000.) fprintf (handle1, "%8.1lf", node[node[i].nr].ny);
    else if (betr >= 1000.) fprintf (handle1, "%8.2lf", node[node[i].nr].ny);
    else if (betr >= 100.) fprintf (handle1, "%8.3lf", node[node[i].nr].ny);
    else if (betr >= 10.) fprintf (handle1, "%8.4lf", node[node[i].nr].ny);
    else  fprintf (handle1, "%8.5f", node[node[i].nr].ny);

    betr= dabs(node[node[i].nr].nz)+1.e-1;
    if (betr >= 100000.) fprintf (handle1, "%8e", node[node[i].nr].nz);
    else if (betr >= 10000.) fprintf (handle1, "%8.1lf", node[node[i].nr].nz);
    else if (betr >= 1000.) fprintf (handle1, "%8.2lf", node[node[i].nr].nz);
    else if (betr >= 100.) fprintf (handle1, "%8.3lf", node[node[i].nr].nz);
    else if (betr >= 10.) fprintf (handle1, "%8.4lf", node[node[i].nr].nz);
    else  fprintf (handle1, "%8.5lf", node[node[i].nr].nz);

    fprintf (handle1, "%8d\n", node[node[i].nr].pflag );
    */
  }

  for (i=0; i<anz->e; i++)
  {
    if (elem[i].type == 1)
    {
      fprintf (handle1, "CHEXA   %8d%-8d%-8d%-8d%-8d%-8d%-8d%-8d X%-6d\n",
        elem[i].nr, elem[i].mat, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2],
        elem[i].nod[3], elem[i].nod[4], elem[i].nod[5], elem[i].nr);
      fprintf (handle1, "+X%-6d%-8d%-8d\n",
        elem[i].nr, elem[i].nod[6], elem[i].nod[7]);
    }
    else if (elem[i].type == 3)
    {
      fprintf (handle1, "CTETRA  %8d%-8d%-8d%-8d%-8d%-8d\n",
        elem[i].nr, elem[i].mat, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2],
        elem[i].nod[3]);
    }
    else if (elem[i].type == 4)
    {
      fprintf (handle1, "CHEXA   %8d%-8d%-8d%-8d%-8d%-8d%-8d%-8d \n",
        elem[i].nr, elem[i].mat, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2],
        elem[i].nod[3], elem[i].nod[4], elem[i].nod[5]);
      fprintf (handle1, "        %-8d%-8d%-8d%-8d%-8d%-8d%-8d%-8d\n",
        elem[i].nod[6],  elem[i].nod[7] , elem[i].nod[8],  elem[i].nod[9],
        elem[i].nod[10], elem[i].nod[11], elem[i].nod[12], elem[i].nod[13] );
      fprintf (handle1, "        %-8d%-8d%-8d%-8d%-8d%-8d\n",
        elem[i].nod[14], elem[i].nod[15], elem[i].nod[16], elem[i].nod[17],
        elem[i].nod[18], elem[i].nod[19] );
    }
    else if (elem[i].type == 6)
    {
      fprintf (handle1, "CTETRA  %8d%-8d%-8d%-8d%-8d%-8d%-8d%-8d X%-6d\n",
        elem[i].nr, elem[i].mat, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2],
        elem[i].nod[3], elem[i].nod[4], elem[i].nod[5], elem[i].nr);
      fprintf (handle1, "+X%-6d%-8d%-8d%-8d%-8d\n",
        elem[i].nr, elem[i].nod[6], elem[i].nod[7], elem[i].nod[8], elem[i].nod[9]);
    }
    else if (elem[i].type == 7)
    {
      fprintf (handle1, "CTRIA3  %8d%-8d%-8d%-8d%-8d\n",
        elem[i].nr, elem[i].mat, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2]);
    }
    else if (elem[i].type == 8)
    {
      fprintf (handle1, "CTRIA6  %8d%-8d%-8d%-8d%-8d%-8d%-8d%-8d\n",
        elem[i].nr, elem[i].mat, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2],
        elem[i].nod[3], elem[i].nod[4], elem[i].nod[5]);
    }
    else if (elem[i].type == 9)
    {
      fprintf (handle1, "CQUAD4  %8d%-8d%-8d%-8d%-8d%-8d\n",
        elem[i].nr, elem[i].mat, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2],
        elem[i].nod[3]);
    }
    else if (elem[i].type == 10)
    {
      fprintf (handle1, "CQUAD8  %8d%-8d%-8d%-8d%-8d%-8d%-8d%-8d X%-6d\n",
        elem[i].nr, elem[i].mat, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2],
        elem[i].nod[3], elem[i].nod[4], elem[i].nod[5], elem[i].nr);
      fprintf (handle1, "+X%-6d%-8d%-8d\n",
        elem[i].nr, elem[i].nod[6], elem[i].nod[7]);
    }
    else
    {
      printf (" elem(%d) not a known type (%d)\n", elem[i].nr, elem[i].type);
    }
  }

  for (lc=0; lc<anz->l; lc++)
  {
    if (lcase[lc].ncomps == 1)
    {
      for (i=0; i<anz->n; i++)
      {
        fprintf (handle2, "TEMP    %-8d%-8d%8.3f\n", lc+1, node[i].nr,
        lcase[lc].dat[0][node[i].nr]);
      }
    }
  }

  fclose(handle1);
  fclose(handle2);
  return (1);
}

