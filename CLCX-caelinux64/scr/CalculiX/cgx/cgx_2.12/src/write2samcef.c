/* -------------------------------------------------------------------------------------------------------------  */
/* write2samcef  : first issue  (04/18/2006) i.e. for SAMCEF(TM) solver (see http://www.samcef.com ; Paul CARRICO (paul.carrico@free.fr)*/
/* -------------------------------------------------------------------------------------------------------------  */
                                                                                                                   
#include <extUtil.h>
                                                                                                                   
int write2samcef( char *datout, Summen *anz, Nodes *node, Elements *elem, Datasets *lcase )
{
  FILE *handle1;
  int  i;

  /* Open the files and check to see that it was opened correctly */
  i=strlen(datout);
  strcpy (&datout[i], ".dat");
  handle1 = fopen (datout, "w+b");
  if (handle1==NULL) 
  {
    printf ("\nThe output file \"%s\" could not be opened.\n\n",datout);
    return(-1);
  }
  else  printf (" file %s opened\n",datout);

  printf ("\n write Samcef export file  \n");

  fprintf (handle1, "! *********************************\n");
  fprintf (handle1, "! This mesh comes from CGX mesher !\n");
  fprintf (handle1, "!    see http://www.calculix.de    \n");
  fprintf (handle1, "! *********************************\n");

  fprintf (handle1, "\n.del.*\n");
  fprintf (handle1, "\nMODE LECTURE 256\n");

  fprintf (handle1, "\n.NOE\n");
  for (i=0; i<anz->n; i++)
      {
  fprintf (handle1, "I %d X %12.5e Y %12.5e Z %12.5e\n", node[i].nr, node[node[i].nr].nx,
      node[node[i].nr].ny, node[node[i].nr].nz);
      }

  /* write elements (tria3&6 ; quad4&8; penta6&15, hexa_8&20) */
  fprintf (handle1, "\n.MAI\n"); 
  for (i=0; i<anz->e; i++)
  {
    if (elem[i].type == 1) // Hexa8
    {
      fprintf (handle1, "I %d ATT 1 0 N %d %d %d %d 0 $ \n %d %d %d %d\n",
        elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2], elem[i].nod[3],
        elem[i].nod[4], elem[i].nod[5], elem[i].nod[6], elem[i].nod[7] );
    }

    else if (elem[i].type == 2) // penta 6
    {
      fprintf (handle1, "I %d ATT 1 0 N %d %d %d 0 $ \n %d %d %d\n",
        elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2],
        elem[i].nod[3], elem[i].nod[4], elem[i].nod[5]);
    }

    else if (elem[i].type == 3) //tetra4
    {
      fprintf (handle1, "I %d ATT 1 0 N %d %d %d 0 $ \n %d \n",
        elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2],
        elem[i].nod[3]);
    }

    else if (elem[i].type == 4) // hexa20
    {
      fprintf (handle1, "I %d ATT 1 0 N %d -%d %d -%d $ \n %d -%d %d -%d 0 $ \n ",
        elem[i].nr, elem[i].nod[0], elem[i].nod[8], elem[i].nod[1], elem[i].nod[9],
        elem[i].nod[2], elem[i].nod[10], elem[i].nod[3], elem[i].nod[11]);
      fprintf (handle1, "%d -%d %d -%d $ \n %d -%d %d -%d 0 $\n",
        elem[i].nod[4], elem[i].nod[16], elem[i].nod[5],elem[i].nod[17], elem[i].nod[6],
        elem[i].nod[18], elem[i].nod[7], elem[i].nod[19]);
      fprintf (handle1, "-%d 0 -%d 0 -%d 0 -%d \n",
        elem[i].nod[12], elem[i].nod[13], elem[i].nod[14], elem[i].nod[15]);
    }
    else if (elem[i].type == 5) // penta15
    {
      fprintf (handle1, "I %d ATT 1 0 N %d -%d %d -%d $\n %d -%d 0 %d -%d $\n",
        elem[i].nr, elem[i].nod[0], elem[i].nod[6], elem[i].nod[1],
        elem[i].nod[7], elem[i].nod[2], elem[i].nod[8], elem[i].nod[3],
        elem[i].nod[12]);
      fprintf (handle1, "%d -%d %d -%d 0 $\n -%d 0 -%d 0 -%d\n",
        elem[i].nod[4], elem[i].nod[13], elem[i].nod[5], elem[i].nod[14], elem[i].nod[9],
        elem[i].nod[10], elem[i].nod[11] );
    }
    else if (elem[i].type == 6) // tetra10
    {
      fprintf (handle1, "I %d ATT 1 0 N %d -%d %d -%d $\n %d -%d 0 %d 0 -%d 0 $\n -%d 0 -%d\n",
        elem[i].nr, elem[i].nod[0], elem[i].nod[4], elem[i].nod[1],
        elem[i].nod[5], elem[i].nod[2], elem[i].nod[6], elem[i].nod[3],
        elem[i].nod[7], elem[i].nod[8], elem[i].nod[9]);
    }
    else if (elem[i].type == 7) // tria3
    {
      fprintf (handle1, "I %d ATT 1 0 N %d %d %d\n",
        elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2] );
    }
    else if (elem[i].type == 8) // tria6
    {
      fprintf (handle1, "I %d ATT 1 0 N %d -%d %d -%d $\n %d -%d\n",
        elem[i].nr, elem[i].nod[0], elem[i].nod[3], elem[i].nod[1],
        elem[i].nod[4], elem[i].nod[2], elem[i].nod[5]);
    }
    else if (elem[i].type == 9) // quad4
    {
      fprintf (handle1, "I %d ATT 1 0 N %d %d %d %d\n",
        elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2], elem[i].nod[3] );
    }
    else if (elem[i].type == 10) // quad8
    {
      fprintf (handle1, "I %d ATT 1 0 N  %d -%d %d -%d $\n %d -%d %d -%d\n"
      , elem[i].nr, elem[i].nod[0], elem[i].nod[4], elem[i].nod[1], elem[i].nod[5]
      , elem[i].nod[2], elem[i].nod[6], elem[i].nod[3], elem[i].nod[7]);
    }
    else if (elem[i].type == 11) //seg2
    {
      fprintf (handle1, "I %d ATT 1 0 N %d %d  \n",
        elem[i].nr, elem[i].nod[0], elem[i].nod[1] );
    }
    else if (elem[i].type == 12) // seg3
    {
      fprintf (handle1, "I %d ATT 1 0 N %d -%d  %d \n",
        elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2] );
    }
    else
      {
      printf (" WARNING: elem(%d) not a known type (%d)\n", elem[i].nr, elem[i].type);
      }
  }

  fprintf (handle1, "\nRETURN\n");

  fclose(handle1);
  return (1);
}

