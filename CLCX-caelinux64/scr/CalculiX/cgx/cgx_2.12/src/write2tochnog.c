
#include <extUtil.h>



int write2tochnog(char *datout, Summen *anz, Nodes *node, Elements *elem, Datasets *lcase )
{
  FILE *handle1;
  register int  i, lc;
  double betr;

  /* Open the files and check to see that it was opened correctly */
  handle1 = fopen (datout, "w+b");
  if (handle1==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n",
     datout); return(-1);}
  else  printf ("\n%s opened\n",datout);


  printf ("\n write tochnog file  \n");
  fprintf(handle1,"(\n\n)\necho -yes\nnumber_of_space_dimensions 3\nmateri_velocity\n");
  fprintf(handle1,"materi_strain_elasti\nmateri_stress\nend_initia\n");
  fprintf(handle1,"options_mesh -fixed_in_space -fixed_in_space -fixed_in_space\noptions_convection -no\n");
  fprintf(handle1,"group_type    0 -materi\ngroup_materi_elasti_young    0   29000.0\n");
  fprintf(handle1,"group_materi_elasti_poisson    0   .25\n");
  fprintf(handle1,"group_materi_memory    0 -updated_without_rotation\n"); 
  for (i=0; i<anz->n; i++)
  {
    fprintf (handle1, "node  %8d ", node[i].nr );

    betr= sqrt(node[node[i].nr].nx*node[node[i].nr].nx);
    if (betr > 9999.) fprintf (handle1, "%8.1f ", node[node[i].nr].nx);
    else if (betr > 999.) fprintf (handle1, "%8.2f ", node[node[i].nr].nx);
    else if (betr > 99.) fprintf (handle1, "%8.3f ", node[node[i].nr].nx);
    else
        fprintf (handle1, "%8.3f ", node[node[i].nr].nx);

    betr= sqrt(node[node[i].nr].ny*node[node[i].nr].ny);
    if (betr > 9999.) fprintf (handle1, "%8.1f  ", node[node[i].nr].ny);
    else if (betr > 999.) fprintf (handle1, "%8.2f  ", node[node[i].nr].ny);
    else if (betr > 99.) fprintf (handle1, "%8.3f  ", node[node[i].nr].ny);
    else
        fprintf (handle1, "%8.3f  ", node[node[i].nr].ny);

    betr= sqrt(node[node[i].nr].nz*node[node[i].nr].nz);
    if (betr > 9999.) fprintf (handle1, "%8.1f  ", node[node[i].nr].nz);
    else if (betr > 999.) fprintf (handle1, "%8.2f  ", node[node[i].nr].nz);
    else if (betr > 99.) fprintf (handle1, "%8.3f  ", node[node[i].nr].nz);
    else
        fprintf (handle1, "%8.3f  ", node[node[i].nr].nz);
    fprintf(handle1,"\n");
  }

  for (i=0; i<anz->e; i++)
  {
    if (elem[i].type == 1)
    {
      fprintf (handle1,"element %8d -hex8 %8d %8d %8d %8d %8d %8d %8d %8d \n",
        elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[3], elem[i].nod[2],
        elem[i].nod[4], elem[i].nod[5], elem[i].nod[7], elem[i].nod[6] );
    }
    else if (elem[i].type == 4)
    {
      fprintf (handle1, "element %8d -hex8 %8d %8d %8d %8d %8d %8d %8d %8d \n",
        elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[3], elem[i].nod[2],
        elem[i].nod[4], elem[i].nod[5], elem[i].nod[7], elem[i].nod[6] );
/*
      fprintf (handle1, "  %8d %8d %8d %8d %8d %8d %8d %8d \n",
        elem[i].nod[8],  elem[i].nod[9], elem[i].nod[10], elem[i].nod[11],
        elem[i].nod[12], elem[i].nod[13], elem[i].nod[14], elem[i].nod[15] );
      fprintf (handle1, "  %8d %8d %8d %8d \n",
        elem[i].nod[16], elem[i].nod[17], elem[i].nod[18], elem[i].nod[19] );
      fprintf (handle1, "  %8d %8d %8d %8d \n",
        elem[i].nod[20], elem[i].nod[21], elem[i].nod[22], elem[i].nod[23] );
      fprintf (handle1, "  %8d %8d %8d \n",
        elem[i].nod[24], elem[i].nod[25], elem[i].nod[26] );
*/
    }
    else if (elem[i].type == 9)
    {
      fprintf (handle1, "element %8d -quad4 %8d %8d %8d %8d \n",
        elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[3], elem[i].nod[2] );
    }
    else if (elem[i].type == 7)
    {
      fprintf (handle1, "element %8d -bar3 %8d   %8d  %8d \n",
        elem[i].nr, elem[i].nod[0], elem[i].nod[1],elem[i].nod[2] );
    }
    else if (elem[i].type == 11)
    {
      fprintf (handle1, "element %8d -bar2 %8d %8d \n",
        elem[i].nr, elem[i].nod[0], elem[i].nod[1] );
    }
    else
    {
      printf (" elem not a known type (%d)\n",  elem[i].type);
    }
  }

  for (lc=0; lc<anz->l; lc++)
  {
    if (lcase[lc].ncomps == 1)
    {
      for (i=0; i<anz->n; i++)
      {
        fprintf (handle1, "bounda_unknown %d %6d -temp\nbounda_time %d %6d 0 %8.3f 100 %8.3f \n",i+1, node[i].nr,i+1,
        node[i].nr, lcase[lc].dat[0][node[i].nr],lcase[lc].dat[0][node[i].nr]);
      }
    }
  }

  fprintf (handle1, "control_timestep            100  1. 1.\n");
  fprintf(handle1,"end_data\n");
  fclose(handle1);
  return (1);
}
