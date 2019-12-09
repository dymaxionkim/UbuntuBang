/* -------------------------------------------------------------------------------------------------------------  */
/* write2aster  i.e. for CODE_ASTER solver (see http://www.code-aster.org ; Paul CARRICO (paul.carrico@free.fr)   */
/* -------------------------------------------------------------------------------------------------------------  */
                                                                                                                   
#include <extUtil.h>
                                                                                                                   
int write2aster( char *datout, Summen *anz, Nodes *node, Elements *elem, Sets *set, Datasets *lcase ) //TODD
{
  FILE *handle1;
  int  i;
  int setIndex;  //TODD
  int length;  //TODD
  char *group;  //TODD
  const int FIELDS_PER_LINE=8;  //TODD

  /* Open the files and check to see that it was opened correctly */
  i=strlen(datout);
  strcpy (&datout[i], ".mail");
  handle1 = fopen (datout, "w+b");
  if (handle1==NULL) 
  {
    printf ("\nThe output file \"%s\" could not be opened.\n\n",datout);
    return(-1);
  }
  else  printf (" file %s opened\n",datout);

  printf ("\n write Code Aster file  \n");

  fprintf (handle1, "TITRE\n");
  fprintf (handle1, "This mesh comes from CGX mesher !\n");
  fprintf (handle1, "see http://www.calculix.de\n");
  fprintf (handle1, "FINSF\n");

  fprintf (handle1, "COOR_3D\n");
  for (i=0; i<anz->n; i++)
      {
		  fprintf (handle1, "N%d %lf %lf %lf\n", node[i].nr, node[node[i].nr].nx,node[node[i].nr].ny, node[node[i].nr].nz);//TODD
//TODD  fprintf (handle1, "N%d %12.5e %12.5e %12.5e\n", node[i].nr, node[node[i].nr].nx,
//TODD      node[node[i].nr].ny, node[node[i].nr].nz);
      }
  /* fprintf (handle1, "FINSF\n");
  fprintf (handle1, "%\n"); */

  /* write elements (tria3&6 ; quad4&8; penta6&15, hexa_8&20) */

  for (i=0; i<anz->e; i++)
  {
    if (elem[i].type == 1)
    {
      if ( ((i>0) && (elem[i-1].type != elem[i].type)) || (i==0))
      {
      fprintf (handle1, "FINSF\n");
      fprintf (handle1, "HEXA8\n");
      }
      fprintf (handle1, "M%d N%d N%d N%d N%d \nN%d N%d N%d N%d\n",
        elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2], elem[i].nod[3],
        elem[i].nod[4], elem[i].nod[5], elem[i].nod[6], elem[i].nod[7] );
    }
    else if (elem[i].type == 2)
    {
      if ( ((i>0) && (elem[i-1].type != elem[i].type)) || (i==0))
      {
      fprintf (handle1, "FINSF\n");
      fprintf (handle1, "PENTA6\n");
      }
      fprintf (handle1, "M%d N%d N%d N%d N%d \nN%d N%d\n",
        elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2],
        elem[i].nod[3], elem[i].nod[4], elem[i].nod[5]);
    }
    else if (elem[i].type == 3)
    {
      if ( ((i>0) && (elem[i-1].type != elem[i].type)) || (i==0))
      {
      fprintf (handle1, "FINSF\n");
      fprintf (handle1, "TETRA4\n");
      }
      fprintf (handle1, "M%d N%d N%d N%d N%d \n",
        elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2],
        elem[i].nod[3]);
    }
    else if (elem[i].type == 4)
    {
      if ( ((i>0) && (elem[i-1].type != elem[i].type)) || (i==0))
      {
      fprintf (handle1, "FINSF\n");
      fprintf (handle1, "HEXA20\n");
      }
      fprintf (handle1, "M%d N%d N%d N%d N%d \nN%d N%d N%d N%d N%d\n",
        elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2], elem[i].nod[3],
        elem[i].nod[4], elem[i].nod[5], elem[i].nod[6], elem[i].nod[7], elem[i].nod[8] );
      fprintf (handle1, "N%d N%d N%d N%d N%d \nN%d N%d N%d N%d N%d\n",
        elem[i].nod[9], elem[i].nod[10], elem[i].nod[11],elem[i].nod[12], elem[i].nod[13],
        elem[i].nod[14], elem[i].nod[15], elem[i].nod[16], elem[i].nod[17], elem[i].nod[18] );
      fprintf (handle1, "N%d \n",
        elem[i].nod[19] );
    }
    else if (elem[i].type == 5)
    {
      if ( ((i>0) && (elem[i-1].type != elem[i].type)) || (i==0))
      {
      fprintf (handle1, "FINSF\n");
      fprintf (handle1, "PENTA15\n");
      }
      fprintf (handle1, "M%d N%d N%d N%d N%d \nN%d N%d N%d N%d N%d \n",
        elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2],
        elem[i].nod[3], elem[i].nod[4], elem[i].nod[5], elem[i].nod[6],
        elem[i].nod[7], elem[i].nod[8]);
      fprintf (handle1, "N%d N%d N%d N%d N%d \nN%d\n",
        elem[i].nod[12], elem[i].nod[13], elem[i].nod[14], elem[i].nod[9],
        elem[i].nod[10], elem[i].nod[11] );
    }
    else if (elem[i].type == 6)
    {
      if ( ((i>0) && (elem[i-1].type != elem[i].type)) || (i==0))
      {
      fprintf (handle1, "FINSF\n");
      fprintf (handle1, "TETRA10\n");
      }
      fprintf (handle1, "M%d N%d N%d N%d N%d \nN%d N%d N%d N%d N%d N%d\n",
        elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2],
        elem[i].nod[3], elem[i].nod[4], elem[i].nod[5], elem[i].nod[6],
        elem[i].nod[7], elem[i].nod[8], elem[i].nod[9]);
    }
    else if (elem[i].type == 7)
    {
      if ( ((i>0) && (elem[i-1].type != elem[i].type)) || (i==0))
      {
      fprintf (handle1, "FINSF\n");
      fprintf (handle1, "TRIA3\n");
      }
      fprintf (handle1, "M%d N%d N%d N%d\n",
        elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2] );
    }
    else if (elem[i].type == 8)
    {
      if ( ((i>0) && (elem[i-1].type != elem[i].type)) || (i==0))
      {
      fprintf (handle1, "FINSF\n");
      fprintf (handle1, "TRIA6\n");
      }
      fprintf (handle1, "M%d N%d N%d N%d N%d N%d N%d\n",
        elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2],
        elem[i].nod[3], elem[i].nod[4], elem[i].nod[5]);
    }
    else if (elem[i].type == 9)
    {
      if ( ((i>0) && (elem[i-1].type != elem[i].type)) || (i==0))
      {
      fprintf (handle1, "FINSF\n");
      fprintf (handle1, "QUAD4\n");
      }
      fprintf (handle1, "M%d N%d N%d N%d N%d\n",
        elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2], elem[i].nod[3] );
    }
    else if (elem[i].type == 10)
    {
      if ( ((i>0) && (elem[i-1].type != elem[i].type)) || (i==0))
      {
      fprintf (handle1, "FINSF\n");
      fprintf (handle1, "QUAD8\n");
      }
      fprintf (handle1, "M%d N%d N%d N%d N%d \nN%d N%d N%d N%d\n"
      , elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2], elem[i].nod[3]
      , elem[i].nod[4], elem[i].nod[5], elem[i].nod[6], elem[i].nod[7]);
    }
    else if (elem[i].type == 11)
    {
      if ( ((i>0) && (elem[i-1].type != elem[i].type)) || (i==0))
      {
      fprintf (handle1, "FINSF\n");
      fprintf (handle1, "SEG2\n");
      }
      fprintf (handle1, "M%d N%d N%d  \n",
        elem[i].nr, elem[i].nod[0], elem[i].nod[1] );
    }
    else if (elem[i].type == 12)
    {
      if ( ((i>0) && (elem[i-1].type != elem[i].type)) || (i==0))
      {
      fprintf (handle1, "FINSF\n");
      fprintf (handle1, "SEG3\n");
      }
      fprintf (handle1, "M%d N%d N%d N%d \n",
        elem[i].nr, elem[i].nod[0], elem[i].nod[2], elem[i].nod[1] );
    }
    else
      {
      printf (" WARNING: elem(%d) not a known type (%d)\n", elem[i].nr, elem[i].type);
      }
  }
  //TODD - start
	/* output node and element groups, when exporting whole mesh */				
	length=strlen(anz->model);
	if (compare(anz->model,"all",length)==length)
	{
		for (setIndex=0; setIndex<anz->sets; setIndex++)
		{
		    if((set[setIndex].name==(char *)NULL)||(set[setIndex].type==1)) continue;
			group=set[setIndex].name;
			length=strlen(group);
			/* ignore group "all" and type groups, which start with "+" */
			if(('+'!=group[0]) & (compare(group,"all",length)!=length))
			{
				i=0;
				/* write element group */				
				if (set[setIndex].anz_e > 0)
				{
					fprintf(handle1, "FINSF\n");					
				fprintf(handle1, "GROUP_MA\n");
					fprintf(handle1, "  %.8s\n",group); //truncate group name to 8 characters
					for (i=0; i < set[setIndex].anz_e; i++)
					{
						fprintf(handle1, "M%-8d", set[setIndex].elem[i]);
						if ((i+1) % FIELDS_PER_LINE==0)
						{
							putc('\n',handle1);
						}
					}
				}
				else
				{
					/* write node group */
					if (set[setIndex].anz_n > 0)
          {
						fprintf(handle1, "FINSF\n");
						fprintf(handle1, "GROUP_NO\n");
					  fprintf(handle1, "  %.8s\n",group); //truncate group name to 8 characters
						for (i=0; i < set[setIndex].anz_n; i++)
						{
							fprintf(handle1, "N%-8d", set[setIndex].node[i]);
							if ((i+1) % FIELDS_PER_LINE==0)
							{
								putc('\n',handle1);
							}
						}
					}
				}
				/* start a new line for the next group */
				if (i % FIELDS_PER_LINE!=0)
				{
					putc('\n',handle1);
				}
			}
		}
	}
  //TODD - end
  fprintf (handle1, "FINSF\n");
  fprintf (handle1, "FIN\n");

  fclose(handle1);
  return (1);
}

