
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

#define MM_TO_M  0.001
#define DT_KELVIN_TO_C -273.15

int write2darwin( char *datout, Summen *anz, Nodes *node, Elements *elem, Datasets *lcase, char **dat )
{
  FILE *handle1=NULL, *handle2=NULL;
  int  lct, i,j, lc, lendatout, step_number_buf=-1;
  int seff=10;       //PS1
  int formatFlag=0;  //0: v7.0

  /* version 7.1 */
  if((dat!=0)&&( (compare( dat[0], "v7.1", 4)>= 4)||(compare( dat[1], "v7.1", 4)>= 4) )) formatFlag=71;

  if(formatFlag==71)
  {
    printf (" write darwin data for version 7.1 (not compatible for older versions)\n");
    printf (" Geometry is assumed to be in mm and not converted, stresses in MPa and temperatures are reduced by:%f (K->C)\n", DT_KELVIN_TO_C);
  }
  else
  {
    printf (" Geometry is assumed to be in mm and converted to m (*%f), stresses in MPa and temperatures are reduced by:%f (K->C)\n", MM_TO_M, DT_KELVIN_TO_C);
    printf (" REMARK: Specify 'v7.1' as the last parameter in your command to switch to darwin 7.1 format\n");
  }
  lendatout=strlen(datout);

  /* open the mesh file */
  if(1)
  {
    strcpy (&datout[lendatout], ".uif");
    handle1 = fopen (datout, "w+b");
    if (handle1==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n",
       datout); return(-1);}
    else  printf (" file %s opened\n",datout);
  
    fprintf (handle1, "NLIM\nBNOD BELM\n%d %d\n", anz->nmax, anz->emax);

    if(formatFlag==71)
    {
      fprintf (handle1, "ANLS\n");
      fprintf (handle1, "DRWN VERSION 7.1\n");
      fprintf (handle1, "DRWN GEOM_UNITS MM\n");
      fprintf (handle1, "END\n");
      fprintf (handle1, "NODE\nNAME  X   Y  Z\n");
      for (i=0; i<anz->n; i++)
      {
        fprintf (handle1, "%8d %.12e %.12e %.12e\n", node[i].nr, node[node[i].nr].nx,
        node[node[i].nr].ny, node[node[i].nr].nz);
       }
    }
    else
    {
      fprintf (handle1, "NODE\nNAME  X   Y  Z\n");
      for (i=0; i<anz->n; i++)
      {
        fprintf (handle1, "%8d %.12e %.12e %.12e\n", node[i].nr, node[node[i].nr].nx*MM_TO_M,
        node[node[i].nr].ny*MM_TO_M, node[node[i].nr].nz*MM_TO_M);
       }
    }

  
    for (i=0; i<anz->e; i++)
    {
      if (elem[i].type == 1)
      {
        if((i==0) || ((i>0)&&(elem[i-1].type!=elem[i].type)) || ((i>0)&&(elem[i-1].attr!=elem[i].attr)))
        {
          fprintf (handle1, "BRI8\nNAME  CONN\n");
        }
        fprintf (handle1, "%6d %6d %6d %6d %6d %6d %6d %6d %6d\n",
          elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2],
          elem[i].nod[3], elem[i].nod[4], elem[i].nod[5], elem[i].nod[6], elem[i].nod[7]);
      }
      else if (elem[i].type == 4)
      {
        if((i==0) || ((i>0)&&(elem[i-1].type!=elem[i].type)) || ((i>0)&&(elem[i-1].attr!=elem[i].attr)))
        {
          fprintf (handle1, "VANS N20\nNAME  CONN\n");
        }
        fprintf (handle1, "%6d %6d %6d %6d %6d %6d %6d %6d %6d %6d %6d ",
          elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2],
          elem[i].nod[3], elem[i].nod[4], elem[i].nod[5], elem[i].nod[6],
          elem[i].nod[7], elem[i].nod[8], elem[i].nod[9]);
        fprintf (handle1, "%6d %6d %6d %6d %6d %6d %6d %6d %6d %6d\n",
          elem[i].nod[10], elem[i].nod[11], elem[i].nod[16], elem[i].nod[17],
          elem[i].nod[18], elem[i].nod[19], elem[i].nod[13],
          elem[i].nod[14], elem[i].nod[15], elem[i].nod[12] );
      }
      else if (elem[i].type == 6)
      {
        if((i==0) || ((i>0)&&(elem[i-1].type!=elem[i].type)) || ((i>0)&&(elem[i-1].attr!=elem[i].attr)))
        {
          fprintf (handle1, "TETS N10\nNAME  CONN\n");
        }
        fprintf (handle1, "%6d %6d %6d %6d %6d %6d %6d %6d %6d %6d %6d \n",
          elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2],
          elem[i].nod[3], elem[i].nod[4], elem[i].nod[5], elem[i].nod[6],
          elem[i].nod[7], elem[i].nod[8], elem[i].nod[9]);
      }
      else if (elem[i].type == 7)
      {
        if((i==0) || ((i>0)&&(elem[i-1].type!=elem[i].type)) || ((i>0)&&(elem[i-1].attr!=elem[i].attr)))
        {
          fprintf (handle1, "EL2D\nNAME  CONN\n");
        }
        fprintf (handle1, "%6d %6d %6d %6d %6d\n",
          elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2], elem[i].nod[2] );
      }
      else if (elem[i].type == 8)
      {
        if((i==0) || ((i>0)&&(elem[i-1].type!=elem[i].type)) || ((i>0)&&(elem[i-1].attr!=elem[i].attr)))
        {
          fprintf (handle1, "PE2D\nNAME  CONN\n");
        }
        fprintf (handle1, "%6d %6d %6d %6d %6d %6d %6d %6d\n",
          elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2], elem[i].nod[2],
          elem[i].nod[3], elem[i].nod[4], elem[i].nod[5]);
      }
      else if (elem[i].type == 9)
      {
        if((i==0) || ((i>0)&&(elem[i-1].type!=elem[i].type)) || ((i>0)&&(elem[i-1].attr!=elem[i].attr)))
        {
          fprintf (handle1, "EL2D\nNAME  CONN\n");
        }
        fprintf (handle1, "%6d %6d %6d %6d %6d\n",
          elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2], elem[i].nod[3] );
      }
      else if (elem[i].type == 10)
      {
        if((i==0) || ((i>0)&&(elem[i-1].type!=elem[i].type)) || ((i>0)&&(elem[i-1].attr!=elem[i].attr)))
        {
          fprintf (handle1, "PE2D\nNAME  CONN\n");
        }
        fprintf (handle1, "%6d %6d %6d %6d %6d %6d %6d %6d %6d\n",
          elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2],
          elem[i].nod[3], elem[i].nod[4], elem[i].nod[5], elem[i].nod[6], elem[i].nod[7]);
      }
      else
      {
        printf (" WARNING: elem(%d) not a known type (%d)\n", elem[i].nr, elem[i].type);
      }
    }
    fclose(handle1);
  }

  /* open node-value file */
  if((anz->l>0)&&(dat!=0)&&(compare( dat[0], "ds", 2)>= 2))
  {
    strcpy (&datout[lendatout], ".uof");
    handle2 = fopen (datout, "w+b");
    if (handle2==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n", datout); return(-1);}
    else  printf (" file %s opened\n",datout);

    if(compare( dat[1], "e", 1)>= 1)
    {
      seff=atoi(&dat[1][1])-1;
    }
   
    /* version 7.1 */
    if(formatFlag==71)
    {
      fprintf (handle2, "SIZE\nNSIG NTMP\n1  1 \n");
      fprintf (handle2, "ANLS\n");
      fprintf (handle2, "DRWN VERSION 7.1\n");
      fprintf (handle2, "DRWN STRESS_UNITS MPA\n");
      fprintf (handle2, "DRWN TEMP_UNITS C\n");
      fprintf (handle2, "END\n");
    }
    /* end version 7.1 */

    j=0;
    for (lc=0; lc<anz->l; lc++)
    {
      if((strlen(dat[0])>2) && (atoi(&dat[0][2])!=lc+1)) goto next;;
  
      if(compare( lcase[lc].name, "STRESS", 6) == 6)
      {
        printf("step_number:%d\n", lcase[lc].step_number);
        printf("write:%s ds:%d with SEFF set to entity:%s\n", lcase[lc].name, lc+1, lcase[lc].compName[seff]);
        if(step_number_buf!=lcase[lc].step_number)
	{
          j++;
          step_number_buf=lcase[lc].step_number;
          fprintf (handle2, "$ ---  LC%d ---\n", j);
          fprintf (handle2, "CASE  IDNT\n");
          fprintf (handle2, "time %lf\n",lcase[lc].value );
          //fprintf (handle2, "NAME CVAL\n%d\n", lcase[lc].step_number);
          fprintf (handle2, "NAME CVAL\n%d\n", j);
	}

        /* SEFF is set to PS1[10] mises is[6] */
        //fprintf (handle2, "NODE SIG\nNAME  S11  S22  S33  S12  S23  S13  SEFF\n");
        fprintf (handle2, "NODE SIG\nNAME  S11  S22  S33  S12  S13  S23  SEFF\n");
        for (i=0; i<anz->n; i++)
        {
          fprintf (handle2, "%8d %12.5f %12.5f %12.5f %12.5f %12.5f %12.5f %12.5f\n", node[i].nr,
	  lcase[lc].dat[0][node[i].nr],lcase[lc].dat[1][node[i].nr],lcase[lc].dat[2][node[i].nr],
	  lcase[lc].dat[3][node[i].nr],lcase[lc].dat[5][node[i].nr],lcase[lc].dat[4][node[i].nr],lcase[lc].dat[seff][node[i].nr]);
        } 
 
        /* search for related temps */      
        //if(lc>0) { lct=lc-1; while((lcase[lct].step_number==lcase[lc].step_number)&&(lct>0)) lct--; lct++; }
        if(lc) { for (lct=lc-1; lct>=0; lct--) { if(lcase[lct].step_number!=lcase[lc].step_number) break; } lct++; }
        else lct=1;
        while((lct<anz->l)&&(lcase[lct].step_number==lcase[lc].step_number))
        {
          if( (compare( lcase[lct].name, "NDTEMP", 6) == 6)||( compare( lcase[lct].name, "TEMP", 4) == 4)||( compare( lcase[lct].name, "TT3D", 4) == 4))
          {
            printf("write:%s ds:%d\n", lcase[lct].name, lct+1);
            fprintf (handle2, "NODE TEMP\nNAME TEMP\n");
            /* check if the data of the specified lcase (Dataset) are already available */
            if (!lcase[lct].loaded)
	    {
              printf("ERROR in write2darwin: data for Dataset:%d not available\n", lct+1); 
              goto next;
            }
               
            for (i=0; i<anz->n; i++)
            {
              fprintf (handle2, "%8d %12.5f\n", node[i].nr, lcase[lct].dat[0][node[i].nr]+ DT_KELVIN_TO_C);
            }
            break;       
    	  }
          lct++;
        } 
      }
      next:;
    }
    fclose(handle2);
  }
  return (1);
}

