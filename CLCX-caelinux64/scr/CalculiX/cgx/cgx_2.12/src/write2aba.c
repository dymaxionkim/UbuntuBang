
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

int write2aba( char *datout, Summen *anz, Nodes *node, Elements *elem, Datasets *lcase, char **dat )
{
  FILE *handle1=NULL, *handle2=NULL;
  int  i,j, lc, n, lendatout;
  double oldtime=0, iniTime=0.001, time1=0, time2=0, frc1=0, frc2=0, tmin, tmax;
  char inletflag=0;
  /* Open the files and check to see that it was opened correctly */


  printf (" write abaqus data \n");


  lendatout=strlen(datout);

  /* open the mesh file */
  if(dat==0)
  {
    strcpy (&datout[lendatout], ".msh");
    handle1 = fopen (datout, "w+b");
    if (handle1==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n",
       datout); return(-1);}
    else  printf (" file %s opened\n",datout);
  
    fprintf (handle1, "*NODE, NSET=N%s\n", anz->model);
    for (i=0; i<anz->n; i++)
    {
      fprintf (handle1, "%8d,%.12e,%.12e,%.12e\n", node[i].nr, node[node[i].nr].nx,
      node[node[i].nr].ny, node[node[i].nr].nz);
    }
  
    for (i=0; i<anz->e; i++)
    {
      if (elem[i].type == 1)
      {
        if((i==0) || ((i>0)&&(elem[i-1].type!=elem[i].type)) || ((i>0)&&(elem[i-1].attr!=elem[i].attr)))
        {
          if (elem[i].attr==1) fprintf (handle1, "*ELEMENT, TYPE=C3D8R, ELSET=E%s\n", anz->model);
          else if (elem[i].attr==2)  fprintf (handle1, "*ELEMENT, TYPE=C3D8I, ELSET=E%s\n", anz->model);
          else if (elem[i].attr==7) fprintf (handle1, "*ELEMENT, TYPE=F3D8, ELSET=E%s\n", anz->model);
          else                 fprintf (handle1, "*ELEMENT, TYPE=C3D8, ELSET=E%s\n", anz->model);
        }
        fprintf (handle1, "%6d,%6d,%6d,%6d,%6d,%6d,%6d,%6d,%6d\n",
          elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2],
          elem[i].nod[3], elem[i].nod[4], elem[i].nod[5], elem[i].nod[6], elem[i].nod[7]);
      }
      else if (elem[i].type == 2)
      {
        if((i==0) || ((i>0)&&(elem[i-1].type!=elem[i].type)) || ((i>0)&&(elem[i-1].attr!=elem[i].attr)))
        {
          if (elem[i].attr==7) fprintf (handle1, "*ELEMENT, TYPE=F3D6, ELSET=E%s\n", anz->model);
          else fprintf (handle1, "*ELEMENT, TYPE=C3D6, ELSET=E%s\n", anz->model);
        }
        fprintf (handle1, "%6d,%6d,%6d,%6d,%6d,%6d,%6d\n",
          elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2],
          elem[i].nod[3], elem[i].nod[4], elem[i].nod[5]);
      }
      else if (elem[i].type == 3)
      {
        if((i==0) || ((i>0)&&(elem[i-1].type!=elem[i].type)) || ((i>0)&&(elem[i-1].attr!=elem[i].attr)))
        {
          if (elem[i].attr==7) fprintf (handle1, "*ELEMENT, TYPE=F3D4, ELSET=E%s\n", anz->model);
          else fprintf (handle1, "*ELEMENT, TYPE=C3D4, ELSET=E%s\n", anz->model);
        }
        fprintf (handle1, "%6d,%6d,%6d,%6d,%6d \n",
          elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2],
          elem[i].nod[3]);
      }
      else if (elem[i].type == 4)
      {
        if((i==0) || ((i>0)&&(elem[i-1].type!=elem[i].type)) || ((i>0)&&(elem[i-1].attr!=elem[i].attr)))
        {
          if (elem[i].attr==1) fprintf (handle1, "*ELEMENT, TYPE=C3D20R, ELSET=E%s\n", anz->model);
          else            fprintf (handle1, "*ELEMENT, TYPE=C3D20, ELSET=E%s\n", anz->model);
        }
        fprintf (handle1, "%6d,%6d,%6d,%6d,%6d,%6d,%6d,%6d,%6d,%6d,%6d,\n",
          elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2],
          elem[i].nod[3], elem[i].nod[4], elem[i].nod[5], elem[i].nod[6],
          elem[i].nod[7], elem[i].nod[8], elem[i].nod[9]);
        fprintf (handle1, "      %6d,%6d,%6d,%6d,%6d,%6d,%6d,%6d,%6d,%6d\n",
          elem[i].nod[10], elem[i].nod[11], elem[i].nod[16], elem[i].nod[17],
          elem[i].nod[18], elem[i].nod[19], elem[i].nod[12], elem[i].nod[13],
          elem[i].nod[14], elem[i].nod[15] );
      }
      else if (elem[i].type == 5)
      {
        if((i==0) || ((i>0)&&(elem[i-1].type!=elem[i].type)) || ((i>0)&&(elem[i-1].attr!=elem[i].attr)))
        {
          fprintf (handle1, "*ELEMENT, TYPE=C3D15, ELSET=E%s\n", anz->model);
        }
        fprintf (handle1, "%6d,%6d,%6d,%6d,%6d,%6d,%6d,%6d,%6d,%6d,\n",
          elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2],
          elem[i].nod[3], elem[i].nod[4], elem[i].nod[5], elem[i].nod[6],
          elem[i].nod[7], elem[i].nod[8]);
        fprintf (handle1, "      %6d,%6d,%6d,%6d,%6d,%6d\n",
          elem[i].nod[12], elem[i].nod[13], elem[i].nod[14], elem[i].nod[9],
          elem[i].nod[10], elem[i].nod[11] );
      }
      else if (elem[i].type == 6)
      {
        if((i==0) || ((i>0)&&(elem[i-1].type!=elem[i].type)) || ((i>0)&&(elem[i-1].attr!=elem[i].attr)))
        {
          if (elem[i].attr==8) fprintf (handle1, "*ELEMENT, TYPE=C3D10M, ELSET=E%s\n", anz->model);
          else fprintf (handle1, "*ELEMENT, TYPE=C3D10, ELSET=E%s\n", anz->model);
        }
        fprintf (handle1, "%6d,%6d,%6d,%6d,%6d,%6d,%6d,%6d,%6d,%6d,%6d \n",
          elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2],
          elem[i].nod[3], elem[i].nod[4], elem[i].nod[5], elem[i].nod[6],
          elem[i].nod[7], elem[i].nod[8], elem[i].nod[9]);
      }
      else if (elem[i].type == 7)
      {
        if((i==0) || ((i>0)&&(elem[i-1].type!=elem[i].type)) || ((i>0)&&(elem[i-1].attr!=elem[i].attr)))
        {
          if (elem[i].attr==4)      fprintf (handle1, "*ELEMENT, TYPE=CPE3, ELSET=E%s\n", anz->model);
          else if (elem[i].attr==5) fprintf (handle1, "*ELEMENT, TYPE=CPS3, ELSET=E%s\n", anz->model);
          else if (elem[i].attr==6) fprintf (handle1, "*ELEMENT, TYPE=CAX3, ELSET=E%s\n", anz->model);
          else fprintf (handle1, "*ELEMENT, TYPE=S3, ELSET=E%s\n", anz->model);
        }
        fprintf (handle1, "%6d,%6d, %6d, %6d \n",
          elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2] );
      }
      else if (elem[i].type == 8)
      {
        if((i==0) || ((i>0)&&(elem[i-1].type!=elem[i].type)) || ((i>0)&&(elem[i-1].attr!=elem[i].attr)))
        {
          if (elem[i].attr==4)      fprintf (handle1, "*ELEMENT, TYPE=CPE6, ELSET=E%s\n", anz->model);
          else if (elem[i].attr==5) fprintf (handle1, "*ELEMENT, TYPE=CPS6, ELSET=E%s\n", anz->model);
          else if (elem[i].attr==6) fprintf (handle1, "*ELEMENT, TYPE=CAX6, ELSET=E%s\n", anz->model);
          //else fprintf (handle1, "*ELEMENT, TYPE=STRI65, ELSET=E%s\n", anz->model);
          else fprintf (handle1, "*ELEMENT, TYPE=S6, ELSET=E%s\n", anz->model);
        }
        fprintf (handle1, "%6d, %6d, %6d, %6d, %6d, %6d, %6d\n",
          elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2],
          elem[i].nod[3], elem[i].nod[4], elem[i].nod[5]);
      }
      else if (elem[i].type == 9)
      {
        if((i==0) || ((i>0)&&(elem[i-1].type!=elem[i].type)) || ((i>0)&&(elem[i-1].attr!=elem[i].attr)))
        {
          if (elem[i].attr==4)      fprintf (handle1, "*ELEMENT, TYPE=CPE4, ELSET=E%s\n", anz->model);
          else if (elem[i].attr==5) fprintf (handle1, "*ELEMENT, TYPE=CPS4, ELSET=E%s\n", anz->model);
          else if (elem[i].attr==6) fprintf (handle1, "*ELEMENT, TYPE=CAX4, ELSET=E%s\n", anz->model);
          else fprintf (handle1, "*ELEMENT, TYPE=S4, ELSET=E%s\n", anz->model);
        }
        fprintf (handle1, "%6d, %6d, %6d, %6d, %6d\n",
          elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2],
          elem[i].nod[3]);
      }
      else if (elem[i].type == 10)
      {
        if((i==0) || ((i>0)&&(elem[i-1].type!=elem[i].type)) || ((i>0)&&(elem[i-1].attr!=elem[i].attr)))
        {
          if (elem[i].attr==1)      fprintf (handle1, "*ELEMENT, TYPE=S8R, ELSET=E%s\n", anz->model);
          else if (elem[i].attr==4) fprintf (handle1, "*ELEMENT, TYPE=CPE8, ELSET=E%s\n", anz->model);
          else if (elem[i].attr==5) fprintf (handle1, "*ELEMENT, TYPE=CPS8, ELSET=E%s\n", anz->model);
          else if (elem[i].attr==6) fprintf (handle1, "*ELEMENT, TYPE=CAX8, ELSET=E%s\n", anz->model);
          else if (elem[i].attr==14) fprintf (handle1, "*ELEMENT, TYPE=CPE8R, ELSET=E%s\n", anz->model);
          else if (elem[i].attr==15) fprintf (handle1, "*ELEMENT, TYPE=CPS8R, ELSET=E%s\n", anz->model);
          else if (elem[i].attr==16) fprintf (handle1, "*ELEMENT, TYPE=CAX8R, ELSET=E%s\n", anz->model);
          else fprintf (handle1, "*ELEMENT, TYPE=S8, ELSET=E%s\n", anz->model);
        }
        fprintf (handle1, "%6d, %6d, %6d, %6d, %6d, %6d, %6d, %6d, %6d\n"
        , elem[i].nr, elem[i].nod[0], elem[i].nod[1], elem[i].nod[2], elem[i].nod[3]
        , elem[i].nod[4], elem[i].nod[5], elem[i].nod[6], elem[i].nod[7]);
      }
      else if (elem[i].type == 11)
      {
	printf("e:%d attr:%d\n", elem[i].nr,elem[i].attr);
        if((i==0) || ((i>0)&&(elem[i-1].type!=elem[i].type)) || ((i>0)&&(elem[i-1].attr!=elem[i].attr)))
        {
          if (elem[i].attr==3)  fprintf (handle1, "*ELEMENT, TYPE=DASHPOTA, ELSET=E%s\n", anz->model);
          else if (elem[i].attr==7)
	  {
            fprintf (handle1, "*ELEMENT, TYPE=D, ELSET=E%s\n", anz->model);
            // search the connected node
            for (j=0; j<anz->e; j++)
            {
              if(elem[j].type == 12)
	      {
                if( (elem[j].nod[0]==elem[i].nod[0])||(elem[j].nod[2]==elem[i].nod[0]) ) { inletflag=0; break; }
                if( (elem[j].nod[0]==elem[i].nod[1])||(elem[j].nod[2]==elem[i].nod[1]) ) { inletflag=1; break; }
	      }
	    }
            if(inletflag)
	    {
              fprintf (handle1, "%6d, 0, %6d, %6d\n",
                elem[i].nr, elem[i].nod[0], elem[i].nod[1] );
              inletflag=0;
	    }
            else
	    {
              fprintf (handle1, "%6d, %6d, %6d, 0\n",
                elem[i].nr, elem[i].nod[0], elem[i].nod[1] );
	    }
            continue;
	  }
          else  fprintf (handle1, "*ELEMENT, TYPE=B31, ELSET=E%s\n", anz->model);
        }
        fprintf (handle1, "%6d, %6d, %6d\n",
          elem[i].nr, elem[i].nod[0], elem[i].nod[1] );
      }
      else if (elem[i].type == 12)
      {
        if((i==0) || ((i>0)&&(elem[i-1].type!=elem[i].type)) || ((i>0)&&(elem[i-1].attr!=elem[i].attr)))
        {
          if (elem[i].attr==7) fprintf (handle1, "*ELEMENT, TYPE=D, ELSET=E%s\n", anz->model);
	  else fprintf (handle1, "*ELEMENT, TYPE=B32R, ELSET=E%s\n", anz->model);
        }
        fprintf (handle1, "%6d, %6d, %6d, %6d\n",
          elem[i].nr, elem[i].nod[0], elem[i].nod[2], elem[i].nod[1] );
      }
      else
      {
        printf (" WARNING: elem(%d) not a known type (%d)\n", elem[i].nr, elem[i].type);
      }
    }
    fclose(handle1);
  }

  /* open node-value file */
  else if((anz->l>0)&&(dat!=NULL))
  {
    if(strlen(dat[0])>0)
    {
      if(compare( dat[0], "ds", 2)== 2)
      {
        if((dat[1]!=(char *)NULL)&&(dat[1][0]=='e'))  sprintf( &datout[lendatout], "_%s%s.dat", dat[0], dat[1]);
        else  sprintf( &datout[lendatout], "_%s.dat", dat[0]);
      }
      else strcpy (&datout[lendatout], ".dat");
      handle2 = fopen (datout, "w+b");
      if (handle2==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n", datout); return(-1);}
      else  printf (" file %s opened\n",datout);
    }

    /* ccx requests the *AMPLITUDE statements in front of all *STEPs */
    /* therefore a loop over all Datasets is performed twice */
    if((anz->l>0)&&(dat!=0))
    {
     /* first loop for the *AMPLITUDEs */
     j=0;
     for (lc=0; lc<anz->l; lc++)
     {
      if (lcase[lc].ncomps == 1)
      {
        if((compare( dat[0], "ds", 2)== 2)&&(atoi(&dat[0][2])!=lc+1)) continue;
        
        j++;
        if(compare( dat[0], "crp", 3)== 3)
        {
          /* local step time and load */
          /* load_new = load_old * speed**2 / refspeed**2 */
          fprintf (handle2, "** AMPLITUDE, FORCE= speed**2 / refspeed**2 **\n");
          time1 = 0.;
          frc1  = frc2;
          time2 = (lcase[lc].value*atof(dat[1]))-oldtime;
          oldtime=(lcase[lc].value*atof(dat[1]));
          if (time2<iniTime) time2=iniTime;
          frc2  = (atof(lcase[lc].dataset_text)*atof(lcase[lc].dataset_text))/(atof(dat[2])*atof(dat[2]));
          tmin  = time2/10.;
          tmax  = time2;
          fprintf (handle2, "*AMPLITUDE, NAME=FORCE%d\n", j);
          fprintf (handle2, "%lf, %lf, %lf, %lf\n", time1, frc1, time2, frc2);
        }
        if(compare( dat[0], "sta", 3)== 3)
        {
          /* local step time and load */
          /* load_new = load_old * speed**2 / refspeed**2 */
          fprintf (handle2, "** AMPLITUDE, FORCE= speed**2 / refspeed**2 **\n");
          time1 = 0.;
          time2 = 1.;
          frc2  = (atof(lcase[lc].dataset_text)*atof(lcase[lc].dataset_text))/(atof(dat[2])*atof(dat[2]));
          fprintf (handle2, "** ---  LC%d ---\n", j);
          fprintf (handle2, "*AMPLITUDE, NAME=FORCE%d\n", j);
          fprintf (handle2, "%lf, %lf, %lf, %lf\n", time1, frc2, time2, frc2);
        }
      }
     }
  
      /* second loop for the *STEPs */
     j=0;
     for (lc=0; lc<anz->l; lc++)
     {
      if( (compare( lcase[lc].name, "NDTEMP", 6) == 6)||( compare( lcase[lc].name, "TEMP", 4) == 4)||( compare( lcase[lc].name, "TT3D", 4) == 4))
        // if (lcase[lc].ncomps == 1)
      {
        if((compare( dat[0], "ds", 2)== 2)&&(atoi(&dat[0][2])!=lc+1)) continue;
        
        j++;
        if(compare( dat[0], "tmf", 3)== 3)
        {
          /* local step time */
          time2 = (lcase[lc].value)-oldtime;
          oldtime=(lcase[lc].value);
          fprintf (handle2, "** ---  LC%d ---\n", j);
          fprintf (handle2, "*STEP\n");
          /* fprintf (handle2, "*STATIC, SOLVER=SPOOLES\n"); */
          fprintf (handle2, "*STATIC\n");
          fprintf (handle2, "%lf, %lf,,,\n", time2, time2);
          fprintf (handle2, "*TEMPERATURE\n");
        }
        if(compare( dat[0], "crp", 3)== 3)
        {
          /* local step time and load */
          /* load_new = load_old * speed**2 / refspeed**2 */
          time1 = 0.;
          frc1  = frc2;
          time2 = (lcase[lc].value*atof(dat[1]))-oldtime;
          oldtime=(lcase[lc].value*atof(dat[1]));
          if (time2<iniTime) time2=iniTime;
          frc2  = (atof(lcase[lc].dataset_text)*atof(lcase[lc].dataset_text))/(atof(dat[2])*atof(dat[2]));
          tmin  = time2/10.;
          tmax  = time2;
          fprintf (handle2, "** ---  LC%d ---\n", j);
          fprintf (handle2, "*STEP, NLGEOM, AMPLITUDE=RAMP\n");
          fprintf (handle2, "*VISCO, CETOL=5.e-4\n");
          fprintf (handle2, "%lf, %lf,,,\n", tmin, tmax);
          fprintf (handle2, "*CLOAD, AMPLITUDE=FORCE%d\n", j);
          fprintf (handle2, "*include, input=ccx.clo\n");
          fprintf (handle2, "*DLOAD\n");
          fprintf (handle2, "Eall, CENTRIF, %f,  0., 0., 0.,  1., 0., 0\n", frc2*(atof(dat[2])/60.*2*PI)*(atof(dat[2])/60.*2*PI));
          fprintf (handle2, "*DLOAD, AMPLITUDE=FORCE%d\n", j);
          fprintf (handle2, "*include, input=ccx.dlo\n");
          fprintf (handle2, "*TEMPERATURE\n");
        }
        if(compare( dat[0], "sta", 3)== 3)
        {
          /* local step time and load */
          /* load_new = load_old * speed**2 / refspeed**2 */
          time2 = (lcase[lc].value)-oldtime;
          oldtime=(lcase[lc].value);
          frc2  = (atof(lcase[lc].dataset_text)*atof(lcase[lc].dataset_text))/(atof(dat[2])*atof(dat[2]));
          fprintf (handle2, "** ---  LC%d ---\n", j);
          fprintf (handle2, "*STEP, NLGEOM\n");
          fprintf (handle2, "*STATIC\n");
          fprintf (handle2, "%lf, %lf,,,\n", time2, time2);
          fprintf (handle2, "*CLOAD, AMPLITUDE=FORCE%d\n", j);
          fprintf (handle2, "*include, input=ccx.clo\n");
          fprintf (handle2, "*DLOAD\n");
          fprintf (handle2, "Eall, CENTRIF, %f,  0., 0., 0.,  1., 0., 0\n", frc2*(atof(dat[2])/60.*2*PI)*(atof(dat[2])/60.*2*PI));
          fprintf (handle2, "*DLOAD, AMPLITUDE=FORCE%d\n", j);
          fprintf (handle2, "*include, input=ccx.dlo\n");
          fprintf (handle2, "*TEMPERATURE\n");
        }
  
        for (i=0; i<anz->n; i++)
        {
          fprintf (handle2, "%8d, %12.5f\n", node[i].nr,
          lcase[lc].dat[0][node[i].nr]);
        }
        
        if( (compare( dat[0], "tmf", 3)== 3) || (compare( dat[0], "sta", 3)== 3) )
        {
          fprintf (handle2, "*NODE PRINT, FREQUENCY=0 \n");
          fprintf (handle2, "*EL PRINT, FREQUENCY=0 \n");
          fprintf (handle2, "*NODE FILE\n");
          fprintf (handle2, "U, NT \n");
          fprintf (handle2, "*EL FILE,POSITION=AVERAGED AT NODES\n");
          fprintf (handle2, "S,E,ME \n");
          fprintf (handle2, "*END STEP\n");
        }
        if(compare( dat[0], "crp", 3)== 3)
        {
          fprintf (handle2, "*NODE PRINT, FREQUENCY=0 \n");
          fprintf (handle2, "*EL PRINT, FREQUENCY=0 \n");
          fprintf (handle2, "*NODE FILE, FREQUENCY=%d\n", atoi(dat[3]));
          fprintf (handle2, "U, NT \n");
          fprintf (handle2, "*EL FILE, FREQUENCY=%d,POSITION=AVERAGED AT NODES\n", atoi(dat[3]));
          fprintf (handle2, "S, CE \n");
          fprintf (handle2, "*END STEP\n");
        }
      }
      else
      {
        if((compare( dat[0], "ds", 2)== 2)&&(atoi(&dat[0][2])==lc+1))
        {
          for (n=0; n<lcase[lc].ncomps; n++)
          {
	    if(lcase[lc].iexist[n]!=0) continue;
            fprintf (handle2, "** %s %s\n", lcase[lc].name,lcase[lc].compName[n] );
            for (i=0; i<anz->n; i++)
            {
	      if(lcase[lc].ncomps==1) fprintf (handle2, "%8d, %f\n", node[i].nr, lcase[lc].dat[n][node[i].nr]);
              else fprintf (handle2, "%8d,%d,%f\n", node[i].nr, n+1, lcase[lc].dat[n][node[i].nr]);
            }
          }
        }
      }
    }
    fclose(handle2);
   }
  }
  return (1);
}

