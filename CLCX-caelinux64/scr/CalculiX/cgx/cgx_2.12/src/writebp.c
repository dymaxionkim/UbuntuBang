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

#include <cgx.h>

#define MAX_GSUR_PARAMERTER 7
extern SpecialSet specialset[1];
extern Eqal eqal;
extern char entity_k[SET_COLS];
extern Psets     *pset;

int writebp(char *setname, Summen *anz, SumGeo *anzGeo, Points *pnt, Sets *set )
{
  FILE *handle1;
  FILE *handle2;
  int  i, j, k, n, p;
  char string[MAX_LINE_LENGTH], datout[2][MAX_LINE_LENGTH], buffer[MAX_LINE_LENGTH];
  int length, setNr;

  setNr=getSetNr(setname);
  if (setNr<0)
  {
    printf (" ERROR: set:%s does not exist\n", setname);
    return (-1);
  }

  length=sword( setname, string);
  string[length]='.';
  string[length+1]='b';
  string[length+2]='p';
  string[length+3]='\0';
  strcpy( datout[0], string);
  string[length+1]='s';
  string[length+2]='t';
  string[length+3]='p';
  string[length+4]='\0';
  strcpy( datout[1], string);


  /* Open the files and check to see that it was opened correctly */
  /* bp-file for cad department */
  handle1 = fopen (datout[0], "w+b");
  if (handle1==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n", datout[0]); return(-1);}
  else  printf ("\n%s opened\n",datout[0]);

  /* step-file for cad department */
  handle2 = fopen (datout[1], "w+b");
  if (handle2==NULL) { printf ("\nThe output file \"%s\" could not be opened.\n\n", datout[1]); return(-1);}
  else  printf ("\n%s opened\n",datout[1]);

  printf ("\n write bp (bulk-point)\n The common points of the specified set and all sets matching 'SS<nr>' will be written\n");
  printf (" continue with: 'stp2vda <set>.stp up|do' 'tomeshvl' etc.\n\n");
  delSet(specialset->tmp);

  fprintf (handle1, "written by cgx\n");
  fprintf (handle2, "HEADER;\n/* written by cgx to be used by stp2vda, file does not conform to any norm */\n");

  k=0;
  for(n=0; n<anz->sets; n++)
  {
    if (set[n].name!= (char *)NULL)
    for(j=0; j<100; j++)
    {
      sprintf(buffer,"SS%d", j);
      if (strlen(set[n].name)==strlen(buffer)) if( compare( set[n].name, buffer, strlen(set[n].name))==strlen(buffer)) k++;
    }
  }
  fprintf (handle1, "%5i\n",k);

  for(n=0; n<anz->sets; n++)
  {
    if (set[n].name!= (char *)NULL)
    for(j=0; j<100; j++)
    {
      sprintf(buffer,"SS%d", j);
      if (strlen(set[n].name)==strlen(buffer)) if( compare( set[n].name, buffer, strlen(set[n].name))==strlen(buffer))
      {
        printf("%s %s\n",set[n].name,buffer );
        k=0;
        for (i=0; i<set[n].anz_p; i++)
	{
          if( getIndex(&set[setNr].pnt,set[setNr].anz_p,set[n].pnt[i]) >-1) k++;
	}
        printf ("%5i pnts written\n",k);
        fprintf (handle1, "%5i    3    1   10\n",k);
        for (p=0; p<set[n].anz_p; p++)
	{
          if( getIndex(&set[setNr].pnt,set[setNr].anz_p,set[n].pnt[p]) >-1)
	  {
            i=set[n].pnt[p];
            if( pnt[i].name != (char *)NULL )
	    {
              fprintf (handle1, "%10.4lf%10.4lf%10.4lf\n", pnt[i].px,pnt[i].py,pnt[i].pz);
              fprintf (handle2, "#%d=CARTESIAN_POINT\(\'%s\',\(%10.4lf,%10.4lf,%10.4lf\)\);\n", i+1,set[n].name,pnt[i].px,pnt[i].py,pnt[i].pz);
	      //#484=CARTESIAN_POINT('',(40.695580000019,176.223812949467,0.));
	    }
          }
        }
      }
    }
  }


  fprintf (handle1, "\n");
  fclose(handle1);
  fclose(handle2);

  /* create an empty  hdr file for prg tomeshvl */
  system("touch hdr");
  return (1);
}





