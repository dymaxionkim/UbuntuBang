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

extern CopiedNodeSets copiedNodeSets[1];
extern Nodes    *node;                                                  
extern Faces    *face;                                            
extern Elements *e_enqire;                                               
extern Datasets *lcase;
extern Sets     *set;
extern Points   *point;
extern Lines    *line ;
extern Lcmb     *lcmb ;
extern Gsur     *surf ;
extern Gbod     *body ;
extern Nurbl    *nurbl;
extern Nurbs    *nurbs;
extern Shapes   *shape;

int readWf(char *wfname)
{
  int i,p=0, length;         
  FILE *handle1;
  char rec_str[MAX_LINE_LENGTH], *ptr;     
  char name[MAX_LINE_LENGTH], sectionSet[MAX_LINE_LENGTH];     

  int seqa=-1, nsec=0, nsections=0, curv=0, ncurves=4, pt=0;
  double vp[3];

  int wedgePoint[4];                   // point-nr of the LE and TE start- and end-points (LE,TE)
  int *pnr=NULL;                       // point indexes
  int npnt;                                // nr of points in this section
  char seqaset[5][MAX_LINE_LENGTH];        // Name of each edge and wedgePoints
  int pLE, pTE;
  char setSECP[MAX_LINE_LENGTH];

  strcpy(setSECP,"SECP");
  handle1 = fopen ( wfname, "r" ); 
  if (handle1== NULL)  printf ("\nThe input file \"%s\" could not be opened.\n\n", wfname);
  else  printf ("\n%s opened", wfname);

  do
  {
    length = frecord( handle1, rec_str);
    if (!length) continue;              
    if (rec_str[length] == (char)EOF) break;
    //printf ("length:%d record:%s\n", length, rec_str);

    /* search for <Component type="Section">. Here is the start of the sections */
    if( (ptr=strstr(rec_str, "ComponentCollection name=\"sections\"")) !=NULL)    
    {
      if( (ptr=strstr(rec_str, "length=")) ==NULL) return(-1);
      nsections=atoi(&ptr[8]);
      printf ("nsections:%d\n", nsections);

      /* from here on go over all sections and read the points, 4 curves per section (SS,LE,PS,TE)*/
      while (length > -1)
      {
        length = frecord( handle1, rec_str);
        if (rec_str[length] == EOF) { printf("EOF\n"); goto finish; }
        if( (ptr=strstr(rec_str, "Data name=\"points\"")) !=NULL)
	{
          if( (ptr=strstr(rec_str, "length=")) ==NULL) return(-1);
          npnt=atoi(&ptr[8]);
          printf ("curv:%d npnt:%d nsec:%d\n", curv, npnt, nsec);

          sprintf(sectionSet,"%s%d",setSECP,nsec+1);
          seto(sectionSet);

          wedgePoint[curv]=p;

          /* allocate the point-indexes */
          if((pnr = (int *)realloc((int *)pnr, (p+npnt)*sizeof(char*)))==NULL)
          errMsg("\n\n ERROR: realloc failed for pnt\n" );

	  /* get points */
          for(pt=0; pt<npnt-1; pt++)
	  {
	    //printf("pt:%d %d\n", pt, npnt);
            length = frecord( handle1, rec_str);
            if( (ptr=strstr(rec_str, "Data name=")) ==NULL)
	    {
              length = frecord( handle1, rec_str);
              if( (ptr=strstr(rec_str, "Data name=")) ==NULL) return(-1);
	    }
            strcpy( name, &ptr[11]);
            for(i=0; i<MAX_LINE_LENGTH; i++) if(name[i]=='"') { name[i]=0; break; }
            //printf ("name:%s\n", name);
            if( (ptr=strstr(rec_str, "type=")) ==NULL) return(-1);
            sscanf(&ptr[13], "%lf %lf %lf",&vp[0],&vp[1],&vp[2]);
            pnr[p++]=pnt(name, vp[0],vp[1],vp[2], 1);
	  }
          curv++;
        }

        /* after all 4 curves are read preprocess them */
        if(curv==ncurves)
        {
          npnt=p;
          p=0; curv=0; nsec++;

          sprintf(seqaset[0],"PS%d",nsec);
          sprintf(seqaset[1],"LE%d",nsec);
          sprintf(seqaset[2],"SS%d",nsec);
          sprintf(seqaset[3],"TE%d",nsec);
          sprintf(seqaset[4],"WP%d",nsec);
    
          /* save the wedgePoints */
          seqa=pre_seta(seqaset[4],"is",0);
          if(seqa<0) { printf("ERROR in readSection\n"); exit(0); }
          for(i=0; i<4; i++) seta(seqa,"p",pnr[wedgePoint[i]]);
    
          /* the mid of the TE is located at point (wedgePoint[3]+npnt)/2 */
          pTE=(wedgePoint[3]+npnt)/2;
    
          /* the mid of the LE is located at point (wedgePoint[1]+wedgePoint[2])/2 */
          pLE=(wedgePoint[1]+wedgePoint[2])/2;
    
          /* save the TE */
          seqa=pre_seta(seqaset[3],"is",0);
          if(seqa<0) { printf("ERROR in readSection\n"); exit(0); }
          for(i=pTE-1; i<=pTE+1; i++) seta(seqa,"p",pnr[i]);
    
          /* save the PS */
          seqa=pre_seta(seqaset[0],"is",0);
          if(seqa<0) { printf("ERROR in readSection\n"); exit(0); }
          for(i=pTE+1; i<npnt-1; i++) seta(seqa,"p",pnr[i]);
          for(i=0; i<pLE; i++) seta(seqa,"p",pnr[i]);
    
          /* save the LE */
          seqa=pre_seta(seqaset[1],"is",0);
          if(seqa<0) { printf("ERROR in readSection\n"); exit(0); }
          for(i=pLE-1; i<=pLE+1; i++) seta(seqa,"p",pnr[i]);
    
          /* save the SS */
          seqa=pre_seta(seqaset[2],"is",0);
          if(seqa<0) { printf("ERROR in readSection\n"); exit(0); }
          for(i=pLE+1; i<pTE; i++) seta(seqa,"p",pnr[i]);
          
          setc(sectionSet);
        }
      }
    }
  } while(length>-1);
 finish:;
  printf(" wireframe read\n");
  free(pnr);
  fclose(handle1);
  return(nsec);
}
