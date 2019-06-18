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

#define TEST  0
#define TEST1 1
#define WRITE_CELLS 0


/* input: */
/*   setname  */
/*   strings: number of arguments */
/*   string:  arguments, ( type of boundary, set of boundary, except if type of boundary is "cyclic" then */
/*            the two sets which are to be connected and the search-vector follow */
/*   and all meshing types for the full mesh */  

int write2foam(char *setname, int strings, char **string, Summen *anz, Nodes *node, Faces *face, Elements *e_enqire, Sets *set, Datasets *lcase )
{
  int  i, j, k=0, n, nr, setNr, masNr;
  int  nof, nif,  noe, ipuf;
  int  lc=0, en[6];
  Faces    *oface=NULL;
  Faces    *iface=NULL;
  Elements *etmp =NULL;
  int *nod=NULL;
  int *startface=NULL, patches;
  FILE *handle=NULL;
  char buffer[MAX_LINE_LENGTH];
  double val[6];
#if WRITE_CELLS
  int    fbuf[6];
#endif

  int  *slvface, boundaries, *nf, boucyc[2]={-1,-1};
  char **type=NULL, **bound=NULL, sys=0, axis=0;
  Rsort *rsort=NULL;
  double vcyc[3], rcyc, dcg[3], cgm[3], cgs[3], vn1[3], vn2[3], xcyc;

  /* check if we have a mesh */
  if(anz->e==0)
  { printf("ERROR: No mesh!\n"); return(0); }


  /* check if we have boundary patches */
  if(strings==0)
  { printf("ERROR: No boundary patches defined!\n"); return(0); }


  /* check if datasets should be written */
  if ((compare(string[0],"ds", 2)==2)&&(string[1][0]=='e'))
  {
    setNr=getSetNr(setname);
    if(setNr<0)
    {
      printf("ERROR set:%s not known\n", setname);
      return(0);
    }

    sprintf( buffer, "%s_%s%s.dat", set[setNr].name, string[0], string[1]);
    handle = fopen ( buffer, "w");
    if ( handle== NULL )
    {
      printf ("\nThe input file points could not be opened.\n\n"); 
      return(0);
    }
    lc=atoi(&string[0][2])-1;
    for(i=1; i<strings; i++) en[i-1]=atoi(&string[i][1])-1;

    /* check which type of data */
    switch (strings)
    {
      /* scalar */
      case 2:
      if(set[setNr].anz_e)
      {
        fprintf(handle, "internalField nonuniform List<scalar>\n");
        fprintf(handle, "%d\n(\n", set[setNr].anz_e ); 
        for(i=0; i<set[setNr].anz_e; i++)
        {
          nr=set[setNr].elem[i];
          /* average the values per element */
          if     (e_enqire[nr].type==1) k=8;
          else if(e_enqire[nr].type==2) k=6;
          else if(e_enqire[nr].type==3) k=4;
          else if(e_enqire[nr].type==4) k=20;
          else if(e_enqire[nr].type==5) k=15;
          else if(e_enqire[nr].type==6) k=10;
          else if(e_enqire[nr].type==7) k=3;
          else if(e_enqire[nr].type==8) k=6;
          else if(e_enqire[nr].type==9) k=4;
          else if(e_enqire[nr].type==10) k=8;
          else if(e_enqire[nr].type==11) k=2;
          else if(e_enqire[nr].type==12) k=3;
          val[0]=0;
          for(n=0; n<k; n++) val[0]+=lcase[lc].dat[en[0]][e_enqire[nr].nod[n]];
          val[0]/=k;
          fprintf (handle, "%e\n", val[0]);
        }
        fprintf(handle, ");\n"); 
      }
      else if((set[setNr].anz_n)||(set[setNr].anz_f))
      {
        if(!set[setNr].anz_f)  completeSet( set[setNr].name, "do" );
        fprintf(handle, "boundaryField\n{\n  %s\n  {\n    type fixedValue;\n    value   nonuniform List<scalar>\n", set[setNr].name);
        fprintf(handle, "    %d\n", set[setNr].anz_f ); 
        fprintf(handle, "    (\n" ); 
        for(i=0; i<set[setNr].anz_f; i++)
        {
          nr=set[setNr].face[i];
          /* average the values per element */
          if     (face[nr].type==7) k=3;
          else if(face[nr].type==9) k=4;
          val[0]=0;
          for(n=0; n<k; n++) val[0]+=lcase[lc].dat[en[0]][face[nr].nod[n]];
          val[0]/=k;
          fprintf (handle, "    %e\n", val[0]);
        }
        fprintf(handle, "    );\n  }\n}\n"); 
      }
      break;

      /* vector */
      case 4:
      if(set[setNr].anz_e)
      {
        fprintf(handle, "internalField nonuniform List<vector>\n");
        fprintf(handle, "%d\n(\n", set[setNr].anz_e ); 
        for(i=0; i<set[setNr].anz_e; i++)
        {
  	  nr=set[setNr].elem[i];
          /* average the values per element */
          if     (e_enqire[nr].type==1) k=8;
          else if(e_enqire[nr].type==2) k=6;
          else if(e_enqire[nr].type==3) k=4;
          else if(e_enqire[nr].type==4) k=20;
          else if(e_enqire[nr].type==5) k=15;
          else if(e_enqire[nr].type==6) k=10;
          else if(e_enqire[nr].type==7) k=3;
          else if(e_enqire[nr].type==8) k=6;
          else if(e_enqire[nr].type==9) k=4;
          else if(e_enqire[nr].type==10) k=8;
          else if(e_enqire[nr].type==11) k=2;
          else if(e_enqire[nr].type==12) k=3;
          for(j=0; j<3; j++)
          {
            val[j]=0;
            for(n=0; n<k; n++) val[j]+=lcase[lc].dat[en[j]][e_enqire[nr].nod[n]];
            val[j]/=k;
          }
          fprintf (handle, "(%e %e %e)\n", val[0], val[1], val[2]);
        }
        fprintf(handle, ");\n"); 
      }
      else if((set[setNr].anz_n)||(set[setNr].anz_f))
      {
        if(!set[setNr].anz_f)  completeSet( set[setNr].name, "do" );
        fprintf(handle, "boundaryField\n{\n  %s\n  {\n    type fixedValue;\n    value   nonuniform List<vector>\n", set[setNr].name);
        fprintf(handle, "    %d\n", set[setNr].anz_f ); 
        fprintf(handle, "    (\n" ); 
        for(i=0; i<set[setNr].anz_f; i++)
        {
          nr=set[setNr].face[i];
          /* average the values per element */
          if     (face[nr].type==7) k=3;
          else if(face[nr].type==9) k=4;
          for(j=0; j<3; j++)
          {
            val[j]=0;
            for(n=0; n<k; n++) val[j]+=lcase[lc].dat[en[j]][face[nr].nod[n]];
            val[j]/=k;
          }
          fprintf (handle, "    (%e %e %e)\n", val[0], val[1], val[2]);
        }
        fprintf(handle, "    );\n  }\n}\n"); 
      }
      break;

      /* tensor */
	case 7:
      fprintf(handle, "//cat this.file >> U\nTBD\n");
      break;
    }
    fclose(handle);
    return(0);
  }


  boundaries=0;

  n=0; do
  {
    if (compare(string[n],"cyclic", 6)==6)
    {
      if((type=(char **)realloc((char **)type, (boundaries+3)*sizeof(char *)))==NULL)
        printf("\n\n ERROR: realloc failure\n\n" );
      if((bound=(char **)realloc((char **)bound, (boundaries+3)*sizeof(char *)))==NULL)
        printf("\n\n ERROR: realloc failure\n\n" );

      if( (type[boundaries]= (char *)malloc( MAX_LINE_LENGTH*sizeof(char))) == NULL )
        printf("\n\n ERROR: malloc failure\n\n" );
      if( (bound[boundaries]= (char *)malloc( MAX_LINE_LENGTH*sizeof(char))) == NULL )
        printf("\n\n ERROR: malloc failure\n\n" );
      strcpy(type[boundaries],string[n++]);
      strcpy(bound[boundaries],string[n++]);

      boundaries++;

      if( (type[boundaries]= (char *)malloc( MAX_LINE_LENGTH*sizeof(char))) == NULL )
        printf("\n\n ERROR: malloc failure\n\n" );
      if( (bound[boundaries]= (char *)malloc( MAX_LINE_LENGTH*sizeof(char))) == NULL )
        printf("\n\n ERROR: malloc failure\n\n" );
      strcpy(type[boundaries],type[boundaries-1]);
      strcpy(bound[boundaries],string[n++]);
      k=sscanf(string[n], "%lf,%lf,%lf", &vcyc[0], &vcyc[1], &vcyc[2]);
      if(k!=3)
      {
	/* is it defined by a translation or rotation about xyz? */
        k=sscanf(string[n], "%c%c", &sys, &axis );
        if((sys=='c')||(sys=='r'))
        {
          if(axis=='x')      { vcyc[0]=1.; vcyc[1]=0.; vcyc[2]=0.; }
          else if(axis=='y') { vcyc[0]=0.; vcyc[1]=1.; vcyc[2]=0.; }
          else if(axis=='z') { vcyc[0]=0.; vcyc[1]=0.; vcyc[2]=1.; }
          else
	  {
            printf(" ERROR: found no vector description for cyclic patches relation, check command!\n");
            return(0);
	  }
	}
        else
	{
          printf(" ERROR: found no vector description for cyclic patches relation, check command!\n");
          return(0);
	}
      }
      n++;
      printf(" use boundary: type:%s set:%s %s vec:%lf %lf %lf\n",type[boundaries],bound[boundaries-1],bound[boundaries],vcyc[0],vcyc[1],vcyc[2]);  
      boundaries++;
    }
    else
    {
      if((type=(char **)realloc((char **)type, (boundaries+2)*sizeof(char *)))==NULL)
        printf("\n\n ERROR: realloc failure\n\n" );
      if((bound=(char **)realloc((char **)bound, (boundaries+2)*sizeof(char *)))==NULL)
        printf("\n\n ERROR: realloc failure\n\n" );
      if( (type[boundaries]= (char *)malloc( MAX_LINE_LENGTH*sizeof(char))) == NULL )
        printf("\n\n ERROR: malloc failure\n\n" );
      if( (bound[boundaries]= (char *)malloc( MAX_LINE_LENGTH*sizeof(char))) == NULL )
        printf("\n\n ERROR: malloc failure\n\n" );
      strcpy(type[boundaries],string[n++]);
      strcpy(bound[boundaries],string[n++]);
      printf(" use boundary: type:%s set:%s \n",type[boundaries],bound[boundaries]);  

      boundaries++;
    }
  }while(n<strings);

  /* cycle through all sets and search for cyclic boundaries (defined in type[i]) */
  /* if a cyclic set exist a second must exist. Sort the faces in the second according */
  /* to the center of gravity of the faces in the first */
  /* and append the faces of the second to the faces of the first */

  /* search a cyclic boundary */
  j=0;
  for(i=0; i<boundaries; i++)
  {
    if (compare(type[i],"cyclic", 6)==6)
    {
      if (j>2)
      {
        printf(" ERROR: found more than 2 cyclic boundaries but it must be 2\n");
        return(0);
      }
      boucyc[j++]=i;
    }
  }

  /* sort the cyclic sets */
  /* search radial to the search-vector for the closest facees */
  if (j==1)
  {
    printf(" ERROR: found only one cyclic boundary but it must be 2\n");
    return(0);
  }
  else if(j==2)
  {
    masNr=getSetNr(bound[boucyc[0]]);
    setNr=getSetNr(bound[boucyc[1]]);
    if ( (slvface = (int *)malloc( (set[setNr].anz_f+1) * sizeof(int))) == NULL )
      printf("ERROR: realloc failed: slvface\n\n" ); 
    if ( (rsort = (Rsort *)malloc( (set[setNr].anz_f+1) * sizeof(Rsort))) == NULL )
      printf("ERROR: realloc failed: rsort\n\n" ); 
    for(i=0; i<set[masNr].anz_f; i++)
    {
      ipuf=0;
      if (face[set[masNr].face[i]].type == 7) ipuf = 3;  /* TRI3  */
      else if (face[set[masNr].face[i]].type == 9) ipuf = 4;  /* QUAD4  */
      else
      {
        printf(" ERROR: face-type:%d not supported\n", face[set[masNr].face[i]].type);
        return(0);
      }
      /* calc the cg */
      cgm[0]=0.;
      cgm[1]=0.;
      cgm[2]=0.;
      if (ipuf!=0) for(n=0; n<ipuf; n++)
      {
        cgm[0]+=node[face[set[masNr].face[i]].nod[n]].nx;
        cgm[1]+=node[face[set[masNr].face[i]].nod[n]].ny;
        cgm[2]+=node[face[set[masNr].face[i]].nod[n]].nz;
      }
      for(k=0; k<3; k++) cgm[k]/=ipuf;

      /* transformation in cylindrical system, cgm[0]=r, cgm[1]=theta=0. cgm[2]=x */
      if(sys=='c')
      {
        xcyc     =cgm[0]*vcyc[0]+cgm[1]*vcyc[1]+cgm[2]*vcyc[2];
        cgm[0]=sqrt(cgm[0]*cgm[0]*!((int)vcyc[0])+
                    cgm[1]*cgm[1]*!((int)vcyc[1])+
		    cgm[2]*cgm[2]*!((int)vcyc[2]));
        cgm[1]=0.;
        cgm[2]=xcyc;
      }

      /* calc the dist to all slave surfaces */
      for(j=0; j<set[setNr].anz_f; j++)
      {
        ipuf=0;
        if (face[set[setNr].face[j]].type == 7) ipuf = 3;  /* TRI3  */
        else if (face[set[setNr].face[j]].type == 9) ipuf = 4;  /* QUAD4  */
        else
        {
          printf(" ERROR: face-type:%d not supported\n", face[set[setNr].face[j]].type);
          return(0);
        }
        /* calc the cg */
	cgs[0]=0.;
        cgs[1]=0.;
        cgs[2]=0.;
        if (ipuf!=0) for(n=0; n<ipuf; n++)
        {
          cgs[0]+=node[face[set[setNr].face[j]].nod[n]].nx;
          cgs[1]+=node[face[set[setNr].face[j]].nod[n]].ny;
          cgs[2]+=node[face[set[setNr].face[j]].nod[n]].nz;
        }
        for(k=0; k<3; k++) cgs[k]/=ipuf;

        /* transformation in cylindrical system, cgm[0]=r, cgm[1]=theta=0. cgm[2]=x */
        if(sys=='c')
        {
          xcyc     =cgs[0]*vcyc[0]+cgs[1]*vcyc[1]+cgs[2]*vcyc[2];
          cgs[0]=sqrt(cgs[0]*cgs[0]*!((int)vcyc[0])+
                      cgs[1]*cgs[1]*!((int)vcyc[1])+
	              cgs[2]*cgs[2]*!((int)vcyc[2]));
          cgs[1]=0.;
          cgs[2]=xcyc;

          /* calc the dist between both faces  */
          for(k=0; k<3; k++)  dcg[k]=cgm[k]-cgs[k];
          rcyc=sqrt(dcg[0]*dcg[0]+dcg[1]*dcg[1]+dcg[2]*dcg[2] );  
        }
        else
	{
          /* calc the projected length of the v between both faces onto a plane normal to the search v */
          for(k=0; k<3; k++)  dcg[k]=cgm[k]-cgs[k];

          /*  vcyc x dcg = vn1 */
          v_prod(vcyc,dcg,vn1);

          /*  vn1 x vcyc = vn2 */
          v_prod(vn1,vcyc,vn2);

          /*  dcg . vn2 = rcyc */
          rcyc=v_sprod(dcg,vn2);
#if TEST1
          printf("%d i:%d r:%lf vcyc:%lf %lf %lf  dcg:%lf %lf %lf vn2:%lf %lf %lf\n",j,set[setNr].face[j], rcyc,vcyc[0],vcyc[1],vcyc[2],dcg[0],dcg[1],dcg[2],vn2[0],vn2[1],vn2[2]);
#endif
	}
        rsort[j].r= rcyc ;
        rsort[j].i=set[setNr].face[j];
      }
      qsort( rsort, set[setNr].anz_f, sizeof(Rsort), (void *)compareRsort );
#if TEST1
      printf("%d e:%d slav:%d r:%lf e:%d\n", i, face[set[masNr].face[i]].elem_nr, rsort[0].i, rsort[0].r, face[rsort[0].i].elem_nr);
#endif
      slvface[i]=rsort[0].i; 
    }
    for(j=0; j<set[setNr].anz_f; j++) set[setNr].face[j]=slvface[j];
    free(rsort);
    free(slvface);
  }


  /* cycle through all sets and append the faces to oface */
  /* and the elements to etmp */
  nof=noe=-1;
  for(i=0; i<boundaries; i++)
  {
    setNr=getSetNr(bound[i]);
    if(setNr>-1)
    {
      /* prepare the outer faces */
      /* the element nr (not index) is stored in .elem_nr */
      /* the element-face-nr is stored in .nr in Abq-format */
      if ( (oface = (Faces *)realloc( (Faces *)oface, (nof+set[setNr].anz_f+2) * sizeof(Faces))) == NULL )
        printf("\n\n ERROR: realloc failed oface\n\n") ;
      for(j=0; j<set[setNr].anz_f; j++)
      {
        nof++;
        oface[nof].elem_nr=face[set[setNr].face[j]].elem_nr; 
        oface[nof].nr=face[set[setNr].face[j]].nr+1; 
        oface[nof].type=face[set[setNr].face[j]].type; 
        oface[nof].side   = NULL;
        oface[nof].group=0;
        ipuf=0;
        if (face[set[setNr].face[j]].type == 7) ipuf = 3;  /* TRI3  */
        else if (face[set[setNr].face[j]].type == 9) ipuf = 4;  /* QUAD4  */
        else 
        {
          printf(" ERROR: face-type:%d not supported\n", face[set[setNr].face[j]].type);
          return(0);
        }
        if (ipuf!=0) for (n=0; n<ipuf; n++)
        {
          oface[nof].nod[n]=face[set[setNr].face[j]].nod[n];
        }
      }
      /* mark the last face of each sequence with group=1 */
      oface[nof].group=1;
    }
    else
    {
      printf("ERROR set:%s not known\n", setname);
      return(0);
    }
  }
  nof++;

#if TEST1
  /* display the outer faces */
  printf("display the outer faces\n");
  n=-1;
  for(i=0; i<boundaries; i++)
  {
      printf("\n outer faces of %s\n", bound[i]);
      do
      {
        n++; printf("  face:%d el:%d side:%d group:%d\n", n, oface[n].elem_nr, oface[n].nr+1, oface[n].group );
      }while(oface[n].group!=1);
  }
#endif

  setNr=getSetNr(setname);
  if(setNr>-1)
  {
    /* store the selected elements in etmp */
    /* allocate enough space for later elem-nr indexing */ 
    if ( (etmp = (Elements *)realloc( (Elements *)etmp, (anz->emax+1) * sizeof(Elements))) == NULL )
      printf("\n\n ERROR: realloc failed etmp\n\n") ;
    for(j=0; j<set[setNr].anz_e; j++)
    {
	/* for the moment only he8 is supported */
      if (e_enqire[set[setNr].elem[j]].type != 1)
      {
        printf(" ERROR: elem-type:%d not supported\n", e_enqire[set[setNr].elem[j]].type);
        return(0);
      }
      noe++;
      etmp[noe].nr   = set[setNr].elem[j]; 
      etmp[noe].type = e_enqire[set[setNr].elem[j]].type; 
      etmp[noe].side = NULL;
      etmp[noe].group= 0;
      etmp[noe].mat  = 0;
      ipuf=0;
      if (e_enqire[set[setNr].elem[j]].type == 1) ipuf = 8;  /* HEXA8 */
      else if (e_enqire[set[setNr].elem[j]].type == 2) ipuf = 6;  /* PENTA6 */
      else if (e_enqire[set[setNr].elem[j]].type == 3) ipuf = 4;  /* TET4 */
      else if (e_enqire[set[setNr].elem[j]].type == 4) ipuf = 20; /* HEX20 */
      else if (e_enqire[set[setNr].elem[j]].type == 5) ipuf = 15; /* PENTA15 */
      else if (e_enqire[set[setNr].elem[j]].type == 6) ipuf = 10; /* TET10 */
      else if (e_enqire[set[setNr].elem[j]].type == 7) ipuf = 3;  /* TRI3  */
      else if (e_enqire[set[setNr].elem[j]].type == 8) ipuf = 6;  /* TRI6  */
      else if (e_enqire[set[setNr].elem[j]].type == 9) ipuf = 4;  /* QUAD4 */
      else if (e_enqire[set[setNr].elem[j]].type == 10) ipuf = 8; /* QUAD8 */
      else if (e_enqire[set[setNr].elem[j]].type == 11) ipuf = 2; /* BEAM */
      else if (e_enqire[set[setNr].elem[j]].type == 12) ipuf = 3; /* BEAM3 */
      else
      { printf("ERROR1: elem-type:%d unsupported %d\n",e_enqire[set[setNr].elem[j]].type, j); }
      for (n=0; n<ipuf; n++)
      {
        etmp[noe].nod[n]=e_enqire[set[setNr].elem[j]].nod[n];
      }
    }
  }
  noe++;


  /* extract the inner faces */
  /* search double faces and keep the first, remove single faces */
  nf=innerFacesHe8(etmp, noe, &iface);
  if(nf[1]!=nof)
  {
     printf(" ERROR: number of specified outer faces:%d does not match detected faces:%d\n", nof, nf[1]);
     goto closeFunc;
  }
  nif=nf[0];
  

#if TEST
  printf("nif:%d\n",nif); 
  printf("\n inner faces of %s\n", setname );
  for(n=0; n<nif; n++)
  {
      printf("  face:%d el:%d side:%d\n", n, iface[n].elem_nr, iface[n].nr+1 );
      printf("  face:%d el:%d side:%d\n", n, iface[n].nod[4], iface[n].nod[5] );
      for (i=0; i<4; i++) printf("%d ", iface[n].nod[i]); printf("\n");
  }
#endif

  /* update the element definition, replace node-nr by face-nr */
  for(i=0; i<=anz->emax; i++)
  {
    etmp[i].group=0;
    for(n=0; n<8; n++) etmp[i].nod[n]=9999;
  }
  for(i=0; i<nif; i++)
  {
    etmp[iface[i].elem_nr].group=1;
    etmp[iface[i].elem_nr].nod[iface[i].nr-1]=i;
    etmp[iface[i].nod[4]].nod[iface[i].nod[5]-1]=i;
  }
  for(i=0; i<nof; i++)
  {
    etmp[oface[i].elem_nr].group=1;
    etmp[oface[i].elem_nr].nod[oface[i].nr-1]=i+nif;
  }


  /* write nodes, elems, faces and boundary description */

  if((nod= (int *)realloc( (int *)nod, (anz->nmax+1) *sizeof(int)))==NULL )
    printf("\n\n ERROR: realloc failed nod\n\n") ;
  if((startface= (int *)realloc( (int *)startface, (boundaries+2) *sizeof(int)))==NULL )
    printf("\n\n ERROR: realloc failed startface\n\n") ;

  /* write points */
  handle = fopen ("points", "w");
  if ( handle== NULL )
  {
    printf ("\nThe input file points could not be opened.\n\n"); 
    return(0);
  }
  fprintf(handle, "FoamFile\n{\n");
  fprintf(handle, "    version 2.0;\n");
  fprintf(handle, "    format ascii;\n");
  fprintf(handle, "\n");
  fprintf(handle, "    root \"..\";\n");
  fprintf(handle, "    case \".\";\n");
  fprintf(handle, "    instance \"constant\";\n");
  fprintf(handle, "    local \"polyMesh\";\n");
  fprintf(handle, "\n");
  fprintf(handle, "    class vectorField;\n");
  fprintf(handle, "    object points;\n}\n\n");

  fprintf(handle, "%d\n(\n", set[setNr].anz_n ); 
  for(i=0; i<set[setNr].anz_n; i++)
  {
    nod[set[setNr].node[i]]=i;
    fprintf(handle, "(%.12lf %.12lf %.12lf)\n",node[set[setNr].node[i]].nx, node[set[setNr].node[i]].ny, node[set[setNr].node[i]].nz);
#if TEST
    printf("%d (%lf %lf %lf)\n",i, node[set[setNr].node[i]].nx, node[set[setNr].node[i]].ny, node[set[setNr].node[i]].nz);
#endif
  }
  fprintf(handle, ")\n"); 
  fclose(handle);


  /* write faces */
  handle = fopen ("faces", "w");
  if ( handle== NULL )
  {
    printf ("\nThe input file faces could not be opened.\n\n"); 
    return(0);
  }
  fprintf(handle, "FoamFile\n{\n");
  fprintf(handle, "    version 2.0;\n");
  fprintf(handle, "    format ascii;\n");
  fprintf(handle, "\n");
  fprintf(handle, "    root \"..\";\n");
  fprintf(handle, "    case \".\";\n");
  fprintf(handle, "    instance \"constant\";\n");
  fprintf(handle, "    local \"polyMesh\";\n");
  fprintf(handle, "\n");
  fprintf(handle, "    class faceList;\n");
  fprintf(handle, "    object faces;\n}\n\n");

  fprintf(handle, "%d\n(\n", nif+nof); 
  for(i=0; i<nif; i++)
  {
    /* replace the node-nr by the node index */
    for(n=0; n<4; n++) iface[i].nod[n]=nod[iface[i].nod[n]];
    fprintf(handle, "4(%d %d %d %d)\n",iface[i].nod[0],iface[i].nod[1],iface[i].nod[2],iface[i].nod[3]);
#if TEST
    printf("%d 4(%d %d %d %d)\n",i, iface[i].nod[0],iface[i].nod[1],iface[i].nod[2],iface[i].nod[3]);
#endif
  }

  patches=0;
  startface[patches]=nif;
  for(i=0; i<nof; i++)
  {
    for(n=0; n<4; n++) oface[i].nod[n]=nod[oface[i].nod[n]];
    fprintf(handle, "4(%d %d %d %d)\n",oface[i].nod[0],oface[i].nod[1],oface[i].nod[2],oface[i].nod[3]);
    if(oface[i].group==1) {  patches++; startface[patches]=i+nif+1; }
#if TEST
    printf("%d 4(%d %d %d %d)\n",i+nif,oface[i].nod[0],oface[i].nod[1],oface[i].nod[2],oface[i].nod[3]);
#endif
  }
  fprintf(handle, ")\n"); 
  fclose(handle);

  /* check if we have patches for all boundaries */
  if(patches!=boundaries)
  { printf("ERROR: %d boundaries specified but found only %d patches in the mesh\n",boundaries, patches); return(0); }


  /* write owner */
  handle = fopen ("owner", "w");
  if ( handle== NULL )
  {
    printf ("\nThe input file faces could not be opened.\n\n"); 
    return(0);
  }
  fprintf(handle, "FoamFile\n{\n");
  fprintf(handle, "    version 2.0;\n");
  fprintf(handle, "    format ascii;\n");
  fprintf(handle, "    class labelList;\n");
  fprintf(handle, "    location \"constant/polyMesh\";\n");
  fprintf(handle, "    object owner;\n}\n\n");

  fprintf(handle, "%d\n(\n", nif+nof); 
  for(i=0; i<nif; i++)
  {
    fprintf(handle, "%d\n",iface[i].elem_nr-1);
  }
  for(i=0; i<nof; i++)
  {
    fprintf(handle, "%d\n",oface[i].elem_nr-1);
  }
  fprintf(handle, ")\n"); 
  fclose(handle);


  /* write neighbour */
  handle = fopen ("neighbour", "w");
  if ( handle== NULL )
  {
    printf ("\nThe input file faces could not be opened.\n\n"); 
    return(0);
  }
  fprintf(handle, "FoamFile\n{\n");
  fprintf(handle, "    version 2.0;\n");
  fprintf(handle, "    format ascii;\n");
  fprintf(handle, "    class labelList;\n");
  fprintf(handle, "    location \"constant/polyMesh\";\n");
  fprintf(handle, "    object neighbour;\n}\n\n");

  fprintf(handle, "%d\n(\n", nif); 
  for(i=0; i<nif; i++)
  {
    fprintf(handle, "%d\n",iface[i].nod[4]-1);
  }
  fprintf(handle, ")\n"); 
  fclose(handle);


  /* old syle of element definition (pre openFoam 1.3) */
#if WRITE_CELLS
  /* cells */
  noe=0;
  for(i=1; i<=anz->emax; i++)
  {
      /* if( etmp[i].group==1) */
    {
#if TEST
      printf( "6(%d %d %d %d %d %d)\n", etmp[i].nod[0], etmp[i].nod[1], etmp[i].nod[2], etmp[i].nod[3], etmp[i].nod[4], etmp[i].nod[5]);
#endif
      /* reordering faces from abq to foam */
      for(n=0; n<6; n++) fbuf[n]=etmp[i].nod[n];
      etmp[noe].nod[4]=fbuf[0];
      etmp[noe].nod[5]=fbuf[1];
      etmp[noe].nod[2]=fbuf[2];
      etmp[noe].nod[1]=fbuf[3];
      etmp[noe].nod[3]=fbuf[4];
      etmp[noe].nod[0]=fbuf[5];
      noe++;
    }
  }
  handle = fopen ("cells", "w");
  if ( handle== NULL )
  {
    printf ("\nThe input file cells could not be opened.\n\n"); 
    return(0);
  }
  fprintf(handle, "FoamFile\n{\n");
  fprintf(handle, "    version 2.0;\n");
  fprintf(handle, "    format ascii;\n");
  fprintf(handle, "\n");
  fprintf(handle, "    root \"..\";\n");
  fprintf(handle, "    case \".\";\n");
  fprintf(handle, "    instance \"constant\";\n");
  fprintf(handle, "    local \"polyMesh\";\n");
  fprintf(handle, "\n");
  fprintf(handle, "    class cellList;\n");
  fprintf(handle, "    object cells;\n}\n\n");

  fprintf(handle, "%d\n(\n", noe); 
  for(i=0; i<noe; i++)
    fprintf(handle, "6(%d %d %d %d %d %d)\n", etmp[i].nod[0], etmp[i].nod[1], etmp[i].nod[2], etmp[i].nod[3], etmp[i].nod[4], etmp[i].nod[5]);
  fprintf(handle, ")\n"); 
  fclose(handle);
#endif
 

  /* boundary */
  handle = fopen ("boundary", "w");
  if ( handle== NULL )
  {
    printf ("\nThe input file cells could not be opened.\n\n"); 
    return(0);
  }
  fprintf(handle, "FoamFile\n{\n");
  fprintf(handle, "    version 2.0;\n");
  fprintf(handle, "    format ascii;\n");
  fprintf(handle, "\n");
  fprintf(handle, "    root \"..\";\n");
  fprintf(handle, "    case \".\";\n");
  fprintf(handle, "    instance \"constant\";\n");
  fprintf(handle, "    local \"polyMesh\";\n");
  fprintf(handle, "\n");
  fprintf(handle, "    class polyBoundaryMesh;\n");
  fprintf(handle, "    object boundary;\n}\n\n");

  if(boucyc[0]>-1) fprintf(handle, "%d\n(\n", boundaries-1);
  else fprintf(handle, "%d\n(\n", boundaries);

  for(i=0; i<boundaries; i++)
  {
    if(i==boucyc[0])
    {
      /* cyclic boundaries */
      fprintf(handle, "  %s\n  {\n",bound[i]);
      fprintf(handle, "    type %s;\n",type[i]);
      j=boucyc[1];
      fprintf(handle, "    nFaces %d;\n",(startface[i+1]-startface[i])+(startface[j+1]-startface[j]));
      fprintf(handle, "    startFace %d;\n  }\n\n",startface[i]);
    }
    else if(i==boucyc[1]);
    else
    {
      fprintf(handle, "  %s\n  {\n",bound[i]);
      fprintf(handle, "    type %s;\n",type[i]);
      fprintf(handle, "    nFaces %d;\n",startface[i+1]-startface[i]);
      fprintf(handle, "    startFace %d;\n  }\n\n",startface[i]);
    }
  }
  fprintf(handle, ")\n"); 
  fclose(handle);

 closeFunc:;
  free(iface);
  free(oface);
  free(etmp);
  free(nod);
  free(startface);
  free(type);
  free(bound);

  printf(" wrote foam mesh\n");
  return (1);
}

