/* ----------------------------------------------------------------  */
/* readFoam                                        7.07.2005 Wittig  */
/* ----------------------------------------------------------------  */


#include <cgx.h>
#include <sys/dir.h>

#define kappa    1.4

#define TEST     0

#define MAX_ENTITIES      100
#define CHECK_INPUT       0

extern Sets      *setx;
extern Summen    *anzx;
extern Alias     *alias;
extern SumGeo    anzGeo[1];
extern SumAsci   sumAsci[1];


typedef struct {
  int anz_f;
  int *face;
  int *ori;
}Elemf;

typedef struct {
  int p;
  int u;
  int t;
  int rho;
}Ds;


int get_values_in_string( char *handle, double *fval)
{
  int i=0, pos=0;
  static int parenthesis=0;
  char byte;
  char string[MAX_LINE_LENGTH];


  /* count open and closed parenthesis */
  do
  {
    byte = handle[pos++];
    if(byte=='(') parenthesis++;
    else if(byte==')')
    {
      parenthesis--;

      /* return if the first open '(' is closed */
      if(parenthesis==0) break;

      /* if one "(" is open get the values and return */
      if(parenthesis==1)
      {
        string[i++]=0;
        sscanf(string, "%lf %lf %lf", &fval[0], &fval[1], &fval[2]);
        return(pos);
      }
    }
    else if (parenthesis==2)
    {
      if(i<MAX_LINE_LENGTH-2) string[i++]=byte;
    }

  }while(byte!=0);
  fval[0]=fval[1]=fval[2]=0.;
  return(-parenthesis);
}

int defineElemNodes(Summen *anzx, Faces *face, Elements *elem, int i, int *eface, int anz_f)
{
  int f, l, n, j, k, ni[8];
  int fbuf[6], slavnod[2];

  if (anz_f==6) /* HEXA8  */
  {
    /* mark the faces as used */
    for(f=0; f<anz_f; f++) face[eface[f]].group++;
  
    elem[i].type = 1;
    anzx->etype[elem[i].type]++;
  
    /* bottom */
    /* owner */
    if(face[eface[0]].group==1)
    {
      elem[i].nod[0] = face[eface[0]].nod[0];         
      elem[i].nod[4] = face[eface[0]].nod[1];         
      elem[i].nod[7] = face[eface[0]].nod[2];         
      elem[i].nod[3] = face[eface[0]].nod[3];
    }
    /* neighbour */
    else if(face[eface[0]].group==2)
    {
      elem[i].nod[0] = face[eface[0]].nod[0];         
      elem[i].nod[4] = face[eface[0]].nod[3];         
      elem[i].nod[7] = face[eface[0]].nod[2];         
      elem[i].nod[3] = face[eface[0]].nod[1];
    }
    else { printf("ERROR in group:%d\n",face[eface[0]].group); exit(1); }
  
    /* search face on node 0 and 1 from face0 */
    for(f=2; f<anz_f; f++)
    { 
      for(l=0; l<4; l++)
      {
        if(face[eface[f]].nod[l]==face[eface[0]].nod[0])
        {
          slavnod[0]=l;
          goto found_face;
        }
      }
    }
    printf("ERROR: found no slavface for elem:%d\n",i ); exit(1);
  found_face:;
    for(l=0; l<4; l++)
    {
      for(n=1; n<4; n++)
      {
        if(face[eface[f]].nod[l]==face[eface[0]].nod[n])
        {
          if((l==0)&&(slavnod[0]==1)) slavnod[1]=2;
        else if((l==1)&&(slavnod[0]==2)) slavnod[1]=3;
        else if((l==2)&&(slavnod[0]==3)) slavnod[1]=0;
        else if((l==3)&&(slavnod[0]==0)) slavnod[1]=1;
  
        else if((l==2)&&(slavnod[0]==1)) slavnod[1]=0;
        else if((l==3)&&(slavnod[0]==2)) slavnod[1]=1;
        else if((l==0)&&(slavnod[0]==3)) slavnod[1]=2;
        else if((l==1)&&(slavnod[0]==0)) slavnod[1]=3;
        else { printf("ERROR: slavnod nod found l:%d slavnod[0]:%d\n",l,slavnod[0]); exit(1); }
          goto found_nodn;
        }
      }
    }
    printf("ERROR: found no slavnod[1] for elem:%d\n",i ); exit(1);
  found_nodn:;
  
    /* determine the node from face1 at the slavnod[1] */
    for(l=0; l<4; l++)
    {
      if(face[eface[f]].nod[slavnod[1]]==face[eface[1]].nod[l])
      {
        /* top */
        if(face[eface[1]].group==1)
        {
          if(l==0)
          {
            elem[i].nod[1] = face[eface[1]].nod[0];         
            elem[i].nod[2] = face[eface[1]].nod[1];         
            elem[i].nod[6] = face[eface[1]].nod[2];         
            elem[i].nod[5] = face[eface[1]].nod[3];
          }        
          else if(l==1)
          {
            elem[i].nod[1] = face[eface[1]].nod[1];         
            elem[i].nod[2] = face[eface[1]].nod[2];         
            elem[i].nod[6] = face[eface[1]].nod[3];         
            elem[i].nod[5] = face[eface[1]].nod[0];
          }        
          else if(l==2)
          {
            elem[i].nod[1] = face[eface[1]].nod[2];         
            elem[i].nod[2] = face[eface[1]].nod[3];         
            elem[i].nod[6] = face[eface[1]].nod[0];         
            elem[i].nod[5] = face[eface[1]].nod[1];
          }        
          else if(l==3)
          {
            elem[i].nod[1] = face[eface[1]].nod[3];         
            elem[i].nod[2] = face[eface[1]].nod[0];         
            elem[i].nod[6] = face[eface[1]].nod[1];         
            elem[i].nod[5] = face[eface[1]].nod[2];
          }        
          else { printf("ERROR: l nod found\n"); exit(1); }
        }
        else if(face[eface[1]].group==2)
        {
          if(l==0)
          {
            elem[i].nod[1] = face[eface[1]].nod[0];         
            elem[i].nod[2] = face[eface[1]].nod[3];         
            elem[i].nod[6] = face[eface[1]].nod[2];         
            elem[i].nod[5] = face[eface[1]].nod[1];
          }         
          else if(l==1)
          {
            elem[i].nod[1] = face[eface[1]].nod[1];         
            elem[i].nod[2] = face[eface[1]].nod[0];         
            elem[i].nod[6] = face[eface[1]].nod[3];         
            elem[i].nod[5] = face[eface[1]].nod[2];
          }         
          else if(l==2)
          {
            elem[i].nod[1] = face[eface[1]].nod[2];         
            elem[i].nod[2] = face[eface[1]].nod[1];         
            elem[i].nod[6] = face[eface[1]].nod[0];         
            elem[i].nod[5] = face[eface[1]].nod[3];
          }         
          else if(l==3)
          {
            elem[i].nod[1] = face[eface[1]].nod[3];         
            elem[i].nod[2] = face[eface[1]].nod[2];         
            elem[i].nod[6] = face[eface[1]].nod[1];         
            elem[i].nod[5] = face[eface[1]].nod[0];
          }         
          else { printf("ERROR: l nod found\n"); exit(1); }
        }
        else { printf("ERROR in group:%d\n",face[eface[1]].group); exit(1); }
        break;
      }
    }
  
    for(f=0; f<anz_f; f++)
    {
      face[eface[f]].elem_nr=elem[i].nr;
  
      n=0;
      for(j=0; j<8; j++) 
        for(k=0; k<4; k++)
          if(elem[i].nod[j]==face[eface[f]].nod[k])
            ni[n++]=j+1;
      if (n==4)
      {
        /* check which sides are involved */
        for (j=0; j<6; j++) fbuf[j]=0;
        for (j=0; j<4; j++)
        {
          if ((ni[j]==1)||(ni[j]==2)||(ni[j]==3)||(ni[j]==4)) fbuf[0]++;
          if ((ni[j]==5)||(ni[j]==8)||(ni[j]==7)||(ni[j]==6)) fbuf[1]++;
            if ((ni[j]==1)||(ni[j]==5)||(ni[j]==6)||(ni[j]==2)) fbuf[2]++;
          if ((ni[j]==2)||(ni[j]==6)||(ni[j]==7)||(ni[j]==3)) fbuf[3]++;
          if ((ni[j]==3)||(ni[j]==7)||(ni[j]==8)||(ni[j]==4)) fbuf[4]++;
          if ((ni[j]==4)||(ni[j]==8)||(ni[j]==5)||(ni[j]==1)) fbuf[5]++;
        }
      }
      for (j=0; j<6; j++)
      {
        /* printf(" j:%d fbuf[j]:%d\n", j, fbuf[j]); */
        if(fbuf[j]==4)
        {
          face[eface[f]].nr=j;
          if( anzx->emax>anzx->f) 
          {
            if ( (face = (Faces *)realloc((Faces *)face, (anzx->emax+2) * sizeof(Faces))) == NULL )
            {  printf("\n\n ERROR: malloc failed, face\n\n") ; exit(1); }
	  }
          face[elem[i].nr].indx[j]=eface[f];
	}
      }
    }
  }
  else if (anz_f==5) /* PE6    */
  {
    elem[i].type = 2; printf("elem-type:pe6 not installed\n"); return(0);
  }
  else if (anz_f==4) /* TET4   */
  {
    elem[i].type = 3; printf("elem-type:tet4 not installed\n"); return(0); 
  }
  else
  {
    printf("\n\n ERROR: number of faces:%d of element:%d do not match available types of elements\n", anz_f, i);
    return(0);
  }
  
#if TEST
          printf (" e:%d t:%d f:",elem[i].nr, elem[i].type);
          for(j=0; j<anz_f; j++)  printf (" %d",eface[j]);
          printf ("\nnode:");
          for(j=0; j<8; j++)  printf (" %d",elem[i].nod[j]);
          printf ("\n");
#endif
   return(1);
}





int readFoam(char *datin, Summen *apre, Sets **sptr, Nodes **nptr, Elements **eptr, Datasets **lptr  )
{
  int i,j,k=0,l,n,f,f1=0,f2, anz_f, length, pos;
  int eface[8], sum_dir, ncomps;
  FILE *handle;
  DIR *dirp;
  struct direct *dp;
  Rsort *rsort=NULL;
  Ds ds;

  double cp, Rg;
  double ps, us, ts, pt, tt, rho, ma;


  char file[MAX_LINE_LENGTH], rec_str[MAX_LINE_LENGTH];
  char dat[2][MAX_LINE_LENGTH];
  char resultname[MAX_LINE_LENGTH];
  char buffer[MAX_LINE_LENGTH];
  char **resultdir=NULL;
  char **resultdir2=NULL;

  Nodes     *node = NULL;
  Faces     *face = NULL;
  Elements  *elem = NULL;
  Datasets  *lcase= NULL;
  Elemf     *elemf= NULL;

  int anz_dat=0, ipuf=0, *vpn=NULL;
  double  fval[10];

  int startFace, nFaces, setNr=-1, formFlag=0;

  anzx=apre;
  setx=*sptr;

  anzx->n=-1;
  anzx->f=-1;
  anzx->e=-1;
  anzx->l=-1;

  /* Open the files and check to see that it was opened correctly */

  /* mesh */
  strcpy(anzx->model, "foam");
  printf (" MODEL NAME:  %s\n", anzx->model);

  anzx->n=anzx->f=anzx->e=0;
  anzx->nmax=-MAX_INTEGER;  anzx->nmin=MAX_INTEGER;
  anzx->emax=-MAX_INTEGER;  anzx->emin=MAX_INTEGER;

  printf ("\n Read the Mesh  \n");

  /* nodes */
  /* located in "datin"/constant/polymesh/points */
  sprintf( file, "%s/constant/polyMesh/points", datin);
  handle = fopen (file, "r");
  if ( handle== NULL )
  {
    printf ("\nThe input file \"%s\" could not be opened.\n", file);
  }
  else printf ("\n%s opened\n",file);
  while(1)
  {
    length = frecord( handle, rec_str);
    if (rec_str[length] == (char)EOF) break;

    /* read data if a block starts */
    if(rec_str[0]=='(')
    {
      if ( (node = (Nodes *)malloc( (anzx->n+1) * sizeof(Nodes))) == NULL )
      {  printf("\n\n ERROR: malloc failed, node\n\n") ; exit(1); }
      for(i=0; i<anzx->n; i++)
      {
        node[i].nr=i+1;
        node[node[i].nr].indx=i;
        length = frecord( handle, rec_str);
        sscanf(&rec_str[1], "%lf %lf %lf", &node[node[i].nr].nx, &node[node[i].nr].ny, &node[node[i].nr].nz);
#if TEST
        printf (" n=%d x=%lf y=%lf z=%lf \n", node[i].nr, node[node[i].nr].nx, node[node[i].nr].ny, node[node[i].nr].nz); 
#endif
      }
      break;
    }
    else if(length>0) anzx->n=atoi(rec_str);
  }
  fclose(handle);
  anzx->nmax=anzx->n;
  anzx->nmin=1;

  /* faces */
  /* located in "datin"/constant/polymesh/faces */
  sprintf( file, "%s/constant/polyMesh/faces", datin);
  handle = fopen (file, "r");
  if ( handle== NULL )
  {
    printf ("\nThe input file \"%s\" could not be opened.\n", file);
  }
  else printf ("\n%s opened\n",file);
  while(1)
  {
    length = frecord( handle, rec_str);
    if (rec_str[length] == (char)EOF) break;

    /* read data if a block starts */
    if(rec_str[0]=='(')
    {
      if ( (face = (Faces *)malloc( (anzx->f+2) * sizeof(Faces))) == NULL )
      {  printf("\n\n ERROR: malloc failed, face\n\n") ; exit(1); }
      for(i=0; i<anzx->f; i++)
      {
        face[i].group=0;
        face[i].mat=-1;
        face[i].side=NULL;
        length = frecord( handle, rec_str);
        sscanf(rec_str, "%d",&n);
        sscanf(&rec_str[2], "%d %d %d %d %d %d %d %d",&face[i].nod[0],&face[i].nod[1],&face[i].nod[2],&face[i].nod[3],&face[i].nod[4],&face[i].nod[5],&face[i].nod[6],&face[i].nod[7]);
        for(j=0; j<n; j++)  face[i].nod[j]++; /* nodenr is one higher! */

        if(n==3)      face[i].type = 7;  /* TRI3  */ 
        else if(n==6) face[i].type = 8;  /* TRI6  */  
        else if(n==4) face[i].type = 9;  /* QUAD4 */  
        else if(n==8) face[i].type = 10; /* QUAD8 */
        else
        {
          printf("\n\n ERROR: number of nodes:%d of face:%d do not match available types of elements\n",n,i);
          exit(1);
        }

#if TEST
        printf (" f:%d t:%d",i, face[i].type);
        for(j=0; j<n; j++)  printf (" %d",face[i].nod[j]);
        printf ("\n");
#endif
      }
      break;
    }
    else if(length>0) anzx->f=atoi(rec_str);
  }
  fclose(handle);

  /* elements */
  /* located in "datin"/constant/polymesh/cells */
  sprintf( file, "%s/constant/polyMesh/cells", datin);
  handle = fopen (file, "r");
  if ( handle== NULL )
  {
    //printf ("\nThe input file \"%s\" could not be opened.\n", file);

    /* no cells found, try to read owner and neighbour */
    sprintf( file, "%s/constant/polyMesh/owner", datin);
    handle = fopen (file, "r");
    if ( handle== NULL )
    {
      printf("ERROR: found no owner file\n"); exit(1);
    }

    /* create a list of faces per element */
    printf ("\n%s opened\n",file);
    while(1)
    {
      length = frecord( handle, rec_str);
      if (rec_str[length] == (char)EOF) break;
  
      if(rec_str[0]=='(') break;
    }
    /* read data if a block starts */
    i=0;
    while(1)
    {
      length = frecord( handle, rec_str);
      if (rec_str[length] == (char)EOF) break;
      if(rec_str[0]==')') break;
      sscanf(rec_str, "%d",&face[i].elem_nr);
      if(face[i].elem_nr>anzx->e) anzx->e=face[i].elem_nr;
      i++;
    }
    fclose(handle);
    anzx->e++;

    sprintf( file, "%s/constant/polyMesh/neighbour", datin);
    handle = fopen (file, "r");
    if ( handle== NULL )
    {
      printf("ERROR: found no owner file\n"); exit(1);
    }

    /* create a list of faces per element */
    printf ("\n%s opened\n",file);
    while(1)
    {
      length = frecord( handle, rec_str);
      if (rec_str[length] == (char)EOF) break;
  
      if(rec_str[0]=='(') break;
    }
    /* read data if a block starts */
    i=0;
    while(1)
    {
      length = frecord( handle, rec_str);
      if (rec_str[length] == (char)EOF) break;
      if(rec_str[0]==')') break;
      sscanf(rec_str, "%d",&face[i++].mat);  
    }
    fclose(handle);

    /* define the cells */
    if ( (elemf = (Elemf *)malloc( (anzx->e+1) * sizeof(Elemf))) == NULL )
    {  printf("\n\n ERROR: malloc failed, elemf\n\n") ; exit(1); }
    for(i=0; i<anzx->e; i++) elemf[i].face=NULL;
    for(i=0; i<anzx->e; i++) elemf[i].anz_f=0;
    for(i=0; i<anzx->f; i++)
    {
      if ( (elemf[face[i].elem_nr].face = (int *)realloc( (int *)elemf[face[i].elem_nr].face, (elemf[face[i].elem_nr].anz_f+1) * sizeof(int))) == NULL )
      {  printf("\n\n ERROR: malloc failed, elemf\n\n") ; exit(1); }

      elemf[face[i].elem_nr].face[elemf[face[i].elem_nr].anz_f]=i;
      elemf[face[i].elem_nr].anz_f++;
      if(face[i].mat>-1)
      {
        if ( (elemf[face[i].mat].face = (int *)realloc( (int *)elemf[face[i].mat].face, (elemf[face[i].mat].anz_f+1) * sizeof(int))) == NULL )
        {  printf("\n\n ERROR: malloc failed, elemf\n\n") ; exit(1); }
        elemf[face[i].mat].face[elemf[face[i].mat].anz_f]=i;
        elemf[face[i].mat].anz_f++;
      }
    }

    /* define the elements */
    if ( (elem = (Elements *)malloc( (anzx->e+1) * sizeof(Elements))) == NULL )
    {  printf("\n\n ERROR: malloc failed, elem\n\n") ; exit(1); }
    for(i=0; i<anzx->e; i++)
    {
      /* search two disjunct faces */
      eface[0]=elemf[i].face[0];
      f2=2;
      if     (face[eface[0]].type == 7) f1=3;  /* TRI3  */ 
      else if(face[eface[0]].type == 8) f1=6;  /* TRI6  */  
      else if(face[eface[0]].type == 9) f1=4;  /* QUAD4 */  
      else if(face[eface[0]].type == 10) f1=8; /* QUAD8 */
      for(f=1; f<elemf[i].anz_f; f++)
      {
	if     (face[elemf[i].face[f]].type == 7) k=3;  /* TRI3  */ 
        else if(face[elemf[i].face[f]].type == 8) k=6;  /* TRI6  */  
        else if(face[elemf[i].face[f]].type == 9) k=4;  /* QUAD4 */  
        else if(face[elemf[i].face[f]].type == 10) k=8; /* QUAD8 */
        for(n=0; n<k; n++)
	{
	  for(j=0; j<f1; j++)
          {
	    if(face[elemf[i].face[f]].nod[n]==face[eface[0]].nod[j])
            {
              eface[f2++]=elemf[i].face[f];
              goto isconnected;
            }
          }
        }
        eface[1]=elemf[i].face[f];
      isconnected:;
      }
#if TEST
      printf("face order (%d): ", elemf[i].anz_f);
      for(f=0; f<elemf[i].anz_f; f++) printf(" %d", eface[f]);
      printf("\n");
#endif

      elem[i].nr = i+1;
      elem[i].group= 0;
      elem[i].mat = 1;

      defineElemNodes(anzx, face, elem, i, eface, elemf[i].anz_f);
    }
  }
  else
  {
    printf ("\n%s opened\n",file);
    while(1)
    {
      length = frecord( handle, rec_str);
      if (rec_str[length] == (char)EOF) break;
  
      /* read data if a block starts */
      if(rec_str[0]=='(')
      {
        if ( (elem = (Elements *)malloc( (anzx->e+1) * sizeof(Elements))) == NULL )
        {  printf("\n\n ERROR: malloc failed, elem\n\n") ; exit(1); }
        for(i=0; i<anzx->e; i++)
        {
    	  elem[i].nr = i+1;
          elem[i].group= 0;
          elem[i].mat = 1;
          length = frecord( handle, rec_str);
          sscanf(rec_str, "%d",&anz_f);
          sscanf(&rec_str[2], "%d %d %d %d %d %d",&eface[0],&eface[1],&eface[2],&eface[3],&eface[4],&eface[5]);
          defineElemNodes(anzx, face, elem, i, eface, anz_f);  
        }
        break;
      }
      else if(length>0) anzx->e=atoi(rec_str);
    }
    fclose(handle);
  }
  anzx->emax=anzx->e;
  anzx->emin=1;


  /* boundaries */
  /* located in "datin"/constant/polymesh/boundary */
  sprintf( file, "%s/constant/polyMesh/boundary", datin);
  handle = fopen (file, "r");
  if ( handle== NULL )
  {
    printf ("\nThe input file \"%s\" could not be opened.\n", file);
  }
  else printf ("\n%s opened\n",file);
  while(1)
  {
    length = frecord( handle, rec_str);
    if (rec_str[length] == (char)EOF) break;

    /* read data if a block starts */
    if(rec_str[0]=='(')
    {
      while(1)
      {
        length = frecord( handle, rec_str);
        if (rec_str[length] == (char)EOF) break;
        length = strfind(rec_str, ")");
        if ( length!=-1 ) break;

        length = strfind(rec_str, "{");
        if ( length!=-1 )
        {
          printf("\n   setname:%s\n",  buffer);
          setNr=getSetNrx(buffer);
          if(setNr<0) setNr=pre_setax( buffer, "is", 0);
        }
        else sscanf(rec_str, "%s", buffer);

        length = strfind(rec_str, "nFaces");
        if ( length!=-1 )
        {
          sscanf(rec_str,"%*s %d", &nFaces);
          printf("   nFaces:%d\n", nFaces );
        }

        length = strfind(rec_str, "startFace");
        if ( length!=-1 )
        {
          sscanf(rec_str,"%*s %d", &startFace);
          printf("   startFace:%d\n", startFace );
        }

        /* store the faces in the set if the defining block was closed */
        length = strfind(rec_str, "}");
        if ( length!=-1 )
        {
          for(j=0; j<nFaces; j++)
          {
            if(face[j+startFace].nr>-1) 
            {
              setax(setNr,"f", j+startFace);
              i=setax(setNr,"j",0);
              if(i>-1)
              {
                setx[setNr].elf[i].e=face[j+startFace].elem_nr;
                setx[setNr].elf[i].f=face[j+startFace].nr;
              }
	    }
          }
        }
      }
    }
  }
  fclose(handle);

  /* scan over all project directories and search for results */
  sum_dir=0;
  dirp = opendir(datin);
  printf("seach for results in %s\n", datin );
  if (dirp != NULL) while ((dp = readdir(dirp)) != NULL)
  {

    /* if the dir-name starts with a number, its a result dir */
    if((dp->d_name[0]>=48)&&(dp->d_name[0]<=57))
    {
      if ( (resultdir2 = (char **)realloc( resultdir2, (sum_dir+2) * sizeof(char *))) == NULL )
        printf("\n\n ERROR: realloc failure\n\n" );
      if ( (resultdir2[sum_dir] = (char *)malloc( (MAX_LINE_LENGTH) * sizeof(char))) == NULL )
        printf("\n\n ERROR: realloc failure\n\n" );
      sprintf( resultdir2[sum_dir], "%s/%s", datin, dp->d_name );

      if ( (rsort = (Rsort *)realloc( rsort, (sum_dir+1) * sizeof(Rsort))) == NULL )
        printf("ERROR: realloc failed: Rsort\n\n" ); 
      rsort[sum_dir].r=atof(dp->d_name);
      rsort[sum_dir].i=sum_dir;
      sum_dir++;
    }
  }
  closedir(dirp);

  /* sort the resultdirs according to their name-value (its the time-step value) */
  qsort( rsort, sum_dir, sizeof(Rsort), (void *)compareRsort );
  if ( (resultdir = (char **)realloc( resultdir, (sum_dir+1) * sizeof(char *))) == NULL )
    printf("\n\n ERROR: realloc failure\n\n" );
  for( i=0; i<sum_dir; i++)
  {
    if ( (resultdir[i] = (char *)malloc( (MAX_LINE_LENGTH) * sizeof(char))) == NULL )
      printf("\n\n ERROR: realloc failure\n\n" );
    strcpy( resultdir[i], resultdir2[rsort[i].i]);
    printf(" found results in dir:%s\n",resultdir[i]);
  }


  if ( (vpn = (int *)malloc( (anzx->nmax+1) * sizeof(int))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );	               
  for(i=0; i<=anzx->nmax; i++) vpn[i]=0;

  for(l=0; l<sum_dir; l++)
  {
    /* search for files in dir */
    dirp = opendir(resultdir[l]);
    if (dirp != NULL)
    {
     ds.p=ds.t=ds.u=ds.rho=-1;

     /* scan all files */
     while ((dp = readdir(dirp)) != NULL)
     {
      if ((strlen(dp->d_name) >0 )&&(dp->d_name[strlen(dp->d_name)-1]!='~')&&(dp->d_name[0]!='.'))
      {
        /* open the dir/file */
        sprintf(file, "%s/%s",resultdir[l],dp->d_name); 
        handle = fopen (file, "r");
        if ( handle== NULL )
        {
          printf ("\nThe input file \"%s\" could not be opened.\n", file);
          goto noFile;
        }
        else printf("file:%s opened\n", file);
 
        /* determine the type of data and create the entity names */
        ncomps=0;
        while(1)
        {
          length = frecord( handle, rec_str);
          // printf ("record: %s", rec_str); 
          if (rec_str[length] == (char)EOF) break;
  
          /* read data when the header starts */
          else if(rec_str[0]=='{')
          {
            while(1)
            {
              length = frecord( handle, rec_str);
              /* printf ("record: %s", rec_str); */
              if (rec_str[length] == (char)EOF) { printf("ERROR\n"); exit(1); }
              if (rec_str[0] == '}') goto foundEntities;

              sscanf(rec_str, "%s %s", dat[0], dat[1]);
              // printf ("dat:%s| dat:%s|\n",dat[0], dat[1]);

              if(compareStrings(dat[0], "class")>0)
              {
                if( compare(dat[1], "volScalarField", 14)==14) ncomps=1;
                else if( compare(dat[1], "volVectorField", 14)==14) ncomps=3;
                else { printf("Warning; class:%s not known\n",dat[1]); ncomps=0; }
              }
              if(compareStrings(dat[0], "object")>0)
              { strcpy(resultname,dat[1]); resultname[strlen(dat[1])]=0; }
            }
          }
        } foundEntities:;
        printf ("  resultname:%s entities:%d\n", resultname, ncomps);

        if(ncomps<1) goto foundNoEntities;
    
        /* define the lcases */
        anzx->l++;
    
        /* store Datasets */
        if ( (lcase = (Datasets *)realloc(lcase, (anzx->l+1) * sizeof(Datasets))) == NULL )
          printf("\n\n ERROR: malloc failed, lcase\n\n") ;

        i=strlen(resultdir[l]); while((i)&&(resultdir[l][i]!='/')) i--;
        lcase[anzx->l].value=atof(&resultdir[l][i+1]);
        if(compareStrings(&resultdir[l][i+1],"0")==1) sprintf( lcase[anzx->l].dataset_name,"%s", "initial");
        else sprintf( lcase[anzx->l].dataset_name,"%s    ", "result");

        lcase[anzx->l].irtype = 1;
        lcase[anzx->l].npheader = 0;
        strcpy(lcase[anzx->l].analysis_name,"");
        strcpy(lcase[anzx->l].name, dp->d_name );
        strcpy(lcase[anzx->l].dataset_text,"");
        lcase[anzx->l].step_number=l+1;
        lcase[anzx->l].analysis_type=1;

        printf ("lcase.name[%d]= %s value:%f\n", anzx->l, lcase[anzx->l].name, lcase[anzx->l].value);
        if(lcase[anzx->l].name[0]=='p') ds.p=anzx->l;
        if(lcase[anzx->l].name[0]=='U') ds.u=anzx->l;
        if(lcase[anzx->l].name[0]=='T') ds.t=anzx->l;
        if(lcase[anzx->l].name[0]=='r') ds.rho=anzx->l;
        lcase[anzx->l].ncomps  = ncomps;
    

        if ( (lcase[anzx->l].nmax = (int *)malloc( lcase[anzx->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[anzx->l].nmin = (int *)malloc( lcase[anzx->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[anzx->l].max = (float *)malloc( lcase[anzx->l].ncomps * sizeof(float))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[anzx->l].min = (float *)malloc( lcase[anzx->l].ncomps * sizeof(float))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[anzx->l].dat = (float **)malloc( lcase[anzx->l].ncomps * sizeof(float *))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[anzx->l].icname = (char **)malloc( lcase[anzx->l].ncomps * sizeof(char *))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[anzx->l].compName = (char **)malloc( lcase[anzx->l].ncomps * sizeof(char *))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        for(i=0; i<lcase[anzx->l].ncomps; i++)
        {
          if ( (lcase[anzx->l].compName[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
            printf("\n\n ERROR: malloc failed\n\n" );
          if ( (lcase[anzx->l].dat[i] = (float *)malloc( (anzx->nmax+1) * sizeof(float))) == NULL )
            printf("\n\n ERROR: malloc failed\n\n" );	               
          if ( (lcase[anzx->l].icname[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
             printf("\n\n ERROR: malloc failed\n\n" );
          lcase[anzx->l].max[i]=-MAX_FLOAT;
          lcase[anzx->l].min[i]=MAX_FLOAT;
        }
        if ( (lcase[anzx->l].menu = (int *)malloc( lcase[anzx->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[anzx->l].ictype = (int *)malloc( lcase[anzx->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[anzx->l].icind1 = (int *)malloc( lcase[anzx->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[anzx->l].icind2 = (int *)malloc( lcase[anzx->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[anzx->l].iexist = (int *)malloc( lcase[anzx->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
    
        for(i=0; i<lcase[anzx->l].ncomps; i++)
        {
          sprintf(lcase[anzx->l].compName[i],"%s%d ",resultname, i+1);
          lcase[anzx->l].menu[i] = 1;
          if(ncomps==1)
	  {
            lcase[anzx->l].ictype[i] = 1;
            lcase[anzx->l].icind1[i] = 0;
	  }
          else if(ncomps==3)
          {
            lcase[anzx->l].ictype[i]=2;
            lcase[anzx->l].icind1[i]=i+1;
          }
          lcase[anzx->l].icind2[i] = 0;
          lcase[anzx->l].iexist[i] = 0;
          for(j=0; j<=anzx->nmax; j++) lcase[anzx->l].dat[i][j]=0.;
        }

        while(1)
        {
          length = frecord( handle, rec_str);
          if (rec_str[length] == (char)EOF) break;
      
          /* read "internalField" data */
          if ( strfind(rec_str, "internalField") !=-1 )
          { 
            sscanf(rec_str, "%*s %s", buffer);
            if(compareStrings(buffer,"uniform")>0)
            {
              formFlag=1;
              if     (ncomps==1) sscanf(rec_str, "%*s %*s %lf", &fval[0]); 
              else if(ncomps==3)
              {
                length = strfind(rec_str, "(");
                 sscanf(&rec_str[length+1], "%lf %lf %lf", &fval[0], &fval[1], &fval[2]);
	      }
              else { printf("ERROR1; class not known\n"); exit(1); }
		//printf("%s vals:%lf %lf %lf\n", &rec_str[length], fval[0], fval[1], fval[2]);
	    }
            else
            {
              formFlag=0;
              length = frecord( handle, rec_str);
              anz_dat=atoi(rec_str);
              if(anzx->e!=anz_dat)
              { printf("ERROR: sum-data:%d do not match sum-elem:%d\n",anz_dat,anzx->e); exit(1); }
              length = frecord( handle, rec_str);
	    }
    
            for(i=0; i<anzx->e; i++)
            {
	      if(formFlag==0)
	      {
                length = frecord( handle, rec_str);
                if     (ncomps==1) sscanf(rec_str, "%lf", &fval[0]); 
                else if(ncomps==3)
                {
                  sscanf(&rec_str[1], "%lf %lf %lf", &fval[0], &fval[1], &fval[2]);
	        }
                else { printf("ERROR2; class not known (ncomps:%d)\n",ncomps); exit(1); }
	      }
              if (elem[i].type == 1) ipuf = 8;  /* HEXA8 */
              if (elem[i].type == 2) ipuf = 6;  /* PENTA6 */
              if (elem[i].type == 3) ipuf = 4;  /* TET4 */
              for(n=0; n<lcase[anzx->l].ncomps; n++)
              {
                for(j=0; j<ipuf; j++)
                {
                  lcase[anzx->l].dat[n][elem[i].nod[j]]+=fval[n];
                  if((vpn[0]==0)&&(n==0))
                  {
                    vpn[elem[i].nod[j]]++; /* count the hits */
	          }
                }
              }
            }
	    vpn[0]=1;
          }
	 
          /* read "boundaryField" data */ 
          if ( strfind(rec_str, "boundaryField") !=-1 )
          {
            length = frecord( handle, rec_str);
            while(1)
            {
              length = frecord( handle, rec_str);
              if (rec_str[length] == (char)EOF) break;
            
              length = strfind(rec_str, "{");
              if ( length!=-1 )
              {
                printf("\n   setname:%s\n",  buffer);

                setNr=getSetNrx(buffer);
                if(setNr<0) { printf("ERROR; Set:%s in boundaryField not known\n",buffer); exit(1); }

		while(1)
                {
                  length = frecord( handle, rec_str);
                  if (rec_str[length] == (char)EOF) break;
                  if ( strfind(rec_str, "}")!=-1) break;


                  if ( strfind(rec_str, "fixedValue")!=-1)
                  {
		    do
                    {
                      length = frecord( handle, rec_str); if (rec_str[length] == (char)EOF) break;
                    }while(strfind(rec_str, "value")==-1);

                    /* reset all affected nodes to 0. */
                    for (j=0; j<setx[setNr].anz_f; j++)
                    {
                      f=setx[setNr].face[j];
                      if (face[f].type == 7) ipuf = 3;  /* TRI3 */
                      else if (face[f].type== 8) ipuf = 6;  /* TRI6  */  
                      else if (face[f].type == 9) ipuf = 4;  /* QU4 */
                      else if (face[f].type == 10) ipuf = 8;  /* QU4 */
                      else
                      {
                        printf("\n\n ERROR: face.type:%d not known\n",face[f].type);
                        exit(1);
                      }
                      for(n=0; n<lcase[anzx->l].ncomps; n++)
                      {
                        for(k=0; k<ipuf; k++)
                        {
    	                  lcase[anzx->l].dat[n][face[f].nod[k]]=0.;
                          vpn[face[f].nod[k]]=0;
                        }
                      }

    		    }

                    if ( strfind(rec_str, "nonuniform")!=-1)
                    {
                      i=0;
		      f=-1;
                      do
		      {
  		        pos=0;
                        do
		        {
		          length=get_values_in_string( &rec_str[pos], &fval[0]);
                          if (length<1) break;
                          pos+=length;
			  f=face[setx[setNr].elf[i].e].indx[setx[setNr].elf[i].f]; i++;

                          if (face[f].type == 7) ipuf = 3;  /* TRI3 */
                          else if (face[f].type== 8) ipuf = 6;  /* TRI6  */  
                          else if (face[f].type == 9) ipuf = 4;  /* QU4 */
                          else if (face[f].type == 10) ipuf = 8;  /* QU4 */
                          else
                          {
                            printf("\n\n ERROR: face.type:%d not known\n",face[f].type);
                            exit(1);
                          }
                          for(n=0; n<lcase[anzx->l].ncomps; n++)
                          {
                            for(j=0; j<ipuf; j++)
                            {
                              lcase[anzx->l].dat[n][face[f].nod[j]]+=fval[n];
                              if(n==0) vpn[face[f].nod[j]]++; /* count the hits */
                            }
                          }
#if TEST
			  if(ncomps==1) printf("face:%d e:%d val:%lf \n", f, face[f].elem_nr, fval[0]);
			  if(ncomps==3) printf("face:%d e:%d vals:%lf %lf %lf\n", f, face[f].elem_nr, fval[0], fval[1], fval[2]);
#endif
			}while(1);

                        if((length<0)||(f==-1)) { length = frecord( handle, rec_str); if (rec_str[length] == (char)EOF) break; }
                        else break;
		      }while(1); /* as long as a parenthesis is open */

		    }
                    else if ( strfind(rec_str, "uniform")!=-1)
		    {
                      if     (ncomps==1) sscanf(rec_str, "%*s %*s %lf", &fval[0]); 
                      else if(ncomps==3)
                      {
                        length = strfind(rec_str, "(");
                        sscanf(&rec_str[length+1], "%lf %lf %lf", &fval[0], &fval[1], &fval[2]);
  	              }
                      else { printf("ERROR3; class not known\n"); exit(1); }
#if TEST
  		      if(ncomps==1)  printf("%s vals:%lf\n", &rec_str[length], fval[0]);
  		      if(ncomps==3)  printf("%s vals:%lf %lf %lf\n", &rec_str[length], fval[0], fval[1], fval[2]);
#endif     
                      for(i=0; i<setx[setNr].anz_elf; i++)
                      {
                        f=face[setx[setNr].elf[i].e].indx[setx[setNr].elf[i].f];

                        if (face[f].type == 7) ipuf = 3;  /* TRI3 */
                        else if (face[f].type== 8) ipuf = 6;  /* TRI6  */  
                        else if (face[f].type == 9) ipuf = 4;  /* QU4 */
                        else if (face[f].type == 10) ipuf = 8;  /* QU4 */
                        else
                        {
                          printf("\n\n ERROR: face.type:%d not known\n",face[f].type);
                          exit(1);
                        }
                        for(n=0; n<lcase[anzx->l].ncomps; n++)
                        {
                          for(j=0; j<ipuf; j++)
                          {
			    lcase[anzx->l].dat[n][face[f].nod[j]]+=fval[n];
                            vpn[face[f].nod[j]]++; /* count the hits */
                          }
                        }
    		      }
		    }
		  }


                }
              }
              else sscanf(rec_str, "%s", buffer);
                
            }

          }
        }
 
        /* divide the nodal values by the amount of contributor elements */
        for(i=0; i<anzx->n; i++)
        {
          for(n=0; n<lcase[anzx->l].ncomps; n++)
          {
            lcase[anzx->l].dat[n][node[i].nr]/=vpn[node[i].nr];
    
            if(lcase[anzx->l].dat[n][node[i].nr] > lcase[anzx->l].max[n])
            {
              lcase[anzx->l].max[n]=lcase[anzx->l].dat[n][node[i].nr];
              lcase[anzx->l].nmax[n]=node[i].nr;
            }
            if(lcase[anzx->l].dat[n][node[i].nr] < lcase[anzx->l].min[n])
            {
              lcase[anzx->l].min[n]=lcase[anzx->l].dat[n][node[i].nr];
              lcase[anzx->l].nmin[n]=node[i].nr;
            }
          }
        }

        foundNoEntities:;
        fclose(handle);
        noFile:;
      }
     }
     closedir(dirp);

     /* if data were found calc derived data like Ma etc. (based on pre-defined kappa ) */
     printf("p:%d u:%d t:%d rho:%d\n",ds.p, ds.u, ds.t, ds.rho) ;
     if((ds.p!=-1)&&(ds.u!=-1)&&(ds.t!=-1)&&(ds.rho!=-1))
     {
       anzx->l++;
   
       /* store Datasets */
       if ( (lcase = (Datasets *)realloc(lcase, (anzx->l+1) * sizeof(Datasets))) == NULL )
         printf("\n\n ERROR: malloc failed, lcase\n\n") ;

       lcase[anzx->l].value=lcase[anzx->l-1].value;
       sprintf( lcase[anzx->l].dataset_name,"%s", lcase[anzx->l-1].dataset_name);

       lcase[anzx->l].irtype = 1;
       lcase[anzx->l].npheader = 0;
       strcpy(lcase[anzx->l].analysis_name,"");
       strcpy(lcase[anzx->l].name, "derived");
       strcpy(lcase[anzx->l].dataset_text,"");
       lcase[anzx->l].step_number=lcase[anzx->l-1].step_number;
       lcase[anzx->l].analysis_type=1;

       printf ("lcase.name[%d]= %s value:%f\n", anzx->l, lcase[anzx->l].name, lcase[anzx->l].value);
       lcase[anzx->l].ncomps  = 3;

       if ( (lcase[anzx->l].nmax = (int *)malloc( lcase[anzx->l].ncomps * sizeof(int))) == NULL )
         printf("\n\n ERROR: malloc failure\n\n" );
       if ( (lcase[anzx->l].nmin = (int *)malloc( lcase[anzx->l].ncomps * sizeof(int))) == NULL )
         printf("\n\n ERROR: malloc failure\n\n" );
       if ( (lcase[anzx->l].max = (float *)malloc( lcase[anzx->l].ncomps * sizeof(float))) == NULL )
         printf("\n\n ERROR: malloc failure\n\n" );
       if ( (lcase[anzx->l].min = (float *)malloc( lcase[anzx->l].ncomps * sizeof(float))) == NULL )
         printf("\n\n ERROR: malloc failure\n\n" );
       if ( (lcase[anzx->l].dat = (float **)malloc( lcase[anzx->l].ncomps * sizeof(float *))) == NULL )
         printf("\n\n ERROR: malloc failure\n\n" );
       if ( (lcase[anzx->l].icname = (char **)malloc( lcase[anzx->l].ncomps * sizeof(char *))) == NULL )
         printf("\n\n ERROR: malloc failure\n\n" );
       if ( (lcase[anzx->l].compName = (char **)malloc( lcase[anzx->l].ncomps * sizeof(char *))) == NULL )
         printf("\n\n ERROR: malloc failure\n\n" );

       for(i=0; i<lcase[anzx->l].ncomps; i++)
       {
         if ( (lcase[anzx->l].compName[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
           printf("\n\n ERROR: malloc failed\n\n" );
         if ( (lcase[anzx->l].dat[i]      = (float *)malloc( (anzx->nmax+1) * sizeof(float))) == NULL )
           printf("\n\n ERROR: malloc failed\n\n" );	               
         if ( (lcase[anzx->l].icname[i]   = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
           printf("\n\n ERROR: malloc failed\n\n" );
         lcase[anzx->l].max[i]=-MAX_FLOAT;
         lcase[anzx->l].min[i]=MAX_FLOAT;
       }     
       if ( (lcase[anzx->l].menu = (int *)malloc( lcase[anzx->l].ncomps * sizeof(int))) == NULL )
         printf("\n\n ERROR: malloc failure\n\n" );
       if ( (lcase[anzx->l].ictype = (int *)malloc(lcase[anzx->l].ncomps * sizeof(int))) == NULL )
         printf("\n\n ERROR: malloc failure\n\n" );
       if ( (lcase[anzx->l].icind1 = (int *)malloc( lcase[anzx->l].ncomps * sizeof(int))) == NULL )
         printf("\n\n ERROR: malloc failure\n\n" );
       if ( (lcase[anzx->l].icind2 = (int *)malloc( lcase[anzx->l].ncomps * sizeof(int))) == NULL )
         printf("\n\n ERROR: malloc failure\n\n" );
       if ( (lcase[anzx->l].iexist = (int *)malloc( lcase[anzx->l].ncomps * sizeof(int))) == NULL )
         printf("\n\n ERROR: malloc failure\n\n" );
       
       for(i=0; i<lcase[anzx->l].ncomps; i++)
       {
         lcase[anzx->l].menu[i] = 1;
         lcase[anzx->l].ictype[i] = 1;
         lcase[anzx->l].icind1[i] = 0;
         lcase[anzx->l].icind2[i] = 0;
         lcase[anzx->l].iexist[i] = 0;
       }

       sprintf(lcase[anzx->l].compName[0],"TT ");
       sprintf(lcase[anzx->l].compName[1],"PT ");
       sprintf(lcase[anzx->l].compName[2],"Ma ");

       for(i=0; i<anzx->n; i++)
       {
         ps=lcase[ds.p].dat[0][node[i].nr];
         us=sqrt(lcase[ds.u].dat[0][node[i].nr]*lcase[ds.u].dat[0][node[i].nr]+lcase[ds.u].dat[1][node[i].nr]*lcase[ds.u].dat[1][node[i].nr]+lcase[ds.u].dat[2][node[i].nr]*lcase[ds.u].dat[2][node[i].nr]);
         ts=lcase[ds.t].dat[0][node[i].nr];
         rho=lcase[ds.rho].dat[0][node[i].nr];
         Rg=ps/rho/ts;
         cp=Rg/(1-1/kappa);
	 //printf("R:%f cp:%f\n", Rg, cp);
         tt=us*us*.5/cp + ts;
	 pt = ps/pow( (ts/tt), (kappa/(kappa-1)) );
         ma=us/sqrt(kappa*ps/rho);
         lcase[anzx->l].dat[0][node[i].nr]=tt;
         lcase[anzx->l].dat[1][node[i].nr]=pt;
         lcase[anzx->l].dat[2][node[i].nr]=ma;

         for(j=0; j<lcase[anzx->l].ncomps; j++)
	 {
           if(lcase[anzx->l].dat[j][node[i].nr] > lcase[anzx->l].max[j])
           {
             lcase[anzx->l].max[j]=lcase[anzx->l].dat[j][node[i].nr];
             lcase[anzx->l].nmax[j]=node[i].nr;
           }
           if(lcase[anzx->l].dat[j][node[i].nr] < lcase[anzx->l].min[j])
           {
             lcase[anzx->l].min[j]=lcase[anzx->l].dat[j][node[i].nr];
             lcase[anzx->l].nmin[j]=node[i].nr;
           }
         }
       }
     }

    }
  }
  free(vpn);
  anzx->l++;
  elemChecker( anzx->e, node, elem);


  /* free the temporary faces */
  free(face);
  anzx->f=0;

  /* set all .loaded to 1 to indicate that the data are available */
  for (i=0; i<anzx->l; i++) { lcase[i].fileptr=NULL; lcase[i].loaded=1; }

  /* delete all faces in sets */
  for(i=0; i<anzx->sets; i++) setx[i].anz_f=0;

  *sptr=setx; *nptr =  node; *eptr = elem; *lptr = lcase;
  return (1);
}
