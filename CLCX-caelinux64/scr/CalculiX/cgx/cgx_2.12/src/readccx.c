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
#include <ctype.h>
#include  <time.h>


#define ANSYS 0
#define TEST  0

#define INI_FIELD_SIZE 1000000
#define DAT_SIZE       80

/* duplication of equations for segments of a cycsym-calc */
#define NODE_OFFSET 0 /* 107064 */
#define SEGMENTS 14

#define     BOU         "+bou"
#define     DEP         "+dep"
#define     IND         "+ind"
#define     MPC         "+mpc"
#define     CLO         "+clo"
#define     DLO         "+dlo"
#define     RAD         "+rad"
#define     FLM         "+flm"
#define     CFLX        "+cflx"
#define     DFLX        "+dflx"
#define     MFLW        "+mflw"
#define     CONT1        "+cont1"
#define     CONT2        "+cont2"
#define     SPCF        "+spcf"
#define     TIE1        "+tie1"
#define     TIE2        "+tie2"
#define     TIECYC1        "+tiecyc1"
#define     TIECYC2        "+tiecyc2"
#define     TIEMUL1        "+tiemult1"
#define     TIEMUL2        "+tiemult2"

#define     DOFT        11
#define     DOFP        8

char ansys_dof[6][5]={"UX","UY","UZ","ROTX","ROTY","ROTZ"}; /* SHORT-CUTS FOR ANSYS DOFS */
char ansys_frc[6][5]={"FX","FY","FZ"}; /* SHORT-CUTS FOR ANSYS FORCE */

FILE *handlein;
FILE *handleans;

int fileOpenError=0;

typedef struct {
    int   nset;
    double d;
}Elprop;

Elprop    elprop[1];
int      *selected_elems=NULL;
int       filesopen=0;
FILE     *openfile[100]; 


int      matnr=0, ansFlag=0;
char     **dat=NULL;

Sets      *setx=NULL;
Elements  *elemx=NULL;
Nodes     *nodex=NULL;
Datasets  *lcasex=NULL;
Summen    *anzx=NULL;
int       anz_fb=-1;

extern int MAKE_SETS_DEF;
extern char buffer[MAX_LINE_LENGTH];
extern Alias     *alias;
extern Materials *material; 
extern Amplitudes *amplitude; 
extern SumGeo    anzGeo[1];
extern SumAsci   sumAsci[1];
extern char printFlag;
Elements  *e_enqirex=NULL;     /* feld in dem e_enqire[elem[i].nr].xx abgelegt ist, statt elem[i].xx */

char dataset_text[MAX_LINE_LENGTH];
double dataset_value=0;
int copyFlag=0, step_nr=0;
FILE *handle_equ=NULL;
int  node_field_size=0, elem_field_size=0, elem_field_size2=0;
int  dlo_field_size=1, film_field_size=1, rad_field_size=1, amp_field_size=1, mat_field_size=1;
int step_flag=0;
clock_t t1, t2 ;
int resetFlag=0;

void annotation( void )
{
  printf("******************************************************************************\n");
  printf("*  read ccx input                                   4.4.2002 wi              *\n");
  printf("*                                                                            *\n");
  printf("*                                                                            *\n");
  printf("* -only free-format is supported. fixed format might cause a prg-crash.      *\n");
  printf("*                                                                            *\n");
  printf("*                                                                            *\n");
  printf("******************************************************************************\n");
}

/* liest einen Record bis '\n'; uebergibt Anzahl gelesene Zeichen */
int arecord( FILE *handle1,  char *string)
{
  register int i, n, c;

  for (i=0; i<MAX_LINE_LENGTH-1; i++)
  {
      do{ string[i] = getc(handle1); }while((string[i]==' ')||(string[i]=='\t'));
    if (string[i] == '\n')
      {
      for (n=i+1; n<MAX_LINE_LENGTH; ++n) string[n] = '\0';
      return(i);
      }
    if (string[i] == '\r')
      {
      c = getc(handle1);
      if (c != '\n')
        ungetc(c, handle1);

      for (n=i+1; n<MAX_LINE_LENGTH; ++n) string[n] = '\0';
      return(i);
     }
    else if (string[i] == (char)EOF)
      {
      for (n=i+1; n<MAX_LINE_LENGTH; ++n) string[n] = '\0';
      return(i);
      }
  }
  string[MAX_LINE_LENGTH-1] = '\0';
  return(MAX_LINE_LENGTH-1);
}



/* return:           */
/* 1: regular string */
/* -2:  end of data block     */
/* -1:  all files closed      */ 
int abqrecord( char *string)
{
  register int l;

 getNextLine:;

  /* check for comments */
  do{ l=arecord( handlein, string); } while(((string[0]=='*')&&(string[1]=='*'))||(string[0]=='\n')||(string[0]=='\r'));

  /* check for include */
  if(string[0]=='*') 
  {
    if ((compare(string, "*INCLUDE",5)==5)||(compare(string, "*include",5)==5)) 
    {
     /* change the filepointer */
     filesopen=getFilePointer( filesopen, openfile, string);
     goto getNextLine;
    }
    else return(-2);
  }

  if (string[l] == (char)EOF)
  {
    /* are there files open? */
    if(filesopen>1)
    {
        fclose(handlein);
	filesopen--;
	handlein=openfile[filesopen-1];
        if(printFlag) printf("open file again:%d\n",filesopen);
    goto getNextLine;
    }
    else return(-1);
  }
  return(l);
}



int __crecord( char *rec_str, char **dat)
{
  int i,j;
  int nextarg=0, letter=0;

  /* scan all args divided by comma */
  nextarg=0;letter=0;
  for(j=0; j<MAX_LINE_LENGTH; j++) dat[nextarg][j]='\0'; 
  for(i=0; i<MAX_LINE_LENGTH; i++)
  {
    if(rec_str[i]==(char)EOF) {  break; } 
    if(rec_str[i]=='\n') {  break; } 
    if(rec_str[i]==0) {  break; } 
    if(rec_str[i]==',') 
    {
      nextarg++;
      letter=0;
      for(j=0; j<MAX_LINE_LENGTH; j++) dat[nextarg][j]='\0'; 
    }
    else
    {
      if((rec_str[i]>32)&& (rec_str[i]<127))
      {
        dat[nextarg][letter]=rec_str[i];
        letter++;
      }
    }
  }
  //if(nextarg>=80) { printf("ERROR in crecord: increase dat\n"); exit(0); }
  if(dat[nextarg][0]=='\0') return(nextarg);
  return(nextarg+1);
}





int crecord( char *rec_str, char **dat)
{
  register int i;
  int nextarg=0, letter=0;

  /* scan all args divided by comma */
  nextarg=0;letter=0;
  //for(j=0; j<MAX_LINE_LENGTH; j++) dat[nextarg][j]='\0'; 
  for(i=0; i<MAX_LINE_LENGTH; i++)
  {
    if(rec_str[i]==(char)EOF) {dat[nextarg][letter]='\0';  break; } 
    if(rec_str[i]=='\n') {dat[nextarg][letter]='\0';  break; } 
    if(rec_str[i]==0) {dat[nextarg][letter]='\0';  break; } 
    if(rec_str[i]==',') 
    {
      dat[nextarg][letter]='\0';
      nextarg++;
      letter=0;
      //for(j=0; j<MAX_LINE_LENGTH; j++) dat[nextarg][j]='\0'; 
    }
    else
    {
      if((rec_str[i]>32)&& (rec_str[i]<127))
      {
        dat[nextarg][letter]=rec_str[i];
        letter++;
      }
    }
  }
  if(nextarg>=DAT_SIZE) { printf("ERROR in crecord: increase DAT_SIZE\n"); exit(0); }
  if(dat[nextarg][0]=='\0') return(nextarg);
  return(nextarg+1);
}


/*------------------------------------------------------------------*/
/* entity einem Set zuordnen                                        */
/*------------------------------------------------------------------*/


/* returns set-Index if known, or -1  */
int getSetNrx(char *name)
{
  int  i;
  int length, pfree=-1;

  if(!anzx->sets) return(-1);
  if(name== (char *)NULL) return(-1);
  length=strlen(name);

  for (i=0; i<anzx->sets; i++)
  {
    if (setx[i].name!= (char *)NULL)
    {
      if ((compare( setx[i].name, name, length)==length) && (sword( setx[i].name, buffer)==length))
      {
        return(i);
      }
    }
  }
  return(pfree);
}




void delSetx( char *setname)
{
  int i, setNr;

  setNr=getSetNrx(setname);

  if (setNr<0)
  {
    if(printFlag) printf (" delSet: set:%s does not exist\n", setname);
    return;
  }

  /* der set wird wieder frei gegeben */
  if(setx[setNr].anz_elf)
    for(i=0; i<setx[setNr].anz_elf; i++)
      if(setx[setNr].elf[i].n) free(setx[setNr].elf[i].v);  

    free(setx[setNr].name);
    free(setx[setNr].valu);
    free(setx[setNr].node);
    free(setx[setNr].elem);
    free(setx[setNr].face);
    free(setx[setNr].elf);
    free(setx[setNr].pnt);
    free(setx[setNr].line);
    free(setx[setNr].lcmb);
    free(setx[setNr].surf);
    free(setx[setNr].body);
    free(setx[setNr].nurl);
    free(setx[setNr].nurs);
    free(setx[setNr].set);
    free(setx[setNr].shp);
    free(setx[setNr].eparm);
    setx[setNr].name=(char *)NULL;
    setx[setNr].valu=(int *)NULL;
    setx[setNr].node=(int *)NULL;
    setx[setNr].elem=(int *)NULL;
    setx[setNr].face=(int *)NULL;
    setx[setNr].elf=(Elfaces *)NULL;
    setx[setNr].pnt= (int *)NULL;
    setx[setNr].line=(int *)NULL;
    setx[setNr].lcmb=(int *)NULL;
    setx[setNr].surf=(int *)NULL;
    setx[setNr].body=(int *)NULL;
    setx[setNr].nurl=(int *)NULL;
    setx[setNr].nurs=(int *)NULL;
    setx[setNr].set=(int *)NULL;
    setx[setNr].shp=(int *)NULL;
    setx[setNr].eparm=(char *)NULL;
    setx[setNr].material = -1;
    setx[setNr].flag = 'c';
    setx[setNr].type = 0;
    setx[setNr].anz_v = 0;
    setx[setNr].anz_n = 0;
    setx[setNr].anz_e = 0;
    setx[setNr].anz_f = 0;
    setx[setNr].anz_elf = 0;
    setx[setNr].anz_p = 0;
    setx[setNr].anz_l = 0;
    setx[setNr].anz_c = 0;
    setx[setNr].anz_s = 0;
    setx[setNr].anz_b = 0;
    setx[setNr].anz_nurl = 0;
    setx[setNr].anz_nurs = 0;
    setx[setNr].anz_se = 0;
    setx[setNr].anz_sh = 0;
}


int setax( int setNr, char *type, int number)
{
  if( setx[setNr].name == (char *)NULL )
  {
    errMsg(" ERROR: setNr:%d is undefined\n", setNr);
    return(-1);
  }

  /* check if item is known and if its already member of the set */
  if ( type[0] == 'r' )
  {
    if (number<0)
    { if(printFlag) printf(" set:%d does not exist\n", number ); return(-1); }
    setx[setNr].anz_se= iinsert(&setx[setNr].set, setx[setNr].anz_se, number);
    setx[number].anz_se= iinsert(&setx[number].set, setx[number].anz_se, setNr);
  }
  else if (( type[0] == 'h' )||(( type[0] == 's' )&&( type[1] == 'h' )))
  {
    if (number<0)
    { if(printFlag) printf(" shape:%d does not exist\n", number ); return(-1); }
    setx[setNr].anz_sh= iinsert(&setx[setNr].shp, setx[setNr].anz_sh, number);
  }
  else if ( type[0] == 'n' )
  {
    setx[setNr].anz_n= iinsert(&setx[setNr].node, setx[setNr].anz_n, number);
  }
  else if ( type[0] == 'e' )
  {
    setx[setNr].anz_e=iinsert(&setx[setNr].elem, setx[setNr].anz_e, number);
  }
  else if ( type[0] == 'f' )
  {
    setx[setNr].anz_f=iinsert(&setx[setNr].face, setx[setNr].anz_f, number);
  }
  else if ( type[0] == 'j' )
  {
    if((setx[setNr].elf= (Elfaces *)realloc(setx[setNr].elf, (setx[setNr].anz_elf+1)*sizeof(Elfaces))) == NULL )
    { printf("ERROR: realloc failed in seta()\n\n" ); return(-1); }
    if(number)
    {
      setx[setNr].elf[setx[setNr].anz_elf].n=number;
      if((setx[setNr].elf[setx[setNr].anz_elf].v= (float *)calloc(number, sizeof(float))) == NULL )
      { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    }
    else setx[setNr].elf[setx[setNr].anz_elf].n=0;
    setx[setNr].anz_elf++;
    return(setx[setNr].anz_elf-1);
  }
  else
  {
    errMsg ("WARNING: in seta type:%s not recognized\n", type);
    return(-1);
  }
  return(1);
}



int pre_setax( char *string, char *type, char *name)
{
  int i, setNr;
  int n=0;
  int number;
  char setname[MAX_LINE_LENGTH]; /* string is not changeable. Therefore a new char is necessary */

  /* remove blanks and other illegal chars*/
  for(i=0;i<strlen(string); i++) if(string[i]>(char)0x20) { setname[n]=string[i]; n++; }
  if(!n) return(-1);
  setname[n]='\0';

  operateAlias( setname, "se" );
  setNr=getSetNrx(setname);

  number=0;

  if (setNr==-1)
  {
    if ((setx = (Sets *)realloc( (Sets *)setx, (anzx->sets+2)*sizeof(Sets)) ) == NULL )
    {
      printf(" ERROR: realloc failure in pre_setax, set:%s not installed\n\n", setname);
      return(-1);
    }
    setNr= anzx->sets;
    anzx->sets++;

    i=strlen(setname);
    if((setx[setNr].name= (char *)malloc((i+1)*sizeof(char))) == NULL )
    { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    strcpy( setx[setNr].name, setname);
    if(printFlag) printf (" create set:%s\n", setx[setNr].name);
    setx[setNr].flag='c';
    if ( type[1] == 's' ) setx[setNr].type=1;
    else                  setx[setNr].type=0;
    setx[setNr].etyp=0;
    if((setx[setNr].valu= (int *)malloc(sizeof(int))) == NULL )
    { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    if((setx[setNr].node= (int *)malloc(sizeof(int))) == NULL )
    { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    if((setx[setNr].elem= (int *)malloc(sizeof(int))) == NULL )
    { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    if((setx[setNr].face= (int *)malloc(sizeof(int))) == NULL )
    { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    if((setx[setNr].elf= (Elfaces *)malloc(sizeof(Elfaces))) == NULL )
    { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    if((setx[setNr].pnt= (int *)malloc(sizeof(int))) == NULL )
    { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    if((setx[setNr].line= (int *)malloc(sizeof(int))) == NULL )
    { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    if((setx[setNr].lcmb= (int *)malloc(sizeof(int))) == NULL )
    { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    if((setx[setNr].surf= (int *)malloc(sizeof(int))) == NULL )
    { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    if((setx[setNr].body= (int *)malloc(sizeof(int))) == NULL )
    { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    if((setx[setNr].nurl= (int *)malloc(sizeof(int))) == NULL )
    { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    if((setx[setNr].nurs= (int *)malloc(sizeof(int))) == NULL )
    { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    if((setx[setNr].set= (int *)malloc(sizeof(int))) == NULL )
    { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    if((setx[setNr].shp= (int *)malloc(sizeof(int))) == NULL )
    { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    setx[setNr].index = -1;
    setx[setNr].material = -1;
    setx[setNr].anz_v = 0;
    setx[setNr].anz_n = 0;
    setx[setNr].anz_e = 0;
    setx[setNr].anz_f = 0;
    setx[setNr].anz_elf = 0;
    setx[setNr].anz_p = 0;
    setx[setNr].anz_l = 0;
    setx[setNr].anz_c = 0;
    setx[setNr].anz_s = 0;
    setx[setNr].anz_b = 0;
    setx[setNr].anz_nurl = 0;
    setx[setNr].anz_nurs = 0;
    setx[setNr].anz_se = 0;
    setx[setNr].anz_sh = 0;
    setx[setNr].eparm=(char *)NULL;
  }
  else if (setNr<-1)    /* replace a deleted set */
  {
    setNr=-(setNr+10);
    i=strlen(setname);
    if((setx[setNr].name= (char *)malloc((i+1)*sizeof(char))) == NULL )
    { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    strcpy( setx[setNr].name, setname);
    if ( type[1] == 's' ) setx[setNr].type=1;
    else                  setx[setNr].type=0;
    if((setx[setNr].valu= (int *)malloc(sizeof(int))) == NULL )
    { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    if((setx[setNr].node= (int *)malloc(sizeof(int))) == NULL )
    { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    if((setx[setNr].elem= (int *)malloc(sizeof(int))) == NULL )
    { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    if((setx[setNr].face= (int *)malloc(sizeof(int))) == NULL )
    { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    if((setx[setNr].elf= (Elfaces *)malloc(sizeof(Elfaces))) == NULL )
    { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    if((setx[setNr].pnt= (int *)malloc(sizeof(int))) == NULL )
    { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    if((setx[setNr].line= (int *)malloc(sizeof(int))) == NULL )
    { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    if((setx[setNr].lcmb= (int *)malloc(sizeof(int))) == NULL )
    { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    if((setx[setNr].surf= (int *)malloc(sizeof(int))) == NULL )
    { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    if((setx[setNr].body= (int *)malloc(sizeof(int))) == NULL )
    { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    if((setx[setNr].nurl= (int *)malloc(sizeof(int))) == NULL )
    { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    if((setx[setNr].nurs= (int *)malloc(sizeof(int))) == NULL )
    { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    if((setx[setNr].set= (int *)malloc(sizeof(int))) == NULL )
    { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    if((setx[setNr].shp= (int *)malloc(sizeof(int))) == NULL )
    { printf("ERROR: malloc failed in seta()\n\n" ); return(-1); }
    setx[setNr].index = -1;
    setx[setNr].material = -1;
    setx[setNr].anz_v = 0;
    setx[setNr].anz_n = 0;
    setx[setNr].anz_e = 0;
    setx[setNr].anz_f = 0;
    setx[setNr].anz_elf = 0;
    setx[setNr].anz_p = 0;
    setx[setNr].anz_l = 0;
    setx[setNr].anz_c = 0;
    setx[setNr].anz_s = 0;
    setx[setNr].anz_b = 0;
    setx[setNr].anz_nurl = 0;
    setx[setNr].anz_nurs = 0;
    setx[setNr].anz_se = 0;
    setx[setNr].anz_sh = 0;
    setx[setNr].eparm=(char *)NULL;
  }

  /* determine the index of the entity */
  if (( type[0] == 's' )&&( type[1] == 'e' ))  number=getSetNrx(name);
  else if (( type[0] == 's' )&&( type[1] == 'h' ))  number=getShapeNr(name);
  else if ( type[0] == 'n' )                   number=atoi(name);
  else if ( type[0] == 'e' )                   number=atoi(name);
  else if ( type[0] == 'f' )                   number=atoi(name);
  else if ( type[0] == 'i' ) 
  {
    if(printFlag) printf (" set initialized\n");
    return(setNr);
  }
  else
  {
    errMsg ("ERROR: in seta type:%s not recognized\n", type);
    return(-1);
  }

  /* add to the set */
  if( setax(setNr, type, number)<0 )
    return(-1);
  else
    return(setNr);
}


int writeNodeVals(int anzx_l, int nd, double f, int dof)
{
  int i;
  if (nd>anzx->nmax)
  {
    printf(" WARNING: found nodeNr:%d in Dataset higher than in Geometry allocated:%d\n"
    , nd, anzx->nmax);
    // exit(-1);
    return(0);
  }
  lcasex[anzx_l].dat[dof][nd]       = f;
  
  /* max and min */
  for(i=0; i<lcasex[anzx_l].ncomps; i++)
  {
  if(lcasex[anzx_l].dat[i][nd] > lcasex[anzx_l].max[i])
  {
    lcasex[anzx_l].max[i]=lcasex[anzx_l].dat[i][nd];
    lcasex[anzx_l].nmax[i]=nd;
  }
  if(lcasex[anzx_l].dat[i][nd] < lcasex[anzx_l].min[i])
  {
    lcasex[anzx_l].min[i]=lcasex[anzx_l].dat[i][nd];
    lcasex[anzx_l].nmin[i]=nd;
  }
  }
  return(1);
}



int getBoundaries(char *rec_str)
{
  int i,j,n;
  int args, nset;
  int ival[80], dof;
  double f;
  static int setNr[7]={0,0,0,0,0,0,0};
  static int local_step_nr=0, anzx_l;

  /* reset all static variables */
  if(resetFlag)
  {
    for(i=0; i<7; i++) setNr[i]=0;
    local_step_nr=0;
    anzx_l=0;
    return(0);
  }

  if(printFlag) printf("%s\n",rec_str);
  /* scan the properties and write to file */

  /* create a dataset if a new step beginns */
  if(step_nr>local_step_nr)
  {
    local_step_nr=step_nr;
    /* ini lcasex */
    sprintf( lcasex[anzx->l].name,"%s", BOU);
    printf (" create dataset:%d name= %s\n", anzx->l, lcasex[anzx->l].name);
    lcasex[anzx->l].value=0;
    lcasex[anzx->l].ncomps=7;
    lcasex[anzx->l].irtype=1;
    lcasex[anzx->l].npheader=0;
  
    strcpy(lcasex[anzx->l].analysis_name,"");
    sprintf(lcasex[anzx->l].dataset_name,"STP %d", step_nr);
    strcpy(lcasex[anzx->l].dataset_text,"");
    lcasex[anzx->l].step_number=step_nr;
    lcasex[anzx->l].analysis_type=1;
  
    if ( (lcasex[anzx->l].nmax = (int *)malloc( lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].nmin = (int *)malloc( lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].max = (float *)malloc(lcasex[anzx->l].ncomps * sizeof(float))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].min = (float *)malloc(lcasex[anzx->l].ncomps * sizeof(float))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].dat = (float **)malloc(lcasex[anzx->l].ncomps * sizeof(float *))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].compName = (char **)malloc(lcasex[anzx->l].ncomps * sizeof(char *))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].icname = (char **)malloc(lcasex[anzx->l].ncomps * sizeof(char *))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].menu = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].ictype = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].icind1 = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].icind2 = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].iexist = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
  
    for(i=0; i<lcasex[anzx->l].ncomps; i++)
    {
      if ( (lcasex[anzx->l].dat[i] = (float *)calloc( (anzx->nmax+1), sizeof(float))) == NULL )
        printf("\n\n ERROR: malloc failure\n\n" );
      if ( (lcasex[anzx->l].compName[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
        printf("\n\n ERROR: malloc failed\n\n" );
      if ( (lcasex[anzx->l].icname[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
        printf("\n\n ERROR: malloc failed\n\n" );
      lcasex[anzx->l].max[i]=-MAX_FLOAT;
      lcasex[anzx->l].min[i]=MAX_FLOAT;
  
      lcasex[anzx->l].menu[i] = 1;
      lcasex[anzx->l].ictype[i] = 1;
      lcasex[anzx->l].icind1[i] = i+1;
      lcasex[anzx->l].icind2[i] = 0;
      lcasex[anzx->l].iexist[i] = 0;
      if(i==0) sprintf( lcasex[anzx->l].compName[i], "bou11");
      else sprintf( lcasex[anzx->l].compName[i], "bou%d",i);
    }
    
    anzx_l=anzx->l;

    anzx->l++;
    if ( (lcasex = (Datasets *)realloc((Datasets *)lcasex, (anzx->l+2) * sizeof(Datasets))) == NULL )
    { printf("\n\n ERROR: malloc failure\n\n" ); exit(1); }
  }  

  do{
    i=abqrecord(rec_str);
    if(i==-1) return(-1);
    if(i==-2) break;

    /* get the arguments of the string */
    args=crecord(rec_str, dat);

    /* screen dump */
    if(printFlag)
    {
      for(i=0; i<args; i++) printf("%s ",dat[i]);
      printf(" (%d)\n",i);
    }

    /* extract node, dofs, disp */
    for(i=0; i<80; i++) ival[i]=0;
    for(i=0; i<args; i++) ival[i]=atoi(dat[i]);
    if(args>3) f=atof(dat[3]); else f=0.;

    /* write to file */
    if(ival[2]<ival[1]) ival[2]=ival[1];

    for(i=ival[1]; i<=ival[2]; i++)
    {
      /* if the nodes are stored in a set */
      if(ival[0]<1)
      {
        nset=getSetNrx(dat[0]);
        if(nset<0)
        {
          printf("ERROR: In *BOUNDARY set:%s not defined\n",dat[0]);
          return(-1);
        }
        for(j=0; j<setx[nset].anz_n; j++)
        {
          if(!setNr[0])
          {
            setNr[0]=pre_setax( BOU, "i", 0);
          }
          if(!setNr[i])
          {
            sprintf(buffer, "%s%d", BOU,i);
            setNr[i]=pre_setax( buffer, "i", 0);
          }
          setax(setNr[0], "n", setx[nset].node[j]);
          setax(setNr[i], "n", setx[nset].node[j]);
          if(step_nr>0)
	  {
            dof=-1;
            if(i<7) dof=i;
            else if(i==11) dof=0;
	    if(dof>-1) writeNodeVals(anzx_l, setx[nset].node[j], lcasex[anzx_l].dat[dof][setx[nset].node[j]]+f, dof);
	  }
          if(ansFlag)
          {
            /* write in ansys-format */
            if ((i<1)||(i>6)) errMsg ("ERROR dof:%d not known\n", i);
            else fprintf(handleans, "D,%d, %s, 0.  \n", setx[nset].node[j], ansys_dof[i-1]);
          }
        }
      }
      else
      {
        if(!setNr[0])
        {
          setNr[0]=pre_setax( BOU, "i", 0);
        }
        if(!setNr[i])
        {
          sprintf(buffer, "%s%d", BOU,i);
          setNr[i]=pre_setax( buffer, "i", 0);
        }
        setax(setNr[0], "n", ival[0]);
        setax(setNr[i], "n", ival[0]);
        if(step_nr>0)
	{
          dof=-1;
          if(i<7) dof=i;
          else if(i==11) dof=0;
	  if(dof>-1) writeNodeVals(anzx_l, ival[0], lcasex[anzx_l].dat[dof][ival[0]]+f, dof);
	}
        if(copyFlag)
        {
          /* write to equ-file */
	  for (n=0; n<SEGMENTS; n++) {printf( "%d,%d, ,\n", ival[0]+(n*NODE_OFFSET), i);
            fprintf(handle_equ, "%d,%d, ,\n", ival[0]+(n*NODE_OFFSET), i); }
        }
        if(ansFlag)
        {
          /* write in ansys-format */
          if ((i<1)||(i>6)) errMsg ("ERROR dof:%d not known\n", i);
          else fprintf(handleans, "D,%d, %s, 0.  \n", ival[0], ansys_dof[i-1]);
        }
      }
    }
  }while(1);     
  return(1);
}


int getEquations(char *rec_str)
{
  int i,ii,n, neq;
  int args;
  int   nd[80], dof[80];
  double val[80];
  static int neqn=0;
  static int setDep[80], setInd[80];
  int transdep=-1, transind=-1;
  static int elem=1;

  /* reset all static variables */
  if(resetFlag)
  {
    neqn=0;
    for(i=0; i<80; i++) setDep[i]=setInd[i]=0;
    return(0);
  }

  if(printFlag) printf("*EQUATION:\n");
  do
  {
    i=abqrecord(rec_str);
    if(i==-1) return(-1);
    if(i==-2) break;

    /* get the number of terms */
    neq=atoi(rec_str);
    n=0;
    do
    {
      /* get the terms */
      i=abqrecord(rec_str);
      if(i==-1) return(-1);
      if(i==-2) break;

      args=crecord(rec_str, dat);

      /* ignore trailing comma */
      args=(int)(args/3)*3;  
      /* extract node, dof, weight of each term */
      for(i=0; i<args; i+=3)
      {
        if(printFlag) printf("eq:%d %s %s %s\n", n, dat[i],dat[i+1],dat[i+2]);
        nd[n]  = atoi(dat[i]);
        dof[n] = atoi(dat[i+1]);
        val[n] = atof(dat[i+2]);
        n++;
      }
    }while(n<neq);

    /* write sets */
    if(!setDep[0]) { setDep[0]=pre_setax( DEP, "i", 0);  }
    if(!setInd[0]) { setInd[0]=pre_setax( IND, "i", 0);  }

    for(i=0; i<neq; i++)
    {
      if(!i)
      {
        if(!setDep[dof[0]])
        {
          sprintf(buffer, "%s%d", DEP,dof[0]);
          setDep[dof[0]]=pre_setax( buffer, "i", 0);
        }
        setax(setDep[0], "n", nd[0]);
        setax(setDep[dof[0]], "n", nd[0]);

        /* look if the nodes belong to a certain transformation */
        for(ii=anzx->sets-1; ii>-1; ii--)
        {
          if((setx[ii].name!=NULL)&&(compare(setx[ii].name, "+trans",6)==6))
          {
            if( getIndex(&setx[ii].node,setx[ii].anz_n,nd[0]) >-1)
            {
  	    //printf("n:%d\n", nd[0]);
              transdep=ii;
              break;
            }
          }
        }
      }
      else
      {
        /* look if the node belongs to the same transformation as the dep-node */
        for(ii=anzx->sets-1; ii>-1; ii--)
        {
          if((setx[ii].name!=NULL)&&(compare(setx[ii].name, "+trans",6)==6))
          {
            if( getIndex(&setx[ii].node,setx[ii].anz_n,nd[i]) >-1)
            {
  	    //printf("n:%d\n", nd[i]);
              transind=ii;
              break;
            }
          }
        }
        if(transdep!=transind)
	{
           printf(" Warning: dep-node %d", nd[0]);
	   if(transdep>-1)  printf(" belongs to %s", setx[transdep].name);
	   else  printf(" belongs to basic system");
           printf(" were ind-node %d", nd[i]);
	   if(transind>-1)  printf(" belongs to %s\n", setx[transind].name);
	   else  printf(" belongs to basic system\n");
	}
        if(!setInd[dof[i]])
        {
          sprintf(buffer, "%s%d", IND,dof[i]);
          setInd[dof[i]]=pre_setax( buffer, "i", 0);
        }
        setax(setInd[0], "n", nd[i]);
        setax(setInd[dof[i]], "n", nd[i]);
        if(printFlag) printf("elem %d %d %d be2\n", elem++, nd[0], nd[i] );
      }
    }

    if(copyFlag)
    {
      /* write to equ-file */
      for (n=0; n<SEGMENTS; n++)
      {
        fprintf(handle_equ, "*EQUATION\n");
        fprintf(handle_equ, "%d\n", neq);
        for (i=0;i<neq;i++)
          fprintf(handle_equ, "%d,%d,%.12lf\n", nd[i]+(n*NODE_OFFSET), dof[i], val[i]);
      }
    }
    if(ansFlag)
    {
      /* write to ans-file */
      neqn++;
      for (i=0;i<neq;i++)
        fprintf(handleans, "CE,%d,0,%d,%s,%8.5f\n", neqn, nd[i], ansys_dof[dof[i]-1], val[i]);
    }
  }while(1);     
  return(1);
}


int getEquationf(char *rec_str)
{
  int i,j,n;
  int args, neq;
  int   el[80],face[80], dof[80];
  double val[80];
  static int setDep[80], setInd[80];

  /* reset all static variables */
  if(resetFlag)
  {
    for(i=0; i<80; i++) setDep[i]=setInd[i]=0;
    return(0);
  }

  if(printFlag) printf("*EQUATIONF:\n");
  do
  {
    i=abqrecord(rec_str);
    if(i==-1) return(-1);
    if(i==-2) break;

    /* get the number of terms */
    neq=atoi(rec_str);
    n=0;
    do
    {
      /* get the terms */
      i=abqrecord(rec_str);
      if(i==-1) return(-1);
      if(i==-2) break;

      args=crecord(rec_str, dat);

      /* ignore trailing comma */
      args=(int)(args/4)*4;  
      /* extract elem,face, dof, weight of each term */
      for(i=0; i<args; i+=4)
      {
        if(printFlag)
          printf("eq:%d %s %s %s %s\n", n, dat[i],dat[i+1],dat[i+2],dat[i+3]);
        el[n]  = atoi(dat[i]);
        face[n]  = (int)atoi(&dat[i+1][1])-1;
        dof[n] = atoi(dat[i+2]);
        val[n] = atof(dat[i+3]);
        if(printFlag)
          printf(" %d %d %d %f\n",el[n],face[n],dof[n],val[n] );
        n++;
      }
    }while(n<neq);

    /* write sets */
    if(!setDep[0]) { setDep[0]=pre_setax( DEP, "i", 0);  }
    if(!setInd[0]) { setInd[0]=pre_setax( IND, "i", 0);  }

    for(i=0; i<neq; i++)
    {
      if((e_enqirex[el[i]].type>6)&&(e_enqirex[el[i]].type<11))
      {
        if(e_enqirex[el[i]].attr>3)
        {
          if(e_enqirex[el[i]].type<9)
          {
            if(face[i]<0) { if(dat[1][1]=='N') face[i]=4; else face[i]=5; }
            else face[i]+=1;
          }
          else
          {
            if(face[i]<0) { if(dat[1][1]=='N') face[i]=5; else face[i]=6; }
            else face[i]+=1;
          }
        }
        else
        {
          if(face[i]>1) face[i]+=1; else { if(dat[1][1]=='N') face[i]=0; else face[i]=1; }
        }
      }

      if(printFlag)
        printf(" %d %d %d %f\n",el[i],face[i],dof[i],val[i] );
      if(!i)
      {
        if(!setDep[dof[0]])
        {
          sprintf(buffer, "%s%d", DEP,dof[0]);
          setDep[dof[0]]=pre_setax( buffer, "i", 0);
        }
        //setax(setDep[0], "e", el[0]);
        //setax(setDep[dof[0]], "e", el[0]);
        j=setax(setDep[0],"j",0);
        if(j>-1)
	{
          setx[setDep[0]].elf[j].e=el[0];
          setx[setDep[0]].elf[j].f=face[0];
        }
        j=setax(setDep[dof[0]],"j",0);
        if(j>-1)
	{
          setx[setDep[dof[0]]].elf[j].e=el[0];
          setx[setDep[dof[0]]].elf[j].f=face[0];
        }
      }
      else
      {
        if(!setInd[dof[i]])
        {
          sprintf(buffer, "%s%d", IND,dof[i]);
          setInd[dof[i]]=pre_setax( buffer, "i", 0);
        }
        //setax(setInd[0], "e", el[i]);
        //setax(setInd[dof[i]], "e", el[i]);
        j=setax(setInd[0],"j",0);
        if(j>-1)
	{
          setx[setInd[0]].elf[j].e=el[i];
          setx[setInd[0]].elf[j].f=face[i];
        }
        j=setax(setInd[dof[i]],"j",0);
        if(j>-1)
	{
          setx[setInd[dof[i]]].elf[j].e=el[i];
          setx[setInd[dof[i]]].elf[j].f=face[i];
        }
      }
    }
  }while(1);     
  return(1);
}



int getContactPair(char *rec_str)
{
  int i,f,args, length;
  int setdep, setind;
  static int setMasCont, setSlavCont, setMasTie, setSlavTie, setMasTieCyc, setSlavTieCyc, setMasTieMult, setSlavTieMult;
  int setMas=0, setSlav=0;

  /* reset all static variables */
  if(resetFlag)
  {
    setMasTieCyc=setSlavTieCyc=setMasTieMult=setSlavTieMult=setMasTie=setSlavTie=setMasCont=setSlavCont=0;
    return(0);
  }

  /* get the arguments of the string */
  args=crecord(rec_str, dat);

  /* toupper */
  i=0; while(dat[0][i]!='\0') { dat[0][i]=toupper(dat[0][i]); i++; }

  if(compare(dat[0], "*TIE",4)==4)
  {
    for(i=1; i<args; i++)
    {
      if(compare(dat[i], "CYCLIC",6)==6)
      {
        if(!setMasTieCyc)  { setMasTieCyc =pre_setax( TIECYC2, "i", 0);  }
        if(!setSlavTieCyc) { setSlavTieCyc=pre_setax( TIECYC1, "i", 0);  }
        setMas=setMasTieCyc;
        setSlav=setSlavTieCyc;
      }
      else if(compare(dat[i], "MULTISTAGE",6)==6)
      {
        if(!setMasTieMult)  { setMasTieMult =pre_setax( TIEMUL2, "i", 0);  }
        if(!setSlavTieMult) { setSlavTieMult=pre_setax( TIEMUL1, "i", 0);  }
        setMas=setMasTieMult;
        setSlav=setSlavTieMult;
      }
    }
    if(setMas==0)
    {
      if(!setMasTie)  { setMasTie =pre_setax( TIE2, "i", 0);  }
      if(!setSlavTie) { setSlavTie=pre_setax( TIE1, "i", 0);  }
      setMas=setMasTie;
      setSlav=setSlavTie;
    }
  }
  else if (compare(dat[0], "*CONTACTPAIR",11)==11)
  {
    if(!setMasCont)  { setMasCont =pre_setax( CONT2, "i", 0);  }
    if(!setSlavCont) { setSlavCont=pre_setax( CONT1, "i", 0);  }
    setMas=setMasCont;
    setSlav=setSlavCont;
  }

  /* get the arguments of the 2nd line */
  length=abqrecord(rec_str);
  if(length==-1) return(-1);
  if(length==-2) return(-1);
  args=crecord(rec_str, dat);
  if(args!=2)
  {
    printf("ERROR: In *CONTACT PAIR 2nd line:%s \n", rec_str);
    return(-1);
  }

  setdep=getSetNrx(dat[0]);
  if(setdep<0)
  {
    printf("ERROR: In *CONTACT PAIR set:%s not defined\n", dat[0]);
    return(-1);
  }
  setind=getSetNrx(dat[1]);
  if(setind<0)
  {
    printf("ERROR: In *CONTACT PAIR set:%s not defined\n", dat[0]);
    return(-1);
  }

  /* relate both contact sets */
  setax(setdep,"r", setind);
  setax(setind,"r", setdep);

  /* store both in contact sets (master &slave) */
  for( i=0; i<setx[setind].anz_n; i++ ) setax(setMas, "n", setx[setind].node[i]);
  for( i=0; i<setx[setind].anz_elf; i++ )
  {
    f=setax(setMas,"j",0);
    if(f>-1)
    {
      setx[setMas].elf[f].e=setx[setind].elf[i].e;
      setx[setMas].elf[f].f=setx[setind].elf[i].f;
    }
  }
  for( i=0; i<setx[setdep].anz_n; i++ ) setax(setSlav, "n",setx[setdep].node[i] );
  for( i=0; i<setx[setdep].anz_elf; i++ )
  {
    f=setax(setSlav,"j",0);
    if(f>-1)
    {
      setx[setSlav].elf[f].e=setx[setdep].elf[i].e;
      setx[setSlav].elf[f].f=setx[setdep].elf[i].f;
    }
  }

  return(1);
}



int getSurfaces(char *rec_str)
{
  int i,j,n,args, length, el, eset;
  char name[MAX_LINE_LENGTH], type[MAX_LINE_LENGTH];
  int setNr, face, facei;

  /* defaults */
  strcpy(type, "ELEM"); 

  /* get the arguments of the string */
  args=crecord(rec_str, dat);

  /* extract the setname */
  sprintf(name, "+sur");
  for(i=0; i<args; i++)
  {
    /* delete blanks */
    n=0; for(j=0;j<strlen(dat[i]); j++) if(dat[i][j]!=' ') { buffer[n]=dat[i][j]; n++; }
    buffer[n]='\0';
    for(j=0;j<=strlen(buffer); j++) dat[i][j]=toupper(buffer[j]);
    if(compare(dat[i],"NAME=",5)==5) { strcpy(name,&buffer[5]); } 
    if(compare(dat[i],"TYPE=",5)==5) { strcpy(type,&dat[i][5]); } 
  }
  if(compareStrings(dat[0], "*SURFACE")<1)
  {
    printf(" ignore statement: %s\n", dat[0]);
    length=abqrecord(rec_str);
    return(1);
  }    
  if(printFlag) printf("*SURFACE, NAME=%s, TYPE=%s\n", name, type);

  setNr=getSetNrx(name);
  if(setNr<0) setNr=pre_setax( name, "i", 0);

  /* store setNr, element and face for later use in cgx */
  if(compare( type, "NODE", 4)==4)
  {
    do{
      length=abqrecord(rec_str);
      if(length==-1) return(-1);
      if(length==-2) break;
  
      args=crecord(rec_str, dat);
      el=atoi(dat[0]);
       
      /* check if the node-number is 0, then an eset is defined */
      if (el==0)
      {
        eset=getSetNrx(dat[0]);
        if(eset<0)
        {
          printf("ERROR: In *SURFACE set:%s not defined\n", dat[0]);
          return(-1);
        }
        for(j=0; j<setx[eset].anz_n; j++) setax(setNr, "n", setx[eset].node[j]);
      }
      else setax(setNr, "n", el);
    }while(1);
  }
  else if(compare( type, "ELEM", 4)==4)
  {
    do{
      length=abqrecord(rec_str);
      if(length==-1) return(-1);
      if(length==-2) break;
  
      args=crecord(rec_str, dat);
      el=atoi(dat[0]);
      face=(int)atoi(&dat[1][1])-1;
       
      // printf("el:%d f:%d\n", el, face);
      if(face>=-1)
      {
        /* check if the element-number is 0, then an eset is defined */
        if (el==0)
        {
          eset=getSetNrx(dat[0]);
          if(eset<0)
          {
            printf("ERROR: In *SURFACE set:%s not defined\n", dat[0]);
            return(-1);
          }
          for(j=0; j<setx[eset].anz_e; j++)
          {
	    facei=face;
	    el=setx[eset].elem[j];
            if((e_enqirex[el].type>6)&&(e_enqirex[el].type<11))
            {
              if(e_enqirex[el].attr>3)
              {
                if(e_enqirex[el].type<9)
                {
                  if(facei<0) { if(dat[1][1]=='N') facei=4; else facei=5; }
                  else facei+=1;
                }
                else
                {
                  if(facei<0) { if(dat[1][1]=='N') facei=5; else facei=6; }
                  else facei+=1;
                }
              }
              else
              {
                if(facei>1) facei+=1; else { if(dat[1][1]=='N') facei=0; else facei=1; }
              }
            }
	    //	printf("el:%d f:%d\n", el, facei);
            i=setax(setNr,"j",0);
            if(i>-1)
	    {
              setx[setNr].elf[i].e=el;
              setx[setNr].elf[i].f=facei;
	    }
          }
        }
        else
        {
          if((e_enqirex[el].type>6)&&(e_enqirex[el].type<11))
          {
            if(e_enqirex[el].attr>3)
            {
              if(e_enqirex[el].type<9)
              {
                if(face<0) { if(dat[1][1]=='N') face=4; else face=5; }
                else face+=1;
              }
              else
              {
                if(face<0) { if(dat[1][1]=='N') face=5; else face=6; }
                else face+=1;
              }
            }
            else
            {
              if(face>1) face+=1; else { if(dat[1][1]=='N') face=0; else face=1; }
            }
          }
          i=setax(setNr,"j",0);
          if(i>-1)
          {
	    //printf(" e:%d f:%d\n", el, face);
            setx[setNr].elf[i].e=el;
            setx[setNr].elf[i].f=face;
	  }
        }
      }
    }while(1);
  }
  else length=abqrecord(rec_str);
  return(1);
}


int getTransform(char *rec_str)
{
  int i,j,n,args, eset;
  char name[MAX_LINE_LENGTH], type[MAX_LINE_LENGTH], setname[MAX_LINE_LENGTH];
  int setNr;

  /* defaults */
  strcpy(type, "R"); 

  /* get the arguments of the string */
  args=crecord(rec_str, dat);

  /* extract the setname */
  for(i=0; i<args; i++)
  {
    /* delete blanks */
    n=0; for(j=0;j<strlen(dat[i]); j++) if(dat[i][j]!=' ') { buffer[n]=dat[i][j]; n++; }
    buffer[n]='\0';
    for(j=0;j<=strlen(buffer); j++) dat[i][j]=toupper(buffer[j]);
    if(compare(dat[i],"NSET=",5)==5) { strcpy(name,&buffer[5]); } 
    if(compare(dat[i],"TYPE=",5)==5) { strcpy(type,&dat[i][5]); } 
  }
  if(compareStrings(dat[0], "*TRANSFORM")<1)
  {
    printf(" ignore statement: %s\n", dat[0]);
    abqrecord(rec_str);
    return(1);
  }    
  if(printFlag) printf("*TRANSFORM, NSET=%s, TYPE=%s\n", name, type);
  abqrecord(rec_str);
  sprintf(setname, "+trans%s%s", type, rec_str);
  setNr=getSetNrx(setname);
  if(setNr<0) setNr=pre_setax( setname, "i", 0);

  eset=getSetNrx(name);
  if(eset<0)
  {
    printf("ERROR: In *TRANSFORM set:%s not defined\n",name );
    return(-1);
  }
  for(j=0; j<setx[eset].anz_n; j++) setax(setNr, "n", setx[eset].node[j]);
  return(1);
}


int getNodes(char *rec_str)
{
  int i,j,n,args,setFlag=0,setNr=0;
  char set[MAX_LINE_LENGTH];

  /* get the arguments of the string */
  args=crecord(rec_str, dat);

  /* extract the setname */
  sprintf(set, "NONE");
  for(i=0; i<args; i++)
  {
    /* delete blanks */
    n=0; for(j=0;j<strlen(dat[i]); j++) if(dat[i][j]!=' ') { buffer[n]=dat[i][j]; n++; }
    buffer[n]='\0';
    for(j=0;j<=strlen(buffer); j++) dat[i][j]=toupper(buffer[j]);
    if(compare(dat[i],"NSET=",5)==5)
    {
      if(strlen(&buffer[5])>0) {  setFlag=1; strcpy(set,&buffer[5]); }
    } 
  }
  if(compareStrings(dat[0], "*NODE")<1)
  {
    printf(" ignore statement: %s\n", dat[0]);
    abqrecord(rec_str);
    return(1);
  }    
  if(printFlag) printf("*NODE, NSET=%s\n", set);

  if(setFlag)
  {
    setNr=getSetNrx(set);
    if(setNr<0) setNr=pre_setax( set, "i", 0);
  }
  do{
    i=abqrecord(rec_str);
    if(i<0)
    {
      if(setFlag)
      {
        qsort( setx[setNr].node, setx[setNr].anz_n, sizeof(int), (void *)compareInt );
        /* erase multiple entities */
        n=0;
        for(j=1; j<setx[setNr].anz_n; j++)
        {
          if(setx[setNr].node[n]!=setx[setNr].node[j]) setx[setNr].node[++n]=setx[setNr].node[j];
        }
        if(setx[setNr].anz_n) setx[setNr].anz_n=n+1;
      }
      if(i==-1) return(-1);
      if(i==-2) break;
    }
    /* sscanf is faster (problem: between number and , is no space allowed) */
    //args=sscanf ( rec_str, "%d,%lf,%lf,%lf", &n, &x,&y,&z);
    dat[1][0]=dat[2][0]=dat[3][0]=0;
    args=crecord(rec_str, dat);
    n=atoi(dat[0]);
    if (n>=node_field_size)
    {
      if(n<MAX_INTEGER/2) node_field_size=n*2+1; else node_field_size=MAX_INTEGER-2;
      do
      {
        if ( (nodex = (Nodes *)realloc( (Nodes *)nodex, (node_field_size+1) * sizeof(Nodes))) == NULL )
        {
          printf("WARNING: INI_FIELD_SIZE:%d to large and is reduced\n", node_field_size );
          node_field_size=n+(node_field_size-n)/2;
        }
        if(node_field_size<=n)
        {
          printf("\n\n ERROR: not enough memory in readccx()\n\n");
          exit(-1);
        }
      }while(!nodex);
      for(j=anzx->nmax+1; j<=node_field_size; j++) nodex[j].indx=-1;
    }
    /* check if that node is already existent */
    /*
    nodex[n].nx=x;
    nodex[n].ny=y;
    nodex[n].nz=z;
    */

    nodex[n].nx=atof(dat[1]);
    nodex[n].ny=atof(dat[2]);
    nodex[n].nz=atof(dat[3]);

    if(nodex[n].indx<0)
    {
      nodex[anzx->n].nr=n;
      nodex[n].indx=anzx->n;
      if (n >  anzx->nmax)  anzx->nmax=n;
      if (n <  anzx->nmin)  anzx->nmin=n;
      if(setFlag)
      {
        //setax(setNr, "n" ,n);
        if ( (setx[setNr].node = (int *)realloc((int *)setx[setNr].node, (setx[setNr].anz_n+1) * sizeof(int))) == NULL )
          printf(" ERROR: malloc failed in set[%d]:%s\n\n", setNr, setx[setNr].name);
        setx[setNr].node[setx[setNr].anz_n]= n;
        setx[setNr].anz_n++;
      }
      anzx->n++; 
    }
#if TEST
    printf("node: %d xyz: %lf %lf %lf\n", n, nodex[n].nx,nodex[n].ny,nodex[n].nz);
#endif
    if(ansFlag)
    {
      /* write in ansys */
      fprintf(handleans, "N,%d,%12.5e,%12.5e,%12.5e,0,0,0\n", n, nodex[n].nx, nodex[n].ny, nodex[n].nz );
    }

  }while(1);     
  return(1);
}


int getMpc(char *rec_str)
{
  int i,n,args,setNr=0, length;

  setNr=getSetNrx(MPC);
  if(setNr<0) setNr=pre_setax( MPC, "i", 0);

  /* get the arguments of the string */
  /* args=crecord(rec_str, dat);     */
  
  /* scan the node-names and write to file */
  do
  {
    length=abqrecord(rec_str);

    if(length==-1) return(-1);
    if(length==-2) break;
    args=crecord(rec_str, dat);

    for(i=1; i<args; i++)
    {
      /* write in list format */
      n=atoi(dat[i]);
      if(n) setax( setNr, "n", n);
    }
  }while(1); 
  return(1);
}



int writeElem(int e, int t, int g, int m, int *nd, int attr)
{
  register int i;
  int ipuf=0,ipuf2=0;
  int prev_emax;

  prev_emax=anzx->emax;
  if (anzx->e>=elem_field_size)
  {
    if(anzx->e<MAX_INTEGER/2) elem_field_size=anzx->e*2+1; else elem_field_size=MAX_INTEGER-2;
    do
    {
      if((elemx = (Elements *)realloc( (Elements *)elemx, (elem_field_size+1) * sizeof(Elements))) == NULL )
      {
        printf("WARNING: INI_FIELD_SIZE:%d to large and is reduced\n", elem_field_size);
        elem_field_size=anzx->e+(elem_field_size-anzx->e)/2;
      }
      if(elem_field_size<=anzx->e)
      {
        printf("\n\n ERROR: not enough memory in readccx()\n\n");
        exit(-1);
      }
    }while(!elemx);
  }
  
  elemx[anzx->e].nr   = e;
  if (elemx[anzx->e].nr >  anzx->emax)  anzx->emax=elemx[anzx->e].nr;
  if (elemx[anzx->e].nr <  anzx->emin)  anzx->emin=elemx[anzx->e].nr;

  elemx[anzx->e].type = t; 
  elemx[anzx->e].group= g;
  elemx[anzx->e].mat  = m;
  if(toupper(attr)=='R') elemx[anzx->e].attr  = 1;
  else if(toupper(attr)=='I') elemx[anzx->e].attr  = 2;
  else if(toupper(attr)=='D') elemx[anzx->e].attr  = 3;
  else if(toupper(attr)=='E') elemx[anzx->e].attr  = 4;
  else if(toupper(attr)=='S') elemx[anzx->e].attr  = 5;
  else if(toupper(attr)=='C') elemx[anzx->e].attr  = 6;
  else if(toupper(attr)=='F') elemx[anzx->e].attr  = 7;
  else if(toupper(attr)=='M') elemx[anzx->e].attr  = 8;
  else elemx[anzx->e].attr  = 0;

  switch(elemx[anzx->e].type)
  {
    case 1:
      ipuf = 8;       /* HEXA8 */
    break;
    case 2:
      ipuf = 6;  /* PENTA6 */
    break;
    case 3:
      ipuf = 4;  /* TET4 */
    break;
    case 4:
      ipuf = 20; /* HEXA20 */
    break;
    case 5:
      ipuf = 15; /* PENTA15 */
    break;
    case 6:
      ipuf = 10; /* TET10 */
    break;
    case 7:
      ipuf = 3;  /* TRI3  */
    break;
    case 8:
      ipuf = 6;  /* TRI6  */
    break;
    case 9:
      ipuf = 4;  /* QUAD4 */
    break;
    case 10:
      ipuf = 8; /* QUAD8 */
    break;
    case 11:
      ipuf = 2; /* BEAM */
    break;
    case 12:
      ipuf = 3; /* BEAM3 */
    break;
  }
  if (ipuf==0)
  {
    printf (" elem(%d) not a known type (%d)\n", elemx[anzx->e].nr, elemx[anzx->e].type);
  }
  else
  {
    anzx->etype[elemx[anzx->e].type]++;
    for (i=0; i<ipuf; i++) { elemx[anzx->e].nod[i] = nd[i];  }

    /* set the midside nodenr to 0 */
    if (elemx[anzx->e].type == 4) ipuf2 = 26; /* HEX20 */
    else if (elemx[anzx->e].type == 5) ipuf2 = 20; /* PENTA15 */
    else if (elemx[anzx->e].type == 10) ipuf2 = 9; /* QUAD8 */
    if (ipuf2!=0) for (i=ipuf; i<ipuf2; i++)
    {
      elemx[anzx->e].nod[i]=0;
    }
  }

  /* ini e_enqire-feld in dem e_enqire[elem[i].nr].xx abgelegt ist, statt elem[i].xx  */
  if (e>=elem_field_size2)
  {
    if(e<MAX_INTEGER/2) elem_field_size2=e*2+1; else elem_field_size2=MAX_INTEGER-2;
    do
    {
      if((e_enqirex=(Elements *)realloc((Elements *)e_enqirex,(elem_field_size2+1)*sizeof(Elements)))==NULL)
      {
        printf("WARNING: INI_FIELD_SIZE:%d to large and is reduced\n", elem_field_size);
        elem_field_size2=e+(elem_field_size2-e)/2;
      }
      if(elem_field_size2<=e)
      {
        printf("\n\n ERROR: not enough memory in readccx()\n\n");
        exit(-1);
      }
    }while(!e_enqirex);
  }

  if(anzx->e>anzx->emax)
  {
    printf("ERROR: found more elements:%d than the maximum number:%d\n probably elements are redefined. This is not permitted.\n", anzx->e,anzx->emax );
    exit(-1);
  }
  for(i=prev_emax+1; i<e; i++) { e_enqirex[i].type=0; }
  e_enqirex[ e ].type    = t;
  e_enqirex[ e ].group   = g;
  e_enqirex[ e ].mat     = m;
  e_enqirex[ e ].attr     = elemx[anzx->e].attr;
  for (i=0; i<ipuf; i++)
  {
    e_enqirex[ e ].nod[i]=nd[i];
  }
  anzx->e++;
  return(1);
}


int writeElemAns(int mat, int *selected_elems, Summen *ans, Nodes *nodex, Elements *elemx)
{
  int i,j;
  static int type=0, anz_e=0;
  int el, nd[20];  

  /* reset all static variables */
  if(resetFlag)
  {
    type=anz_e=0;
    return(0);
  }

  for(i=0; i<anzx->e; i++) if(selected_elems[elemx[i].nr])
  {

    el=elemx[i].nr;
    for(j=0; j<20; j++) nd[j]=elemx[i].nod[j];

    if(elemx[i].type==1)
    {
      if (((anz_e>0)&&(type != 1))||(anz_e==0))
      {
        fprintf (handleans, "ET, %d, 45\n", elemx[i].type);
        fprintf (handleans, "TYPE, %d\n", elemx[i].type);
      }
      fprintf (handleans, "EN,%8d,%8d,%8d,%8d,%8d,%8d,%8d,%8d,%8d \n",el, nd[0], nd[1], nd[2], nd[3],nd[4], nd[5], nd[6], nd[7] );
      type=1;
      anz_e++;
    }

    if(elemx[i].type==4)
    {
      if (((anz_e>0)&&(type != 4))||(anz_e==0))
      {
        fprintf (handleans, "ET, %d, 95\n", elemx[i].type);
        fprintf (handleans, "TYPE, %d\n", elemx[i].type);
      }
      fprintf (handleans, "EN,%8d,%8d,%8d,%8d,%8d,%8d,%8d,%8d,%8d \n",el, nd[0], nd[1], nd[2], nd[3],nd[4], nd[5], nd[6], nd[7] );
      fprintf (handleans, "EMORE,%8d,%8d,%8d,%8d,%8d,%8d,%8d,%8d \n", nd[8],  nd[9], nd[10], nd[11], nd[16], nd[17], nd[18], nd[19] );
      fprintf (handleans, "EMORE,%8d,%8d,%8d,%8d \n", nd[12], nd[13], nd[14], nd[15] );
      type=4;
      anz_e++;
    }

    if(elemx[i].type==9)
    {
      if (((anz_e>0)&&(type != 9))||(anz_e==0))
      {
	/* plastic shell */
        fprintf (handleans, "ET, %d, 43\n", elemx[i].type);
        fprintf (handleans, "TYPE, %d\n", elemx[i].type);
        /* elastic shell */
        /* fprintf (handleans, "ET, %d, 63\n", elem[i].type); */
      }
      fprintf (handleans, "EN,%8d,%8d,%8d,%8d,%8d \n",el, nd[0], nd[1], nd[2], nd[3] );
      type=9;
      anz_e++;
    }

    if(elemx[i].type==7)
    {
      if (((anz_e>0)&&(type != 7))||(anz_e==0))
      {
	/* plastic shell */
        fprintf (handleans, "ET, %d, 43\n", elemx[i].type);
        /* elastic shell */
        /* fprintf (handleans, "ET, %d, 63\n", elem[i].type); */
        fprintf (handleans, "TYPE, %d\n", elemx[i].type);
      }
      fprintf (handleans, "EN,%8d,%8d,%8d,%8d,%8d \n",el, nd[0], nd[1], nd[2], nd[2] );
      type=7;
      anz_e++;
    }

    if(elemx[i].type==6)
    {
      if (((anz_e>0)&&(type != 6))||(anz_e==0))
      {
        fprintf (handleans, "ET, %d, 92\n", elemx[i].type);
        fprintf (handleans, "TYPE, %d\n", elemx[i].type);
      }
      fprintf (handleans, "EN,%8d,%8d,%8d,%8d,%8d,%8d,%8d,%8d,%8d,\n",el, nd[0], nd[1], nd[2], nd[3],nd[4], nd[5], nd[6], nd[7]);
      fprintf (handleans, "EMORE,%8d,%8d \n", nd[8],  nd[9]);
      type=6;
      anz_e++;
    }
  }
  return(1);
}



int hashAmplitude( SumAsci *sumAsci, char *name, int nr)
{
  int i=0,j=0;
  int sum=0;

  while(name[i]!='\0') { sum+=name[i]*(++j); i++;}

  /* check if sum is higher as the allocated value */
  /* else look for a free entry */
  if(sum>sumAsci->max_sumamp)
  {
    if ((sumAsci->anzamp=(int *)realloc( (int *)sumAsci->anzamp, (sum+1)*sizeof(int)) ) == NULL )
    { printf("\n\nERROR: realloc failure in hashAmplitude(), Amplitude:%s not included\n\n", name); return(-1); }
    if ((sumAsci->ampindx=(int **)realloc( (int **)sumAsci->ampindx, (sum+1)*sizeof(int *)) ) == NULL )
    { printf("\n\nERROR: realloc failure in hashAmplitude(), Amplitude:%s not included\n\n", name); return(-1); }
    for(i=sumAsci->max_sumamp+1; i<=sum; i++) { sumAsci->anzamp[i]=0; sumAsci->ampindx[i]=NULL; }
    sumAsci->max_sumamp=sum;
  }

  /* alloc of a new entry in the hash table */
  if ((sumAsci->ampindx[sum] 
  =(int *)realloc( (int *)sumAsci->ampindx[sum], (sumAsci->anzamp[sum]+1)*sizeof(int)) ) == NULL )
  { printf("\n\nERROR: realloc failure in hashAmplitude(), pnt:%s not included\n\n", name); return(-1); }

  sumAsci->ampindx[sum][sumAsci->anzamp[sum]] = nr;
  sumAsci->anzamp[sum]++;
  return(sum);
}


int hashMaterial( SumAsci *sumAsci, char *name, int nr)
{
  int i=0,j=0;
  int sum=0;

  while(name[i]!='\0') { sum+=name[i]*(++j); i++;}

  /* check if sum is higher as the allocated value */
  /* else look for a free entry */
  if(sum>sumAsci->max_summat)
  {
    if ((sumAsci->anzmat=(int *)realloc( (int *)sumAsci->anzmat, (sum+1)*sizeof(int)) ) == NULL )
    { printf("\n\nERROR: realloc failure in hashMaterial(), Material:%s not included\n\n", name); return(-1); }
    if ((sumAsci->matindx=(int **)realloc( (int **)sumAsci->matindx, (sum+1)*sizeof(int *)) ) == NULL )
    { printf("\n\nERROR: realloc failure in hashMaterial(), Material:%s not included\n\n", name); return(-1); }
    for(i=sumAsci->max_summat+1; i<=sum; i++) { sumAsci->anzmat[i]=0; sumAsci->matindx[i]=NULL; }
    sumAsci->max_summat=sum;
  }

  /* alloc of a new entry in the hash table */
  if ((sumAsci->matindx[sum] 
  =(int *)realloc( (int *)sumAsci->matindx[sum], (sumAsci->anzmat[sum]+1)*sizeof(int)) ) == NULL )
  { printf("\n\nERROR: realloc failure in hashMaterial(), pnt:%s not included\n\n", name); return(-1); }

  sumAsci->matindx[sum][sumAsci->anzmat[sum]] = nr;
  sumAsci->anzmat[sum]++;
  return(sum);
}


int getAmplitudeNr(char *name, int checkFlag)
{
  static int i, n, length,sum;

  i=length=sum=0; 

  while(name[length]!='\0') { sum+=name[length++]*(++i); }
  if(!length) return(-1);
  
  if(sum<0)
  {
    printf ("ERROR: Illegal name:|%s| sum-ascii:%d\n", name, sum);
    return(-1); 
  }
  if(sum<=sumAsci->max_sumamp)
  {
   for (i=0; i<sumAsci->anzamp[sum]; i++)
   {
    if(( amplitude[sumAsci->ampindx[sum][i]].name != (char *)NULL ) && (strlen(amplitude[sumAsci->ampindx[sum][i]].name) == length))
    { 
      n=length-1;
      while(amplitude[sumAsci->ampindx[sum][i]].name[n]==name[n])
      {
        if(!n--)
        {
          return(sumAsci->ampindx[sum][i]);
	}
      }
      /*
      printf(" name:%s nam:%s indx:%d\n"
      , name, amplitude[sumAsci->ampindx[sum][i]].name, sumAsci->ampindx[sum][i]);
      */
    }
   }
  }

  if (checkFlag) return(-1);

  if (anzx->amps>=amp_field_size-1)
  {
    if(anzx->amps<MAX_INTEGER/2) amp_field_size=anzx->amps*2+1; else amp_field_size=MAX_INTEGER-2;
    do
    {
      if ((amplitude = (Amplitudes *)realloc( (Amplitudes *)amplitude, (amp_field_size+1)*sizeof(Amplitudes)) ) == NULL )
      {
        printf("WARNING: INI_FIELD_SIZE:%d to large and is reduced\n", amp_field_size );
        amp_field_size=anzx->amps+(amp_field_size-anzx->amps)/2;
      }
      if(amp_field_size<=anzx->amps)
      {
        printf("\n\n ERROR: not enough memory in readccx()\n\n");
        exit(-1);
      }
    }while(!amplitude);
  }
  if(printFlag) printf("create amplitude[%d]:%s\n", anzx->amps, name);  
  strcpy(amplitude[anzx->amps].name, name);
  hashAmplitude( sumAsci, name, anzx->amps );
  amplitude[anzx->amps].n=0;
  amplitude[anzx->amps].x=NULL;
  amplitude[anzx->amps].y=NULL;
  anzx->amps++;

  return(anzx->amps-1);
}


int getMatNr(char *name, int checkFlag)
{
  static int i, n, length, sum;

  i=length=sum=0; 

  while(name[length]!='\0') { sum+=name[length++]*(++i); }
  if(!length) return(-1);
  
  if(sum<0)
  {
    printf ("ERROR: Illegal name:|%s| sum-ascii:%d\n", name, sum);
    return(-1); 
  }
  if(sum<=sumAsci->max_summat)
  {
   for (i=0; i<sumAsci->anzmat[sum]; i++)
   {
    if(( material[sumAsci->matindx[sum][i]].name != (char *)NULL ) && (strlen(material[sumAsci->matindx[sum][i]].name) == length))
    { 
      n=length-1;
      while(material[sumAsci->matindx[sum][i]].name[n]==name[n])
      {
        if(!n--)
        {
          return(sumAsci->matindx[sum][i]);
	}
      }
      /*
      printf(" name:%s nam:%s indx:%d\n"
      , name, material[sumAsci->matindx[sum][i]].name, sumAsci->matindx[sum][i]);
      */
    }
   }
  }

  if (checkFlag) return(-1);

  if (anzx->mats>=mat_field_size-1)
  {
    if(anzx->mats<MAX_INTEGER/2) mat_field_size=anzx->mats*2+1; else mat_field_size=MAX_INTEGER-2;
    do
    {
      if ((material = (Materials *)realloc( (Materials *)material, (mat_field_size+1)*sizeof(Materials)) ) == NULL )
      {
        printf("WARNING: INI_FIELD_SIZE:%d to large and is reduced\n", mat_field_size );
        mat_field_size=anzx->mats+(mat_field_size-anzx->mats)/2;
      }
      if(mat_field_size<=anzx->mats)
      {
        printf("\n\n ERROR: not enough memory in readccx()\n\n");
        exit(-1);
      }
    }while(!material);
  }
  if(printFlag) printf("create material[%d]:%s\n", anzx->mats, name);  
  strcpy(material[anzx->mats].name, name);
  hashMaterial( sumAsci, name, anzx->mats );
  material[anzx->mats].rho=-1.;
  material[anzx->mats].nela=0;
  material[anzx->mats].tela=NULL;
  material[anzx->mats].ela=NULL;
  material[anzx->mats].nue=NULL;
  material[anzx->mats].nexp=0;
  material[anzx->mats].texp=NULL;
  material[anzx->mats].exp=NULL;
  material[anzx->mats].ncon=0;
  material[anzx->mats].tcon=NULL;
  material[anzx->mats].con=NULL;
  material[anzx->mats].nsph=0;
  material[anzx->mats].tsph=NULL;
  material[anzx->mats].sph=NULL;
  anzx->mats++;

  return(anzx->mats-1);
}


int getAbqElement(char *rec_str, int nn, int *el, int *nd)
{
  register int i,j,n;

      n=0;
      i=abqrecord(rec_str);
      if(i<0)
      {
        if(i==-1) return(-1);
        if(i==-2) return(-2);
      }
      i=crecord(rec_str, dat);
      *el=atoi(dat[0]);
      for(j=0; j<i-1; j++)
      {
        if(dat[j+1][0]!=0) nd[n]=atoi(dat[j+1]); else break;
        n++;
        //  printf("a j:%d n:%d nd:%d\n",j,n-1,nd[n-1]);  
        //if(nd[n-1]==0) { n--; break; }
      }
      while(n<nn)
      {
        i=abqrecord(rec_str);
        if(i<0)
        {
          if(i==-1) return(-1);
          if(i==-2) return(-2);
	}
        i=crecord(rec_str, dat);
        for(j=0; j<i; j++)
        {
          nd[n]=atoi(dat[j]);
          n++;
          // printf("b j:%d n:%d nd:%d\n",j,n-1,nd[n-1]); 
          //if(nd[n-1]==0) { n--; break; }
	}
      }
  return(1);
}

/* uses sscanf (problem: between number and ',' is no space allowed, if removed -> not longer faster!) */
int __getAbqElement(char *rec_str, int nn, int *el, int *nd)
{
    int i,j,n=0,args;

      i=abqrecord(rec_str);
      if(i<0)
      {
        if(i==-1) return(-1);
        if(i==-2) return(-2);
      }
      args=sscanf ( rec_str, "%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d", el, &nd[0], &nd[1], &nd[2], &nd[3], &nd[4], &nd[5], &nd[6], &nd[7], &nd[8], &nd[9], &nd[10], &nd[11], &nd[12], &nd[13], &nd[14]);

      // printf("a:%d e:%d ", args, *el);

      for(n=0; n<args-1; n++)
      {
	//printf(" n[%d]:%d ",n, nd[n]);
        if(nd[n]==0) break;
      }
      while(n<nn)
      {
        i=abqrecord(rec_str);
        if(i<0)
        {
          if(i==-1) return(-1);
          if(i==-2) return(-2);
	}
        args=sscanf ( rec_str, "%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d", &nd[n], &nd[n+1], &nd[n+2], &nd[n+3], &nd[n+4], &nd[n+5], &nd[n+6], &nd[n+7], &nd[n+8], &nd[n+9], &nd[n+10], &nd[n+11], &nd[n+12], &nd[n+13], &nd[n+14], &nd[n+15]);
        for(j=0; j<args; j++)
        {
          // printf(" n[%d]:%d ",n, nd[n]);
          if(nd[n]==0) break;
          n++;
	}
      }
      // printf("\n");
  return(1);
}



/* seta replaced with direct malloc (somehow not faster?? ->because data are usually ordered? */
/* not used because if *ELEMENT occurs for each element than this new function is extreme slow */
/* original function is getElements */   
int __getElements(char *rec_str)
{
  int i,j,n, args, setFlag=0, setNr=0;
  char set[MAX_LINE_LENGTH], elemtyp[MAX_LINE_LENGTH], setname[MAX_LINE_LENGTH], cbuf;
  int el, se, nd[40];
  int eattr=0;  


  /* get the arguments of the string */
  args=crecord(rec_str, dat);
  
  /* extract element-type and setname */
  for(i=0; i<args; i++)
  {
    /* delete blanks */
    n=0; for(j=0;j<strlen(dat[i]); j++) if(dat[i][j]!=' ') { buffer[n]=dat[i][j]; n++; }
    buffer[n]='\0';
    for(j=0;j<=strlen(buffer); j++) dat[i][j]=toupper(buffer[j]);
    if(compare(dat[i],"TYPE=",5)==5) strcpy(elemtyp,&dat[i][5]);
    if(compare(dat[i],"ELSET=",6)==6) 
    {
      if(strlen(&buffer[6])>0) { strcpy(set,&buffer[6]); setFlag=1; }
    } 
  }
  if(printFlag) printf("*ELEMENT, TYPE=%s ,ELSET=%s\n", elemtyp, set);

  /* write to file */
  if(setFlag)
  { 
    setNr=getSetNrx(set);
    if(setNr<0) setNr=pre_setax( set, "i", 0);
  }
  sprintf(setname,"+%s", elemtyp);

  /* quickfix: conversion from fluit type into structural */
  if(elemtyp[0]=='F') { elemtyp[0]='C'; eattr='F'; }

  if(compare(elemtyp,"C3D8",4)==4)
  {
    se=pre_setax(setname, "i", 0);
    do{
      if(getAbqElement(rec_str, 8, &el, nd) <0)
      {
        if(setFlag)
	{
          qsort( setx[setNr].elem, setx[setNr].anz_e, sizeof(int), (void *)compareInt );
          /* erase multiple entities */
          n=0;
          for(i=1; i<setx[setNr].anz_e; i++)
          {
  	  if(setx[setNr].elem[n]!=setx[setNr].elem[i]) setx[setNr].elem[++n]=setx[setNr].elem[i];
          }
          if(setx[setNr].anz_e) setx[setNr].anz_e=n+1;
	}
        qsort( setx[se].elem, setx[se].anz_e, sizeof(int), (void *)compareInt );
        /* erase multiple entities */
        n=0;
        for(i=1; i<setx[se].anz_e; i++)
        {
  	if(setx[se].elem[n]!=setx[se].elem[i]) setx[se].elem[++n]=setx[se].elem[i];
        }
        if(setx[se].anz_e) setx[se].anz_e=n+1;
        break;
      }

      if(eattr=='F') writeElem(el, 1, 1, 1, nd, eattr );
      else writeElem(el, 1, 1, 1, nd, elemtyp[4]);

      /* write set */
      if(setFlag)
      {
        if ( (setx[setNr].elem = (int *)realloc((int *)setx[setNr].elem, (setx[setNr].anz_e+1) * sizeof(int))) == NULL )
          printf(" ERROR: malloc failed in set[%d]:%s\n\n", setNr, setx[setNr].name);
        setx[setNr].elem[setx[setNr].anz_e]= el;
        setx[setNr].anz_e++;
      }
      if ( (setx[se].elem = (int *)realloc((int *)setx[se].elem, (setx[se].anz_e+1) * sizeof(int))) == NULL )
        printf(" ERROR: malloc failed in set[%d]:%s\n\n", se, setx[se].name);
      setx[se].elem[setx[se].anz_e]= el;
      setx[se].anz_e++;
    }while(1); 
  }
  else if(compare(elemtyp,"C3D20",5)==5)
  {
    se=pre_setax(setname, "i", 0);
    do{
      if(getAbqElement(rec_str, 20, &el, nd) <0)
      {
        if(setFlag)
	{
          qsort( setx[setNr].elem, setx[setNr].anz_e, sizeof(int), (void *)compareInt );
          /* erase multiple entities */
          n=0;
          for(i=1; i<setx[setNr].anz_e; i++)
          {
  	  if(setx[setNr].elem[n]!=setx[setNr].elem[i]) setx[setNr].elem[++n]=setx[setNr].elem[i];
          }
          if(setx[setNr].anz_e) setx[setNr].anz_e=n+1;
	}
        qsort( setx[se].elem, setx[se].anz_e, sizeof(int), (void *)compareInt );
        /* erase multiple entities */
        n=0;
        for(i=1; i<setx[se].anz_e; i++)
        {
	  if(setx[se].elem[n]!=setx[se].elem[i]) setx[se].elem[++n]=setx[se].elem[i];
        }
        if(setx[se].anz_e) setx[se].anz_e=n+1;
        break;
      }

      for(i=0; i<12; i++) nd[20+i]=nd[i];
      for(i=0; i<4; i++) nd[32+i]=nd[i+16];
      for(i=0; i<4; i++) nd[36+i]=nd[i+12];

      writeElem(el, 4, 4, 1, &nd[20], elemtyp[5]);

      /* write set */
      if(setFlag)
      {
        if ( (setx[setNr].elem = (int *)realloc((int *)setx[setNr].elem, (setx[setNr].anz_e+1) * sizeof(int))) == NULL )
          printf(" ERROR: malloc failed in set[%d]:%s\n\n", setNr, setx[setNr].name);
        setx[setNr].elem[setx[setNr].anz_e]= el;
        setx[setNr].anz_e++;
      }
      if ( (setx[se].elem = (int *)realloc((int *)setx[se].elem, (setx[se].anz_e+1) * sizeof(int))) == NULL )
        printf(" ERROR: malloc failed in set[%d]:%s\n\n", se, setx[se].name);
      setx[se].elem[setx[se].anz_e]= el;
      setx[se].anz_e++;
    }while(1); 
  }
  else if(compare(elemtyp,"C3D4",4)==4)
  {
    se=pre_setax(setname, "i", 0);
    do{
      if(getAbqElement(rec_str, 4, &el, nd) <0)
      {
        if(setFlag)
	{
          qsort( setx[setNr].elem, setx[setNr].anz_e, sizeof(int), (void *)compareInt );
          /* erase multiple entities */
          n=0;
          for(i=1; i<setx[setNr].anz_e; i++)
          {
  	  if(setx[setNr].elem[n]!=setx[setNr].elem[i]) setx[setNr].elem[++n]=setx[setNr].elem[i];
          }
          if(setx[setNr].anz_e) setx[setNr].anz_e=n+1;
	}
        qsort( setx[se].elem, setx[se].anz_e, sizeof(int), (void *)compareInt );
        /* erase multiple entities */
        n=0;
        for(i=1; i<setx[se].anz_e; i++)
        {
	  if(setx[se].elem[n]!=setx[se].elem[i]) setx[se].elem[++n]=setx[se].elem[i];
        }
        if(setx[se].anz_e) setx[se].anz_e=n+1;
        break;
      }

      writeElem(el, 3, 3, 1, nd, elemtyp[4]);

      /* write set */
      if(setFlag)
      {
        if ( (setx[setNr].elem = (int *)realloc((int *)setx[setNr].elem, (setx[setNr].anz_e+1) * sizeof(int))) == NULL )
          printf(" ERROR: malloc failed in set[%d]:%s\n\n", setNr, setx[setNr].name);
        setx[setNr].elem[setx[setNr].anz_e]= el;
        setx[setNr].anz_e++;
      }
      if ( (setx[se].elem = (int *)realloc((int *)setx[se].elem, (setx[se].anz_e+1) * sizeof(int))) == NULL )
        printf(" ERROR: malloc failed in set[%d]:%s\n\n", se, setx[se].name);
      setx[se].elem[setx[se].anz_e]= el;
      setx[se].anz_e++;
    }while(1); 
  }
  else if(compare(elemtyp,"C3D6",5)==5)
  {
    se=pre_setax(setname, "i", 0);
    do{
      if(getAbqElement(rec_str, 6, &el, nd) <0)
      {
        if(setFlag)
	{
          qsort( setx[setNr].elem, setx[setNr].anz_e, sizeof(int), (void *)compareInt );
          /* erase multiple entities */
          n=0;
          for(i=1; i<setx[setNr].anz_e; i++)
          {
  	  if(setx[setNr].elem[n]!=setx[setNr].elem[i]) setx[setNr].elem[++n]=setx[setNr].elem[i];
          }
          if(setx[setNr].anz_e) setx[setNr].anz_e=n+1;
	}
        qsort( setx[se].elem, setx[se].anz_e, sizeof(int), (void *)compareInt );
        /* erase multiple entities */
        n=0;
        for(i=1; i<setx[se].anz_e; i++)
        {
	  if(setx[se].elem[n]!=setx[se].elem[i]) setx[se].elem[++n]=setx[se].elem[i];
        }
        if(setx[se].anz_e) setx[se].anz_e=n+1;
        break;
      }

      writeElem(el, 2, 2, 1, nd, elemtyp[4]);

      /* write set */
      if(setFlag)
      {
        if ( (setx[setNr].elem = (int *)realloc((int *)setx[setNr].elem, (setx[setNr].anz_e+1) * sizeof(int))) == NULL )
          printf(" ERROR: malloc failed in set[%d]:%s\n\n", setNr, setx[setNr].name);
        setx[setNr].elem[setx[setNr].anz_e]= el;
        setx[setNr].anz_e++;
      }
      if ( (setx[se].elem = (int *)realloc((int *)setx[se].elem, (setx[se].anz_e+1) * sizeof(int))) == NULL )
        printf(" ERROR: malloc failed in set[%d]:%s\n\n", se, setx[se].name);
      setx[se].elem[setx[se].anz_e]= el;
      setx[se].anz_e++;
    }while(1); 
  }
  else if(compare(elemtyp,"C3D10",5)==5)
  {
    se=pre_setax(setname, "i", 0);
    do{
      if(getAbqElement(rec_str, 10, &el, nd) <0)
      {
        if(setFlag)
	{
          qsort( setx[setNr].elem, setx[setNr].anz_e, sizeof(int), (void *)compareInt );
          /* erase multiple entities */
          n=0;
          for(i=1; i<setx[setNr].anz_e; i++)
          {
  	  if(setx[setNr].elem[n]!=setx[setNr].elem[i]) setx[setNr].elem[++n]=setx[setNr].elem[i];
          }
          if(setx[setNr].anz_e) setx[setNr].anz_e=n+1;
	}
        qsort( setx[se].elem, setx[se].anz_e, sizeof(int), (void *)compareInt );
        /* erase multiple entities */
        n=0;
        for(i=1; i<setx[se].anz_e; i++)
        {
	  if(setx[se].elem[n]!=setx[se].elem[i]) setx[se].elem[++n]=setx[se].elem[i];
        }
        if(setx[se].anz_e) setx[se].anz_e=n+1;
        break;
      }

      writeElem(el, 6, 6, 1, nd, elemtyp[5]);

      /* write set */
      if(setFlag)
      {
        if ( (setx[setNr].elem = (int *)realloc((int *)setx[setNr].elem, (setx[setNr].anz_e+1) * sizeof(int))) == NULL )
          printf(" ERROR: malloc failed in set[%d]:%s\n\n", setNr, setx[setNr].name);
        setx[setNr].elem[setx[setNr].anz_e]= el;
        setx[setNr].anz_e++;
      }
      if ( (setx[se].elem = (int *)realloc((int *)setx[se].elem, (setx[se].anz_e+1) * sizeof(int))) == NULL )
        printf(" ERROR: malloc failed in set[%d]:%s\n\n", se, setx[se].name);
      setx[se].elem[setx[se].anz_e]= el;
      setx[se].anz_e++;
    }while(1); 
  }
  else if(compare(elemtyp,"C3D15",5)==5)
  {
    se=pre_setax(setname, "i", 0);
    do{
      if(getAbqElement(rec_str, 15, &el, nd) <0)
      {
        if(setFlag)
	{
          qsort( setx[setNr].elem, setx[setNr].anz_e, sizeof(int), (void *)compareInt );
          /* erase multiple entities */
          n=0;
          for(i=1; i<setx[setNr].anz_e; i++)
          {
  	  if(setx[setNr].elem[n]!=setx[setNr].elem[i]) setx[setNr].elem[++n]=setx[setNr].elem[i];
          }
          if(setx[setNr].anz_e) setx[setNr].anz_e=n+1;
	}
        qsort( setx[se].elem, setx[se].anz_e, sizeof(int), (void *)compareInt );
        /* erase multiple entities */
        n=0;
        for(i=1; i<setx[se].anz_e; i++)
        {
	  if(setx[se].elem[n]!=setx[se].elem[i]) setx[se].elem[++n]=setx[se].elem[i];
        }
        if(setx[se].anz_e) setx[se].anz_e=n+1;
        break;
      }

      for(i=0; i<9; i++) nd[20+i]=nd[i];
      for(i=0; i<3; i++) nd[29+i]=nd[i+12];
      for(i=0; i<3; i++) nd[32+i]=nd[i+9];

      writeElem(el, 5, 5, 1, &nd[20], elemtyp[5]);

      /* write set */
      if(setFlag)
      {
        if ( (setx[setNr].elem = (int *)realloc((int *)setx[setNr].elem, (setx[setNr].anz_e+1) * sizeof(int))) == NULL )
          printf(" ERROR: malloc failed in set[%d]:%s\n\n", setNr, setx[setNr].name);
        setx[setNr].elem[setx[setNr].anz_e]= el;
        setx[setNr].anz_e++;
      }
      if ( (setx[se].elem = (int *)realloc((int *)setx[se].elem, (setx[se].anz_e+1) * sizeof(int))) == NULL )
        printf(" ERROR: malloc failed in set[%d]:%s\n\n", se, setx[se].name);
      setx[se].elem[setx[se].anz_e]= el;
      setx[se].anz_e++;
    }while(1); 
  }
  else if((compare(elemtyp,"DS3",3)==3)||(compare(elemtyp,"M3D3",4)==4)||
          (compare(elemtyp,"S3",2)==2)||(compare(elemtyp,"SFM3D3",6)==6)||
          (compare(elemtyp,"CPE3",4)==4)||(compare(elemtyp,"CPS3",4)==4)||
          (compare(elemtyp,"CAX3",4)==4)||(compare(elemtyp,"STRI3",5)==5))
  {
    if(compare(elemtyp,"CPE",3)==3) cbuf='E';
    else if(compare(elemtyp,"CPS",3)==3) cbuf='S';
    else if(compare(elemtyp,"CAX",3)==3) cbuf='C';
    else cbuf=0;
    se=pre_setax(setname, "i", 0);
    do{
      if(getAbqElement(rec_str, 3, &el, nd) <0)
      {
        if(setFlag)
	{
          qsort( setx[setNr].elem, setx[setNr].anz_e, sizeof(int), (void *)compareInt );
          /* erase multiple entities */
          n=0;
          for(i=1; i<setx[setNr].anz_e; i++)
          {
  	  if(setx[setNr].elem[n]!=setx[setNr].elem[i]) setx[setNr].elem[++n]=setx[setNr].elem[i];
          }
          if(setx[setNr].anz_e) setx[setNr].anz_e=n+1;
	}
        qsort( setx[se].elem, setx[se].anz_e, sizeof(int), (void *)compareInt );
        /* erase multiple entities */
        n=0;
        for(i=1; i<setx[se].anz_e; i++)
        {
	  if(setx[se].elem[n]!=setx[se].elem[i]) setx[se].elem[++n]=setx[se].elem[i];
        }
        if(setx[se].anz_e) setx[se].anz_e=n+1;
        break;
      }

      writeElem(el, 7, 7, 1, nd, cbuf);

      /* write set */
      if(setFlag)
      {
        if ( (setx[setNr].elem = (int *)realloc((int *)setx[setNr].elem, (setx[setNr].anz_e+1) * sizeof(int))) == NULL )
          printf(" ERROR: malloc failed in set[%d]:%s\n\n", setNr, setx[setNr].name);
        setx[setNr].elem[setx[setNr].anz_e]= el;
        setx[setNr].anz_e++;
      }
      if ( (setx[se].elem = (int *)realloc((int *)setx[se].elem, (setx[se].anz_e+1) * sizeof(int))) == NULL )
        printf(" ERROR: malloc failed in set[%d]:%s\n\n", se, setx[se].name);
      setx[se].elem[setx[se].anz_e]= el;
      setx[se].anz_e++;
    }while(1); 
  }
  else if((compare(elemtyp,"S6",2)==2)||(compare(elemtyp,"DS6",3)==3)||
          (compare(elemtyp,"CPE6",4)==4)||(compare(elemtyp,"CPS6",4)==4)||
          (compare(elemtyp,"CAX6",4)==4)||
          (compare(elemtyp,"M3D6",4)==4)||(compare(elemtyp,"SC6",3)==3)||
          (compare(elemtyp,"SFM3D6",6)==6)||(compare(elemtyp,"STRI65",6)==6))
  {
    if(compare(elemtyp,"CPE",3)==3) cbuf='E';
    else if(compare(elemtyp,"CPS",3)==3) cbuf='S';
    else if(compare(elemtyp,"CAX",3)==3) cbuf='C';
    else cbuf=0;
    se=pre_setax(setname, "i", 0);
    do{
      if(getAbqElement(rec_str, 6, &el, nd) <0)
      {
        if(setFlag)
	{
          qsort( setx[setNr].elem, setx[setNr].anz_e, sizeof(int), (void *)compareInt );
          /* erase multiple entities */
          n=0;
          for(i=1; i<setx[setNr].anz_e; i++)
          {
  	  if(setx[setNr].elem[n]!=setx[setNr].elem[i]) setx[setNr].elem[++n]=setx[setNr].elem[i];
          }
          if(setx[setNr].anz_e) setx[setNr].anz_e=n+1;
	}
        qsort( setx[se].elem, setx[se].anz_e, sizeof(int), (void *)compareInt );
        /* erase multiple entities */
        n=0;
        for(i=1; i<setx[se].anz_e; i++)
        {
	  if(setx[se].elem[n]!=setx[se].elem[i]) setx[se].elem[++n]=setx[se].elem[i];
        }
        if(setx[se].anz_e) setx[se].anz_e=n+1;
        break;
      }

      writeElem(el, 8, 8, 1, nd, cbuf);

      /* write set */
      if(setFlag)
      {
        if ( (setx[setNr].elem = (int *)realloc((int *)setx[setNr].elem, (setx[setNr].anz_e+1) * sizeof(int))) == NULL )
          printf(" ERROR: malloc failed in set[%d]:%s\n\n", setNr, setx[setNr].name);
        setx[setNr].elem[setx[setNr].anz_e]= el;
        setx[setNr].anz_e++;
      }
      if ( (setx[se].elem = (int *)realloc((int *)setx[se].elem, (setx[se].anz_e+1) * sizeof(int))) == NULL )
        printf(" ERROR: malloc failed in set[%d]:%s\n\n", se, setx[se].name);
      setx[se].elem[setx[se].anz_e]= el;
      setx[se].anz_e++;
    }while(1); 
  }
  else if((compare(elemtyp,"DS4",3)==3)||(compare(elemtyp,"M3D4",4)==4)||
          (compare(elemtyp,"SC4",3)==3)||(compare(elemtyp,"CAX4",4)==4)||
          (compare(elemtyp,"CPE4",4)==4)||(compare(elemtyp,"CPS4",4)==4)||
          (compare(elemtyp,"S4",2)==2)||(compare(elemtyp,"SFM3D4",6)==6))
  {
    if(compare(elemtyp,"CPE",3)==3) cbuf='E';
    else if(compare(elemtyp,"CPS",3)==3) cbuf='S';
    else if(compare(elemtyp,"CAX",3)==3) cbuf='C';
    else cbuf=0;
    se=pre_setax(setname, "i", 0);
    do{
      if(getAbqElement(rec_str, 4, &el, nd) <0)
      {
        if(setFlag)
	{
          qsort( setx[setNr].elem, setx[setNr].anz_e, sizeof(int), (void *)compareInt );
          /* erase multiple entities */
          n=0;
          for(i=1; i<setx[setNr].anz_e; i++)
          {
  	  if(setx[setNr].elem[n]!=setx[setNr].elem[i]) setx[setNr].elem[++n]=setx[setNr].elem[i];
          }
          if(setx[setNr].anz_e) setx[setNr].anz_e=n+1;
	}
        qsort( setx[se].elem, setx[se].anz_e, sizeof(int), (void *)compareInt );
        /* erase multiple entities */
        n=0;
        for(i=1; i<setx[se].anz_e; i++)
        {
	  if(setx[se].elem[n]!=setx[se].elem[i]) setx[se].elem[++n]=setx[se].elem[i];
        }
        if(setx[se].anz_e) setx[se].anz_e=n+1;
        break;
      }

      writeElem(el, 9, 9, 1, nd, cbuf);

      /* write set */
      if(setFlag)
      {
        if ( (setx[setNr].elem = (int *)realloc((int *)setx[setNr].elem, (setx[setNr].anz_e+1) * sizeof(int))) == NULL )
          printf(" ERROR: malloc failed in set[%d]:%s\n\n", setNr, setx[setNr].name);
        setx[setNr].elem[setx[setNr].anz_e]= el;
        setx[setNr].anz_e++;
      }
      if ( (setx[se].elem = (int *)realloc((int *)setx[se].elem, (setx[se].anz_e+1) * sizeof(int))) == NULL )
        printf(" ERROR: malloc failed in set[%d]:%s\n\n", se, setx[se].name);
      setx[se].elem[setx[se].anz_e]= el;
      setx[se].anz_e++;
    }while(1); 
  }
  else if((compare(elemtyp,"M3D8",4)==4)||(compare(elemtyp,"S8",2)==2)||
          (compare(elemtyp,"DS8",3)==3)||(compare(elemtyp,"CAX8",4)==4)||
          (compare(elemtyp,"CPE8",4)==4)||(compare(elemtyp,"CPS8",4)==4)||
          (compare(elemtyp,"SC8",3)==3)||(compare(elemtyp,"SFM3D8",6)==6)||
          (compare(elemtyp,"S8R",3)==3)||(compare(elemtyp,"S8R5",4)==4))
  {
    if(compare(elemtyp,"CPE",3)==3) cbuf='E';
    else if(compare(elemtyp,"CPS",3)==3) cbuf='S';
    else if(compare(elemtyp,"CAX",3)==3) cbuf='C';
    else cbuf=0;
    se=pre_setax(setname, "i", 0);
    do{
      if(getAbqElement(rec_str, 8, &el, nd) <0)
      {
        if(setFlag)
	{
          qsort( setx[setNr].elem, setx[setNr].anz_e, sizeof(int), (void *)compareInt );
          /* erase multiple entities */
          n=0;
          for(i=1; i<setx[setNr].anz_e; i++)
          {
  	  if(setx[setNr].elem[n]!=setx[setNr].elem[i]) setx[setNr].elem[++n]=setx[setNr].elem[i];
          }
          if(setx[setNr].anz_e) setx[setNr].anz_e=n+1;
	}
        qsort( setx[se].elem, setx[se].anz_e, sizeof(int), (void *)compareInt );
        /* erase multiple entities */
        n=0;
        for(i=1; i<setx[se].anz_e; i++)
        {
	  if(setx[se].elem[n]!=setx[se].elem[i]) setx[se].elem[++n]=setx[se].elem[i];
        }
        if(setx[se].anz_e) setx[se].anz_e=n+1;
        break;
      }

      writeElem(el, 10, 10, 1, nd, cbuf);

      /* write set */
      if(setFlag)
      {
        if ( (setx[setNr].elem = (int *)realloc((int *)setx[setNr].elem, (setx[setNr].anz_e+1) * sizeof(int))) == NULL )
          printf(" ERROR: malloc failed in set[%d]:%s\n\n", setNr, setx[setNr].name);
        setx[setNr].elem[setx[setNr].anz_e]= el;
        setx[setNr].anz_e++;
      }
      if ( (setx[se].elem = (int *)realloc((int *)setx[se].elem, (setx[se].anz_e+1) * sizeof(int))) == NULL )
        printf(" ERROR: malloc failed in set[%d]:%s\n\n", se, setx[se].name);
      setx[se].elem[setx[se].anz_e]= el;
      setx[se].anz_e++;
    }while(1); 
  }
  else if((compare(elemtyp,"B21",3)==3)||(compare(elemtyp,"B31",3)==3)||(compare(elemtyp,"T3D2",4)==4)||
          (compare(elemtyp,"DASHPOT",7)==7)||(compare(elemtyp,"SPRING",6)==6)||(compare(elemtyp,"GAPUNI",6)==6))
  {
    if(compare(elemtyp,"DASHPOT",3)==3) cbuf='D';
    else cbuf=0;
    se=pre_setax(setname, "i", 0);
    do{
      if(getAbqElement(rec_str, 2, &el, nd) <0)
      {
        if(setFlag)
	{
          qsort( setx[setNr].elem, setx[setNr].anz_e, sizeof(int), (void *)compareInt );
          /* erase multiple entities */
          n=0;
          for(i=1; i<setx[setNr].anz_e; i++)
          {
  	  if(setx[setNr].elem[n]!=setx[setNr].elem[i]) setx[setNr].elem[++n]=setx[setNr].elem[i];
          }
          if(setx[setNr].anz_e) setx[setNr].anz_e=n+1;
	}
        qsort( setx[se].elem, setx[se].anz_e, sizeof(int), (void *)compareInt );
        /* erase multiple entities */
        n=0;
        for(i=1; i<setx[se].anz_e; i++)
        {
	  if(setx[se].elem[n]!=setx[se].elem[i]) setx[se].elem[++n]=setx[se].elem[i];
        }
        if(setx[se].anz_e) setx[se].anz_e=n+1;
        break;
      }

      writeElem(el, 11, 11, 1, nd, 0);

      /* write set */
      if(setFlag)
      {
        if ( (setx[setNr].elem = (int *)realloc((int *)setx[setNr].elem, (setx[setNr].anz_e+1) * sizeof(int))) == NULL )
          printf(" ERROR: malloc failed in set[%d]:%s\n\n", setNr, setx[setNr].name);
        setx[setNr].elem[setx[setNr].anz_e]= el;
        setx[setNr].anz_e++;
      }
      if ( (setx[se].elem = (int *)realloc((int *)setx[se].elem, (setx[se].anz_e+1) * sizeof(int))) == NULL )
        printf(" ERROR: malloc failed in set[%d]:%s\n\n", se, setx[se].name);
      setx[se].elem[setx[se].anz_e]= el;
      setx[se].anz_e++;
    }while(1); 
  }
  else if((compare(elemtyp,"B22",3)==3)||(compare(elemtyp,"B32",3)==3)||
          (compare(elemtyp,"D",1)==strlen(elemtyp)))
  {
    se=pre_setax(setname, "i", 0);
    do{
      if(getAbqElement(rec_str, 3, &el, nd) <0)
      {
        if(setFlag)
	{
          qsort( setx[setNr].elem, setx[setNr].anz_e, sizeof(int), (void *)compareInt );
          /* erase multiple entities */
          n=0;
          for(i=1; i<setx[setNr].anz_e; i++)
          {
  	  if(setx[setNr].elem[n]!=setx[setNr].elem[i]) setx[setNr].elem[++n]=setx[setNr].elem[i];
          }
          if(setx[setNr].anz_e) setx[setNr].anz_e=n+1;
	}
        qsort( setx[se].elem, setx[se].anz_e, sizeof(int), (void *)compareInt );
        /* erase multiple entities */
        n=0;
        for(i=1; i<setx[se].anz_e; i++)
        {
	  if(setx[se].elem[n]!=setx[se].elem[i]) setx[se].elem[++n]=setx[se].elem[i];
        }
        if(setx[se].anz_e) setx[se].anz_e=n+1;
        break;
      }
      i=nd[1];
      nd[1]=nd[2];
      nd[2]=i;

      writeElem(el, 12, 12, 1, nd, 0);

      /* write set */
      if(setFlag)
      {
        if ( (setx[setNr].elem = (int *)realloc((int *)setx[setNr].elem, (setx[setNr].anz_e+1) * sizeof(int))) == NULL )
          printf(" ERROR: malloc failed in set[%d]:%s\n\n", setNr, setx[setNr].name);
        setx[setNr].elem[setx[setNr].anz_e]= el;
        setx[setNr].anz_e++;
      }
      if ( (setx[se].elem = (int *)realloc((int *)setx[se].elem, (setx[se].anz_e+1) * sizeof(int))) == NULL )
        printf(" ERROR: malloc failed in set[%d]:%s\n\n", se, setx[se].name);
      setx[se].elem[setx[se].anz_e]= el;
      setx[se].anz_e++;
    }while(1); 
  }
  else    
  {
    i=abqrecord(rec_str);
    if(i==-1) return(-1);
  }
  return(1);
}

    
int getElements(char *rec_str)
{
  int i,j,n, args, setFlag=0, setNr=0;
  char set[MAX_LINE_LENGTH], elemtyp[MAX_LINE_LENGTH], setname[MAX_LINE_LENGTH], cbuf;
  int el, se, nd[40];
  int eattr=0;  


  /* get the arguments of the string */
  args=crecord(rec_str, dat);
  
  /* extract element-type and setname */
  for(i=0; i<args; i++)
  {
    /* delete blanks */
    n=0; for(j=0;j<strlen(dat[i]); j++) if(dat[i][j]!=' ') { buffer[n]=dat[i][j]; n++; }
    buffer[n]='\0';
    for(j=0;j<=strlen(buffer); j++) dat[i][j]=toupper(buffer[j]);
    if(compare(dat[i],"TYPE=",5)==5) strcpy(elemtyp,&dat[i][5]);
    if(compare(dat[i],"ELSET=",6)==6) 
    {
      if(strlen(&buffer[6])>0) { strcpy(set,&buffer[6]); setFlag=1; }
    } 
  }
  if(printFlag) printf("*ELEMENT, TYPE=%s ,ELSET=%s\n", elemtyp, set);

  /* write to file */
  if(setFlag)
  { 
    setNr=getSetNrx(set);
    if(setNr<0) setNr=pre_setax( set, "i", 0);
  }
  sprintf(setname,"+%s", elemtyp);

  /* quickfix: conversion from fluit type into structural */
  if(elemtyp[0]=='F') { elemtyp[0]='C'; eattr='F'; }

  if(compare(elemtyp,"C3D8",4)==4)
  {
    se=pre_setax(setname, "i", 0);
    do{
      if(getAbqElement(rec_str, 8, &el, nd) <0) break;

      if(eattr=='F') writeElem(el, 1, 1, 1, nd, eattr );
      else writeElem(el, 1, 1, 1, nd, elemtyp[4]);

      /* write set */
      if(setFlag) setax( setNr, "e", el);
      setax( se, "e", el);
    }while(1); 
  }
  else if(compare(elemtyp,"C3D20",5)==5)
  {
    se=pre_setax(setname, "i", 0);
    do{
      if(getAbqElement(rec_str, 20, &el, nd) <0) break;
      for(i=0; i<12; i++) nd[20+i]=nd[i];
      for(i=0; i<4; i++) nd[32+i]=nd[i+16];
      for(i=0; i<4; i++) nd[36+i]=nd[i+12];

      writeElem(el, 4, 4, 1, &nd[20], elemtyp[5]);

      /* write set */
      if(setFlag) setax( setNr, "e", el);
      setax( se, "e", el);
    }while(1); 
  }
  else if(compare(elemtyp,"C3D4",4)==4)
  {
    se=pre_setax(setname, "i", 0);
    do{
      if(getAbqElement(rec_str, 4, &el, nd) <0) break;

      writeElem(el, 3, 3, 1, nd, elemtyp[4]);

      /* write set */
      if(setFlag) setax( setNr, "e", el);
      setax( se, "e", el);
    }while(1); 
  }
  else if(compare(elemtyp,"C3D6",5)==5)
  {
    se=pre_setax(setname, "i", 0);
    do{
      if(getAbqElement(rec_str, 6, &el, nd) <0) break;

      writeElem(el, 2, 2, 1, nd, elemtyp[4]);

      /* write set */
      if(setFlag) setax( setNr, "e", el);
      setax( se, "e", el);
    }while(1); 
  }
  else if(compare(elemtyp,"C3D10",5)==5)
  {
    se=pre_setax(setname, "i", 0);
    do{
      if(getAbqElement(rec_str, 10, &el, nd) <0) break;

      writeElem(el, 6, 6, 1, nd, elemtyp[5]);

      /* write set */
      if(setFlag) setax( setNr, "e", el);
      setax( se, "e", el);
    }while(1); 
  }
  else if(compare(elemtyp,"C3D15",5)==5)
  {
    se=pre_setax(setname, "i", 0);
    do{
      if(getAbqElement(rec_str, 15, &el, nd) <0) break;
      for(i=0; i<9; i++) nd[20+i]=nd[i];
      for(i=0; i<3; i++) nd[29+i]=nd[i+12];
      for(i=0; i<3; i++) nd[32+i]=nd[i+9];

      writeElem(el, 5, 5, 1, &nd[20], elemtyp[5]);

      /* write set */
      if(setFlag) setax( setNr, "e", el);
      setax( se, "e", el);
    }while(1); 
  }
  else if((compare(elemtyp,"DS3",3)==3)||(compare(elemtyp,"M3D3",4)==4)||
          (compare(elemtyp,"S3",2)==2)||(compare(elemtyp,"SFM3D3",6)==6)||
          (compare(elemtyp,"CPE3",4)==4)||(compare(elemtyp,"CPS3",4)==4)||
          (compare(elemtyp,"CAX3",4)==4)||(compare(elemtyp,"STRI3",5)==5))
  {
    if(compare(elemtyp,"CPE",3)==3) cbuf='E';
    else if(compare(elemtyp,"CPS",3)==3) cbuf='S';
    else if(compare(elemtyp,"CAX",3)==3) cbuf='C';
    else cbuf=0;
    se=pre_setax(setname, "i", 0);
    do{
      if(getAbqElement(rec_str, 3, &el, nd) <0) break;

      writeElem(el, 7, 7, 1, nd, cbuf);

      /* write set */
      if(setFlag) setax( setNr, "e", el);
      setax( se, "e", el);
    }while(1); 
  }
  else if((compare(elemtyp,"S6",2)==2)||(compare(elemtyp,"DS6",3)==3)||
          (compare(elemtyp,"CPE6",4)==4)||(compare(elemtyp,"CPS6",4)==4)||
          (compare(elemtyp,"CAX6",4)==4)||
          (compare(elemtyp,"M3D6",4)==4)||(compare(elemtyp,"SC6",3)==3)||
          (compare(elemtyp,"SFM3D6",6)==6)||(compare(elemtyp,"STRI65",6)==6))
  {
    if(compare(elemtyp,"CPE",3)==3) cbuf='E';
    else if(compare(elemtyp,"CPS",3)==3) cbuf='S';
    else if(compare(elemtyp,"CAX",3)==3) cbuf='C';
    else cbuf=0;
    se=pre_setax(setname, "i", 0);
    do{
      if(getAbqElement(rec_str, 6, &el, nd) <0) break;

      writeElem(el, 8, 8, 1, nd, cbuf);

      /* write set */
      if(setFlag) setax( setNr, "e", el);
      setax( se, "e", el);
    }while(1); 
  }
  else if((compare(elemtyp,"DS4",3)==3)||(compare(elemtyp,"M3D4",4)==4)||
          (compare(elemtyp,"SC4",3)==3)||(compare(elemtyp,"CAX4",4)==4)||
          (compare(elemtyp,"CPE4",4)==4)||(compare(elemtyp,"CPS4",4)==4)||
          (compare(elemtyp,"S4",2)==2)||(compare(elemtyp,"SFM3D4",6)==6))
  {
    if(compare(elemtyp,"CPE",3)==3) cbuf='E';
    else if(compare(elemtyp,"CPS",3)==3) cbuf='S';
    else if(compare(elemtyp,"CAX",3)==3) cbuf='C';
    else cbuf=0;
    se=pre_setax(setname, "i", 0);
    do{
      if(getAbqElement(rec_str, 4, &el, nd) <0) break;

      writeElem(el, 9, 9, 1, nd, cbuf);

      /* write set */
      if(setFlag) setax( setNr, "e", el);
      setax( se, "e", el);
    }while(1); 
  }
  else if((compare(elemtyp,"M3D8",4)==4)||(compare(elemtyp,"S8",2)==2)||
          (compare(elemtyp,"DS8",3)==3)||(compare(elemtyp,"CAX8",4)==4)||
          (compare(elemtyp,"CPE8",4)==4)||(compare(elemtyp,"CPS8",4)==4)||
          (compare(elemtyp,"SC8",3)==3)||(compare(elemtyp,"SFM3D8",6)==6)||
          (compare(elemtyp,"S8R",3)==3)||(compare(elemtyp,"S8R5",4)==4))
  {
    if(compare(elemtyp,"CPE",3)==3) cbuf='E';
    else if(compare(elemtyp,"CPS",3)==3) cbuf='S';
    else if(compare(elemtyp,"CAX",3)==3) cbuf='C';
    else cbuf=0;
    se=pre_setax(setname, "i", 0);
    do{
      if(getAbqElement(rec_str, 8, &el, nd) <0) break;

      writeElem(el, 10, 10, 1, nd, cbuf);

      /* write set */
      if(setFlag) setax( setNr, "e", el);
      setax( se, "e", el);
    }while(1); 
  }
  else if((compare(elemtyp,"B21",3)==3)||(compare(elemtyp,"B31",3)==3)||(compare(elemtyp,"T3D2",4)==4)||
          (compare(elemtyp,"DASHPOT",7)==7)||(compare(elemtyp,"SPRING",6)==6)||(compare(elemtyp,"GAPUNI",6)==6))
  {
    if(compare(elemtyp,"DASHPOT",3)==3) cbuf='D';
    else cbuf=0;
    se=pre_setax(setname, "i", 0);
    do{
      if(getAbqElement(rec_str, 2, &el, nd) <0) break;

      writeElem(el, 11, 11, 1, nd, 0);

      /* write set */
      if(setFlag) setax( setNr, "e", el);
      setax( se, "e", el);
    }while(1); 
  }
  else if((compare(elemtyp,"B22",3)==3)||(compare(elemtyp,"B32",3)==3)||
          (compare(elemtyp,"D",1)==strlen(elemtyp)))
  {
    se=pre_setax(setname, "i", 0);
    do{
      if(getAbqElement(rec_str, 3, &el, nd) <0) break;
      /* check if its an inflow or out-flow element */
      if(!nd[0])
        writeElem(el, 11, 11, 1, &nd[1], 0);
      else if(!nd[2])
        writeElem(el, 11, 11, 1, nd, 0);
      else 
      {
        i=nd[1];
        nd[1]=nd[2];
        nd[2]=i;
        writeElem(el, 12, 12, 1, nd, 0);
      }
      /* write set */
      if(setFlag) setax( setNr, "e", el);
      setax( se, "e", el);
    }while(1); 
  }
  else    
  {
    i=abqrecord(rec_str);
    if(i==-1) return(-1);
  }
  return(1);
}



int getAmplitude(char *rec_str)
{
  int i,j,n, args, ampnr;
  char aname[MAX_LINE_LENGTH];


  /* get the arguments of the string */
  args=crecord(rec_str, dat);
  
  /* extract the material-name */
  for(i=0; i<args; i++)
  {
    /* delete blanks */
    n=0; for(j=0;j<strlen(dat[i]); j++) if(dat[i][j]!=' ') { buffer[n]=dat[i][j]; n++; }
    buffer[n]='\0';
    for(j=0;j<=strlen(buffer); j++) dat[i][j]=toupper(buffer[j]);
    if(compare(dat[i],"NAME=",5)==5) strcpy(aname,&buffer[5]);
  }
  if(printFlag) printf("*AMPLITUDE, NAME=%s\n", aname);

  /* get or define an amplitude-number */
  ampnr=getAmplitudeNr(aname,0);

  
  do{
    i=abqrecord(rec_str);

    if(i==-1) return(-1);
    if(i==-2) break;

    /* get the arguments of the string */
    args=crecord(rec_str, dat);

    /* screen dump */
    if(printFlag)
    {
      for(i=0; i<args; i++) printf("%s ",dat[i]);
      printf(" (%d)\n",i);
    }

    /* extract x,y pairs */
    for(i=0; i<args-1; i+=2) 
    {
      if ((amplitude[ampnr].x = (double *)realloc( (double *)amplitude[ampnr].x, (amplitude[ampnr].n+2)*sizeof(double)) ) == NULL )
      {
        printf(" ERROR: realloc failure in getAmplitude\n\n");
        return(-1);
      }
      if ((amplitude[ampnr].y = (double *)realloc( (double *)amplitude[ampnr].y, (amplitude[ampnr].n+2)*sizeof(double)) ) == NULL )
      {
        printf(" ERROR: realloc failure in getAmplitude\n\n");
        return(-1);
      }
      amplitude[ampnr].x[amplitude[ampnr].n]=atof(dat[i]);
      amplitude[ampnr].y[amplitude[ampnr].n]=atof(dat[i+1]);
      amplitude[ampnr].n++;
    }
  }while(args>0);

  if(printFlag) for (i=0; i<amplitude[ampnr].n; i++)
    printf("%d %lf %lf\n",i,amplitude[ampnr].x[i],amplitude[ampnr].y[i] );
  return(1);
}


int getMaterial(char *rec_str)
{
  int i,j,n, args;
  char mname[MAX_LINE_LENGTH];
 

  /* get the arguments of the string */
  args=crecord(rec_str, dat);
  
  /* extract the material-name */
  for(i=0; i<args; i++)
  {
    /* delete blanks */
    n=0; for(j=0;j<strlen(dat[i]); j++) if(dat[i][j]!=' ') { buffer[n]=dat[i][j]; n++; }
    buffer[n]='\0';
    for(j=0;j<=strlen(buffer); j++) dat[i][j]=toupper(buffer[j]);
    if(compare(dat[i],"NAME=",5)==5) strcpy(mname,&buffer[5]);
  }
  if(printFlag) printf("*MATERIAL, NAME=%s\n", mname);

  /* get the next line */
  i=abqrecord(rec_str);
  if(i==-1) return(-1);

  /* get or define a material-number */
  matnr=getMatNr(mname,0);

  if(ansFlag) 
    fprintf( handleans, "! ----  MATERIAL: %s, NR: %d -----\n",mname, matnr);
  return(1);
}


int getElastic(char *rec_str)
{
  int i,j,n, args,length;
  static char **dat=NULL;
  char type[MAX_LINE_LENGTH];
  double E,nue,t;

  if((dat = (char **)realloc((char **)dat, (80)*sizeof(char*)))==NULL)
    errMsg("\n\n ERROR: realloc failed for **dat\n" );
    for (i=0; i<80; i++)
    {
      dat[i]=NULL;
      if((dat[i] = (char *)realloc((char *)dat[i], (MAX_LINE_LENGTH)*sizeof(char)))==NULL)
        errMsg("\n\n ERROR: realloc failed for *dat\n" );
    }

  /* get the arguments of the string */
  args=crecord(rec_str, dat);
  
  /* extract the type */
  strcpy(type,"ISO"); /*default */
  for(i=0; i<args; i++)
  {
    /* delete blanks */
    n=0; for(j=0;j<strlen(dat[i]); j++) if(dat[i][j]!=' ') { buffer[n]=dat[i][j]; n++; }
    buffer[n]='\0';
    for(j=0;j<=strlen(buffer); j++) dat[i][j]=toupper(buffer[j]);
    if(compare(dat[i],"TYPE=",5)==5) strcpy(type,&dat[i][5]);
  }
  if(printFlag) printf("*ELASTIC, TYPE=%s\n", type);

  /* scan the properties and write to file */
  if(compare(type,"ISO",3)==3)
  {
    i=0;
    do{
      length=abqrecord(rec_str);
      if(length==-1) return(-1);
      if(length==-2) break;
      length=sscanf ( rec_str, "%lf, %lf, %lf", &E,&nue,&t);
      if(printFlag) printf("temp:%lf E:%lf nue:%lf\n",t,E,nue); 

      if ((material[matnr].tela = (double *)realloc( (double *)material[matnr].tela, (material[matnr].nela+2)*sizeof(double)) ) == NULL )
      {
        printf(" ERROR: realloc failure in getElastic\n\n");
        return(-1);
      }
      if ((material[matnr].ela = (double *)realloc( (double *)material[matnr].ela, (material[matnr].nela+2)*sizeof(double)) ) == NULL )
      {
        printf(" ERROR: realloc failure in getElastic\n\n");
        return(-1);
      }
      if ((material[matnr].nue = (double *)realloc( (double *)material[matnr].nue, (material[matnr].nela+2)*sizeof(double)) ) == NULL )
      {
        printf(" ERROR: realloc failure in getElastic\n\n");
        return(-1);
      }
      material[matnr].tela[material[matnr].nela]=t;
      material[matnr].ela[material[matnr].nela]=E;
      material[matnr].nue[material[matnr].nela]=nue;
      material[matnr].nela++;

      if(ansFlag) 
      {
        i++;
        /* write in ansys format */
        fprintf(handleans, "MPTEMP, %d, %lf,\n",i, t);
        fprintf(handleans, "MPDATA, EX, %d, %d, %e,\n",matnr, i, E);
        fprintf(handleans, "MPDATA, NUXY, %d, %d, %lf,\n",matnr, i, nue);
      }
    }while(1); 
  }
  else
  {
    errMsg(" WARNING: TYPE:%s not known\n",type);
      length=abqrecord(rec_str);
      if(length==-1) return(-1);
  }
  return(1);
}


int getExpansion(char *rec_str)
{
  int i,j,n, args,length;
  char type[MAX_LINE_LENGTH];
  double a,t;


  /* get the arguments of the string */
  args=crecord(rec_str, dat);
  
  /* extract the type */
  strcpy(type,"ISO"); /*default */
  for(i=0; i<args; i++)
  {
    /* delete blanks */
    n=0; for(j=0;j<strlen(dat[i]); j++) if(dat[i][j]!=' ') { buffer[n]=dat[i][j]; n++; }
    buffer[n]='\0';
    for(j=0;j<=strlen(buffer); j++) dat[i][j]=toupper(buffer[j]);
    if(compare(dat[i],"TYPE=",5)==5) strcpy(type,&dat[i][5]);
  }
  if(printFlag) printf("*EXPANSION, TYPE=%s\n", type);

  /* scan the properties and write to file */
  if(compare(type,"ISO",3)==3)
  {
    i=0;
    do{
      length=abqrecord(rec_str);
      if(length==-1) return(-1);
      if(length==-2) break;
      length=sscanf ( rec_str, "%lf, %lf", &a,&t);
      if(printFlag) printf("temp:%lf alpha:%lf\n",t,a); 

      if ((material[matnr].texp = (double *)realloc( (double *)material[matnr].texp, (material[matnr].nexp+2)*sizeof(double)) ) == NULL )
      {
        printf(" ERROR: realloc failure in getElastic\n\n");
        return(-1);
      }
      if ((material[matnr].exp = (double *)realloc( (double *)material[matnr].exp, (material[matnr].nexp+2)*sizeof(double)) ) == NULL )
      {
        printf(" ERROR: realloc failure in getElastic\n\n");
        return(-1);
      }
      material[matnr].texp[material[matnr].nexp]=t;
      material[matnr].exp[material[matnr].nexp]=a;
      material[matnr].nexp++;

      if(ansFlag) 
      {
        i++;
        /* write in ansys format */
        fprintf(handleans, "MPTEMP, %d, %lf,\n",i, t);
        fprintf(handleans, "MPDATA, ALPX, %d, %d, %e,\n",matnr, i, a);
      }
    }while(1); 
  }
  else
  {
    errMsg(" WARNING: TYPE:%s not known\n",type);
      length=abqrecord(rec_str);
      if(length==-1) return(-1);
  }
  return(1);
}


int getConductivity(char *rec_str)
{
  int i,j,n, args,length;
  char type[MAX_LINE_LENGTH];
  double a,t;


  /* get the arguments of the string */
  args=crecord(rec_str, dat);
  
  /* extract the type */
  strcpy(type,"ISO"); /*default */
  for(i=0; i<args; i++)
  {
    /* delete blanks */
    n=0; for(j=0;j<strlen(dat[i]); j++) if(dat[i][j]!=' ') { buffer[n]=dat[i][j]; n++; }
    buffer[n]='\0';
    for(j=0;j<=strlen(buffer); j++) dat[i][j]=toupper(buffer[j]);
    if(compare(dat[i],"TYPE=",5)==5) strcpy(type,&dat[i][5]);
  }
  if(printFlag) printf("*CONDUCTIVITY, TYPE=%s\n", type);

  /* scan the properties and write to file */
  if(compare(type,"ISO",3)==3)
  {
    i=0;
    do{
      length=abqrecord(rec_str);
      if(length==-1) return(-1);
      if(length==-2) break;
      length=sscanf ( rec_str, "%lf, %lf", &a,&t);
      if(printFlag) printf("temp:%lf w:%lf\n",t,a); 

      if ((material[matnr].tcon = (double *)realloc( (double *)material[matnr].tcon, (material[matnr].ncon+2)*sizeof(double)) ) == NULL )
      {
        printf(" ERROR: realloc failure in getConductivity\n\n");
        return(-1);
      }
      if ((material[matnr].con = (double *)realloc( (double *)material[matnr].con, (material[matnr].ncon+2)*sizeof(double)) ) == NULL )
      {
        printf(" ERROR: realloc failure in getConductivity\n\n");
        return(-1);
      }
      material[matnr].tcon[material[matnr].ncon]=t;
      material[matnr].con[material[matnr].ncon]=a;
      material[matnr].ncon++;

      if(ansFlag) 
      {
        i++;
        /* write in ansys format */
        printf("ERROR: Coductivity not jet supported. EXIT\n"); exit(1);
        fprintf(handleans, "MPTEMP, %d, %lf,\n",i, t);
        fprintf(handleans, "MPDATA, ALPX, %d, %d, %e,\n",matnr, i, a);
      }
    }while(1); 
  }
  else
  {
    errMsg(" WARNING: TYPE:%s not known\n",type);
      length=abqrecord(rec_str);
      if(length==-1) return(-1);
  }
  return(1);
}


int getSpecificHeat(char *rec_str)
{
  int i,j,n, args,length;
  char type[MAX_LINE_LENGTH];
  double a,t;


  /* get the arguments of the string */
  args=crecord(rec_str, dat);
  
  /* extract the type */
  strcpy(type,"ISO"); /*default */
  for(i=0; i<args; i++)
  {
    /* delete blanks */
    n=0; for(j=0;j<strlen(dat[i]); j++) if(dat[i][j]!=' ') { buffer[n]=dat[i][j]; n++; }
    buffer[n]='\0';
    for(j=0;j<=strlen(buffer); j++) dat[i][j]=toupper(buffer[j]);
    if(compare(dat[i],"TYPE=",5)==5) strcpy(type,&dat[i][5]);
  }
  if(printFlag) printf("*SPECIFIC HEAT, TYPE=%s\n", type);

  /* scan the properties and write to file */
  if(compare(type,"ISO",3)==3)
  {
    i=0;
    do{
      length=abqrecord(rec_str);
      if(length==-1) return(-1);
      if(length==-2) break;
      length=sscanf ( rec_str, "%lf, %lf", &a,&t);
      if(printFlag) printf("temp:%lf h:%lf\n",t,a); 

      if ((material[matnr].tsph = (double *)realloc( (double *)material[matnr].tsph, (material[matnr].nsph+2)*sizeof(double)) ) == NULL )
      {
        printf(" ERROR: realloc failure in getSpecificHeat\n\n");
        return(-1);
      }
      if ((material[matnr].sph = (double *)realloc( (double *)material[matnr].sph, (material[matnr].nsph+2)*sizeof(double)) ) == NULL )
      {
        printf(" ERROR: realloc failure in getSpecificHeat\n\n");
        return(-1);
      }
      material[matnr].tsph[material[matnr].nsph]=t;
      material[matnr].sph[material[matnr].nsph]=a;
      material[matnr].nsph++;

      if(ansFlag) 
      {
        i++;
        /* write in ansys format */
        printf("ERROR: SpecificHeat not jet supported. EXIT\n"); exit(1);
        fprintf(handleans, "MPTEMP, %d, %lf,\n",i, t);
        fprintf(handleans, "MPDATA, ALPX, %d, %d, %e,\n",matnr, i, a);
      }
    }while(1); 
  }
  else
  {
    errMsg(" WARNING: TYPE:%s not known\n",type);
      length=abqrecord(rec_str);
      if(length==-1) return(-1);
  }
  return(1);
}


int getDensity(char *rec_str)
{
  int length;
  double rho=0.,t=0.;

  /* scan the properties and write to file */
  do{
    length=abqrecord(rec_str);
    if(length==-1) return(-1);
    if(length==-2) break;
    length=sscanf ( rec_str, "%lf, %lf", &rho,&t);
    material[matnr].rho=rho;
    if(printFlag) printf("*DENSITY\n %e, %lf\n", material[matnr].rho, t);
  
    if(ansFlag) 
    {
      /* write in ansys format */
      fprintf(handleans, "MP,DENS, %d, %e,\n", matnr, rho);
    }
  }while(1); 
  return(1);
}


int calcMaxMinValues(int l)
{
  int i,j;
  for(i=0; i<lcasex[l].ncomps; i++)
  {
    lcasex[l].max[i]=-MAX_FLOAT;
    lcasex[l].min[i]=MAX_FLOAT;
    for(j=0; j<anzx->n; j++)
    {
      if (lcasex[l].dat[i][nodex[j].nr] > lcasex[l].max[i])
      {  lcasex[l].max[i]=lcasex[l].dat[i][nodex[j].nr]; lcasex[l].nmax[i]=nodex[j].nr; }
      if (lcasex[l].dat[i][nodex[j].nr] < lcasex[l].min[i])
      {  lcasex[l].min[i]=lcasex[l].dat[i][nodex[j].nr]; lcasex[l].nmin[i]=nodex[j].nr; }
    }
  }
  return(1);
}



int writeFaceVals(int anzx_l, int el, int face, int nr, float *val, char *string)
{
  int i,j, anzn, nface[26];
  char setName[MAX_LINE_LENGTH], nodeName[MAX_LINE_LENGTH];

  if(nr<1) return(0);

    if (el>anzx->emax)
    {
      printf("WARNING: found elementNr:%d in Dataset higher than in Geometry allocated:%d\n", el, anzx->emax);
      return(0);
    }

    /* convert from abaqus syntax to cgx syntax */
    if((e_enqirex[el].type>6)&&(e_enqirex[el].type<11))
    {
      if(e_enqirex[el].attr>3)
      {
        face++;
        if(e_enqirex[el].type<9)
        {
          if(face>4) face=1;
        }
        else
        {
          if(face>5) face=1;
        }
      }
      else
      {
        face--;
        if(face==0) face=1;
      }
    }
    anzn=getElemFaceNodes(e_enqirex, el,face,nface);
    for (i=0; i<anzn; i++)
    {
      /* load */
      
      for(j=0; j<nr; j++) lcasex[anzx_l].dat[j][nface[i]]+=val[j];
      /* number of loads per node */
      lcasex[anzx_l].dat[nr][nface[i]]++;

      /* add node to special set */
      if(string)
      {
        for(j=0; j<nr; j++)
        {
          sprintf( setName, "%s%d", string, (int)val[j]); 
          sprintf( nodeName, "%d", nface[i]); 
          pre_setax( setName, "n", nodeName);
        }
      }
    }

  return(1);
}



int getScalarvalue(char *rec_str, char *type)
{
  int i,j,n; 
  int length, nd, foundLoads=0, nset, comp;
  double t;

  if(printFlag) printf("%s\n",rec_str);
  /* scan the properties and write to file */

  if(compare(type,"TEMP",4)==4)
  {
    sprintf( lcasex[anzx->l].name,"TEMP");
  }
  else if(compare(type,"PRES",4)==4)
  {
    sprintf( lcasex[anzx->l].name,"PRES");
  }
  else if(compare(type,"TOTA",4)==4)
  {
    sprintf( lcasex[anzx->l].name,"TTEMP");
  }
  else if(compare(type,"MASS",4)==4)
  {
    sprintf( lcasex[anzx->l].name,"MASSFL");
  }
  else
  {
    printf("ERROR: In getScalarValue type:%s not known\n", type);
    return(-1);
  }
  printf (" create dataset:%d name= %s\n", anzx->l, lcasex[anzx->l].name);
  comp=0;
  lcasex[anzx->l].ncomps=1;
  lcasex[anzx->l].irtype=1;
  lcasex[anzx->l].npheader=0;

  if(anzx->l) lcasex[anzx->l].value=lcasex[anzx->l-1].value+dataset_value;
  else lcasex[anzx->l].value=dataset_value;
  //dataset_value=0.;

  strcpy(lcasex[anzx->l].analysis_name,"");
  sprintf(lcasex[anzx->l].dataset_name,"STP %d", step_nr);
  if(strlen(dataset_text))
  {
    strcpy(lcasex[anzx->l].dataset_text,"");
    //dataset_text[0]=0;
  }
  else strcpy(lcasex[anzx->l].dataset_text,dataset_text);
  lcasex[anzx->l].step_number=step_nr;
  lcasex[anzx->l].analysis_type=1;

  if ( (lcasex[anzx->l].nmax = (int *)malloc( lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );
  if ( (lcasex[anzx->l].nmin = (int *)malloc( lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );
  if ( (lcasex[anzx->l].max = (float *)malloc(lcasex[anzx->l].ncomps * sizeof(float))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );
  if ( (lcasex[anzx->l].min = (float *)malloc(lcasex[anzx->l].ncomps * sizeof(float))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );
  if ( (lcasex[anzx->l].dat = (float **)malloc(lcasex[anzx->l].ncomps * sizeof(float *))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );
  if ( (lcasex[anzx->l].compName = (char **)malloc(lcasex[anzx->l].ncomps * sizeof(char *))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );
  if ( (lcasex[anzx->l].icname = (char **)malloc(lcasex[anzx->l].ncomps * sizeof(char *))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );
  if ( (lcasex[anzx->l].menu = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );
  if ( (lcasex[anzx->l].ictype = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );
  if ( (lcasex[anzx->l].icind1 = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );
  if ( (lcasex[anzx->l].icind2 = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );
  if ( (lcasex[anzx->l].iexist = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );

  for(i=comp; i<lcasex[anzx->l].ncomps; i++)
  {
    if ( (lcasex[anzx->l].dat[i] = (float *)calloc( (anzx->nmax+1), sizeof(float))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );	               
    if ( (lcasex[anzx->l].compName[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
      printf("\n\n ERROR: malloc failed\n\n" );
    if ( (lcasex[anzx->l].icname[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
      printf("\n\n ERROR: malloc failed\n\n" );
    lcasex[anzx->l].max[i]=-MAX_FLOAT;
    lcasex[anzx->l].min[i]=MAX_FLOAT;

    lcasex[anzx->l].menu[i] = 1;
    lcasex[anzx->l].ictype[i] = 1;
    lcasex[anzx->l].icind1[i] = 0;
    lcasex[anzx->l].icind2[i] = 0;
    lcasex[anzx->l].iexist[i] = 0;
    lcasex[anzx->l].max[i]=-MAX_FLOAT;  lcasex[anzx->l].min[i]=MAX_FLOAT;
  }
  

  if(compare(type,"TEMP",4)==4)
  {
    strcpy ( lcasex[anzx->l].compName[0], "T  ");
  }
  else if(compare(type,"PRES",4)==4)
  {
    strcpy ( lcasex[anzx->l].compName[0], "P  ");
  }
  else if(compare(type,"TOTA",4)==4)
  {
    strcpy ( lcasex[anzx->l].compName[0], "TT");
  }
  else if(compare(type,"MASS",4)==4)
  {
    strcpy ( lcasex[anzx->l].compName[0], "M  ");
  }

  do{
    length=abqrecord(rec_str);
    if(length==-1)
    { 
      anzx->l++;
      if ( (lcasex = (Datasets *)realloc((Datasets *)lcasex, (anzx->l+2) * sizeof(Datasets))) == NULL )
      { printf("\n\n ERROR: malloc failure\n\n" ); exit(1); }
      return(-1);
    }
    if(length==-2) break;
    foundLoads=1;
 
    crecord(rec_str, dat);
    nd=atoi(dat[0]);
    t=atof(dat[1]);
    /* check if the node-number is 0, then a set was defined */
    if (nd==0)
    {
      nset=getSetNrx(dat[0]);
      if(nset<0)
      {
        printf("ERROR: In getScalarvalue type *%s set:%s not defined\n", type, dat[0]);
        return(-1);
      }
      for(j=0; j<setx[nset].anz_n; j++)
      {
        writeNodeVals(anzx->l, setx[nset].node[j], t, 0);
        if(ansFlag)
        {
          /* write in ansys format */
	  fprintf(handleans, "BF, %d, %s, %lf\n", setx[nset].node[j], type, t);
        }
      }
    }
    else
    {
      writeNodeVals(anzx->l, nd, t, 0);
      if(copyFlag)
      {
        /* write to equ-file */
        for (n=0; n<SEGMENTS; n++) {
          fprintf(handle_equ, "%d,%lf\n", nd+(n*NODE_OFFSET), t); }
      }
      if(ansFlag)
      {
        /* write in ansys format */
        fprintf(handleans, "BF, %d, %s, %lf\n", nd, type, t);
      }
    }
  }while(1); 

  if(foundLoads)
  {
    anzx->l++;
    if ( (lcasex = (Datasets *)realloc((Datasets *)lcasex, (anzx->l+2) * sizeof(Datasets))) == NULL )
    { printf("\n\n ERROR: malloc failure\n\n" ); exit(1); }
  }
  return(1);
}



int getInitialVector(char *rec_str, char *type)
{
  int i,j,n; 
  int length, nd, dof, foundLoads=0,nset=0;
  double f;

  if(printFlag) printf("%s\n",rec_str);
  /* scan the properties and write to file */

  /* ini lcasex */
  if(compare(type,"DISP",4)==4)
  {
    sprintf( lcasex[anzx->l].name,"DISP");
  }
  else if(compare(type,"FLUID",4)==4)
  {
    sprintf( lcasex[anzx->l].name,"VELO");
  }
  else
  {
    printf("ERROR: In getInitialVector type:%s not known\n", type);
    return(-1);
  }
  printf (" create dataset:%d name= %s\n", anzx->l, lcasex[anzx->l].name);
  lcasex[anzx->l].value=0;
  lcasex[anzx->l].ncomps=3;
  lcasex[anzx->l].irtype=1;
  lcasex[anzx->l].npheader=0;

  strcpy(lcasex[anzx->l].analysis_name,"");
  sprintf(lcasex[anzx->l].dataset_name,"STP %d", step_nr);
  strcpy(lcasex[anzx->l].dataset_text,"");
  lcasex[anzx->l].step_number=step_nr;
  lcasex[anzx->l].analysis_type=1;

  if ( (lcasex[anzx->l].nmax = (int *)malloc( lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );
  if ( (lcasex[anzx->l].nmin = (int *)malloc( lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );
  if ( (lcasex[anzx->l].max = (float *)malloc(lcasex[anzx->l].ncomps * sizeof(float))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );
  if ( (lcasex[anzx->l].min = (float *)malloc(lcasex[anzx->l].ncomps * sizeof(float))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );
  if ( (lcasex[anzx->l].dat = (float **)malloc(lcasex[anzx->l].ncomps * sizeof(float *))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );
  if ( (lcasex[anzx->l].compName = (char **)malloc(lcasex[anzx->l].ncomps * sizeof(char *))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );
  if ( (lcasex[anzx->l].icname = (char **)malloc(lcasex[anzx->l].ncomps * sizeof(char *))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );
  if ( (lcasex[anzx->l].menu = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );
  if ( (lcasex[anzx->l].ictype = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );
  if ( (lcasex[anzx->l].icind1 = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );
  if ( (lcasex[anzx->l].icind2 = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );
  if ( (lcasex[anzx->l].iexist = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
    printf("\n\n ERROR: malloc failure\n\n" );

  for(i=0; i<lcasex[anzx->l].ncomps; i++)
  {
    if ( (lcasex[anzx->l].dat[i] = (float *)calloc( (anzx->nmax+1), sizeof(float))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );	               
    if ( (lcasex[anzx->l].compName[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
      printf("\n\n ERROR: malloc failed\n\n" );
    if ( (lcasex[anzx->l].icname[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
      printf("\n\n ERROR: malloc failed\n\n" );
    lcasex[anzx->l].max[i]=-MAX_FLOAT;
    lcasex[anzx->l].min[i]=MAX_FLOAT;

    lcasex[anzx->l].menu[i] = 1;
    lcasex[anzx->l].ictype[i] = 2;
    lcasex[anzx->l].icind1[i] = i+1;
    lcasex[anzx->l].icind2[i] = 0;
    lcasex[anzx->l].iexist[i] = 0;
  }
  
  if(compare(type,"DISP",4)==4)
  {
    strcpy ( lcasex[anzx->l].compName[0], "dx ");
    strcpy ( lcasex[anzx->l].compName[1], "dy ");
    strcpy ( lcasex[anzx->l].compName[2], "dz ");
/*
    strcpy ( lcasex[anzx->l].compName[3], "rx ");
    strcpy ( lcasex[anzx->l].compName[4], "ry ");
    strcpy ( lcasex[anzx->l].compName[5], "rz ");
*/
  }
  if(compare(type,"FLUID",4)==4)
  {
    strcpy ( lcasex[anzx->l].compName[0], "vx ");
    strcpy ( lcasex[anzx->l].compName[1], "vy ");
    strcpy ( lcasex[anzx->l].compName[2], "vz ");
  }
  do{
    length=abqrecord(rec_str);
    if(length==-1)
    { 
      anzx->l++;
      if ( (lcasex = (Datasets *)realloc((Datasets *)lcasex, (anzx->l+2) * sizeof(Datasets))) == NULL )
      { printf("\n\n ERROR: malloc failure\n\n" ); exit(1); }
      return(-1);
    }
    if(length==-2) break;

    crecord(rec_str, dat);
    nd=atoi(dat[0]);
    dof=atoi(dat[1]);
    f=atof(dat[2]);

    if((dof<1)||(dof>3))
    {
      printf("ERROR: In getInitialVector dof:%d at:%s not supported\n", dof,dat[0]);
      continue;
    }
    
    /* check if the node-number is 0, then a set was defined */
    if (nd==0)
    {
      nset=getSetNrx(dat[0]);
      if(nset<0)
      {
        printf("ERROR: In getInitialVector set:%s not defined\n", dat[0]);
        return(-1);
      }
      foundLoads=1;
      for(j=0; j<setx[nset].anz_n; j++)
      {
        writeNodeVals(anzx->l, setx[nset].node[j], f, dof-1);
      }
    }
    else
    {
      foundLoads=1;
      writeNodeVals(anzx->l, nd, f, dof-1);
    }

    if(copyFlag)
    {
      /* write to equ-file */
      for (n=0; n<SEGMENTS; n++)
        fprintf(handle_equ, "%d, %d, %lf\n", nd+(n*NODE_OFFSET), dof, f);
    }
    /* write in ansys format */
    if(ansFlag)
    {
      // TBD
	;
    }
  }while(1); 
  if(foundLoads)
  {
    anzx->l++;
    if ( (lcasex = (Datasets *)realloc((Datasets *)lcasex, (anzx->l+2) * sizeof(Datasets))) == NULL )
    { printf("\n\n ERROR: malloc failure\n\n" ); exit(1); }
  }
  return(1);
}



int getCload(char *rec_str)
{
  int i,j,k,n; 
  int length, nd, dof, foundLoads=0,nset=0,args;
  double f;
  static int setNr[7]={0,0,0,0,0,0,0};
  int ncur=0;
  static int *nclo=NULL;
  double csab[6];
  static int sector;
  static char **dat=NULL;
  static int local_step_nr=0, anzx_l;

  /* reset all static variables */
  if(resetFlag)
  {
    for(i=0; i<7; i++) setNr[i]=0;
    sector=0;
    local_step_nr=0;
    anzx_l=0;
    return(0);
  }

  if(printFlag) printf("%s\n",rec_str);
  /* scan the properties and write to file */

  /* get the arguments of the string */
  if((dat = (char **)realloc((char **)dat, (80)*sizeof(char*)))==NULL)
    errMsg("\n\n ERROR: realloc failed for **dat\n" );
    for (i=0; i<80; i++)
    {
      dat[i]=NULL;
      if((dat[i] = (char *)realloc((char *)dat[i], (MAX_LINE_LENGTH)*sizeof(char)))==NULL)
        errMsg("\n\n ERROR: realloc failed for *dat\n" );
    }
  args=crecord(rec_str, dat);
  for(i=0; i<args; i++)
  {
    /* delete blanks */
    n=0; for(j=0;j<strlen(dat[i]); j++) if(dat[i][j]!=' ') { buffer[n]=dat[i][j]; n++; }
    buffer[n]='\0';
    for(j=0;j<=strlen(buffer); j++) dat[i][j]=toupper(buffer[j]);
    if(compare(dat[i],"SECTOR=",6)==6)
    {
      /* data for a new sector, create a new dataset */
      local_step_nr=0;
      sector=atoi(&dat[i][7]);
    }
  }

  /* create a dataset if a new step beginns */
  if(!step_flag) { printf("%s\n ERROR: load was defined outside a *STEP\n\n",rec_str); exit(0); }
  else if(step_nr>local_step_nr)
  {
    local_step_nr=step_nr;

    /* ini lcasex, one for forces, one for moments */
  
    /* forces */
    if(sector) sprintf( lcasex[anzx->l].name,"%s%d", CLO, sector);
    else sprintf( lcasex[anzx->l].name,"%s", CLO);
    printf (" create dataset:%d name= %s\n", anzx->l, lcasex[anzx->l].name);
    lcasex[anzx->l].value=0;
    lcasex[anzx->l].ncomps=3;
    lcasex[anzx->l].irtype=1;
    lcasex[anzx->l].npheader=0;
  
    strcpy(lcasex[anzx->l].analysis_name,"");
    sprintf(lcasex[anzx->l].dataset_name,"STP %d", step_nr);
    strcpy(lcasex[anzx->l].dataset_text,"");
    lcasex[anzx->l].step_number=step_nr;
    lcasex[anzx->l].analysis_type=1;
  
    if ( (lcasex[anzx->l].nmax = (int *)malloc( lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].nmin = (int *)malloc( lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].max = (float *)malloc(lcasex[anzx->l].ncomps * sizeof(float))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].min = (float *)malloc(lcasex[anzx->l].ncomps * sizeof(float))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].dat = (float **)malloc(lcasex[anzx->l].ncomps * sizeof(float *))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].compName = (char **)malloc(lcasex[anzx->l].ncomps * sizeof(char *))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].icname = (char **)malloc(lcasex[anzx->l].ncomps * sizeof(char *))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].menu = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].ictype = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].icind1 = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].icind2 = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].iexist = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
  
    for(i=0; i<lcasex[anzx->l].ncomps; i++)
    {
      if ( (lcasex[anzx->l].dat[i] = (float *)calloc( (anzx->nmax+1), sizeof(float))) == NULL )
        printf("\n\n ERROR: malloc failure\n\n" );	               
      if ( (lcasex[anzx->l].compName[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
        printf("\n\n ERROR: malloc failed\n\n" );
      if ( (lcasex[anzx->l].icname[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
        printf("\n\n ERROR: malloc failed\n\n" );
      lcasex[anzx->l].max[i]=-MAX_FLOAT;
      lcasex[anzx->l].min[i]=MAX_FLOAT;
  
      lcasex[anzx->l].menu[i] = 1;
      lcasex[anzx->l].ictype[i] = 2;
      lcasex[anzx->l].icind1[i] = i+1;
      lcasex[anzx->l].icind2[i] = 0;
      lcasex[anzx->l].iexist[i] = 0;
    }
    
    strcpy ( lcasex[anzx->l].compName[0], "fx ");
    strcpy ( lcasex[anzx->l].compName[1], "fy ");
    strcpy ( lcasex[anzx->l].compName[2], "fz ");
    anzx->l++;
    if ( (lcasex = (Datasets *)realloc((Datasets *)lcasex, (anzx->l+3) * sizeof(Datasets))) == NULL )
    { printf("\n\n ERROR: malloc failure\n\n" ); exit(1); }
  
    /* Moments */
    if(sector) sprintf( lcasex[anzx->l].name,"%s%d", CLO, sector);
    else sprintf( lcasex[anzx->l].name,"%s", CLO);
    printf (" create dataset:%d name= %s\n", anzx->l, lcasex[anzx->l].name);
    lcasex[anzx->l].ncomps=3;
    lcasex[anzx->l].irtype=1;
    lcasex[anzx->l].npheader=0;
  
    strcpy(lcasex[anzx->l].analysis_name,"");
    sprintf(lcasex[anzx->l].dataset_name,"STP %d", step_nr);
    strcpy(lcasex[anzx->l].dataset_text,"");
    lcasex[anzx->l].step_number=step_nr;
    lcasex[anzx->l].analysis_type=1;
  
    if ( (lcasex[anzx->l].nmax = (int *)malloc( lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].nmin = (int *)malloc( lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].max = (float *)malloc(lcasex[anzx->l].ncomps * sizeof(float))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].min = (float *)malloc(lcasex[anzx->l].ncomps * sizeof(float))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].dat = (float **)malloc(lcasex[anzx->l].ncomps * sizeof(float *))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].compName = (char **)malloc(lcasex[anzx->l].ncomps * sizeof(char *))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].icname = (char **)malloc(lcasex[anzx->l].ncomps * sizeof(char *))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].menu = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].ictype = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].icind1 = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].icind2 = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].iexist = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
  
    for(i=0; i<lcasex[anzx->l].ncomps; i++)
    {
      if ( (lcasex[anzx->l].dat[i] = (float *)calloc( (anzx->nmax+1), sizeof(float))) == NULL )
        printf("\n\n ERROR: malloc failure\n\n" );	               
      if ( (lcasex[anzx->l].compName[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
        printf("\n\n ERROR: malloc failed\n\n" );
      if ( (lcasex[anzx->l].icname[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
        printf("\n\n ERROR: malloc failed\n\n" );
      lcasex[anzx->l].max[i]=-MAX_FLOAT;
      lcasex[anzx->l].min[i]=MAX_FLOAT;
  
      lcasex[anzx->l].menu[i] = 1;
      lcasex[anzx->l].ictype[i] = 2;
      lcasex[anzx->l].icind1[i] = i+1;
      lcasex[anzx->l].icind2[i] = 0;
      lcasex[anzx->l].iexist[i] = 0;
    }
    
    strcpy ( lcasex[anzx->l].compName[0], "mx ");
    strcpy ( lcasex[anzx->l].compName[1], "my ");
    strcpy ( lcasex[anzx->l].compName[2], "mz ");
    
    anzx_l=anzx->l;

    anzx->l++;
    if ( (lcasex = (Datasets *)realloc((Datasets *)lcasex, (anzx->l+3) * sizeof(Datasets))) == NULL )
    { printf("\n\n ERROR: malloc failure\n\n" ); exit(1); }
  }

  do{
    length=abqrecord(rec_str);
    if(length==-1) return(-1);
    if(length==-2) break;

    args=crecord(rec_str, dat);
    nd=atoi(dat[0]);
    dof=atoi(dat[1]);
    if(args>2) f=atof(dat[2]); else f=0.;

    if((dof<1)||(dof>6))
    {
      printf("ERROR: In *CLOAD dof:%d not known\n", dof);
      return(-1);
    }
    
    /* check if the node-number is 0, then a set was defined */
    if (nd==0)
    {
      nset=getSetNrx(dat[0]);
      if(nset<0)
      {
        printf("ERROR: In *CLOAD set:%s not defined\n", dat[0]);
        return(-1);
      }
      foundLoads=1;
      if((nclo=(int *)realloc((int *)nclo, (ncur+setx[nset].anz_n+1)*sizeof(int)) )==NULL)
      {  printf("\n\n ERROR: realloc failure\n\n" ); return(-1); }
      for(j=0; j<setx[nset].anz_n; j++)
      {
        if(!setNr[0])
        {
          setNr[0]=pre_setax( CLO, "i", 0);
        }
        if(!setNr[dof])
        {
          sprintf(buffer,"%s%d", CLO, dof);
          setNr[dof]=pre_setax( buffer, "i", 0);
        }
        nclo[ncur++]=setx[nset].node[j];
        setax(setNr[0], "n", setx[nset].node[j]);
        setax(setNr[dof], "n", setx[nset].node[j]);

        /* cloads must sum up on each node! */
        if(dof<4)
        {
          writeNodeVals(anzx_l-1, setx[nset].node[j], lcasex[anzx_l-1].dat[dof-1][nd]+f, dof-1);
	}
        else
	{
          writeNodeVals(anzx_l, setx[nset].node[j], lcasex[anzx_l].dat[dof-4][nd]+f, dof-4);
	}
      }
    }
    else
    {
      foundLoads=1;
      if(!setNr[0])
      {
        setNr[0]=pre_setax( CLO, "i", 0);
      }
      if(!setNr[dof])
      {
        sprintf(buffer,"%s%d", CLO, dof);
        setNr[dof]=pre_setax( buffer, "i", 0);
      }
      if((nclo=(int *)realloc((int *)nclo, (ncur+2)*sizeof(int)) )==NULL)
      {  printf("\n\n ERROR: realloc failure\n\n" ); return(-1); }
      nclo[ncur++]=nd;
      setax(setNr[0], "n", nd);
      setax(setNr[dof], "n", nd);

      /* cloads must sum up on each node! */
      if(dof<4) writeNodeVals(anzx_l-1,nd,  lcasex[anzx_l-1].dat[dof-1][nd]+f, dof-1);
      else
      {
        writeNodeVals(anzx_l,nd,  lcasex[anzx_l].dat[dof-4][nd]+f, dof-4);
      }
    }

    if(copyFlag)
    {
      /* write to equ-file */
      for (n=0; n<SEGMENTS; n++)
        fprintf(handle_equ, "%d, %d, %lf\n", nd+(n*NODE_OFFSET), dof, f);
    }
    /* write in ansys format */
    if(ansFlag)
    {
      if ((dof<1)||(dof>6)) errMsg ("ERROR dof:%d not known\n", dof);
      else fprintf(handleans, "F,%d, %s, %lf\n", nd, ansys_frc[dof-1], f);
    }
  }while(1); 

  if(foundLoads)
  {
    /* if the nodes were assigned previously to a cyl-sys, values have to be transformed */

    /* all nodenrs have to be uniqe */
    qsort( nclo, ncur, sizeof(int), (void *)compareInt );  
    i=0;
    for(j=1; j<ncur; j++)
    {
      if(nclo[i]!=nclo[j]) nclo[++i]=nclo[j];
    }
    ncur=i+1;

    for(j=0; j<ncur; j++)
    {
      for(i=anzx->sets-1; i>-1; i--)
      {
        if((setx[i].name!=NULL)&&(compare(setx[i].name, "+transC",7)==7))
        {
          if( getIndex(&setx[i].node,setx[i].anz_n,nclo[j]) >-1)
          {
	    //printf("set[%d]:%s n[%d]:%d\n", i, setx[i].name, j, nclo[j]);
            /* transform from cylindrical to cartesian coordinate */
            args=crecord(&setx[i].name[7], dat);
            for (k=0; k<6; k++) csab[k]=atof(dat[k]);
            rectcyl(-1, csab, nclo[j], nodex, lcasex, anzx_l-1, 'd');
            rectcyl(-1, csab, nclo[j], nodex, lcasex, anzx_l, 'd');
            break;
          }
        }
      }
    }
    free(nclo);
    nclo=NULL;

    /* get new max min */
    calcMaxMinValues(anzx_l-1);
    calcMaxMinValues(anzx_l);
  }
  return(1);
}


int getCflux(char *rec_str)
{
  int i,j; 
  int length, nd, dof, nset, args;
  double f;
  static int setNr[7]={0,0,0,0,0,0,0};
  static int local_step_nr=0, anzx_l;

  /* reset all static variables */
  if(resetFlag)
  {
    for(i=0; i<7; i++) setNr[i]=0;
    local_step_nr=0;
    anzx_l=0;
    return(0);
  }

  if(printFlag) printf("%s\n",rec_str);
  /* scan the properties and write to file */

  /* create a dataset if a new step beginns */
  if(!step_flag) { printf("%s\n ERROR: load was defined outside a *STEP\n\n",rec_str); exit(0); }
  else if(step_nr>local_step_nr)
  {
    local_step_nr=step_nr;
    /* ini lcasex */
    sprintf( lcasex[anzx->l].name,"%s", CFLX);
    printf (" create dataset:%d name= %s\n", anzx->l, lcasex[anzx->l].name);
    lcasex[anzx->l].value=0;
    lcasex[anzx->l].ncomps=1;
    lcasex[anzx->l].irtype=1;
    lcasex[anzx->l].npheader=0;
  
    strcpy(lcasex[anzx->l].analysis_name,"");
    sprintf(lcasex[anzx->l].dataset_name,"STP %d", step_nr);
    strcpy(lcasex[anzx->l].dataset_text,"");
    lcasex[anzx->l].step_number=step_nr;
    lcasex[anzx->l].analysis_type=1;
  
    if ( (lcasex[anzx->l].nmax = (int *)malloc( lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].nmin = (int *)malloc( lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].max = (float *)malloc(lcasex[anzx->l].ncomps * sizeof(float))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].min = (float *)malloc(lcasex[anzx->l].ncomps * sizeof(float))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].dat = (float **)malloc(lcasex[anzx->l].ncomps * sizeof(float *))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].compName = (char **)malloc(lcasex[anzx->l].ncomps * sizeof(char *))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].icname = (char **)malloc(lcasex[anzx->l].ncomps * sizeof(char *))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].menu = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].ictype = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].icind1 = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].icind2 = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].iexist = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
  
    for(i=0; i<lcasex[anzx->l].ncomps; i++)
    {
      if ( (lcasex[anzx->l].dat[i] = (float *)calloc( (anzx->nmax+1), sizeof(float))) == NULL )
        printf("\n\n ERROR: malloc failure\n\n" );
      if ( (lcasex[anzx->l].compName[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
        printf("\n\n ERROR: malloc failed\n\n" );
      if ( (lcasex[anzx->l].icname[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
        printf("\n\n ERROR: malloc failed\n\n" );
      lcasex[anzx->l].max[i]=-MAX_FLOAT;
      lcasex[anzx->l].min[i]=MAX_FLOAT;
  
      lcasex[anzx->l].menu[i] = 1;
      lcasex[anzx->l].ictype[i] = 1;
      lcasex[anzx->l].icind1[i] = i+1;
      lcasex[anzx->l].icind2[i] = 0;
      lcasex[anzx->l].iexist[i] = 0;
    }
    
    strcpy ( lcasex[anzx->l].compName[0], "flux");
    
    anzx_l=anzx->l;

    anzx->l++;
    if ( (lcasex = (Datasets *)realloc((Datasets *)lcasex, (anzx->l+2) * sizeof(Datasets))) == NULL )
    { printf("\n\n ERROR: malloc failure\n\n" ); exit(1); }
  }  

  do{
    length=abqrecord(rec_str);
    if(length==-1) return(-1);
    if(length==-2) break;

    args=crecord(rec_str, dat);
    nd=atoi(dat[0]);
    dof=0;
    if(args>2) f=atof(dat[2]); else f=0.;
    
    /* check if the node-number is 0, then a set was defined */
    if (nd==0)
    {
      nset=getSetNrx(dat[0]);
      if(nset<0)
      {
        printf("ERROR: In *CFLUX set:%s not defined\n", dat[0]);
        return(-1);
      }
      for(j=0; j<setx[nset].anz_n; j++)
      {
        if(!setNr[0])
        {
          setNr[0]=pre_setax( CFLX, "i", 0);
        }
        setax(setNr[0], "n", setx[nset].node[j]);
        /* cflux must sum up on each node! */
        writeNodeVals(anzx_l, setx[nset].node[j], lcasex[anzx_l].dat[dof][nd]+f, dof);
      }
    }
    else
    {
      if(!setNr[0])
      {
        setNr[0]=pre_setax( CFLX, "i", 0);
      }
      setax(setNr[0], "n", nd);
      /* cflux must sum up on each node! */
      writeNodeVals(anzx_l, nd, lcasex[anzx_l].dat[dof][nd]+f, dof);
    }

    /* write in ansys format */
    if(ansFlag)
    {
	printf("ERROR: CFLUX for ansys not yet supported\n");
    }
  }while(1); 
  return(1);
}


int getDload(char *rec_str)
{
  int j,i,n; 
  int length, el, sum_e,args,face;
  int setNr, eset;
  static int created_ds=0;
  static int sector;
  float vals[1];
  static char **dat=NULL;
  static int local_step_nr=0, anzx_l;
  char setname[MAX_LINE_LENGTH];

  /* reset all static variables */
  if(resetFlag)
  {
    sector=0;
    local_step_nr=0;
    anzx_l=0;
    return(0);
  }

  if(printFlag) printf("%s\n",rec_str);

  /* scan the properties and write to file */

  /* get the arguments of the string */
  if((dat = (char **)realloc((char **)dat, (80)*sizeof(char*)))==NULL)
    errMsg("\n\n ERROR: realloc failed for **dat\n" );
    for (i=0; i<80; i++)
    {
      dat[i]=NULL;
      if((dat[i] = (char *)realloc((char *)dat[i], (MAX_LINE_LENGTH)*sizeof(char)))==NULL)
        errMsg("\n\n ERROR: realloc failed for *dat\n" );
    }
  args=crecord(rec_str, dat);
  
  sprintf(setname,"%s_ds",DLO);
  for(i=0; i<args; i++)
  {
    /* delete blanks */
    n=0; for(j=0;j<strlen(dat[i]); j++) if(dat[i][j]!=' ') { buffer[n]=dat[i][j]; n++; }
    buffer[n]='\0';
    for(j=0;j<=strlen(buffer); j++) dat[i][j]=toupper(buffer[j]);
    if(compare(dat[i],"SECTOR=",6)==6)
    {
      /* data for a new sector, create a new dataset */
      local_step_nr=0;
      sector=atoi(&dat[i][7]);
      sprintf(setname,"%s_s%d_ds",DLO, sector);
    }
  }

  /* create a dataset if a new step beginns */
  if(!step_flag) { printf("%s\n ERROR: load was defined outside a *STEP\n\n",rec_str); exit(0); }
  else if(step_nr>local_step_nr)
  {
    /* ini lcasex */
    anzx_l=anzx->l;
    created_ds=1;
  }

  /* store the datasetNr in the setname. this will be used later to fill the dataset in generateNodeValuesFromFaces() */
  sprintf(&setname[strlen(setname)],"%d", anzx_l);
  setNr=getSetNrx(setname);
  if(setNr<0)
  {
    setNr=pre_setax( setname, "i", 0);
  }

  do{
    length=abqrecord(rec_str);
    if(length==-1) return(-1);
    if(length==-2) break;

    crecord(rec_str, dat);

    /* check if it is a face load */
    if((toupper(dat[1][0])=='P')||(toupper(dat[1][0])=='E'))
    {
      if(toupper(dat[1][0])=='E') face=(int)atoi(&dat[1][5])+1;
      else face=(int)atoi(&dat[1][1])-1;
  
      eset=getSetNrx(dat[0]);
      if (eset>-1) sum_e=setx[eset].anz_e;
      else sum_e=1;
      for(j=0; j<sum_e; j++)
      {
        if(eset>-1) el=setx[eset].elem[j];
        else el=atoi(dat[0]);
  
        if((el<anzx->emin)||(el>anzx->emax)) { printf("elem:%d does not exist\n",el); continue;}
        if((e_enqirex[el].type>6)&&(e_enqirex[el].type<11))
        {
           if(e_enqirex[el].attr>3)
           {
             if(e_enqirex[el].type<9)
  	   {
               if(face<0) { if(dat[1][1]=='N') face=4; else face=5; }
               else face+=1;
  	   }
             else
  	   {
               if(face<0) { if(dat[1][1]=='N') face=5; else face=6; }
               else face+=1;
  	   }
           }
           else
           {
             if(face>1) face+=1; else { if(dat[1][1]=='N') face=0; else face=1; }
           }
        }
        vals[0]=atof(dat[2]);
       
        /* check if the node-number is 0, then an eset is defined */
        if(face>=0)
        {
          i=setax(setNr,"j",1);
          if(i>-1)
          {
            setx[setNr].elf[i].e=el;
            setx[setNr].elf[i].f=face;
            setx[setNr].elf[i].v[0]=vals[0];
          }
        }
    
        /* write in ansys format */
        if(ansFlag)
        {
          errMsg ("ERROR dload not supported\n");
        }
      }
    }
  }while(1);

  /* activate a new dataset only if data are stored */
  if(created_ds)
  {
    if(setx[setNr].anz_elf)
    {
      if ( (lcasex = (Datasets *)realloc((Datasets *)lcasex, (anzx->l+2) * sizeof(Datasets))) == NULL )
      { printf("\n\n ERROR: malloc failure\n\n" ); exit(1); }
      lcasex[anzx->l].step_number=step_nr;
      anzx->l++;
      created_ds=0;
      local_step_nr=step_nr;
    }
  }

  return(1);
}


int getDflux(char *rec_str)
{
  int j,i; 
  int length, el, sum_e, face;
  int setNr, eset;
  static int created_ds=0;
  float vals[1];
  static int local_step_nr=0, anzx_l;
  char setname[MAX_LINE_LENGTH];

  /* reset all static variables */
  if(resetFlag)
  {
    local_step_nr=0;
    anzx_l=0;
    return(0);
  }


  if(printFlag) printf("%s\n",rec_str);

  /* scan the properties and write to file */
  sprintf(setname,"%s_ds",DFLX);

  /* create a dataset if a new step beginns */
  if(!step_flag) { printf("%s\n ERROR: load was defined outside a *STEP\n\n",rec_str); exit(0); }
  else if(step_nr>local_step_nr)
  {
    /* ini lcasex */
    anzx_l=anzx->l;
    created_ds=1;
  }  

  /* store the datasetNr in the setname. this will be used later to fill the dataset in generateNodeValuesFromFaces() */
  sprintf(&setname[strlen(setname)],"%d", anzx_l);
  setNr=getSetNrx(setname);
  if(setNr<0)
  {
    setNr=pre_setax( setname, "i", 0);
  }

  do{
    length=abqrecord(rec_str);
    if(length==-1) return(-1);
    if(length==-2) break;

    crecord(rec_str, dat);

    /* check if it is a face load */
    if(toupper(dat[1][0])=='S')
    {
      face=(int)atoi(&dat[1][1])-1;
  
      eset=getSetNrx(dat[0]);
      if (eset>-1) sum_e=setx[eset].anz_e;
      else sum_e=1;
      for(j=0; j<sum_e; j++)
      {
        if(eset>-1) el=setx[eset].elem[j];
        else el=atoi(dat[0]);
  
        if((el<anzx->emin)||(el>anzx->emax)) { printf("elem:%d does not exist\n",el); continue;}
        if((e_enqirex[el].type>6)&&(e_enqirex[el].type<11))
        {
           if(e_enqirex[el].attr>3)
           {
             if(e_enqirex[el].type<9)
  	   {
               if(face<0) { if(dat[1][1]=='N') face=4; else face=5; }
               else face+=1;
  	   }
             else
  	   {
               if(face<0) { if(dat[1][1]=='N') face=5; else face=6; }
               else face+=1;
  	   }
           }
           else
           {
             if(face>1) face+=1; else { if(dat[1][1]=='N') face=0; else face=1; }
           }
        }
        vals[0]=atof(dat[2]);
       
        /* check if the node-number is 0, then an eset is defined */
        if(face>=0)
        {
          i=setax(setNr,"j",1);
          if(i>-1)
          {
            setx[setNr].elf[i].e=el;
            setx[setNr].elf[i].f=face;
            setx[setNr].elf[i].v[0]=vals[0];
          }
        }

        /* write in ansys format */
        if(ansFlag)
        {
          errMsg ("ERROR dflux not supported\n");
        }
      }
    }
  }while(1);

  /* activate a new dataset only if data are stored */
  if(created_ds)
  {
    if(setx[setNr].anz_elf)
    {
      if ( (lcasex = (Datasets *)realloc((Datasets *)lcasex, (anzx->l+2) * sizeof(Datasets))) == NULL )
      { printf("\n\n ERROR: malloc failure\n\n" ); exit(1); }
      lcasex[anzx->l].step_number=step_nr;
      anzx->l++;
      created_ds=0;
      local_step_nr=step_nr;
    }
  }

  return(1);
}



int getMassflow(char *rec_str)
{
  int j,i; 
  int length, el, sum_e,face;
  int setNr, eset;
  static int created_ds=0;
  float vals[1];
  static int local_step_nr=0, anzx_l;
  char setname[MAX_LINE_LENGTH];

  /* reset all static variables */
  if(resetFlag)
  {
    local_step_nr=0;
    anzx_l=0;
    return(0);
  }


  if(printFlag) printf("%s\n",rec_str);

  /* scan the properties and write to file */
  sprintf(setname,"%s_ds",MFLW);

  /* create a dataset if a new step beginns */
  if(!step_flag) { printf("%s\n ERROR: load was defined outside a *STEP\n\n",rec_str); exit(0); }
  else if(step_nr>local_step_nr)
  {
    /* ini lcasex */
    anzx_l=anzx->l;
    created_ds=1;
  }  

  /* store the datasetNr in the setname. this will be used later to fill the dataset in generateNodeValuesFromFaces() */
  sprintf(&setname[strlen(setname)],"%d", anzx_l);
  setNr=getSetNrx(setname);
  if(setNr<0)
  {
    setNr=pre_setax( setname, "i", 0);
  }

  
  do{
    length=abqrecord(rec_str);
    if(length==-1) return(-1);
    if(length==-2) break;

    crecord(rec_str, dat);

    /* check if it is a face load */
    if(toupper(dat[1][0])=='M')
    {
      face=(int)atoi(&dat[1][1])-1;
  
      eset=getSetNrx(dat[0]);
      if (eset>-1) sum_e=setx[eset].anz_e;
      else sum_e=1;
      for(j=0; j<sum_e; j++)
      {
        if(eset>-1) el=setx[eset].elem[j];
        else el=atoi(dat[0]);
  
        if((el<anzx->emin)||(el>anzx->emax)) { printf("elem:%d does not exist\n",el); continue;}
        if((e_enqirex[el].type>6)&&(e_enqirex[el].type<11))
        {
           if(e_enqirex[el].attr>3)
           {
             if(e_enqirex[el].type<9)
  	   {
               if(face<0) { if(dat[1][1]=='N') face=4; else face=5; }
               else face+=1;
  	   }
             else
  	   {
               if(face<0) { if(dat[1][1]=='N') face=5; else face=6; }
               else face+=1;
  	   }
           }
           else
           {
             if(face>1) face+=1; else { if(dat[1][1]=='N') face=0; else face=1; }
           }
        }
        vals[0]=atof(dat[2]);
       
        /* check if the node-number is 0, then an eset is defined */
        if(face>=0)
        {
          i=setax(setNr,"j",1);
          if(i>-1)
          {
            setx[setNr].elf[i].e=el;
            setx[setNr].elf[i].f=face;
            setx[setNr].elf[i].v[0]=vals[0];
          }
        }
    
        /* write in ansys format */
        if(ansFlag)
        {
          errMsg ("ERROR massflow not supported\n");
        }
      }
    }
  }while(1);

  /* activate a new dataset only if data are stored */
  if(created_ds)
  {
    if(setx[setNr].anz_elf)
    {
      if ( (lcasex = (Datasets *)realloc((Datasets *)lcasex, (anzx->l+2) * sizeof(Datasets))) == NULL )
      { printf("\n\n ERROR: malloc failure\n\n" ); exit(1); }
      lcasex[anzx->l].step_number=step_nr;
      anzx->l++;
      created_ds=0;
      local_step_nr=step_nr;
    }
  }

  return(1);
}


int getRadiation(char *rec_str)
{
  int j,i,n; 
  int length, el, sum_e, args, face;
  int setNr, eset;
  int firstTimeFlag=1;
  float vals[2];
  static char **dat=NULL;
  static int local_step_nr=0, anzx_l;
  char setname[MAX_LINE_LENGTH];

  /* reset all static variables */
  if(resetFlag)
  {
    local_step_nr=0;
    anzx_l=0;
    return(0);
  }

  if(printFlag) printf("%s\n",rec_str);

  /* scan the properties and write to file */

  /* get the arguments of the string */
  if((dat = (char **)realloc((char **)dat, (80)*sizeof(char*)))==NULL)
    errMsg("\n\n ERROR: realloc failed for **dat\n" );
    for (i=0; i<80; i++)
    {
      dat[i]=NULL;
      if((dat[i] = (char *)realloc((char *)dat[i], (MAX_LINE_LENGTH)*sizeof(char)))==NULL)
        errMsg("\n\n ERROR: realloc failed for *dat\n" );
    }
  args=crecord(rec_str, dat);
  
  sprintf(setname,"%s_stp%d",RAD, step_nr);
  for(i=0; i<args; i++)
  {
    /* delete blanks */
    n=0; for(j=0;j<strlen(dat[i]); j++) if(dat[i][j]!=' ') { buffer[n]=dat[i][j]; n++; }
    buffer[n]='\0';
    for(j=0;j<=strlen(buffer); j++) dat[i][j]=toupper(buffer[j]);
    if(compare(dat[i],"CAVITY=",6)==6) sprintf(setname,"%s_stp%d_cav:%s",RAD, step_nr, &dat[i][7]);
  }
  setNr=getSetNrx(setname);
  if(setNr<0)
  {
    setNr=pre_setax( setname, "i", 0);
  }

  /* create a dataset if a new step beginns */
  if(!step_flag) { printf("%s\n ERROR: load was defined outside a *STEP\n\n",rec_str); exit(0); }
  else if(step_nr>local_step_nr)
  {
    local_step_nr=step_nr;
    /* ini lcasex */
    sprintf( lcasex[anzx->l].name,"%s", RAD);
    printf (" create dataset:%d name= %s\n", anzx->l, lcasex[anzx->l].name);
    lcasex[anzx->l].ncomps=3;
    lcasex[anzx->l].irtype=1;
    lcasex[anzx->l].npheader=0;
    
    lcasex[anzx->l].value=0;
    strcpy(lcasex[anzx->l].analysis_name,"");
    sprintf(lcasex[anzx->l].dataset_name,"STP %d", step_nr);
    strcpy(lcasex[anzx->l].dataset_text,"");
    lcasex[anzx->l].step_number=step_nr;
    lcasex[anzx->l].analysis_type=1;
    
    if ( (lcasex[anzx->l].nmax = (int *)malloc( lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].nmin = (int *)malloc( lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].max = (float *)malloc(lcasex[anzx->l].ncomps * sizeof(float))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].min = (float *)malloc(lcasex[anzx->l].ncomps * sizeof(float))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].dat = (float **)malloc(lcasex[anzx->l].ncomps * sizeof(float *))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].compName = (char **)malloc(lcasex[anzx->l].ncomps * sizeof(char *))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].icname = (char **)malloc(lcasex[anzx->l].ncomps * sizeof(char *))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].menu = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].ictype = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].icind1 = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].icind2 = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].iexist = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    
    for(i=0; i<lcasex[anzx->l].ncomps; i++)
    {
      if ( (lcasex[anzx->l].dat[i] = (float *)calloc( (anzx->nmax+1), sizeof(float))) == NULL )
        printf("\n\n ERROR: malloc failure\n\n" );	               
      if ( (lcasex[anzx->l].compName[i] = (char *)calloc( MAX_LINE_LENGTH, sizeof(char))) == NULL )
        printf("\n\n ERROR: malloc failed\n\n" );
      if ( (lcasex[anzx->l].icname[i] = (char *)calloc( MAX_LINE_LENGTH, sizeof(char))) == NULL )
        printf("\n\n ERROR: malloc failed\n\n" );
      lcasex[anzx->l].max[i]=-MAX_FLOAT;
      lcasex[anzx->l].min[i]=MAX_FLOAT;
      for(j=0; j<=anzx->nmax; j++) lcasex[anzx->l].dat[i][j]=0.;
    
      lcasex[anzx->l].menu[i] = 1;
      lcasex[anzx->l].ictype[i] = 1;
      lcasex[anzx->l].icind1[i] = i+1;
      lcasex[anzx->l].icind2[i] = 0;
      lcasex[anzx->l].iexist[i] = -2;
    }
    lcasex[anzx->l].iexist[2] = 0;
    strcpy ( lcasex[anzx->l].compName[0], "temp");
    strcpy ( lcasex[anzx->l].compName[1], "emis");
    strcpy ( lcasex[anzx->l].compName[2], "nodes");
    lcasex[anzx->l].menu[2] = 0;
    
    anzx_l=anzx->l;

    anzx->l++;
    if ( (lcasex = (Datasets *)realloc((Datasets *)lcasex, (anzx->l+2) * sizeof(Datasets))) == NULL )
    { printf("\n\n ERROR: malloc failure\n\n" ); exit(1); }
  }  

  do{
    length=abqrecord(rec_str);
    if(length==-1) return(-1);
    if(length==-2) break;

    crecord(rec_str, dat);
    /* delete blanks and get face-nr */
    i=0; for(j=0;j<strlen(dat[1]); j++) if(dat[1][j]!=' ') { buffer[i]=dat[1][j]; i++; }
    buffer[i]='\0';
    face=atoi(&buffer[1])-1;

    eset=getSetNrx(dat[0]);
    if (eset>-1) sum_e=setx[eset].anz_e;
    else sum_e=1;
    for(j=0; j<sum_e; j++)
    {
      if(eset>-1) el=setx[eset].elem[j];
      else el=atoi(dat[0]);

      if((el<anzx->emin)||(el>anzx->emax)) { printf("elem:%d in *film does not exist\n",el); continue;}
      if((e_enqirex[el].type>6)&&(e_enqirex[el].type<11))
      {
        if(e_enqirex[el].attr>3)
        {
          if(e_enqirex[el].type<9)
	  {
            if(face<0) { if(dat[1][1]=='N') face=4; else face=5; }
            else face+=1;
	  }
          else
	  {
            if(face<0) { if(dat[1][1]=='N') face=5; else face=6; }
            else face+=1;
	  }
        }
        else
        {
          if(face>1) face+=1; else { if(dat[1][1]=='N') face=0; else face=1; }
        }
      }
      if(face<0) printf(" ERROR, face from string:%s unknown\n", buffer);
      //snip1

      if(compare(&buffer[2],"CRNU", 4)==4)
      {
        vals[0]=-1;
        vals[1]=atof(&buffer[6]);
        if(firstTimeFlag)
        {
          strcpy ( lcasex[anzx_l].compName[0], "undef");
          strcpy ( lcasex[anzx_l].compName[1], "pattrn");
          strcpy ( lcasex[anzx_l].compName[2], "nodes");
          lcasex[anzx_l].value=vals[1];
        }
        else firstTimeFlag=0;
      }
      else if(compare(&buffer[2],"CR", 2)==2)
      {
        vals[0]=atof(dat[2]);
        vals[1]=atof(dat[3]);
        if(firstTimeFlag)
        {
          strcpy ( lcasex[anzx_l].compName[0], "temp");
          strcpy ( lcasex[anzx_l].compName[1], "emis");
          strcpy ( lcasex[anzx_l].compName[2], "nodes");
        }
        else firstTimeFlag=0;
      }
      else if(compare(&buffer[2],"NU", 2)==2)
      {
        vals[0]=-1.;
        vals[1]=atof(&buffer[6]);
        if(firstTimeFlag)
        {
          strcpy ( lcasex[anzx_l].compName[0], "undef");
          strcpy ( lcasex[anzx_l].compName[1], "pattrn");
          strcpy ( lcasex[anzx_l].compName[2], "nodes");
          lcasex[anzx_l].value=vals[1];
        }
        else firstTimeFlag=0;
      }
      else
      {
        vals[0]=atof(dat[2]);
        vals[1]=atof(dat[3]);
      }

      //snip2
      if(face>=0)
      {
        i=setax(setNr,"j",0);
        if(i>-1)
        {
          setx[setNr].elf[i].e=el;
          setx[setNr].elf[i].f=face;
        }
        writeFaceVals(anzx_l, el, face, 2, vals, 0);
      }
  
      /* write in ansys format */
      if(ansFlag)
      {
        errMsg ("ERROR radiation not supported\n");
      }
    }
  }while(1); 
  return(1);
}


int getFilm(char *rec_str)
{
  int j,i; 
  int length, el, sum_e, face;
  int setNr, eset;
  int firstTimeFlag=1;
  float vals[2];
  static int local_step_nr=0, anzx_l;
  char setname[MAX_LINE_LENGTH];

  /* reset all static variables */
  if(resetFlag)
  {
    local_step_nr=0;
    anzx_l=0;
    return(0);
  }

  if(printFlag) printf("%s\n",rec_str);
  /* scan the properties and write to file */
  sprintf(setname,"%s_stp%d",FLM, step_nr);
  setNr=getSetNrx(setname);
  if(setNr<0)
  {
    setNr=pre_setax( setname, "i", 0);
  }

  /* create a dataset if a new step beginns */
  if(!step_flag) { printf("%s\n ERROR: load was defined outside a *STEP\n\n",rec_str); exit(0); }
  else if(step_nr>local_step_nr)
  {
    local_step_nr=step_nr;
    /* ini lcasex */
    sprintf( lcasex[anzx->l].name,"%s", FLM);
    printf (" create dataset:%d name= %s\n", anzx->l, lcasex[anzx->l].name);
    lcasex[anzx->l].ncomps=3;
    lcasex[anzx->l].irtype=1;
    lcasex[anzx->l].npheader=0;
  
    lcasex[anzx->l].value=0;
    strcpy(lcasex[anzx->l].analysis_name,"");
    sprintf(lcasex[anzx->l].dataset_name,"STP %d", step_nr);
    strcpy(lcasex[anzx->l].dataset_text,"");
    lcasex[anzx->l].step_number=step_nr;
    lcasex[anzx->l].analysis_type=1;
  
    if ( (lcasex[anzx->l].nmax = (int *)malloc( lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].nmin = (int *)malloc( lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].max = (float *)malloc(lcasex[anzx->l].ncomps * sizeof(float))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].min = (float *)malloc(lcasex[anzx->l].ncomps * sizeof(float))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].dat = (float **)malloc(lcasex[anzx->l].ncomps * sizeof(float *))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].compName = (char **)malloc(lcasex[anzx->l].ncomps * sizeof(char *))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].icname = (char **)malloc(lcasex[anzx->l].ncomps * sizeof(char *))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].menu = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].ictype = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].icind1 = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].icind2 = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcasex[anzx->l].iexist = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
  
    for(i=0; i<lcasex[anzx->l].ncomps; i++)
    {
      if ( (lcasex[anzx->l].dat[i] = (float *)calloc( (anzx->nmax+1), sizeof(float))) == NULL )
        printf("\n\n ERROR: malloc failure\n\n" );	               
      if ( (lcasex[anzx->l].compName[i] = (char *)calloc( MAX_LINE_LENGTH, sizeof(char))) == NULL )
        printf("\n\n ERROR: malloc failed\n\n" );
      if ( (lcasex[anzx->l].icname[i] = (char *)calloc( MAX_LINE_LENGTH, sizeof(char))) == NULL )
        printf("\n\n ERROR: malloc failed\n\n" );
      lcasex[anzx->l].max[i]=-MAX_FLOAT;
      lcasex[anzx->l].min[i]=MAX_FLOAT;
      for(j=0; j<=anzx->nmax; j++) lcasex[anzx->l].dat[i][j]=0.;
      lcasex[anzx->l].menu[i] = 1;
      lcasex[anzx->l].ictype[i] = 1;
      lcasex[anzx->l].icind1[i] = i+1;
      lcasex[anzx->l].icind2[i] = 0;
      /* remember the component which stores the nr of summands */
      lcasex[anzx->l].iexist[i] = -2;
    }
    lcasex[anzx->l].iexist[2] = 0;
    strcpy ( lcasex[anzx->l].compName[0], "temp");
    strcpy ( lcasex[anzx->l].compName[1], "alpha");
    strcpy ( lcasex[anzx->l].compName[2], "nodes");
    lcasex[anzx->l].menu[2] = 0;
    
    anzx_l=anzx->l;

    anzx->l++;
    if ( (lcasex = (Datasets *)realloc((Datasets *)lcasex, (anzx->l+2) * sizeof(Datasets))) == NULL )
    { printf("\n\n ERROR: malloc failure\n\n" ); exit(1); }
  }  

  do{
    length=abqrecord(rec_str);
    if(length==-1) return(-1);
    if(length==-2) break;

    crecord(rec_str, dat);
    /* delete blanks and get face-nr */
    i=0; for(j=0;j<strlen(dat[1]); j++) if(dat[1][j]!=' ') { buffer[i]=dat[1][j]; i++; }
    buffer[i]='\0';
    face=atoi(&buffer[1])-1;

    eset=getSetNrx(dat[0]);
    if (eset>-1) sum_e=setx[eset].anz_e;
    else sum_e=1;
    for(j=0; j<sum_e; j++)
    {
      if(eset>-1) el=setx[eset].elem[j];
      else el=atoi(dat[0]);

      if((el<anzx->emin)||(el>anzx->emax)) { printf("elem:%d in *film does not exist\n",el); continue;}
      if((e_enqirex[el].type>6)&&(e_enqirex[el].type<11))
      {
        if(e_enqirex[el].attr>3)
        {
          if(e_enqirex[el].type<9)
	  {
            if(face<0) { if(dat[1][1]=='N') face=4; else face=5; }
            else face+=1;
	  }
          else
	  {
            if(face<0) { if(dat[1][1]=='N') face=5; else face=6; }
            else face+=1;
	  }
        }
        else
        {
          if(face>1) face+=1; else { if(dat[1][1]=='N') face=0; else face=1; }
        }
      }
      if(face<0) printf(" ERROR, face from string:%s unknown\n", buffer);
      if(compare(&buffer[2],"FCNU", 4)==4)
      {
        vals[0]=atof(dat[2]);
        vals[1]=atof(&buffer[6]);
        if(firstTimeFlag)
        {
          strcpy ( lcasex[anzx_l].compName[0], "refnod");
          strcpy ( lcasex[anzx_l].compName[1], "pattrn");
          strcpy ( lcasex[anzx_l].compName[2], "nodes");
          lcasex[anzx_l].value=vals[1];
        }
        else firstTimeFlag=0;
      }
      else if(compare(&buffer[2],"FC", 2)==2)
      {
        vals[0]=atof(dat[2]);
        vals[1]=atof(dat[3]);
        if(firstTimeFlag)
        {
          strcpy ( lcasex[anzx_l].compName[0], "refnod");
          strcpy ( lcasex[anzx_l].compName[1], "alpha");
          strcpy ( lcasex[anzx_l].compName[2], "nodes");
        }
        else firstTimeFlag=0;
      }
      else if(compare(&buffer[2],"NU", 2)==2)
      {
        vals[0]=-1.;
        vals[1]=atof(&buffer[6]);
        if(firstTimeFlag)
        {
          strcpy ( lcasex[anzx_l].compName[0], "undef");
          strcpy ( lcasex[anzx_l].compName[1], "pattrn");
          strcpy ( lcasex[anzx_l].compName[2], "nodes");
        }
        else firstTimeFlag=0;
      }
      else
      {
        vals[0]=atof(dat[2]);
        vals[1]=atof(dat[3]);
      }
  
      /* check if the node-number is 0, then an eset is defined */
      if(face>=0)
      {
        i=setax(setNr,"j",0);
        if(i>-1)
        {
          setx[setNr].elf[i].e=el;
          setx[setNr].elf[i].f=face;
        }
        writeFaceVals(anzx_l, el, face, 2, vals, 0);
      }
  
      /* write in ansys format */
      if(ansFlag)
      {
        errMsg ("ERROR film not supported\n");
      }
    }
  }while(1); 
  return(1);
}



int getSpcf(char *rec_str)
{
  int i,j,lc; 
  int length, el, sum_e, face;
  int setNr, eset, setNr_dof[12];
  int dof[2];
  float vals[12];
  static int local_step_nr=-1, anzx_l;
  char setname[MAX_LINE_LENGTH];

  /* reset all static variables */
  if(resetFlag)
  {
    local_step_nr=-1;
    anzx_l=0;
    return(0);
  }

  if(printFlag) printf("%s\n",rec_str);
  /* scan the properties and write to file */
  sprintf(setname,"%s_stp%d",SPCF, step_nr);
  setNr=getSetNrx(setname);
  if(setNr<0)
  {
    setNr=pre_setax( setname, "i", 0);
  }

  /* create a dataset if a new step beginns */
  /* .. local_step_nr was initialized with 0
  if(!step_flag) { printf("%s\n ERROR: load was defined outside a *STEP\n\n",rec_str); exit(0); }
  else
  */
  if(step_nr>local_step_nr)
  {
    anzx_l=anzx->l;

    local_step_nr=step_nr;
    /* ini lcasex */
    for(lc=0; lc<5; lc++)
    {
      sprintf( lcasex[anzx->l].name,"%s", SPCF);
      printf (" create dataset:%d name= %s\n", anzx->l, lcasex[anzx->l].name);
      lcasex[anzx->l].ncomps=2;
      lcasex[anzx->l].irtype=1;
      lcasex[anzx->l].npheader=0;
    
      lcasex[anzx->l].value=0;
      strcpy(lcasex[anzx->l].analysis_name,"");
      sprintf(lcasex[anzx->l].dataset_name,"STP %d", step_nr);
      strcpy(lcasex[anzx->l].dataset_text,"");
      lcasex[anzx->l].step_number=step_nr;
      lcasex[anzx->l].analysis_type=1;
    
      if ( (lcasex[anzx->l].nmax = (int *)malloc( lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
        printf("\n\n ERROR: malloc failure\n\n" );
      if ( (lcasex[anzx->l].nmin = (int *)malloc( lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
        printf("\n\n ERROR: malloc failure\n\n" );
      if ( (lcasex[anzx->l].max = (float *)malloc(lcasex[anzx->l].ncomps * sizeof(float))) == NULL )
        printf("\n\n ERROR: malloc failure\n\n" );
      if ( (lcasex[anzx->l].min = (float *)malloc(lcasex[anzx->l].ncomps * sizeof(float))) == NULL )
        printf("\n\n ERROR: malloc failure\n\n" );
      if ( (lcasex[anzx->l].dat = (float **)malloc(lcasex[anzx->l].ncomps * sizeof(float *))) == NULL )
        printf("\n\n ERROR: malloc failure\n\n" );
      if ( (lcasex[anzx->l].compName = (char **)malloc(lcasex[anzx->l].ncomps * sizeof(char *))) == NULL )
        printf("\n\n ERROR: malloc failure\n\n" );
      if ( (lcasex[anzx->l].icname = (char **)malloc(lcasex[anzx->l].ncomps * sizeof(char *))) == NULL )
        printf("\n\n ERROR: malloc failure\n\n" );
      if ( (lcasex[anzx->l].menu = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
        printf("\n\n ERROR: malloc failure\n\n" );
      if ( (lcasex[anzx->l].ictype = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
        printf("\n\n ERROR: malloc failure\n\n" );
      if ( (lcasex[anzx->l].icind1 = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
        printf("\n\n ERROR: malloc failure\n\n" );
      if ( (lcasex[anzx->l].icind2 = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
        printf("\n\n ERROR: malloc failure\n\n" );
      if ( (lcasex[anzx->l].iexist = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
        printf("\n\n ERROR: malloc failure\n\n" );
    
      for(i=0; i<lcasex[anzx->l].ncomps; i++)
      {
        if ( (lcasex[anzx->l].dat[i] = (float *)calloc( (anzx->nmax+1), sizeof(float))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );	               
        if ( (lcasex[anzx->l].compName[i] = (char *)calloc( MAX_LINE_LENGTH, sizeof(char))) == NULL )
          printf("\n\n ERROR: malloc failed\n\n" );
        if ( (lcasex[anzx->l].icname[i] = (char *)calloc( MAX_LINE_LENGTH, sizeof(char))) == NULL )
          printf("\n\n ERROR: malloc failed\n\n" );
        lcasex[anzx->l].max[i]=-MAX_FLOAT;
        lcasex[anzx->l].min[i]=MAX_FLOAT;
        for(j=0; j<=anzx->nmax; j++) lcasex[anzx->l].dat[i][j]=0.;
        lcasex[anzx->l].menu[i] = 1;
        lcasex[anzx->l].ictype[i] = 1;
        lcasex[anzx->l].icind1[i] = i+1;
        lcasex[anzx->l].icind2[i] = 0;
        /* remember the component which stores the nr of summands */
        lcasex[anzx->l].iexist[i] = -1;
      }
      lcasex[anzx->l].iexist[1] = 0;
      lcasex[anzx->l].menu[1] = 0;
      strcpy ( lcasex[anzx->l].compName[0], "value");
      strcpy ( lcasex[anzx->l].compName[1], "nodes");
      anzx->l++;
      if ( (lcasex = (Datasets *)realloc((Datasets *)lcasex, (anzx->l+2) * sizeof(Datasets))) == NULL )
      { printf("\n\n ERROR: malloc failure\n\n" ); exit(1); }
    }

    strcpy ( lcasex[anzx_l].name,   "DOF1");
    strcpy ( lcasex[anzx_l+1].name, "DOF2");
    strcpy ( lcasex[anzx_l+2].name, "DOF3");
    strcpy ( lcasex[anzx_l+3].name, "PRESS");
    strcpy ( lcasex[anzx_l+4].name, "TEMP ");
  }  
  do{
    length=abqrecord(rec_str);
    if(length==-1) return(-1);
    if(length==-2) break;

    crecord(rec_str, dat);
    /* delete blanks and get face-nr */
    i=0; for(j=0;j<strlen(dat[1]); j++) if(dat[1][j]!=' ') { buffer[i]=dat[1][j]; i++; }
    buffer[i]='\0';
    face=atoi(&buffer[1])-1;

    eset=getSetNrx(dat[0]);
    if (eset>-1) sum_e=setx[eset].anz_e;
    else sum_e=1;
    for(j=0; j<sum_e; j++)
    {
      if(eset>-1) el=setx[eset].elem[j];
      else el=atoi(dat[0]);

      if((el<anzx->emin)||(el>anzx->emax)) { printf("elem:%d in *film does not exist\n",el); continue;}
      if((e_enqirex[el].type>6)&&(e_enqirex[el].type<11))
      {
        if(e_enqirex[el].attr>3)
        {
          if(e_enqirex[el].type<9)
	  {
            if(face<0) { if(dat[1][1]=='N') face=4; else face=5; }
            else face+=1;
	  }
          else
	  {
            if(face<0) { if(dat[1][1]=='N') face=5; else face=6; }
            else face+=1;
	  }
        }
        else
        {
          if(face>1) face+=1; else { if(dat[1][1]=='N') face=0; else face=1; }
        }
      }
      if(face<0) printf(" ERROR, face from string:%s unknown\n", buffer);
      //snip1

      for(i=0; i<12; i++)   vals[i]=0.;
      dof[0]=atoi(dat[2]);
      dof[1]=atoi(dat[3]);
      for(i=0; i<2; i++)
      {
        if(dof[i]==DOFP) dof[i]=4;
        if(dof[i]==DOFT) dof[i]=5;
      }
      if(dof[1]<dof[0]) dof[1]=dof[0];
      
      for(i=dof[0]-1; i<=dof[1]-1; i++)
      {
        vals[i]=(double)atof(dat[4]);
      }
  
      /* create sets for the actual dofs */
      for(i=dof[0]-1; i<=dof[1]-1; i++)
      {
        if(i+1==4) sprintf(setname,"%s%d_stp%d",SPCF, 8, step_nr);
        else if(i+1==5) sprintf(setname,"%s%d_stp%d",SPCF, 11, step_nr);
        else sprintf(setname,"%s%d_stp%d",SPCF, i+1, step_nr);
        setNr_dof[i]=getSetNrx(setname);
        if(setNr_dof[i]<0)
        {
          setNr_dof[i]=pre_setax( setname, "i", 0);
        }
        printf("dof: i+1:%d set:%s nr:%d\n", i+1, setname, setNr_dof[i]);
      }

      //snip2
      if(face>=0)
      {
        i=setax(setNr,"j",0);
        if(i>-1)
        {
          setx[setNr].elf[i].e=el;
          setx[setNr].elf[i].f=face;
        }
        for(i=dof[0]-1; i<=dof[1]-1; i++)
	{
          j=setax(setNr_dof[i],"j",0);
          if(j>-1)
          {
            setx[setNr_dof[i]].elf[j].e=el;
            setx[setNr_dof[i]].elf[j].f=face;
          }
          writeFaceVals(anzx_l+i, el, face, 1, &vals[i], 0);
        }
      }
  
      /* write in ansys format */
      if(ansFlag)
      {
        errMsg ("ERROR spcf not supported\n");
      }
    }
  }while(1); 
  return(1);
}


int getElset(char *rec_str)
{
  int i,j,n, args,length, generate=0, setNr, eset;
  char set[MAX_LINE_LENGTH];
  int el[20];

  /* get the arguments of the string */
  args=crecord(rec_str, dat);
  
  /* extract the type */
  for(i=0; i<args; i++)
  {
    /* delete blanks */
    n=0; for(j=0;j<strlen(dat[i]); j++) if(dat[i][j]!=' ') { buffer[n]=dat[i][j]; n++; }
    buffer[n]='\0';
    for(j=0;j<=strlen(buffer); j++) dat[i][j]=toupper(buffer[j]);
    if(compare(dat[i],"ELSET=",6)==6) strcpy(set,&buffer[6]);
    if(compare(dat[i],"GENERATE",5)==5) generate=1;
  }
  if(printFlag) printf("*ELSET: ELSET:%s\n", set);
  setNr=getSetNrx(set);
  if(setNr<0) setNr=pre_setax( set, "i", 0);

  /* scan the element-names and write to file */
  do
  {
    length=abqrecord(rec_str);
    if(length==-1) return(-1);
    if(length==-2) break;
    if(generate)
    {
      el[0]=el[1]=0;
      el[2]=1;
      length=sscanf ( rec_str, "%d, %d, %d", &el[0],&el[1],&el[2]);
      /* write in list format */
      if(!el[2])
      {
        setax( setNr, "e", el[0]);
        setax( setNr, "e", el[1]);
      }
      else
      {
        for(i=el[0]; i<=el[1]; i+=el[2])
        {
          setax( setNr, "e", i);
        }
      }
    }
    else
    {
      args=crecord(rec_str, dat);
      /* extract the type */
      for(i=0; i<args; i++)
      {
        /* write in list format */
        el[0]=atoi(dat[i]);
        if(el[0]) setax( setNr, "e", el[0]);
        else
	{
          eset=getSetNrx(dat[i]);
          if(eset<0)
          {
            printf("ERROR: In *ELSET set:%s not defined\n",dat[i]);
            return(-1);
          }
          for(j=0; j<setx[eset].anz_e; j++) setax( setNr, "e", setx[eset].elem[j]);
	}
      }
    }
  }while(1); 
  return(1);
}


int getNset(char *rec_str)
{
  int i,j,n, args,length, generate=0, setNr, nset;
  char set[MAX_LINE_LENGTH];
  int nd[20];  


  /* get the arguments of the string */
  args=crecord(rec_str, dat);
  
  /* extract the type */
  for(i=0; i<args; i++)
  {
    /* delete blanks */
    n=0; for(j=0;j<strlen(dat[i]); j++) if(dat[i][j]!=' ') { buffer[n]=dat[i][j]; n++; }
    buffer[n]='\0';
    for(j=0;j<=strlen(buffer); j++) dat[i][j]=toupper(buffer[j]);
    if(compare(dat[i],"NSET=",5)==5) strcpy(set,&buffer[5]);
    if(compare(dat[i],"GENERATE",5)==5) generate=1;
  }
  if(printFlag) printf("*NSET: NSET:%s\n", set);
  setNr=getSetNrx(set);
  if(setNr<0) setNr=pre_setax( set, "i", 0);

  /* scan the node-names and write to file */
  do
  {
    length=abqrecord(rec_str);
    if(length==-1) return(-1);
    if(length==-2) break;
    if(generate)
    {
      nd[0]=nd[1]=0;
      nd[2]=1;
      length=sscanf ( rec_str, "%d, %d, %d", &nd[0],&nd[1],&nd[2]);
      /* write in list format */
      if(!nd[2])
      {
        setax( setNr, "n", nd[0]);
        setax( setNr, "n", nd[1]);
      }
      else
      {
        for(i=nd[0]; i<=nd[1]; i+=nd[2])
        {
          setax( setNr, "n", i);
        }
      }
    }
    else
    {
      args=crecord(rec_str, dat);
      /* extract the type */
      for(i=0; i<args; i++)
      {
        /* write in list format */
        nd[0]=atoi(dat[i]);
        if(nd[0]) setax( setNr, "n", nd[0]);
        else
	{
          nset=getSetNrx(dat[i]);
          if(nset<0)
          {
            printf("ERROR: In *NSET set:%s not defined\n",dat[i]);
            return(-1);
          }
          for(j=0; j<setx[nset].anz_n; j++) setax( setNr, "n", setx[nset].node[j]);
	}
      }
    }
  }while(1); 
  return(1);
}


int getShellSelection(char *rec_str)
{
  int i,j,n, args,length, setNr;
  char elset[MAX_LINE_LENGTH];
  char material[MAX_LINE_LENGTH];
  int ip=5;
  double d=0.;


  if((selected_elems=(int *)realloc((int *)selected_elems,(anzx->emax+1)*sizeof(int )))==NULL)
  {
    printf("\n\n ERROR: realloc failed in getShellSelection\n") ;
    return(1);
  }
  for(i=0; i<=anzx->emax; i++) selected_elems[i]=0; 


  /* get the arguments of the string */
  args=crecord(rec_str, dat);
  
  /* extract the material-name */
  for(i=0; i<args; i++)
  {
    /* delete blanks */
    n=0; for(j=0;j<strlen(dat[i]); j++) if(dat[i][j]!=' ') { buffer[n]=dat[i][j]; n++; }
    buffer[n]='\0';
    for(j=0;j<=strlen(buffer); j++) dat[i][j]=toupper(buffer[j]);
    if(compare(dat[i],"ELSET=",6)==6) strcpy(elset,&buffer[6]);
    if(compare(dat[i],"MATERIAL=",9)==9) strcpy(material,&buffer[9]);
    if((compare(buffer,"NODAL",5)==5)||
       (compare(buffer,"COMPOS",5)==5)||
       (compare(buffer,"ORIENT",5)==5))
    {
      errMsg("WARNING: option:%s not supported, keyword ignored\n", buffer);
      // return(1);
    }
  }
  if(printFlag) printf("*SHELL SECTION: ELSET:%s MATERIAL:%s\n", elset, material);

  /* get the next line */
  length=abqrecord(rec_str);
  if(length==-1) return(-1);
  sscanf(rec_str, "%lf, %d",&d, &ip);
  elprop->nset++;
  elprop->d=d;
  if(printFlag) printf("set:%d d:%lf ip:%d\n",elprop->nset, elprop->d, ip);

  /* get or define a material-number */
  matnr=getMatNr(material,0);

  if(ansFlag) 
  {
    /* write ansys commands */
    setNr=getSetNrx(elset);
    if ( setNr<0 )  { printf ("\nThe set \"%s\" could not be opened. The setdefinition was used before the set was created.\n\n",elset); exit(-1); }
    
    fprintf (handleans,"! ----------- ELSET: %s MAT:%s ---------------\n", elset, material);
    fprintf (handleans, "REAL, %d\n", elprop->nset);
    fprintf (handleans, "R, %d, %lf\n", elprop->nset, elprop->d);
    fprintf (handleans,"MAT, %d\n",matnr); 
    for(i=0; i<setx[setNr].anz_e; i++)
    {
      selected_elems[setx[setNr].elem[i]]=1;
    }
    writeElemAns(matnr, selected_elems, anzx, nodex, elemx);
  }
  return(1);
}


int getStatic(char *rec_str)
{
  int args,length;
  length=abqrecord(rec_str);
  if(length==-1) return(-1);

  /* get the arguments of the string */
  args=crecord(rec_str, dat);
  
  if((args>1)&&(dat[0][0]!='*')) dataset_value=atof(dat[1]);
  else dataset_value=0.;
  return(1);
}

int getSolidSelection(char *rec_str)
{
  int i,j,n, args,length, setNr;
  char elset[MAX_LINE_LENGTH];
  char material[MAX_LINE_LENGTH];


  if((selected_elems=(int *)realloc((int *)selected_elems,(anzx->emax+1)*sizeof(int )))==NULL)
  {
    printf("\n\n ERROR: realloc failed in getSolidSelection\n") ;
    return(1);
  }
  for(i=0; i<=anzx->emax; i++) selected_elems[i]=0; 

  /* get the arguments of the string */
  args=crecord(rec_str, dat);
  
  /* extract the material-name */
  for(i=0; i<args; i++)
  {
    /* delete blanks */
    n=0; for(j=0;j<strlen(dat[i]); j++) if(dat[i][j]!=' ') { buffer[n]=dat[i][j]; n++; }
    buffer[n]='\0';
    for(j=0;j<=strlen(buffer); j++) dat[i][j]=toupper(buffer[j]);
    if(compare(dat[i],"ELSET=",6)==6) strcpy(elset,&buffer[6]);
    if(compare(dat[i],"MATERIAL=",9)==9) strcpy(material,&buffer[9]);
    if((compare(buffer,"COMPOS",5)==5)||
       (compare(buffer,"ORIENT",5)==5))
    {
      errMsg("WARNING: option:%s not supported, keyword ignored\n", buffer);
      /* get the next line */
      /*
      length=abqrecord(rec_str);
      if(length==-1) return(-1);
      return(1);
      */
    }
  }
  if(printFlag) printf("*SOLID SECTION: ELSET:%s MATERIAL:%s\n", elset, material);

  /* get or define a material-number */
  matnr=getMatNr(material,0);
  setNr=getSetNrx(elset);
  if ( setNr<0 )  { printf ("\nThe set \"%s\" could not be opened. The setdefinition was used before the set was created.\n\n",elset); exit(-1); }
  setx[setNr].material=matnr;
  
  if(ansFlag) 
  {
    /* write ansys commands */

    fprintf(handleans,"! ----------- ELSET: %s MAT:%s ---------------\n", elset, material);
    fprintf(handleans,"MAT, %d\n",matnr); 
    for(i=0; i<setx[setNr].anz_e; i++)
    {
      selected_elems[setx[setNr].elem[i]]=1;
    }
    writeElemAns(matnr, selected_elems, anzx, nodex, elemx);
  }

  /* get the next line */
  length=abqrecord(rec_str);
  if(length==-1) return(-1);
  return(1);
}


int getInitialConditions(char *rec_str)
{
  int i,j,n, args, length, setNr;
  int enr,enrold=0,e,p,g=0,v, nnr,nm,n1,n2;
  double enodval[20][6];
  int *vpnode=NULL;

  typedef struct {
    int nr;
    double dat[27][6];
  }Elresult; 
  Elresult *elresult=NULL;

  /* midside node-pos based on frd definition (different to ccx) */
  double nonei10[]={ 5,1,2,6,2,3,7,3,1,8,1,4,9,2,4,10,3,4 };
  double nonei15[]={ 7,1,2,8,2,3,9,3,1,13,4,5,14,5,6,15,6,4,10,1,4,11,2,5,12,3,6 };
  double nonei20[]={ 9,1,2,10,2,3,11,3,4,12,4,1,17,5,6,18,6,7,19,7,8,20,8,5,13,1,5,14,2,6,15,3,7,16,4,8 };

  /* extrapolation factors from gausp to nodep */
  /* f: a8(8,8),a4(4,4),a27(20,27),a9(6,9),a2(6,2) */
  /* c: a8[8][8],a4[4][4],a27[27][20],a9[9][6],a2[2][6] */
  double a2[]={
      1.1455,-0.1455,1.1455,-0.1455,1.1455,-0.1455,
      -0.1455,1.1455,-0.1455,1.1455,-0.1455,1.1455 };
  double a4[]={
      1.92705, -0.30902, -0.30902, -0.30902,
      -0.30902,  1.92705, -0.30902, -0.30902,
      -0.30902, -0.30902,  1.92705, -0.30902,
      -0.30902, -0.30902, -0.30902,  1.92705 };
  double a9[]={
      1.63138,-0.32628,-0.32628,-0.52027, 0.10405, 0.10405,
      -0.32628, 1.63138,-0.32628, 0.10405,-0.52027, 0.10405,
      -0.32628,-0.32628, 1.63138, 0.10405, 0.10405,-0.52027,
       0.55556,-0.11111,-0.11111, 0.55556,-0.11111,-0.11111,
      -0.11111, 0.55556,-0.11111,-0.11111, 0.55556,-0.11111,
      -0.11111,-0.11111, 0.55556,-0.11111,-0.11111, 0.55556,
      -0.52027, 0.10405, 0.10405, 1.63138,-0.32628,-0.32628,
       0.10405,-0.52027, 0.10405,-0.32628, 1.63138,-0.32628,
      0.10405, 0.10405,-0.52027,-0.32628,-0.32628, 1.63138 };
  double a8[]={
      2.549,-.683,.183,-.683,-.683,.183,
      -.04904,.183,-.683,2.549,-.683,.183,
      .183,-.683,.183,-.04904,-.683,.183,
      -.683,2.549,.183,-.04904,.183,-.683,
      .183,-.683,2.549,-.683,-.04904,.183,
      -.683,.183,-.683,.183,-.04904,.183,
      2.549,-.683,.183,-.683,.183,-.683,
      .183,-.04904,-.683,2.549,-.683,.183,
      .183,-.04904,.183,-.683,-.683,.183,
      -.683,2.549,-.04904,.183,-.683,.183,
      .183,-.683,2.549,-.683 } ;  
  double a27[]={
       2.37499,-0.12559,-0.16145,-0.12559,-0.12559,-0.16145, 0.11575,
      -0.16145, 0.32628, 0.11111, 0.11111, 0.32628, 0.11111,-0.10405,
      -0.10405, 0.11111, 0.32628, 0.11111,-0.10405, 0.11111,-0.31246,
      -0.31246, 0.31481, 0.31481, 0.31481, 0.31481,-0.16902,-0.16902,
       1.28439,-0.27072,-0.19444,-0.27072,-0.19444, 0.15961,-0.00661,
       0.15961,-0.27072,-0.27072, 0.15961, 0.15961,-0.12559, 2.37499,
      -0.12559,-0.16145,-0.16145,-0.12559,-0.16145, 0.11575, 0.32628,
       0.32628, 0.11111, 0.11111, 0.11111, 0.11111,-0.10405,-0.10405,
       0.11111, 0.32628, 0.11111,-0.10405,-0.31246, 0.31481, 0.31481,
      -0.31246, 0.31481,-0.16902,-0.16902, 0.31481,-0.27072,-0.19444,
      -0.27072, 1.28439, 0.15961,-0.00661, 0.15961,-0.19444,-0.27072,
       0.15961, 0.15961,-0.27072,-0.48824,-0.48824,-0.48824,-0.48824,
       0.22898, 0.22898, 0.22898, 0.22898, 0.05556, 0.05556, 0.05556,
       0.05556, 0.05556, 0.05556, 0.05556, 0.05556,-0.22222,-0.22222,
      -0.22222,-0.22222, 0.31481,-0.31246,-0.31246, 0.31481,-0.16902,
       0.31481, 0.31481,-0.16902,-0.27072, 1.28439,-0.27072,-0.19444,
       0.15961,-0.19444, 0.15961,-0.00661, 0.15961,-0.27072,-0.27072,
       0.15961,-0.12559,-0.16145,-0.12559, 2.37499,-0.16145, 0.11575,
      -0.16145,-0.12559, 0.11111, 0.11111, 0.32628, 0.32628,-0.10405,
      -0.10405, 0.11111, 0.11111, 0.11111,-0.10405, 0.11111, 0.32628,
       0.31481, 0.31481,-0.31246,-0.31246,-0.16902,-0.16902, 0.31481,
       0.31481,-0.19444,-0.27072, 1.28439,-0.27072,-0.00661, 0.15961,
      -0.19444, 0.15961, 0.15961, 0.15961,-0.27072,-0.27072,-0.16145,
      -0.12559, 2.37499,-0.12559, 0.11575,-0.16145,-0.12559,-0.16145,
       0.11111, 0.32628, 0.32628, 0.11111,-0.10405, 0.11111, 0.11111,
      -0.10405,-0.10405, 0.11111, 0.32628, 0.11111,-0.31246, 0.31481,
      -0.16902, 0.31481,-0.31246, 0.31481,-0.16902, 0.31481,-0.27072,
       0.15961, 0.15961,-0.27072,-0.27072, 0.15961, 0.15961,-0.27072,
       1.28439,-0.19444,-0.00661,-0.19444,-0.48824,-0.48824, 0.22898,
       0.22898,-0.48824,-0.48824, 0.22898, 0.22898, 0.05556,-0.22222,
       0.05556,-0.22222, 0.05556,-0.22222, 0.05556,-0.22222, 0.05556,
       0.05556, 0.05556, 0.05556, 0.31481,-0.31246, 0.31481,-0.16902,
       0.31481,-0.31246, 0.31481,-0.16902,-0.27072,-0.27072, 0.15961,
       0.15961,-0.27072,-0.27072, 0.15961, 0.15961,-0.19444, 1.28439,
      -0.19444,-0.00661,-0.48824, 0.22898, 0.22898,-0.48824,-0.48824,
       0.22898, 0.22898,-0.48824,-0.22222, 0.05556,-0.22222, 0.05556,
      -0.22222, 0.05556,-0.22222, 0.05556, 0.05556, 0.05556, 0.05556,
       0.05556,-0.29630,-0.29630,-0.29630,-0.29630,-0.29630,-0.29630,
      -0.29630,-0.29630,-0.11111,-0.11111,-0.11111,-0.11111,-0.11111,
      -0.11111,-0.11111,-0.11111,-0.11111,-0.11111,-0.11111,-0.11111,
       0.22898,-0.48824,-0.48824, 0.22898, 0.22898,-0.48824,-0.48824,
       0.22898,-0.22222, 0.05556,-0.22222, 0.05556,-0.22222, 0.05556,
      -0.22222, 0.05556, 0.05556, 0.05556, 0.05556, 0.05556, 0.31481,
      -0.16902, 0.31481,-0.31246, 0.31481,-0.16902, 0.31481,-0.31246,
       0.15961, 0.15961,-0.27072,-0.27072, 0.15961, 0.15961,-0.27072,
      -0.27072,-0.19444,-0.00661,-0.19444, 1.28439, 0.22898, 0.22898,
      -0.48824,-0.48824, 0.22898, 0.22898,-0.48824,-0.48824, 0.05556,
      -0.22222, 0.05556,-0.22222, 0.05556,-0.22222, 0.05556,-0.22222,
       0.05556, 0.05556, 0.05556, 0.05556,-0.16902, 0.31481,-0.31246,
       0.31481,-0.16902, 0.31481,-0.31246, 0.31481, 0.15961,-0.27072,
      -0.27072, 0.15961, 0.15961,-0.27072,-0.27072, 0.15961,-0.00661,
      -0.19444, 1.28439,-0.19444,-0.12559,-0.16145, 0.11575,-0.16145,
       2.37499,-0.12559,-0.16145,-0.12559, 0.11111,-0.10405,-0.10405,
       0.11111, 0.32628, 0.11111, 0.11111, 0.32628, 0.32628, 0.11111,
      -0.10405, 0.11111, 0.31481, 0.31481,-0.16902,-0.16902,-0.31246,
      -0.31246, 0.31481, 0.31481,-0.19444, 0.15961,-0.00661, 0.15961,
       1.28439,-0.27072,-0.19444,-0.27072,-0.27072,-0.27072, 0.15961,
       0.15961,-0.16145,-0.12559,-0.16145, 0.11575,-0.12559, 2.37499,
      -0.12559,-0.16145, 0.11111, 0.11111,-0.10405,-0.10405, 0.32628,
       0.32628, 0.11111, 0.11111, 0.11111, 0.32628, 0.11111,-0.10405,
       0.31481,-0.16902,-0.16902, 0.31481,-0.31246, 0.31481, 0.31481,
      -0.31246, 0.15961,-0.00661, 0.15961,-0.19444,-0.27072,-0.19444,
      -0.27072, 1.28439,-0.27072, 0.15961, 0.15961,-0.27072, 0.22898,
       0.22898, 0.22898, 0.22898,-0.48824,-0.48824,-0.48824,-0.48824,
       0.05556, 0.05556, 0.05556, 0.05556, 0.05556, 0.05556, 0.05556,
       0.05556,-0.22222,-0.22222,-0.22222,-0.22222,-0.16902, 0.31481,
       0.31481,-0.16902, 0.31481,-0.31246,-0.31246, 0.31481, 0.15961,
      -0.19444, 0.15961,-0.00661,-0.27072, 1.28439,-0.27072,-0.19444,
       0.15961,-0.27072,-0.27072, 0.15961,-0.16145, 0.11575,-0.16145,
      -0.12559,-0.12559,-0.16145,-0.12559, 2.37499,-0.10405,-0.10405,
       0.11111, 0.11111, 0.11111, 0.11111, 0.32628, 0.32628, 0.11111,
      -0.10405, 0.11111, 0.32628,-0.16902,-0.16902, 0.31481, 0.31481,
       0.31481, 0.31481,-0.31246,-0.31246,-0.00661, 0.15961,-0.19444,
       0.15961,-0.19444,-0.27072, 1.28439,-0.27072, 0.15961, 0.15961,
      -0.27072,-0.27072, 0.11575,-0.16145,-0.12559,-0.16145,-0.16145,
      -0.12559, 2.37499,-0.12559,-0.10405, 0.11111, 0.11111,-0.10405,
       0.11111, 0.32628, 0.32628, 0.11111,-0.10405, 0.11111, 0.32628,
       0.11111 };



  /* get the arguments of the string */
  args=crecord(rec_str, dat);
  
  /* extract the name */
  for(i=0; i<args; i++)
  {
    /* delete blanks */
    n=0; for(j=0;j<strlen(dat[i]); j++) if(dat[i][j]!=' ') { buffer[n]=dat[i][j]; n++; }
    buffer[n]='\0';
    for(j=0;j<=strlen(buffer); j++) dat[i][j]=toupper(buffer[j]);
    if(compare(dat[i],"TYPE=",5)==5)
    {
      strcpy(dat[i],&buffer[5]);
      /* delete blanks */
      n=0; for(j=0;j<strlen(dat[i]); j++) if(dat[i][j]!=' ') { buffer[n]=dat[i][j]; n++; }
      buffer[n]='\0';
      for(j=0;j<=strlen(buffer); j++) dat[i][j]=toupper(buffer[j]);
      if(printFlag) printf("*INITIAL CONDITIONS: TYPE:%s \n", buffer);

      if(compare(dat[i],"DISP",4)==4)
      {
        if(ansFlag) 
        {
	    printf("ERROR: type:%s is not supportet for ansys\n", dat[i]);
        }
        else getInitialVector(rec_str,dat[i]);
        return(1);
      }
      else if(compare(dat[i],"FLUID",4)==4)
      {
        if(ansFlag) 
        {
	    printf("ERROR: type:%s is not supportet for ansys\n", dat[i]);
        }
        else getInitialVector(rec_str,dat[i]);
        return(1);
      }
      else if(compare(dat[i],"TEMPERATURE",6)==6)
      {
        if(ansFlag) 
        {
          /* get the next line */
          length=abqrecord(rec_str);
          if(length==-1) return(-1);
          if(length==-2) return(1);

          /* get the arguments of the string */
          args=crecord(rec_str, dat);

          /* extract the set-name */
          /* delete blanks */
          n=0; for(j=0;j<strlen(dat[0]); j++) if(dat[0][j]!=' ') { buffer[n]=dat[0][j]; n++; }        buffer[n]='\0';
  
          if((selected_elems=(int *)realloc((int *)selected_elems,(anzx->nmax+1)*sizeof(int )))==NULL)
          {
            printf("\n\n ERROR: realloc failed in getSolidSelection\n") ;
            return(1);
          }
          for(j=0; j<anzx->nmax; j++) selected_elems[j]=0;
  
          /* check if a setname or a node-number is provided */
          if(atoi(buffer)>0)
          { 
            length=abqrecord(rec_str);
  
            if(length==-1) return(-1);
            if(length==-2) break;
            args=crecord(rec_str, dat);
            selected_elems[atoi(dat[0])]=1;
          }
          else
          {
            /* open the file which contains the affected nodes */
            setNr=getSetNrx(buffer);
            if ( setNr<0 )  { printf ("\nThe set \"%s\" could not be opened. The setdefinition was used before the set was created.\n\n", buffer); exit(-1); }
            for(i=0; i<setx[setNr].anz_n; i++)
            { 
              selected_elems[setx[setNr].node[i]]=1;
            }
          }
        
          /* write ansys */
          for(j=0; j<anzx->n; j++) if(selected_elems[nodex[j].nr]==0)
	  {
            printf("WARNING: TREF not unique, no TREF written for set:%s. n:%d ntot:%d\n", buffer, j,anzx->n);
            return(1);
          }
          fprintf(handleans,"! ----------- INITIAL CONDITIONS, SET:%s ---------------\n", buffer);
          fprintf(handleans, "TREF, %s\n",dat[1]);
        }
        else getScalarvalue(rec_str,dat[i]);
        return(1);
      }
      else if(compare(dat[i],"PRES",4)==4)
      {
        if(ansFlag) 
        {
	    printf("ERROR: type:%s is not supportet for ansys\n", dat[i]);
        }
        else getScalarvalue(rec_str,dat[i]);
        return(1);
      }
      else if(compare(dat[i],"TOTA",4)==4)
      {
        if(ansFlag) 
        {
	    printf("ERROR: type:%s is not supportet for ansys\n", dat[i]);
        }
        else getScalarvalue(rec_str,dat[i]);
        return(1);
      }
      else if(compare(dat[i],"MASS",4)==4)
      {
        if(ansFlag) 
        {
	    printf("ERROR: type:%s is not supportet for ansys\n", dat[i]);
        }
        else getScalarvalue(rec_str,dat[i]);
        return(1);
      }
      else if((compare(dat[i],"STRESS",6)==6) || (compare(dat[i],"PLASTICSTRAIN",13)==13) || (compare(dat[i],"PLASTIC STRAIN",14)==14))
      {
        /* create a new dataset */
        if(compare(dat[i],"STRESS",6)==6) sprintf( lcasex[anzx->l].name,"STRESS");
        if(compare(dat[i],"PLASTICSTRAIN",13)==13) sprintf( lcasex[anzx->l].name,"PE");
        if(compare(dat[i],"PLASTIC STRAIN",14)==14) sprintf( lcasex[anzx->l].name,"PE");
        printf (" create dataset:%d name= %s\n", anzx->l, lcasex[anzx->l].name);
        lcasex[anzx->l].value=0;          
        lcasex[anzx->l].ncomps=6;
        lcasex[anzx->l].irtype=1;
        lcasex[anzx->l].npheader=0;
      
        strcpy(lcasex[anzx->l].analysis_name,"");
        sprintf(lcasex[anzx->l].dataset_name,"STP %d", step_nr);
        strcpy(lcasex[anzx->l].dataset_text,"INITIAL");
        lcasex[anzx->l].step_number=step_nr;
        lcasex[anzx->l].analysis_type=1;
      
        if ( (lcasex[anzx->l].nmax = (int *)malloc( lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcasex[anzx->l].nmin = (int *)malloc( lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcasex[anzx->l].max = (float *)malloc(lcasex[anzx->l].ncomps * sizeof(float))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcasex[anzx->l].min = (float *)malloc(lcasex[anzx->l].ncomps * sizeof(float))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcasex[anzx->l].dat = (float **)malloc(lcasex[anzx->l].ncomps * sizeof(float *))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcasex[anzx->l].compName = (char **)malloc(lcasex[anzx->l].ncomps * sizeof(char *))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcasex[anzx->l].icname = (char **)malloc(lcasex[anzx->l].ncomps * sizeof(char *))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcasex[anzx->l].menu = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcasex[anzx->l].ictype = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcasex[anzx->l].icind1 = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcasex[anzx->l].icind2 = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcasex[anzx->l].iexist = (int *)malloc(lcasex[anzx->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
      
        for(j=0; j<lcasex[anzx->l].ncomps; j++)
        {
          if ( (lcasex[anzx->l].dat[j] = (float *)calloc( (anzx->nmax+1), sizeof(float))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );	               
          if ( (lcasex[anzx->l].compName[j] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
            printf("\n\n ERROR: malloc failed\n\n" );
          if ( (lcasex[anzx->l].icname[j] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
            printf("\n\n ERROR: malloc failed\n\n" );
          lcasex[anzx->l].max[j]=-MAX_FLOAT;
          lcasex[anzx->l].min[j]=MAX_FLOAT;
      
          lcasex[anzx->l].menu[j] = 1;
          lcasex[anzx->l].ictype[j] = 1;
          lcasex[anzx->l].icind1[j] = 0;
          lcasex[anzx->l].icind2[j] = 0;
          lcasex[anzx->l].iexist[j] = 0;
        }
        if(compare(dat[i],"STRESS",6)==6)
        {
          strcpy ( lcasex[anzx->l].compName[0], "SXX   ");
          strcpy ( lcasex[anzx->l].compName[1], "SYY   ");
          strcpy ( lcasex[anzx->l].compName[2], "SZZ   ");
          strcpy ( lcasex[anzx->l].compName[3], "SXY   ");
          strcpy ( lcasex[anzx->l].compName[4], "SXZ   ");
          strcpy ( lcasex[anzx->l].compName[5], "SYZ   ");
	}
        if(compare(dat[i],"PLASTICSTRAIN",13)==13)
        {
          strcpy ( lcasex[anzx->l].compName[0], "EXX   ");
          strcpy ( lcasex[anzx->l].compName[1], "EYY   ");
          strcpy ( lcasex[anzx->l].compName[2], "EZZ   ");
          strcpy ( lcasex[anzx->l].compName[3], "EXY   ");
          strcpy ( lcasex[anzx->l].compName[4], "EXZ   ");
          strcpy ( lcasex[anzx->l].compName[5], "EYZ   ");
	}
        if ( (vpnode = (int *)calloc( (anzx->nmax+1), sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );	               

        /* read elem-nr, all gaus-vals and extrapolate to the nodes */
        if ( (elresult = (Elresult *)realloc( (Elresult *)elresult, (anzx->emax+1) * sizeof(Elresult))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );

        e=-1;
        do{
            length=abqrecord(rec_str);
            if(length==-1)
            { 
              anzx->l++;
              if ( (lcasex = (Datasets *)realloc((Datasets *)lcasex, (anzx->l+2) * sizeof(Datasets))) == NULL )
              { printf("\n\n ERROR: malloc failure\n\n" ); exit(1); }
              return(-1);
            }
            if(length==-2) break;
            args=crecord(rec_str, dat);
	    if(args!=8) return(-1);
	    enr=atoi(dat[0]);
	    //printf("enr:%d e:%d :%d\n",enr,e,anzx->e);
            if (enr!= enrold)
            {
              enrold=enr;
              e++;
	      elresult[e].nr=enr;
	    }
            p=atoi(dat[1])-1;
            elresult[e].dat[p][0]=atof(dat[2]);
            elresult[e].dat[p][1]=atof(dat[3]);
            elresult[e].dat[p][2]=atof(dat[4]);
            elresult[e].dat[p][3]=atof(dat[5]);
            elresult[e].dat[p][4]=atof(dat[6]);
            elresult[e].dat[p][5]=atof(dat[7]);
        }while(1); 

        /* extrapolate to the node-pos */
        for(e=0; e<anzx->e; e++)
	{
          for(n=0; n<20; n++) for(v=0; v<6; v++) enodval[n][v]=0.;

          /* he8 */
          if((e_enqirex[elresult[e].nr].type==1)&&(e_enqirex[elresult[e].nr].attr==1))
	  { nnr=8;  for(n=0; n<8; n++) for(v=0; v<6; v++) enodval[n][v]+=elresult[e].dat[g][v]; }
          else if((e_enqirex[elresult[e].nr].type==1)&&(e_enqirex[elresult[e].nr].attr==0))
          { nnr=8;  for(n=0; n<8; n++) for(g=0; g<8; g++) for(v=0; v<6; v++) enodval[n][v]+=a8[g*8+n]*elresult[e].dat[g][v]; }

          /* he20 */
          else if((e_enqirex[elresult[e].nr].type==4)&&(e_enqirex[elresult[e].nr].attr==1))
          {
            nnr=20;  
            for(n=0; n<8; n++) for(g=0; g<8; g++) for(v=0; v<6; v++) enodval[n][v]+=a8[g*8+n]*elresult[e].dat[g][v]; 
            for(n=8; n<20; n++)
            {
              nm=nonei20[(n-8)*3]-1;
              n1=nonei20[(n-8)*3+1]-1;
              n2=nonei20[(n-8)*3+2]-1;
              for(v=0; v<6; v++) enodval[nm][v]=(enodval[n1][v]+enodval[n2][v])*.5;
            }
	  }
          else if((e_enqirex[elresult[e].nr].type==4)&&(e_enqirex[elresult[e].nr].attr==0))
          {
            nnr=20;
            for(n=0; n<12; n++) for(g=0; g<27; g++) for(v=0; v<6; v++) enodval[n][v]+=a27[g*20+n]*elresult[e].dat[g][v];
            for(n=12; n<16; n++) for(g=0; g<27; g++) for(v=0; v<6; v++) enodval[n][v]+=a27[g*20+n+4]*elresult[e].dat[g][v];
            for(n=16; n<20; n++) for(g=0; g<27; g++) for(v=0; v<6; v++) enodval[n][v]+=a27[g*20+n-4]*elresult[e].dat[g][v];
          }

          /* TET4 and TET10 */
          else if((e_enqirex[elresult[e].nr].type==3)&&(e_enqirex[elresult[e].nr].attr==0))
	  { nnr=4;  for(n=0; n<4; n++) for(v=0; v<6; v++) enodval[n][v]+=elresult[e].dat[g][v]; }
          else if((e_enqirex[elresult[e].nr].type==6)&&(e_enqirex[elresult[e].nr].attr==0))
          { 
            nnr=10;
            for(n=0; n<4; n++) for(g=0; g<4; g++) for(v=0; v<6; v++) enodval[n][v]+=a4[g*4+n]*elresult[e].dat[g][v];
            for(n=4; n<10; n++)
            {
              nm=nonei10[(n-4)*3]-1;
              n1=nonei10[(n-4)*3+1]-1;
              n2=nonei10[(n-4)*3+2]-1;
              for(v=0; v<6; v++) enodval[nm][v]=(enodval[n1][v]+enodval[n2][v])*.5;
            }
          }

	  /* PE6 and PE15 */
          else if((e_enqirex[elresult[e].nr].type==2)&&(e_enqirex[elresult[e].nr].attr==0))
          { nnr=6;  for(n=0; n<6; n++) for(g=0; g<2; g++) for(v=0; v<6; v++) enodval[n][v]+=a2[g*6+n]*elresult[e].dat[g][v]; }
          else if((e_enqirex[elresult[e].nr].type==5)&&(e_enqirex[elresult[e].nr].attr==0))
          { 
            nnr=15;
            for(n=0; n<6; n++) for(g=0; g<9; g++) for(v=0; v<6; v++) enodval[n][v]+=a9[g*6+n]*elresult[e].dat[g][v];
            for(n=6; n<15; n++)
            {
              nm=nonei15[(n-6)*3]-1;
              n1=nonei15[(n-6)*3+1]-1;
              n2=nonei15[(n-6)*3+2]-1;
              for(v=0; v<6; v++) enodval[nm][v]=(enodval[n1][v]+enodval[n2][v])*.5;
	      //printf("el:%d n: %d %d %d\n",elresult[e].nr,e_enqirex[elresult[e].nr].nod[n1],e_enqirex[elresult[e].nr].nod[nm],e_enqirex[elresult[e].nr].nod[n2]);
            }
          }

          else goto jumpElem;

          /* add the node-vals per node and count the values per node */
          for(n=0; n<nnr; n++)
          {
            for(v=0; v<6; v++) lcasex[anzx->l].dat[v][e_enqirex[elresult[e].nr].nod[n]]+=enodval[n][v];
            vpnode[e_enqirex[elresult[e].nr].nod[n]]++;
	    //printf("n:%d e:%d nod:%d nrv:%d\n", n, elresult[e].nr,e_enqirex[elresult[e].nr].nod[n], vpnode[e_enqirex[elresult[e].nr].nod[n]]); 
	  }

	jumpElem:;
	}

        /* divide the node-vals by the number of values per node */  
        for(n=0; n<anzx->n; n++)
	{
          if(vpnode[nodex[n].nr]) for(v=0; v<6; v++) lcasex[anzx->l].dat[v][nodex[n].nr]/=vpnode[nodex[n].nr];
	}

        /* get max and min vals */
        for(n=0; n<anzx->n; n++) for(v=0; v<6; v++)
        {
          if(lcasex[anzx->l].dat[v][nodex[n].nr] > lcasex[anzx->l].max[v])
          {
            lcasex[anzx->l].max[v]=lcasex[anzx->l].dat[v][nodex[n].nr];
            lcasex[anzx->l].nmax[v]=nodex[n].nr;
          }
          if(lcasex[anzx->l].dat[v][nodex[n].nr] < lcasex[anzx->l].min[v])
          {
            lcasex[anzx->l].min[v]=lcasex[anzx->l].dat[v][nodex[n].nr];
            lcasex[anzx->l].nmin[v]=nodex[n].nr;
          }
	}

        anzx->l++;
        if ( (lcasex = (Datasets *)realloc((Datasets *)lcasex, (anzx->l+2) * sizeof(Datasets))) == NULL )
        { printf("\n\n ERROR: malloc failure\n\n" ); exit(1); }
        free(vpnode);
        free(elresult);
        return(1);
      }
      else 
      {
        errMsg("WARNING: option:%s not supported, keyword ignored\n", buffer);
        /* get the next line */
        length=abqrecord(rec_str);
        if(length==-1) return(-1);
        return(1);
      }
    }
  }
  /* get the next line */
  length=abqrecord(rec_str);
  if(length==-1) return(-1);
  return(1);
}


int getFilePointer( int filesopen, FILE **openfile, char *rec_str)
{
  int i,j,n,args,setFlag=0, length;
  char set[MAX_LINE_LENGTH];
  FILE *handle;

  /* get the arguments of the string */
  args=crecord(rec_str, dat);
  
  /* extract the filename */
  sprintf(set, "NONE");
  for(i=0; i<args; i++)
  {
    /* delete blanks */
    n=0; for(j=0;j<strlen(dat[i]); j++) if(dat[i][j]!=' ') { buffer[n]=dat[i][j]; n++; }
    buffer[n]='\0';
    if(compare(buffer,"INPUT=",6)==6) { setFlag=1; strcpy(set,&buffer[6]); } 
    if(compare(buffer,"input=",6)==6) { setFlag=1; strcpy(set,&buffer[6]); } 
    if(compare(buffer,"Input=",6)==6) { setFlag=1; strcpy(set,&buffer[6]); } 
  }
  if(!setFlag) 
  { 
    printf(" WARNING: No input file provided %s\n", rec_str);
    length=abqrecord(rec_str);
    if(length==-1) return(filesopen);
    return(filesopen);
  }
  if(printFlag) printf("*INCLUDE: FILE=%s\n", set);

  /* open input file */
  handle = fopen(set, "r");
  if ( handle== NULL ) { printf ("\nERROR: The input file \"%s\" could not be opened.\n\n",set); fileOpenError=1; return(filesopen); }
  else 
  {
    handlein=handle;
    openfile[filesopen]=handlein; filesopen++;
  }

  return(filesopen);
}



int compareElfaces(Elfaces *a, Elfaces *b)
{
  /* wird von qsort aufgerufen, vergleicht int-Felder */

  if ( a[0].e < b[0].e )
    return -1 ;
  else if ( a[0].e > b[0].e )
    return 1 ;
  else if ( a[0].f < b[0].f )
    return -1 ;
  else if ( a[0].f > b[0].f )
    return 1 ;
  else
    return 0 ;
}



void generateNodeValuesFromFaces( void )
{
  int i,j, setNr, anzx_l=0;
  int sector;
  char *string;

  /* go over all sets and perform due actions */
  for(setNr=0; setNr<anzx->sets; setNr++)
  {
    if(setx[setNr].name!=NULL)
    {
      /* is a certain dataset referenced? Only then create the additional data */
      string=strstr(setx[setNr].name,"ds");
      //printf("set:%s str:%s\n",setx[setNr].name,string);

      if((setx[setNr].anz_elf)&&(string))
      {
        printf(" generate node-values for set:%s\n", setx[setNr].name);

        anzx_l=atoi(&string[2]);
        *(string-1)=0;

        /* is it valid for a certain sector? */
        string=strstr(setx[setNr].name,"_s");
        if(string) sector=atoi(&string[2]);

        /* ini lcase */
        sprintf( lcasex[anzx_l].name,"%s",setx[setNr].name);
        printf (" fill dataset:%d name= %s\n", anzx_l, lcasex[anzx_l].name);
        lcasex[anzx_l].ncomps=setx[setNr].elf[0].n+1;
	printf(" ncomps:%d\n",lcasex[anzx_l].ncomps); 
        lcasex[anzx_l].irtype=1;
        lcasex[anzx_l].npheader=0;
        lcasex[anzx_l].value=0;
      
        strcpy(lcasex[anzx_l].analysis_name,"");
        strcpy(lcasex[anzx_l].dataset_text,"");
        //lcasex[anzx_l].step_number=step_nr;
        sprintf(lcasex[anzx_l].dataset_name,"STP %d", lcasex[anzx_l].step_number);
        lcasex[anzx_l].analysis_type=1;
      
        if ( (lcasex[anzx_l].nmax = (int *)malloc( lcasex[anzx_l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcasex[anzx_l].nmin = (int *)malloc( lcasex[anzx_l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcasex[anzx_l].max = (float *)malloc(lcasex[anzx_l].ncomps * sizeof(float))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcasex[anzx_l].min = (float *)malloc(lcasex[anzx_l].ncomps * sizeof(float))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcasex[anzx_l].dat = (float **)malloc(lcasex[anzx_l].ncomps * sizeof(float *))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcasex[anzx_l].compName = (char **)malloc(lcasex[anzx_l].ncomps * sizeof(char *))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcasex[anzx_l].icname = (char **)malloc(lcasex[anzx_l].ncomps * sizeof(char *))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcasex[anzx_l].menu = (int *)malloc(lcasex[anzx_l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcasex[anzx_l].ictype = (int *)malloc(lcasex[anzx_l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcasex[anzx_l].icind1 = (int *)malloc(lcasex[anzx_l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcasex[anzx_l].icind2 = (int *)malloc(lcasex[anzx_l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcasex[anzx_l].iexist = (int *)malloc(lcasex[anzx_l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
      
        for(i=0; i<lcasex[anzx_l].ncomps; i++)
        {
          if ( (lcasex[anzx_l].dat[i] = (float *)calloc( (anzx->nmax+1), sizeof(float))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );	               
          if ( (lcasex[anzx_l].compName[i] = (char *)calloc( MAX_LINE_LENGTH, sizeof(char))) == NULL )
            printf("\n\n ERROR: malloc failed\n\n" );
          if ( (lcasex[anzx_l].icname[i] = (char *)calloc( MAX_LINE_LENGTH, sizeof(char))) == NULL )
            printf("\n\n ERROR: malloc failed\n\n" );
          lcasex[anzx_l].menu[i] = 1;
          lcasex[anzx_l].ictype[i] = 1;
          lcasex[anzx_l].icind1[i] = i+1;
          lcasex[anzx_l].icind2[i] = 0;
          lcasex[anzx_l].iexist[i] = 0;
          lcasex[anzx_l].max[i]=-MAX_FLOAT;
          lcasex[anzx_l].min[i]=MAX_FLOAT;
          sprintf ( lcasex[anzx_l].compName[i], "val%d",i+1);
        }
        lcasex[anzx_l].iexist[0] = -1;
        lcasex[anzx_l].menu[i-1] = 0;
        strcpy ( lcasex[anzx_l].compName[i-1], "nodes");

        
        /* combine loads from same faces first! */
        /* sort according to elem, face */
        qsort( setx[setNr].elf, setx[setNr].anz_elf, sizeof(Elfaces), (void *)compareElfaces );

        /* if previous elem has same nr and face, add the values to this one and set n to 0 of the previous one */ 
        for(i=1; i<setx[setNr].anz_elf; i++)
        {
          if((setx[setNr].elf[i].e==setx[setNr].elf[i-1].e) && (setx[setNr].elf[i].f==setx[setNr].elf[i-1].f))
	  {
            for(j=0; j<setx[setNr].elf[i].n; j++) setx[setNr].elf[i].v[j]+=setx[setNr].elf[i-1].v[j];
            setx[setNr].elf[i-1].n=0;
	  }
	}

        for(i=0; i<setx[setNr].anz_elf; i++)
        {
          writeFaceVals(anzx_l, setx[setNr].elf[i].e, setx[setNr].elf[i].f, setx[setNr].elf[i].n, setx[setNr].elf[i].v,setx[setNr].name);
	}
      }
    }
  }  
}



int readccx(char *datin, Summen *apre, Sets **sptr, Nodes **nptr, Elements **eptr, Datasets **lptr )
{
  int i, j, jj, l, nr, length;
  char rec_str[MAX_LINE_LENGTH];
  char command[MAX_LINE_LENGTH];

  annotation();

  t1=clock() ;

  anzx=apre;
  setx=*sptr;
  //nodex=*nptr;
  //elemx=*eptr;
  nodex=NULL;
  elemx=NULL;
  lcasex=*lptr;
  anz_fb=-1;
  step_flag=0;
#if ANSYS
  ansFlag=1;
  /* open the ansys file */
  handleans = fopen("ccx.ans", "w");
  elprop->nset=0;
  fprintf(handleans, "/PREP7\n");
  fprintf(handleans, "SHPP, OFF\n");
#endif
#if TEST
  printFlag=1; 
#endif
#if NODE_OFFSET
  copyFlag=1;
  handle_equ = fopen ("cgx.equ", "w");
  if ( handle_equ== NULL )  { printf ("\nThe file cgx.equ could not be opened.\n"); return(-1); }
#endif

  /* allocate space */

  node_field_size=INI_FIELD_SIZE;
  do
  {
    if ( (nodex = (Nodes *)realloc( (Nodes *)nodex, (node_field_size+1) * sizeof(Nodes))) == NULL )
    {
      printf("WARNING: in readccx() is INI_FIELD_SIZE:%d to large and is reduced\n", node_field_size );
      node_field_size/=2;
    }
    if(node_field_size<100)
    {
      printf("\n\n ERROR: not enough memory in readccx()\n\n");
      exit(0);
    }
    for(i=0; i<node_field_size; i++) nodex[i].indx=-1;
  }while(!nodex);

  elem_field_size=INI_FIELD_SIZE;
  elem_field_size2=INI_FIELD_SIZE;
  do
  {
    if((elemx = (Elements *)realloc( (Elements *)elemx, (elem_field_size+1) * sizeof(Elements))) == NULL )
    {
      printf("WARNING: in readccx() is INI_FIELD_SIZE:%d to large and is reduced\n", elem_field_size );
      elem_field_size/=2;
    }
    if(elem_field_size<100)
    {
      printf("\n\n ERROR: not enough memory in readccx()\n\n");
      exit(-1);
    }
  }while(!elemx);
  do
  {
    if((e_enqirex=(Elements *)realloc((Elements *)e_enqirex,(elem_field_size2+1)*sizeof(Elements)))==NULL)
    {
      printf("WARNING: in readccx() is INI_FIELD_SIZE:%d to large and is reduced\n", elem_field_size2 );
      elem_field_size2/=2;
    }
    if(elem_field_size2<100)
    {
      printf("\n\n ERROR: not enough memory in readccx()\n\n");
      exit(-1);
    }
  }while(!e_enqirex);

  if ( (lcasex = (Datasets *)realloc((Datasets *)lcasex, (anzx->l+2) * sizeof(Datasets))) == NULL )
    printf("\n\n ERROR: realloc failed\n\n") ;

  if((dat = (char **)realloc((char **)dat, (DAT_SIZE)*sizeof(char*)))==NULL)
  errMsg("\n\n ERROR: realloc failed for **dat\n" );
  for (i=0; i<DAT_SIZE; i++)
  {
    dat[i]=NULL;
    if((dat[i] = (char *)realloc((char *)dat[i], (MAX_LINE_LENGTH)*sizeof(char)))==NULL)
      errMsg("\n\n ERROR: realloc failed for *dat\n" );
  }

  /* scan through the input-file and look for keywords */

  handlein = fopen(datin, "r");
  if ( handlein== NULL )  { printf ("\nThe input file \"%s\" could not be opened.\n\n",datin); return(-1);}
  else { filesopen=1; openfile[0]=handlein; }

  do
  {
    /* all subroutines return eventually the next keyword which could be any. Therefore check again */
    length=abqrecord(rec_str);
    //printf("Length:%d string:%s\n",length,rec_str);
    if(length==-1) break;
    checknextstr1:;

    /* get the arguments of the string */
    crecord(rec_str, dat);

    /* toupper */
    i=0; while(dat[0][i]!='\0') { dat[0][i]=toupper(dat[0][i]); i++; }

    /* get the nodes */
    if (compare(dat[0], "*NODE",5)==5) { length=getNodes(rec_str); goto checknextstr1; }

    /* get the elements */
    else if (compare(dat[0], "*ELEMENT",8)==8) { length=getElements(rec_str); goto checknextstr1; }

    /* get the set-definitions */
    else if (compare(dat[0], "*ELSET",6)==6) { length=getElset(rec_str); goto checknextstr1; }
    else if (compare(dat[0], "*NSET",5)==5) { length=getNset(rec_str); goto checknextstr1; }
  }while(length!=-1);
  fclose(handlein);
 
  node_field_size=anzx->nmax+1;
  if((nodex = (Nodes *)realloc( (Nodes *)nodex, node_field_size * sizeof(Nodes))) == NULL )
    printf("\n\n ERROR: realloc failed\n\n") ;
  else
    printf ("\n %d nodes reallocated \n",anzx->nmax);
  elem_field_size=anzx->e+1;
  if ( (elemx = (Elements *)realloc((Elements *)elemx, elem_field_size * sizeof(Elements))) == NULL )
    printf("\n\n ERROR: in readccx realloc failed\n\n") ;
  else
    printf ("\n %d elements reallocated \n", anzx->e);

  handlein = fopen(datin, "r");
  if ( handlein== NULL )  { printf ("\nThe input file \"%s\" could not be opened.\n\n",datin); return(-1);}
  else { filesopen=1; openfile[0]=handlein; }

  do
  {
    /* all subroutines return eventually the next keyword which could be any. Therefore check again */
    length=abqrecord(rec_str);
    if(length==-1) break;
    checknextstr:;

    /* get the arguments of the string */
    crecord(rec_str, dat);

    /* toupper */
    i=0; while(dat[0][i]!='\0') { dat[0][i]=toupper(dat[0][i]); i++; }

    /* get the cload */
    if (compare(dat[0], "*CLOAD",5)==5) { length=getCload(rec_str); goto checknextstr; }

    /* get the cflux */
    else if (compare(dat[0], "*CFLUX",5)==5) { length=getCflux(rec_str); goto checknextstr; }

    /* get the dload */
    else if (compare(dat[0], "*DLOAD",5)==5) { length=getDload(rec_str); goto checknextstr; }

    /* get the radiation */
    else if (compare(dat[0], "*RADIATION",5)==5) { length=getRadiation(rec_str); goto checknextstr; }

    /* get the film */
    else if (compare(dat[0], "*FILM",5)==5) { length=getFilm(rec_str); goto checknextstr; }

    /* get the massflow */
    else if ((compare(dat[0], "*MASSFLOW",9)==9)||(compare(dat[0], "*MASS FLOW",9)==9)) { length=getMassflow(rec_str); goto checknextstr; }

    /* get the dflux */
    else if (compare(dat[0], "*DFLUX",5)==5) { length=getDflux(rec_str); goto checknextstr; }

    /* get the heading */
    else if (compare(dat[0], "*HEADING",7)==7)
    {
      strcpy(rec_str,"TEXT ");
      length=abqrecord(&rec_str[5]);
      strcpy(command,"TEXT");
      commandoInterpreter( command, rec_str, 4, 0, 0, 0, 0 );
      goto checknextstr;
    }

    /* get step-info */
    else if (compare(dat[0], "*STEP",5)==5) { step_nr++; step_flag=1; }
    //else if (compare(dat[0], "*ENDSTEP",8)==8) { endstep(); }
    else if (compare(dat[0], "*STATIC",7)==7) { length=getStatic(rec_str); goto checknextstr; }

    /* get the surfaces and contact pairs */
    else if (compare(dat[0], "*SURFACE",7)==7) { length=getSurfaces(rec_str); goto checknextstr; }
    else if (compare(dat[0], "*CONTACTPAIR",11)==11) { length=getContactPair(rec_str); goto checknextstr; }
    else if (compare(dat[0], "*TIE",4)==4) { length=getContactPair(rec_str); goto checknextstr; }

    /* get the initial conditions */
    else if (compare(dat[0], "*INITIALCONDITIONS",18)==18) { length=getInitialConditions(rec_str); goto checknextstr; }

    /* get the boundaries */
    else if (compare(dat[0], "*BOUNDARYF",10)==10) { length=getSpcf(rec_str); goto checknextstr; }
    else if (compare(dat[0], "*BOUNDARY",9)==9) { length=getBoundaries(rec_str); goto checknextstr; }

    /* get the equations */
    else if (compare(dat[0], "*EQUATIONF",10)==10) { length=getEquationf(rec_str); goto checknextstr; }
    else if (compare(dat[0], "*EQUATION",9)==9) { length=getEquations(rec_str); goto checknextstr; }

    /* get the mpc */
    else if (compare(dat[0], "*MPC",4)==4) { length=getMpc(rec_str); goto checknextstr; }

    /* open the material-file */
    else if (compare(dat[0], "*MATERIAL",9)==9) { length=getMaterial(rec_str); goto checknextstr; }

    /* get the material-definitions */
    else if (compare(dat[0], "*ELASTIC",8)==8) { length=getElastic(rec_str); goto checknextstr; }
    else if (compare(dat[0], "*EXPANSION",8)==8) { length=getExpansion(rec_str); goto checknextstr; }
    else if (compare(dat[0], "*CONDUCTIVITY",8)==8) { length=getConductivity(rec_str); goto checknextstr; }
    else if (compare(dat[0], "*SPECIFICHEAT",8)==8) { length=getSpecificHeat(rec_str); goto checknextstr; }
    else if (compare(dat[0], "*DENSITY",8)==8) { length=getDensity(rec_str); goto checknextstr; }

    /* get the amplitude-definitions */
    else if (compare(dat[0], "*AMPLITUDE",6)==6) { length=getAmplitude(rec_str); goto checknextstr; }

    /* asign material to sets */
    else if (compare(dat[0], "*SHELLSECTION",13)==13) { length=getShellSelection(rec_str); goto checknextstr; }
    else if (compare(dat[0], "*SOLIDSECTION",13)==13) { length=getSolidSelection(rec_str); goto checknextstr; }

    /* asign TEMPERATURE to sets */
    else if (compare(dat[0], "*TEMPERATURE",12)==12) { length=getScalarvalue(rec_str, &dat[0][1]); goto checknextstr; }

    /* get the coordinate systems */
    else if (compare(dat[0], "*TRANSFORM",7)==7) { length=getTransform(rec_str); goto checknextstr; }


    /* jump the nodes */
    else if (compare(dat[0], "*NODE",5)==5) ;

    /* jump the elements */
    else if (compare(dat[0], "*ELEMENT",8)==8) ;

    /* jump the set-definitions */
    else if (compare(dat[0], "*ELSET",6)==6) ;
    else if (compare(dat[0], "*NSET",5)==5) ;

    else 
    {
      if ((dat[0][0]=='*')&&(dat[0][1]!='*')) printf("WARNING: %s not known. Data ignored.\n", dat[0]);
    }
  }while(length!=-1);
  fclose(handlein);

  /* reset temporary setpointer */
  resetFlag=1;
  getBoundaries(0);
  getEquations(0);
  getContactPair(0);
  writeElemAns(0,0,0,0,0);
  getCload(0);
  getCflux(0);
  getDload(0);
  getDflux(0);
  getMassflow(0);
  getRadiation(0);
  getFilm(0);
  resetFlag=0;

  /*
  t2=clock() ;
  printf("\nread in %.2f seconds\n", ((double)t2 - (double)t1)/(double)CLOCKS_PER_SEC) ;
  */

  /* check if nodes or elements were found */
  if(anzx->nmax==-MAX_INTEGER)
  {
    /* no nodes allocated */
    anzx->nmax=anzx->nmin=0;
  }
  if(anzx->emax==-MAX_INTEGER)
  {
    /* no elements allocated */
    anzx->emax=anzx->emin=0;
  }
  if(fileOpenError)
  {
    /* no nodes allocated */
    anzx->nmax=anzx->nmin=0;
    /* no elements allocated */
    anzx->emax=anzx->emin=0;
  }

  /* generate node-values (additional Datasets) from face-loads (dload, film etc.) */
  generateNodeValuesFromFaces();

  /* set all lcasex.loaded to 1 to indicate that the data are available */
  for (i=0; i<anzx->l; i++) { lcasex[i].fileptr=NULL; lcasex[i].loaded=1; }

  for(l=0; l<anzx->l; l++)
  {
    for(j=0; j<lcasex[l].ncomps; j++)
    {
      /* divide all face-loads by the nr of nodes who summed up to the value */
      if(lcasex[l].iexist[j] <0)
      {
        nr=-lcasex[l].iexist[j];
        lcasex[l].iexist[j]=0;
        for (i=0; i<anzx->n; i++)
        {
          if(lcasex[l].dat[nr][nodex[i].nr])
          {
            for(jj=0; jj<nr; jj++) lcasex[l].dat[jj][nodex[i].nr]/= lcasex[l].dat[nr][nodex[i].nr];
            lcasex[l].dat[nr][nodex[i].nr]=1;
	  }
	}
      }
    }

    for(j=0; j<lcasex[l].ncomps; j++)
    {
      for (i=0; i<anzx->n; i++)
      {
        if (lcasex[l].dat[j][nodex[i].nr] >  lcasex[l].max[j]) 
        {
          lcasex[l].max[j]=lcasex[l].dat[j][nodex[i].nr];
          lcasex[l].nmax[j]=nodex[i].nr;
        }
        if (lcasex[l].dat[j][nodex[i].nr] <  lcasex[l].min[j])
        {
          lcasex[l].min[j]=lcasex[l].dat[j][nodex[i].nr];
          lcasex[l].nmin[j]=nodex[i].nr;
        }
      }
    }
  }

  /* resize the entities */
  if (anzx->amps)
    if ((amplitude = (Amplitudes *)realloc( (Amplitudes *)amplitude, (anzx->amps+1)*sizeof(Amplitudes)) ) == NULL )
    {
      printf("\n\n ERROR: amplitudes:%d could not be resized\n", anzx->amps );
    }
  if (anzx->mats)
    if ((material = (Materials *)realloc( (Materials *)material, (anzx->mats+1)*sizeof(Materials)) ) == NULL )
    {
      printf("\n\n ERROR: materials:%d could not be resized\n", anzx->mats );
    }

  *sptr= setx; *nptr =  nodex; *eptr = elemx; *lptr = lcasex;
  if(ansFlag) fclose(handleans);

  return(1);
}



