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

/* todo:
The shape sets used in transformations do not include their initial coorinate axes. This might be necessary in future. 

- b_spline, line have to regard trimming
 */


#include <cgx.h>

#define  INI_PARAM_LENGTH 10


extern double gtol;
extern Scale     scale[1];
extern Summen    anz[1];

extern Alias     *alias;
extern Sets      *set;
extern Materials *material; 
extern Psets     *pset;
extern Points    *point;
extern Lines     *line;
extern Lcmb      *lcmb;
extern Gsur      *surf;
extern Gbod      *body;
extern Nurbl     *nurbl;
extern Nurbs     *nurbs;
extern Materials *material; 
extern Amplitudes *amplitude; 
extern SumGeo    anzGeo[1];
extern SumAsci   sumAsci[1];
extern Nodes     *node;

extern SpecialSet specialset[1];
extern int     ddiv;
extern int       setall;              /* setNr of the default set "all" */

typedef struct {
  char type;
  int  indx;
} Entity;

typedef struct {
  char name[MAX_LINE_LENGTH];
  char class_name[MAX_LINE_LENGTH];
  char **param;
  int nr;
  char *rest;          /* contains to-be-eval-rest of command-string */
  int ne;
  Entity *entity;      /* list of derived cgx-entities (cgx-type, index) */
} Step_Object;

int max_indx;
int unit_deg=0;
char setbody[]={'+','b','r','e','p','\0'};     /* holds all bodysets */
char setcopy[]={'+','c','o','p','y','\0'};     /* holds all copied (transformed) bodysets */

FILE *handlefbl=NULL;
int assembly_set;
int splitFlag=0;               /* 1: write fbd assembly file and all separate parts */

int get_param_in_string( char *string, char ***args, int *pos)
{
  int i=0,j,nr=0,k;
  int parenthesis;
  char **param;

  param=*args;

  if((param=(char **)realloc((char **)param, (1)*sizeof(char *)))==NULL)
    printf("ERROR: in get_param_in_string\n");
  if((param[0]= (char *)malloc( INI_PARAM_LENGTH*sizeof(char))) == NULL )
    printf("ERROR: in get_param_in_string\n");

  /* terminate the string */
  param[0][0]=0;

  /* scip first parenthesis, count open and closed parenthesis, if all closed stop param at next ',' */
  i++; j=0; parenthesis=0; nr=0;
  while(i<strlen(string))
  {
    if(string[i]=='(') parenthesis++;
    if(string[i]==')') parenthesis--;

    /* break if the first open '(' is closed */
    if(parenthesis==-1)
    {
      /* terminate the parameter */
      param[nr][j]=0;
      nr++;
      i++;   /* jump ")" the ";" remains */
      j=0;
      break;
    }

    if((parenthesis==0)&&(string[i]==',')) 
    {
      /* terminate the string */
      param[nr][j]=0;
      nr++; i++; j=0;

      /* alloc the next param */
      if((param=(char **)realloc((char **)param, (nr+1)*sizeof(char *)))==NULL)
        printf("ERROR: in get_param_in_string\n");
      if((param[nr]= (char *)malloc( INI_PARAM_LENGTH*sizeof(char))) == NULL )
        printf("ERROR: in get_param_in_string\n");

      /* terminate the string */
      param[nr][0]=0;
    }
    else
    {
      /* add char */
      k=strlen(param[nr])+2;
      if(k>INI_PARAM_LENGTH)
        if((param[nr]= (char *)realloc((char *)param[nr], (k)*sizeof(char)))==NULL)
          printf("ERROR: in get_param_in_string\n");
      param[nr][j++]=string[i++];

      /* terminate the string */
      param[nr][j]=0;
    }
  }
  /* read until last ')' */

  *args=param;
  *pos=i;
  return(nr);
}



void fill_step_object( char *string, Step_Object *step_object)
{
  int i=0, j=0, pos=0;

  /* get the class name which ends before the first "(" */
  while(i<strlen(string))
  {
    if(string[i]!='(') step_object->class_name[j++]=string[i++];
    else break;
  }
  step_object->class_name[j]=0;
  
  /* get all parameters. Separated by 1st level "," */
  step_object->param=NULL;
  step_object->nr=get_param_in_string(&string[i], &step_object->param, &pos);
  pos+=i;

  /* alloc a param for the rest of the string */
  i=strlen(&string[pos]);
  if(i>1)
  {
    if((step_object->rest= (char *)malloc( (i+2)*sizeof(char))) == NULL )
      printf("ERROR: in fill_step_object\n");
    strcpy(step_object->rest,&string[pos]);
    //printf("rest:%s|\n",step_object->rest);
  }
  else step_object->rest=NULL;

  /* for cgx-entities */
  step_object->ne=0;
  step_object->entity=NULL;
}



void parse_step_record(char *string, Step_Object **ptr_step_object)
{
  int i=0,j=0;
  int indx;
  Step_Object *step_object;

  step_object= *ptr_step_object;

  /* erase all blanks */
  for(i=0; i<strlen(string); i++) { if(string[i]!=' ') string[j++]=string[i]; }
  string[j]=0;

  //printf("string:%s\n", string);

  /* if 1st char # then 1st arg will be the index (and name) of the command and the second arg is the class of the command */
  if(string[0]=='#') 
  {
    /* index in the step_object array */
    indx=atoi(&string[1]);
    if(indx > max_indx)
    {
      /* add a new member to ptr_step_object */
      if((step_object = (Step_Object *)realloc( (Step_Object *)step_object, (indx+1) * sizeof(Step_Object))) == NULL )
        printf("ERROR: in parse_step_record\n");
      for(j=max_indx+1; j<=indx; j++)
      {
        step_object[j].class_name[0]=0;
        step_object[j].name[0]=0;
      }
      max_indx=indx;
    }

    /* search end-of-name and store name (=='#'+number) */
    i=0; j=0;
    while(i<strlen(string))
    {
      if(string[i]!='=') step_object[indx].name[j++]=string[i++];
      else break;
    }
    step_object[indx].name[j]=0;
    i++; j=0;

    /* check if a combination of step-types follow */
    if(string[i]=='(') i++;
    
    /* fill the step_object from string */
    fill_step_object(&string[i], &step_object[indx]);
  }

  *ptr_step_object = step_object;
}



void step_object_AddEntity(Step_Object *step_object, char type, int indx)
{
  if((step_object->entity= (Entity *)realloc(step_object->entity, (step_object->ne+1)*sizeof(Entity))) == NULL )
      printf("ERROR\n");
  step_object->entity[step_object->ne].type=type;
  step_object->entity[step_object->ne++].indx=indx;
}



void make_point(Step_Object *step_object)
{
  int pnr;
  char buffer[MAX_LINE_LENGTH];
  double px,py,pz;

  /* make pnt based on name and point coordinates */
  sscanf(step_object->param[1],"(%lf,%lf,%lf",&px,&py,&pz);
  sprintf( buffer, "%s %lf %lf %lf",step_object->name,px,py,pz);
  pnr=pre_pnt( buffer, 0 );
  step_object_AddEntity(step_object, 'p', pnr);
  //debug printf("ne:%d %c %d %s\n", step_object->ne, step_object->entity[0].type, step_object->entity[0].indx, point[pnr].name);

  /* look if a label (setname) was specified */
  if (step_object->param[0][1]!='\'')
  {
    strcpy(buffer,step_object->param[0]);
    buffer[0]='S'; buffer[(int)strlen(buffer)-1]='\0';
    pre_seta( buffer, "p", step_object->name);
  }
}



void make_line(Step_Object *step_object, int indx_l)
{
  char buffer[MAX_LINE_LENGTH];
  int  indx_p1, indx_v, indx_dir, enr;
  double vl,p1[3],p2[3],dir[3],vec[3];


  /* line needs point, second point, generated out of direction and vector */
  indx_p1=atoi(&step_object[indx_l].param[1][1]);
  indx_v=atoi(&step_object[indx_l].param[2][1]);

  /* indx of dir based on vector */
  indx_dir=atoi(&step_object[indx_v].param[1][1]);

  /* length of vector */
  vl=atof(step_object[indx_v].param[2]);

  /* make pnt based on name and direction coordinates and vector length*/
  sscanf(step_object[indx_dir].param[1],"(%lf,%lf,%lf",&dir[0],&dir[1],&dir[2]);
  v_scal(&vl,dir,vec);
  sscanf(step_object[indx_p1].param[1],"(%lf,%lf,%lf",&p1[0],&p1[1],&p1[2]);
  v_add(p1,vec,p2);

  sprintf( buffer, "%s %lf %lf %lf",step_object[indx_v].name,p2[0],p2[1],p2[2]);
  enr=pre_pnt( buffer, 0 );
  step_object_AddEntity(&step_object[indx_l], 'p', enr);

  /* make line */
  sprintf( buffer, "%s %s %s",step_object[indx_l].name,step_object[indx_p1].name,step_object[indx_v].name);
  enr=pre_line( buffer, 0 );
  step_object_AddEntity(&step_object[indx_l], 'l', enr);

  /* look if a label (setname) was specified */
  if (step_object[indx_l].param[0][1]!='\'')
  {
    strcpy(buffer,step_object[indx_l].param[0]);
    buffer[0]='S'; buffer[strlen(buffer)-1]='\0';
    pre_seta( buffer, "l", step_object[indx_l].name);
  }
}


/* parm1: start angle of line */
/* parm2: end angle of line */
void make_circle(Step_Object *step_object, int indx_l, char *string1, char *string2, int type)
{
  int       i, pnr, enr;
  char buffer[MAX_LINE_LENGTH], pname[3][MAX_LINE_LENGTH], p1name[MAX_LINE_LENGTH], p2name[MAX_LINE_LENGTH];
  char lname[4][MAX_LINE_LENGTH];
  int  indx_pc, indx_p1, indx_p2, indx_ax, indx_dir1, indx_dir2=0;
  double Rl,pc[3],p1[3],p2[3],dir1[3],dir2[3],fi1,fi2,fi_mid;
  double pmid[3],pbuf[3],vbuf[3], v1[3], v2[3];
  double parm1, parm2;


  /* circle uses AXIS2_PLACEMENT_3D which specifies cener_p(CARTESIAN_POINT), rot-axis(DIRECTION), start-dir(DIRECTION) */

  /* get cp and dirs from AXIS2_PLACEMENT_3D */
  indx_ax=atoi(&step_object[indx_l].param[1][1]);
  indx_pc=atoi(&step_object[indx_ax].param[1][1]);
  indx_dir1=atoi(&step_object[indx_ax].param[2][1]);

  /* make pnt pbuf based on direction coordinates and Radius */
  sscanf(step_object[indx_dir1].param[1],"(%lf,%lf,%lf",&dir1[0],&dir1[1],&dir1[2]);
  if(step_object[indx_ax].param[3][0]=='#')
  {
    indx_dir2=atoi(&step_object[indx_ax].param[3][1]);
    sscanf(step_object[indx_dir2].param[1],"(%lf,%lf,%lf",&dir2[0],&dir2[1],&dir2[2]);
  }
  else
  {
    dir2[0]= dir1[1];
    dir2[1]= dir1[2];
    dir2[2]= dir1[0]+1.;
    v_prod(dir1, dir2, dir2 );
  }

  /* Radius Rl */
  Rl=atof(step_object[indx_l].param[2]);
  sscanf(step_object[indx_pc].param[1],"(%lf,%lf,%lf",&pc[0],&pc[1],&pc[2]);

  if(type==1) /* points known */
  {
    strcpy(p1name,string1);
    strcpy(p2name,string2);

    if(compareStrings( string1, string2)>0)
    {
      indx_p1=atoi(&string1[1]);
      sscanf(step_object[indx_p1].param[1],"(%lf,%lf,%lf",&p1[0],&p1[1],&p1[2]);
      sscanf(step_object[indx_p1].param[1],"(%lf,%lf,%lf",&p2[0],&p2[1],&p2[2]);
      fi1=0.;
      fi2=2*PI;
    }
    else
    {
      /* determine angles parm1 and parm2 */
      indx_p1=atoi(&string1[1]);
      indx_p2=atoi(&string2[1]);
      sscanf(step_object[indx_p1].param[1],"(%lf,%lf,%lf",&p1[0],&p1[1],&p1[2]);
      sscanf(step_object[indx_p2].param[1],"(%lf,%lf,%lf",&p2[0],&p2[1],&p2[2]);
      v_result(pc,p1,v1);
      v_result(pc,p2,v2);
      fi2=v_angle_ref(v1,v2,dir1);
      fi1=0.;
    }

    pbuf[0]=p1[0];
    pbuf[1]=p1[1];
    pbuf[2]=p1[2];
  }
  else if(type==2) /* arcs known */
  {
    parm1=atof(string1);
    parm2=atof(string2);

    /* calc pnts */
    v_scal(&Rl,dir2,vbuf);
    v_add(pc,vbuf,pbuf);

    strcpy(p1name,step_object[indx_dir1].name);
    strcpy(p2name,step_object[indx_dir2].name);

    /* rotate pbuf by angle1 and make p1 */
    if(parm1!=0.)
    {
      if(unit_deg) fi1=(parm1)*PI/180.; else fi1=(parm1);
      v_rot(fi1,pc,dir1,pbuf,p1);
    }
    else { fi1=0.; for(i=0; i<3; i++) p1[i]=pbuf[i]; }
    sprintf( buffer, "%s %lf %lf %lf",p1name,p1[0],p1[1],p1[2]);
    enr=pre_pnt( buffer, 0 );
    step_object_AddEntity(&step_object[indx_l], 'p', enr);
  
    /* rotate pbuf by angle1 and make p2 */
    if(parm2!=0.)
    {
      if(unit_deg) fi2=(parm2)*PI/180.; else fi2=(parm2);
      v_rot(fi2,pc,dir1,pbuf,p2);
    }
    else { fi2=0.; for(i=0; i<3; i++) p2[i]=pbuf[i]; }
    sprintf( buffer, "%s %lf %lf %lf",p2name,p2[0],p2[1],p2[2]);
    enr=pre_pnt( buffer, 0 );
    step_object_AddEntity(&step_object[indx_l], 'p', enr);
  }
  else
  {
    printf("ERROR: type of circle unknown\n");
    return;
  }

  if(fi1<0.) fi1+=2*PI;
  if(fi2<0.) fi2+=2*PI;
  if(fi2<fi1) fi2+=2*PI;

  if(fabs(fi2-fi1)>=2.*PI)
  {
   /* if an arc has 360. degrees create 4 lines and create a lcmb with the step-line-name */

    /* rotate pbuf by 90 deg and make pnts */
    fi_mid=PI*.5; 
    v_rot(fi_mid,pc,dir1,pbuf,pmid);
    getNewName( pname[0], "p" );
    enr=pnt( pname[0], pmid[0], pmid[1], pmid[2], 1 );
    step_object_AddEntity(&step_object[indx_l], 'p', enr);
    getNewName( lname[0], "l" );
    sprintf( buffer, "%s %s %s %s %d",lname[0], p1name, pname[0], step_object[indx_pc].name, MAX_LINE_DIV);
    enr=pre_line( buffer, 0 );
    step_object_AddEntity(&step_object[indx_l], 'l', enr);

    fi_mid=PI; 
    v_rot(fi_mid,pc,dir1,pbuf,pmid);
    getNewName( pname[1], "p" );
    enr=pnt( pname[1], pmid[0], pmid[1], pmid[2], 1 );
    step_object_AddEntity(&step_object[indx_l], 'p', enr);
    getNewName( lname[1], "l" );
    sprintf( buffer, "%s %s %s %s %d",lname[1], pname[0], pname[1], step_object[indx_pc].name, MAX_LINE_DIV);
    enr=pre_line( buffer, 0 );
    step_object_AddEntity(&step_object[indx_l], 'l', enr);

    fi_mid=PI*1.5; 
    v_rot(fi_mid,pc,dir1,pbuf,pmid);
    getNewName( pname[2], "p" );
    enr=pnt( pname[2], pmid[0], pmid[1], pmid[2], 1 );
    step_object_AddEntity(&step_object[indx_l], 'p', enr);
    getNewName( lname[2], "l" );
    sprintf( buffer, "%s %s %s %s %d",lname[2], pname[1], pname[2], step_object[indx_pc].name, MAX_LINE_DIV);
    enr=pre_line( buffer, 0 );
    step_object_AddEntity(&step_object[indx_l], 'l', enr);

    getNewName( lname[3], "l" );
    sprintf( buffer, "%s %s %s %s %d",lname[3], pname[2], p2name, step_object[indx_pc].name, MAX_LINE_DIV);
    enr=pre_line( buffer, 0 );
    step_object_AddEntity(&step_object[indx_l], 'l', enr);

    /* create lcmb with the basis line name */
    //sprintf( buffer, "%s + %s + %s + %s + %s",step_object[indx_l].name,lname[0],lname[1],lname[2],lname[3]);
    //pre_lcmb( buffer, 0 );
  }
  else if(fabs(fi2-fi1)>=PI)
  {
    /* if an arc has 180 or more degrees create 2 lines and create a lcmb with the step-line-name */

    /* rotate pbuf by angle and make pmid */
    fi_mid=fi1+(fi2-fi1)/2.;
    v_rot(fi_mid,pc,dir1,pbuf,pmid);

    getNewName( pname[0], "p" );
    pnr=pnt( pname[0], pmid[0], pmid[1], pmid[2], 1 );
    step_object_AddEntity(&step_object[indx_l], 'p', pnr);

    /* make lines */
    getNewName( lname[0], "l" );
    sprintf( buffer, "%s %s %s %s %d",lname[0], p1name, pname[0], step_object[indx_pc].name, MAX_LINE_DIV);
    enr=pre_line( buffer, 0 );
    step_object_AddEntity(&step_object[indx_l], 'l', enr);
    getNewName( lname[1], "l" );
    sprintf( buffer, "%s %s %s %s %d",lname[1], pname[0], p2name, step_object[indx_pc].name, MAX_LINE_DIV);
    enr=pre_line( buffer, 0 );
    step_object_AddEntity(&step_object[indx_l], 'l', enr);

    /* create lcmb with the basis line name */
    //sprintf( buffer, "%s + %s + %s",step_object[indx_l].name,lname[0],lname[1]);
    //pre_lcmb( buffer, 0 );
  }
  else
  {
    /* make line */
    sprintf( buffer, "%s %s %s %s %d",step_object[indx_l].name, p1name, p2name, step_object[indx_pc].name, MAX_LINE_DIV);
    enr=pre_line( buffer, 0 );
    step_object_AddEntity(&step_object[indx_l], 'l', enr);
  }

}



#define D_FI 10;
int points_on_ellypse(double Rl1, double Rl2, double fi1c, double fi2c, double *dir1, double *dir2, double **pptr)
{
  int i,j,n=0, sum_p=0;
  double fi1, fi2, dfi=0, dir3[3], pnew[3], *ptrans;


  /* transform fi1 and fi2 into the elliptical system */
  fi1=p_angle(cos(fi1c), sin(fi1c)*Rl1/Rl2);
  fi2=p_angle(cos(fi2c), sin(fi2c)*Rl1/Rl2);

  //printf("fi %lf %lf\n", fi1, fi2);

  if(fi1<0.) fi1+=2*PI;
  if(fi2<0.) fi2+=2*PI;
  if(fi2<fi1) fi2+=2*PI;

  v_prod(dir1,dir2,dir3);


  /* generate points between angle fi1 and fi2, Rl1 is on local x and Rl2 is on local y */

  sum_p=(fi2-fi1)/D_FI;
  if(sum_p<3) sum_p=3;
  dfi= (fi2-fi1)/sum_p;

  if( (ptrans=(double *)malloc((sum_p+2)*3*sizeof(double)) ) == NULL )
  { printf("ERROR malloc in ellipse\n"); return(-1); }

  for(i=0; i<=sum_p; i++)
  {
    //printf("fi:%lf\n", fi1);
    pnew[0]=Rl1*cos(fi1);
    pnew[1]=Rl2*sin(fi1);
    pnew[2]=0.;

    /* transform p */
    for(j=0; j<3; j++) ptrans[n+j]=pnew[0]*dir2[j]+pnew[1]*dir3[j]+pnew[2]*dir1[j];
    //for(j=0; j<3; j++) ptrans[n+j]=pnew[j];
    fi1+=dfi;
    n+=3;
  }

  *pptr=ptrans;
  return(sum_p+1);
}



void make_ellipse(Step_Object *step_object, int indx_l, char *parm1, char *parm2, int type)
{
  /* dir1 is the rotation axis */
  /* assumption: dir2 is in direction of Rl1 (big axis) */
  /* r(fi) of the ellipse is determined by interpolating between ra and rb as a function of fi (0-360 deg) */

  int  i, p=0,lp1=0,lp2, n, sq, sum_p=0, enr;
  char buffer[MAX_LINE_LENGTH];
  char seqname[MAX_LINE_LENGTH];
  int  indx_pc, indx_ax, indx_dir1, indx_dir2, indx_p1, indx_p2;
  double Rl1,Rl2,pc[3], p1[3], p2[3], v1[3], v2[3], dir1[3],dir2[3],fi1,fi2;
  double *pnew;

  /* uses AXIS2_PLACEMENT_3D which specifies cener_p(CARTESIAN_POINT), rot-axis(DIRECTION), start-dir(DIRECTION) */

  /* get cp and dirs from AXIS2_PLACEMENT_3D */
  indx_ax=atoi(&step_object[indx_l].param[1][1]);
  indx_pc=atoi(&step_object[indx_ax].param[1][1]);
  indx_dir1=atoi(&step_object[indx_ax].param[2][1]);
  indx_dir2=atoi(&step_object[indx_ax].param[3][1]);

  /* Radius Rl */
  Rl1=atof(step_object[indx_l].param[2]);
  Rl2=atof(step_object[indx_l].param[3]);
  sscanf(step_object[indx_pc].param[1],"(%lf,%lf,%lf",&pc[0],&pc[1],&pc[2]);

  /* make pnt pbuf based on direction coordinates and Radius */
  sscanf(step_object[indx_dir1].param[1],"(%lf,%lf,%lf",&dir1[0],&dir1[1],&dir1[2]);
  sscanf(step_object[indx_dir2].param[1],"(%lf,%lf,%lf",&dir2[0],&dir2[1],&dir2[2]);

  if(type==1) /* points known */
  {
    if(compareStrings( parm1, parm2)>0)
    {
      fi1=0.;
      fi2=360.;
    }
    else
    {
      /* determine angles parm1 and parm2 */
      indx_p1=atoi(&parm1[1]);
      indx_p2=atoi(&parm2[1]);
      sscanf(step_object[indx_p1].param[1],"(%lf,%lf,%lf",&p1[0],&p1[1],&p1[2]);
      sscanf(step_object[indx_p2].param[1],"(%lf,%lf,%lf",&p2[0],&p2[1],&p2[2]);
      v_result(pc,p1,v1);
      v_result(pc,p2,v2);
      fi1=v_angle_ref(dir2,v1,dir1);
      fi2=v_angle_ref(dir2,v2,dir1);
      //printf("fi %lf %lf, %lf %lf\n", fi1, fi2, fi1*180./PI, fi2*180./PI);
    }
  }
  else
  {
    /* parm1: start angle of line */
    /* parm2: end angle of line */
    if(unit_deg)
    {
      fi1=(atof(parm1))*PI/180.;
      fi2=(atof(parm2))*PI/180.;
    }
    else
    {
      fi1=(atof(parm1));
      fi2=(atof(parm2));
    }
  }

  sum_p=points_on_ellypse(Rl1, Rl2, fi1, fi2, dir1, dir2, &pnew);
  getNewName( seqname, "se" );
  sq=pre_seta( seqname, "is", 0);
  n=0;
  for(i=0; i<sum_p; i++)
  {
    getNewName( buffer, "p" );
    p=pnt(buffer,pnew[n],pnew[n+1],pnew[n+2], 1 );
    step_object_AddEntity(&step_object[indx_l], 'p', p);
    n+=3;
    v_add(&point[p].px, pc, &point[p].px);
    //printf("%s %lf %lf %lf\n",buffer,point[p].px, point[p].py ,point[p].pz);

    /* add to seq */
    seta( sq, "p", p );
    if(i==0) lp1=p;
  }
  lp2=p;
  free(pnew);

  sprintf( buffer, "%s %s %s %s",step_object[indx_l].name, point[lp1].name, point[lp2].name, seqname);
  enr=pre_line( buffer, 0 );
  step_object_AddEntity(&step_object[indx_l], 'l', enr);
}



void make_edge_curve(Step_Object *step_object, int indx_trim)
{
  char buffer[MAX_LINE_LENGTH];
  int indx_l, indx_lu;

  indx_l=atoi(&step_object[indx_trim].param[3][1]);

  if (compareStrings(step_object[indx_l].class_name, "CIRCLE")>0) 
  {
    if(step_object[indx_trim].param[4][1]=='F') make_circle(step_object, indx_l, step_object[atoi(&step_object[indx_trim].param[2][1])].param[1], step_object[atoi(&step_object[indx_trim].param[1][1])].param[1], 1);
    else  make_circle(step_object, indx_l, step_object[atoi(&step_object[indx_trim].param[1][1])].param[1], step_object[atoi(&step_object[indx_trim].param[2][1])].param[1], 1);
  }
  else if (compareStrings(step_object[indx_l].class_name, "ELLIPSE")>0)
  {
    if(step_object[indx_trim].param[4][1]=='F') make_ellipse(step_object, indx_l, step_object[atoi(&step_object[indx_trim].param[2][1])].param[1], step_object[atoi(&step_object[indx_trim].param[1][1])].param[1], 1);
    else make_ellipse(step_object, indx_l, step_object[atoi(&step_object[indx_trim].param[1][1])].param[1], step_object[atoi(&step_object[indx_trim].param[2][1])].param[1], 1);
  }
  else if(compareStrings(step_object[indx_l].class_name, "LINE")>0)
  {
    sprintf( buffer, "%s %s %s",step_object[indx_l].name, step_object[atoi(&step_object[indx_trim].param[1][1])].param[1], step_object[atoi(&step_object[indx_trim].param[2][1])].param[1]);
    step_object_AddEntity(&step_object[indx_l], 'l', pre_line( buffer, 0 ));
  }
  else if(compareStrings(step_object[indx_l].class_name, "B_SPLINE_CURVE_WITH_KNOTS")>0)
  {
    /* do nothing, curve will be created later */
    ;
  }
  else if(compareStrings(step_object[indx_l].class_name, "BOUNDED_CURVE")>0)
  {
    /* do nothing, curve will be created later */
    ;
  }
  else if(compareStrings(step_object[indx_l].class_name, "SURFACE_CURVE")>0)
  {
    indx_lu=atoi(&step_object[indx_l].param[1][1]);
    if(compareStrings(step_object[indx_lu].class_name, "LINE")>0)
    {
      sprintf( buffer, "%s %s %s",step_object[indx_lu].name, step_object[atoi(&step_object[indx_trim].param[1][1])].param[1], step_object[atoi(&step_object[indx_trim].param[2][1])].param[1]);
      step_object_AddEntity(&step_object[indx_l], 'l', pre_line( buffer, 0 ));
      //printf("step_object_AddEntity %s %d\n", step_object[indx_l].class_name, step_object[indx_l].ne);
    }
    else
      printf("not known SURFACE_CURVE-type:%s for object:%s\n",step_object[indx_l].class_name, step_object[indx_trim].name);
  }
  else
    printf("not known edge_curve-type:%s for object:%s\n",step_object[indx_l].class_name, step_object[indx_trim].name);

  /* look if a label (setname) was specified */
  if (step_object[indx_trim].param[0][1]!='\'')
  {
    strcpy(buffer,step_object[indx_trim].param[0]);
    buffer[0]='S'; buffer[strlen(buffer)-1]='\0';
    pre_seta( buffer, "l", step_object[indx_trim].name);
  }
}


void make_trimmed_curve(Step_Object *step_object, int indx_trim)
{
  char buffer[MAX_LINE_LENGTH];
  int indx_l;

  indx_l=atoi(&step_object[indx_trim].param[1][1]);

  if (compareStrings(step_object[indx_l].class_name, "CIRCLE")>0) 
  {
    if(step_object[indx_trim].param[4][1]=='F')  make_circle(step_object, indx_l, &step_object[indx_trim].param[3][17], &step_object[indx_trim].param[2][17], 2);
    else   make_circle(step_object, indx_l, &step_object[indx_trim].param[2][17], &step_object[indx_trim].param[3][17], 2);
  }
  else if (compareStrings(step_object[indx_l].class_name, "ELLIPSE")>0) 
  {
    if(step_object[indx_trim].param[4][1]=='F')  make_ellipse(step_object, indx_l, &step_object[indx_trim].param[3][17], &step_object[indx_trim].param[2][17], 2);
    else make_ellipse(step_object, indx_l, &step_object[indx_trim].param[2][17], &step_object[indx_trim].param[3][17], 2);
  }
  else if(compareStrings(step_object[indx_l].class_name, "LINE")>0) make_line(step_object, indx_l);
  else if(compareStrings(step_object[indx_l].class_name, "B_SPLINE_CURVE_WITH_KNOTS")>0)
  {
    /* do nothing, curve will be created later */
    ;
  }
  else if(compareStrings(step_object[indx_l].class_name, "BOUNDED_CURVE")>0)
  {
    /* do nothing, curve will be created later */
    ;
  }
  else
    printf("not known trimmed_curve-type:%s for object:%s\n",step_object[indx_l].class_name, step_object[indx_trim].name);


  /* look if a label (setname) was specified */
  if (step_object[indx_trim].param[0][1]!='\'')
  {
    strcpy(buffer,step_object[indx_trim].param[0]);
    buffer[0]='S'; buffer[strlen(buffer)-1]='\0';
    pre_seta( buffer, "l", step_object[indx_trim].name);
  }
}


void make_b_spline_curve_with_knots(Step_Object *step_object, int indx)
{
  int       i,j,k, pos, enr;
  char buffer[MAX_LINE_LENGTH], **args=NULL;
  int deg, npnt=0, nknt=0, div=99, *cpnt=NULL, *knt_i=NULL, nknt_t=0;
  double *knt_v=NULL;

  /* determine degree */
  deg=atoi(step_object[indx].param[1]);

  /* determine controll-points */
  npnt=get_param_in_string(step_object[indx].param[2], &args, &pos);

  /* get the controll-points from args */
  if((cpnt = (int *)malloc( (npnt+1) * sizeof(int))) == NULL )
    printf("ERROR: in make_b_spline_curve_with_knots\n");
  for (i=0; i<npnt; i++) cpnt[i]=atoi(&args[i][1]);

  /* determine knots */
  nknt=get_param_in_string(step_object[indx].param[6], &args, &pos);

  /* get the knt-indx from args */
  if((knt_i = (int *)malloc( (nknt+1) * sizeof(int))) == NULL )
    printf("ERROR: in make_b_spline_curve_with_knots\n");
  for (i=0; i<nknt; i++) knt_i[i]=atoi(&args[i][0]);

  /* get knot-values */
  nknt=get_param_in_string(step_object[indx].param[7], &args, &pos);

  /* get the knt-values from args */
  if((knt_v = (double *)malloc( (nknt+1) * sizeof(double))) == NULL )
    printf("ERROR: in make_b_spline_curve_with_knots\n");
  for (i=0; i<nknt; i++) knt_v[i]=atof(args[i]);

  /* total nr-of-nkots= sum( knt_i) */
  for(i=0; i<nknt; i++) nknt_t+=knt_i[i];

  sprintf(buffer,"%s DEFINE FULL #%d #%d %d %d %d %d\n"
  , step_object[indx].name, cpnt[0], cpnt[npnt-1], deg, npnt, nknt_t, div);
  nurl( buffer, 0 );

  for (i=0; i<npnt; i++)
  {
    sprintf(buffer,"%s CONTROL %d #%d 1.\n"
    , step_object[indx].name, i+1, cpnt[i]);
    nurl( buffer, 0 );
  }

  k=1;
  for (i=0; i<nknt; i++)
  {
    for(j=0; j<knt_i[i]; j++)
    {
      sprintf(buffer,"%s KNOT %d %lf\n"
      , step_object[indx].name, k++, knt_v[i] );
      nurl( buffer, 0 );
    }
  }

  sprintf(buffer,"%s END\n", step_object[indx].name);
  enr=nurl( buffer, 0 );
  step_object_AddEntity(&step_object[indx], 'l', enr);
}


void make_bounded_curve(Step_Object *step_object, int indx)
{
  int       i,j,k, pos, enr;
  char buffer[MAX_LINE_LENGTH], **args=NULL;
  int deg, npnt=0, nknt=0, div=99, *cpnt=NULL, *knt_i=NULL, nknt_t=0;
  double *knt_v=NULL, *weight=NULL;
  Step_Object stepObject;

  
  //printf("string:%s\n", step_object[indx].rest);
  fill_step_object(step_object[indx].rest, &stepObject);

  if(compareStrings(stepObject.class_name,"B_SPLINE_CURVE")>0)
  {
    //printf("found %s\n", stepObject.class_name );

    /* determine degree */
    deg=atoi(stepObject.param[0]);

    /* determine controll-points */
    npnt=get_param_in_string(stepObject.param[1], &args, &pos);

    /* get the controll-points from args */
    if((cpnt = (int *)malloc( (npnt+1) * sizeof(int))) == NULL )
      printf("ERROR: in make_bounded_curve\n");
    for (i=0; i<npnt; i++) cpnt[i]=atoi(&args[i][1]);

    while(strlen(stepObject.rest)>3)
    {
      fill_step_object(stepObject.rest, &stepObject);
      //printf("object:%s\n", stepObject.class_name );

      if(compareStrings(stepObject.class_name,"B_SPLINE_CURVE_WITH_KNOTS")>0)
      {
        //printf("found %s\n", stepObject.class_name );

        /* determine knots */
        nknt=get_param_in_string(stepObject.param[0], &args, &pos);
  
        /* get the knt-indx from args */
        if((knt_i = (int *)malloc( (nknt+1) * sizeof(int))) == NULL )
          printf("ERROR: in make_bounded_curve\n");
        for (i=0; i<nknt; i++) knt_i[i]=atoi(&args[i][0]);
  
        /* get knot-values */
        nknt=get_param_in_string(stepObject.param[1], &args, &pos);
  
        /* get the knt-values from args */
        if((knt_v = (double *)malloc( (nknt+1) * sizeof(double))) == NULL )
          printf("ERROR: in make_bounded_curve\n");
        for (i=0; i<nknt; i++) knt_v[i]=atof(args[i]);
  
        /* total nr-of-nkots= sum( knt_i) */
        for(i=0; i<nknt; i++) nknt_t+=knt_i[i];
      }
      if(compareStrings(stepObject.class_name,"RATIONAL_B_SPLINE_CURVE")>0)
      {
        //printf("found %s\n", stepObject.class_name );

        /* determine weights */
        k=get_param_in_string(stepObject.param[0], &args, &pos);

        if((weight = (double *)malloc( (k+1) * sizeof(double))) == NULL )
          printf("ERROR: in make_bounded_curve\n");
        for (i=0; i<k; i++) weight[i]=atof(args[i]);
      }
    }


    /* define the NURL as fbd command */
    sprintf(buffer,"%s DEFINE FULL #%d #%d %d %d %d %d\n"
    , step_object[indx].name, cpnt[0], cpnt[npnt-1], deg, npnt, nknt_t, div);
    nurl( buffer, 0 );
  
    for (i=0; i<npnt; i++)
    {
      sprintf(buffer,"%s CONTROL %d #%d %lf\n"
      , step_object[indx].name, i+1, cpnt[i], weight[i]);
      nurl( buffer, 0 );
    }
  
    k=1;
    for (i=0; i<nknt; i++)
    {
      for(j=0; j<knt_i[i]; j++)
      {
        sprintf(buffer,"%s KNOT %d %lf\n"
        , step_object[indx].name, k++, knt_v[i] );
        nurl( buffer, 0 );
      }
    }

    sprintf(buffer,"%s END\n", step_object[indx].name);
    enr=nurl( buffer, 0 );
    step_object_AddEntity(&step_object[indx], 'l', enr);
  }

  return;
}


void make_axisSet(Step_Object *step_object, int indx_ax)
{
  int       i, pnr, enr;
  char buffer[MAX_LINE_LENGTH], pname[3][MAX_LINE_LENGTH];
  char lname[4][MAX_LINE_LENGTH];
  int  indx_pc, indx_dir1, indx_dir2=0, axisSet;
  double pc[3],p1[3],p2[3],dir1[3],dir2[3];

  /* open a set for the axis */
  sprintf(buffer,"-axis_#%d",indx_ax);
  axisSet=pre_seta(buffer,"i",0);

  /* get cp and dirs from AXIS2_PLACEMENT_3D */
  indx_pc=atoi(&step_object[indx_ax].param[1][1]);
  indx_dir1=atoi(&step_object[indx_ax].param[2][1]);

  /* make pnt pbuf based on direction coordinates and Radius */
  sscanf(step_object[indx_dir1].param[1],"(%lf,%lf,%lf",&dir1[0],&dir1[1],&dir1[2]);
  if(step_object[indx_ax].param[3][0]=='#')
  {
    indx_dir2=atoi(&step_object[indx_ax].param[3][1]);
    sscanf(step_object[indx_dir2].param[1],"(%lf,%lf,%lf",&dir2[0],&dir2[1],&dir2[2]);
  }
  else
  {
    dir2[0]= dir1[1];
    dir2[1]= dir1[2];
    dir2[2]= dir1[0]+1.;
    v_prod(dir1, dir2, dir2 );
  }
  sscanf(step_object[indx_pc].param[1],"(%lf,%lf,%lf",&pc[0],&pc[1],&pc[2]);

  getNewName( pname[0], "p" );
  pnr=pnt( pname[0], pc[0], pc[1], pc[2], 1 );
  step_object_AddEntity(&step_object[indx_ax], 'p', pnr);
  v_add( pc,dir1, p1);
  v_add( pc,dir2, p2);
  getNewName( pname[1], "p" );
  pnr=pnt( pname[1], p1[0], p1[1], p1[2], 1 );
  step_object_AddEntity(&step_object[indx_ax], 'p', pnr);
  getNewName( pname[2], "p" );
  pnr=pnt( pname[2], p2[0], p2[1], p2[2], 1 );
  step_object_AddEntity(&step_object[indx_ax], 'p', pnr);

  getNewName( lname[0], "l" );
  sprintf( buffer, "%s %s %s %d",lname[0], pname[0], pname[1], MAX_LINE_DIV);
  enr=pre_line( buffer, 0 );
  step_object_AddEntity(&step_object[indx_ax], 'l', enr);
  getNewName( lname[1], "l" );
  sprintf( buffer, "%s %s %s %d",lname[1], pname[0], pname[2], MAX_LINE_DIV);
  enr=pre_line( buffer, 0 );
  step_object_AddEntity(&step_object[indx_ax], 'l', enr);

  for(i=0; i<step_object[indx_ax].ne; i++)
  {
    sprintf(buffer,"%c",step_object[indx_ax].entity[i].type); 
    seta(axisSet,buffer,step_object[indx_ax].entity[i].indx);
  }
}



/* nearly a complete copy of make_manifoldSet() */
void make_surfaceSet(Step_Object *step_object, int indx)
{
  int  i,k,l,m, pos, ns, nf, ne, pnr;
  int  bodySet, face, faceBound, loop, edge, curve;
  char buffer[MAX_LINE_LENGTH], **shells=NULL, **faces=NULL, **bounds=NULL, **edges=NULL;
  int  indx_cs;

  /* open a set for the surface */
  //sprintf(buffer,"-brep_#%d",indx);
  sprintf(buffer,"+surf_#%d",indx);
  bodySet=pre_seta(buffer,"i",0);

  /* get the SHELL */
  ns=get_param_in_string(step_object[indx].param[1], &shells, &pos);
  for (m=0; m< ns; m++)
  {
    indx_cs=atoi(&shells[m][1]);
    //printf("shell:%s\n",shells[m]);
    //printf("class_name:%s\n",step_object[indx_cs].class_name);
    //printf("param[1]:%s\n",step_object[indx_cs].param[1]);

  /* get the FACE's and track down to the lines and points used by it */
  nf=get_param_in_string(step_object[indx_cs].param[1], &faces, &pos);
  for (i=0; i< nf; i++)
  {
    face=atoi(&faces[i][1]);

    /* if its a known class go deeper */
    if(strstr(step_object[face].class_name, "FACE") !=NULL)
    {
      //printf(" %d %s %s\n", face, step_object[face].name, step_object[face].class_name);
      /* get the bounds */
      get_param_in_string(step_object[face].param[1], &bounds, &pos);
      //for (j=0; j< nb; j++) printf(" %s\n", bounds[j]);
      faceBound=atoi(&bounds[0][1]);
      /* get the BOUND */
      if(strstr(step_object[faceBound].class_name, "BOUND") !=NULL)
      {
        //printf(" %d %s %s %s\n", faceBound, step_object[faceBound].name, step_object[faceBound].class_name, step_object[faceBound].param[1]);
        /* get the loop */
        loop=atoi(&step_object[faceBound].param[1][1]);
	//printf("loop:%d\n", loop);
        /* get the EDGE_LOOP */
        if(strstr(step_object[loop].class_name, "EDGE_LOOP") !=NULL)
        {
          //printf(" %d %s %s %s\n", loop, step_object[loop].name, step_object[loop].class_name, step_object[loop].param[1]);
          /* get the edges */
          ne=get_param_in_string(step_object[loop].param[1], &edges, &pos);
          for (k=0; k< ne; k++)
	  {
            //printf(" %s\n", edges[k]);
            edge=atoi(&edges[k][1]);
            /* get the ORIENTED_EDGE */
            if(strstr(step_object[edge].class_name, "ORIENTED_EDGE") !=NULL)
            {
              //printf(" %d %s %s %s\n", edge, step_object[edge].name, step_object[edge].class_name, step_object[edge].param[3]);
              /* get the EDGE_CURVE */
              curve=atoi(&step_object[edge].param[3][1]);
              //printf(" %d %s %s %s\n", curve, step_object[curve].name, step_object[curve].class_name, step_object[curve].param[1]);

              /* get the 1st point */
              pnr=atoi(&step_object[curve].param[1][1]);
              if(strstr(step_object[pnr].class_name, "CARTESIAN_POINT") !=NULL)
	      {
                seta(bodySet,"p",step_object[pnr].entity[0].indx);
	      }
              else if(strstr(step_object[pnr].class_name, "VERTEX_POINT") !=NULL)
	      {
                pnr=atoi(&step_object[pnr].param[1][1]);
                seta(bodySet,"p",step_object[pnr].entity[0].indx);
   	      }

              /* get the 2nd point */
              pnr=atoi(&step_object[curve].param[2][1]);
              if(strstr(step_object[pnr].class_name, "CARTESIAN_POINT") !=NULL)
	      {
                seta(bodySet,"p",step_object[pnr].entity[0].indx);
	      }
              else if(strstr(step_object[pnr].class_name, "VERTEX_POINT") !=NULL)
	      {
                pnr=atoi(&step_object[pnr].param[1][1]);
                seta(bodySet,"p",step_object[pnr].entity[0].indx);
   	      }

              /* get the line */
              pnr=atoi(&step_object[curve].param[3][1]);
	      //printf("%s %d\n", step_object[pnr].class_name, step_object[pnr].ne);
              for(l=0; l<step_object[pnr].ne; l++)
	      {
                sprintf(buffer,"%c",step_object[pnr].entity[l].type); 
                seta(bodySet,buffer,step_object[pnr].entity[l].indx);
	      }
 	    }
	  }
        }
      }
    }
  }

  }
  completeSet(set[bodySet].name, "do");

  return;
}



void make_manifoldSet(Step_Object *step_object, int indx)
{
  int  i,k,l, pos, nf, ne, pnr;
  int  bodySet, face, faceBound, loop, edge, curve;
  char buffer[MAX_LINE_LENGTH], **faces=NULL, **bounds=NULL, **edges=NULL;
  int  indx_cs;

  /* open a set for the body */
  sprintf(buffer,"-brep_#%d",indx);
  bodySet=pre_seta(buffer,"i",0);

  /* add the body to the +brep set */
  pre_seta( setbody, "se", buffer );

  /* get the CLOSED_SHELL */
  indx_cs=atoi(&step_object[indx].param[1][1]);

  /* get the FACE's and track down to the lines and points used by it */
  nf=get_param_in_string(step_object[indx_cs].param[1], &faces, &pos);
  for (i=0; i< nf; i++)
  {
    face=atoi(&faces[i][1]);

    /* if its a known class go deeper */
    if(strstr(step_object[face].class_name, "FACE") !=NULL)
    {
      //printf(" %d %s %s\n", face, step_object[face].name, step_object[face].class_name);
      /* get the bounds */
      get_param_in_string(step_object[face].param[1], &bounds, &pos);
      //for (j=0; j< nb; j++) printf(" %s\n", bounds[j]);
      faceBound=atoi(&bounds[0][1]);
      /* get the BOUND */
      if(strstr(step_object[faceBound].class_name, "BOUND") !=NULL)
      {
        //printf(" %d %s %s %s\n", faceBound, step_object[faceBound].name, step_object[faceBound].class_name, step_object[faceBound].param[1]);
        /* get the loop */
        loop=atoi(&step_object[faceBound].param[1][1]);
	//printf("loop:%d\n", loop);
        /* get the EDGE_LOOP */
        if(strstr(step_object[loop].class_name, "EDGE_LOOP") !=NULL)
        {
          //printf(" %d %s %s %s\n", loop, step_object[loop].name, step_object[loop].class_name, step_object[loop].param[1]);
          /* get the edges */
          ne=get_param_in_string(step_object[loop].param[1], &edges, &pos);
          for (k=0; k< ne; k++)
	  {
            //printf(" %s\n", edges[k]);
            edge=atoi(&edges[k][1]);
            /* get the ORIENTED_EDGE */
            if(strstr(step_object[edge].class_name, "ORIENTED_EDGE") !=NULL)
            {
              //printf(" %d %s %s %s\n", edge, step_object[edge].name, step_object[edge].class_name, step_object[edge].param[3]);
              /* get the EDGE_CURVE */
              curve=atoi(&step_object[edge].param[3][1]);
              //printf(" %d %s %s %s\n", curve, step_object[curve].name, step_object[curve].class_name, step_object[curve].param[1]);

              /* get the 1st point */
              pnr=atoi(&step_object[curve].param[1][1]);
              if(strstr(step_object[pnr].class_name, "CARTESIAN_POINT") !=NULL)
	      {
                seta(bodySet,"p",step_object[pnr].entity[0].indx);
	      }
              else if(strstr(step_object[pnr].class_name, "VERTEX_POINT") !=NULL)
	      {
                pnr=atoi(&step_object[pnr].param[1][1]);
                seta(bodySet,"p",step_object[pnr].entity[0].indx);
   	      }

              /* get the 2nd point */
              pnr=atoi(&step_object[curve].param[2][1]);
              if(strstr(step_object[pnr].class_name, "CARTESIAN_POINT") !=NULL)
	      {
                seta(bodySet,"p",step_object[pnr].entity[0].indx);
	      }
              else if(strstr(step_object[pnr].class_name, "VERTEX_POINT") !=NULL)
	      {
                pnr=atoi(&step_object[pnr].param[1][1]);
                seta(bodySet,"p",step_object[pnr].entity[0].indx);
   	      }

              /* get the line */
              pnr=atoi(&step_object[curve].param[3][1]);
              for(l=0; l<step_object[pnr].ne; l++)
	      {
                sprintf(buffer,"%c",step_object[pnr].entity[l].type); 
                seta(bodySet,buffer,step_object[pnr].entity[l].indx);
	      }
 	    }
	  }
        }
      }
    }
  }
  completeSet(set[bodySet].name, "do");

  /* copy the set for the accumulation of copied entities */
  sprintf(buffer,"+brep_#%d",indx);
  pre_seta(buffer,"se",set[bodySet].name);
  completeSet(buffer, "do");
  return;
}



void make_shapeSet(Step_Object *step_object, int indx)
{
  int  indx_set=0;
  int  npnt;
  int  pos;
  char **args=NULL;
  char buffer[MAX_LINE_LENGTH];
  char advandedBrepSet[MAX_LINE_LENGTH];

  /* determine related brep_set (MANIFOLD_SOLID_BREP) */
  npnt=get_param_in_string(step_object[indx].param[1], &args, &pos);
  printf("MANIFOLD_SOLID_BREP: %s\n", args[npnt-1]);
  indx_set=atoi(&args[npnt-1][1]);
  sprintf(buffer,"-brep_#%d",indx_set);

  /* add the body to the advanded_brep_set set */
  /* ignore the AXIS2_PLACEMENT_3D for the moment */
  sprintf(advandedBrepSet,"-shape_#%d",indx);
  indx_set=pre_seta( advandedBrepSet, "se", buffer );
  completeSet(advandedBrepSet, "do");

  /* write the shapes to the filesystem */
  if( splitFlag)
  { 
    sprintf(advandedBrepSet,"shape_#%d",indx);
    rnam( indx_set, advandedBrepSet );

    sprintf(buffer," %s fbd", advandedBrepSet);
    pre_write(buffer);

    sprintf(advandedBrepSet,"-shape_#%d",indx);
    rnam( indx_set, advandedBrepSet );

    /* mark the set as written */
    set[indx_set].etyp=1;
  }

}



void copy_shapeSet(Step_Object *step_object, int indx)
{
  int  i,indx_set=0;
  int  npnt,pos, indx_buf[2];
  char **args=NULL;;
  char buffer[MAX_LINE_LENGTH];
  char shapeSet[MAX_LINE_LENGTH];

  indx_buf[0]=atoi(&step_object[indx].param[2][1]);
  indx_buf[1]=atoi(&step_object[indx].param[3][1]);
  printf("#%d %s\n", indx_buf[0], step_object[indx_buf[0]].class_name);
  printf("#%d %s\n", indx_buf[1], step_object[indx_buf[1]].class_name);

  /* determine related brep_set (MANIFOLD_SOLID_BREP) */
  npnt=get_param_in_string(step_object[indx_buf[1]].param[1], &args, &pos);
  for(i=0; i<npnt; i++)
  {
    indx_set=atoi(&args[i][1]);
    sprintf(buffer,"-brep_#%d",indx_set);
    getSetNr(buffer);

    /* add the body to the shape_set set */
    sprintf(shapeSet,"-shape_#%d",indx_buf[0]);
    pre_seta(shapeSet, "se", buffer );
  }
}



void copy_and_move_shapeSet(Step_Object *step_object, int indx)
{
  int  i;
  char buffer[MAX_LINE_LENGTH];
  int  indx_pc, indx_ax, indx_dir1, indx_dir2=0, indx_set, indx_trfm, indx_buf[2];
  double p0[3]={0,0,0}, pc[2][3],dir1[2][3],dir2[2][3], vn[2][3], prot[3], pcheck[3], vres[3], vl, angle[2];
  int  vn_indx[2], p0_indx, setNr[2];
  static int copyNr=0;
  Step_Object stepObject;


  printf("transform based on:#%d\n", indx);
  indx_buf[0]=atoi(&step_object[indx].param[2][1]);
  indx_buf[1]=atoi(&step_object[indx].param[3][1]);
  //printf("#%d %s\n", indx_buf[0], step_object[indx_buf[0]].class_name);
  //printf("#%d %s\n", indx_buf[1], step_object[indx_buf[1]].class_name);

  /* get the ADVANCED_BREP_SHAPE_REPRESENTATION, return() if the entity is not of that type */
  if (compareStrings(step_object[indx_buf[0]].class_name, "ADVANCED_BREP_SHAPE_REPRESENTATION")>0)
  {
    /* determine related brep_set (MANIFOLD_SOLID_BREP) */
    indx_set=indx_buf[0];
    sprintf(buffer,"-shape_#%d",indx_set);
    setNr[0]=getSetNr(buffer);
    if(setNr[0]<0) return;
  }
  else if (compareStrings(step_object[indx_buf[0]].class_name, "SHAPE_REPRESENTATION")>0)
  {
    /* the referenced item is a set of the transformed brep_sets (->assembly) */
    indx_set=indx_buf[0];
    sprintf(buffer,"-shape_#%d",indx_set);
    setNr[0]=getSetNr(buffer);
    if(setNr[0]<0) return;
  }
  else return;

  /* open the target-set of the transformation */
  copyNr++;
  sprintf(buffer,"-copy%d_#%d", copyNr, indx_set);
  setNr[1]=pre_seta(buffer,"i",0);

  /* add the copy to the +copy set */
  pre_seta( setcopy, "se", buffer );

  /* open a shape_set and assign the above referenced shape_set. Use the # of the referenced SHAPE_REPRESENTATION */ 
  if (compareStrings(step_object[indx_buf[1]].class_name, "SHAPE_REPRESENTATION")>0)
  {
    sprintf(buffer,"-shape_#%d",indx_buf[1]);
    assembly_set=pre_seta(buffer,"se",set[setNr[1]].name);
  }
  else return;

  /* get the ITEM_DEFINED_TRANSFORMATION, return() if the entity is not of that type */
  //printf("string:%s\n", step_object[indx].rest);
  fill_step_object(step_object[indx].rest, &stepObject);
  indx_trfm=atoi(&stepObject.param[0][1]);
  if (compareStrings(step_object[indx_trfm].class_name, "ITEM_DEFINED_TRANSFORMATION")>0)
  {
    /* get the AXIS2_PLACEMENT_3D */
    //printf("%s %s\n", step_object[indx_trfm].param[2], step_object[indx_trfm].param[3]);
    indx_buf[0]=atoi(&step_object[indx_trfm].param[2][1]);
    indx_buf[1]=atoi(&step_object[indx_trfm].param[3][1]);
  }
  else return;

  /* get cp and dirs from AXIS2_PLACEMENT_3D */
  for(i=0; i<2; i++)
  {
    indx_ax=indx_buf[i];
    indx_pc=atoi(&step_object[indx_ax].param[1][1]);
    indx_dir1=atoi(&step_object[indx_ax].param[2][1]);
  
    /* make pnt pbuf based on direction coordinates and Radius */
    sscanf(step_object[indx_dir1].param[1],"(%lf,%lf,%lf",&dir1[i][0],&dir1[i][1],&dir1[i][2]);
    if(step_object[indx_ax].param[3][0]=='#')
    {
      indx_dir2=atoi(&step_object[indx_ax].param[3][1]);
      sscanf(step_object[indx_dir2].param[1],"(%lf,%lf,%lf",&dir2[i][0],&dir2[i][1],&dir2[i][2]);
    }
    else
    {
      dir2[i][0]= dir1[i][1];
      dir2[i][1]= dir1[i][2];
      dir2[i][2]= dir1[i][0]+1.;
      v_prod(dir1[i], dir2[i], dir2[i] );
    }
    sscanf(step_object[indx_pc].param[1],"(%lf,%lf,%lf",&pc[i][0],&pc[i][1],&pc[i][2]);
  }

  /* determine the rotations to go from position 1 to 2 */
  //printf("dir10: %f %f %f\n", dir1[0][0], dir1[0][1], dir1[0][2]);
  //printf("dir11: %f %f %f\n", dir1[1][0], dir1[1][1], dir1[1][2]);
  v_prod(dir1[0], dir1[1], vn[0]);
  if(v_betrag(vn[0])==0.)
  {
    vn[0][0]=dir2[0][0];
    vn[0][1]=dir2[0][1];
    vn[0][2]=dir2[0][2];
  }

  /* rotate dir1[0] around vn[0] and check if it is at the place of dir1[1] by calculating the length of v_result (has to be 0)  */
  angle[0]=acos(v_sprod(dir1[0], dir1[1]));
  v_rot(angle[0], p0, vn[0], dir1[0], pcheck);
  //printf("pcheck:%f %f %f\n", pcheck[0], pcheck[1], pcheck[2]);
  v_result(dir1[1], pcheck, vres);
  vl=v_betrag(vres);
  if(abs(vl)>0.001) angle[0]*=-1;
  //printf("v1: %f %f %f a:%f vl:%f\n", vn[0][0], vn[0][1], vn[0][2], angle[0], vl );

  /* rotate dir2[0] around vn, dir1[0] after rotation is known as dir1[1] */
  v_rot(angle[0], p0, vn[0], dir2[0], prot);

  /* second rotation */
  //printf("dir20: %f %f %f\n", dir2[0][0], dir2[0][1], dir2[0][2]);
  //printf("dir21: %f %f %f\n", dir2[1][0], dir2[1][1], dir2[1][2]);
  vn[1][0]=dir1[1][0];
  vn[1][1]=dir1[1][1];
  vn[1][2]=dir1[1][2];

  /* rotate prot around vn[1] and check if dir2[1] results in the same direction */
  angle[1]=acos(v_sprod(prot, dir2[1]));
  v_rot(angle[1], p0, vn[1], prot, pcheck);
  //printf("pcheck:%f %f %f a:%f\n", pcheck[0], pcheck[1], pcheck[2], angle[1]);
  v_result(dir2[1], pcheck, vres);
  vl=v_betrag(vres);
  if(abs(vl)>0.001) angle[1]*=-1;
  //printf("v2: %f %f %f a:%f vl:%f\n", vn[1][0], vn[1][1], vn[1][2], angle[1], vl);

  /* generate points on rotation-axis */
  p0_indx=pnt( "+p0", 0.,0.,0., 1 );
  vn_indx[0]=pnt( "+p1",vn[0][0],vn[0][1],vn[0][2], 1 );
  vn_indx[1]=pnt( "+p2",vn[1][0],vn[1][1],vn[1][2], 1 );

  /* copy set to the new position and write transformations */
  if( splitFlag)
  {
    fprintf(handlefbl,"pnt +p0 0. 0. 0.\n");
    fprintf(handlefbl,"pnt +p1 %f %f %f\n",vn[0][0],vn[0][1],vn[0][2]);
    fprintf(handlefbl,"pnt +p2 %f %f %f\n",vn[1][0],vn[1][1],vn[1][2]);
  }
  sprintf(buffer,"%s %s rot %s %s %f a", set[setNr[0]].name, set[setNr[1]].name, point[p0_indx].name, point[vn_indx[0]].name, angle[0]*180./PI );
  pre_copy(buffer);
  printf("copy %s\n",buffer); 
  if( splitFlag) fprintf(handlefbl,"copy %s\n",buffer);

  if(angle[1]!=0.)
  {
    sprintf(buffer,"%s rot %s %s %f", set[setNr[1]].name, point[p0_indx].name, point[vn_indx[1]].name, angle[1]*180./PI );
    pre_move(buffer);
    printf("move %s\n",buffer); 
    if( splitFlag) fprintf(handlefbl,"move %s\n",buffer); 
  }

  if( ((pc[1][0]-pc[0][0])!=0.) || ((pc[1][1]-pc[0][1])!=0.) || ((pc[1][2]-pc[0][2])!=0.) )
  {
    sprintf(buffer,"%s tra %f %f %f", set[setNr[1]].name, pc[1][0]-pc[0][0], pc[1][1]-pc[0][1], pc[1][2]-pc[0][2]);
    pre_move(buffer);
    printf("move %s\n",buffer); 
    if( splitFlag) fprintf(handlefbl,"move %s\n",buffer); 
  }

  if( splitFlag) fprintf(handlefbl,"seta %s se %s\n", set[assembly_set].name, set[setNr[1]].name);
  sprintf(buffer,"l %s    \n", set[setNr[1]].name);
  printf("plot %s\n", buffer);
  if( splitFlag) fprintf(handlefbl,"plot %s\n", buffer);
  plot(buffer);
  descalPoints( anzGeo->p, point, scale);
  getScaleValues( setall, set, point, node, scale);
  scalPoints ( anzGeo->p, point, scale );
}



void make_setName(Step_Object *step_object, int indx)
{
  int  indx_buf[2], setNr=0;
  char buffer[MAX_LINE_LENGTH];
  char name[MAX_LINE_LENGTH];
  FILE *handle=NULL;

  printf("rnam based on:#%d\n", indx);
  indx_buf[0]=atoi(&step_object[indx].param[0][1]);
  indx_buf[1]=atoi(&step_object[indx].param[1][1]);
  //printf("#%d %s\n", indx_buf[0], step_object[indx_buf[0]].class_name);
  //printf("#%d %s\n", indx_buf[1], step_object[indx_buf[1]].class_name);

  /* look for a name-definition for an existing assembly set */
  if (compareStrings(step_object[indx_buf[0]].class_name, "PRODUCT_DEFINITION_SHAPE")>0)
  {
    sprintf(name,"%s", &step_object[indx_buf[0]].param[1][1]);
    name[strlen(name)-1]='\0';
    if(strlen(name)<1)  return;
  }
  else return;

  sprintf(buffer,"-shape_#%d",indx_buf[1]);
  setNr=getSetNr(buffer); 
  if(setNr>-1)
  {
    /* move the parts to directories */
    if(( splitFlag)&&(set[setNr].etyp==1))
    {
      sprintf(buffer, "mkdir -p %s", name);
      printf("%s\n",buffer);
      system(buffer);
      sprintf(buffer, "mv -f %s.fbd %s/%s.fbd", &set[setNr].name[1], name, &set[setNr].name[1]);
      printf("%s\n",buffer);
      system(buffer);

      handle = fopen ("cgx_tmp2.fbl", "a");
      if (handle==NULL)
      {
        printf (" ERROR: The command file \"cgx_tmp2.fbl\" could not be opened.\n\n");
        exit(0);
      }
      fprintf(handle,"read %s/%s.fbd add\n", name, &set[setNr].name[1]);
      fprintf(handle,"rnam %s %s\n", &set[setNr].name[1], set[setNr].name);
      fclose(handle);
    }
    if( splitFlag)
    {
      fprintf(handlefbl," rnam %s %s\ncomp %s do\n", set[setNr].name, name, name); 
    }

    printf(" rnam %s %s\n comp %s do\n", set[setNr].name, name, name); 
    rnam( setNr, name);
    completeSet( set[setNr].name, "do" );
    return;
  }

  printf(" WARNING: found no assembly-set for SHAPE:#%d of name:%s\n",  indx_buf[1], name);
}



int readstep( char *datin, int flag )
{
  FILE      *handle1;
  int       length, i,j, terminated=0, eof=0, gtolFlag=0;
  char *string=NULL;
  char string1[MAX_LINE_LENGTH];
  double max[3], min[3];
  char buffer[MAX_LINE_LENGTH];

  Step_Object *step_object=NULL;

  splitFlag=flag;

  handle1 = fopen (datin, "r");
  if (handle1==NULL)
  {
    printf (" ERROR: The input file \"%s\" could not be opened.\n\n", datin);
    return(-1);
  }
  else  printf ("\n%s opened",datin);

  printf ("\n reading file\n");
  while (1)
  {
    do {
      length=frecord( handle1, string1);
      if (string1[length] == (char)EOF) eof=1;

      /* a step-record is terminated by a ';'. If it can not be found then read the next record and add it */
      for (i=strlen(string1); i>=0; i--) if(string1[i]==';') terminated=1;
      //      if (!terminated)
      string1[length]=0;

      if(string==NULL)
      {
        if((string=(char *)malloc((strlen(string1)+2)*sizeof(char)))==NULL)
          printf("ERROR: in readstep\n");
        string[0]=0;
        strcpy(string,string1);
      }
      else
      {
        if((string=(char *)realloc((char *)string, (strlen(string)+strlen(string1)+2)*sizeof(char)))==NULL)
           printf("ERROR: in readstep\n");
        strcpy(&string[strlen(string)],string1);
      }
      if(eof) terminated=1;
    }while(!terminated);

    terminated=0;

    /* parse record, get class and parameters */
    if(string[0]) parse_step_record(string, &step_object);
    string[0]=0;
    if (eof) break;
  }
  printf ("\n finished file reading, starting now interpretation...\n");

  /* check objects */
  /*
  for(j=1; j<=max_indx; j++)
  {
    if(step_object[j].nr>0)
    {
     if (compareStrings(step_object[j].class_name, "REPRESENTATION_RELATIONSHIP")>0)
     {      printf("%d %s %s",j, step_object[j].name,step_object[j].class_name);
      for (i=0; i<step_object[j].nr; i++) printf(" [%s]",step_object[j].param[i]);
      printf(" [%s]",step_object[j].rest);
      printf("\n");
     }
    }
  }
  */

  /* generate cgx-geometry objects from step_objects */
  if( splitFlag)
  {
    sprintf(buffer,"cgx_tmp.fbl");
    handlefbl = fopen (buffer, "w");
    if (handlefbl==NULL)
    {
      printf (" ERROR: The command file \"%s\" could not be opened.\n\n", buffer);
      exit(0);
    }
  }
  for(j=1; j<=max_indx; j++) if (compareStrings(step_object[j].class_name, "CONVERSION_BASED_UNIT")>0) if(compare(step_object[j].param[0],"'DEGREE",5)==5) { unit_deg=1; }
  for(j=1; j<=max_indx; j++) if (compareStrings(step_object[j].class_name, "CARTESIAN_POINT")>0) make_point(&step_object[j]);
  for(j=1; j<=max_indx; j++) if (compareStrings(step_object[j].class_name, "EDGE_CURVE")>0) make_edge_curve(step_object, j);
  for(j=1; j<=max_indx; j++) if (compareStrings(step_object[j].class_name, "TRIMMED_CURVE")>0) make_trimmed_curve(step_object, j);
  for(j=1; j<=max_indx; j++) if (compareStrings(step_object[j].class_name, "B_SPLINE_CURVE_WITH_KNOTS")>0) make_b_spline_curve_with_knots(step_object, j);
  for(j=1; j<=max_indx; j++) if (compareStrings(step_object[j].class_name, "BOUNDED_CURVE")>0) make_bounded_curve(step_object, j);

  /* make sets */
  for(j=1; j<=max_indx; j++) if (compareStrings(step_object[j].class_name, "SHELL_BASED_SURFACE_MODEL")>0) make_surfaceSet(step_object, j);
  for(j=1; j<=max_indx; j++) if (compareStrings(step_object[j].class_name, "MANIFOLD_SOLID_BREP")>0) make_manifoldSet(step_object, j);
  for(j=1; j<=max_indx; j++) if (compareStrings(step_object[j].class_name, "ADVANCED_BREP_SHAPE_REPRESENTATION")>0) make_shapeSet(step_object, j);
  for(j=1; j<=max_indx; j++) if (compareStrings(step_object[j].class_name, "SHAPE_REPRESENTATION_RELATIONSHIP")>0) copy_shapeSet(step_object, j);
  //for(j=1; j<=max_indx; j++) if (compareStrings(step_object[j].class_name, "AXIS2_PLACEMENT_3D")>0) make_axisSet(step_object, j);
  for(j=1; j<=max_indx; j++) if (compareStrings(step_object[j].class_name, "REPRESENTATION_RELATIONSHIP")>0) copy_and_move_shapeSet(step_object, j);

  /* change the setnames to the partnames */
  for(j=1; j<=max_indx; j++) if (compareStrings(step_object[j].class_name, "SHAPE_DEFINITION_REPRESENTATION")>0) make_setName(step_object, j);

  /* create the assemble file */
  if( splitFlag)
  {
    fprintf(handlefbl,"plot l %s\ntext displayed shape: %s\n", set[assembly_set].name, set[assembly_set].name);
    fclose(handlefbl);

    sprintf(buffer,"cat cgx_tmp.fbl >> cgx_tmp2.fbl");
    system(buffer);
    sprintf(buffer,"mv -f cgx_tmp2.fbl %s.fbl", datin);
    system(buffer);
    sprintf(buffer,"rm -f cgx_tmp.fbl");
    system(buffer);
  }

  /* complete the sets which holds all copied bodysets */
  completeSet( setcopy, "do" );
  completeSet( setbody, "do" );

  /* scal the work-space */
  if(!gtolFlag)
  {
    for(i=0; i<3; i++)
    {
      max[i]=-MAX_INTEGER;
      min[i]= MAX_INTEGER;
    }
    /* calculate the geometric tolerance based on all points */
    for(i=0; i<anzGeo->p; i++) if(point[i].name!=(char *)NULL)
    {
      if(point[i].px > max[0]) max[0]=point[i].px;
      if(point[i].py > max[1]) max[1]=point[i].py;
      if(point[i].pz > max[2]) max[2]=point[i].pz;
      if(point[i].px < min[0]) min[0]=point[i].px;
      if(point[i].py < min[1]) min[1]=point[i].py;
      if(point[i].pz < min[2]) min[2]=point[i].pz;
    }
    /* calculate the geometric tolerance based on all line-end-points */
    /*
    for(i=0; i<anzGeo->l; i++) if(line[i].name!=(char *)NULL)
    {
      if(point[line[i].p1].px > max[0]) max[0]=point[line[i].p1].px;
      if(point[line[i].p2].px > max[0]) max[0]=point[line[i].p2].px;
      if(point[line[i].p1].py > max[1]) max[1]=point[line[i].p1].py;
      if(point[line[i].p2].py > max[1]) max[1]=point[line[i].p2].py;
      if(point[line[i].p1].pz > max[2]) max[2]=point[line[i].p1].pz;
      if(point[line[i].p2].pz > max[2]) max[2]=point[line[i].p2].pz;
  
      if(point[line[i].p1].px < min[0]) min[0]=point[line[i].p1].px;
      if(point[line[i].p2].px < min[0]) min[0]=point[line[i].p2].px;
      if(point[line[i].p1].py < min[1]) min[1]=point[line[i].p1].py;
      if(point[line[i].p2].py < min[1]) min[1]=point[line[i].p2].py;
      if(point[line[i].p1].pz < min[2]) min[2]=point[line[i].p1].pz;
      if(point[line[i].p2].pz < min[2]) min[2]=point[line[i].p2].pz;
    }
    */
    gtol=0.;
    for(i=0; i<3; i++)
    {
      max[i]-=min[i];
      if(max[i]>gtol) gtol=max[i];
    }
    gtol=GTOL*gtol*scale->w;
    printf("\ngtol set to:%e\n\n", gtol);
  }

  /* delete the entities in specialSet->zap */
  zap(specialset->zap);

  descalAll();
  getScaleValues( setall, set, point, node, scale);
  scalNodes ( anz->n, node, scale );
  scalPoints ( anzGeo->p, point, scale );
  scalSurfs( anzGeo->s, surf, scale);

  /* recalculate the line-shapes */
  for (i=0; i<anzGeo->l; i++) repLine(i);
  /* recalculate the nurbl-controll-points */
  for (i=0; i<anzGeo->nurl; i++) repNurl(i);
  /* recalculate the nurbs-controll-points */
  for (i=0; i<anzGeo->nurs; i++) repNurs(i);

  /* das neue netz muss noch zur beleuchteten ansicht aufbereitet werden  */
  makeSurfaces();
  realloc_colNr();

  sprintf(buffer,"l %s    \n", set[assembly_set].name);
  printf("plot %s\n", buffer);
  plot(buffer);
  printf (" done \n\n");
  return(assembly_set);
}

