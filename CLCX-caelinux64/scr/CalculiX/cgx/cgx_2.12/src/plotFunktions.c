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

/* TO DO
*/
#include <cgx.h>

#define TEST 0


extern GLfloat entity_r[SET_COLS];                     /* predefined colors of entities */
extern GLfloat entity_g[SET_COLS];
extern GLfloat entity_b[SET_COLS];


extern int   basCol[3];                        /* color indexes due to basic colormap: 0=bl. 1=w 2=gy */
extern int   foregrndcol, backgrndcol;          /* default fore- and background color */
extern double foregrndcol_rgb[4];
extern double backgrndcol_rgb[4];

extern char  datin[MAX_LINE_LENGTH];         /* Input-data-file */
extern char  buffer[MAX_LINE_LENGTH];
extern int   offset, maxIndex;                        /* offset+steps-1 = maxIndex */
extern int   width_ini, height_ini; /* Grafig-Fensterbreite/hoehe */
extern int   width_menu, height_menu;
extern int   w0, w1, w_index, w_rgb, activWindow;
extern int   width_w0, height_w0;
extern int   width_w1, height_w1;
extern int   MouseMode;                                   /* status maustasten */
extern double dtx, dty, dtz, drx, dry, drz, ds;                 /* Verschiebungen */
extern double vmem[4];                                     /* kor. bis auswahl eines neuen drehpkts */
extern double anim_faktor;                            /* Scalierung der Amplitude */
extern char  drawMode;               /* protokoliert drawFunktion (Load=1,Light=2,Animate=3,preprocessor=4, vector=5)*/
extern   double lmodel_twoside[1];
extern GLint   gl_max_eval_order;                         /* max order of NURBS */
extern GLdouble R[4][4];                                   /* Rotationsmatrix */
extern char  surfFlag;                /* zeichne nur Oberflaechenelemente (1), sonst (0)*/
extern char  modelEdgeFlag;                /* zeichne mit Modell-Ecken (1), sonst (0)*/
extern char  frameFlag;                   /* mit (1) oder ohne Rahmen um das Grafikfenster */
extern char  filenamFlag;                   /* mit (1) oder ohne filename im Menufenster */
extern char  textFlag;                   /* mit (1) oder ohne text im Menufenster */
extern char  printFlag;                     /* printf 1:on 0:off */
extern char  frameFlag;               /* mit (1) oder ohne Rahmen um das Grafigfenster */
extern char  scalaFlag;                 /* mit (1) oder ohne scala und wertetexte */ 
extern char  addDispFlag;                    /* 0: original node-coordinates, 1: node-coordinates+displacements */
extern double dx ,dy;                                      /* Mauskoordinaten */
extern int   steps;                          /* Schrittweite der Farbscala */
extern double gtol;
extern int     ddiv;
extern double     dbias;
extern char  picture_text[MAX_LINE_LENGTH], datin[MAX_LINE_LENGTH];
extern double v_scale;                                    /* scaling-factor for the vectors in the vector-plot */

extern char  delPntFlag;                    /* 1: deleted points exists */
extern char  delLineFlag;                   /* 1: deleted lines exists */
extern char  delLcmbFlag;                   /* 1: deleted lcmbs exists */
extern char  delSurfFlag;                   /* 1: deleted surfs exists */
extern char  delBodyFlag;                   /* 1: deleted bodys exists */
extern char  delNursFlag;
extern char  delSetFlag;                  /* 1: deleted sets exists */
extern char  writeCFDflag;                /* 1: write cfd-files for duns during meshing */
extern char  movieFlag;                     /* >0: save sequence of gif pictures */

extern int     elemMat[MAX_MATERIALS];      /*  Material Numbers, Number of Materials stored in elemMat[0]  */

#if INX_MODE
int       *colNr;
#define  GLCOLOR   glIndexi
#endif
#if TEX_MODE
double     *colNr;
#define  GLCOLOR   glTexCoord1d
#endif


extern Scale     scale[1];
extern Elements  *e_enqire;
extern Summen    anz[1];
extern Edges     *edge;
extern Nodes     *node;
extern Datasets *lcase;
extern Faces     *face;
extern Texts     *ntext;

extern Meshp meshp;

extern Alias     *alias;
extern Sets      *set;
extern Psets     *pset;
extern Points    *point;
extern Lines     *line;
extern Lcmb      *lcmb;
extern Gsur      *surf;
extern Gbod      *body;
extern Nurbl     *nurbl;
extern Nurbs     *nurbs;
extern SumGeo    anzGeo[1];
extern SumAsci   sumAsci[1];

extern int  cur_entity;                                       /* aktive entity (component) */
extern int  cur_lc;
extern void       *glut_font[];  /* glut fonts */
extern int       pixPerChar[];
extern int       legend_font;                         /* active font for the legend */
extern int       draw_font;                         /* active font for the annotation of entities */
extern int       menu_font;                         /* active font for the menu */


/* additional entities */
extern OpenSets   openSets[1];
extern SpecialSet specialset[1];


int current_nurbs;
static void NurbsErrorCallback(GLenum which)
{
  printf(" GluNURBS: error occured (%d):\n", which);
  if(current_nurbs>-1)
  {
    printf("    %s in nurbs:%s see set:%s\n", gluErrorString(which), nurbs[current_nurbs].name,specialset->bnur);
    pre_seta(specialset->bnur,"S",nurbs[current_nurbs].name);
  }
  else
  {
    printf("    %s in nurl:%s see set:%s\n", gluErrorString(which), nurbs[-current_nurbs-1].name,specialset->bnur);
    pre_seta(specialset->bnur,"L",nurbs[current_nurbs].name);
  }
}



/* prepares vectors pointing from the nodes  (like a tape in a storm)*/
/* the length is proportional to the value of the vector */
/* lc Dataset */
/* ne number of entities */
/* *e entities */
/* v_factor scale factor for the vectors */
void drawNodes_vector(  int lc, int ne, int *e, double v_factor, int num, int *name, Nodes *node )
{
  register int i,k;
  double p1[3], p2[3], v2[3];

#if TEST
  printf ("in drawNodes_vector\n");
#endif 

  glLineWidth(1);
  glPointSize (2);
#if INX_MODE
  glIndexi( basCol[foregrndcol] );
#endif
#if TEX_MODE
  glColor3dv( foregrndcol_rgb );
#endif

  if(ne>3) ne=3;
  for (i=0; i<num; i++ )
  {
    /* calculate the start and end-point of the vector */
    if(node[name[i]].pflag!=1)
    {
      for(k=0; k<3; k++) v2[k]=0.;
      for(k=0; k<ne; k++) v2[k]+=lcase[lc].dat[e[k]][name[i]];
      p1[0]=node[name[i]].nx-(v2[0]*v_factor);  
      p1[1]=node[name[i]].ny-(v2[1]*v_factor);  
      p1[2]=node[name[i]].nz-(v2[2]*v_factor);  
      p2[0]=node[name[i]].nx+(v2[0]*v_factor);  
      p2[1]=node[name[i]].ny+(v2[1]*v_factor);  
      p2[2]=node[name[i]].nz+(v2[2]*v_factor);  
      glBegin ( GL_LINES );
      glVertex3dv ( p1 );
      glVertex3dv ( p2 );
      glEnd();
      glBegin ( GL_POINTS );
      glVertex3dv ( p2 );
      glEnd();
    }
  }
}



/* prepares vectors pointing from the nodes of the faces (like a tape in a storm)*/
/* the length is proportional to the value of the vector */
/* lc Dataset */
/* ne number of entities */
/* *e entities */
/* v_factor scale factor for the vectors */
void drawFaces_vector(  int lc, int ne, int *e, double v_factor, int num, int *name, Nodes *node, Faces *face )
{
  register int i,j,k,n;
  double p1[3], p2[3], v2[3];

#if TEST
  printf ("in drawFaces_vector\n");
#endif 

  glLineWidth(1);
  glPointSize (2);
#if INX_MODE
  glIndexi( basCol[foregrndcol] );
#endif
#if TEX_MODE
  glColor3dv( foregrndcol_rgb );
#endif

  if(ne>3) ne=3;
  for (i=0; i<num; i++ )
  {
    n=0;
    if ( face[name[i]].type == 7 ) n=3;
    else
    if ( face[name[i]].type == 8 ) n=6;
    else
    if ( face[name[i]].type == 9 ) n=4;
    else
    if ( face[name[i]].type == 10) n=8;
    if(n>0)
    {
      /* calculate the start and end-point of the vector */
      for(j=0; j<n; j++)
      {
        if(node[face[name[i]].nod[j]].pflag!=1)
        {
          for(k=0; k<3; k++) v2[k]=0.;
          for(k=0; k<ne; k++) v2[k]+=lcase[lc].dat[e[k]][face[name[i]].nod[j]];
          p1[0]=node[face[name[i]].nod[j]].nx+face[name[i]].side[0][0]*1.e-2*ds-(v2[0]*v_factor);  
          p1[1]=node[face[name[i]].nod[j]].ny+face[name[i]].side[0][1]*1.e-2*ds-(v2[1]*v_factor);  
          p1[2]=node[face[name[i]].nod[j]].nz+face[name[i]].side[0][2]*1.e-2*ds-(v2[2]*v_factor);  
          p2[0]=node[face[name[i]].nod[j]].nx+face[name[i]].side[0][0]*1.e-2*ds+(v2[0]*v_factor);  
          p2[1]=node[face[name[i]].nod[j]].ny+face[name[i]].side[0][1]*1.e-2*ds+(v2[1]*v_factor);  
          p2[2]=node[face[name[i]].nod[j]].nz+face[name[i]].side[0][2]*1.e-2*ds+(v2[2]*v_factor);  
          glBegin ( GL_LINES );
          glVertex3dv ( p1 );
          glVertex3dv ( p2 );
          glEnd();
          glBegin ( GL_POINTS );
          glVertex3dv ( p2 );
          glEnd();
        }
      }
    }
  }
}


void drawElements_vector( int lc, int ne, int *e, double v_factor, int num, int *name, Nodes *node, Elements *e_enqire )
{
  register int i,j,k,n;
  double p1[3], p2[3], v2[3];

#if TEST
  printf ("in drawElements_vector\n");
#endif 

  glLineWidth(1);
  glPointSize (2);
#if INX_MODE
  glIndexi( basCol[foregrndcol] );
#endif
#if TEX_MODE
  glColor3dv( foregrndcol_rgb );
#endif

  if(ne>3) ne=3;
  for (i=0; i<num; i++ )
  {
    n=0;
    if( e_enqire[name[i]].type == 1 ) n=8;
    else
    if( e_enqire[name[i]].type == 2 ) n=6;
    else
    if( e_enqire[name[i]].type == 3 ) n=4;
    else
    if( e_enqire[name[i]].type == 4 ) n=20;
    else
    if( e_enqire[name[i]].type == 5 ) n=15;
    else
    if( e_enqire[name[i]].type == 6 ) n=10;
    else
    if( e_enqire[name[i]].type == 7 ) n=3;
    else
    if( e_enqire[name[i]].type == 8 ) n=6;
    else
    if( e_enqire[name[i]].type == 9 ) n=4;
    else
    if( e_enqire[name[i]].type == 10 ) n=8;
    if(n>0)
    {
      /* calculate the start and end-point of the vector */
      for(j=0; j<n; j++)
      {
        for(k=0; k<3; k++) v2[k]=0.;
        for(k=0; k<ne; k++) v2[k]+=lcase[lc].dat[e[k]][e_enqire[name[i]].nod[j]];
        p1[0]=node[e_enqire[name[i]].nod[j]].nx-(v2[0]*v_factor);  
        p1[1]=node[e_enqire[name[i]].nod[j]].ny-(v2[1]*v_factor);  
        p1[2]=node[e_enqire[name[i]].nod[j]].nz-(v2[2]*v_factor);  
        p2[0]=node[e_enqire[name[i]].nod[j]].nx+(v2[0]*v_factor);  
        p2[1]=node[e_enqire[name[i]].nod[j]].ny+(v2[1]*v_factor);  
        p2[2]=node[e_enqire[name[i]].nod[j]].nz+(v2[2]*v_factor);  
        glBegin ( GL_LINES );
        glVertex3dv ( p1 );
        glVertex3dv ( p2 );
        glEnd();
        glBegin ( GL_POINTS );
        glVertex3dv ( p2 );
        glEnd();
      }
    }
  }
}




void drawNodes_plot( int num, int *name, Nodes *node , int col, char type)
{
  register int i,j;
  int      mode;
  GLint    viewport[4];
  GLdouble mvmatrix[16], projmatrix[16];
  static GLdouble wx, wy, wz, wxl;  /*  returned window x, y, z coords  */
  double mxl,myl,mzl;
  static int flag2;

  glLineWidth(1);
  glPointSize (4);

#if INX_MODE
  glIndexi (col+offset+CMAP_CELLS_LIGHT);
#endif
#if TEX_MODE
  glColor3f ( entity_r[col], entity_g[col], entity_b[col]  );
#endif

  if(type=='t')
  {
      glGetIntegerv (GL_VIEWPORT, viewport);
      glGetDoublev (GL_MODELVIEW_MATRIX, mvmatrix);
      glGetDoublev (GL_PROJECTION_MATRIX, projmatrix);
      for (i=0; i<num; i++ )
      {
       for(j=0; j<anz->t; j++)
       {
        if(ntext[j].node_nr==name[i])
	{
          if((ntext[j].nFlag)&&(ntext[j].vFlag)&&(anz->l))
	  {
            switch(ntext[j].fFlag)
	    {
	    case 0:
               sprintf(buffer,"%d %6.2le", name[i], lcase[cur_lc].dat[cur_entity][name[i]]);
	      break;
	    case 1:
               sprintf(buffer,"%d %.2f", name[i], lcase[cur_lc].dat[cur_entity][name[i]]);
	      break;
	    case 2:
	      sprintf(buffer,"%d %d", name[i], (int)lcase[cur_lc].dat[cur_entity][name[i]]);
	      break;
	      printf("ERROR case:%d\n", ntext[j].fFlag);
	    }
	  }
          else if(ntext[j].nFlag) sprintf(buffer,"%d", name[i]);
          else if((ntext[j].vFlag)&&(anz->l))
	  {
            switch(ntext[j].fFlag)
	    {
	    case 0:
               sprintf(buffer,"%6.2le", lcase[cur_lc].dat[cur_entity][name[i]]);
	      break;
	    case 1:
               sprintf(buffer,"%.2f", lcase[cur_lc].dat[cur_entity][name[i]]);
	      break;
	    case 2:
	      sprintf(buffer,"%d", (int)lcase[cur_lc].dat[cur_entity][name[i]]);
	      break;
	      printf("ERROR case:%d\n", ntext[j].fFlag);
	    }
	  }

          glLoadName('n');
            glPushName(name[i]);
            glBegin ( GL_POINTS );
            glVertex3d ( node[name[i]].nx, node[name[i]].ny, node[name[i]].nz );
            glEnd();
            glPopName();
          glLoadName(-1);

          /* remark, calc of correct text coords works only if the related node is visible. this is not the case during picking and the last coords are taken */ 
          glGetIntegerv(GL_RENDER_MODE,&mode);
          //printf("mode:%d GL_SELECT:%d GL_RENDER:%d\n", mode, GL_SELECT, GL_RENDER);
          if(mode==GL_SELECT)
	  {
            glLoadName('t');
            glPushName(j);
            glBegin ( GL_LINE_STRIP );
	      glVertex3dv( &node[name[i]].nx);
              /* mxl, myl, mzl would have to be updated and therefore this code has to be skipped for picking */
	      //if(wxl!=ntext[j].tx) glVertex3d( mxl, myl, mzl);
	      //else
                glVertex3d( ntext[j].mx, ntext[j].my, ntext[j].mz);
            glEnd();
            text( ntext[j].mx, ntext[j].my, ntext[j].mz, buffer, glut_font[draw_font] );
            glPopName();
            glLoadName(-1);
            continue;
	  }
          flag2=gluProject( node[name[i]].nx, node[name[i]].ny, node[name[i]].nz, mvmatrix, projmatrix, viewport,  &wx, &wy, &wz);
          if (flag2==GL_FALSE) printf("WARNING: Malfunction, please reselect\n");
	  //printf("nxyz:%f %f %f\n",node[name[i]].nx, node[name[i]].ny, node[name[i]].nz); 
	  //printf("wxyz:%f %f %f  %f %f\n",wx, wy, wz, ntext[j].tx, ntext[j].ty); 
          if(ntext[j].tx>0)
	  {
            /* line-start might be at the end of text (wxl) if CG-text_x < node_x */
	    if((int)ntext[j].tx+strlen(buffer)*pixPerChar[draw_font]/2 < wx)
	    {
              wxl=ntext[j].tx+strlen(buffer)*pixPerChar[draw_font];
            }
            else wxl=ntext[j].tx;   

            wx=ntext[j].tx;
            wy=viewport[3]-ntext[j].ty;

	  }
          else wxl=wx;


          wz=0.01; // close to the user, in front of the model
          if (gluUnProject (wx, wy, wz,  mvmatrix, projmatrix, viewport, &ntext[j].mx, &ntext[j].my, &ntext[j].mz)==GL_TRUE)
          {
            /* new model coords of the text-end */
            gluUnProject (wxl, wy, wz,  mvmatrix, projmatrix, viewport, &mxl, &myl, &mzl);
	    //printf ("model coords are %lf, %lf, %lf\n", ntext[j].mx, ntext[j].my, ntext[j].mz);
            glBegin ( GL_LINE_STRIP );
	      glVertex3dv( &node[name[i]].nx);
	      if(wxl!=ntext[j].tx) glVertex3d( mxl, myl, mzl);
	      else glVertex3d( ntext[j].mx, ntext[j].my, ntext[j].mz);
            glEnd();
            text( ntext[j].mx, ntext[j].my, ntext[j].mz, buffer, glut_font[draw_font] );
          }
          else printf("WARNING: Malfunction, please reselect\n");
          break;
        }
       }
      }
    return;
  }

  glLoadName('n');
  for (i=0; i<num; i++ )
  {
    glPushName(name[i]);
    glBegin ( GL_POINTS );
    glVertex3d ( node[name[i]].nx, node[name[i]].ny, node[name[i]].nz );
    glEnd();
    glPopName();
  }
  glLoadName(-1);

  if(type=='a')
  {
    for (i=0; i<num; i++ )
    {
      sprintf( buffer, "%d ", name[i]);
      text( node[name[i]].nx, node[name[i]].ny, node[name[i]].nz, buffer, glut_font[draw_font] );
    }
  }
  else if(type=='v')
  {
    if(anz->l)
    {
      glGetIntegerv (GL_VIEWPORT, viewport);
      glGetDoublev (GL_MODELVIEW_MATRIX, mvmatrix);
      glGetDoublev (GL_PROJECTION_MATRIX, projmatrix);
      for (i=0; i<num; i++ )
      {
        sprintf( buffer, "%6.2le ", lcase[cur_lc].dat[cur_entity][name[i]]);
        text( node[name[i]].nx, node[name[i]].ny, node[name[i]].nz, buffer, glut_font[draw_font] );
      }
    }
  }
}



void drawElements_plot( int num, int *name, Nodes *node, double *colNr, Elements *e_enqire, int col, char type, int pickflag )
{
  register int i, j, n;
  double x, y, z;
  static GLint ipuf[2];

  glLineWidth(1.);
  glGetIntegerv( GL_POLYGON_MODE, ipuf );

  if((type=='v')&&(!pickflag))
  {
#if TEX_MODE
    /* enable all colors for the TEX_MODE */
    glColor3d( 1,1,1);
    glEnable(GL_TEXTURE_1D);
#endif
    for (i=0; i<num; i++ )
    {
      if(( e_enqire[name[i]].type <7 )&&( ipuf[1] == GL_FILL )) glEnable ( GL_CULL_FACE ); else glDisable ( GL_CULL_FACE );
      switch(e_enqire[name[i]].type)
      {
    	case 1:
        glBegin ( GL_TRIANGLE_STRIP );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[1]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[2]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[5]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[6]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[6]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[4]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[7]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[7]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[0]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[3]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
        glEnd();
        glBegin ( GL_TRIANGLE_STRIP );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[5]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[4]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[1]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[0]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[2]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[3]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[6]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[6]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[7]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[7]].nx );
        glEnd();
      	break;

    	case 2:
        glBegin ( GL_TRIANGLE_STRIP );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[3]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[0]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[4]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[1]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[5]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[2]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[3]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[0]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
        glEnd();
        glBegin ( GL_TRIANGLES      );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[3]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[4]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[5]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[0]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[2]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[1]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
        glEnd();
      	break;

    	case 3:
        glBegin ( GL_TRIANGLE_FAN );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[0]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[1]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[3]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[2]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[1]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
        glEnd();
        glBegin ( GL_TRIANGLES );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[1]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[2]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[3]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
        glEnd();
      	break;

    	case 4:
        glBegin ( GL_TRIANGLE_STRIP );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[1]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[9]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[9]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[13]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[13]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[21]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[21]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[5]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[17]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[17]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[16]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[16]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[25]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[25]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[4]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[19]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[19]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[12]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[12]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[23]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[23]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[0]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[11]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[11]].nx );
        glEnd();
        glBegin ( GL_TRIANGLE_STRIP );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[9]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[9]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[2]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[21]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[21]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[14]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[14]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[17]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[17]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[6]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[6]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[25]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[25]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[18]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[18]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[19]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[19]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[7]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[7]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[23]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[23]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[15]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[15]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[11]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[11]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[3]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
        glEnd();
        glBegin ( GL_TRIANGLE_STRIP );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[5]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[16]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[16]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[13]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[13]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[20]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[20]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[1]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[8]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[8]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[9]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[9]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[24]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[24]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[2]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[10]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[10]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[14]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[14]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[22]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[22]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[6]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[6]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[18]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[18]].nx );
        glEnd();
        glBegin ( GL_TRIANGLE_STRIP );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[16]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[16]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[4]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[20]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[20]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[12]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[12]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[8]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[8]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[0]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[24]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[24]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[11]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[11]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[10]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[10]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[3]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[22]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[22]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[15]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[15]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[18]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[18]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[7]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[7]].nx );
        glEnd();
      	break;

    	case 5:
        glBegin ( GL_TRIANGLE_STRIP );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[1]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[10]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[10]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[ 6]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 6]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[15]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[15]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[ 0]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 0]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[ 9]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 9]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[ 0]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 0]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[ 9]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 9]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[8]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[8]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[17]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[17]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[ 2]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 2]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[11]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[11]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[7]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[7]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[16]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[16]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[1]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[10]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[10]].nx );
        glEnd();
        glBegin ( GL_TRIANGLE_STRIP );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[10]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[10]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[ 4]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 4]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[15]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[15]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[12]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[12]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[ 9]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 9]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[ 3]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 3]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[17]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[17]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[14]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[14]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[11]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[11]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[ 5]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 5]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[16]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[16]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[13]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[13]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[10]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[10]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[ 4]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 4]].nx );
        glEnd();
        glBegin ( GL_TRIANGLE_STRIP );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[ 2]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 2]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[ 7]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 7]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[8]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[8]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[18]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[18]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[0]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
        glEnd();
        glBegin ( GL_TRIANGLE_STRIP );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[ 7]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 7]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[ 1]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 1]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[18]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[18]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[ 6]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 6]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[0]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
        glEnd();
        glBegin ( GL_TRIANGLE_STRIP );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[ 4]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 4]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[13]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[13]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[12]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[12]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[19]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[19]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[3]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
        glEnd();
        glBegin ( GL_TRIANGLE_STRIP );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[13]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[13]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[ 5]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 5]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[19]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[19]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[14]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[14]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[3]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
        glEnd();
      	break;

    	case 6:
        glBegin ( GL_TRIANGLE_STRIP );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[1]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[8]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[8]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[4]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[7]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[7]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[0]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
        glEnd();
        glBegin ( GL_TRIANGLE_STRIP );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[2]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[9]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[9]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[5]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[8]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[8]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[1]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
        glEnd();
        glBegin ( GL_TRIANGLE_STRIP );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[0]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[7]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[7]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[6]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[6]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[9]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[9]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[2]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
        glEnd();
        glBegin ( GL_TRIANGLE_STRIP );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[2]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[5]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[6]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[6]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[4]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[0]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
        glEnd();
        glBegin ( GL_TRIANGLE_FAN   );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[3]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[7]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[7]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[8]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[8]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[9]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[9]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[7]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[7]].nx );
        glEnd();
        glBegin ( GL_TRIANGLES   );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[1]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[4]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[5]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
        glEnd();
      	break;

    	case 7:
        glBegin ( GL_TRIANGLES );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[0]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[1]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[2]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
        glEnd();
      	break;

    	case 8:
        glBegin ( GL_TRIANGLES );
          GLCOLOR     ( colNr[e_enqire[name[i]].nod[0]] );
          glVertex3dv  ( &node[e_enqire[name[i]].nod[0]].nx );
          GLCOLOR     ( colNr[e_enqire[name[i]].nod[3]] );
          glVertex3dv  ( &node[e_enqire[name[i]].nod[3]].nx );
          GLCOLOR     ( colNr[e_enqire[name[i]].nod[5]] );
          glVertex3dv  ( &node[e_enqire[name[i]].nod[5]].nx );
    
          GLCOLOR     ( colNr[e_enqire[name[i]].nod[2]] );
          glVertex3dv  ( &node[e_enqire[name[i]].nod[2]].nx );
          GLCOLOR     ( colNr[e_enqire[name[i]].nod[5]] );
          glVertex3dv  ( &node[e_enqire[name[i]].nod[5]].nx );
          GLCOLOR     ( colNr[e_enqire[name[i]].nod[4]] );
          glVertex3dv  ( &node[e_enqire[name[i]].nod[4]].nx );
    
          GLCOLOR     ( colNr[e_enqire[name[i]].nod[4]] );
          glVertex3dv  ( &node[e_enqire[name[i]].nod[4]].nx );
          GLCOLOR     ( colNr[e_enqire[name[i]].nod[5]] );
          glVertex3dv  ( &node[e_enqire[name[i]].nod[5]].nx );
          GLCOLOR     ( colNr[e_enqire[name[i]].nod[3]] );
          glVertex3dv  ( &node[e_enqire[name[i]].nod[3]].nx );
    
          GLCOLOR     ( colNr[e_enqire[name[i]].nod[3]] );
          glVertex3dv  ( &node[e_enqire[name[i]].nod[3]].nx );
          GLCOLOR     ( colNr[e_enqire[name[i]].nod[1]] );
          glVertex3dv  ( &node[e_enqire[name[i]].nod[1]].nx );
          GLCOLOR     ( colNr[e_enqire[name[i]].nod[4]] );
          glVertex3dv  ( &node[e_enqire[name[i]].nod[4]].nx );
        glEnd();
      	break;

    	case 9:
        glBegin ( GL_TRIANGLE_STRIP );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[0]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[1]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[3]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[2]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
        glEnd();
      	break;

    	case 10:
        glBegin ( GL_TRIANGLE_FAN   );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[8]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[8]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[0]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[4]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[1]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[5]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[2]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[6]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[6]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[3]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[7]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[7]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[0]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
        glEnd();
      	break;

    	case 11:
        glBegin ( GL_LINES );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[0]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[1]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
        glEnd();
      	break;

    	case 12:
        glBegin ( GL_LINE_STRIP );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[0]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[2]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
          GLCOLOR    ( colNr[e_enqire[name[i]].nod[1]] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
        glEnd();
      }
    }
#if TEX_MODE
    glDisable(GL_TEXTURE_1D);
#endif
  }
  else if(pickflag)
  {
    /* any other color */
#if INX_MODE
    glIndexi (col+offset+CMAP_CELLS_LIGHT);
#endif
#if TEX_MODE
    glColor3f ( entity_r[col], entity_g[col], entity_b[col]  );
#endif 
    glLoadName('e');

    glGetIntegerv( GL_POLYGON_MODE, ipuf );

    for (i=0; i<num; i++ )
    {
      if(( e_enqire[name[i]].type <7 )&&( ipuf[1] == GL_FILL )) glEnable ( GL_CULL_FACE ); else glDisable ( GL_CULL_FACE );
      glPushName(name[i]);
      switch(e_enqire[name[i]].type)
      {
    	case 1:
    	case 4:
        glBegin ( GL_TRIANGLES );
         glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
        glEdgeFlag ( GL_FALSE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
        glEdgeFlag ( GL_TRUE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
  
         glVertex3dv ( &node[e_enqire[name[i]].nod[6]].nx );
        glEdgeFlag ( GL_FALSE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
        glEdgeFlag ( GL_TRUE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
  
         glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
        glEdgeFlag ( GL_FALSE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[6]].nx );
        glEdgeFlag ( GL_TRUE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
  
         glVertex3dv ( &node[e_enqire[name[i]].nod[7]].nx );
        glEdgeFlag ( GL_FALSE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
        glEdgeFlag ( GL_TRUE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[6]].nx );
  
         glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
        glEdgeFlag ( GL_FALSE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[7]].nx );
        glEdgeFlag ( GL_TRUE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
  
         glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
        glEdgeFlag ( GL_FALSE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
        glEdgeFlag ( GL_TRUE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[7]].nx );
  
      /* new strip */
         glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
        glEdgeFlag ( GL_FALSE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
        glEdgeFlag ( GL_TRUE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
  
         glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
        glEdgeFlag ( GL_FALSE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
        glEdgeFlag ( GL_TRUE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
  
         glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
        glEdgeFlag ( GL_FALSE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
        glEdgeFlag ( GL_TRUE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
  
         glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
        glEdgeFlag ( GL_FALSE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
        glEdgeFlag ( GL_TRUE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
  
         glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
        glEdgeFlag ( GL_FALSE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
        glEdgeFlag ( GL_TRUE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[6]].nx );
  
         glVertex3dv ( &node[e_enqire[name[i]].nod[7]].nx );
        glEdgeFlag ( GL_FALSE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[6]].nx );
        glEdgeFlag ( GL_TRUE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
       glEnd();
       break;

       case 2:
       case 5:
       glBegin ( GL_TRIANGLES );
         glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
        glEdgeFlag ( GL_FALSE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
        glEdgeFlag ( GL_TRUE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
  
         glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
        glEdgeFlag ( GL_FALSE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
        glEdgeFlag ( GL_TRUE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
  
         glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
        glEdgeFlag ( GL_FALSE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
        glEdgeFlag ( GL_TRUE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
  
         glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
        glEdgeFlag ( GL_FALSE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
        glEdgeFlag ( GL_TRUE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
  
         glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
        glEdgeFlag ( GL_FALSE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
        glEdgeFlag ( GL_TRUE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
  
         glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
        glEdgeFlag ( GL_FALSE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
        glEdgeFlag ( GL_TRUE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
  
         glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
        glEdgeFlag ( GL_FALSE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
        glEdgeFlag ( GL_TRUE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
  
         glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
        glEdgeFlag ( GL_FALSE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
        glEdgeFlag ( GL_TRUE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
       glEnd();
       break;

       case 3:
       case 6:
       glBegin ( GL_TRIANGLES );
         glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
         glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
         glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
       glEnd();
       glBegin ( GL_TRIANGLE_FAN   );
         glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
         glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
         glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
         glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
         glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
       glEnd();
       break;

       case 7:
       case 8:
       glBegin ( GL_TRIANGLES );
         glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
         glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
         glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
       glEnd();
       break;

       case 9:
       glBegin ( GL_TRIANGLES );
         glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
        glEdgeFlag ( GL_FALSE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
        glEdgeFlag ( GL_TRUE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
  
         glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
        glEdgeFlag ( GL_FALSE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
        glEdgeFlag ( GL_TRUE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
       glEnd();
       break;

       case 10:
       glBegin ( GL_TRIANGLES  );
         glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
        glEdgeFlag ( GL_FALSE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
        glEdgeFlag ( GL_TRUE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[7]].nx );
  
        glEdgeFlag ( GL_FALSE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[7]].nx );
         glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
         glVertex3dv ( &node[e_enqire[name[i]].nod[6]].nx );
  
        glEdgeFlag ( GL_TRUE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[6]].nx );
         glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
        glEdgeFlag ( GL_FALSE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[7]].nx );
  
  
        glEdgeFlag ( GL_TRUE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
        glEdgeFlag ( GL_FALSE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[6]].nx );
        glEdgeFlag ( GL_TRUE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
  
        glEdgeFlag ( GL_FALSE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
         glVertex3dv ( &node[e_enqire[name[i]].nod[6]].nx );
         glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
  
        glEdgeFlag ( GL_TRUE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
         glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
        glEdgeFlag ( GL_FALSE );
         glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
        glEdgeFlag ( GL_TRUE );
       glEnd();
       break;

       case 11:
       glBegin ( GL_LINES );
         glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
         glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
       glEnd();
       break;

       case 12:
       glBegin ( GL_LINE_STRIP );
         glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
         glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
         glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
       glEnd();
       break;
      }
  
      if(type=='a')
      {
	switch(e_enqire[name[i]].type)
	{
		case 1:
		case 4:
			j=8;
			break;
		case 2:
		case 5:
			j=6;
			break;
		case 3:
		case 6:
		case 9:
		case 10:
			j=4;
			break;
		case 7:
		case 8:
		case 12:
			j=3;
			break;
		case 11: 
			j=2;
			break;
		default:
			// assure that j!=0 (see division below)
		        printf("ERROR: type:%d of elem:%d not known\n",e_enqire[name[i]].type,name[i]);
			j=1;
			break;
	}
        x=y=z=0.;
        for (n=0; n<j; n++)
        {
          x+= node[e_enqire[name[i]].nod[n]].nx;
          y+= node[e_enqire[name[i]].nod[n]].ny;
          z+= node[e_enqire[name[i]].nod[n]].nz;
        }
        x/= j; y/= j; z/= j;
        sprintf( buffer, "%d ", name[i]);
        text( x, y, z, buffer, glut_font[draw_font] );
      }
      glPopName();
    }
    glLoadName(-1);
  }
  else
  {
    setLightAndMaterial_rgb(col);
    glEnable( GL_LIGHTING );

    for (i=0; i<num; i++ )
    {
      if(( e_enqire[name[i]].type <7 )&&( ipuf[1] == GL_FILL )) glEnable ( GL_CULL_FACE ); else glDisable ( GL_CULL_FACE );

      switch(e_enqire[name[i]].type)
      {
        case 1:
        glBegin ( GL_TRIANGLE_STRIP );
          glNormal3dv ( e_enqire[name[i]].side[0] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[6]].nx );
        glEnd();
        glBegin ( GL_TRIANGLE_STRIP );
          glNormal3dv ( e_enqire[name[i]].side[1] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[6]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[7]].nx );
        glEnd();
        glBegin ( GL_TRIANGLE_STRIP );
          glNormal3dv ( e_enqire[name[i]].side[2] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[7]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
        glEnd();
        /* math.pos von z->y  */
        glBegin ( GL_TRIANGLE_STRIP );
          glNormal3dv ( e_enqire[name[i]].side[3] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
        glEnd();
        glBegin ( GL_TRIANGLE_STRIP );
          glNormal3dv ( e_enqire[name[i]].side[4] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
        glEnd();
        glBegin ( GL_TRIANGLE_STRIP );
          glNormal3dv ( e_enqire[name[i]].side[5] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[6]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[7]].nx );
        glEnd();
        break;

        case 2:
        glBegin ( GL_TRIANGLES      );
          glNormal3dv ( e_enqire[name[i]].side[0] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
        glEnd();
        glBegin ( GL_TRIANGLES      );
          glNormal3dv ( e_enqire[name[i]].side[1] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
        glEnd();
        glBegin ( GL_TRIANGLE_STRIP );
          glNormal3dv ( e_enqire[name[i]].side[2] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
        glEnd();
        glBegin ( GL_TRIANGLE_STRIP );
          glNormal3dv ( e_enqire[name[i]].side[3] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
        glEnd();
        glBegin ( GL_TRIANGLE_STRIP );
          glNormal3dv ( e_enqire[name[i]].side[4] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
        glEnd();
        break;

        case 3:
        glBegin ( GL_TRIANGLE_FAN );
          glNormal3dv ( e_enqire[name[i]].side[0] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 3]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 0]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 1]].nx );
          glNormal3dv ( e_enqire[name[i]].side[1] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 2]].nx );
          glNormal3dv ( e_enqire[name[i]].side[2] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 0]].nx );
        glEnd();
        glBegin ( GL_TRIANGLES );
          glNormal3dv ( e_enqire[name[i]].side[3] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 0]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 2]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 1]].nx );
        glEnd();
        break;

        case 4:
        glBegin ( GL_TRIANGLE_FAN );
          glNormal3dv ( e_enqire[name[i]].side[0] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[21]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 1]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 9]].nx );
          glNormal3dv ( e_enqire[name[i]].side[1] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 2]].nx );
          glNormal3dv ( e_enqire[name[i]].side[2] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[14]].nx );
          glNormal3dv ( e_enqire[name[i]].side[3] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 6]].nx );
          glNormal3dv ( e_enqire[name[i]].side[4] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[17]].nx );
          glNormal3dv ( e_enqire[name[i]].side[5] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 5]].nx );
          glNormal3dv ( e_enqire[name[i]].side[6] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[13]].nx );
          glNormal3dv ( e_enqire[name[i]].side[7] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 1]].nx );
        glEnd();
        glBegin ( GL_TRIANGLE_FAN );
          glNormal3dv ( e_enqire[name[i]].side[8] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[22]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 2]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[10]].nx );
          glNormal3dv ( e_enqire[name[i]].side[9] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 3]].nx );
          glNormal3dv ( e_enqire[name[i]].side[10] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[15]].nx );
          glNormal3dv ( e_enqire[name[i]].side[11] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 7]].nx );
          glNormal3dv ( e_enqire[name[i]].side[12] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[18]].nx );
          glNormal3dv ( e_enqire[name[i]].side[13] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 6]].nx );
          glNormal3dv ( e_enqire[name[i]].side[14] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[14]].nx );
          glNormal3dv ( e_enqire[name[i]].side[15] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 2]].nx );
        glEnd();
        glBegin ( GL_TRIANGLE_FAN );
          glNormal3dv ( e_enqire[name[i]].side[16] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[23]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 0]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[12]].nx );
          glNormal3dv ( e_enqire[name[i]].side[17] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 4]].nx );
          glNormal3dv ( e_enqire[name[i]].side[18] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[19]].nx );
          glNormal3dv ( e_enqire[name[i]].side[19] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 7]].nx );
          glNormal3dv ( e_enqire[name[i]].side[20] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[15]].nx );
          glNormal3dv ( e_enqire[name[i]].side[21] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 3]].nx );
          glNormal3dv ( e_enqire[name[i]].side[22] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[11]].nx );
          glNormal3dv ( e_enqire[name[i]].side[23] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 0]].nx );
        glEnd();
        glBegin ( GL_TRIANGLE_FAN );
          glNormal3dv ( e_enqire[name[i]].side[24] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[24]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 1]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 8]].nx );
          glNormal3dv ( e_enqire[name[i]].side[25] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 0]].nx );
          glNormal3dv ( e_enqire[name[i]].side[26] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[11]].nx );
          glNormal3dv ( e_enqire[name[i]].side[27] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 3]].nx );
          glNormal3dv ( e_enqire[name[i]].side[28] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[10]].nx );
          glNormal3dv ( e_enqire[name[i]].side[29] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 2]].nx );
          glNormal3dv ( e_enqire[name[i]].side[30] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 9]].nx );
          glNormal3dv ( e_enqire[name[i]].side[31] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 1]].nx );
        glEnd();
        glBegin ( GL_TRIANGLE_FAN );
          glNormal3dv ( e_enqire[name[i]].side[32] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[25]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 4]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[16]].nx );
          glNormal3dv ( e_enqire[name[i]].side[33] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 5]].nx );
          glNormal3dv ( e_enqire[name[i]].side[34] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[17]].nx );
          glNormal3dv ( e_enqire[name[i]].side[35] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 6]].nx );
          glNormal3dv ( e_enqire[name[i]].side[36] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[18]].nx );
          glNormal3dv ( e_enqire[name[i]].side[37] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 7]].nx );
          glNormal3dv ( e_enqire[name[i]].side[38] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[19]].nx );
          glNormal3dv ( e_enqire[name[i]].side[39] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 4]].nx );
        glEnd();
        glBegin ( GL_TRIANGLE_FAN );
          glNormal3dv ( e_enqire[name[i]].side[40] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[20]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 0]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 8]].nx );
          glNormal3dv ( e_enqire[name[i]].side[41] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 1]].nx );
          glNormal3dv ( e_enqire[name[i]].side[42] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[13]].nx );
          glNormal3dv ( e_enqire[name[i]].side[43] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 5]].nx );
          glNormal3dv ( e_enqire[name[i]].side[44] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[16]].nx );
          glNormal3dv ( e_enqire[name[i]].side[45] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 4]].nx );
          glNormal3dv ( e_enqire[name[i]].side[46] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[12]].nx );
          glNormal3dv ( e_enqire[name[i]].side[47] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 0]].nx );
        glEnd();
        break;

        case 5:
        glBegin ( GL_TRIANGLES );
          glNormal3dv ( e_enqire[name[i]].side[0] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 1]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[10]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 6]].nx );
          glNormal3dv ( e_enqire[name[i]].side[1] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 6]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[10]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[15]].nx );
          glNormal3dv ( e_enqire[name[i]].side[2] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 6]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[15]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 0]].nx );
          glNormal3dv ( e_enqire[name[i]].side[3] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 0]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[15]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 9]].nx );
          glNormal3dv ( e_enqire[name[i]].side[4] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[10]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 4]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[15]].nx );
          glNormal3dv ( e_enqire[name[i]].side[5] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[15]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 4]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[12]].nx );
          glNormal3dv ( e_enqire[name[i]].side[6] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[15]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[12]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 9]].nx );
          glNormal3dv ( e_enqire[name[i]].side[7] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 9]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[12]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 3]].nx );
          glNormal3dv ( e_enqire[name[i]].side[8] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 2]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[11]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 7]].nx );
          glNormal3dv ( e_enqire[name[i]].side[9] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 7]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[11]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[16]].nx );
          glNormal3dv ( e_enqire[name[i]].side[10] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 7]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[16]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 1]].nx );
          glNormal3dv ( e_enqire[name[i]].side[11] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 1]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[16]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[10]].nx );
          glNormal3dv ( e_enqire[name[i]].side[12] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[11]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 5]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[16]].nx );
          glNormal3dv ( e_enqire[name[i]].side[13] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[16]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 5]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[13]].nx );
          glNormal3dv ( e_enqire[name[i]].side[14] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[16]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[13]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[10]].nx );
          glNormal3dv ( e_enqire[name[i]].side[15] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[10]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[13]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 4]].nx );
          glNormal3dv ( e_enqire[name[i]].side[16] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 0]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 9]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 8]].nx );
          glNormal3dv ( e_enqire[name[i]].side[17] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 8]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 9]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[17]].nx );
          glNormal3dv ( e_enqire[name[i]].side[18] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 8]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[17]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 2]].nx );
          glNormal3dv ( e_enqire[name[i]].side[19] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 2]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[17]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[11]].nx );
          glNormal3dv ( e_enqire[name[i]].side[20] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 9]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 3]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[17]].nx );
          glNormal3dv ( e_enqire[name[i]].side[21] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[17]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 3]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[14]].nx );
          glNormal3dv ( e_enqire[name[i]].side[22] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[17]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[14]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[11]].nx );
          glNormal3dv ( e_enqire[name[i]].side[23] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[11]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[14]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 5]].nx );
          glNormal3dv ( e_enqire[name[i]].side[24] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 2]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 7]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 8]].nx );
          glNormal3dv ( e_enqire[name[i]].side[25] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 8]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 7]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[18]].nx );
          glNormal3dv ( e_enqire[name[i]].side[26] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 8]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[18]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 0]].nx );
          glNormal3dv ( e_enqire[name[i]].side[27] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 0]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[18]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 0]].nx );
          glNormal3dv ( e_enqire[name[i]].side[28] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 7]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 1]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[18]].nx );
          glNormal3dv ( e_enqire[name[i]].side[29] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[18]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 1]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 6]].nx );
          glNormal3dv ( e_enqire[name[i]].side[30] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[18]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 6]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 0]].nx );
          glNormal3dv ( e_enqire[name[i]].side[31] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 0]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 6]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 0]].nx );
          glNormal3dv ( e_enqire[name[i]].side[32] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 4]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[13]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[12]].nx );
          glNormal3dv ( e_enqire[name[i]].side[33] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[12]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[13]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[19]].nx );
          glNormal3dv ( e_enqire[name[i]].side[34] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[12]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[19]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 3]].nx );
          glNormal3dv ( e_enqire[name[i]].side[35] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 3]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[19]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 3]].nx );
          glNormal3dv ( e_enqire[name[i]].side[36] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[13]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 5]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[19]].nx );
          glNormal3dv ( e_enqire[name[i]].side[37] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[19]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 5]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[14]].nx );
          glNormal3dv ( e_enqire[name[i]].side[38] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[19]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[14]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 3]].nx );
          glNormal3dv ( e_enqire[name[i]].side[39] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 3]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[14]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 3]].nx );
        glEnd();
        break;

        case 6:
        glBegin ( GL_TRIANGLES );
          glNormal3dv ( e_enqire[name[i]].side[0] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 0]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 4]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 7]].nx );
          glNormal3dv ( e_enqire[name[i]].side[1] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 7]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 4]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 8]].nx );
          glNormal3dv ( e_enqire[name[i]].side[2] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 8]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 4]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 1]].nx );
          glNormal3dv ( e_enqire[name[i]].side[3] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 7]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 8]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 3]].nx );
          glNormal3dv ( e_enqire[name[i]].side[4] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 1]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 5]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 8]].nx );
          glNormal3dv ( e_enqire[name[i]].side[5] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 8]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 5]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 9]].nx );
          glNormal3dv ( e_enqire[name[i]].side[6] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 9]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 5]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 2]].nx );
          glNormal3dv ( e_enqire[name[i]].side[7] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 8]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 9]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 3]].nx );
          glNormal3dv ( e_enqire[name[i]].side[8] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 2]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 6]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 9]].nx );
          glNormal3dv ( e_enqire[name[i]].side[9] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 9]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 6]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 7]].nx );
          glNormal3dv ( e_enqire[name[i]].side[10] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 7]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 6]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 0]].nx );
          glNormal3dv ( e_enqire[name[i]].side[11] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 9]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 7]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 3]].nx );
          glNormal3dv ( e_enqire[name[i]].side[12] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 0]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 6]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 4]].nx );
          glNormal3dv ( e_enqire[name[i]].side[13] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 4]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 6]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 5]].nx );
          glNormal3dv ( e_enqire[name[i]].side[14] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 5]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 6]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 2]].nx );
          glNormal3dv ( e_enqire[name[i]].side[15] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 4]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 5]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 1]].nx );
        glEnd();
        break;

        case 7:
        glBegin ( GL_TRIANGLES );
          glNormal3dv ( e_enqire[name[i]].side[0] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
        glEnd();
        break;

        case 8:
        glBegin ( GL_TRIANGLES );
          glNormal3dv ( e_enqire[name[i]].side[0] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
          glNormal3dv ( e_enqire[name[i]].side[1] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
          glNormal3dv ( e_enqire[name[i]].side[2] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
          glNormal3dv ( e_enqire[name[i]].side[3] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
        glEnd();
        break;

        case 9:
        glBegin ( GL_TRIANGLE_STRIP );
          glNormal3dv ( e_enqire[name[i]].side[0] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
        glEnd();
        break;

        case 10:
        glBegin ( GL_TRIANGLE_FAN );
          glNormal3dv ( e_enqire[name[i]].side[0] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 8]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 0]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 4]].nx );
          glNormal3dv ( e_enqire[name[i]].side[1] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 1]].nx );
          glNormal3dv ( e_enqire[name[i]].side[2] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 5]].nx );
          glNormal3dv ( e_enqire[name[i]].side[3] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 2]].nx );
          glNormal3dv ( e_enqire[name[i]].side[4] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 6]].nx );
          glNormal3dv ( e_enqire[name[i]].side[5] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 3]].nx );
          glNormal3dv ( e_enqire[name[i]].side[6] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 7]].nx );
          glNormal3dv ( e_enqire[name[i]].side[7] );
          glVertex3dv ( &node[e_enqire[name[i]].nod[ 0]].nx );
        glEnd();
        break;

        case 11:
        glBegin ( GL_LINES );
          glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
        glEnd();
        break;

        case 12:
        glBegin ( GL_LINE_STRIP );
          glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
          glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
        glEnd();
        break;
      }
  
      if(type=='a')
      {
	switch(e_enqire[name[i]].type)
	{
		case 1:
		case 4:
			j=8;
			break;
		case 2:
		case 5:
			j=6;
			break;
		case 3:
		case 6:
		case 9:
		case 10:
			j=4;
			break;
		case 7:
		case 8:
		case 12:
			j=3;
			break;
		case 11: 
			j=2;
			break;
		default:
			// assure that j!=0 (see division below)
		        printf("ERROR: type:%d of elem:%d not known\n",e_enqire[name[i]].type,name[i]);
			j=1;
			break;
	}
        x=y=z=0.;
        for (n=0; n<j; n++)
        {
          x+= node[e_enqire[name[i]].nod[n]].nx;
          y+= node[e_enqire[name[i]].nod[n]].ny;
          z+= node[e_enqire[name[i]].nod[n]].nz;
        }
        x/= j; y/= j; z/= j;
        sprintf( buffer, "%d ", name[i]);
#if INX_MODE
        glIndexi (offset+CMAP_CELLS_LIGHT);
#endif
#if TEX_MODE
        glColor3f ( entity_r[0], entity_g[0], entity_b[0]  );
#endif 
        glDisable( GL_LIGHTING );
        text( x, y, z, buffer, glut_font[draw_font] );
        glEnable( GL_LIGHTING );
      }
    }
    glDisable( GL_LIGHTING );
  }
}



void drawFaces_plot( int num, int *name, Nodes *node, double *colNr, Faces *face, int col, char type, int pickflag )
{
  register int i, j, n;
  double x, y, z;
  static GLint ipuf[2];
  extern Elements  *e_enqire;

  glLineWidth(1.);
  if((type=='v')&&(!pickflag))
  {
#if TEX_MODE
    /* enable all colors for the TEX_MODE */
    glColor3d( 1,1,1);
    glEnable(GL_TEXTURE_1D);
#endif
    for (i=0; i<num; i++ )
    {
      if(( e_enqire[face[name[i]].elem_nr].type <7 )&&(drawMode<4)) glEnable ( GL_CULL_FACE ); else glDisable ( GL_CULL_FACE );
      switch(face[name[i]].type)
      {
    	case 7:
        glBegin ( GL_TRIANGLES );
          GLCOLOR    ( colNr[face[name[i]].nod[0]] );
          glVertex3dv ( &node[face[name[i]].nod[0]].nx );
          GLCOLOR    ( colNr[face[name[i]].nod[1]] );
          glVertex3dv ( &node[face[name[i]].nod[1]].nx );
          GLCOLOR    ( colNr[face[name[i]].nod[2]] );
          glVertex3dv ( &node[face[name[i]].nod[2]].nx );
        glEnd();
        break;

    	case 8:
        glBegin ( GL_TRIANGLES );
          GLCOLOR     ( colNr[face[name[i]].nod[0]] );
          glVertex3dv  ( &node[face[name[i]].nod[0]].nx );
          GLCOLOR     ( colNr[face[name[i]].nod[3]] );
          glVertex3dv  ( &node[face[name[i]].nod[3]].nx );
          GLCOLOR     ( colNr[face[name[i]].nod[5]] );
          glVertex3dv  ( &node[face[name[i]].nod[5]].nx );
    
          GLCOLOR     ( colNr[face[name[i]].nod[2]] );
          glVertex3dv  ( &node[face[name[i]].nod[2]].nx );
          GLCOLOR     ( colNr[face[name[i]].nod[5]] );
          glVertex3dv  ( &node[face[name[i]].nod[5]].nx );
          GLCOLOR     ( colNr[face[name[i]].nod[4]] );
          glVertex3dv  ( &node[face[name[i]].nod[4]].nx );
    
          GLCOLOR     ( colNr[face[name[i]].nod[4]] );
          glVertex3dv  ( &node[face[name[i]].nod[4]].nx );
          GLCOLOR     ( colNr[face[name[i]].nod[5]] );
          glVertex3dv  ( &node[face[name[i]].nod[5]].nx );
          GLCOLOR     ( colNr[face[name[i]].nod[3]] );
          glVertex3dv  ( &node[face[name[i]].nod[3]].nx );
    
          GLCOLOR     ( colNr[face[name[i]].nod[3]] );
          glVertex3dv  ( &node[face[name[i]].nod[3]].nx );
          GLCOLOR     ( colNr[face[name[i]].nod[1]] );
          glVertex3dv  ( &node[face[name[i]].nod[1]].nx );
          GLCOLOR     ( colNr[face[name[i]].nod[4]] );
          glVertex3dv  ( &node[face[name[i]].nod[4]].nx );
        glEnd();
        break;

    	case 9:
        glBegin ( GL_TRIANGLE_STRIP );
          GLCOLOR    ( colNr[face[name[i]].nod[0]] );
          glVertex3dv ( &node[face[name[i]].nod[0]].nx );
          GLCOLOR    ( colNr[face[name[i]].nod[1]] );
          glVertex3dv ( &node[face[name[i]].nod[1]].nx );
          GLCOLOR    ( colNr[face[name[i]].nod[3]] );
          glVertex3dv ( &node[face[name[i]].nod[3]].nx );
          GLCOLOR    ( colNr[face[name[i]].nod[2]] );
          glVertex3dv ( &node[face[name[i]].nod[2]].nx );
        glEnd();
        break;

    	case 10:
        glBegin ( GL_TRIANGLE_FAN );
          GLCOLOR    ( colNr[face[name[i]].nod[8]] );
          glVertex3dv ( &node[face[name[i]].nod[8]].nx );
          GLCOLOR    ( colNr[face[name[i]].nod[0]] );
          glVertex3dv ( &node[face[name[i]].nod[0]].nx );
          GLCOLOR    ( colNr[face[name[i]].nod[4]] );
          glVertex3dv ( &node[face[name[i]].nod[4]].nx );
          GLCOLOR    ( colNr[face[name[i]].nod[1]] );
          glVertex3dv ( &node[face[name[i]].nod[1]].nx );
          GLCOLOR    ( colNr[face[name[i]].nod[5]] );
          glVertex3dv ( &node[face[name[i]].nod[5]].nx );
          GLCOLOR    ( colNr[face[name[i]].nod[2]] );
          glVertex3dv ( &node[face[name[i]].nod[2]].nx );
          GLCOLOR    ( colNr[face[name[i]].nod[6]] );
          glVertex3dv ( &node[face[name[i]].nod[6]].nx );
          GLCOLOR    ( colNr[face[name[i]].nod[3]] );
          glVertex3dv ( &node[face[name[i]].nod[3]].nx );
          GLCOLOR    ( colNr[face[name[i]].nod[7]] );
          glVertex3dv ( &node[face[name[i]].nod[7]].nx );
          GLCOLOR    ( colNr[face[name[i]].nod[0]] );
          glVertex3dv ( &node[face[name[i]].nod[0]].nx );
        glEnd();
        break;

    	case 11:
        glBegin ( GL_LINES );
          GLCOLOR    ( colNr[face[name[i]].nod[0]] );
          glVertex3dv ( &node[face[name[i]].nod[0]].nx );
          GLCOLOR    ( colNr[face[name[i]].nod[1]] );
          glVertex3dv ( &node[face[name[i]].nod[1]].nx );
        glEnd();
        break;

    	case 12:
        glBegin ( GL_LINE_STRIP );
          GLCOLOR    ( colNr[face[name[i]].nod[0]] );
          glVertex3dv ( &node[face[name[i]].nod[0]].nx );
          GLCOLOR    ( colNr[face[name[i]].nod[2]] );
          glVertex3dv ( &node[face[name[i]].nod[2]].nx );
          GLCOLOR    ( colNr[face[name[i]].nod[1]] );
          glVertex3dv ( &node[face[name[i]].nod[1]].nx );
        glEnd();
        break;
      }
    }
#if TEX_MODE
    glDisable(GL_TEXTURE_1D);
#endif
  }
  else if(pickflag)
  {
    /* any other color */
#if INX_MODE
    glIndexi (col+offset+CMAP_CELLS_LIGHT);
#endif
#if TEX_MODE
    glColor3f ( entity_r[col], entity_g[col], entity_b[col]  );
#endif
    if(type=='e') glLoadName('e'); else glLoadName('f');

    glGetIntegerv( GL_POLYGON_MODE, ipuf );

    for (i=0; i<num; i++ )
    {
      if(( e_enqire[face[name[i]].elem_nr].type <7 )&&(drawMode<4)) glEnable ( GL_CULL_FACE ); else glDisable ( GL_CULL_FACE );

      if(type=='e') glPushName(face[name[i]].elem_nr); else glPushName(name[i]);
      switch(face[name[i]].type)
      {
    	case 7:
        glBegin ( GL_TRIANGLES );
         glVertex3dv ( &node[face[name[i]].nod[0]].nx );
         glVertex3dv ( &node[face[name[i]].nod[1]].nx );
         glVertex3dv ( &node[face[name[i]].nod[2]].nx );
        glEnd();
        break;

    	case 8:
        glBegin ( GL_TRIANGLES );
         glVertex3dv ( &node[face[name[i]].nod[0]].nx );
         glVertex3dv ( &node[face[name[i]].nod[3]].nx );
         glVertex3dv ( &node[face[name[i]].nod[5]].nx );
         glVertex3dv ( &node[face[name[i]].nod[2]].nx );
         glVertex3dv ( &node[face[name[i]].nod[5]].nx );
         glVertex3dv ( &node[face[name[i]].nod[4]].nx );
         glVertex3dv ( &node[face[name[i]].nod[4]].nx );
         glVertex3dv ( &node[face[name[i]].nod[5]].nx );
         glVertex3dv ( &node[face[name[i]].nod[3]].nx );
         glVertex3dv ( &node[face[name[i]].nod[3]].nx );
         glVertex3dv ( &node[face[name[i]].nod[1]].nx );
         glVertex3dv ( &node[face[name[i]].nod[4]].nx );
        glEnd();
        break;

    	case 9:
        glBegin ( GL_TRIANGLES );
         glVertex3dv ( &node[face[name[i]].nod[0]].nx );
         glVertex3dv ( &node[face[name[i]].nod[1]].nx );
         glVertex3dv ( &node[face[name[i]].nod[3]].nx );  
         glVertex3dv ( &node[face[name[i]].nod[2]].nx );
         glVertex3dv ( &node[face[name[i]].nod[3]].nx );
         glVertex3dv ( &node[face[name[i]].nod[1]].nx );
        glEnd();
        break;

    	case 10:
        glBegin ( GL_TRIANGLE_FAN);
         glVertex3dv ( &node[face[name[i]].nod[8]].nx );
         glVertex3dv ( &node[face[name[i]].nod[0]].nx );  
         glVertex3dv ( &node[face[name[i]].nod[4]].nx );
         glVertex3dv ( &node[face[name[i]].nod[1]].nx );
         glVertex3dv ( &node[face[name[i]].nod[5]].nx );
         glVertex3dv ( &node[face[name[i]].nod[2]].nx );
         glVertex3dv ( &node[face[name[i]].nod[6]].nx );
         glVertex3dv ( &node[face[name[i]].nod[3]].nx );
         glVertex3dv ( &node[face[name[i]].nod[7]].nx );
         glVertex3dv ( &node[face[name[i]].nod[0]].nx );
        glEnd();
        break;

    	case 11:
        glBegin ( GL_LINES );
         glVertex3dv ( &node[face[name[i]].nod[0]].nx );
         glVertex3dv ( &node[face[name[i]].nod[1]].nx );
        glEnd();
        break;

    	case 12:
        glBegin ( GL_LINE_STRIP );
         glVertex3dv ( &node[face[name[i]].nod[0]].nx );
         glVertex3dv ( &node[face[name[i]].nod[2]].nx );
         glVertex3dv ( &node[face[name[i]].nod[1]].nx );
        glEnd();
        break;
      }
  
      if(type=='a')
      {
        j=0;
        switch(face[name[i]].type)
	{
	case 7:
	j=3;  break;
	case 8:
	j=3;  break;
	case 9:
	j=4;  break;
	case 10:
	j=4;  break;
	case 11:
	j=2;  break;
	case 12:
	j=2;  break;
	}
        x=y=z=0.;
        for (n=0; n<j; n++)
        {
          x+= node[face[name[i]].nod[n]].nx;
          y+= node[face[name[i]].nod[n]].ny;
          z+= node[face[name[i]].nod[n]].nz;
        }
        x/= j; y/= j; z/= j;
        sprintf( buffer, "%d-S%d", face[name[i]].elem_nr, face[name[i]].nr+1);
        text( x, y, z, buffer, glut_font[draw_font] );
      }
      glPopName();
    }
    glLoadName(-1);
  }
  else
  {
    setLightAndMaterial_rgb(col);
    glEnable( GL_LIGHTING );

        /* Achtung! glNormal3dv nur VOR den ersten nodes fuert (nur hier im face-code!) auf mtu-linux zum haengen */
        /* Die normale des ersten Dreiecks muss vor- und im block spez. werden damit alle modi laufen */
	/*
       glBegin ( GL_TRIANGLE_FAN   );
         <glNormal3dv ( face[name[i]].side[0] );>
         glVertex3dv ( &node[face[name[i]].nod[8]].nx );
         etc.
	*/

    for (i=0; i<num; i++ )
    {
      if(( e_enqire[face[name[i]].elem_nr].type <7 )&&(drawMode<4)) glEnable ( GL_CULL_FACE ); else glDisable ( GL_CULL_FACE );
      switch(face[name[i]].type)
      {
    	case 7:
        glNormal3dv ( face[name[i]].side[0] );
        glBegin ( GL_TRIANGLES );
         glVertex3dv ( &node[face[name[i]].nod[0]].nx );
         glVertex3dv ( &node[face[name[i]].nod[1]].nx );
         glNormal3dv ( face[name[i]].side[0] );
         glVertex3dv ( &node[face[name[i]].nod[2]].nx );
        glEnd();
        break;

    	case 8:
        glNormal3dv ( face[name[i]].side[0] );
        glBegin ( GL_TRIANGLES );
         glVertex3dv ( &node[face[name[i]].nod[0]].nx );
         glVertex3dv ( &node[face[name[i]].nod[3]].nx );
         glNormal3dv ( face[name[i]].side[0] );
         glVertex3dv ( &node[face[name[i]].nod[5]].nx );
         glVertex3dv ( &node[face[name[i]].nod[2]].nx );
         glVertex3dv ( &node[face[name[i]].nod[5]].nx );
         glNormal3dv ( face[name[i]].side[1] );
         glVertex3dv ( &node[face[name[i]].nod[4]].nx );
         glVertex3dv ( &node[face[name[i]].nod[4]].nx );
         glVertex3dv ( &node[face[name[i]].nod[5]].nx );
         glNormal3dv ( face[name[i]].side[2] );
         glVertex3dv ( &node[face[name[i]].nod[3]].nx );
         glVertex3dv ( &node[face[name[i]].nod[3]].nx );
         glVertex3dv ( &node[face[name[i]].nod[1]].nx );
         glNormal3dv ( face[name[i]].side[3] );
         glVertex3dv ( &node[face[name[i]].nod[4]].nx );
        glEnd();
        break;

    	case 9:
        glNormal3dv ( face[name[i]].side[0] );
        glBegin ( GL_TRIANGLE_STRIP );
         glVertex3dv ( &node[face[name[i]].nod[0]].nx );
         glVertex3dv ( &node[face[name[i]].nod[1]].nx );
         glNormal3dv ( face[name[i]].side[0] );
         glVertex3dv ( &node[face[name[i]].nod[3]].nx );
         glVertex3dv ( &node[face[name[i]].nod[2]].nx );
        glEnd();
        break;

    	case 10:
        glNormal3dv ( face[name[i]].side[0] );
        glBegin ( GL_TRIANGLE_FAN   );
         glVertex3dv ( &node[face[name[i]].nod[8]].nx );
         glVertex3dv ( &node[face[name[i]].nod[0]].nx );
         glNormal3dv ( face[name[i]].side[0] );
         glVertex3dv ( &node[face[name[i]].nod[4]].nx );
         glNormal3dv ( face[name[i]].side[1] );
         glVertex3dv ( &node[face[name[i]].nod[1]].nx );
         glNormal3dv ( face[name[i]].side[2] );
         glVertex3dv ( &node[face[name[i]].nod[5]].nx );
         glNormal3dv ( face[name[i]].side[3] );
         glVertex3dv ( &node[face[name[i]].nod[2]].nx );
         glNormal3dv ( face[name[i]].side[4] );
         glVertex3dv ( &node[face[name[i]].nod[6]].nx );
         glNormal3dv ( face[name[i]].side[5] );
         glVertex3dv ( &node[face[name[i]].nod[3]].nx );
         glNormal3dv ( face[name[i]].side[6] );
         glVertex3dv ( &node[face[name[i]].nod[7]].nx );
         glNormal3dv ( face[name[i]].side[7] );
         glVertex3dv ( &node[face[name[i]].nod[0]].nx );
         glEnd();
        break;

    	case 11:
        glBegin ( GL_LINES );
         glVertex3dv ( &node[face[name[i]].nod[0]].nx );
         glVertex3dv ( &node[face[name[i]].nod[1]].nx );
        glEnd();
        break;

    	case 12:
        glBegin ( GL_LINE_STRIP );
         glVertex3dv ( &node[face[name[i]].nod[0]].nx );
         glVertex3dv ( &node[face[name[i]].nod[2]].nx );
         glVertex3dv ( &node[face[name[i]].nod[1]].nx );
        glEnd();
        break;
      }
  
      if(type=='a')
      {
        j=0;
        switch(face[name[i]].type)
	{
	case 7:
	j=3;  break;
	case 8:
	j=3;  break;
	case 9:
	j=4;  break;
	case 10:
	j=4;  break;
	case 11:
	j=2;  break;
	case 12:
	j=2;  break;
	}
        x=y=z=0.;
        for (n=0; n<j; n++)
        {
          x+= node[face[name[i]].nod[n]].nx;
          y+= node[face[name[i]].nod[n]].ny;
          z+= node[face[name[i]].nod[n]].nz;
        }
        x/= j; y/= j; z/= j;
        if(e_enqire[face[name[i]].elem_nr].type>6)  // shell
	{
          if(e_enqire[face[name[i]].elem_nr].type>10)  // beam
            sprintf( buffer, "%d-S?", face[name[i]].elem_nr);
	  else if(e_enqire[face[name[i]].elem_nr].attr>3)  // plain strain,stress or axisym
	  {
            //if(face[name[i]].nr==0) sprintf( buffer, "%d-SN", face[name[i]].elem_nr);
            //else if(face[name[i]].nr==1) sprintf( buffer, "%d-SP", face[name[i]].elem_nr);
            if(face[name[i]].nr==1) sprintf( buffer, "%d-SNP", face[name[i]].elem_nr);
            else sprintf( buffer, "%d-S%d", face[name[i]].elem_nr, face[name[i]].nr-1);
	  }
	  else
	  {
            //if(face[name[i]].nr==0) sprintf( buffer, "%d-SNEG", face[name[i]].elem_nr);
            //else if(face[name[i]].nr==1) sprintf( buffer, "%d-SPOS", face[name[i]].elem_nr);
            if(face[name[i]].nr==1) sprintf( buffer, "%d-SNP", face[name[i]].elem_nr);
            else sprintf( buffer, "%d-S%d", face[name[i]].elem_nr, face[name[i]].nr+1);
	  }
	}
        else sprintf( buffer, "%d-S%d", face[name[i]].elem_nr, face[name[i]].nr+1);
#if INX_MODE
        glIndexi (offset+CMAP_CELLS_LIGHT);
#endif
#if TEX_MODE
        glColor3f ( entity_r[0], entity_g[0], entity_b[0]  );
#endif 
        glDisable( GL_LIGHTING );
        text( x, y, z, buffer, glut_font[draw_font] );
        glEnable( GL_LIGHTING );
      }
    }
    glDisable( GL_LIGHTING );
  }
  glEnable ( GL_CULL_FACE );
}




void drawFaces_edge( int num, int *name, Nodes *node, Faces *face, int color, char type )
{
  register int i;
  static GLint ipuf[2];

  /* change the offset of the edges to the backside of the elements if */
  /* the interiour is visible */
  glGetIntegerv( GL_CULL_FACE_MODE, ipuf );
  if ( ipuf[0] == GL_FRONT ) ds*=-1;

#if INX_MODE
  glIndexi    ( color );
#endif
#if TEX_MODE
  glColor3d(color,color,color);
#endif
  glLineWidth(1.);

  for (i=0; i<num; i++ )
  {
    switch(face[name[i]].type)
    {
      case 7:
      glBegin ( GL_LINE_LOOP );
      glVertex3d ( node[face[name[i]].nod[0]].nx+face[name[i]].side[0][0]*1.e-3*ds, 
                   node[face[name[i]].nod[0]].ny+face[name[i]].side[0][1]*1.e-3*ds,
                   node[face[name[i]].nod[0]].nz+face[name[i]].side[0][2]*1.e-3*ds );
      glVertex3d ( node[face[name[i]].nod[1]].nx+face[name[i]].side[0][0]*1.e-3*ds, 
                   node[face[name[i]].nod[1]].ny+face[name[i]].side[0][1]*1.e-3*ds,
                   node[face[name[i]].nod[1]].nz+face[name[i]].side[0][2]*1.e-3*ds );
      glVertex3d ( node[face[name[i]].nod[2]].nx+face[name[i]].side[0][0]*1.e-3*ds, 
                   node[face[name[i]].nod[2]].ny+face[name[i]].side[0][1]*1.e-3*ds,
                   node[face[name[i]].nod[2]].nz+face[name[i]].side[0][2]*1.e-3*ds );
      glEnd();
      break;

      case 8:
      glBegin ( GL_LINE_LOOP );
      glVertex3d ( node[face[name[i]].nod[0]].nx+face[name[i]].side[0][0]*1.e-3*ds, 
                   node[face[name[i]].nod[0]].ny+face[name[i]].side[0][1]*1.e-3*ds,
                   node[face[name[i]].nod[0]].nz+face[name[i]].side[0][2]*1.e-3*ds );
      glVertex3d ( node[face[name[i]].nod[3]].nx+face[name[i]].side[2][0]*1.e-3*ds, 
                   node[face[name[i]].nod[3]].ny+face[name[i]].side[2][1]*1.e-3*ds,
                   node[face[name[i]].nod[3]].nz+face[name[i]].side[2][2]*1.e-3*ds );
      glVertex3d ( node[face[name[i]].nod[1]].nx+face[name[i]].side[3][0]*1.e-3*ds, 
                   node[face[name[i]].nod[1]].ny+face[name[i]].side[3][1]*1.e-3*ds,
                   node[face[name[i]].nod[1]].nz+face[name[i]].side[3][2]*1.e-3*ds );
      glVertex3d ( node[face[name[i]].nod[4]].nx+face[name[i]].side[2][0]*1.e-3*ds, 
                   node[face[name[i]].nod[4]].ny+face[name[i]].side[2][1]*1.e-3*ds,
                   node[face[name[i]].nod[4]].nz+face[name[i]].side[2][2]*1.e-3*ds );
      glVertex3d ( node[face[name[i]].nod[2]].nx+face[name[i]].side[1][0]*1.e-3*ds, 
                   node[face[name[i]].nod[2]].ny+face[name[i]].side[1][1]*1.e-3*ds,
                   node[face[name[i]].nod[2]].nz+face[name[i]].side[1][2]*1.e-3*ds );
      glVertex3d ( node[face[name[i]].nod[5]].nx+face[name[i]].side[2][0]*1.e-3*ds, 
                   node[face[name[i]].nod[5]].ny+face[name[i]].side[2][1]*1.e-3*ds,
                   node[face[name[i]].nod[5]].nz+face[name[i]].side[2][2]*1.e-3*ds );
      glEnd();
      break;

      case 9:
      glBegin ( GL_LINE_LOOP );
      glVertex3d ( node[face[name[i]].nod[0]].nx+face[name[i]].side[0][0]*1.e-3*ds, 
                   node[face[name[i]].nod[0]].ny+face[name[i]].side[0][1]*1.e-3*ds,
                   node[face[name[i]].nod[0]].nz+face[name[i]].side[0][2]*1.e-3*ds );
      glVertex3d ( node[face[name[i]].nod[1]].nx+face[name[i]].side[0][0]*1.e-3*ds, 
                   node[face[name[i]].nod[1]].ny+face[name[i]].side[0][1]*1.e-3*ds,
                   node[face[name[i]].nod[1]].nz+face[name[i]].side[0][2]*1.e-3*ds );
      glVertex3d ( node[face[name[i]].nod[2]].nx+face[name[i]].side[0][0]*1.e-3*ds, 
                   node[face[name[i]].nod[2]].ny+face[name[i]].side[0][1]*1.e-3*ds,
                   node[face[name[i]].nod[2]].nz+face[name[i]].side[0][2]*1.e-3*ds );
      glVertex3d ( node[face[name[i]].nod[3]].nx+face[name[i]].side[0][0]*1.e-3*ds, 
                   node[face[name[i]].nod[3]].ny+face[name[i]].side[0][1]*1.e-3*ds,
                   node[face[name[i]].nod[3]].nz+face[name[i]].side[0][2]*1.e-3*ds );
      glEnd();	
      break;

      case 10:
      glBegin ( GL_LINE_LOOP );
      glVertex3d ( node[face[name[i]].nod[0]].nx+face[name[i]].side[0][0]*1.e-3*ds, 
                   node[face[name[i]].nod[0]].ny+face[name[i]].side[0][1]*1.e-3*ds,
                   node[face[name[i]].nod[0]].nz+face[name[i]].side[0][2]*1.e-3*ds );
      glVertex3d ( node[face[name[i]].nod[4]].nx+face[name[i]].side[1][0]*1.e-3*ds, 
                   node[face[name[i]].nod[4]].ny+face[name[i]].side[1][1]*1.e-3*ds,
                   node[face[name[i]].nod[4]].nz+face[name[i]].side[1][2]*1.e-3*ds );
      glVertex3d ( node[face[name[i]].nod[1]].nx+face[name[i]].side[5][0]*1.e-3*ds, 
                   node[face[name[i]].nod[1]].ny+face[name[i]].side[5][1]*1.e-3*ds,
                   node[face[name[i]].nod[1]].nz+face[name[i]].side[5][2]*1.e-3*ds );
      glVertex3d ( node[face[name[i]].nod[5]].nx+face[name[i]].side[4][0]*1.e-3*ds, 
                   node[face[name[i]].nod[5]].ny+face[name[i]].side[4][1]*1.e-3*ds,
                   node[face[name[i]].nod[5]].nz+face[name[i]].side[4][2]*1.e-3*ds );
      glVertex3d ( node[face[name[i]].nod[2]].nx+face[name[i]].side[3][0]*1.e-3*ds, 
                   node[face[name[i]].nod[2]].ny+face[name[i]].side[3][1]*1.e-3*ds,
                   node[face[name[i]].nod[2]].nz+face[name[i]].side[3][2]*1.e-3*ds );
      glVertex3d ( node[face[name[i]].nod[6]].nx+face[name[i]].side[4][0]*1.e-3*ds, 
                   node[face[name[i]].nod[6]].ny+face[name[i]].side[4][1]*1.e-3*ds,
                   node[face[name[i]].nod[6]].nz+face[name[i]].side[4][2]*1.e-3*ds );
      glVertex3d ( node[face[name[i]].nod[3]].nx+face[name[i]].side[2][0]*1.e-3*ds, 
                   node[face[name[i]].nod[3]].ny+face[name[i]].side[2][1]*1.e-3*ds,
                   node[face[name[i]].nod[3]].nz+face[name[i]].side[2][2]*1.e-3*ds );
      glVertex3d ( node[face[name[i]].nod[7]].nx+face[name[i]].side[1][0]*1.e-3*ds, 
                   node[face[name[i]].nod[7]].ny+face[name[i]].side[1][1]*1.e-3*ds,
                   node[face[name[i]].nod[7]].nz+face[name[i]].side[1][2]*1.e-3*ds );
      glEnd();
      break;
    }
  }

  glGetIntegerv( GL_CULL_FACE_MODE, ipuf );
  if ( ipuf[0] == GL_FRONT ) ds*=-1;
}




void drawElem_edge( int num, int *name, Nodes *node, Elements *e_enqire, int color, char type )
{
  register int i;

#if INX_MODE
  glIndexi    ( color );
#endif
#if TEX_MODE
  glColor3d(color,color,color);
#endif
  glLineWidth(1.);

#if TEST
  printf ("in drawElemEdges\n");
#endif 

  for (i=0; i<num; i++ )
  {
    switch(e_enqire[name[i]].type)
    {
      case 1:
      glBegin ( GL_LINE_LOOP );
      glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
      glEnd();
      glBegin ( GL_LINE_LOOP );
      glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[6]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[7]].nx );
      glEnd();
      glBegin ( GL_LINES );
      glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[6]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[7]].nx );
      glEnd();
      break;

      case 2:
      glBegin ( GL_LINE_LOOP );
      glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
      glEnd();
      glBegin ( GL_LINE_LOOP );
      glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
      glEnd();
      glBegin ( GL_LINES );
      glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
      glEnd();
      break;

      case 3:
      glBegin ( GL_LINE_STRIP);
      glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
      glEnd();
      glBegin ( GL_LINE_STRIP );
      glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
      glEnd();
      break;

      case 4:
      glBegin ( GL_LINE_LOOP );
      glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[11]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[10]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[9]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[8]].nx );
      glEnd();
      glBegin ( GL_LINE_LOOP );
      glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[16]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[17]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[6]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[18]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[7]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[19]].nx );
      glEnd();
      glBegin ( GL_LINES );
      glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[12]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[12]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[13]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[13]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[14]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[14]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[6]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[15]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[15]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[7]].nx );
      glEnd();
      break;

      case 5:
      glBegin ( GL_LINE_LOOP );
      glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[ 8]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[ 7]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[6]].nx );
      glEnd();
      glBegin ( GL_LINE_LOOP );
      glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[14]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[13]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[12]].nx );
      glEnd();
      glBegin ( GL_LINES );
      glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[ 9]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[ 9]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[ 1]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[10]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[10]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[ 4]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[11]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[11]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
      glEnd();
      break;

      case 6:
      glBegin ( GL_LINE_STRIP);
      glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[7]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[8]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[9]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
      glEnd();
      glBegin ( GL_LINE_STRIP );
      glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[6]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
      glEnd();
      break;

      case 7:
      glBegin ( GL_LINE_LOOP );
      glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
      glEnd();
      break;

      case 8:
      glBegin ( GL_LINE_LOOP );
      glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
      glEnd();
      break;

      case 9:
      glBegin ( GL_LINE_LOOP );
      glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
      glEnd();
      break;

      case 10:
      glBegin ( GL_LINE_LOOP );
      glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[4]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[5]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[6]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[3]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[7]].nx );
      glEnd();
      break;

      case 11:
      glBegin ( GL_LINES );
      glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
      glEnd();
      break;

      case 12:
      glBegin ( GL_LINE_STRIP );
      glVertex3dv ( &node[e_enqire[name[i]].nod[0]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[2]].nx );
      glVertex3dv ( &node[e_enqire[name[i]].nod[1]].nx );
      glEnd();
      break;
    }
  }
}



void drawElemNodes_plot( int num, int *name, Nodes *node, Elements *e_enqire, int col, char type )
{
  register int i, n;
  int  sum=0;
  int *nodnr;

  if ( (nodnr = (int *)malloc( (int)(anz->nmax+1) * sizeof(int))) == NULL )
  {  printf(" ERROR: malloc in drawElemNodes_plot(), nodnr\n\n") ; return;}
  for (i=0; i<=anz->nmax; i++ ) nodnr[i]=0;

  for (i=0; i<num; i++ )
  {
    switch(e_enqire[name[i]].type)
    {
    	case 1:
		sum=8;
		break;
    	case 2:
		sum=6;
		break;
    	case 3:
		sum=4;
		break;
    	case 4:
		sum=20;
		break;
    	case 5:
		sum=20;
		break;
    	case 6:
		sum=10;
		break;
    	case 7:
		sum=3;
		break;
    	case 8:
		sum=6;
		break;
    	case 9:
		sum=4;
		break;
    	case 10:
		sum=8;
		break;
    	case 11:
		sum=2;
		break;
    	case 12:
		sum=3;
		break;
    	default:
		sum=0;
		break;
    }
    for (n=0; n<sum; n++) nodnr[e_enqire[name[i]].nod[n]]=e_enqire[name[i]].nod[n];
  }

  /* draw all selected nodes */
#if INX_MODE
  glIndexi (2);
#endif
#if TEX_MODE
  glColor3d  ( 0.5,0.5,0. );
#endif
  glLoadName('n');
  for (i=0; i<=anz->nmax; i++ )
  {
    if(nodnr[i])
    {
      glPushName(nodnr[i]);
      glBegin ( GL_POINTS );
      glVertex3dv ( &node[nodnr[i]].nx );
      glEnd();
      glPopName();
    }
  }
  glLoadName(-1);
  free(nodnr);
}



void drawFaceNodes_plot( int num, int *name, Nodes *node, Faces *face, int col, char type )
{
  register int i, n;
  int  sum=0;
  int *nodnr;

  if ( (nodnr = (int *)malloc( (int)(anz->nmax+1) * sizeof(int))) == NULL )
  {  printf(" ERROR: malloc in drawSurfNodes_plot(), nodnr\n\n") ; return;}
  for (i=0; i<=anz->nmax; i++ ) nodnr[i]=0;

  for (i=0; i<num; i++ )
  {
    switch(face[name[i]].type)
    {
    	case 7:
		sum=3;
		break;
    	case 8:
		sum=6;
		break;
    	case 9:
		sum=4;
		break;
    	case 10:
		sum=8;
		break;
    	case 11:
		sum=1;
		break;
    	case 12:
		sum=3;
		break;
    	default:
		sum=0;
		break;
    }
    for (n=0; n<sum; n++) nodnr[face[name[i]].nod[n]]=face[name[i]].nod[n];
  }

  /* draw all selected nodes */
#if INX_MODE
  glIndexi (2);
#endif
#if TEX_MODE
  glColor3d  ( 0.5,0.5,0. );
#endif
  glLoadName('n');
  for (i=0; i<=anz->nmax; i++ )
  {
    if(nodnr[i])
    {
      glPushName(nodnr[i]);
      glBegin ( GL_POINTS );
      glVertex3dv ( &node[nodnr[i]].nx );
      glEnd();
      glPopName();
    }
  }
  glLoadName(-1);
  free(nodnr);
}


void drawPoints_plot( int num, int *name, Points *point , int col, char type)
{
  register int i;

  glPointSize (4);

#if INX_MODE
    glIndexi (col+offset+CMAP_CELLS_LIGHT);
#endif
#if TEX_MODE
    glColor3f ( entity_r[col], entity_g[col], entity_b[col]  );
#endif 
    glLoadName('p');
      for (i=0; i<num; i++ )
      {
      glPushName(name[i]);
       glBegin ( GL_POINTS );
       glVertex3d ( point[name[i]].px, point[name[i]].py, point[name[i]].pz );
       glEnd();
      glPopName();
      }
    glLoadName(-1);

  if(type=='a')
  {
    for (i=0; i<num; i++ )
      text( point[name[i]].px, point[name[i]].py, point[name[i]].pz, point[name[i]].name, glut_font[draw_font] );
  }
}


void drawLines_plot( int num, int *name, Lines *line , Points *point, int col, char type)
{
  register int i,n;
  int bias_fbd;
  double x,y,z;

  int anz_p=0;
  int *pnt_indx=NULL;

  glLineWidth(1.);
#if INX_MODE
  glIndexi (col+offset+CMAP_CELLS_LIGHT);
#endif
#if TEX_MODE
  glColor3f ( entity_r[col], entity_g[col], entity_b[col]  );
#endif 
  glLoadName('l');
  for (i=0; i<num; i++ )
  {
    glPushName(name[i]);
    glBegin ( GL_LINE_STRIP );
      for (n=0; n<line[name[i]].nip; n+=3)
      {
        glVertex3dv( &line[name[i]].ip[n]);
      }
    glEnd();
    if (type=='p')
    {
      if ( (pnt_indx = (int *)realloc( pnt_indx, (anz_p+2) * sizeof(int)))== NULL )
         printf("\n\n ERROR: realloc failure in drawLines_plot\n\n" );
      pnt_indx[anz_p++]=line[name[i]].p1;
      pnt_indx[anz_p++]=line[name[i]].p2;
    }
    else if ((type=='a')||(type=='d'))
    {
      x =point[line[name[i]].p1].px;
      y =point[line[name[i]].p1].py;
      z =point[line[name[i]].p1].pz;
      x+=point[line[name[i]].p2].px;
      y+=point[line[name[i]].p2].py;
      z+=point[line[name[i]].p2].pz;
      x*=.5;
      y*=.5;
      z*=.5;
      if (type=='a') sprintf( buffer, "%s", line[name[i]].name);
      if (type=='d')
      {
        /* load bias and division in buffer */
        if(line[name[i]].div ==1) sprintf(buffer, "%d", line[name[i]].div);
        else if(line[name[i]].bias!=1.)
	{
          bias_fbd=getBias_fbd(name[i],line);
          if(line[name[i]].div<10) sprintf(buffer, "%d0%d", bias_fbd, line[name[i]].div);
          //else if(line[name[i]].div<100) sprintf(buffer, "%d%d", bias_fbd, line[name[i]].div);
          //else
            sprintf(buffer, "%d#%d", bias_fbd, line[name[i]].div);
	}
        else sprintf(buffer, "%d", line[name[i]].div);
      }
      text( x, y, z, buffer, glut_font[draw_font] );
    }
    glPopName();
    }
  glLoadName(-1);

  if(anz_p)  drawPoints_plot( anz_p, pnt_indx, point, col, 0 );
}


void drawShapes_plot( int num, int *name, Shapes *shape, Points *point, int col, char type)
{
  register int i,j,k,n;
  static GLint ipuf[2];
  double cx,cy,cz;

  glLineWidth(1.);

  glLoadName('h');

  if(type=='i')
  {
  /* so far not usable, *pgn not filled */
    glGetIntegerv( GL_CULL_FACE, ipuf );
    if( ipuf[0] == GL_TRUE ) glDisable ( GL_CULL_FACE );
    glShadeModel ( GL_FLAT );
    glEnable( GL_LIGHTING );

    for (i=0; i<num; i++)
    {
      glPushName(name[i]);
      n=0;
      while((shape[name[i]].npgn-n))
      {
        n++; /* jump over the polygon token (ie.GL_POLYGON_TOKEN) */
        j=shape[name[i]].pgn[n++];
        glBegin ( GL_POLYGON );
        glNormal3dv (&shape[name[i]].pgn[n]); n+=3;
        for(k=0; k<j; k++)
        {
          glVertex3dv (&shape[name[i]].pgn[n]); n+=3; 
        }
        glEnd();
      }
      sprintf( buffer, "%s", shape[name[i]].name );
      cx=cy=cz=0.;
      for(j=0; j<3; j++)
      {
        cx+=point[shape[name[i]].p[j]].px;
        cy+=point[shape[name[i]].p[j]].py;
        cz+=point[shape[name[i]].p[j]].pz;
      }
      cx/=3.;
      cy/=3.;
      cz/=3.;
      text( cx,cy,cz, buffer, glut_font[draw_font] );
      glPopName();
    }
    glDisable( GL_LIGHTING );
    if( ipuf[0] == GL_TRUE ) glEnable ( GL_CULL_FACE );
  }
  else
  {

#if INX_MODE
    glIndexi (col+offset+CMAP_CELLS_LIGHT);
#endif
#if TEX_MODE
    glColor3f ( entity_r[col], entity_g[col], entity_b[col]  );
#endif 
    for (i=0; i<num; i++)
    {
      if(shape[name[i]].type==0)
      {
        glPushName(name[i]);
        glBegin ( GL_LINE_LOOP );
        glVertex3dv ( &point[shape[name[i]].p[0]].px);
        glVertex3dv ( &point[shape[name[i]].p[1]].px);
        glVertex3dv ( &point[shape[name[i]].p[2]].px);
        glEnd();
        sprintf( buffer, "%s", shape[name[i]].name );
        cx=cy=cz=0.;
        for(j=0; j<3; j++)
        {
          cx+=point[shape[name[i]].p[j]].px;
          cy+=point[shape[name[i]].p[j]].py;
          cz+=point[shape[name[i]].p[j]].pz;
        }
        cx/=3.;
        cy/=3.;
        cz/=3.;
        text( cx,cy,cz, buffer, glut_font[draw_font] );
        glPopName();
      }
      else if(shape[name[i]].type==1)
      {
        glPushName(name[i]);
        glBegin ( GL_LINES );
        glVertex3dv ( &point[shape[name[i]].p[0]].px);
        glVertex3dv ( &point[shape[name[i]].p[1]].px);
        glEnd();
        sprintf( buffer, "%s", shape[name[i]].name );
        cx=cy=cz=0.;
        for(j=0; j<2; j++)
        {
          cx+=point[shape[name[i]].p[j]].px;
          cy+=point[shape[name[i]].p[j]].py;
          cz+=point[shape[name[i]].p[j]].pz;
        }
        cx/=2.;
        cy/=2.;
        cz/=2.;
        text( cx,cy,cz, buffer, glut_font[draw_font] );
        glPopName();
      }
      else if(shape[name[i]].type==2)
      {
        glPushName(name[i]);
        glBegin ( GL_LINE_LOOP );
        glVertex3dv ( &point[shape[name[i]].p[2]].px);
        glVertex3dv ( &point[shape[name[i]].p[0]].px);
        glVertex3dv ( &point[shape[name[i]].p[1]].px);
        glVertex3dv ( &point[shape[name[i]].p[3]].px);
        glEnd();
        sprintf( buffer, "%s", shape[name[i]].name );
        cx=cy=cz=0.;
        for(j=0; j<2; j++)
        {
          cx+=point[shape[name[i]].p[j]].px;
          cy+=point[shape[name[i]].p[j]].py;
          cz+=point[shape[name[i]].p[j]].pz;
        }
        cx/=2.;
        cy/=2.;
        cz/=2.;
        text( cx,cy,cz, buffer, glut_font[draw_font] );
        glPopName();
      }
      else if(shape[name[i]].type==3)
      {
        glPushName(name[i]);
        glBegin ( GL_LINE_STRIP );
        glVertex3dv ( &point[shape[name[i]].p[1]].px);
        glVertex3dv ( &point[shape[name[i]].p[2]].px);
        glVertex3dv ( &point[shape[name[i]].p[4]].px);
        glVertex3dv ( &point[shape[name[i]].p[5]].px);

        glVertex3dv ( &point[shape[name[i]].p[1]].px);
        glVertex3dv ( &point[shape[name[i]].p[6]].px);
        glVertex3dv ( &point[shape[name[i]].p[4]].px);
        glVertex3dv ( &point[shape[name[i]].p[3]].px);
        glVertex3dv ( &point[shape[name[i]].p[1]].px);

        glVertex3dv ( &point[shape[name[i]].p[2]].px);
        glVertex3dv ( &point[shape[name[i]].p[3]].px);
        glVertex3dv ( &point[shape[name[i]].p[5]].px);
        glVertex3dv ( &point[shape[name[i]].p[6]].px);
        glVertex3dv ( &point[shape[name[i]].p[2]].px);
        glEnd();
        sprintf( buffer, "%s", shape[name[i]].name );
        cx=cy=cz=0.;
        for(j=1; j<7; j++)
        {
          cx+=point[shape[name[i]].p[j]].px;
          cy+=point[shape[name[i]].p[j]].py;
          cz+=point[shape[name[i]].p[j]].pz;
        }
        cx/=6.;
        cy/=6.;
        cz/=6.;
        text( cx,cy,cz, buffer, glut_font[draw_font] );
        glPopName();
      }
      else if(shape[name[i]].type==5)
      {
        glPushName(name[i]);
        glBegin ( GL_LINE_STRIP );
        glVertex3dv ( &point[shape[name[i]].p[3]].px);
        glVertex3dv ( &point[shape[name[i]].p[2]].px);
        glVertex3dv ( &point[shape[name[i]].p[0]].px);
        glVertex3dv ( &point[shape[name[i]].p[1]].px);
        glEnd();
        sprintf( buffer, "%s", shape[name[i]].name );
        cx=cy=cz=0.;
        for(j=0; j<2; j++)
        {
          cx+=point[shape[name[i]].p[j]].px;
          cy+=point[shape[name[i]].p[j]].py;
          cz+=point[shape[name[i]].p[j]].pz;
        }
        cx/=2.;
        cy/=2.;
        cz/=2.;
        text( cx,cy,cz, buffer, glut_font[draw_font] );
        glPopName();
      }
    }
  }
  glLoadName(-1);
}


void drawSurfs_plot( int num, int *name, Gsur *surf, Lcmb *lcmb, Lines *line , Points *point, int col, char type)
{
  register int i,j,k,n;
  register int    nl,nc;
  static GLint ipuf[2];

  glLineWidth(1.);

  glLoadName('s');

  if(type=='i')
  {
    glGetIntegerv( GL_CULL_FACE, ipuf );
    if( ipuf[0] == GL_TRUE ) glDisable ( GL_CULL_FACE );
    glShadeModel ( GL_FLAT );
    glEnable( GL_LIGHTING );

    for (i=0; i<num; i++)
    {
      /* printf("surf:%s w:%lf xyz: %lf %lf %lf\n",surf[name[i]].name,scale->w,scale->x,scale->y,scale->z); */
      glPushName(name[i]);
      n=0;
      if(surf[name[i]].npgn>0) while((surf[name[i]].npgn-n))
      {
        n++; /* jump over the polygon token (ie.GL_POLYGON_TOKEN) */
        j=surf[name[i]].pgn[n++];
        glBegin ( GL_POLYGON );
        glNormal3dv (&surf[name[i]].pgn[n]); n+=3;
        for(k=0; k<j; k++)
        {
          /* printf("%d %lf %lf %lf\n", k,surf[name[i]].pgn[n],surf[name[i]].pgn[n+1],surf[name[i]].pgn[n+2]); */
          glVertex3dv (&surf[name[i]].pgn[n]); n+=3; 
        }
        glEnd();
      }
      glPopName();
    }
    glDisable( GL_LIGHTING );
    if( ipuf[0] == GL_TRUE ) glEnable ( GL_CULL_FACE );
  }
  else
  {
#if INX_MODE
    glIndexi (col+offset+CMAP_CELLS_LIGHT);
#endif
#if TEX_MODE
    glColor3f ( entity_r[col], entity_g[col], entity_b[col]  );
#endif 
    for (i=0; i<num; i++)
    {
      glPushName(name[i]);
      for (j=0; j<surf[name[i]].nl; j++ )
      {
        nl=surf[name[i]].l[j];
        if (surf[name[i]].typ[j]=='l')
        {
          glBegin ( GL_LINE_STRIP );
          for (n=0; n<line[nl].nip; n+=3)
          {
            glVertex3dv ( &line[nl].ip[n]);
          }
          glEnd();
        }
        else
        {
          for (k=0; k<lcmb[nl].nl; k++ )
          {
            nc=lcmb[nl].l[k];
            glBegin ( GL_LINE_STRIP );
            for (n=0; n<line[nc].nip; n+=3)
            {
              glVertex3dv ( &line[nc].ip[n]);
            }
            glEnd();
	  }
        } 
      }
    
      if(type=='a')
      {
        sprintf( buffer, "%s", surf[name[i]].name );
        text( (surf[name[i]].cx-scale->x)/scale->w,
            (surf[name[i]].cy-scale->y)/scale->w,
            (surf[name[i]].cz-scale->z)/scale->w, buffer, glut_font[draw_font] );
      }
      glPopName();
    }
  }
  glLoadName(-1);
}


void drawBodys_plot( int num, int *name, Gbod *body, Gsur *surf, Lcmb *lcmb, Lines *line , Points *point, int col, char type)
{
  register int i,j,k,n, b, s;
  register int nl,nc;
  glLineWidth(1.);

#if INX_MODE
    glIndexi (col+offset+CMAP_CELLS_LIGHT);
#endif
#if TEX_MODE
    glColor3f ( entity_r[col], entity_g[col], entity_b[col]  );
#endif 
  glLoadName('b');
  for (i=0; i<num; i++)
  {
    glPushName(name[i]);
    for (b=0; b<body[name[i]].ns; b++ )
    {
      s=body[name[i]].s[b];
      for (j=0; j<surf[s].nl; j++ )
      {
        nl=surf[s].l[j];
        if (surf[s].typ[j]=='l')
        {
          glBegin ( GL_LINE_STRIP );
            for (n=0; n<line[nl].nip; n+=3)
            {
              glVertex3dv
              ( &line[nl].ip[n]);
            }
          glEnd();
        }
        else
        {
          for (k=0; k<lcmb[nl].nl; k++ )
          {
          nc=lcmb[nl].l[k];
          glBegin ( GL_LINE_STRIP );
            for (n=0; n<line[nc].nip; n+=3)
            {
              glVertex3dv
              ( &line[nc].ip[n]);
            }
          glEnd();
          }
        }
      }
      if(type=='a')
      {
        sprintf( buffer, "%s", body[name[i]].name );
        text( (body[name[i]].cx-scale->x)/scale->w,
              (body[name[i]].cy-scale->y)/scale->w,
              (body[name[i]].cz-scale->z)/scale->w, buffer, glut_font[draw_font] );
      }
    }
    glPopName();
  }
  glLoadName(-1);

}

void drawNurl_plot( int num, int *name, int col, char type, int pickflag)
{
  register int i, nr;
  int mode[2];
  GLfloat NurlFillFlag;
  double tx, ty, tz;
  glLineWidth(1.);

  glGetIntegerv( GL_POLYGON_MODE, mode );
  if ( mode[1] == GL_FILL )
      NurlFillFlag=GLU_FILL;
  else
      NurlFillFlag=GLU_OUTLINE_POLYGON;

#if INX_MODE
    glIndexi (col+offset+CMAP_CELLS_LIGHT);
#endif
#if TEX_MODE
    glColor3f ( entity_r[col], entity_g[col], entity_b[col]  );
#endif 
  glLoadName('L');

  for (nr=0; nr<num; nr++ )
  {
    if((nurbl[name[nr]].u_exp<gl_max_eval_order)&&(nurbl[name[nr]].endFlag==1))
    {
      /*
      printf("u_nknt:%d  u_stride:%d  exp:%d typ:%d %d\n", nurbl[name[nr]].u_nknt, nurbl[name[nr]].u_stride, nurbl[name[nr]].u_exp, nurbl[name[nr]].type, GL_MAP1_VERTEX_4);
      for(i=0; i<nurbl[name[nr]].u_nknt; i++) printf("uknt[%d]:%f\n", i, nurbl[name[nr]].uknt[i]);
      for(i=0; i<(nurbl[name[nr]].u_npnt*nurbl[name[nr]].u_stride); i++) { if(i%4==0) printf("\n"); printf(" ctlarray[%d]:%f", i, nurbl[name[nr]].ctlarray[i]);  }
      */

      /* for NurbsErrorCallback */
      if(printFlag) 
      {
        current_nurbs=-name[nr]-1;
        gluNurbsCallback(nurbl[name[nr]].Nurb, GLU_ERROR, (void*)NurbsErrorCallback);
      }
      
      gluNurbsProperty(nurbl[name[nr]].Nurb, GLU_CULLING, GL_TRUE);
      gluNurbsProperty(nurbl[name[nr]].Nurb, GLU_DISPLAY_MODE, NurlFillFlag);
      if(pickflag==PICK) gluNurbsProperty(nurbl[name[nr]].Nurb, GLU_SAMPLING_TOLERANCE, NURS_TOLERANCE_PICK );
      else gluNurbsProperty(nurbl[name[nr]].Nurb, GLU_SAMPLING_TOLERANCE, NURS_TOLERANCE );
      
      glPushName(name[nr]);
      gluBeginCurve(nurbl[name[nr]].Nurb);
      gluNurbsCurve(nurbl[name[nr]].Nurb,
            nurbl[name[nr]].u_nknt,
            nurbl[name[nr]].uknt,
            nurbl[name[nr]].u_stride,
            nurbl[name[nr]].ctlarray,
            nurbl[name[nr]].u_exp+1,
            nurbl[name[nr]].type);
      gluEndCurve(nurbl[name[nr]].Nurb);

      if(type=='a')
      {
        tx=ty=tz=0.; for (i=0; i<nurbl[name[nr]].u_npnt; i++)
        {
          tx+= point[nurbl[name[nr]].ctlpnt[i]].px ;
          ty+= point[nurbl[name[nr]].ctlpnt[i]].py ;
          tz+= point[nurbl[name[nr]].ctlpnt[i]].pz ;
        }
        tx/=nurbl[name[nr]].u_npnt;
        ty/=nurbl[name[nr]].u_npnt;
        tz/=nurbl[name[nr]].u_npnt;
        text( tx,ty,tz, nurbl[name[nr]].name, glut_font[draw_font] );
      }
      glPopName();
    }
    else
    {
      if(printFlag)  printf("ERROR: Nurbl[%d]:%s of order:%d is not supported. Only %d is supported by the gl-lib.\n", nr, nurbl[name[nr]].name, nurbl[name[nr]].u_exp+1, gl_max_eval_order);

    }
  }
  glLoadName(-1);
}



/* the nurbs was evaluated and drawn into a back-buffer, the contents of the back-buffer will be displayed */

void drawNurs_plot( int num, int *name, int col, char type, int pickflag)
{
  int nr;
  int mode[2];
  GLfloat ustep,vstep;
  GLfloat NursFillFlag;
  static GLint ipuf[2];

  glLineWidth(1.);

  /* evaluate and draw into the back-buffer */
  glGetIntegerv( GL_POLYGON_MODE, mode );
  if ( mode[1] == GL_FILL )
      NursFillFlag=GLU_FILL;
  else
      NursFillFlag=GLU_OUTLINE_POLYGON;
  
  //glShadeModel ( GL_SMOOTH );
  glEnable( GL_LIGHTING );
  glEnable(GL_AUTO_NORMAL);
  /* glLightModelfv(GL_LIGHT_MODEL_TWO_SIDE, lmodel_twoside); */

  glLoadName('S');

  glGetIntegerv( GL_CULL_FACE_MODE, ipuf );
  glDisable ( GL_CULL_FACE );

  for (nr=0; nr<num; nr++ )
  {
    if(printFlag)
      printf("nurs:%s\n",nurbs[name[nr]].name); 
    if ((nurbs[name[nr]].u_exp<gl_max_eval_order)&&(nurbs[name[nr]].v_exp<gl_max_eval_order)&&(nurbs[name[nr]].endFlag==1)&&(nurbs[name[nr]].name != (char *)NULL))
    {
      /* for NurbsErrorCallback */
      if(printFlag) 
      {
        current_nurbs=name[nr];
        gluNurbsCallback(nurbs[name[nr]].Nurb, GLU_ERROR, (void*)NurbsErrorCallback);
      }     
      gluNurbsProperty(nurbs[name[nr]].Nurb, GLU_DISPLAY_MODE, NursFillFlag);
      gluNurbsProperty(nurbs[name[nr]].Nurb, GLU_CULLING, GL_TRUE);
      gluNurbsProperty(nurbs[name[nr]].Nurb, GLU_SAMPLING_METHOD, GLU_DOMAIN_DISTANCE );

      /* minimum resolution */
      ustep=nurbs[name[nr]].u_exp*4./(nurbs[name[nr]].uknt[nurbs[name[nr]].u_nknt-1]-nurbs[name[nr]].uknt[0]);
      vstep=nurbs[name[nr]].v_exp*4./(nurbs[name[nr]].vknt[nurbs[name[nr]].v_nknt-1]-nurbs[name[nr]].vknt[0]);

      gluNurbsProperty(nurbs[name[nr]].Nurb, GLU_U_STEP, ustep );
      gluNurbsProperty(nurbs[name[nr]].Nurb, GLU_V_STEP, vstep );

      glPushName(name[nr]);
      gluBeginSurface(nurbs[name[nr]].Nurb);
      gluNurbsSurface(nurbs[name[nr]].Nurb,
        nurbs[name[nr]].u_nknt, nurbs[name[nr]].uknt,
        nurbs[name[nr]].v_nknt, nurbs[name[nr]].vknt,
        nurbs[name[nr]].u_stride, nurbs[name[nr]].v_stride,
        nurbs[name[nr]].ctlarray,
        nurbs[name[nr]].u_exp+1, nurbs[name[nr]].v_exp+1,
        nurbs[name[nr]].type);
      gluEndSurface(nurbs[name[nr]].Nurb);
      glPopName();
    }
    else
    {
      if(printFlag)  printf("ERROR: Nurbs[%d]:%s of order:%d %d is not supported. Only %d is supported by the gl-lib.\n", nr, nurbs[name[nr]].name, nurbs[name[nr]].u_exp+1, nurbs[name[nr]].v_exp+1, gl_max_eval_order);

    }
  }
  glLoadName(-1);

  glDisable(GL_AUTO_NORMAL);
  glDisable( GL_LIGHTING );
  glShadeModel ( GL_FLAT );

  if(type=='a')
  {
    for (nr=0; nr<num; nr++ )
    {
      text( nurbs[name[nr]].tx,nurbs[name[nr]].ty,nurbs[name[nr]].tz, nurbs[name[nr]].name, glut_font[draw_font] );
    }
  }

  /* resore the original cull mode */
  if ( ipuf[0] == GL_BACK ) glCullFace ( GL_BACK );
  if ( ipuf[0] == GL_FRONT ) glCullFace ( GL_FRONT );
 
}
