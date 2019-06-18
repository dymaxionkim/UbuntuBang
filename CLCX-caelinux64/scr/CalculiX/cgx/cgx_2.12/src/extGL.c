
#include <cgx.h>



#define     MY_COLORS       0
#define     MAX_LABELS      32

/* color model */
void define_rgb(float v, float *r, float *g, float *b)
{
   /* v,r,g,b == 0->1 */

#if MY_COLORS
        *r = v;
        *g = cos( (v-0.5) * 3.1415 );
        *b = 1. - v;
#else
        if ( v < 0.1 )       *r = .5 - v*1./0.2;
        else if ( v < 0.5 )  *r = 0. ;
        else if ( v < 0.9 )  *r=  (v-.5)*1./0.4 ;
        else                 *r= 1. ;

        if ( v < 0.1 )       *r = .5 - v*1./0.2;
        else if ( v < 0.5 )  *r = 0. ;
        else if ( v < 0.8 )  *r=  (v-.5)*1./0.3 ;
        else                 *r= 1. - (v-.8)*1./0.4 ;

        if ( v < 0.1 )       *r = .5 - v*1./0.2;
        else if ( v < 0.5 )  *r = 0. ;
        else if ( v < 0.8 )  *r=  (v-.5)*1./0.3 ;
        else if ( v < 0.90 )  *r=  1. ;
        else                 *r=  1. - (v-0.90)*1./0.4 ;


        if ( v < 0.1 )
          *g = 0.;
        else if ( v < 0.3 )  *g = (v-.1) * 1./0.2;
        else if ( v < 0.5 )  *g = 1. - (v-.3) * 1./0.4;
        else if ( v < 0.9 )  *g = .5 + (v-.5) * 1./0.8;
        else                 *g = 1. - (v-.9) * 1./0.1;

        if ( v < 0.1 )
          *g = 0.;
        else if ( v < 0.3 )  *g = (v-.1) * 1./0.2;
        else if ( v < 0.5 )  *g = 1. - (v-.3) * 1./0.4;
        else if ( v < 0.8 )  *g = .5 + (v-.5) * 1./0.6;
        else if ( v < 0.9 )  *g = 1. - (v-.8) * 1./0.1;
        else                 *g = 0.;

        if ( v < 0.1 )
          *g = 0.;
        else if ( v < 0.3 )  *g = (v-.1) * 1./0.2;
        else if ( v < 0.5 )  *g = 1. - (v-.3) * 1./0.4;
        else if ( v < 0.8 )  *g = .5 + (v-.5) * 1./0.6;
        else   *g = 1. - (v-.8) * 1./0.2;


        if ( v < 0.5 )  *b = 1. - (v) * 1./0.5;
        else if ( v < 0.9 )  *b = 0.;
        else *b = (v-.9)*1./0.1;

        if ( v < 0.5 )  *b = 1. - (v) * 1./0.5;
        else *b = 0;
#endif
}


/* schreibt einen Text (x,y,z linkes unteres Eck)  */
void text(double x, double y, double z,char *msg, void *glut_font)
{
  glRasterPos3d(x, y, z);
  while (*msg) {
    glutBitmapCharacter(glut_font, *msg);
    msg++;
  }
}


/* scala verbindet Farben mit Zahlenwerten (rgb-mode)*/
/* kb, kh Kaestchenhoehe und Breite                  */
/* dx, dy  Ursprung der Scala (linkes unteres Eck)   */
/* divisions anzahl der Kaestchen                    */
void scala_rgb( double dx, double dy, int divisions, double bmin, double bmax, double *col, void *glut_font, char format)
{
  int  j, i;
  float r, g, b;
  double df, f;
  static char string[13];
  double     kb, kh;
  int flag=0;

  df = (bmax-bmin)/divisions;
  f  = bmin;

  kh= 1.0/divisions*1.5;
  kb= 1.0/20.;


  for (i=0; i<divisions; i++)
  {
    glBegin ( GL_POLYGON );
      define_rgb( (float)(f-bmin)/(bmax-bmin), &r,&g,&b);
      glColor3d ( r, g, b );
      glVertex2d ( dx-kb*0., dy+kh*0. );
      glVertex2d ( dx-kb*1., dy+kh*0. );
      define_rgb( (float)(f+df-bmin)/(bmax-bmin), &r,&g,&b);
      glColor3d ( r, g, b );
      glVertex2d ( dx-kb*1., dy+kh*1. );
      glVertex2d ( dx-kb*0., dy+kh*1. );
    glEnd();

    if(flag==0)
    {
      if(format=='f') sprintf ( string, "%-10f ", f);
      else if(format=='i') sprintf ( string, "%-10d ", (int)f);
      else sprintf ( string, "%-10.2e ", f);
      glColor3dv ( col );
      glRasterPos2d( (dx+kb*0.2), dy-kh*0.1 );
      for ( j=0; j<10; j++) glutBitmapCharacter(glut_font, string[j]);
    }
    flag++;
    if(flag>divisions/MAX_LABELS) flag=0;

    dy+=kh;
    f+=df;
  }
  if(flag==0)
  {
    if(format=='f') sprintf ( string, "%-10f ", bmax);
    else if(format=='i') sprintf ( string, "%-10d ", (int)bmax);
    else sprintf ( string, "%-10.2e ", bmax);
    glColor3dv ( col );
    glRasterPos2d( (dx+kb*0.2), dy-kh*0.1 );
    for ( j=0; j<10; j++) glutBitmapCharacter(glut_font, string[j]);
  }
}


/* scalai verbindet Farben mit Zahlenwerten         */
/* kb, kh Kaestchenhoehe und Breite                */
/* dx, dy  Ursprung der Scala (linkes unteres Eck) */
/* divisions anzahl der Kaestchen                  */
void scala_indx( double dx, double dy, int divisions, double bmin, double bmax, int offset, int col, void *glut_font, char format)
{
  int j, i;
  double     df, f;
  static char string[13];
  double     kb, kh;
  int flag=0;

  df = (bmax-bmin)/(divisions);
  f  = bmin;

  kh= 1.0/divisions*1.5;
  kb= 1.0/20.;

  for (i=0; i<divisions; i++)
  {
    glIndexi ( offset+i );
    glBegin ( GL_POLYGON );
      glVertex2d ( dx-kb*0., dy+kh*0. );
      glVertex2d ( dx-kb*1., dy+kh*0. );
      glVertex2d ( dx-kb*1., dy+kh*1. );
      glVertex2d ( dx-kb*0., dy+kh*1. );
    glEnd();

    if(flag==0)
    {
      if(format=='f') sprintf ( string, "%-10f ", f);
      else if(format=='i') sprintf ( string, "%-10d ", (int)f);
      else sprintf ( string, "%-10.2e ", f);
      glIndexi  ( col );
      glRasterPos2d( (dx+kb*0.2), dy-kh*0.1 );
      for ( j=0; j<10; j++) glutBitmapCharacter(glut_font, string[j]);
    }
    flag++;
    if(flag>divisions/MAX_LABELS) flag=0;

    dy+=kh;
    f+=df;
  }
  if(flag==0)
  {
    if(format=='f') sprintf ( string, "%-10f ", bmax);
    else if(format=='i') sprintf ( string, "%-10d ", (int)bmax);
    else sprintf ( string, "%-10.2e ", bmax);
    glIndexi  ( col );
    glRasterPos2d( (dx+kb*0.2), dy-kh*0.1 );
    for ( j=0; j<10; j++) glutBitmapCharacter(glut_font, string[j]);
  }
}


/* scalai verbindet Farben mit Zahlenwerten         */
/* kb, kh Kaestchenhoehe und Breite                */
/* dx, dy  Ursprung der Scala (linkes unteres Eck) */
/* divisions anzahl der Kaestchen                  */
void scala_tex( double dx, double dy, int divisions, double bmin, double bmax, double scale, double *col, void *glut_font, char format)
{
  int j, i;
  double     df, f;
  static char string[13];
  double     kb, kh;
  int flag=0;

  df = (bmax-bmin)/(divisions);
  f  = bmin;

  kh= 1.0/divisions*1.5;
  kb= 1.0/20.;

  for (i=0; i<divisions; i++)
  {
    /* enable all colors for the TEX_MODE */
    glColor3d( 1,1,1);
    glEnable(GL_TEXTURE_1D);
    glTexCoord1d    ( (GLdouble)i/(GLdouble)divisions*scale );
    glBegin ( GL_POLYGON );
      glVertex2d ( dx-kb*0., dy+kh*0. );
      glVertex2d ( dx-kb*1., dy+kh*0. );
      glVertex2d ( dx-kb*1., dy+kh*1. );
      glVertex2d ( dx-kb*0., dy+kh*1. );
    glEnd();
    glDisable(GL_TEXTURE_1D);

    if(flag==0)
    {
      if(format=='f') sprintf ( string, "%-10f ", f);
      else if(format=='i') sprintf ( string, "%-10d ", (int)f);
      else sprintf ( string, "%-10.2e ", f);
      glColor3dv ( col );
      glRasterPos2d( (dx+kb*0.2), dy-kh*0.1 );
      for ( j=0; j<10; j++) glutBitmapCharacter(glut_font, string[j]);
    }
    flag++;
    if(flag>divisions/MAX_LABELS) flag=0;

    dy+=kh;
    f+=df;
  }
  if(flag==0)
  {
    if(format=='f') sprintf ( string, "%-10f ", bmax);
    else if(format=='i') sprintf ( string, "%-10d ", (int)bmax);
    else sprintf ( string, "%-10.2e ", bmax);
    glColor3dv ( col );
    glRasterPos2d( (dx+kb*0.2), dy-kh*0.1 );
    for ( j=0; j<10; j++) glutBitmapCharacter(glut_font, string[j]);
  }
}

/* Button liefert 1 wenn maus im Button, sonst 0  */
/* dx,dy    Ursprung des Buttons (linkes unteres Eck)  */
/* bh, bb   Hoehe, Breite des Buttons  */
/* mx, my   Mauskoordinaten  */
int button(double dx, double dy, char *msg, double mx, double my, void *glut_font)
{
    register int   i, n;
    double bh, bb;

    i=0;
    bb=bh=0.05;

    while (*msg) {i++; msg++; }
    bb+=0.025*i;
    msg-=i;



    if ((mx > dx+bb*0.) & (mx < dx+bb*1.) & (my > dy+bh*0.) & (my < dy+bh*1.))
      {
      glColor3d ( 1., 0.0, 0.0 );
      glBegin ( GL_POLYGON );
        glVertex2d ( dx+bb*0., dy+bh*0. );
        glVertex2d ( dx+bb*1., dy+bh*0. );
        glVertex2d ( dx+bb*1., dy+bh*1. );
        glVertex2d ( dx+bb*0., dy+bh*1. );
      glEnd();
      glColor3d ( 0., 0., 0. );
      glRasterPos2d((dx+bh*0.3), (dy+bh*0.2));
      for (n=0; n<i; n++)
        glutBitmapCharacter(glut_font, msg[n]);
      glBegin ( GL_LINE_LOOP );
        glVertex2d ( dx+bb*0., dy+bh*0. );
        glVertex2d ( dx+bb*1., dy+bh*0. );
        glVertex2d ( dx+bb*1., dy+bh*1. );
        glVertex2d ( dx+bb*0., dy+bh*1. );
      glEnd();
      return (1);
      }
    else
      {
      glColor3d ( 0.7, 0.7, 0.7 );
      glBegin ( GL_POLYGON );
        glVertex2d ( dx+bb*0., dy+bh*0. );
        glVertex2d ( dx+bb*1., dy+bh*0. );
        glVertex2d ( dx+bb*1., dy+bh*1. );
        glVertex2d ( dx+bb*0., dy+bh*1. );
      glEnd();
      glColor3d ( 0., 0., 0. );
      glRasterPos2d((dx+bh*0.3), (dy+bh*0.2));
      for (n=0; n<i; n++)
        glutBitmapCharacter(glut_font, msg[n]);
      glBegin ( GL_LINE_LOOP );
        glVertex2d ( dx+bb*0., dy+bh*0. );
        glVertex2d ( dx+bb*1., dy+bh*0. );
        glVertex2d ( dx+bb*1., dy+bh*1. );
        glVertex2d ( dx+bb*0., dy+bh*1. );
      glEnd();
      return(0);
      }
}


/* Plaziert farbige Punkte  */
void polymark ( int n, double *col_r, double *col_g, double *col_b, double *x,
 double *y, double *z )
{
  GLint mode;
  int i;

  glGetIntegerv(GL_RENDER_MODE, &mode);

  if (mode == GL_RENDER)
    {
    for (i=1; i<=n; i++ )
      {
      glBegin ( GL_POINTS );
       glColor3d ( col_r[i], col_g[i], col_b[i] );
       glVertex3d ( x[i], y[i], z[i] );
      glEnd();
      }
    }
  if (mode == GL_SELECT)
    {
    for (i=1; i<=n; i++ )
      {
      glLoadName(i);
      glBegin ( GL_POINTS );
       glColor3d ( col_r[i], col_g[i], col_b[i] );
       glVertex3d ( x[i], y[i], z[i] );
      glEnd();
      }
    }
}



