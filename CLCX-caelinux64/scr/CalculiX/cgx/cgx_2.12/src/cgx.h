/* --------------------------------------------------------------------  */
/*                          CALCULIX                                     */
/*                   - GRAPHICAL INTERFACE -                             */
/*                                                                       */
/*     A 3-dimensional pre- and post-processor for finite elements       */
/*              Copyright (C) 1996 Klaus Wittig                          */
/*                                                                       */
/*     This program is free software; you can redistribute it and/or     */
/*     modify it under the terms of the GNU General Public License as    */
/*     published by the Free Software Foundation; either version 2 of    */
/*     the License, or (at your option) any later version.               */
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

/* ALL function prototypes for the program "CalculiX GraphiX (cgx)" */

#include "extUtil.h"
#include "trackball.h"

/* 
choose the compiler option "-D SGI_MIPS" in the Makefile for best results 
if you are working on a sgi workstation or
if you are working on a linux system with 8-Bit color depth. In this case 
rendering is done in the index-mode.
*/
#ifdef SGI_MIPS
  #define     INX_MODE    1
  #define     TEX_MODE    0
#else
  #define     INX_MODE    0
  #define     TEX_MODE    1
#endif


/* factors for the geometric tolerance for merging */
/* the space used by all lines will be multiplied with this value to calculate the default */
/* geometric tolerance (dx,dy or dz whichever is greatest. ie: gtol=GTOL*dx) */
#define     GTOL        1.e-5    
/* amplification factor for the automatic merging of edge-points if a cad-file is read (tol=gtol*GTOL_EDGES) */
#define     GTOL_EDGES  2.e1

/* default values for calcLineDiv(). Can be overwritten with the "div" command */
/* defines the minimum angle between sectors of a line during automatic line division calculation */
//  7.5 deg
#define     GTOL_COS_A 0.9914448
/* defines the minimum angle difference before the line bias is increased */
#define     MIN_DCOS_A 0.004 
/* Defines the default maximum distance between nodes during automatic line division calculation */
/* GTOL_NODE_DIST * gtol = maximum distance between nodes */
#define     GTOL_NODE_DIST 5.e2
/* defines the default minimum-element-length/maximum-element-length, used at curves */
#define     ELEM_LENGTH_RATIO 0.3

/* meshing parameters for the unstructured tr3 mesher (mesh2d) */
#define     NADAPT   8                 // for mesh2d()
#define     ALPHA    0.8               // alfa, beta both 0.8 results in an unchanged average mesh density
#define     BETA     0.8
#define     MESH2D_CURVATURE_FACTOR 10. // modifies mesh density requirement (afpha) if qfactor failes:
        //  alpha=alpha-(qfactor*MESH2D_CURVATURE_FACTOR), a higher value forces a stronger mesh change.
#define     MESH2D_MAX_APLHABETA    20.      // max-value for alpha and beta in auto-refine
#define     MESH2D_MIN_APLHABETA    0.38      // min-value for alpha and beta in auto-refine
#define     ALPHAFACTOR 0.7            // modifies alpha in case of bad elems (in both directions)
#define     MAX_MESH2D_LOOPS 3              // max refinement loops of mesh2d()
#define     MESH2D_QUALITY_THRESHOLD 0.01     // qfactor=dist_cg_nurbs/circumlength_of_tri (curvature, 1st parameter)

/* element splitter parameter to force a certain maximum distance of the element cg to the nurbs */
#define     TRISPLITTER_QUALITY_THRESHOLD_FACTOR 0.3 // dist_nurbs/v_n1n2 *TRISPLITTER_QUALITY_THRESHOLD_FACTOR, to modify the req. curv in triSplitter()


/* default location of the help files, please insert the actual one */
#ifdef MTU
  #ifdef DEVEL
    #define     HELPFILE        {"/yaprod/yae55/Linux_x86_64/bin/cgx_doc/cgx.html",\
                             "/yaprod/yae55/Linux_x86_64/bin/ccx_doc/ccx.html"}
  #else
    #define     HELPFILE        {"/yaprod/cae/application/CalculiX/doc/cgx_2.12/cgx.html",\
                             "/yaprod/cae/application/CalculiX/doc/ccx_2.12/ccx.html"}
  #endif
#elif defined AFLIB
  #define     HELPFILE        {"/usr/local/CalculiX/cgx_2.12/doc/cgx/cgx.html",\
    "/usr/local/CalculiX/ccx_2.12/doc/ccx/ccx.html",\
    "/usr/local/CalculiX/cgx_2.12/doc/aflib/aflib.pdf"}
#else
  #define     HELPFILE        {"/usr/local/CalculiX/cgx_2.12/doc/cgx/cgx.html",\
    "/usr/local/CalculiX/ccx_2.12/doc/ccx/ccx.html"}
#endif

/* html browser, change if necessary */
/* postscript viewer, change if necessary */
#ifdef MTU
  #define     BROWSER         {"firefox"}
  #define     PSVIEWER         {"gv"}
#else
/* #define     BROWSER         {"netscape"} */
/*   #define     BROWSER         {"mozilla"} */
  #define     BROWSER         {"firefox"}
/* #define     PSVIEWER         {"ghostview"} */
  #define     PSVIEWER         {"gv"}
#endif

/* more parameters, better do not change them! */
#define     TEX_PIXELS  512          /* number of possible colors in texture mode */
#define     GAMMA       1.9          /* gamma lightens or darkens an image (Hcpy) */
#define     INI_SCREEN      600
#define     INI_MENU_WIDTH  184
#define     INI_MENU_HEIGHT  72
#define     PS_DENSITY       107     /* basic-density to convert xwd to ps */
#define     Z_DEPTH          2.      /* edge length of the drawing cube */
#define     SET_COLS    9            /* no. of predefined colors */
#define     BAS_COLS    3            /* basic colours: black, white, nickel */
#define     DEF_COL     2            /* default colour */
#define     PICK        1            /* lower nurbs-density during picking */
#define     PICK_LENGTH 5            /* default cursor-length during picking */
#define     DEF_LINE_DIV 4           /* default line division for new lines */    
#define     MAX_LINE_DIV 99          /* maximum line division (cadfix restriction) */
#define     MIN_LINE_DIV 2           /* minimum line division used for the automatic division calculation */
#define     NURS_TOLERANCE 30
#define     NURS_TOLERANCE_PICK 80
#define     TOL_AMBIG  1.e-2        /* so close must a point lie to an ambiguous edge to be considered as ambiguous (trimming of nurbs) */
#define     MAX_PARAM_PER_RECORD 20 /* parameter per record in command files */
#define     GL_FEEDBACK_BUFF_SIZE 3000000  /* size of the feedback buffer for nurbs-rendering */
#define     UV_STEPS        101           /* resolution of the uv-space of nurbs (triangulation ) */
#define     MIN_ANGLE_TRI3  0.99          /* badelems.c: S*S, alfa=acos(sqrt(MIN_ANGLE_TRI3)) */
#define     MAX_MATERIALS     99999       /* maximum material number (limited by frd-format to 99999) */
#define     MILLISECONDS_PER_PERIOD 1200  /* default length of one period of animation */
#define     GLUT_MENU_POINT_HEIGHT  18    /* seems to be always that value anyway(?) */
#define     GLUT_FONT      {  GLUT_BITMAP_TIMES_ROMAN_10,\
                              GLUT_BITMAP_HELVETICA_12,\
                              GLUT_BITMAP_8_BY_13,\
                              GLUT_BITMAP_9_BY_15,\
                              GLUT_BITMAP_HELVETICA_18,\
                              GLUT_BITMAP_TIMES_ROMAN_24}
#define     GLUT_FONT_WIDTH  { 6,7,8,9,10,11 }
#define     DEF_GLUT_FONT      3          /* indx starts with 0 */
#define     SUM_GLUT_FONTS     6

/* glu steps seem to be restricted */
#define     CGX_GLU_MAX_STEPS 200


/*
used fom material illumination
*/
#define AMB      1.0
#define DIFF     0.5
#define DIFF_I     1.
#define MAT_DIFF 0.6
#define MAT_SPEC 0.0

#define  CMAP_CELLS_LIGHT  34             /* number of grey-values in the colormap */
#define  CMAP_DARKSHIFT     5             /* black-shift of the most bright color-cell */

/* fixed values necessary for the mesher, must not be changed */
#define  MAX_SURFS_PER_BODY   6
#define  MAX_EDGES_PER_SURF 4
#define  MAX_CORNERS_PER_BODY 8

/* for convenience fixed arrays are used in interactive functions, values can be changed */
#define  EDGES_PER_SURF 1000
#define  CORNERS_PER_BODY 100
#define  SURFS_PER_BODY   1000
#define  LINES_PER_EDGE  100

/* for programmer convenience a max-value of faces per element is given (sufficient for all types) */
#define  MAX_FACES_PER_ELEM 6


/* suppress the creation of nurbsobjects, only splines are made from nurb-lines */
#define NO_NURL 0
#define NO_NURS 0


/* special sets, generated by cgx  */
/* internal sets should have a leading "+" or "-" */
/* Setnames with leading "-" are not shown with qenq */ 
#define     NJBY          "-NJBY"   /*  */ 
#define     COPY          "-COPY"   /*  */
#define     MESH          "-MESH"   /*  */
#define     NOMESH        "-NOMESH" /*  */
#define     ZAP           "-ZAP"    /*  */
#define     IMPC          "-IMPC"   /* independent nodes of equations  */
#define     DMPC          "-DMPC"   /* dependent nodes of equations    */
#define     NOMPC         "-NOMPC"  /* not used dependent nodes */
#define     NOEL          "-NOEL"   /* dependent nodes for which no matching indep-elem was found but it was conn to a close one */
#define     ORI           "-ORI"    /*  */
#define     DEP           "-DEP"    /* used for proj */
#define     IND           "-IND"    /* used for proj  */
#define     TMP           "-TMP"    /* temporary buffer for several purposes */
#define     HIGHLIGHT     "-HIGHL"  /* highlights identified entities */
#define     UORI          "-UORI"   /* save all unoriented items */
#define     BNUR          "-BNUR"   /* save all bad nurbs */
#define     BSUR          "-BSUR"   /* save all bad surfs */
#define     NOPRJ         "-NOPRJ"  /* failed projection */
#define     CF            "+CF"     /* contact faces */
#define     PLOT2D        "+PLT2D"  /* selected nodes for the menu-graph (2d-plot) */

typedef struct {
  char flpb[MAX_LINE_LENGTH];
  char njby[MAX_LINE_LENGTH];
  char copy[MAX_LINE_LENGTH];
  char mesh[MAX_LINE_LENGTH];
  char nomesh[MAX_LINE_LENGTH];
  char zap[MAX_LINE_LENGTH];
  char impc[MAX_LINE_LENGTH];
  char mpc[MAX_LINE_LENGTH];
  char nompc[MAX_LINE_LENGTH];
  char noel[MAX_LINE_LENGTH];
  char ori[MAX_LINE_LENGTH];
  char dep[MAX_LINE_LENGTH];
  char ind[MAX_LINE_LENGTH];
  char tmp[MAX_LINE_LENGTH];
  char highl[MAX_LINE_LENGTH];
  char uori[MAX_LINE_LENGTH];
  char bnur[MAX_LINE_LENGTH];
  char bsur[MAX_LINE_LENGTH];
  char noprj[MAX_LINE_LENGTH];
  char cf[MAX_LINE_LENGTH];
  char plot2d[MAX_LINE_LENGTH];
} SpecialSet;

typedef struct {
  double r, g, b;
} Colours;


typedef struct {
    char n, e, p, l, c, s, b, L, S, se, sh;
} Lchar;


typedef struct {
  double r;
  int i;
}Rsort;


typedef struct {
  char typ;
  int indx;
  int nr;
}CfdSurf;


typedef struct {
  char *name;
  GLsizei width;
  GLsizei height;
  GLenum format;
  GLenum type;
  GLvoid *pixels;
  GLfloat zoom[2];
}BGpicture;

typedef struct {
  int nr;
  int n1, n2;
  double val;
} Qcut_nodes;

typedef struct {
  double jbir, aspr, mca;
}Eqal;

typedef struct {
  int sets;          /* nr of sets */
  int *type;         /* from "int transform()":   fehler: -1   tra: 1   rad: 2   rot: 3   sca: 4 */
  int *anz_n;
  int **axis;
  int **mnod, **snod ;
  double *fi;
}CopiedNodeSets;

typedef struct {
  int nds;
  int *ds;
} DsSequence;

typedef struct {
  double p1[3];
  double p2[3];
  double p3[3];
  double cg[3];
  int fnr;
} Tri;

//****************  NURBS Utilities   ****************//
typedef struct 
{
  double * cX;
  double * cY;
  double * cZ;
  double * weights;
  double * uKnt;
  double * vKnt;
  int nUPol, nVPol, nUKnt,nVKnt;
  int uDeg,vDeg;
}BSplineSurface;

typedef struct 
{
  double * cX;
  double * cY;
  double * cZ;
  double * w;
  double * k;
  int nPol, nKnt;
  int deg;
}BSplineCurve;

void makeTorus(double *p1, double *p2, double r1, double r2, BSplineSurface * mySurf);

void translateBSpline(double *p1, double *p2, BSplineCurve * myCurve ,BSplineSurface * mySurf);

void rotateBSpline(double *p1, double *p2, BSplineCurve * myCurve ,BSplineSurface * mySurf);

double wik(int k,int i,double *u,double x);

double deBoor(int k, int i, double * u ,double x);

void calculateBSpline(double * pnt, BSplineCurve * myCurve,double u);

void piaFitting(double pCloud [][3],int nPnt,BSplineCurve * fitCurve, int deg, double tolerance);

//**************  NURBS Utilities END  ***************//


int getElemFaceNodes(Elements *e_enqire, int el, int face, int *nface);
int getFaceNr(Elements *e_enqire, int elem_nr, int *nod);
int nurbl2seq(int nurlNr, const Nurbl *nurbl);
int getBias_fbd(int l, Lines *line);
double calcLineLength(int l);
double pre_length(char *setname);
double pre_area(char *setname);
double pre_volu(char *setname);
void pre_bia( char *record);
void pre_animate(char *string);
void pre_view(char *string);
void completeFacesByTolerance(int set, int setNrbuf, double qaddTol);
void splitBiasDiv(int *ptrdiv, double *ptrbias);
int improveBadTr3(char *setname);
void printHash();
int write2foam(char *setname, int strings, char **string, Summen *anz, Nodes *node, Faces *face, Elements *e_enqire, Sets *set, Datasets *lcase );
int write2dolfyn(char *setname, int strings, char **string, Summen *anz, Nodes *node, Faces *face, Elements *e_enqire, Sets *set);
int readFoam(char *datin, Summen *anz, Sets **sptr, Nodes **nptr, Elements **eptr, Datasets **lptr );
int readAnsysList(char *datin, Summen *anz, Sets **sptr, Nodes **nptr, Elements **eptr, Datasets **lptr );

/* writebp.c */
int writebp(char *setname, Summen *anz, SumGeo *anzGeo, Points *pnt, Sets *set );

/* writefbd.c */
int writefbd(char *setname, Summen *anz, SumGeo *anzGeo, Points *pnt, Lines *line, Lcmb *lcmb, Gsur *surf, Gbod *body, Nurbs *nurs, Sets *set, Shapes *shape, Values *value );

/* XFunktions_mesa.h */
void calcOffset(void);
void getColormap(void);
void storeColors(int anz_col, int pix );

/* copyEntity.h */
int createCenterpnt( char *pkt1, char *pkt2, char *buf, char *pkt3, char r );
int copy_set( int settrgt, char *trans, int setNr, int *dep_n, int *dep_e, int *dep_p, int *dep_l, int *dep_c, int *dep_s, int *dep_se, int *dep_sh, int *dep_S, int mastersetNr, int appendSetFlag  );
void pre_swep( char *record );
void pre_copy( char *record );
int  body_( char *name, char *corner );

/* dataGeo.h */
void descalPoints ( int anz_p, Points *point, Scale *scale );
void scalPoints ( int anz_p,  Points *point, Scale *scale );
void descalSurfs ( int anz_s,  Gsur *surf, Scale *scale );
void scalSurfs ( int anz_s,  Gsur *surf, Scale *scale );
void scalNurbs ( Nurbs *nurbs,  int nr, int patch,  Scale *scale);
void descalNurbs ( Nurbs *nurbs,  int nr, int patch,  Scale *scale);

/* dataMesh */
void descalNodes ( int anz_n, Nodes *node, Scale *scale );
void scalNodes ( int anz_n, Nodes *node, Scale *scale );
void calcDatasets( int num_olc, Summen *anz, Nodes *node, Datasets *lcase );
void transformDatasets( int lc, Summen *anz, Nodes *node, Datasets *lcase, char *method, char *axis  );
void calcAnimation( int anim_steps, double anim_faktor, int *anim_alfa, int halfperiode, int centerNode, Summen *anz, Nodes *node, Elements *e_enqire, Datasets *lcase, int lc, Scale *scale, char surfFlag, double *colNr, int steps );
void calcSequence( DsSequence dsSequence, double anim_faktor, int halfperiode, int centerNode, Summen *anz, Nodes *node, Elements *e_enqire, Datasets *lcase, Scale *scale, char surfFlag, double *colNr, int steps, int lcase_animList, int dispFlag );

/* defineEntity.h */
int hashAlias( SumAsci *sumAsci, char *name, int nr);
void operateAlias( char *name, char *type );
void delNod( int anzn, int *number );
int  nod( Summen *anz, Nodes **nptr, int flag, int nodnr, double x, double y, double z, int scalFlag );
void pre_nod( char *record);
void delElem( int anze, int *index );
int elem_define( int elnr, int type, int *node, int setFlag, int eattr );
int hashPoint( SumAsci *sumAsci, char *name, int nr);
int hashLine( SumAsci *sumAsci, char *name, int nr);
int hashLcmb( SumAsci *sumAsci, char *name, int nr);
int hashSurf( SumAsci *sumAsci, char *name, int nr);
int hashBody( SumAsci *sumAsci, char *name, int nr);
int hashSet( SumAsci *sumAsci, char *name, int nr);
void pre_elem( char *record);
void delVal( int anzv, int *number );
int value_i( char *name, char *string );
int hashValue( SumAsci *sumAsci, char *name, int nr);
int pre_value( char *record);
void delPnt( int anz, int *number );
int  pnt( char *name, double x, double y, double z, int scalFlag );
int pre_pnt( char *record, int flag);
void delShape( int anz, int *number );
int  shape_i( char *name, int type, int ip1, int ip2, int ip3, int ip4, int ip5, int ip6, int ip7 );
void pre_shape( char *record, int addFlag);
void delLine( int anz, int *number );
int line_i( char *name, int ip1, int ip2, int trk, int div, double bias, char type );
int  line_( char *name, char *p1, char *p2, char *trk, int div, double bias );
int pre_line( char *record, int addFlag);
void delLcmb( int anz, int *number );
int lcmb_i( char *name, int add, int anz_l, char  *ori, int *lin );
int  lcmb_( char *name, int add, int anz_l, char *ori, char *lin );
void pre_lcmb( char *record, int addFlag);
void bak_delSurf( int anz, int *number );
void delSurf( int anz, int *number );
int  surface_i( char *name, char ori, int blend, int anz_c, char *cori, int *edge, char *typ );
int  surface( char *name, char ori, char *blend, int anz_c, char *cori, char *corner );
void pre_gsur( char *record, int addFlag);
void pre_surf( char *record);
void bak_delBody( int anz, int *number );
void delBody( int anz, int *number );
int  gbod_i( char *name, int blend, int anz_c, char *cori, int *corner );
int  gbod( char *name, char *blend, int anz_c, char *cori, char *corner );
int  pre_gbod( char *record, int addFlag);
int  pre_body( char *record);
int  nurl( char *string, int addFlag );
void delNurs( int anz, int *number );
int hashNurs( SumAsci *sumAsci, char *name, int nr);
int  nurs( char *string, int addFlag );


/* dispLists.h */
void drawNodes_vector(  int lc, int ne, int *e, double v_factor, int num, int *name, Nodes *node );
void drawFaces_vector(  int lc, int ne, int *e, double v_factor, int num, int *name, Nodes *node, Faces *face );
void drawElements_vector( int lc, int ne, int *e, double v_factor, int num, int *name, Nodes *node, Elements *e_enqire );
void drawModelEdges( GLuint list_model_edges, int color, double width,
                     int numEdges, Nodes *node );
void drawDispListEdges( GLuint list, int color, double width, char key, Nodes *node );
void drawFaces_edge( int num, int *name, Nodes *node, Faces *face, int col, char type );
void drawElem_edge( int num, int *name, Nodes *node, Elements *e_enqire, int col, char type );
#if INX_MODE
void drawDispList( GLuint list, char key, Nodes *node, int *colNr );
#endif
#if TEX_MODE
void drawDispList( GLuint list, char key, Nodes *node, double *colNr );
#endif





/*  extFunktions.h */
void help( void );
void resetScaleValues( Scale *scale);
void getScaleValues( int setNr, Sets *set, Points *point, Nodes *node, Scale *scale);
int compareInt(int *a, int *b);
int compareFloat(double *a, double *b);
int askGLError(char buffer[134]);
void defineColIndexes_light(void);
void defineColIndexes_load(void);
void defineColIndexes_set(void);
void defineColTextur_load(void);
void zoom(double xcur0, double ycur0, double xcur1, double ycur1);
void qzoom(void);
void center(double x, double y, double z);
void qcenter(int x, int y);
void qsequence(int x, int y);
void qgraph(int x, int y);
void rot_u(double a);
void rot_r(double a);
void rot_c(double a);
void rot_x(double a);
void rot_y(double a);
void rot_z(double a);
void pre_rot( char *record);
void transformResults( char *record);
int transform( char *record, int anz_n, Nodes *node );
int calcLineDiv(Lines *line, int nr, double gtol_cos_a, double lmax, double lmin);
int fillBlendedSurf(int nr);
void repNurl(int nr );
void repNurs(int nr );
void untrimNurs(int nr );
int repSurf(int nr, int renderFlag);
void repShape(int nr );
int repLine( int j );
void fixMidsideNodes( char *setname, char *parameter);
void adjustDrawNodes(int flag);
void addDispToNodes(Summen *anz, Nodes *n_anim, Nodes *node, Elements *e_enqire, Datasets *lcase, int lc, double amplitude);
int write2stack(int n, char parameter[20][MAX_LINE_LENGTH]);
int enquireEntities(char *string);
int write2stack(int n, char parameter[20][MAX_LINE_LENGTH]);


/* cgx.h */
#if INX_MODE
void nodalDataset( int cur_entity, int lc, Summen *anz, Scale *scale, Nodes *node
		   , Datasets *lcase int *colNr, int offset, int maxIndex, int steps, int scalaFlag );
#endif 
#if TEX_MODE
void nodalDataset( int cur_entity, int lc, Summen *anz, Scale *scale, Nodes *node
		   , Datasets *lcase, double *colNr, int scalaFlag );
#endif 
void elementDataset( int auswahl, int lc, Summen *anz, Scale *scale, Datasets *lcase, int offset, int maxIndex, int steps );
void realloc_colNr(void);
void entryfunktion( int state );
void WindowState( int state );
void Mouse( int x, int y );
void MouseState( int button, int state, int x, int y );
void frame(void);
void menu( int selection );
void createHardcopy( int selection );
void updateDispLists(void);
void ConfigureAndShowWindow_Light(void);
void ConfigureAndShowWindow_Load( void );
void ConfigureAndShowWindow_Sequence(int dispFlag );
void ConfigureAndShowWindow_Vector( void );
int minus( char *record );
int  plot( char *record );
int  plus( char *record );
void changeAnimation( int selection );
void newTimePerPeriode( int selection );
void tuneAnimation( int selection );
void stepsAnimation( int selection );
void idleFunction(void);
void redraw(void);
void selectEntity( int selection );
void createDatasetEntries(void);
void selectDataset( int selection );
void createNewMainMenu(void);
void recompileEntitiesInMenu(int pre_lc);
void addDispToCoordinates( Nodes *nodes);
void selectView( int selection );
void selectData( char *record);
void specialKeyboard( int gkey, int x, int y );
void Keyboard( unsigned char gkey, int x, int y );
void moveModel(void);
void DrawGraficLoad( void );
void DrawGraficLight( void );
void DrawGraficAnimate( void );
void DrawGraficSequence( void );
void DrawGraficVector( void );
void DrawPickedItems(void);
void drawSets(int mode);
void DrawAxes();
void iniDrawMenu(void);
void DrawMenuLoad( void );
void DrawMenuSequence( void );
void DrawMenuLight( void );
void DrawMenuAnimate( void );
void DrawMenuSet( void );
void reshape( int width, int height );
void initLightAndMaterial_rgb( void );
void initLightAndMaterial_index( void );

/* iniMeshData.h     */
void vdsort ( int *a );
void vdsort2 ( int *a );
int compareElnum1 (int *a, int *b);
int selectDisplayFaces (Summen *anz, Elements *e_enqire, Faces **facePtr, Edges **edgePtr) ;
void makeSurfaces(void);
void iniMeshData( char *datin, char *format );
void iniElements(Summen *anz, Elements *elem, int ini_e_enqire);

/* mergEntity.h */
int *findCloseEntity( char type, Nodes *node, Points *point, Rsort *rsort, int indx, int anz_n, double local_gtol);
int mergeNode( int *arrayIndx, int anz_n, int indx, int lock, int *nodes, int **nod2elem );
int mergePnt( Rsort *rsort, int anz_p, int indx, double local_gtol, int lock );
int mergeLine( int indx, int lock );
int mergeLcmb( int indx );
int mergeSurf( int indx, double local_gtol );
int compareRsort(Rsort *a, Rsort *b);
void pre_merge( char *record );

/* orient.h */
int orientLcmb( int nr );
int orientSurf( int nr );
int ori7SidedBody( int nr, int **cp );
int ori6SidedBody( int nr, int **cp );
int ori5SidedBody( int nr, int **cp );
int orientBody( int nr );
void orientSet( char *record);

/* pickFunktions.h    */
int hitAction( GLuint *name, char *type, int x, int y );
int hitUndo( GLuint *name, char *type, int x, int y );
int pickstack( GLuint name );
int processHits( GLint hits, GLuint *buffer, char *type, char *mode, int x, int y );
void defineDiv( unsigned char gkey, int x, int y );
void defineValue( unsigned char gkey, int x, int y );
void pick( unsigned char gkey, int x, int y );
void goPicking( int x, int y, char *type );
void drawListsPick(void);
void qali(void);
void qbia(void);
void qnod(void);
void qcnt(void);
void qcut(void);
void qnor(void);
void qdis(void);
void qmsh(void);
void qenq(void);
void qflp(void);
void qfil( char *record);
void qint(void);
void qdiv(void);
void qadd( char *record);
void qcyc( char *record);
void qdel( void );
void qrem( char *record);
void qpnt( char *record);
void qlin( char *record);
void qsur( char *record);
void qbod( char *record);
void qseq( char *record);
void qshp( char *record);
void qspl(void);
void qtxt(void);
void moveText(int n, int x, int y);
void moveNode(int n, int x, int y);
void movePoint(GLuint *p, int x, int y);
int createFillet(int lin, double filletRadius);
int createElem(int n);
int createText( int nodenr, int x, int y );
int createPoint( int x, int y );
int normalLine(char *name, int *pnts, double l);
int createLine( char *pnt, int flag );
int splitSurf( int lmaster, int smaster );
int combineSurfs( int setNr );
void convertLCMB( int lcmb );
void convertLine( int line, int pickbuf );
int intersect(int lin);
int qsplitLine( int lin, int x, int y  );
void pre_align( int nr, int flag );
void updLcase(int l, int setNr);
void pre_cut( int nr, int flag );
void moveModel(void);
void updateDispLists(void);
void flip( char *type, int indx);

/* sendSet.h  */
void sendSurfNormalen( char *setname, char *format, Summen *anz, Nodes *node, Elements *e_enqire, Datasets *lcase , Scale *scale );
void sendQuadLin( char *setname, char *format, Summen *anz, Nodes *node, Elements *e_enqire );
void sendPressure( char *setname, char *format, Summen *anz, Nodes *node, Elements *e_enqire, char *val1, char *val2, char *val3, char *val4 );
void sendFilm( char *setname, char *format, Summen *anz, Nodes *node, Elements *e_enqire, char *val1, char *val2, char *val3, char *val4, char *val5 );
void sendRadiate( char *setname, char *format, Summen *anz, Nodes *node, Elements *e_enqire, char *val1, char *val2, char *val3, char *val4, char *val5 );
void sendCflux( char *setname, char *format, Summen *anz, Nodes *node, Elements *e_enqire, char *value, char *val2 );
void sendDflux( char *setname, char *format, Summen *anz, Nodes *node, Elements *e_enqire, char *val1, char *val2, char *val3, char type );
void sendSPC( char *setname, char *format, Summen *anz, Nodes *node, Elements *e_enqire, char *value, char *val1, char *val2, char *val3 );
void sendSPCF( char *setname, char *format, Summen *anz, Nodes *node, Elements *e_enqire, char *value, char *val1, char *val2, char *val3 );
void getNodeNormalen(int **sum_n, Nodes **norm, int setNr, Summen *anz, Faces *face);
void sendSliders( char *setname, char *format, Summen *anz, Nodes *node, Elements *e_enqire, char *spcType );
void sendSlidersf( char *setname, char *format, char *spcType );
void sendNames( char *setname, char *format, Summen *anz, Nodes *node, Elements *e_enqire );
void sendSurfaces( char *setname, char *format, Summen *anz, Nodes *node, Elements *e_enqire, char *val );
void sendForce( char *setname, char *format, Summen *anz, Nodes *node, Elements *e_enqire, double *f );

/* setFunktions.h  */
void iniSetFunktions(void);
void drawNodes_plot( int num, int *name, Nodes *node, int col, char type);
void drawElements_plot( int num, int *name, Nodes *node, double *colNr, Elements *e_enqire, int col, char type, int pickflag );
void drawElemNodes_plot( int num, int *name, Nodes *node, Elements *e_enqire, int col, char type );
void drawFaces_plot( int num, int *name, Nodes *node, double *colNr, Faces *face, int col, char type, int pickflag );
void drawFaceNodes_plot( int num, int *name, Nodes *node, Faces *face, int col, char type );
void drawPoints_plot( int num, int *name, Points *point , int col, char type);
void drawLines_plot( int num, int *name, Lines *line , Points *point, int col, char type);
void drawSurfs_plot( int num, int *name, Gsur *surf, Lcmb *lcmb, Lines *line , Points *point, int col, char type);
void drawBodys_plot( int num, int *name, Gbod *body, Gsur *surf, Lcmb *lcmb, Lines *line , Points *point, int col, char type);
void drawNurl_plot( int num, int *name, int col, char type, int pickflag);
void drawNurs_plot( int num, int *name, int col, char type, int pickflag);
void drawShapes_plot( int num, int *name, Shapes *shape, Points *point, int col, char type);
void setLightAndMaterial_rgb(int col);
int generateTetFromBody(int nr, double teth);
int generateTetFromSet(int setNr, double teth);
int getSetNr(char *setname);
int getAliasNr(char *name);
int getNodNr(Summen *anz,Nodes *node,int nodnr );
int getElemNr (int elnr );
int getE_enqNr (int elnr );
int getValuNr(char *name);
int getPntNr(char *name);
int getShapeNr(char *name);
int getLineNr(char *name);
int getLcmbNr(char *name);
int getSurfNr(char *name);
int getBodyNr(char *name);
int getNurlNr(char *name);
int getNursNr(char *name);
int getMatNr(char *matname, int checkFlag);
int getAmplitudeNr(char *ampname, int checkFlag);
int getIndex(int **ipnt, int n, int x0 );
void delSet( char *setname);
int seto( char *setname );
void setc( char *setname );
int rnam( int setNr, char *new_name);
int pre_seta( char *setname, char *type, char *name);
int seta( int setNr, char *type, int number);
int setr( int setNr, char *type, int number);
int seqr( int setNr, char *type, int number);
int seti( int setNr, char *type, int sets, char dat[MAX_PARAM_PER_RECORD][MAX_LINE_LENGTH]);
int sete( int setNr, char *type, char *mode);
void pre_del( char *record);
void pre_move( char *record);
void pre_move_pfast( char *record);
void free_proj(double *nx, double *nrad, int *nmissed  );
int genSplitTrias(int trgtNr, Tri **ptr_tri, int flag );
void pre_split( char *record);
void pre_proj( char *record);
void pre_proc( char *record);
int splitLine(int l, int *l_nr, int ps_nr, double ps_lbez);
void  generateSetIndexes();
int prnt(char *record);
int  zap(char *record);
void grpa( int *elemGroup, int nr, int setNr );
void mata( int *elemMat, int nr, int setNr );
char getLetter(int i);
int getFamName( int n, char *c );
int getNewName( char *name, char *type );
void pre_movie(char *string);
int commandoInterpreter( char *type, char *string, int na, int nb, FILE *handle1, int addFlag, int *flag );
void readfbd( char *datin, int addFlag );
void readfrdfile( char *datin, char *setname );
void pre_read( char *record );
void createSuperSets(void);
void pre_elty( char *record );
void pre_eqal( char *record );
void pre_mesh( char *record );
int completeSet_Mesh( int setNr, int setNrbuf, int *elUsed, int flag);
int completeSet_Lines( int setNr, int setNrbuf, int *elUsed, int flag);
int completeSet( char *setname, char *type );
int sendTriangles(  char *setname );
int write2netgenEdges(char *datout, Summen *anz, Nodes *node, Elements *elem, Datasets *lcase );
void pre_write( char *record );
void senddata( char *setname, char *format, Summen *anz, Nodes *node, Elements *e_enqire, Datasets *lcase , Scale *scale, int complete, char **val );
int calcCoefficientsTet(int nslav, int *emas, int n_closest_tets, Nodes *node, Tetraeder *tet, double cof[4], int extrapolflag);
int interpol3d(int set1, int set2, char *format, char *dataset, int extrapolflag);

/* sendMpc.c */
void sendMpc( char *setname, char *format, char *rotation, double *vector  );
void cycmpc(int set1, int set2, char *format, char *value, char *corr);
void areampc(int set1, int set2, char *format, char *type, char *value, char *corr, int offset, Nodes *nptr, int extrapolflag);
void gap(char *record);
int calcCoefficientsTri(  Nodes *node, int n0, CTri3 *ctri3, int  e, double *dist, double *c, double *n);
double find_tri3( Nodes *node, int n, CTri3 *ctri3, int e);
void corrNode(  Nodes *node, CTri3 *ctri3, int  e, double g, int n, double *vNneu );
void createMPCs();

/* contact.c */
typedef struct {
  int nn, nf;
  int *node;
  int *face;
} Cluster;



/* graph.c */
void param2D(char *par1, int *dsNr, char *par2 );
void plot2D(char *type, int setNr, int *dsNr, int entity );
void graph( char *record);

/* corrad.c */
void corrad( int setNr, double minradius);

/* badelems.c */
int calcBadElements(char *record);

/* write2tochnog.c */
int write2tochnog(char *datout, Summen *anz, Nodes *node, Elements *elem, Datasets *lcase );

/* readccx.c */
void delSetx( char *setname);
int pre_setax( char *setname, char *type, char *name);
int readccx(char *datin, Summen *anz, Sets **sptr, Nodes **nptr, Elements **eptr, Datasets **lptr);
int getFilePointer( int filesopen, FILE **openfile, char *rec_str);


double repairNurbs( Nurbs *nurbs, int nr, int deg, int dir);
int evalNurbsWithNormalVector( Nurbs *nurbs, int nr, int sum_p, double *pnt_u, double *pnt_v, Points *pnt, Points *nv);
int evalNurbs( Nurbs *nurbs, int nr, int sum_p, double *pnt_u, double *pnt_v, Points *pnt);
void cppfreearray(double *array);
int createBlendedNurbs(int nr, Points **pntpntr, Lines *line, Lcmb *lcmb, Gsur *surf );
void projSurfToNurbs( Nurbs *nurbs, int nr, Gsur *surf, int snr, Nodes *node );
void projSetToNurbs( Nurbs *nurbs, int nr, Sets *set, int setNr, Points *pnt);
double *projPntsToNurbs( Nurbs *nurbs, int nr, int anz_p, Points *pnt);
double proj1PntToNurbs( Nurbs *nurbs, int nr, double *pnt);
int trimNurbs( Nurbs *nurbs, int nr, int patch, double);


/* fillBody2.c */
int  fillSurf_ab( int j, int umax, int vmax, int *n_uv, int amax, int bmax, int *n_ab, int offs1, int offs2, double *x, double *y, double *z );
int  fillSurf_ba2( int sur, int *div_l, int div_a, int div_b, int sa1, int sa2, int sb1, int sb2, int *n_uv, int *umax, int *vmax, int *n_ba, int *amax, int *bmax,  int *offs_sa1, int *offs_sa2, double *x, double *y, double *z, double *x_ba, double *y_ba, double *z_ba );
int fillBody2( int b_indx, int *srefp, int **div_l, int *unbalance, int **nabc, int *amax, int *bmax, int *cmax, int *offs_sa1, int *offs_sa2, int *msur, int *div_sa1, int *div_sa2 );


/* meshSet.c */
void cornerNodes( int vmax, int umax, int j, int *n_uv );
int  newCornerPositions( int n, int u,int v,int umax, int vmax, double *lx,double *ly,double *lz,
 double *x,double *y,double *z, int flag );
int  fillSurf( int j, int *n_uv, int umax, int vmax, double *x, double *y, double *z );
int  fillSurf2( int sur, int *div_l, int div_a, int div_b, int sa1, int sa2, int sb1, int sb2,
 int *n_uv, int *umax, int *vmax, int *n_ba, int *amax, int *bmax, int *offs_sa1, int *offs_sa2,
 double *x, double *y, double *z );
void edgeNodes( int vmax, int umax, int j, int *n_uv );
int  newEdgePositions( int n, int u,int v,int umax, int vmax, double *lx,double *ly,double *lz, double *x,double *y,double *z, int flag );

double calcMeshDistance( Nurbs *nurbs, int nr, int np, Points *ppre, int e, Points *pcg, int *tri);
int triSplitter( Nurbs *nurbs, int nr, int *ptr_np,  Points **ptr_ppre, int *ptr_ne, int **ptr_tri, int **ptr_pntflag, int **edge_n2, int *pnt_indx, double **ptr_pntu, double **ptr_pntv, double max_ratio, double *tab_u, double *tab_lu, double *tab_bu, double *tab_v, double *tab_lv, double *tab_bv);
int checkMesh(int np, Points *ppre, int e, int *tri);
int meshPoints( int setNr );
int straightNodes( int j, int k, int d, double *pn );
int arcNodes( int j, int k, int d, double *pn );
int splineNodes( int j, int k, int d, double *pn );
int nurlNodes( int j, int k, int d, double *pn );
int meshLines( int setNr );
int newDivisions( int *div_l, int *div_a,int *div_b, int *sa1,int *sa2,int *sb1,int *sb2 );
int addTwoLines( int l1, char o1, char typ1, int l2, char o2, char typ2 );
int splitLineAtDivratio(int edge, int typ, double splitdiv, int *lnew, char  *typnew);
int mesh_tr3u(int nr );
int merge_nlocal( Rsort *rsort, Nodes *nlocal, Elements *ctri3, int anz_e, int anz_n, int indx, int lock, double tol );
int deleteFreeEdges(Nodes *node, int *snods, int anz_n, Elements *elems, int anz_e );
int mesh_tr3g( int s );
int determineBestCorners( int s, int *cl);
void *meshSurf( void *arg);
int meshSurfs( int setNr );
int fillBody( int b, int *srefp, int umax, int vmax, int wmax, int *n_uvw, double *x, double *y, double *z );
int getSurfDivs( int b, int s, int *div_l );
int meshImprover( int *etyp, int *nodes, int *elements, int *n_indx, int *n_ori, double **n_coord, int **e_nod, int *n_type, int **n_corner, int **s_div );
int bodyFrom5Surfs( int *b);
int bodyFrom7Surfs( int *b);
int meshBodys( int setNr );
void meshSet(char *setname, int blockFlag, int lonly, int projFlag, int meshoptFlag_length, int meshoptFlag_angle );


/* surfMesh.h */
int surfMesh(int *imax, int *jmax, double *x, double *y, double *z);

/*  bodyMesh2.h */
int bodyMesh2(int *wmax, int *vmax, int *umax, double *x, double *y, double * z);

/*  improveMesh.o */
int smooth_length( double *co, int *nk, int *kon, int *ne, int *iptr1, int *iptr2, int *nside, int *ineino, int *nkind, int *neigh, int *ndiv, double *pneigh, int *maxsumdiv, int *etyp, int *nori);
int smooth_angle( double *co, int *nk, int *kon, int *ne, int *iptr1, int *iptr2, int *nside, int *ineino, int *nkind, int *neigh, int *ndiv, double *pneigh, int *maxsumdiv, int *etyp, int *nori);
int smooth_midnodes( double *co, int *nk, int *kon, int *ne, int *iptr1, int *iptr2, int *nside, int *ineino, int *nkind, int *neigh, int *ndiv, double *pneigh, int *maxsumdiv, int *etyp, int *nori);

/* userFunction.c */
void userFunction(char *string, Summen   *sum, SumGeo   *sumGeo );

/* calcWeight.c */
#define real double
#define integer int

/* Subroutine */ int hexaeder_(integer *iel, integer *istat, real *x, real *y,
			       real *z__, real *v, real *xcg, real *ycg, real *zcg);
/* Subroutine */ int tetraeder_(integer *iel, integer *istat, real *x, real *y,
				real *z__, real *v, real *xcg, real *ycg, real *zcg);

int calcBadElements(char *setname);

/* contact.c */
int mergeCluster(Cluster *cluster, int c1, int c2);
int getMeshSections(int setNr, Faces *face, Nodes *node);
int getDepNodes( Summen *anz, int set1, int set2, Elements *e_enqire, Faces *face, Nodes *node, double tol);
int getFacePair( Summen *anz, int set1, int set2, Elements *e_enqire, Faces *face, Nodes *node, double tol, int *mpcset);

/* makeTriFromElems.c */
int makeTriFromElems(int setNr, int eset, int nmax, Sets *set, Elements *elem,  CTri3 **pctri3, int ***ptri3_index, int **pntri_nodes);

/* others */
int readstep( char *datin, int addFlag );
void copyDatasetToNodes(Summen *anz, Nodes *node, Datasets *lcase, int lc, CopiedNodeSets copiedNodeSets);
int pre_readfrdblock( CopiedNodeSets *copiedNodeSets, int lc, Summen *anz, Nodes *node, Datasets *lcase );
int surfToNurs(int s);
int coneToNurs(int s);
int sphToNurs(int s);
int surfToShape(int s);
void uncut(int surfFlagBuffer);
int crecord( char *rec_str, char **dat);
void descalAll(void);
int getSetNrx(char *name);
int setax( int setNr, char *type, int number);
int pre_setax( char *string, char *type, char *name);
