
#ifdef WIN32
  #include <windows.h>
  #undef near
  #undef max
  #undef min
  #define DEV_NULL  " "
  #define DEV_NULL2 " "
#else
  #define DEV_NULL   " >/dev/null"
  #define DEV_NULL2 " 2>/dev/null"
  #include <unistd.h>
#endif

#include <math.h>
#include <string.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <GL/gl.h>
#include <GL/glx.h>
#include <pthread.h>
#include <semaphore.h>

#ifdef WIN32
  #include <GL/glut.h>
  #define printf printf_fflush
  #ifdef __cplusplus
extern "C" {
  #endif
void printf_fflush(const char *fmt,...);
  #ifdef __cplusplus
}
  #endif
#else
  #include <GL/glut_cgx.h>
#endif

#define     PI          3.14159265358979323846264338327950288
#define     MAX_INTEGER 2147483647
#define     MAX_FLOAT   1.e32

#define     MAX_LINE_LENGTH 256
#define     NODES       1000000
#define     ELEMENTS    1000000


/* intpol2.c, spline.c */
#define     PNTS  10000

/* from #include "f2c.h" */
#define abs(x) ((x) >= 0 ? (x) : -(x))
#define dabs(x) (double)abs(x)
#if 0 // prool's dirty hack for ubuntu 16
#define min(a,b) ((a) <= (b) ? (a) : (b))
#define max(a,b) ((a) >= (b) ? (a) : (b))
#endif
#define smin(a,b) (float)min(a,b)
#define smax(a,b) (float)max(a,b)
#define dmin(a,b) (double)min(a,b)
#define dmax(a,b) (double)max(a,b)


typedef struct {
  char  model[MAX_LINE_LENGTH]; /* model-name header*/
  char  **uheader; /* user header */
  char  **pheader; /* project header (remark: leading dataset-project-headers are stored in the related dataset!) */
  int   v;         /* number of values */
  int   u;         /* number of user headers */
  int   p;         /* number of project headers */
  int   n;         /* number of nodes */
  int   e;         /* number of elements  */
  int   f;         /* number of faces */
  int   g;         /* number of edges */
  int   t;         /* number of texts */
  int   sets;      /* sets (groups) of entities */
  int   mats;      /* materials   */
  int   amps;      /* amplitudes  */
  int   l;         /* number of loadcases (Datasets) */
  int   b;         /* number of nodeBlocks */
  int   c;         /* number of 'cuts' over all nodeBlocks (block-to-block interfaces for isaac) */
  int   etype[100];/* number of elements of a certain type */
  int   nmax;      /* maximum nodenumber */
  int   nmin;      /* minimum nodenumber */
  int   emax;      /* maximum elemnumber */
  int   emin;      /* minimum elemnumber */
  int   orignmax;  /* max-node-nr-of-original-nodes (w/o nodes for drawing purposes) */
  int   orign;     /* nr-of-original-nodes (w/o nodes for drawing purposes) */
  int   olc;       /* nr-of-original-loadcases (w/o cgx generated datasets (lc)) */
  int   noffs;     /* node-nr offset, defined with asgn */
  int   eoffs;     /* elem-nr offset, defined with asgn */
} Summen;


typedef struct {
  int   nr;              /*   external node-nr (node[node-indx].nr) */
  int   indx;            /*   node-index (node[ext-node-nr].indx)   */
  char  pflag;           /*   1 if used for display purposes    */
                         /*  -1 if the node is deleted          */
                         /*   0 default                         */
  double nx;             /*   coordinates  node[ext-node-nr].nx */
  double ny;
  double nz;
  double nv[3];          /* normal vector */
} Nodes;


typedef struct {
  int nr;                /* external element-nr */
  // int indx;              /* -index (elem[external elem-nr].indx)   */
  int type;              /* element type (1:Hexa8)  */
  int group;
  int mat;
  int attr;              /* -1: unstructured mesh tr3u (-2 for mesh with libGLu tr3g ) */
                         /*  0: default           */
                         /*  1: reduced integration he8r he20r */
                         /*  2: incompatible modes he8i */
                         /*  3: DASHPOTA be2d */
                         /*  4: plane strain (CPE) tr3e tr6e qu4e qu8e */
                         /*  5: plane stress (CPS) tr3s */
                         /*  6: axisymmetric  (CAX) tr3c */
                         /*  7: fluid he8f */
                         /*  8: tet10m */
                         /*  14: reduced integration, plane strain (CPE)  */
                         /*  15: reduced integration, plane stress (CPS)  */
                         /*  16: reduced integration, axisymmetric  (CAX) */
  int nod[27];
  double **side;         /* side[Nr.][x|y|z]== normal vector */
} Elements;


typedef struct {
  int nr;                /* element-face-nr in Abaqus Format  (-1 for shell elements) */
  int indx[6];           /* face[elemnr].indx[side]==index of the face of that element of that side-nr   */
  int elem_nr;           /* reference to element number  */ 
  int type;
  int group;
  int mat;
  int nod[10];
  double **side;          /* normal vector: side[Nr.][x|y|z] */
} Faces;


typedef struct {
  int p1;
  int p2;
} Edges;


typedef struct {
  int node_nr;           /* reference to node number  */ 
  int nFlag;             /* 0: no node-nr */
  int vFlag;             /* 0: no value */
  int fFlag;             /* 0: e-format, 1: float, 2: int */
  double tx;             /*   window coordinates */
  double ty;
  double mx;             /*   model coordinates */
  double my;
  double mz;
} Texts;


/* for structured cfd meshes */
typedef struct {
  int dim;                /* 2: surf, 3: body */
  int i,j,k;              /* block dimension in i,j,k direction */
  int *nod;
  int geo;                /* related surf/body */
  int bcface[6];          /* index of the line/face of the related surf/body */
  int neighbor[6];        /* adjacent surface/body index */
  int map[6][3];          /* relative orientation of the neighbor 1:i==i_neigh, 4:i==-i_neigh */
  int strt1[6][3]; 
  int end_1[6][3]; 
  int strt2[6][3]; 
  int end_2[6][3];
  char bctype[6][MAX_LINE_LENGTH];          /* bondary condition type */
} NodeBlocks;


typedef struct {
  int nod[3];
  double ncol[3][3];
  int elem_nr;
  int group;
  int mat;
} CTri3;


typedef struct{
  int e;
  int f;
  int n;
  float *v;
} Elfaces;


typedef struct {
  char  name[MAX_LINE_LENGTH];
  double rho;                 /* *DENSITY */
  int    nela;                /* *ELASTIC */
  double *tela, *nue, *ela;
  int    nexp;                /* *EXPANSION */
  double *texp, *exp;
  int    ncon;                /* *CONDUCTIVITY */
  double *tcon, *con;
  int    nsph;                /* *SPECIFIC HEAT */
  double *tsph, *sph;
} Materials;


typedef struct {
  char  name[MAX_LINE_LENGTH];
  int    n;
  double *x, *y;
} Amplitudes;


typedef struct {
  char  **pheader;    /* project header */
  int   npheader;              /* number of headers */
  char  **compName;
  char  **icname;
  char  name[MAX_LINE_LENGTH];
  char  dataset_name[MAX_LINE_LENGTH];
  char  dataset_text[MAX_LINE_LENGTH];
  char  analysis_name[MAX_LINE_LENGTH];
  float value;
  char  filename[MAX_LINE_LENGTH];
  FILE *handle;
  fpos_t *fileptr;
  int   loaded;       /* if data are stored:1 else: 0 */
  int format_flag;
  int analysis_type;
  int step_number;
  int ncomps;         /* components of a result of an entity (node, gauspnt) */
  int irtype;
  int *menu;
  int *ictype;
  int *icind1;
  int *icind2;
  int *iexist;
  float **dat;        /* node related data */
  float ***edat;      /* element related data, not propper implemented */
  float *max;         /* maximum datum */
  float *min;         /* minimum datum */
  int *nmax;          /* node with maximum datum */
  int *nmin;          /* node with minimum datum */
} Datasets;


typedef struct {
  double w;                     /* scalierung in den Einheitswuerfel==max(xyzmax) */
  double x,y,z;                     /* Mittelpunktsversatz der unscalierten Vektoren */
  double smin, smax;                            /* max,min Werte der Scala  */
  double xmax, xmin, ymax, ymin, zmax, zmin;    /* max==|max|Abstand eines Punktes vom Zentrum */
  char format;                   /* representation, either f float, i int, e exp */
  char lock;                  /* !=0: locks the scale, no recalc of scale->smin, >smax */
} Scale;


typedef struct {
  char *name;
  char flag;                  /* if the set is open: 'o' else: 'c' */
  char type;                  /* ordered entities:1 (seq) or not: 0 (set) */ 
  int material;
  int index;           /* index of type-0 sets, assigned and updated in prnt(), eval. in getSetNr() */
  int anz_v;
  int anz_n;
  int anz_e;
  int anz_f;
  int anz_elf;
  int anz_p;
  int anz_l;
  int anz_c;
  int anz_s;
  int anz_b;
  int anz_nurl;
  int anz_nurs;
  int anz_se;
  int anz_sh;
  int *valu;
  int *node;
  int *elem;
  int *face;
  Elfaces *elf;
  int *pnt;
  int *line;
  int *lcmb;
  int *surf;
  int *body;
  int *nurl;
  int *nurs;
  int *set;
  int *shp;
  int etyp;
  int eattr;       /* -1:unstructured mesh, 0:default, 1:reduced integration, 2:incompatible modes */
  int eseq;        /* remember the order of the elty commands */
  char *eparm;
} Sets;


typedef struct {
  int nr;
  int *set;
} OpenSets;


/* compiles the actual displayed sets */
typedef struct {
  int nr;
  char type[MAX_LINE_LENGTH];
  int col;
} Psets;


/* sum of char of asci letters */
typedef struct {
  int  max_suma;                   /* maximum value of sum_ascii of aliases stored  */
  int  *anza;                      /* nr of aliases stored */
  int  **aindx;                    /* alias indexes       */
  int  max_sumv;                   /* maximum value of sum_ascii of values stored  */
  int  *anzv;                      /* nr of values stored */
  int  **vindx;                    /* value indexes       */
  int  max_sump;                   /* maximum value of sum_ascii of points stored  */
  int  *anzp;                      /* nr of points stored */
  int  **pindx;                    /* point indexes       */
  int  max_suml;                   /* maximum value of sum_ascii of lines stored  */
  int  *anzl;                      /* nr of lines stored  */
  int  **lindx;                    /* line indexes        */
  int  max_sumc;                   /* maximum value of sum_ascii of lcmbs stored  */
  int  *anzc;                      /* nr of lcmbs stored  */
  int  **cindx;                    /* lcmb indexes        */
  int  max_sums;                   /* maximum value of sum_ascii of surfs stored  */
  int  *anzs;                      /* nr of surfs stored  */
  int  **sindx;                    /* surf indexes        */
  int  max_sumb;                   /* maximum value of sum_ascii of bodies stored  */
  int  *anzb;                      /* nr of bodys stored  */
  int  **bindx;                    /* body indexes        */
  int  max_sumS;                   /* maximum value of sum_ascii of nurs stored  */
  int  *anzS;                      /* nr of Nurs stored  */
  int  **Sindx;                    /* nurs indexes        */
  int  max_sumse;                  /* maximum value of sum_ascii of sets stored  */
  int  *anzse;                     /* nr of sets stored  */
  int  **seindx;                   /* set indexes        */
  int  max_sumsh;                  /* maximum value of sum_ascii of shapes stored  */
  int  *anzsh;                     /* nr of shapes stored  */
  int  **shindx;                   /* shape indexes        */
  int  max_sumamp;                 /* maximum value of sum_ascii of amplitudes stored  */
  int  *anzamp;                    /* nr of amplitudes stored  */
  int  **ampindx;                  /* amplitudes indexes        */
  int  max_summat;                 /* maximum value of sum_ascii of materials stored  */
  int  *anzmat;                    /* nr of materials stored  */
  int  **matindx;                  /* materials indexes        */
} SumAsci;


typedef struct {
  char  model[MAX_LINE_LENGTH]; /* haeder model */
  int   alias;                           /* alias-names     */
  int   p;                               /* points */
  int   l;                               /* lines  */
  int   c;                               /* lcmbs  */
  int   s;                               /* surfs  */
  int   b;                               /* bodys  */
  int   sh;                              /* shapes  */
  int   nurl;                            /* nurbs lines     */
  int   nurs;                            /* nurbs surfaces  */
  int   psets;                           /* actal displayed sets */ 
} SumGeo;


typedef struct{
  char   *name;
  char   *entityName;
} Alias;


typedef struct {
  char  *name;
  int type;              /* 0:plane, 1:cyl, 2: cone, 3:sph, 4:nurbs */
  int p[7];              /* 0:3p, 1:3p, 2:4p 3:7p 4:p[0]=nurbsindx */
  GLint   npgn;           /* size of feed-back-Buffer pgn */
  GLdouble *pgn;           /* stores poligons which defines the interiour */  
} Shapes;


typedef struct {
  char  *name;
  char  *string;
} Values;


typedef struct {
  char  *name;
  double px;     /* coordinates will be scaled during run-time, scalPoints() */
  double py;
  double pz;
  int   nn;
  int   *nod;
} Points;


typedef struct {
  char  *name;
  char  typ;      /* straight:' ', arc:'a', spline:'s', nurbs:'n' */
  int   p1;
  int   p2;
  int   trk;      /* if arc: 3.pnt, if spline: setname, if nurbs: nurblname */
  int   div;
  double bias;
  int   nip;
  double *ip;
  int   fail;             /* if 1 then the meshing failed */
  int   nn;
  int   *nod;
  int   ne;
  int   *elem;
  int  etyp;
  int  eattr;       /* -1:unstructured mesh, 0:default, 1:reduced integration, 2:incompatible modes */
} Lines;

typedef struct {
  char  *name;
  char  nl;
  char  *o;      /* l-orient +- */
  int   *l;               /* lines */
  int   p1;               /* starting point */
  int   p2;               /* end point */
  double cx;               /* CG, not used so far */
  double cy;
  double cz;
} Lcmb;

typedef struct {
  char  *name;
  char  ori;     /* surface-orientation +- */
  int   sh;               /* embedded shape  */
  char   sho;    /* embedded shape (nurs) orientation  */
  int   nl;               /* corners (either line or lcmb) */
  char  *typ;    /* type: l=line c=lcmb */
  char  *o;      /* l-orient +- */
  int   *l;               /* index of lines, lcmbs, also used to store the index of a substitute surf (last item) */
  int   *cp;              /* index of points at the junction between  lines, lcmbs, starting at the beginning of the first line */
  int   nc;               /* number of closed-line-loops (curves) */
  int   *c;               /* number of lines in each curve */
  double cx;               /* CG, in orient determined, not scaled during run! */
  double cy;
  double cz;
  int   fail;             /* if 1 then the meshing failed, if 2 trimming had failed */
  int   nn;
  int   *nod;
  int   ne;
  int   *elem;
  int   etyp;
  int   eattr;       /* -1:unstructured mesh, 0:default, 1:reduced integration, 2:incompatible modes */
  char  *eparm;      /* parameter, so far used to store the mesh-density requirement for the mesher */
  int   patch;            /* number of trimming patch in the related nurbs */
  GLint   npgn;           /* size of feed-back-Buffer pgn */
  GLdouble *pgn;           /* stores poligons of the trimmed nurbs which defines the interiour */
} Gsur;

typedef struct {
  char  *name;
  char  ori;     /* body-orientation +- */
  int   ns;               /* nr. of surfaces (currently must be 6 for meshing) */
  char  *o;      /* s-orient +- */
  int   *s;               /* surfaces */
  double cx;               /* CG, in orient determined, not scaled during run! */
  double cy;
  double cz;
  int   fail;             /* if 1 then the meshing failed */
  int   nn;
  int   *nod;
  int   ne;
  int   *elem;
  int   etyp;
  int  eattr;       /* -1:unstructured mesh, 0:default, 1:reduced integration, 2:incompatible modes */
  char  *eparm;
} Gbod;

typedef struct {
  char  *name;
  char  endFlag;
  GLUnurbsObj *Nurb;
  GLint   u_npnt;
  GLint   u_exp;
  GLint   u_nknt;
  GLfloat *uknt;
  GLint   u_stride;
  GLfloat *ctlarray;
  GLfloat *weight;
  GLenum  type;
  GLint   *ctlpnt;
} Nurbl;

typedef struct {
  char  *name;
  char  endFlag;
  GLUnurbsObj *Nurb;
  GLint   u_npnt;
  GLint   u_exp;
  GLint   u_nknt;
  GLint   v_npnt;
  GLint   v_exp;
  GLint   v_nknt;
  GLfloat *uknt;
  GLfloat *vknt;
  GLint   u_stride;
  GLint   v_stride;
  GLfloat *ctlarray;
  GLenum  type;
  GLint   **ctlpnt;
  GLfloat **weight;

  /* additional values for rendering purposes */
  int trimFlag;      /* 1: is trimmed, 0: new trimming necessary (not used so far) */
  double ures, vres;    /* realworldlength(xyz)/nurbslength(uv) */
  GLfloat *ustep, *vstep;    /* division per u or v for trimmed plotting */
  int patches;        /* number of trimming patches (separate surfaces)*/
  int *nc;             /* number of trimming curves */
  int **np;            /* number of points in each trimming curve */
  GLfloat ***uv;         /* u,v coordinates of the spline-points of the trimming curves */
  double ***xyz;        /* x,y,z coordinates of the spline-points of the trimming curves */
  double tx, ty, tz;   /* average position for the name-string */
  double *umax, *vmax;   /* max val for u,v, for rendering purposes */

  /* additional values for meshing purposes (needed for tr6u) */
  int nurbsType;            /* 0plate, 1cyl, 2torus, 3ball, 4half-ball-bot, 5half-ball-top */
  int **sum_ambiguousPnts;  /* sum of double points in uv-space in each trimming curve */
  int **uvflipped;          /* 1 if the uv loop is reversed in a trimming curve*/
} Nurbs;



typedef struct {
  double alpha, beta;
  int nadapt;
} Meshp;

typedef struct {
  char name[MAX_LINE_LENGTH];
  char bctype[MAX_LINE_LENGTH];
  int surfs;
  int *surf;
} Dunsbou;


typedef struct {
  int e;          /* mother element nr */
  int n[4];
  double cg[3];   /* center of grav */
  double v;       /* volume */
} Tetraeder;


void adjustMidsideNode(double *P1, double *P2, double *Pm, int method);
int strsplt( char *rec_str, char breakchar, char ***ptr);
int strfind (char *as1, char *as2);
int readEdges( char *datin, Summen *anz, Nodes **nptr, Elements **eptr );
int *innerFacesHe8(Elements *elems, int numElems, Faces **ptr);

void calcElemNorm_quad4(int i, int n1, int n2, int n3, int n4, int f, Nodes *node, Elements *elem );
void calcElemNorm_tri3(int i, int n1, int n2, int n3, int f, Nodes *node, Elements *elem );
void calcFaceNorm_quad4(int i, int n1, int n2, int n3, int n4, int f, Nodes *node, Faces *face);
void calcFaceNorm_tri3(int i, int n1, int n2, int n3, int f, Nodes *node, Faces *face );
void getElemNormalen( Elements *e_enqire, Nodes *node, int elems );
void getFaceNormalen( Faces *face, Nodes *node, Summen *anz );


int renumberfrd( int firstelem, int firstnode, Summen *anz, Nodes **nptr, Elements **eptr, Datasets **lptr, int **enew_ptr, int **nnew_ptr  );

void define_rgb(float v, float *r, float *g, float *b);
void text(double x, double y, double z,char *msg, void *glut_font);
void scala_rgb( double dx, double dy, int divisions, double bmin, double bmax, double *col, void *glut_font, char format);
void scala_indx( double dx, double dy, int divisions, double bmin, double bmax, int offset, int col, void *glut_font, char format);
void scala_tex( double dx, double dy, int divisions, double bmin, double bmax, double scale, double *col, void *glut_font, char format);
int button(double dx, double dy, char *msg, double mx, double my, void *glut_font);
void polymark ( int n, double *col_r, double *col_g, double *col_b, double *x,
		double *y, double *z );

double nullstelle(double xmin, double xmax, double funktion(double), double *result);
int  calcPrinc( double *s, double *p, double *a1, double *a2, double *a3, int sortFlag );
int calcPvector( double *s, double *p, double *a );
void stopClock(int zaeler);
void bsort (double *wert, double *wertsort, int *isort, int anzahl);
int *bsort2(double *wert, int anzahl, int n );
int *bsortf(double *wert, int anzahl, int n );
int *bsortfp(double *wert, int anzahl, int n );
int *bsorti( int *wert, int anzahl, int n );
int compare (char *str1, char *str2, int length);
int compareStrings (char *str1, char *str2);
int elemChecker(int sum_e, Nodes *node, Elements *elem);
int frecord( FILE *handle1,  char *string);
int getGeoDataTria( double *p1, double *p2, double *p3, double *Ix, double *Iy, double *Ixy,
                double *A, double *pcg);
int getrecord (int bufstart, int bufsize, char *inputdata, char *buffer);
int gl3grades( long double a, long double b, long double c, double *x);
int AsplitA( double *pa1, double *pa2, double *pa3, double *pb1, double *pb2, double *pb3, double *ps1, double *ps2);
double AsplitL( double *b, double *eu, double *ev, double *eg );
double interpol(double *x, double *y, int nn, double x0);
double intpol2(double *x, double *y, int n, double x0, int *method );
double intpol3(double *x, double *y, int n, double x0, int *method, double s, int iopt );
double intpol(double *x, double *y, int n, double x0 );
int nearNodes(double *po, int *n, double *p, int *node, int *k, int *flag);
int iinsert(int **ipnt, int n, int x0 );
int ifind(int **ipnt, int n, int x0 );
int iremove(int **ipnt, int n, int x0 );
void linelength(double *x, double *y, double *z, int n, double *s );
int kbrecord( char *string);
int p3_finder(int anzahl_nodes, double *nx, double *nz, int *p);
int parser( char gkey, char *record, int *curshft);
void freeDataset(Datasets *lcase, int nr);
int readAnsys(char *datin, Summen *anz, Nodes **nptr, Elements **eptr, Datasets **lptr, double phi, int frequency );
int readDyna( char *datin, Summen *anz, Nodes **nptr, Elements **eptr, Datasets **lptr );
int readfrd(char *datin, Summen *anz, Nodes **nptr, Elements **eptr, Datasets **lptr, int read_mode );
int readfrdblock( int lc, Summen *anz,   Nodes     *node, Datasets *lcase );
int readOneNode( int lc, Summen *anz, Datasets *lcase, int nodenr, double **vptr, long *offset );
int read2frd(char *datin, Summen *anz, Nodes **nptr, Elements **eptr, Datasets **lptr );
int readFElt(char *datin, Summen *anz, Nodes **nptr, Elements **eptr, Datasets **lptr );
int readDuns(char *datin, Summen *anz, Nodes **nptr, Elements **eptr, Datasets **lptr, int elem_type );
int readIsaac(char *datin, Summen *anz, Nodes **nptr, Elements **eptr, Datasets **lptr, NodeBlocks **bptr, int elem_type,  double A, double B, double C );
int readGiff( char *datin, Summen *anz, Nodes **nptr, Elements **eptr, Datasets **lptr );
int readNastran( char *datin, Summen *anz, Nodes **nptr, Elements **eptr, Datasets **lptr );
int readNG( char *datin, Summen *apre, Sets **sptr, Nodes **nptr, Elements **eptr, Datasets **lptr );
int readStl( char *datin, Summen *anz, Nodes **nptr, Elements **eptr, Datasets **lptr );
int rotiere(double *x, double *y, int n, double phi, double *xneu, double *yneu);
int checkIfNumber( char *string );
int srecord( char *handle1,  char *string);
double stof(char *string, int a, int b);
int stoi(char *string, int a, int b);
void stos(char *string, int a, int b, char *puffer);
int sins(char *string, int a, int b, char *puffer);
int sword( char *string, char *word);
double p_angle(double x, double y);
void v_add( double *A, double *B, double *C );
double v_betrag(double *a);
void  v_matmult(double *v, double *m);
double v_norm( double *A, double *C );
void v_prod( double *A, double *B, double *C );
void v_result( double *A, double *B, double *C );
int v_rot(double fi, double *p0, double *v, double *pin, double *pout);
void v_scal( double *A, double *B, double *C );
void v_scalf( float *A, double *B, double *C );
double v_sprod( double *a, double *b);
double v_angle( double *v0, double *v1 );
double v_angle_ref( double *v0, double *v1, double *en );
double v_distA( double *N0, double *N1, double *N2, double *N, double *vray, double  triScale, int *orient);
int  v_rec2cyl( double *pr, int axis, int *csys, double *pc );
int v_sgg( double *p1, double *p2, double *e1s, double *e2s, double *ps);

void vl_add( double *A, double *B, double *C );
double vl_betrag(double *a);
void  vl_matmult(double *v, double *m);
double vl_norm( double *A, double *C );
void vl_prod( double *A, double *B, double *C );
void vl_result( double *A, double *B, double *C );
void vl_scal( double *A, double *B, double *C );
double vl_sprod( double *a, double *b);
double vl_angle( double *v0, double *v1, double *v2, double *scg, double *bcg );
void  m_copy(double *s, double *m);
void  m_print( double *m);

int m_prod(int *n,double *a,double *b,double *c);
int m_prodtr(int *n,double *a,double *b,double *c);

void  m_sub(double *ms, double *m, double *s);
int write2frd(char *datout, Summen *anz, Nodes *node, Elements *elem, Datasets *lcase, int binFlag );
int write2nas(char *datout, Summen *anz, Nodes *node, Elements *elem, Datasets *lcase );
int write2aba(char *datout, Summen *anz, Nodes *node, Elements *elem, Datasets *lcase, char **dat );
int write2ansys(char *datout, Summen *anz, Nodes *node, Elements *elem, Datasets *lcase );
int write2aster(char *datout, Summen *anz, Nodes *node, Elements *elem, Sets *set, Datasets *lcase ); //TODD
int write2darwin( char *datout, Summen *anz, Nodes *node, Elements *elem, Datasets *lcase, char **dat );
int write2samcef(char *datout, Summen *anz, Nodes *node, Elements *elem, Datasets *lcase );
int write2isaac( char *datout, Summen *anz, Nodes *node, Elements *elem, Datasets *lcase, NodeBlocks *nBlock );
int write2duns( char *datout, Summen *anz, Nodes *node, Elements *elem, Datasets *lcase, NodeBlocks *nBlock, int bouNr, Dunsbou *dunsbou );



void readStdCmap( Display **ptr_dpy, int *ptr_dpycells, Colormap *ptr_cmap, XColor **ptr_c,
                 unsigned long **ptr_pix, unsigned int *ptr_npixels, int anzCells );

/*selectFaces.c */
int selectDisplayFacesHe8 (Elements *elems, int numElems, int **pfaces, int *);
int selectDisplayFacesHe20 (Elements *elems, int numElems, int **pfaces, int *);
int selectDisplayFacesTet4 (Elements *elems, int numElems, int **ptr, int *edges);
int selectDisplayFacesTet10 (Elements *elems, int numElems, int **ptr, int *edges);
int selectDisplayFacesPe6 (Elements *elems, int numElems, int **ptr, int *edges);
int selectDisplayFacesPe20 (Elements *elems, int numElems, int **ptr, int *edges);
int commonEdge3 (int *a, int *b);
int findCTri3Edges(Elements *elems, int numElems, int **edges);
int findCTri6Edges(Elements *elems, int numElems, int **edges);
int findCQuad4Edges(Elements *elems, int numElems, int **edges);
int findCQuad8Edges(Elements *elems, int numElems, int **edges);
int compareFaces (int *a, int *b);
int compareFaces3 (int *a, int *b);
int compareFaces4 (int *a, int *b);

double spline_int( int nc, double xneu, double **c );
void createSpline( int n, double **c);
int read2fbd( char *datin, SumGeo *anz, Points **pntptr, Lines **lineptr, Lcmb **lcmbptr, Gsur **surptr, Gbod **bodptr, Nurbs **nursptr, Sets *set );

void shape4q(double xi, double et, double *xl, double *xsj);
void shape6tri(double xi, double et, double *xl, double *xsj);
void shape8q(double xi, double et, double *xl, double *xsj);
int  attach_new(double *coords,double *conode,int *i,double *ratio,double *dist,double *elemcoords);
int distattach_new(double *xig, double *etg, double *pneigh, double *pnode, double *a, double *p, double *ratio, int *nterms);
int attachhe20_(double *co, int *nk, int *neigh, int *node, double *xi_loc__,double *et_loc__);
int attachhe8_(double *co, int *nk, int *neigh, int *node,double *xi_loc__,double *et_loc__);
int distattachhe20_(double *xi,double *et,double *pneigh,double *pnode,double *dist,double *p);
int distattachhe8_(double *xi,double * et, double *pneigh, double *pnode, double *dist, double *p);
int shapefhe20_(double *xi, double *et, double *coef);
int shapefhe8_(double *xi, double *et, double *coef);

int mesh2d(int *_nt, int _nb, int *npc, double **_pnt_u, double **_pnt_v, int **_pnt_flag, int **_tri3, double _alpha, double _beta, int _nadapt);

void errMsg(char *msg, ...);

void transformatrix( double *xab, double *p, double **a);
void rectcyl(int icntrl, double *csab, int nr, Nodes *node, Datasets *lcase, int lc, char type);

void shapeHe8(double xi,double et,double ze, double xl[20][3],double *xsj,double *shp,int iflag);
void shapeHe20(double xi,double et,double ze, double xl[20][3],double *xsj,double *shp,int iflag);
void shapeTet10(double xi,double et,double ze, double shp[20]);
int shape8h_(double *xi, double *et, double *ze, double *xl, double *xsj);
int shape20h_(double *xi, double *et, double *ze, double *xl, double *xsj);
int shape10tet_(double *xi, double *et, double *ze, double *xl, double *xsj);
int e_c3d_volu_(double *xl,char *elty, double *volu, double *cg );
int e_c3d__(double *xl,char *elty);
int e_c3d_nodes_(double *xl,char *elty, int *elem, double *eqal);
int ident(double *x,double px,int n);
void near3d(double *xo,double *yo,double *zo,double *x,double *y,double *z,int *nx,int *ny,int *nz,double xp,double yp,double zp,int n,int *node,int k);
int splitElementsToTets(int anz_e, Nodes *node, Elements  *e_enqire, Tetraeder **ptet);

int delaun_( int *numpts, int *maxtri, double *smalld, double *x, double *y, int *list, int *pointr, int *v1, int *v2, int *v3, int *numtri);
void cartcyl( double *csab, int node, double *node_pos, Datasets *lcase, int lc, char type );
