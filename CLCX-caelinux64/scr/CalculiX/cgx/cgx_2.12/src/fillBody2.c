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

#define INTERPOL_MODE 0

#define TEST 0
#define TEST1 0

extern int printFlag;

extern Scale     scale[1];
extern Summen    anz[1];
extern Nodes     *node;
extern Elements  *e_enqire;
extern Datasets *lcase;

extern Sets      *set;                     /* sets */
extern Points    *point;
extern Lines     *line;
extern Lcmb      *lcmb;
extern Gsur      *surf;
extern Gbod      *body;
extern Nurbs     *nurbs;
extern SumGeo    anzGeo[1];

/* prliminary nodes, will be deleded after the elements are created */
extern Summen    apre[1];
extern Nodes     *npre;


/**********************************************************************************/
/* Fuellt ein xyz-feld mit den koordinaten einer Flaeche im ba raum, ausgehend    */
/* von daten im uv raum.                                                          */
/*                                                                                */
/*                                                                                */
/*                                                                                */
/*                                                                                */
/* in:                                                                            */
/* sur      surface-index                                                         */
/* div_l    feld mit den divisions der surface-edges                              */
/* div_a    ersatzdivision der 'a'-edges der surf.                                */
/* div_b    ersatzdivision der 'b'-edges der surf.                                */
/* sa1,sa2,sb1,sb2  Zuordnung der original-edges zu den a&b-edges                 */
/* offs_sa1    div_a-div_l[sa1], zur elementerzeugung notwendig                   */
/* offs_sa2    div_a-div_l[sa2]                                                   */
/*                                                                                */
/* out:                                                                           */
/* n_uv        nodeNr im uv-feld, die zusaetzlichen ersatz-stuetzpunkte sind      */
/*             nicht mit nodes belegt da sie bei der Vernetzung rausfallen        */
/* umax, vmax  anzahl der ersatz-stuetzpunkte in 'u' und 'v' bzw entlang edge     */
/*             '0' und '3' ( umax = div_(a|b) +1 )                                */
/* n_ba        nodeNr im ba-feld, siehe n_uv                                      */
/* amax, bmax  anzahl der ersatz-stuetzpunkte in 'a' und 'b'                      */
/*                         ( amax = div_a +1 )                                    */
/* x,y,z       f(u,v) aus dem surfmesher, alle u,v positionen sind belegt         */
/* x_ba..      f(b,a) aus dem surfmesher, alle b,a positionen sind belegt         */
/*                                                                                */
/*                                                                                */
/**********************************************************************************/
int  fillSurf_ba1( int sur, int *div_l, int div_a, int div_b, int sa1, int sa2, int sb1, int sb2, int *n_uv, int *n_ba,  int offs1   , int offs2   , double *x, double *y, double *z, double *x_ba, double *y_ba, double *z_ba )
{
  int n, u,v, b=0,a=0;
  int nodnr;
  double *lx, *ly, *lz;
  int flag;
  int amax, bmax, umax, vmax, sumax, svmax;

  int k;

      bmax=div_b+1;
      amax=div_a+1;
      if ((sa1==0)||(sa1==2))
      {
        umax=amax    ;
        vmax=bmax    ;
      }
      else
      {
        umax=bmax    ;
        vmax=amax    ;
      }
      sumax=div_l[0]+1;
      svmax=div_l[3]+1;
      edgeNodes( svmax, sumax, sur, n_uv );
      k=0;
#if TEST
      for (u=0; u<sumax; u++)
      {
        for (v=0; v<svmax; v++)
	{
          printf("%d  n(%d,%d):%d  n:%d\n",k,u,v,n_uv[u*svmax+v], n_uv[k]); k++;
	}
      }
#endif

      /* die Randknoten sind nun bekannt. es muessen nun ersatzkoordinaten */
      /* mit dem spacing der ersatzdivisions berechnet werden. dazu wird  */
      /* in einem feld aus koordinate(x,y oder z) und der lauflaenge s int-*/
      /* erpoliert.                                                        */

      /* side 0 */
      n=div_l[0]+1;        /* anzahl von original-nodes auf der line */
      if (((lx = (double *)malloc( (n)*sizeof(double)) ) == NULL )||
          ((ly = (double *)malloc( (n)*sizeof(double)) ) == NULL )||
          ((lz = (double *)malloc( (n)*sizeof(double)) ) == NULL ))
      { errMsg(" ERROR: realloc failure in fillSurf_ba()\n");    return(-1); }
      v=0;
      for (u=0; u<n; u++)
      { 
        nodnr=n_uv[u*svmax +v];
        lx[u]=npre[nodnr].nx;
        ly[u]=npre[nodnr].ny;
        lz[u]=npre[nodnr].nz;
      }
      v=0;
      u=0;
      flag=0; /* v const */
      if( newEdgePositions( n, u,v, umax,vmax, lx,ly,lz, x,y,z, flag) <0) return(-1);
      free(lx);free(ly);free(lz); 

      /* side 1 */
      n=div_l[3]+1;
      if (((lx = (double *)malloc( (n)*sizeof(double)) ) == NULL )||
          ((ly = (double *)malloc( (n)*sizeof(double)) ) == NULL )||
          ((lz = (double *)malloc( (n)*sizeof(double)) ) == NULL ))
      { errMsg(" ERROR: realloc failure in fillSurf_ba()\n");   return(-1); }
      u=div_l[0];
      for (v=0; v<n; v++)
      { 
        nodnr=n_uv[u*svmax +v];
        lx[v]=npre[nodnr].nx;
        ly[v]=npre[nodnr].ny;
        lz[v]=npre[nodnr].nz;
      }
      u=umax-1;
      v=0;
      flag=1; /* u const */
      if( newEdgePositions( n, u,v, umax,vmax, lx,ly,lz, x,y,z, flag) <0) return(-1);
      free(lx);free(ly);free(lz); 

      /* side 2 */
      n=div_l[0]+1;
      if (((lx = (double *)malloc( (n)*sizeof(double)) ) == NULL )||
          ((ly = (double *)malloc( (n)*sizeof(double)) ) == NULL )||
          ((lz = (double *)malloc( (n)*sizeof(double)) ) == NULL ))
      { errMsg(" ERROR: realloc failure in fillSurf_ba()\n");   return(-1); }
      v=div_l[3];
      for (u=0; u<n; u++) 
      { 
        nodnr=n_uv[u*svmax +v];
        lx[u]=npre[nodnr].nx;
        ly[u]=npre[nodnr].ny;
        lz[u]=npre[nodnr].nz;
      }
      v=vmax-1;
      u=0;
      flag=0; /* v const */
      if( newEdgePositions( n, u,v, umax, vmax, lx,ly,lz, x,y,z, flag) <0) return(-1);
      free(lx);free(ly);free(lz); 

      /* side 3 */
      n=div_l[3]+1;
      if (((lx = (double *)malloc( (n)*sizeof(double)) ) == NULL )||
          ((ly = (double *)malloc( (n)*sizeof(double)) ) == NULL )||
          ((lz = (double *)malloc( (n)*sizeof(double)) ) == NULL ))
      { errMsg(" ERROR: realloc failure in fillSurf_ba()\n");    return(-1); }
      u=0;
      for (v=0; v<n; v++)
      { 
        nodnr=n_uv[u*svmax +v];
        lx[v]=npre[nodnr].nx;
        ly[v]=npre[nodnr].ny;
        lz[v]=npre[nodnr].nz;
      }
      u=0;
      v=0;
      flag=1; /* u const */
      if( newEdgePositions( n, u,v, umax,vmax, lx,ly,lz, x,y,z, flag) <0) return(-1);
      free(lx);free(ly);free(lz); 

      surfMesh( &vmax, &umax, x, y, z);

      /* umspeichern der rand-nodes ins ba system */
      k=0;
      for (u=0; u< div_l[0]+1; u++)
      {
        for (v=0; v< div_l[3]+1; v++)
        {
        nodnr=n_uv[u*svmax +v];
        if ((sb1==0)&&(sa1==3)) { b=u+ offs1            ; a=v                    ; }
        if ((sb1==3)&&(sa1==2)) { b=vmax-v-1 - offs2   ; a=u                    ; }
        if ((sb1==2)&&(sa1==1)) { b=umax-u-1 - offs2   ; a=vmax-v-1            ; }
        if ((sb1==1)&&(sa1==0)) { b=v+ offs1            ; a=umax-u-1            ; }
        if ((sb1==0)&&(sa1==1)) { b=umax-u-1 - offs2   ; a=v                    ; }
        if ((sb1==3)&&(sa1==0)) { b=v+ offs1            ; a=u                    ;  }
        if ((sb1==2)&&(sa1==3)) { b=u+ offs1             ; a=vmax-v-1            ;  }
        if ((sb1==1)&&(sa1==2)) { b=vmax-v-1 - offs2   ; a=umax-u-1            ; }
        n_ba[b* amax +a]=nodnr;
#if TEST
  printf(" Fall  b:%d a:%d\n", b, a);
#endif
        }
      }

      /* umspeichern der surf-nodes ins ba system */
      k=0;
      for (u=1; u< div_l[0]; u++)
      {
        for (v=1; v< div_l[3]; v++)
        {
        if ((sb1==0)&&(sa1==3)) { b=u+ offs1            ; a=v                ;}
        if ((sb1==3)&&(sa1==2)) { b=vmax-v-1 - offs2   ; a=u                  ;}
        if ((sb1==2)&&(sa1==1)) { b=umax-u-1 - offs2   ; a=vmax-v-1          ;}
        if ((sb1==1)&&(sa1==0)) { b=v+ offs1            ; a=umax-u-1          ;}
        if ((sb1==0)&&(sa1==1)) { b=umax-u-1 - offs2   ; a=v                  ;}
        if ((sb1==3)&&(sa1==0)) { b=v+ offs1            ; a=u                ;}
        if ((sb1==2)&&(sa1==3)) { b=u+ offs1             ; a=vmax-v-1        ;}
        if ((sb1==1)&&(sa1==2)) { b=vmax-v-1 - offs2   ; a=umax-u-1          ;}
        n_ba[b* amax +a]=surf[sur].nod[k]; k++;
        x_ba[b* amax +a]=x[u* vmax +v];
        y_ba[b* amax +a]=y[u* vmax +v];
        z_ba[b* amax +a]=z[u* vmax +v];
#if TEST
  printf(" Fall b:%d a:%d\n", b, a);
#endif
        }
      }

      /* umspeichern aller koordinaten ins ba system */
      for (u=0; u< umax; u++)
      {
        for (v=0; v< vmax; v++)
        {
        if ((sb1==0)&&(sa1==3)) { b=u                   ; a=v         ;}
        if ((sb1==3)&&(sa1==2)) { b=vmax-v-1           ; a=u           ;}
        if ((sb1==2)&&(sa1==1)) { b=umax-u-1           ; a=vmax-v-1   ;}
        if ((sb1==1)&&(sa1==0)) { b=v                   ; a=umax-u-1   ;}
        if ((sb1==0)&&(sa1==1)) { b=umax-u-1           ; a=v           ;}
        if ((sb1==3)&&(sa1==0)) { b=v                   ; a=u         ;}
        if ((sb1==2)&&(sa1==3)) { b=u                   ; a=vmax-v-1   ;}
        if ((sb1==1)&&(sa1==2)) { b=vmax-v-1           ; a=umax-u-1   ;}
        x_ba[b* amax +a]=x[u* vmax +v];
        y_ba[b* amax +a]=y[u* vmax +v];
        z_ba[b* amax +a]=z[u* vmax +v];
#if TEST
  printf("u:%d v:%d  x_ba[%d][%d]=%lf\n", u, v, b, a, x_ba[b* amax +a]);
#endif
        }
      }


  return(1);
}


/**********************************************************************************/
/* Fuellt ein xyz-feld mit den koordinaten einer Flaeche bei der gegenueber       */
/* liegende seiten ungleiche divisions haben.                                     */
/* Die erforderlichen divisions muessen vorher mit newDivisions() best. werden.   */
/* Die xyz-Koordinaten gelten fuer ein Feld im ba-Raum                            */
/*                                                                                */
/* in:                                                                            */
/* sur      surface-index                                                         */
/* div_l    feld mit den divisions der surface-edges                            */
/* div_a    ersatzdivision der 'a'-edges der surf.                              */
/* div_b    ersatzdivision der 'b'-edges der surf.                              */
/* sa1,sa2,sb1,sb2  Zuordnung der original-edges zu den a&b-edges             */
/*                                                                                */
/* out:                                                                           */
/* n_uv        nodeNr im uv-feld, die zusaetzlichen ersatz-stuetzpunkte sind      */
/*             nicht mit nodes belegt da sie bei der Vernetzung rausfallen        */
/* umax, vmax  anzahl der ersatz-stuetzpunkte in 'u' und 'v' bzw entlang edge   */
/*             '0' und '3' ( umax = div_(a|b) +1 )                                */
/* n_ba        nodeNr im ba-feld, siehe n_uv                                      */
/* amax, bmax  anzahl der ersatz-stuetzpunkte in 'a' und 'b'                      */
/*                         ( amax = div_a +1 )                                    */
/* offs_sa1    div_a-div_l[sa1], zur elementerzeugung notwendig                   */
/* offs_sa2    div_a-div_l[sa2]                                                   */
/* x,y,z       f(u,v) aus dem surfmesher, alle u,v positionen sind belegt         */
/* x_ba..      f(b,a) aus dem surfmesher, alle b,a positionen sind belegt         */
/*                                                                                */
/*                                                                                */
/**********************************************************************************/
int  fillSurf_ba2( int sur, int *div_l, int div_a, int div_b, int sa1, int sa2, int sb1, int sb2, int *n_uv, int *umax, int *vmax, int *n_ba, int *amax, int *bmax,  int *offs_sa1, int *offs_sa2, double *x, double *y, double *z, double *x_ba, double *y_ba, double *z_ba )
{
  int n,o,m, u,v, b=0,a=0;
  int nodnr;
  double *lx, *ly, *lz; /* line koordinates for linelength() */
  int flag;

  int k;

      *bmax=div_b+1;
      *amax=div_a+1;
      if ((sa1==0)||(sa1==2))
      {
        *umax=*amax    ;
        *vmax=*bmax    ;
      }
      else
      {
        *umax=*bmax    ;
        *vmax=*amax    ;
      }
      edgeNodes( *vmax, *umax, sur, n_uv );

      /* die Randknoten sind nun bekannt. es muessen nun ersatzkoordinaten */
      /* mit dem spacing der ersatzdivissions berechnet werden. dazu wird  */
      /* in einem feld aus koordinate(x,y oder z) und der lauflaenge s int-*/
      /* erpoliert. Ausserdem werden die Randknoten im b,a system          */
      /* gespeichert, dazu ist auch ein offset erforderlich                */

      *offs_sa1=div_a-div_l[sa1];
      *offs_sa2=div_a-div_l[sa2];

      /* side 0 */
      v=0;
      n=div_l[0]+1;        /* anzahl von original-nodes auf der line */
      if (((lx = (double *)malloc( (n)*sizeof(double)) ) == NULL )||
          ((ly = (double *)malloc( (n)*sizeof(double)) ) == NULL )||
          ((lz = (double *)malloc( (n)*sizeof(double)) ) == NULL ))
      { errMsg(" ERROR: realloc failure in fillSurf_ba()\n");    return(-1); }
      for (u=0; u<n; u++)
      {
        nodnr=n_uv[u* *vmax +v];
        o=0;
        if ((sb1==0)&&(sa1==3)) { b=u                     ; a=0                    ;  o=1 ;}
        if ((sb1==3)&&(sa1==2)) { b=*vmax-1               ; a=u                    ;  o=2 ;}
        if ((sb1==2)&&(sa1==1)) { b=*umax-u-1-*offs_sa2   ; a=*vmax-1              ;  o=3 ;}
        if ((sb1==1)&&(sa1==0)) { b=0                     ; a=*umax-u-1-*offs_sa1  ;  o=4 ;}
        if ((sb1==0)&&(sa1==1)) { b=*umax-u-1             ; a=0                    ;  o=5 ;}
        if ((sb1==3)&&(sa1==0)) { b=0                     ; a=u                    ;  o=6 ;}
        if ((sb1==2)&&(sa1==3)) { b=u+*offs_sa1           ; a=*vmax-1              ;  o=7 ;}
        if ((sb1==1)&&(sa1==2)) { b=*vmax-1               ; a=*umax-u-1-*offs_sa2  ;  o=8 ;}
        n_ba[b* *amax +a]=nodnr;
        lx[u]=npre[nodnr].nx;
        ly[u]=npre[nodnr].ny;
        lz[u]=npre[nodnr].nz;
      }
      /* nun lx,ly,lz auf die geaenderte division umrechnen */
      u=0;
      flag=0;
      if( newEdgePositions( n, u,v, *umax,*vmax, lx,ly,lz, x,y,z, flag) <0) return(-1);
      free(lx);free(ly);free(lz); 

      /* side 1 */
      u=*umax-1;
      n=div_l[1]+1;
      if (((lx = (double *)malloc( (n)*sizeof(double)) ) == NULL )||
          ((ly = (double *)malloc( (n)*sizeof(double)) ) == NULL )||
          ((lz = (double *)malloc( (n)*sizeof(double)) ) == NULL ))
      { errMsg(" ERROR: realloc failure in fillSurf_ba()\n");    return(-1); }
      for (v=0; v<n; v++)
      {
        nodnr=n_uv[u**vmax +v];
        o=0;
        if ((sb1==0)&&(sa1==3)) { b=*umax-1              ; a=v                    ;  o=1 ;}
        if ((sb1==3)&&(sa1==2)) { b=*vmax-v-1-*offs_sa2   ; a=*umax-1               ;  o=2 ;}
        if ((sb1==2)&&(sa1==1)) { b=0                   ; a=*vmax-v-1-*offs_sa1    ;  o=3 ;}
        if ((sb1==1)&&(sa1==0)) { b=v                   ; a=0                    ;  o=4 ;}
        if ((sb1==0)&&(sa1==1)) { b=0                   ; a=v                    ;  o=5 ;}
        if ((sb1==3)&&(sa1==0)) { b=v+*offs_sa1          ; a=*umax-1               ;  o=6 ;}
        if ((sb1==2)&&(sa1==3)) { b=*umax-1              ; a=*vmax-v-1-*offs_sa2    ;  o=7 ;}
        if ((sb1==1)&&(sa1==2)) { b=*vmax-v-1            ; a=0                    ;  o=8 ;}
        n_ba[b**amax +a]=nodnr;
        lx[v]=npre[nodnr].nx;
        ly[v]=npre[nodnr].ny;
        lz[v]=npre[nodnr].nz;
      }
      /* nun auf die geaenderte division umrechnen */
      v=0;
      flag=1;
      if( newEdgePositions( n, u,v, *umax,*vmax, lx,ly,lz, x,y,z, flag) <0) return(-1);
      free(lx);free(ly);free(lz); 


      /* side 2 */
      v=*vmax-1;
      n=div_l[2]+1;
      if (((lx = (double *)malloc( (n)*sizeof(double)) ) == NULL )||
          ((ly = (double *)malloc( (n)*sizeof(double)) ) == NULL )||
          ((lz = (double *)malloc( (n)*sizeof(double)) ) == NULL ))
      { errMsg(" ERROR: realloc failure in fillSurf_ba()\n");    return(-1); }
      m=*umax-n;
      for (u=*umax-1; u>=m; u--)
      {
        nodnr=n_uv[u**vmax +v];
        o=0;
        if ((sb1==0)&&(sa1==3)) { b=u-*offs_sa2          ; a=*vmax-1               ;  o=1 ;}
        if ((sb1==3)&&(sa1==2)) { b=0                   ; a=u-*offs_sa1           ;  o=2 ;}
        if ((sb1==2)&&(sa1==1)) { b=*umax-u-1            ; a=0                    ;  o=3 ;}
        if ((sb1==1)&&(sa1==0)) { b=*vmax-1              ; a=*umax-u-1             ;  o=4 ;}
        if ((sb1==0)&&(sa1==1)) { b=*umax-u-1+*offs_sa1   ; a=*vmax-1               ;  o=5 ;}
        if ((sb1==3)&&(sa1==0)) { b=*vmax-1              ; a=u-*offs_sa2           ;  o=6 ;}
        if ((sb1==2)&&(sa1==3)) { b=u                   ; a=0                    ;  o=7 ;}
        if ((sb1==1)&&(sa1==2)) { b=0                   ; a=*umax-u-1             ;  o=8 ;}
        n_ba[b**amax +a]=nodnr;
        lx[u-m]=npre[nodnr].nx;
        ly[u-m]=npre[nodnr].ny;
        lz[u-m]=npre[nodnr].nz;
      }
      /* nun auf die geaenderte division umrechnen */
      u=0;
      flag=0;
      if( newEdgePositions( n, u,v, *umax,*vmax, lx,ly,lz, x,y,z, flag) <0) return(-1);
      free(lx);free(ly);free(lz); 


      /* side 3 */
      u=0;
      n=div_l[3]+1;
      if (((lx = (double *)malloc( (n)*sizeof(double)) ) == NULL )||
          ((ly = (double *)malloc( (n)*sizeof(double)) ) == NULL )||
          ((lz = (double *)malloc( (n)*sizeof(double)) ) == NULL ))
      { errMsg(" ERROR: realloc failure in fillSurf_ba()\n");    return(-1); }
      m=*vmax-n;
      for (v=*vmax-1; v>=m; v--)
      {
        nodnr=n_uv[u**vmax +v];
        o=0;
        if ((sb1==0)&&(sa1==3)) { b=0                   ; a=v-*offs_sa1           ;  o=1 ;}
        if ((sb1==3)&&(sa1==2)) { b=*vmax-v-1            ; a=0                    ;  o=2 ;}
        if ((sb1==2)&&(sa1==1)) { b=*umax-1              ; a=*vmax-v-1             ;  o=3 ;}
        if ((sb1==1)&&(sa1==0)) { b=v-*offs_sa2          ; a=*umax-1               ;  o=4 ;}
        if ((sb1==0)&&(sa1==1)) { b=*umax-1              ; a=v-*offs_sa2           ;  o=5 ;}
        if ((sb1==3)&&(sa1==0)) { b=v                   ; a=0                    ;  o=6 ;}
        if ((sb1==2)&&(sa1==3)) { b=0                   ; a=*vmax-v-1             ;  o=7 ;}
        if ((sb1==1)&&(sa1==2)) { b=*vmax-v-1+*offs_sa1   ; a=*umax-1               ;  o=8 ;}
        n_ba[b* *amax +a]=nodnr;
        lx[v-m]=npre[nodnr].nx;
        ly[v-m]=npre[nodnr].ny;
        lz[v-m]=npre[nodnr].nz;
      }
      /* nun auf die geaenderte division umrechnen */
      v=0;
      flag=1;
      if( newEdgePositions( n, u,v, *umax,*vmax, lx,ly,lz, x,y,z, flag) <0) return(-1);
      free(lx);free(ly);free(lz); 


      k=0;
      surfMesh( vmax, umax, x, y, z);
      for (u=1; u< *umax-1; u++)
      {
        for (v=1; v< *vmax-1; v++)
        {
        /* umspeichern der temp-nodes ins ba system */
        o=0;
        if ((sb1==0)&&(sa1==3)) { b=u                   ; a=v                    ;  o=1 ;}
        if ((sb1==3)&&(sa1==2)) { b=*vmax-v-1           ; a=u                    ;  o=2 ;}
        if ((sb1==2)&&(sa1==1)) { b=*umax-u-1           ; a=*vmax-v-1            ;  o=3 ;}
        if ((sb1==1)&&(sa1==0)) { b=v                   ; a=*umax-u-1            ;  o=4 ;}
        if ((sb1==0)&&(sa1==1)) { b=*umax-u-1           ; a=v                    ;  o=5 ;}
        if ((sb1==3)&&(sa1==0)) { b=v                   ; a=u                    ;  o=6 ;}
        if ((sb1==2)&&(sa1==3)) { b=u                   ; a=*vmax-v-1            ;  o=7 ;}
        if ((sb1==1)&&(sa1==2)) { b=*vmax-v-1           ; a=*umax-u-1            ;  o=8 ;}
        n_ba[b* *amax +a]=surf[sur].nod[k]; k++;
        x_ba[b* *amax +a]=x[u* *vmax +v];
        y_ba[b* *amax +a]=y[u* *vmax +v];
        z_ba[b* *amax +a]=z[u* *vmax +v];
#if TEST
  printf(" Fall:%d b:%d a:%d\n", o, b, a);
#endif
        }
      }
      for (u=0; u< *umax; u++)
      {
        for (v=0; v< *vmax; v++)
        {
        /* umspeichern der koordinaten ins ba system */
        if ((sb1==0)&&(sa1==3)) { b=u                   ; a=v                    ;  o=1 ;}
        if ((sb1==3)&&(sa1==2)) { b=*vmax-v-1           ; a=u                    ;  o=2 ;}
        if ((sb1==2)&&(sa1==1)) { b=*umax-u-1           ; a=*vmax-v-1            ;  o=3 ;}
        if ((sb1==1)&&(sa1==0)) { b=v                   ; a=*umax-u-1            ;  o=4 ;}
        if ((sb1==0)&&(sa1==1)) { b=*umax-u-1           ; a=v                    ;  o=5 ;}
        if ((sb1==3)&&(sa1==0)) { b=v                   ; a=u                    ;  o=6 ;}
        if ((sb1==2)&&(sa1==3)) { b=u                   ; a=*vmax-v-1            ;  o=7 ;}
        if ((sb1==1)&&(sa1==2)) { b=*vmax-v-1           ; a=*umax-u-1            ;  o=8 ;}
        x_ba[b* *amax +a]=x[u* *vmax +v];
        y_ba[b* *amax +a]=y[u* *vmax +v];
        z_ba[b* *amax +a]=z[u* *vmax +v];
#if TEST
  printf("u:%d v:%d  x_ba[%d][%d]=%lf\n", u, v, b, a, x_ba[b* *amax +a]);
#endif
        }
      }

      /* randbereiche umspeichern (nodenr der diagonale den raendern der undefinierten 
         luecken zuordnen) */
      a=div_l[sa1];
      for (b=1; b<*offs_sa1; b++)
      {
        n_ba[b* *amax +a]=n_ba[b* *amax +(div_a-b)];
#if TEST
  printf("1 node:%d b:%d a:%d (div_a-b):%d\n", n_ba[b* *amax +a], b, a,(div_a-b));
#endif
      }
      o=0;
      b=*offs_sa1;
      for (a=div_l[sa1]+1; a<div_a; a++)
      {
        o++;
        n_ba[b* *amax +a]=n_ba[(b-o)* *amax +a];
#if TEST
  printf("2 node:%d b:%d a:%d\n", n_ba[b* *amax +a], b, a);
#endif
      }
      o=0;
      a=div_l[sa2];
      for (b=div_b-1; b>div_b - *offs_sa2; b--)
      {
        o++;
        n_ba[b* *amax +a]=n_ba[b* *amax +(div_a-o)];
#if TEST
  printf("3 node:%d b:%d a:%d\n", n_ba[b* *amax +a], b, a);
#endif
     }
      o=0;
      b=div_b- *offs_sa2;
      for (a=div_l[sa2]+1; a<div_a; a++)
      {
        o++;
        n_ba[b* *amax +a]=n_ba[(b+o)* *amax +a];
#if TEST
  printf("4 node:%d b:%d a:%d\n", n_ba[b* *amax +a], b, a);
#endif
      }

  return(1);
}




/**********************************************************************************/
/*                                                                                */
/*                                                                                */
/*                                                                                */
/*                                                                                */
/*                                                                                */
/*                                                                                */
/*                                                                                */
/*                                                                                */
/**********************************************************************************/
int fillBody2( int b_indx, int *srefp, int **div_l, int *unbalance, int **nabc, int *amax, int *bmax, int *cmax, int *offs_sa1, int *offs_sa2, int *msur, int *div_sa1, int *div_sa2 )
{
  int i,j=0,k,n,a,b,c,o;
  int bs[6], ml[5];
  char mlt[5];
  int  s_indx[6], buf[4];
  int *n_uv=0, *n_ba=0, umax, vmax, dum1, dum2, offs1, offs2;
  double  *x=0, *y=0, *z=0, *x_ba=0, *y_ba=0, *z_ba=0, xn,yn,zn;

  static int *n_abc=0;
  static double  *x_abc=0, *y_abc=0, *z_abc=0;

  int   div_a,div_b, sa1,sa2,sb1,sb2;

#if TEST
  printf("\n unbalanced Body:%s \n\n",body[b_indx].name); 
#endif


  /* mastersurf bestimmen */
  for(i=0; i<body[b_indx].ns; i++)
  {
    if (unbalance[i]) bs[0]=i;
#if TEST1
    printf(" unbalance[%d]:%d\n", i, unbalance[i]); 
#endif
  }

  /* new div fuer die Mastersurf */
  n=newDivisions( &div_l[bs[0]][0], &div_a, &div_b, &sa1, &sa2, &sb1, &sb2 );
  if (n==-1) goto noMesh;
  if( body[b_indx].o[bs[0]]=='-')
  {
    dum1=sa1; dum2=sa2;
    sa1=dum2; sa2=dum1;
  }    

  /* store the masterlines ml[] */
  ml[0]=sb1; mlt[0]=surf[body[b_indx].s[bs[0]]].typ[sb1];
  ml[1]=sa2; mlt[1]=surf[body[b_indx].s[bs[0]]].typ[sa2];
  ml[2]=sb2; mlt[2]=surf[body[b_indx].s[bs[0]]].typ[sb2];
  ml[3]=sa1; mlt[3]=surf[body[b_indx].s[bs[0]]].typ[sa1];

  /* Hilfsfelder fuer die surfs */
  if( (n_uv=(int *)malloc( (div_a+1)*(div_b+1)*sizeof(int) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (n_ba=(int *)malloc( (div_a+1)*(div_b+1)*sizeof(int) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (x=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (y=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (z=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (x_ba=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (y_ba=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (z_ba=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }

  /* Fuelle ein xyz-feld mit den koordinaten einer Flaeche mit ungleichen divisions. */
  s_indx[0]=body[b_indx].s[bs[0]];
#if TEST1
  for (i=0; i<4; i++) 
    printf ("ml[%d]:%s\n",i,line[surf[body[b_indx].s[bs[0]]].l[ml[i]]].name);
  printf ("sur0:%s \n", surf[s_indx[0]].name);
#endif
  fillSurf_ba2( s_indx[0], &div_l[bs[0]][0], div_a, div_b, sa1, sa2, sb1, sb2,
       n_uv, &umax, &vmax, n_ba, amax, bmax, offs_sa1, offs_sa2, x,y,z, x_ba,y_ba,z_ba);
  offs1=*offs_sa1;
  offs2=*offs_sa2;
  *msur=body[b_indx].s[bs[0]];
  *div_sa1=div_l[bs[0]][sa1];
  *div_sa2=div_l[bs[0]][sa2];


  /* suchen der surf an masterline ml[0] zur bestimmung von cmax */
  *cmax=-1;
  for (i=0; i<body[b_indx].ns; i++)
  {
    n= body[b_indx].s[i];
    if(i!=bs[0]) for (j=0; j<surf[n].nl; j++)
    {
      if((surf[n].l[j]==surf[s_indx[0]].l[ml[0]])&&(surf[n].typ[j]==surf[s_indx[0]].typ[ml[0]]))
      {
        if (unbalance[i]!=0)
	{
          errMsg(" ERROR, body %s not meshable, correct div, side:%d (unbal:%d)!\n", body[b_indx].name, i, unbalance[i]);
          goto noMesh;
        }
        bs[1]=i;
        s_indx[1]=n;
#if TEST1
        printf ("sur1:%s \n", surf[s_indx[1]].name);
#endif
        /* bestimme die parametrische kantenlaenge cmax = div von linie davor oder danach */  
        if(j) *cmax= div_l[bs[1]][j-1]+1;
        else  *cmax= div_l[bs[1]][1]+1;
      }
    }
  }
  if(*cmax==-1)
  { printf(" FATAL ERROR, could not determine cmax:%d, check program-code\n", *cmax); exit(-1); }

  /* amax, bmax, cmax sind bekannt, damit die Datenfelder dimensionieren */
#if TEST1
  printf ("Nr of Nodes in 3D amax:%d bmax:%d cmax:%d\n", *amax, *bmax, *cmax );
#endif

  if( (n_abc=(int *)realloc((int *)n_abc, (*amax)*(*bmax)*(*cmax)*sizeof(int)) ) == NULL )
  { printf(" ERROR: realloc failure in fillBody body can not be meshed\n\n"); goto noMesh; }
  if( (x_abc=(double *)realloc((double *)x_abc, (*amax)*(*bmax)*(*cmax)*sizeof(double)) ) == NULL )
  { printf(" ERROR: realloc failure in fillBody body can not be meshed\n\n"); goto noMesh; }
  if( (y_abc=(double *)realloc((double *)y_abc, (*amax)*(*bmax)*(*cmax)*sizeof(double)) ) == NULL )
  { printf(" ERROR: realloc failure in fillBody body can not be meshed\n\n"); goto noMesh; }
  if( (z_abc=(double *)realloc((double *)z_abc, (*amax)*(*bmax)*(*cmax)*sizeof(double)) ) == NULL )
  { printf(" ERROR: realloc failure in fillBody body can not be meshed\n\n"); goto noMesh; }

  /* felder initialisieren */
  for (a=0; a<*amax; a++)
  {
    for (b=0; b<*bmax; b++)
    {
      for (c=0; c<*cmax; c++)
      {
        n_abc[a**bmax**cmax +b**cmax +c]=0;
        x_abc[a**bmax**cmax +b**cmax +c]=-scale->x;
        y_abc[a**bmax**cmax +b**cmax +c]=-scale->y;
        z_abc[a**bmax**cmax +b**cmax +c]=-scale->z;
      }
    }
  }

  /* Uebertrage die daten der mastersurf: bs[0] bzw s_indx[0] ins abc-sys. */  
  c=0;
  for (a=0; a<*amax; a++)
  {
    for (b=0; b<*bmax; b++)
    {
      n_abc[a**bmax**cmax +b**cmax +c]=n_ba[b**amax +a];
      x_abc[a**bmax**cmax +b**cmax +c]=x_ba[b**amax +a];
      y_abc[a**bmax**cmax +b**cmax +c]=y_ba[b**amax +a];
      z_abc[a**bmax**cmax +b**cmax +c]=z_ba[b**amax +a];
    }
  }



  /* Zuordnen der edges von bs[1] zum surf-ba system */
  /* surf-edge b1 entspricht body-edge c */
  /* surf-edge a1 entspricht body-edge b */
  if( body[b_indx].o[bs[1]]=='+')
  {
    if(( surf[s_indx[0]].l[ml[0]]==surf[s_indx[1]].l[0])
    && ( mlt[0]==surf[s_indx[1]].typ[0])) {i=0; sb1=1; sa2=2; sb2=3; sa1=0; }
    else if(( surf[s_indx[0]].l[ml[0]]==surf[s_indx[1]].l[1])
    && ( mlt[0]==surf[s_indx[1]].typ[1])) {i=1;  sb1=2; sa2=3; sb2=0; sa1=1; }
    else if(( surf[s_indx[0]].l[ml[0]]==surf[s_indx[1]].l[2])
    && ( mlt[0]==surf[s_indx[1]].typ[2])) {i=2;  sb1=3; sa2=0; sb2=1; sa1=2; }
    else if(( surf[s_indx[0]].l[ml[0]]==surf[s_indx[1]].l[3])
    && ( mlt[0]==surf[s_indx[1]].typ[3])) { i=3; sb1=0; sa2=1; sb2=2; sa1=3; }
    else
      { printf(" FATAL ERROR, found no edge\n"); exit(-1); }
  }    
  else
  {
    if(( surf[s_indx[0]].l[ml[0]]==surf[s_indx[1]].l[0])
    && ( mlt[0]==surf[s_indx[1]].typ[0])) {i=0;  sb1=3; sa2=2; sb2=1; sa1=0; }
    else if(( surf[s_indx[0]].l[ml[0]]==surf[s_indx[1]].l[1])
    && ( mlt[0]==surf[s_indx[1]].typ[1])) {i=1;  sb1=0; sa2=3; sb2=2; sa1=1; }
    else if(( surf[s_indx[0]].l[ml[0]]==surf[s_indx[1]].l[2])
    && ( mlt[0]==surf[s_indx[1]].typ[2])) {i=2;  sb1=1; sa2=0; sb2=3; sa1=2; }
    else if(( surf[s_indx[0]].l[ml[0]]==surf[s_indx[1]].l[3])
    && ( mlt[0]==surf[s_indx[1]].typ[3])) {i=3;  sb1=2; sa2=1; sb2=0; sa1=3; }
    else
      { printf(" FATAL ERROR, found no edge\n"); exit(-1); }
  }
  ml[4]=sa2; mlt[4]=surf[body[b_indx].s[bs[1]]].typ[sa2];
  div_a= *bmax-1;
  div_b= *cmax-1;
  offs1=0;
  offs2=0;
 
  /* hilfsspeicher freigeben und neu allocieren */
  free(n_uv);
  free(n_ba);
  free(x);
  free(y);
  free(z);
  free(x_ba);
  free(y_ba);
  free(z_ba);
  if( (n_uv=(int *)malloc( (div_a+1)*(div_b+1)*sizeof(int) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (n_ba=(int *)malloc( (div_a+1)*(div_b+1)*sizeof(int) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (x=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (y=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (z=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (x_ba=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (y_ba=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (z_ba=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }

  /* Fülle ein xyz-feld mit den koordinaten einer Flaeche mit gleichen divisions. */
  fillSurf_ba1( s_indx[1], &div_l[bs[1]][0], div_a, div_b, sa1, sa2, sb1, sb2,
       n_uv, n_ba, offs1, offs2, x,y,z, x_ba,y_ba,z_ba);

  /* Uebertrage die daten der bs[1] ins abc-sys. */  
  a=0;
  for (b=0; b<*bmax; b++)
  {
    for (c=0; c<*cmax; c++)
    {
      n_abc[a**bmax**cmax +b**cmax +c]=n_ba[c**bmax +b];
      x_abc[a**bmax**cmax +b**cmax +c]=x_ba[c**bmax +b];
      y_abc[a**bmax**cmax +b**cmax +c]=y_ba[c**bmax +b];
      z_abc[a**bmax**cmax +b**cmax +c]=z_ba[c**bmax +b];
    }
  }



  /* suchen der surf an masterline ml[4], diese liegt der mastersurf gegenueber */
  /* und muss unbalanced sein */
  for (i=0; i<body[b_indx].ns; i++)
  {
    n= body[b_indx].s[i];
    if((i!=bs[1])&&(i!=bs[0])) for (j=0; j<surf[n].nl; j++)
    {
      if((surf[n].l[j]==surf[s_indx[1]].l[ml[4]])&&(surf[n].typ[j]==surf[s_indx[1]].typ[ml[4]]))
      {
        if (unbalance[i]!=1)
	{
          errMsg(" ERROR, body %s not meshable, correct div, side:%d (unbal:%d)!\n", body[b_indx].name, i, unbalance[i]);
          goto noMesh;
        }
        bs[2]=i;
        s_indx[2]=n;
#if TEST1
        printf ("sur2:%s \n", surf[s_indx[2]].name);
#endif
      }
    }
  }

  /* Fülle ein xyz-feld mit den koordinaten einer Flaeche mit ungleichen divisions. */
  n=newDivisions( &div_l[bs[2]][0], &div_a, &div_b, &sa1, &sa2, &sb1, &sb2 );
  if (n==-1) goto noMesh;

  /* check the corresponding divisions of surf2 with surf0 */
  /* could be wrong if there are only two different divisions on the 4 edges (ie. 16,16,4,4) */
  buf[0]=sa1;
  buf[1]=sb2;
  buf[2]=sa2;
  buf[3]=sb1;
  if( body[b_indx].o[bs[2]]=='+')
  {
    if( div_l[bs[2]][sb1]==div_l[bs[2]][sa1])
      if( div_l[bs[2]][sa1]!= div_l[bs[0]][ml[1]])
      { sb1=buf[0]; sa1=buf[1]; sb2=buf[2]; sa2=buf[3]; }
    if( div_l[bs[2]][sb1]==div_l[bs[2]][sa2])
      if( div_l[bs[2]][sa2]!= div_l[bs[0]][ml[3]])
      { sb1=buf[2]; sa1=buf[3]; sb2=buf[0]; sa2=buf[1]; }
  }
  else
  {
    if( div_l[bs[2]][sb1]==div_l[bs[2]][sa2])
      if( div_l[bs[2]][sa1]!= div_l[bs[0]][ml[3]])
      { sb1=buf[2]; sa1=buf[3]; sb2=buf[0]; sa2=buf[1]; }
    if( div_l[bs[2]][sb1]==div_l[bs[2]][sa1])
      if( div_l[bs[2]][sa1]!= div_l[bs[0]][ml[3]])
      { sb1=buf[0]; sa1=buf[1]; sb2=buf[2]; sa2=buf[3]; }
  }

  /* hilfsspeicher freigeben und neu allocieren */
  free(n_uv);
  free(n_ba);
  free(x);
  free(y);
  free(z);
  free(x_ba);
  free(y_ba);
  free(z_ba);
  if( (n_uv=(int *)malloc( (div_a+1)*(div_b+1)*sizeof(int) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (n_ba=(int *)malloc( (div_a+1)*(div_b+1)*sizeof(int) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (x=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (y=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (z=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (x_ba=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (y_ba=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (z_ba=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }

  fillSurf_ba2( s_indx[2], &div_l[bs[2]][0], div_a, div_b, sa1, sa2, sb1, sb2,
       n_uv, &umax, &vmax, n_ba, &dum1, &dum2, &offs1, &offs2, x,y,z, x_ba,y_ba,z_ba);

  /* Uebertrage die daten der surf bs[2] bzw s_indx[2] ins abc-sys. */  
  c=*cmax-1;
  if( body[b_indx].o[bs[2]]=='+')
  {
   for (a=0; a<*amax; a++)
   {
    for (b=0; b<*bmax; b++)
    {
      n_abc[a**bmax**cmax +b**cmax +c]=n_ba[(*bmax-1-b)**amax +a];
      x_abc[a**bmax**cmax +b**cmax +c]=x_ba[(*bmax-1-b)**amax +a];
      y_abc[a**bmax**cmax +b**cmax +c]=y_ba[(*bmax-1-b)**amax +a];
      z_abc[a**bmax**cmax +b**cmax +c]=z_ba[(*bmax-1-b)**amax +a];
    }
   }
  }
  else
  {
   for (a=0; a<*amax; a++)
   {
    for (b=0; b<*bmax; b++)
    {
      n_abc[a**bmax**cmax +b**cmax +c]=n_ba[b**amax +a];
      x_abc[a**bmax**cmax +b**cmax +c]=x_ba[b**amax +a];
      y_abc[a**bmax**cmax +b**cmax +c]=y_ba[b**amax +a];
      z_abc[a**bmax**cmax +b**cmax +c]=z_ba[b**amax +a];
    }
   }
  }



  /* suchen der surf3 an masterline ml[3] */
  /* und darf nicht unbalanced sein */
  for (i=0; i<body[b_indx].ns; i++)
  {
    n= body[b_indx].s[i];
    if(i!=bs[0]) for (j=0; j<surf[n].nl; j++)
    {
      if((surf[n].l[j]==surf[s_indx[0]].l[ml[3]])&&(surf[n].typ[j]==surf[s_indx[0]].typ[ml[3]]))
      {
        if (unbalance[i]==1)
	{
          errMsg(" ERROR, body %s not meshable, correct div, side:%d (unbal:%d)!\n", body[b_indx].name, i, unbalance[i]);
          goto noMesh;
        }
        bs[3]=i;
        s_indx[3]=n;
#if TEST1
        printf ("sur3:%s \n", surf[s_indx[3]].name);
#endif
      }
    }
  }

  /* Zuordnen der edges von bs[3] zum surf-ba system */
  /* surf-edge b1 entspricht body-edge a */
  /* surf-edge a1 entspricht body-edge c */
  if( body[b_indx].o[bs[3]]=='+')
  {
    if(( surf[s_indx[0]].l[ml[3]]==surf[s_indx[3]].l[1])
    && ( mlt[3]==surf[s_indx[3]].typ[1])) { sb1=1; sa2=2; sb2=3; sa1=0; }
    else if(( surf[s_indx[0]].l[ml[3]]==surf[s_indx[3]].l[2])
    && ( mlt[3]==surf[s_indx[3]].typ[2])) { sb1=2; sa2=3; sb2=0; sa1=1; }
    else if(( surf[s_indx[0]].l[ml[3]]==surf[s_indx[3]].l[3])
    && ( mlt[3]==surf[s_indx[3]].typ[3])) { sb1=3; sa2=0; sb2=1; sa1=2; }
    else if(( surf[s_indx[0]].l[ml[3]]==surf[s_indx[3]].l[0])
    && ( mlt[3]==surf[s_indx[3]].typ[0])) { sb1=0; sa2=1; sb2=2; sa1=3; }
    else
      { printf(" FATAL ERROR, found no edge\n"); exit(-1); }
  }    
  else
  {
    if(( surf[s_indx[0]].l[ml[3]]==surf[s_indx[3]].l[3])
    && ( mlt[3]==surf[s_indx[3]].typ[3])) { sb1=3; sa2=2; sb2=1; sa1=0; }
    else if(( surf[s_indx[0]].l[ml[3]]==surf[s_indx[3]].l[0])
    && ( mlt[3]==surf[s_indx[3]].typ[0])) { sb1=0; sa2=3; sb2=2; sa1=1; }
    else if(( surf[s_indx[0]].l[ml[3]]==surf[s_indx[3]].l[1])
    && ( mlt[3]==surf[s_indx[3]].typ[1])) { sb1=1; sa2=0; sb2=3; sa1=2; }
    else if(( surf[s_indx[0]].l[ml[3]]==surf[s_indx[3]].l[2])
    && ( mlt[3]==surf[s_indx[3]].typ[2])) { sb1=2; sa2=1; sb2=0; sa1=3; }
    else
      { printf(" FATAL ERROR, found no edge\n"); exit(-1); }
  }
  div_a= *cmax-1;
  div_b= *amax-1;
  offs1=0;
  offs2=*offs_sa1;
 
  /* hilfsspeicher freigeben und neu allocieren */
  free(n_uv);
  free(n_ba);
  free(x);
  free(y);
  free(z);
  free(x_ba);
  free(y_ba);
  free(z_ba);
  if( (n_uv=(int *)malloc( (div_a+1)*(div_b+1)*sizeof(int) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (n_ba=(int *)malloc( (div_a+1)*(div_b+1)*sizeof(int) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (x=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (y=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (z=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (x_ba=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (y_ba=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (z_ba=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  /* Fülle ein xyz-feld mit den koordinaten einer Flaeche mit gleichen divisions. */
  fillSurf_ba1( s_indx[3], &div_l[bs[3]][0], div_a, div_b, sa1, sa2, sb1, sb2,
       n_uv, n_ba, offs1, offs2, x,y,z, x_ba,y_ba,z_ba);
  
  /* Uebertrage die daten der surf: bs[3] bzw s_indx[3] ins abc-sys. */  
  b=0;
  for (a=0; a<*amax; a++)
  {
    for (c=0; c<*cmax; c++)
    {
      n_abc[a**bmax**cmax +b**cmax +c]=n_ba[a**cmax +c];
      x_abc[a**bmax**cmax +b**cmax +c]=x_ba[a**cmax +c];
      y_abc[a**bmax**cmax +b**cmax +c]=y_ba[a**cmax +c];
      z_abc[a**bmax**cmax +b**cmax +c]=z_ba[a**cmax +c];
    }
  }



  /* suchen der surf4 an masterline ml[1] */
  /* und darf nicht unbalanced sein */
  for (i=0; i<body[b_indx].ns; i++)
  {
    n= body[b_indx].s[i];
    if(i!=bs[0]) for (j=0; j<surf[n].nl; j++)
    {
      if((surf[n].l[j]==surf[s_indx[0]].l[ml[1]])&&(surf[n].typ[j]==surf[s_indx[0]].typ[ml[1]]))
      {
        if (unbalance[i]==1)
	{
          errMsg(" ERROR, body %s not meshable, correct div, side:%d (unbal:%d)!\n", body[b_indx].name, i, unbalance[i]);
          goto noMesh;
        }
        bs[4]=i;
        s_indx[4]=n;
#if TEST1
        printf ("sur4:%s \n", surf[s_indx[4]].name);
#endif
      }
    }
  }

  /* Zuordnen der edges von bs[4] zum surf-ba system */
  /* surf-edge b1 entspricht body-edge a */
  /* surf-edge a1 entspricht body-edge c */
  if( body[b_indx].o[bs[4]]=='+')
  {
    if(( surf[s_indx[0]].l[ml[1]]==surf[s_indx[4]].l[3])
    && ( mlt[1]==surf[s_indx[4]].typ[3])) { sb1=3; sa2=2; sb2=1; sa1=0; }
    else if(( surf[s_indx[0]].l[ml[1]]==surf[s_indx[4]].l[0])
    && ( mlt[1]==surf[s_indx[4]].typ[0])) { sb1=0; sa2=3; sb2=2; sa1=1; }
    else if(( surf[s_indx[0]].l[ml[1]]==surf[s_indx[4]].l[1])
    && ( mlt[1]==surf[s_indx[4]].typ[1])) { sb1=1; sa2=0; sb2=3; sa1=2; }
    else if(( surf[s_indx[0]].l[ml[1]]==surf[s_indx[4]].l[2])
    && ( mlt[1]==surf[s_indx[4]].typ[2])) { sb1=2; sa2=1; sb2=0; sa1=3; }
    else
      { printf(" FATAL ERROR, found no edge\n"); exit(-1); }
  }    
  else
  {
    if(( surf[s_indx[0]].l[ml[1]]==surf[s_indx[4]].l[1])
    && ( mlt[1]==surf[s_indx[4]].typ[1])) { sb1=1; sa2=2; sb2=3; sa1=0; }
    else if(( surf[s_indx[0]].l[ml[1]]==surf[s_indx[4]].l[2])
    && ( mlt[1]==surf[s_indx[4]].typ[2])) { sb1=2; sa2=3; sb2=0; sa1=1; }
    else if(( surf[s_indx[0]].l[ml[1]]==surf[s_indx[4]].l[3])
    && ( mlt[1]==surf[s_indx[4]].typ[3])){ sb1=3; sa2=0; sb2=1; sa1=2; }
    else if(( surf[s_indx[0]].l[ml[1]]==surf[s_indx[4]].l[0])
    && ( mlt[1]==surf[s_indx[4]].typ[0])) { sb1=0; sa2=1; sb2=2; sa1=3; }
    else
      { printf(" FATAL ERROR, found no edge\n"); exit(-1); }
  }
  div_a= *cmax-1;
  div_b= *amax-1;
  offs1=0;
  offs2=*offs_sa2;
 
  /* hilfsspeicher freigeben und neu allocieren */
  free(n_uv);
  free(n_ba);
  free(x);
  free(y);
  free(z);
  free(x_ba);
  free(y_ba);
  free(z_ba);
  if( (n_uv=(int *)malloc( (div_a+1)*(div_b+1)*sizeof(int) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (n_ba=(int *)malloc( (div_a+1)*(div_b+1)*sizeof(int) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (x=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (y=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (z=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (x_ba=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (y_ba=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (z_ba=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  /* Fülle ein xyz-feld mit den koordinaten einer Flaeche mit gleichen divisions. */
  fillSurf_ba1( s_indx[4], &div_l[bs[4]][0], div_a, div_b, sa1, sa2, sb1, sb2,
       n_uv, n_ba, offs1, offs2, x,y,z, x_ba,y_ba,z_ba);
  
  /* Uebertrage die daten der surf: bs[4] bzw s_indx[4] ins abc-sys. */  
  b= *bmax-1;
  for (a=0; a<*amax; a++)
  {
    for (c=0; c<*cmax; c++)
    {
      n_abc[a**bmax**cmax +b**cmax +c]=n_ba[a**cmax +c];
      x_abc[a**bmax**cmax +b**cmax +c]=x_ba[a**cmax +c];
      y_abc[a**bmax**cmax +b**cmax +c]=y_ba[a**cmax +c];
      z_abc[a**bmax**cmax +b**cmax +c]=z_ba[a**cmax +c];
    }
  }



  /* suchen der surf5 an masterline ml[2] */
  /* und darf nicht unbalanced sein */
  for (i=0; i<body[b_indx].ns; i++)
  {
    n= body[b_indx].s[i];
    if(i!=bs[0]) for (j=0; j<surf[n].nl; j++)
    {
      if((surf[n].l[j]==surf[s_indx[0]].l[ml[2]])&&(surf[n].typ[j]==surf[s_indx[0]].typ[ml[2]]))
      {
        if (unbalance[i]==1)
	{
          errMsg(" ERROR, body %s not meshable, correct div, side:%d (unbal:%d)!\n", body[b_indx].name, i, unbalance[i]);
          goto noMesh;
        }
        bs[5]=i;
        s_indx[5]=n;
#if TEST1
        printf ("sur5:%s \n", surf[s_indx[5]].name);
#endif
      }
    }
  }

  /* Zuordnen der edges von bs[4] zum surf-ba system */
  /* surf-edge b1 entspricht body-edge c */
  /* surf-edge a1 entspricht body-edge a */
  if( body[b_indx].o[bs[5]]=='+')
  {
    if(( surf[s_indx[0]].l[ml[2]]==surf[s_indx[5]].l[1])
    && ( mlt[2]==surf[s_indx[5]].typ[1])) { sb1=1; sa2=2; sb2=3; sa1=0; }
    else if(( surf[s_indx[0]].l[ml[2]]==surf[s_indx[5]].l[2])
    && ( mlt[2]==surf[s_indx[5]].typ[2])) { sb1=2; sa2=3; sb2=0; sa1=1; }
    else if(( surf[s_indx[0]].l[ml[2]]==surf[s_indx[5]].l[3])
    && ( mlt[2]==surf[s_indx[5]].typ[3])) { sb1=3; sa2=0; sb2=1; sa1=2; }
    else if(( surf[s_indx[0]].l[ml[2]]==surf[s_indx[5]].l[0])
    && ( mlt[2]==surf[s_indx[5]].typ[0])) { sb1=0; sa2=1; sb2=2; sa1=3; }
    else
      { printf(" FATAL ERROR, found no edge\n"); exit(-1); }
  }    
  else
  {
    if(( surf[s_indx[0]].l[ml[2]]==surf[s_indx[5]].l[3])
    && ( mlt[2]==surf[s_indx[5]].typ[3])) { sb1=3; sa2=2; sb2=1; sa1=0; }
    else if(( surf[s_indx[0]].l[ml[2]]==surf[s_indx[5]].l[0])
    && ( mlt[2]==surf[s_indx[5]].typ[0])) { sb1=0; sa2=3; sb2=2; sa1=1; }
    else if(( surf[s_indx[0]].l[ml[2]]==surf[s_indx[5]].l[1])
    && ( mlt[2]==surf[s_indx[5]].typ[1])) { sb1=1; sa2=0; sb2=3; sa1=2; }
    else if(( surf[s_indx[0]].l[ml[2]]==surf[s_indx[5]].l[2])
    && ( mlt[2]==surf[s_indx[5]].typ[2])) { sb1=2; sa2=1; sb2=0; sa1=3; }
    else
      { printf(" FATAL ERROR, found no edge\n"); exit(-1); }
  }
  div_a= *cmax-1;
  div_b= *bmax-1;
  offs1= *offs_sa1;
  offs2= *offs_sa2;
 
  /* hilfsspeicher freigeben und neu allocieren */
  free(n_uv);
  free(n_ba);
  free(x);
  free(y);
  free(z);
  free(x_ba);
  free(y_ba);
  free(z_ba);
  if( (n_uv=(int *)malloc( (div_a+1)*(div_b+1)*sizeof(int) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (n_ba=(int *)malloc( (div_a+1)*(div_b+1)*sizeof(int) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (x=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (y=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (z=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (x_ba=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (y_ba=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  if( (z_ba=(double *)malloc( (div_a+1)*(div_b+1)*sizeof(double) ) )==NULL)
  { printf(" ERROR: realloc failure in meshSurfs surf:%s can not be meshed\n\n", surf[j].name);
    goto noMesh; }
  /* Fülle ein xyz-feld mit den koordinaten einer Flaeche mit gleichen divisions. */
  fillSurf_ba1( s_indx[5], &div_l[bs[5]][0], div_a, div_b, sa1, sa2, sb1, sb2,
       n_uv, n_ba, offs1, offs2, x,y,z, x_ba,y_ba,z_ba);

  /* Uebertrage die daten der surf: bs[4] bzw s_indx[4] ins abc-sys. */  
  a= *amax-1;
  for (b=0; b<*bmax; b++)
  {
    for (c=0; c<*cmax; c++)
    {
      n_abc[a**bmax**cmax +b**cmax +c]=n_ba[b**cmax +c];
      x_abc[a**bmax**cmax +b**cmax +c]=x_ba[b**cmax +c];
      y_abc[a**bmax**cmax +b**cmax +c]=y_ba[b**cmax +c];
      z_abc[a**bmax**cmax +b**cmax +c]=z_ba[b**cmax +c];
    }
  }
  free(n_uv);
  free(n_ba);
  free(x);
  free(y);
  free(z);
  free(x_ba);
  free(y_ba);
  free(z_ba);



  /* allocate memory for embeded nodes */
  if((body[b_indx].nod=(int *)realloc((int *)body[b_indx].nod,((*amax-2)*(*bmax-2)*(*cmax-2))*sizeof(int)))==NULL)
  { printf(" ERROR: realloc failure in meshBodies body:%s can not be meshed\n\n", body[b_indx].name); return(-1); }

  /* auffüllen des bodies mit nodes */
  bodyMesh2( cmax, bmax, amax, x_abc, y_abc, z_abc);

  k=0;
  for (a=1; a<*amax-1; a++)
  {
    for (b=1; b<*bmax-1; b++)
    {
      for (c=1; c<*cmax-1; c++)
      {
      xn=x_abc[a**bmax**cmax +b**cmax +c];
      yn=y_abc[a**bmax**cmax +b**cmax +c];
      zn=z_abc[a**bmax**cmax +b**cmax +c];
      nod(  apre, &npre, 0, apre->nmax+1, xn, yn, zn, 0 );
      /* apre->nmax wird in nod um 1 erhoeht!  */
      n_abc[a**bmax**cmax +b**cmax +c]=apre->nmax;
      body[b_indx].nod[k]=apre->nmax;
      k++;
      }
    }
  }
  body[b_indx].nn=k;

  /* durch die offsets beschriebene randbereiche umspeichern */
  /* (nodenr der diagonale den raendern der undefinierten luecken zuordnen) */

  div_a=*amax-1;
  div_b=*bmax-1;
#if TEST1
  printf(" div_a:%d div_b:%d  div_sa1:%d div_sa2:%d off1:%d off2:%d\n", div_a,div_b,*div_sa1,*div_sa2,*offs_sa1, *offs_sa2);  
#endif
  for (c=1; c<*cmax-1; c++)
  {
      a=*div_sa1;
      for (b=1; b<*offs_sa1; b++)
      {
        n_abc[a**bmax**cmax +b**cmax +c]=n_abc[(div_a-b)**bmax**cmax +b**cmax +c];
#if TEST
  printf("1 n(%d,%d,%d):%d -> n(%d,%d,%d):%d\n", a,b,c, n_abc[a**bmax**cmax +b**cmax +c],a,b,c, n_abc[(div_a-b)**bmax**cmax +b**cmax +c]);
#endif
      }
      o=0;
      b=*offs_sa1;
      for (a=*div_sa1+1; a<div_a; a++)
      {
        o++;
        n_abc[a**bmax**cmax +b**cmax +c]=n_abc[a**bmax**cmax +(b-o)**cmax +c];
#if TEST
  printf("2 n(%d,%d,%d):%d -> n(%d,%d,%d):%d\n", a,b,c, n_abc[a**bmax**cmax +b**cmax +c],a,b,c,n_abc[a**bmax**cmax +(b-o)**cmax +c] );
#endif
      }
      o=0;
      a=*div_sa2;
      for (b=div_b-1; b>div_b - *offs_sa2; b--)
      {
        o++;
        n_abc[a**bmax**cmax +b**cmax +c]=n_abc[(div_a-o)**bmax**cmax +b**cmax +c];
#if TEST
  printf("3 n(%d,%d,%d):%d -> n(%d,%d,%d):%d\n", a,b,c, n_abc[a**bmax**cmax +b**cmax +c],a,b,c,n_abc[(div_a-o)**bmax**cmax +b**cmax +c]);
#endif
     }
      o=0;
      b=div_b- *offs_sa2;
      for (a=*div_sa2+1; a<div_a; a++)
      {
        o++;
        n_abc[a**bmax**cmax +b**cmax +c]=n_abc[a**bmax**cmax +(b+o)**cmax +c];
#if TEST
  printf("4 n(%d,%d,%d):%d -> n(%d,%d,%d):%d\n", a,b,c, n_abc[a**bmax**cmax +b**cmax +c],a,b,c,n_abc[a**bmax**cmax +(b+o)**cmax +c] );
#endif
      }
  }



#if TEST
  /* darstellen der ergebnisse */
  for (a=0; a<*amax; a++)
  {
   for (b=0; b<*bmax; b++)
   {
    for (c=0; c<*cmax; c++)
    {
      printf ("n(%d,%d,%d):%d %lf %lf %lf \n", a,b,c, n_abc[a**bmax**cmax +b**cmax +c], x_abc[a**bmax**cmax +b**cmax +c]* scale->w+scale->x, y_abc[a**bmax**cmax +b**cmax +c]* scale->w+scale->y, z_abc[a**bmax**cmax +b**cmax +c]* scale->w+scale->z);
    }
   }
  }
#endif

  *nabc=n_abc;
  return(1);
 noMesh:;
  *nabc=0;
  return(-1);
}





