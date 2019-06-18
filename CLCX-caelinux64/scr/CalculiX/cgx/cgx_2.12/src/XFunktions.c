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

#define TEST        0

extern Display       *dpy;
extern int           dpycells;
extern Colormap      cmap;
extern XColor        *xcolor;
extern unsigned long *pixels_return;
extern unsigned int  npixels;
extern int   basCol[3];
extern double  priv_cmap[256][3];

extern int   w0, w1, w_index, w_rgb;                      /* Fenster identifier  */
extern int   activWindow;                                 /* das aktuelle Fenster */

extern int offset, steps;

void calcOffset()
{
  int i;
  double r,g,b;

  if ((int)CMAP_CELLS_LIGHT+SET_COLS > steps) i=(int)CMAP_CELLS_LIGHT+SET_COLS+BAS_COLS;
  else i=steps+BAS_COLS;

  if ( (pixels_return[0]+i) > dpycells ) offset = (dpycells-i);
  else offset = pixels_return[0];
  if (npixels<1) offset = dpycells-i;
  /* anlegen der Colormap */
  basCol[0]=-1;
  basCol[1]=-1;
  basCol[2]=-1;
  for (i=0; i<256; i++)
  {
    r = xcolor[i].red / 65535.;
    g = xcolor[i].green / 65535.;
    b = xcolor[i].blue / 65535.;
    /* printf ("colIndex %d %lf %lf %lf\n", (int)xcolor[i].pixel, r, g ,b); */
        
    /* get stand alone colors */
    if ((r>0.999)&&(g==r)&&(b==r)&&(basCol[1]==-1)) basCol[1]=i;
    if ((r<0.001)&&(g==r)&&(b==r)&&(basCol[0]==-1)) basCol[0]=i;
    if ((r>0.7)&&(r<0.8)&&(b==r)&&(g==r)&&(basCol[2]==-1)) basCol[2]=i;

    priv_cmap[i][0]=r ;
    priv_cmap[i][1]=g ;
    priv_cmap[i][2]=b ;
    glutSetColor( i, priv_cmap[i][0], priv_cmap[i][1], priv_cmap[i][2] );
  }

  if (basCol[0] < 0) {
    basCol[0]=offset; printf("basCol.bk not found\n");
    priv_cmap[offset][0]=0. ;
    priv_cmap[offset][1]=0. ;
    priv_cmap[offset][2]=0. ;
    glutSetColor( offset, 0., 0., 0. );
    if ( (npixels>0) && ((offset) >= pixels_return[0]) && ((offset)<=pixels_return[npixels-1]) )
    {
      xcolor[offset].pixel = offset;
      xcolor[offset].red   = 0.*65535.;
      xcolor[offset].green = 0.*65535.;
      xcolor[offset].blue  = 0.*65535.;
    }
    offset++;
  }
  else  printf("basCol.bk:%d found\n", basCol[0]);

  if (basCol[1] < 0) {
    basCol[1]=offset; printf("basCol.wt not found\n");
    priv_cmap[offset][0]=1. ;
    priv_cmap[offset][1]=1. ;
    priv_cmap[offset][2]=1. ;
    glutSetColor( offset, 1., 1., 1. );
    if ( (npixels>0) && ((offset) >= pixels_return[0]) && ((offset)<=pixels_return[npixels-1]) )
    {
      xcolor[offset].pixel = offset;
      xcolor[offset].red   = 1.*65535.;
      xcolor[offset].green = 1.*65535.;
      xcolor[offset].blue  = 1.*65535.;
    }
    printf("basCol.wt:%d \n", basCol[1]);
    offset++;
  }
  else printf("basCol.wt:%d found\n", basCol[1]);

  if (basCol[2] < 0) {
    basCol[2]=offset; printf("basCol.gr not found\n");
    priv_cmap[offset][0]=0.7 ;
    priv_cmap[offset][1]=0.7 ;
    priv_cmap[offset][2]=0.7 ;
    glutSetColor( offset, 0.7, 0.7, 0.7 );
    if ( (npixels>0) && ((offset) >= pixels_return[0]) && ((offset)<=pixels_return[npixels-1]) )
    {
      xcolor[offset].pixel = offset;
      xcolor[offset].red   = 0.7*65535.;
      xcolor[offset].green = 0.7*65535.;
      xcolor[offset].blue  = 0.7*65535.;
    }
    printf("basCol.gr:%d \n", basCol[2]);
    offset++;
  }
  else printf("basCol.gr:%d found\n", basCol[2]);
  printf(" npixels:%d offset:%d dpycells:%d\n",npixels,offset,dpycells); 
}

void getColormap()
{
  readStdCmap( &dpy, &dpycells, &cmap, &xcolor, &pixels_return, &npixels, (int)CMAP_CELLS_LIGHT+SET_COLS );
  printf (" freeColCells:%d  1st FreeCell:%ld\n", npixels, pixels_return[0]);


  calcOffset();
}


void storeColors(int anz_col, int pix )
{
  register int i;

  for (i=0; i<anz_col; i++)
    if (pixels_return[i+pix]==0) break;
  anz_col=i;

#if TEST
  printf ("anz_col:%d 1st.cell:%d i:%d \n", anz_col, pix, i );
#endif

  if (anz_col>0)
  {
    XStoreColors( dpy, cmap, &xcolor[pixels_return[pix]], anz_col );
    for (i=pix; i<pix+anz_col; i++)
    {
#if TEST
    printf ("XQueryColor:%d. cell=%lu\n", i-pix, i);
#endif
    XQueryColor( dpy, cmap, &xcolor[pixels_return[i]]);
    }
  }
}
