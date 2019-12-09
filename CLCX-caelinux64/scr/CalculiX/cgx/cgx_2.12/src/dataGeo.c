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

void descalPoints ( int anz_p, Points *point, Scale *scale )
{
  register int  i;
  for (i=0; i<anz_p; i++ )
    {
    /*  descalieren  */
    point[i].px = (point[i].px* scale->w+scale->x);
    point[i].py = (point[i].py* scale->w+scale->y);
    point[i].pz = (point[i].pz* scale->w+scale->z);
    }
}


void scalPoints ( int anz_p,  Points *point, Scale *scale )
{
  register int  i;
  for (i=0; i<anz_p; i++ )
    {
    /* scalieren  */
    point[i].px = (point[i].px-scale->x)/scale->w;
    point[i].py = (point[i].py-scale->y)/scale->w;
    point[i].pz = (point[i].pz-scale->z)/scale->w;
    }
}

void descalSurfs ( int anz_s,  Gsur *surf, Scale *scale )
{
  register int  i,j,n,k;
  for (i=0; i<anz_s; i++ )
  {
    n=0;
    while((surf[i].npgn-n))
    {
      n++; /* jump over the polygon token (ie.GL_POLYGON_TOKEN) */
      j=surf[i].pgn[n++];
      n+=3;
      for(k=0; k<j; k++)
      {
        surf[i].pgn[n]=(surf[i].pgn[n]*scale->w)+scale->x; 
        surf[i].pgn[n+1]=(surf[i].pgn[n+1]*scale->w)+scale->y; 
        surf[i].pgn[n+2]=(surf[i].pgn[n+2]*scale->w)+scale->z;
        n+=3; 
      }
    }
  }
}

void scalSurfs ( int anz_s,  Gsur *surf, Scale *scale )
{
  register int  i,j,n,k;
  for (i=0; i<anz_s; i++ )
  {
    /* printf("surf:%s w:%lf xyz: %lf %lf %lf\n",surf[i].name,scale->w,scale->x,scale->y,scale->z); */
    n=0;
    while((surf[i].npgn-n))
    {
      n++; /* jump over the polygon token (ie.GL_POLYGON_TOKEN) */
      j=surf[i].pgn[n++];
      n+=3;
      for(k=0; k<j; k++)
      {
        /* printf(" %lf %lf %lf  -> ", surf[i].pgn[n],surf[i].pgn[n+1],surf[i].pgn[n+2]); */
        surf[i].pgn[n]=(surf[i].pgn[n]-scale->x)/scale->w; 
        surf[i].pgn[n+1]=(surf[i].pgn[n+1]-scale->y)/scale->w; 
        surf[i].pgn[n+2]=(surf[i].pgn[n+2]-scale->z)/scale->w; 
        n+=3;
        /* printf(" %lf %lf %lf\n", surf[i].pgn[n-3],surf[i].pgn[n-2],surf[i].pgn[n-1]); */ 
      }
    }
  }
}
