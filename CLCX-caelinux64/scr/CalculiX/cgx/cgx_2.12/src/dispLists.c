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

/*
TODO:
*/

#include <cgx.h>

#define     TEST            0

extern Edges     *edge;
extern Elements  *e_enqire;
extern Nodes     *node;
extern Faces     *face;
extern SumGeo    anzGeo[1];
extern Sets      *set;
extern Psets     *pset;
extern Summen    anz[1];

extern double foregrndcol_rgb[4];

extern double      ds;





void drawModelEdges( GLuint list_model_edges, int color, double width, int numEdges, Nodes *node )
{
  int i;

  if(!numEdges)
  {
    glNewList( list_model_edges, GL_COMPILE );
    glEndList();
    return;
  }

  glNewList( list_model_edges, GL_COMPILE );
#if INX_MODE
  glIndexi    ( color );
#endif
#if TEX_MODE
  glColor3d(color,color,color);
#endif
  glLineWidth(width);
   glBegin ( GL_LINES );
   for (i=0; i<numEdges; i++ )
   {
      glVertex3dv ( &node[edge[i].p1].nx );
      glVertex3dv ( &node[edge[i].p2].nx );
   }
   glEnd();
  glEndList();
}



void drawDispListEdges( GLuint list, int color, double width, char key, Nodes *node )
{
  int j;

  if(!anz->n)
  {
    glNewList( list, GL_COMPILE );
    glEndList();
    return;
  }
  if((key=='f')&&(!anz->f)) return;
  if((key=='e')&&(!anz->e)) return;
  if(!anzGeo->psets) return;

  glNewList( list, GL_COMPILE );

  /* glLineWidth(width) not implemented */

  for (j=0; j<anzGeo->psets; j++ )
  {
    if (pset[j].type[0]==key)
    {
      if(key=='f') drawFaces_edge( set[pset[j].nr].anz_f, set[pset[j].nr].face, node, face, color, pset[j].type[1] );
      if(key=='e') drawElem_edge( set[pset[j].nr].anz_e, set[pset[j].nr].elem, node, e_enqire, color, pset[j].type[1] );
    }
  }
  glEndList();
}



#if INX_MODE
void drawDispList( GLuint list, char key, Nodes *node, int *colNr )
#endif
#if TEX_MODE
void drawDispList( GLuint list, char key, Nodes *node, double *colNr )
#endif
{
  int j;

  if(!anz->n)
  {
    glNewList( list, GL_COMPILE );
    glEndList();
    return;
  }
  if((key=='f')&&(!anz->f)) return;
  if((key=='e')&&(!anz->e)) return;
  if(!anzGeo->psets) return;

  glNewList( list, GL_COMPILE );
  for (j=0; j<anzGeo->psets; j++ )
  {
    if (pset[j].type[0]==key)
    {
      if(key=='f') drawFaces_plot( set[pset[j].nr].anz_f, set[pset[j].nr].face, node, colNr, face, pset[j].col, pset[j].type[1],!PICK );
      if(key=='e') drawElements_plot( set[pset[j].nr].anz_e, set[pset[j].nr].elem, node, colNr, e_enqire, pset[j].col, pset[j].type[1],!PICK );
    }
  }
  glEndList();
}


