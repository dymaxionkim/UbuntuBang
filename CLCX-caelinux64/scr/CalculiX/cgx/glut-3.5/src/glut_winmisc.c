/* Copyright (c) Mark J. Kilgard, 1994.  */

/* This program is freely distributable without licensing fees
   and is provided without guarantee or warrantee expressed or
   implied. This program is -not- in the public domain. */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#if !defined(WIN32)
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>  /* for XA_STRING atom */
#else
extern int __glutMinWindowWidth;
extern int __glutMinWindowHeight;
#endif /* !WIN32 */

#include <GL/glut_cgx.h>
#include "glutint.h"

/* CENTRY */
void APIENTRY 
glutSetWindowTitle(const char *title)
{
  XTextProperty textprop;

  assert(!__glutCurrentWindow->parent);
  textprop.value = (unsigned char *) title;
  textprop.encoding = XA_STRING;
  textprop.format = 8;
  textprop.nitems = strlen(title);
  XSetWMName(__glutDisplay,
    __glutCurrentWindow->win, &textprop);
  XFlush(__glutDisplay);
}

void APIENTRY 
glutSetIconTitle(const char *title)
{
  XTextProperty textprop;

  assert(!__glutCurrentWindow->parent);
  textprop.value = (unsigned char *) title;
  textprop.encoding = XA_STRING;
  textprop.format = 8;
  textprop.nitems = strlen(title);
  XSetWMIconName(__glutDisplay,
    __glutCurrentWindow->win, &textprop);
  XFlush(__glutDisplay);
}

void APIENTRY 
glutPositionWindow(int x, int y)
{
  __glutCurrentWindow->desiredX = x;
  __glutCurrentWindow->desiredY = y;
  __glutCurrentWindow->desiredConfMask |= CWX | CWY;
  __glutPutOnWorkList(__glutCurrentWindow, GLUT_CONFIGURE_WORK);
}

void APIENTRY 
glutReshapeWindow(int w, int h)
{
  if (w <= 0 || h <= 0)
    __glutWarning("glutReshapeWindow: non-positive width or height not allowed");

#if defined(WIN32)
  if (w < __glutMinWindowWidth && !__glutCurrentWindow->parent)
    __glutWarning("requested width is less than minimum allowed.");
  if (h < __glutMinWindowHeight && !__glutCurrentWindow->parent)
    __glutWarning("requested height is less than minimum allowed.");
#endif /* WIN32 */

  __glutCurrentWindow->desiredWidth = w;
  __glutCurrentWindow->desiredHeight = h;
  __glutCurrentWindow->desiredConfMask |= CWWidth | CWHeight;
  __glutPutOnWorkList(__glutCurrentWindow, GLUT_CONFIGURE_WORK);
}

void APIENTRY 
glutPopWindow(void)
{
  __glutCurrentWindow->desiredStack = Above;
  __glutCurrentWindow->desiredConfMask |= CWStackMode;
  __glutPutOnWorkList(__glutCurrentWindow, GLUT_CONFIGURE_WORK);
}

void APIENTRY 
glutPushWindow(void)
{
  __glutCurrentWindow->desiredStack = Below;
  __glutCurrentWindow->desiredConfMask |= CWStackMode;
  __glutPutOnWorkList(__glutCurrentWindow, GLUT_CONFIGURE_WORK);
}

void APIENTRY 
glutIconifyWindow(void)
{
  assert(!__glutCurrentWindow->parent);
  __glutCurrentWindow->desiredMapState = IconicState;
  __glutPutOnWorkList(__glutCurrentWindow, GLUT_MAP_WORK);
}

void APIENTRY 
glutShowWindow(void)
{
  __glutCurrentWindow->desiredMapState = NormalState;
  __glutPutOnWorkList(__glutCurrentWindow, GLUT_MAP_WORK);
}

void APIENTRY 
glutHideWindow(void)
{
  __glutCurrentWindow->desiredMapState = WithdrawnState;
  __glutPutOnWorkList(__glutCurrentWindow, GLUT_MAP_WORK);
}

/* ENDCENTRY */
