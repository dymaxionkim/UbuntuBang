
/* Copyright (c) Nate Robins, 1997. */

/* This program is freely distributable without licensing fees 
   and is provided without guarantee or warrantee expressed or 
   implied. This program is -not- in the public domain. */


#include <stdio.h>
#include "win32_glx.h"

/* global current HDC */
extern HDC XHDC;

void
glXDestroyContext(Display* display, GLXContext context)
{
  /* nothing magic about this */
  wglDeleteContext(context);
}

GLXContext
glXCreateContext(Display* display, XVisualInfo* visinfo,
		 GLXContext share, Bool direct)
{
  /* KLUDGE: GLX really expects a display pointer to be passed in as
     the first parameter, but Win32 needs an HDC instead, so BE SURE
     that the global XHDC is set before calling this routine. */

  HGLRC context;

//   printf("glXCreateContext(0x%x, 0x%x, 0x%x, %d)\n", display, visinfo, 
// 	 share, direct);
//   printf("context = 0x%x, XHDC = 0x%x\n", context, XHDC);

  context = wglCreateContext(XHDC);


  if (share)
    wglShareLists(share, context);

  /* since direct rendering is implicit, the direct flag is
     ignored. */

  return context;
}

Bool
glXIsDirect(Display* display, GLXContext context)
{
  /* Win32 is ALWAYS direct rendering.  There is no notion of indirect
     rendering in Win32, therefore always return True. */
  return True;
}

void
glXSwapBuffers(Display* display, Window window)
{
  /* not too much magic here - it is another function call layer,
     though...eliminating it would be faster. */
  //  printf("glXSwapBuffers(0x%x, 0x%x)\n", display, window);

  SwapBuffers(GetDC(window));
}

Bool
glXMakeCurrent(Display* display, Window window, GLXContext context)
{
  //  printf("glXMakeCurrent(0x%x, 0x%x, 0x%x)\n", display, window, context);
  return wglMakeCurrent(GetDC(window), context);
}

int
glXGetConfig(Display* display, XVisualInfo* visual, int attrib, int* value)
{
  if (!visual)
    return GLX_BAD_VISUAL;

  switch (attrib) {
  case GLX_USE_GL:
    *value = visual->dwFlags & PFD_SUPPORT_OPENGL;
    break;
  case GLX_BUFFER_SIZE:
    /* KLUDGE: if we're RGBA, return the number of bits/pixel,
       otherwise, return 8 (we guessed at 256 colors in CI mode). */
    if (visual->iPixelType == PFD_TYPE_RGBA)
      *value = visual->cColorBits;
    else
      *value = 8;
    break;
  case GLX_LEVEL:
    /* the bReserved flag of the pfd contains the overlay/underlay
       info. */
    *value = visual->bReserved;
    break;
  case GLX_RGBA:
    *value = visual->iPixelType == PFD_TYPE_RGBA;
    break;
  case GLX_DOUBLEBUFFER:
    *value = visual->dwFlags & PFD_DOUBLEBUFFER;
    break;
  case GLX_STEREO:
    *value = visual->dwFlags & PFD_STEREO;
    break;
  case GLX_AUX_BUFFERS:
    *value = visual->cAuxBuffers;
    break;
  case GLX_RED_SIZE:
    *value = visual->cRedBits;
    break;
  case GLX_GREEN_SIZE:
    *value = visual->cGreenBits;
    break;
  case GLX_BLUE_SIZE:
    *value = visual->cBlueBits;
    break;
  case GLX_ALPHA_SIZE:
    *value = visual->cAlphaBits;
    break;
  case GLX_DEPTH_SIZE:
    *value = visual->cDepthBits;
    break;
  case GLX_STENCIL_SIZE:
    *value = visual->cStencilBits;
    break;
  case GLX_ACCUM_RED_SIZE:
    *value = visual->cAccumRedBits;
    break;
  case GLX_ACCUM_GREEN_SIZE:
    *value = visual->cAccumGreenBits;
    break;
  case GLX_ACCUM_BLUE_SIZE:
    *value = visual->cAccumBlueBits;
    break;
  case GLX_ACCUM_ALPHA_SIZE:
    *value = visual->cAccumAlphaBits;
    break;
  default:
    return GLX_BAD_ATTRIB;
  }

  return 0;
}

XVisualInfo*
glXChooseVisual(Display* display, int screen, int* attribList)
{
  /* KLUDGE: since we need the HDC, MAKE SURE to set XHDC before
     calling this routine. */

  int* p = attribList;
  int pf;
  PIXELFORMATDESCRIPTOR pfd;
  PIXELFORMATDESCRIPTOR* match = NULL;
  int stereo = 0;

  /* avoid seg-faults */
  if (!p)
    return NULL;

  memset(&pfd, 0, sizeof(PIXELFORMATDESCRIPTOR));
  pfd.nSize = (sizeof(PIXELFORMATDESCRIPTOR));
  pfd.nVersion = 1;

  /* defaults */
  pfd.dwFlags = PFD_SUPPORT_OPENGL | PFD_DRAW_TO_WINDOW;
  pfd.iPixelType = PFD_TYPE_COLORINDEX;
  pfd.cColorBits = 32;
  pfd.cDepthBits = 1;

  while(*p) {
    switch(*p) {
    case GLX_USE_GL:
      pfd.dwFlags |= PFD_SUPPORT_OPENGL;
      break;
    case GLX_BUFFER_SIZE:
      pfd.cColorBits = *(++p);
      break;
    case GLX_LEVEL:
      /* the bReserved flag of the pfd contains the overlay/underlay
	 info. */
      pfd.bReserved = *(++p);
      break;
    case GLX_RGBA:
      pfd.iPixelType = PFD_TYPE_RGBA;
      break;
    case GLX_DOUBLEBUFFER:
      pfd.dwFlags |= PFD_DOUBLEBUFFER;
      break;
    case GLX_STEREO:
      stereo = 1;
      pfd.dwFlags |= PFD_STEREO;
      break;
    case GLX_AUX_BUFFERS:
      pfd.cAuxBuffers = *(++p);
      break;
    case GLX_RED_SIZE:
      pfd.cRedBits = 8;			/* try to get the maximum */
      ++p;
      break;
    case GLX_GREEN_SIZE:
      pfd.cGreenBits = 8;
      ++p;
      break;
    case GLX_BLUE_SIZE:
      pfd.cBlueBits = 8;
      ++p;
      break;
    case GLX_ALPHA_SIZE:
      pfd.cAlphaBits = 8;
      ++p;
      break;
    case GLX_DEPTH_SIZE:
      pfd.cDepthBits = 32;
      ++p;
      break;
    case GLX_STENCIL_SIZE:
      pfd.cStencilBits = *(++p);
      break;
    case GLX_ACCUM_RED_SIZE:
      pfd.cAccumRedBits = *(++p);
      break;
    case GLX_ACCUM_GREEN_SIZE:
      pfd.cAccumGreenBits = *(++p);
      break;
    case GLX_ACCUM_BLUE_SIZE:
      pfd.cAccumBlueBits = *(++p);
      break;
    case GLX_ACCUM_ALPHA_SIZE:
      pfd.cAccumAlphaBits = *(++p);
      break;
    }
    ++p;
  }

//   printf("wanted:\n");
//   glXPrintPixelFormat(0, &pfd);
//   printf("got:\n");

  /* let Win32 choose one for us */
  pf = ChoosePixelFormat(XHDC, &pfd);
  if (pf > 0) {
    match = (PIXELFORMATDESCRIPTOR*)malloc(sizeof(PIXELFORMATDESCRIPTOR));
    DescribePixelFormat(XHDC, pf, sizeof(PIXELFORMATDESCRIPTOR), match);
//     glXPrintPixelFormat(pf, match);

    /* ChoosePixelFormat is dumb in that it will return a pixel format
       that doesn't have stereo even if it was requested...so we need
       to make sure that if stereo was selected, we got it. */
    if (stereo) {
      if (!(match->dwFlags & PFD_STEREO)) {
	free(match);
	match = NULL;
      }
    }
  }

  return match;
}

void
glXPrintPixelFormat(int pf, PIXELFORMATDESCRIPTOR* pfd)
{
  printf("   visual  x  bf lv rg d st  r  g  b a  ax dp st accum buffs  ms \n");
  printf(" id dep cl sp sz l  ci b ro sz sz sz sz bf th cl  r  g  b  a ns b\n");
  printf("-----------------------------------------------------------------\n");

  printf("0x%02x ", pf);
  printf("%2d ", pfd->cColorBits);
  if(pfd->dwFlags & PFD_DRAW_TO_WINDOW)      printf("wn ");
  else if(pfd->dwFlags & PFD_DRAW_TO_BITMAP) printf("bm ");
  else printf(".  ");
  printf(" . "); // x sp = transparent ???
  printf("%2d ", pfd->cColorBits);
  //	if(pfd->iLayerType) printf(" %d ", pfd->iLayerType);  // bReserved --v
  if(pfd->bReserved) printf(" %d ", pfd->bReserved);
  else printf(" . "); 
  printf(" %c ", pfd->iPixelType == PFD_TYPE_RGBA ? 'r' : 'c');
  printf("%c ", pfd->dwFlags & PFD_DOUBLEBUFFER ? 'y' : '.');
  printf(" %c ", pfd->dwFlags & PFD_STEREO ? 'y' : '.');
  if(pfd->cRedBits)        printf("%2d ", pfd->cRedBits);
  else printf(" . ");
  if(pfd->cGreenBits)      printf("%2d ", pfd->cGreenBits);
  else printf(" . ");
  if(pfd->cBlueBits)       printf("%2d ", pfd->cBlueBits);
  else printf(" . ");
  if(pfd->cAlphaBits)      printf("%2d ", pfd->cAlphaBits);
  else printf(" . ");
  if(pfd->cAuxBuffers)     printf("%2d ", pfd->cAuxBuffers);
  else printf(" . ");
  if(pfd->cDepthBits)      printf("%2d ", pfd->cDepthBits);
  else printf(" . ");
  if(pfd->cStencilBits)    printf("%2d ", pfd->cStencilBits);
  else printf(" . ");
  if(pfd->cAccumRedBits)   printf("%2d ", pfd->cAccumRedBits);
  else printf(" . ");
  if(pfd->cAccumGreenBits) printf("%2d ", pfd->cAccumGreenBits);
  else printf(" . ");
  if(pfd->cAccumBlueBits)  printf("%2d ", pfd->cAccumBlueBits);
  else printf(" . ");
  if(pfd->cAccumAlphaBits) printf("%2d ", pfd->cAccumAlphaBits);
  else printf(" . ");
  printf(" . .\n");  // multisample stuff
}
