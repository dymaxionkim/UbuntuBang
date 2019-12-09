#include <extUtil.h>

/*
  8.8.99 wi

  readStdCmap allociert den groessten noch freien zusammenhaengenden Speicherbereich,
  bzw. anzCells wenn mehr als anzCells frei ist, 
  in der default colormap (max 256 colorcells). Und liefert den Inhalt der colormap.

*/

#define TEST        0
#define MAX_BLOCKS  300
#define MAX_CELLS   256
#define MAX_PLANES  100

void readStdCmap( Display **ptr_dpy, int *ptr_dpycells, Colormap *ptr_cmap, XColor **ptr_c,
                 unsigned long **ptr_pix, unsigned int *ptr_npixels, int anzCells )
  {
  register int i, j, k, n, nmax;
  int maxBlock=0, maxCells=0;

  Display *dpy;                              /* **ptr_dpy (Display Nr.) */
  int      scn;
  XColor *xcolor;                            /* **ptr_c (full actual colormap) */

  int           dpycells;                    /* *ptr_dpycells (amount of colormap-cells) */
  Colormap      cmap;
  unsigned int  npixels;                     /* *ptr_npixels (amount of allocated colormap-cells */

  unsigned long plane_masks_return[MAX_PLANES];
  unsigned long *pixels_return;              /* **ptr_pix (all allocated colormap-cells)*/
  unsigned int  nplanes, status;

  unsigned long pix_return[MAX_BLOCKS][MAX_CELLS];
  unsigned int  npix[MAX_BLOCKS];


  nplanes= 0;

  /* get a connection */
  dpy = XOpenDisplay (0);

  /* get the default screen */
  scn = DefaultScreen (dpy);

  /* create a colormap */
  cmap = XDefaultColormap( dpy, scn);

  /* Nr of available DisplayCells in the default colormap*/
  dpycells = DisplayCells( dpy, scn );

  /* read the colormap */
  /* max color = 65535 */
  if ( ( xcolor = (XColor *)malloc( (dpycells+1) * sizeof(XColor))) == NULL )
    printf("\n\n ERROR: malloc Fehler in readStdCmap (xcolor)\n\n" ) ;
  else
    printf ("\n xcolor in readStdCmap allocated \n");

  
  for (i=0; i<dpycells; i++)
    {
    xcolor[i].pixel=(unsigned long)i;
    XQueryColor(dpy, cmap, &xcolor[i]);
    }
  
  /* determine and allocate the biggest contiguous range of colorcells */

  if ( ( pixels_return = (unsigned long *)malloc( (dpycells+1) * sizeof(unsigned long))) == NULL )
    printf("\n\n ERROR: malloc Fehler in readStdCmap (pixels_return)\n\n" ) ;
  else
    printf ("\n pixels_return in readStdCmap allocated \n");
  for (i=0; i<dpycells; i++) pixels_return[i]=(unsigned long)0;

  /* look how much collor-cells are free (not necessarily continous) */
  for (npixels=1; npixels<dpycells; npixels++)
    {
    status = XAllocColorCells( dpy, cmap, 1, plane_masks_return, nplanes, pixels_return, npixels );
#if TEST
    printf ("status:%u: found %u free cells, 1st cell:%u\n",  status, npixels, pixels_return[0]);
#endif
    if (status==0)  break;
    XFreeColors( dpy, cmap, pixels_return, npixels, nplanes );
    }
#if TEST
  printf ("status:%u: %u cells could not be allocated\n", status, npixels );
#endif

  n=1; nmax=0, j=0;
  /* check one cell to much (npixels) to make shure that the last cont. block will also be allocated */
  for (i=1; i<npixels; i++) 
    {
    /* look if the cells are continuous */
    if (pixels_return[i]==pixels_return[i-1]+1)
      {
#if TEST
      printf("pixels_return[%d]=%u %d\n", i, pixels_return[i], pixels_return[i]);
#endif
      n++ ;
      }
    else
      {
#if TEST
      printf(" not continous: pixels_return[%d]=%u pixels_return[%d-1]+1=%u\n",
        i, pixels_return[i], i, pixels_return[i-1]+1);
#endif
      /* allocate the continuous block */
      if (n<anzCells) nmax=n; else nmax=anzCells;
      npix[j]=nmax;
      status = 0;
      status = XAllocColorCells( dpy, cmap, 1, plane_masks_return, nplanes,
                                 pix_return[j], npix[j] );
      if (status==0)
        {
        printf ("WARNING: Could not alloc %d color cells for step=%d! \n", nmax, j);
        npix[j]=0;
        }
#if TEST
      else printf (" Block:%d with %d cells, start with cell %d allocated\n",
        j, npix[j], pix_return[j][0] ); 
#endif
      /* keep track of the biggest block */
      if (maxCells<npix[j]) { maxCells=npix[j]; maxBlock=j; }
      j++;
      if (nmax==anzCells) goto found_enough;
      n=1;
      }
    }
  found_enough:;
  for (i=0; i<j; i++)
    {
    if (maxBlock==i)
      {
#if TEST
      printf ("biggest continuous block %d with %u cells saved\n", maxBlock, maxCells );
#endif
      for (k=0; k<npix[i]; k++)
        {
        pixels_return[k]=pix_return[i][k];
#if TEST
        printf ("%d. cell=%lu\n", k, pixels_return[k]);
#endif
        }
      }
    else
      {
      XFreeColors( dpy, cmap, pix_return[i], npix[i], nplanes );
#if TEST
      printf ("free block %d with %u cells\n", maxBlock, maxCells );
      for (k=0; k<npix[i]; k++)
        {
        printf ("%d. cell=%lu\n", k, pix_return[i][k]);
        }
#endif
      }
    }

  if (nmax==1) nmax=0;
  /* return pointer */
  *ptr_c   = xcolor;
  *ptr_dpy = dpy;
  *ptr_pix = pixels_return;
  *ptr_dpycells = dpycells;
  *ptr_cmap     = cmap;
  *ptr_npixels  = maxCells;


  /* nice to know .. */

  printf ("DefaultDepth=%d\n", DefaultDepth( dpy, scn ));
  printf ("DisplayCells=%d\n", dpycells);
  printf ("DisplayPlanes=%d\n", DisplayPlanes( dpy, scn ));

}



