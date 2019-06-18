#include <extUtil.h>

/*---------------------------------------------------------------------*/
/* Sortiert ein Datenfeld                                              */
/*                                                                     */
/* wert    : Datenfeld                                                 */
/* wertsort: sortiertes Datenfeld  (groesstes Datum im 1. Element)     */
/* isort   : sortierte Indexe von wert                                 */
/* anzahl  : Anzahl der Elemente                                       */
/*---------------------------------------------------------------------*/

void bsort (double *wert, double *wertsort, int *isort, int anzahl)
{
/*     *****************************************************************   */
/*     * wert()       zu sortierendes feld   */
/*     * wertsort()   sortiertes feld   */
/*     * isort()      sortierte indexe des zu sortierenden feldes   */
/*     *****************************************************************   */


      int     i,j,imax=0;
      double   wertmax,puffer;

      puffer=-1000000000;

      for (i=0; i < anzahl; i++)
      {
        wertsort[i]=wert[i];
      }

      for (i=0; i < anzahl; i++)
      {
        wertmax=puffer;
        for (j=0; j < anzahl; j++)
        {
          if (wertmax < wertsort[j])
          {
            wertmax = wertsort[j];
            imax=j;
          }
        }
        isort[i]=imax;
        wertsort[imax]=puffer;
      }

      for (i=0; i < anzahl; i++)
      {
        wertsort[i]=wert[isort[i]];
      }
}

