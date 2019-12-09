#include <extUtil.h>

/*---------------------------------------------------------------------*/
/* Sortiert ein Datenfeld                                              */
/*                                                                     */
/* wert    : Datenfeld                                                 */
/* isort   : sortierte Indexe von wert (1.wert==kleinster Wert)        */
/* anzahl  : Anzahl der Elemente                                       */
/* n       : die n naechsten datenindexe werden gesucht                */
/*---------------------------------------------------------------------*/

#define MAX_VALUE 1.e32;

int *bsortf(double *wert, int anzahl, int n )
{

      static int *isort;
      int     i,j;
      double   *wertsort, wertmin;

      if ( (wertsort = (double *)malloc( (anzahl+1) * sizeof(double))) == NULL )
        printf("\n\n ERROR: malloc failed in bsort: wertsort\n\n" );
      if ( (isort = (int *)realloc( isort, (anzahl+1) * sizeof(int))) == NULL )
        printf("\n\n ERROR: realloc failed in bsort: isort\n\n" );


      for (i=0; i < anzahl; i++)
      {
        wertsort[i]=wert[i];
      }

      for (i=0; i < n; i++)
      {
        wertmin=MAX_VALUE;
        for (j=0; j < anzahl; j++)
        {
          if (wertmin > wertsort[j])
          {
            wertmin = wertsort[j];
            isort[i]=j;
          }
        }
        wertsort[isort[i]]=MAX_VALUE;
      }
      free(wertsort);
      return(isort);
}



