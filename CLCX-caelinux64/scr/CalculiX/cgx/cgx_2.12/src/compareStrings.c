#include <extUtil.h>
/*---------------------------------------------------------------------*/
/* Strings vergleichen (laenge und sequenz)                            */
/* return(-1) if both strings are not of the same length               */
/* else return(0) if they are not of the same sequence                 */
/* else return(stringlength) if they are equal                        */
/*---------------------------------------------------------------------*/

int compareStrings (char *str1, char *str2)
{
  int     i=0, length;

  length=strlen(str1);
  if(strlen(str2)!=length) return(-1);
  while ((str1[i]==str2[i]) && (i<length)) i++;
  if (i==length) return(i);
  else  return(0);
}

