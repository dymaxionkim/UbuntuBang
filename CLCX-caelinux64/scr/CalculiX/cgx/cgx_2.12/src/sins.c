/* ---------------------------------------------------------------- */
/* ---------------------------------------------------------------- */

#include <extUtil.h>

/* schreibt string in puffer von position a bis b */
/* return liefert die geschriebenen Zeichen */
int sins(char *string, int a, int b, char *puffer)
{
  int n, i, length;
  
  length=strlen(string);
  n=-1;
  for (i=a-1; i<b; i++)
  {
    n++;
    if ((i>=MAX_LINE_LENGTH)||(n>=MAX_LINE_LENGTH)) { return(n+1); break; }
    if(n<length) puffer[i] = string[n];
    else puffer[i] = ' ';
  }
  return(n+1);
}

