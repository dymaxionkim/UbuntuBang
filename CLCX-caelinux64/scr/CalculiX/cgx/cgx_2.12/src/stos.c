/* ---------------------------------------------------------------- */
/* ---------------------------------------------------------------- */

#include <extUtil.h>


/* liefert puffer aus string von position a bis b */
void stos(char *string, int a, int b, char *puffer)
{
  register int n, i;

  n=-1;
  for (i=a-1; i<b; i++)
    {
    n++;
    if ((i>=MAX_LINE_LENGTH)||(n>=MAX_LINE_LENGTH)) break;
    puffer[n] = string[i];
  }
  puffer[n+1] = '\0';
}
