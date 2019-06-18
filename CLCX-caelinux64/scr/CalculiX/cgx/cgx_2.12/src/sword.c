/* ---------------------------------------------------------------- */
/* ---------------------------------------------------------------- */

#include <extUtil.h>

               
/* liest einen string bis ' ' oder '\n'; uebergibt Anzahl gelesene Zeichen */
/* uebergibt wordlaenge */
int sword( char *string, char *word)
{
  int i,n, flag;

    i=n=0;
    while (string[i] == ' ') i++;
    flag = sscanf(string, "%s", word);
    if ((flag <= 0)||(flag == (char)EOF )) { return(-1); }

    while (word[n] != '\0')
      n++;
    return(n);

}


