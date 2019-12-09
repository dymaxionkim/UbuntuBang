#include <extUtil.h>


/* summiert keystrokes, wenn key= "RETURN" dann 1, sonst 0 */
/*   record ist dann auszuwerten.                           */
int parser( char gkey, char *keystroke, int *curshft )
{
  int i,j,n;
  char echo;

  if (gkey == ( char )0xff0d)  /* RETURN */
  {
    /* new-line, command ready */
    putchar('\n');
    return(1);
  }
  else
  {
    i=strlen(keystroke)+*curshft;
    if (gkey == ( char )0xff08) /* backspace */
    {
      for(j=0; j<strlen(keystroke); j++)
      {
        /* go left */
        echo=( char )0xff08;
        putchar(echo);
      }
      for(j=0; j<strlen(keystroke); j++)
      {
        /* overwrite old command */
        echo=' ';
        putchar(echo);
      }
      for(j=0; j<strlen(keystroke); j++)
      {
        /* go left */
        echo=( char )0xff08;
        putchar(echo);
      }

      n=strlen(keystroke);
      for (j=i-1; j<n; j++) keystroke[j]=keystroke[j+1];

      printf("%s",keystroke);
      for(j=0; j<-*curshft; j++)
      {
        /* go left */
        echo=( char )0xff08;
        putchar(echo);
      }
    }
    else
    {

      for(j=0; j<strlen(keystroke); j++)
      {
        /* go left */
        echo=( char )0xff08;
        putchar(echo);
      }

      n=strlen(keystroke);
      for (j=n; j>=i; j--) keystroke[j+1]=keystroke[j];
      keystroke[i] = gkey;

      printf("%s",keystroke);
      for(j=0; j<-*curshft; j++)
      {
        /* go left */
        echo=( char )0xff08;
        putchar(echo);
      }
      //if (*curshft<0) *curshft=*curshft+1;
    }
    fflush(stdout);
  }
  return(0);
}

