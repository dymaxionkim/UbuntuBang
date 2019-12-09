#include <extUtil.h>


int checkIfNumber( char *string )
{
  int i;
  for(i=0; i<strlen(string); i++)
    if((string[i]<48)||(string[i]>57))
    {
      if((string[i]==46)||(string[i]==45)||(string[i]==43));
      /* exp format (DEde, not at first location) */
      else if((i>0)&&((string[i]==69)||(string[i]==68)||(string[i]==101)||(string[i]==100)));
      else return(0);
    }
  return(1);
}

