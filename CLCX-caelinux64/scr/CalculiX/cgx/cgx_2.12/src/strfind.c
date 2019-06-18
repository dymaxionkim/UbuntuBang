#include "extUtil.h"

int strfind (char *as1, char *as2)
{
  int i, length_as1, length_as2;

  length_as1 = strlen(as1);
  length_as2 = strlen(as2);

  for(i=0; i<length_as1; i++)
  {
    if(as1[i]==as2[0]) if(compare(&as1[i], as2, length_as2)==length_as2) return(i);
  }
  return(-1);
}
