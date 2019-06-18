/* ---------------------------------------------------------------- */
/* finds an integer in a sorted array */
/* return index or if not found: */
/* -1 if lower or -2 if higher than defined else -3 */
/* ---------------------------------------------------------------- */
#define TEST 0
#include <extUtil.h>

int ifind(int **ipnt, int n, int x0 )
{
  int i=0,ii,m,n1,n2;
  int *x;
#if TEST
    printf("a:%d\n", *ipnt); 
#endif

  if(n<1)
  {
    /* no array  */
    return(-3);
  }

  x=*ipnt;
  /* if x0 is lower than the first elem */
  if(x0<x[0])
  {
    /* is no member of array  */
    return(-1);
  }

  /* if x0 is higher than the last elem */
  else if(x0>x[n-1])
  {
    /* is no member of array  */
    return(-2);
  }

  else
  {
    /* search the intersection, regula falsi */
    n1=0;                              
    n2=n;                            
    for(ii=0; ii<n; ii++)
    {                     
      m=(n2+n1)/2;                      
      if(x0>= x[m] ) n1=m;              
      if(x0 < x[m] ) n2=m;              
      if((n2-n1) == 1) break;           
    }                                 
    i=n1;
#if TEST
    printf("i:%d x:%d x0:%d x++:%d\n", i,x[i],x0,x[i+1]); 
#endif

    if (x0==x[i])
    {
      /* is member of array at pos i  */
      return(i);
    }
  }
  /* is no member of array  */
  return(-3);
}





