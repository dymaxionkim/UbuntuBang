/* ---------------------------------------------------------------- */
/* remove an integer from a sorted array */
/* ---------------------------------------------------------------- */
#define TEST 0
#include <extUtil.h>

int iremove(int **ipnt, int n, int x0 )
{
  int i=0,ii,m,n1,n2;
  int *x;
#if TEST
    printf("a:%d\n", *ipnt); 
#endif

  if(n==0) return(n);
  x=*ipnt;

  /* return if x0 is lower than the first elem or if x0 is higher than the last elem */
  if((x0<x[0])||(x0>x[n-1])) return(n);
  else
  {
    /* search the intersection */
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
      /* reduce array by x0 */
      for(ii=i; ii<n-1; ii++)
      {
        x[ii]=x[ii+1];
      }
      n--;

      if((x= (int *)realloc((int *)x, (n+1) * sizeof(int))) == NULL )
        printf("ERROR: realloc failed in iremove()\n\n" );
    }
  }

#if TEST
      for(ii=0; ii<n; ii++)
      {
        printf("i:%d x:%d \n", ii,x[ii]); 
      }
#endif
 
  *ipnt=x;
#if TEST
    printf("b:%d\n", *ipnt); 
#endif
  return(n);
}





