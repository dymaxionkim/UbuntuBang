/* ---------------------------------------------------------------- */
/* insert an integer in a sorted array */
/* ---------------------------------------------------------------- */
#define TEST 0
#include <extUtil.h>

int iinsert(int **ipnt, int n, int x0 )
{
  int i=0,ii,m,n1,n2;
  int *x;
#if TEST
    printf("a:%d\n", *ipnt); 
#endif

  x=*ipnt;
  /* if x0 is lower than the first elem */
  if((n==0)||(x0<x[0]))
  {
    /* extend array by x0 */
    if((x= (int *)realloc((int *)x, (n+1) * sizeof(int))) == NULL )
      printf("ERROR: realloc failed in iinsert()\n\n" );
    for(ii=n; ii>0; ii--)
    {
      x[ii]=x[ii-1];
    }
    x[0]=x0;
    n++;
  }

  /* if x0 is higher than the last elem */
  else if(x0>x[n-1])
  {
    /* extend array by x0 */
    if((x= (int *)realloc((int *)x, (n+1) * sizeof(int))) == NULL )
      printf("ERROR: realloc failed in iinsert()\n\n" );
    x[n]=x0;
    n++;
  }

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

    if (x0!=x[i])
    {
      /* extend array by x0 */
      if((x= (int *)realloc((int *)x, (n+1) * sizeof(int))) == NULL )
        printf("ERROR: realloc failed in iinsert()\n\n" );
      for(ii=n; ii>i+1; ii--)
      {
        x[ii]=x[ii-1];
      }
      x[i+1]=x0;
      n++;
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





