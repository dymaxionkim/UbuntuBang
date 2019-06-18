#include <math.h>
#include <string.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include  <time.h>



void stopClock(int zaeler)
/*********************************************************/
/*********************************************************/
{
  static clock_t t0, t1 ;
  static int flag;
  static int start, stop;
  double  zeitspanne;

  if (!flag)
  {
    flag=!flag;
    start=zaeler;
    t0=clock() ;
  }
  else
  {
    flag=!flag;
    stop=zaeler;
    t1=clock() ;
    zeitspanne = (double)(t1-t0)/CLOCKS_PER_SEC;
    printf (" Zeitspanne:    %lf sec \n", zeitspanne);
    printf (" Ereignisse/sec %lf     \n", (stop-start)/zeitspanne);
  }
}




