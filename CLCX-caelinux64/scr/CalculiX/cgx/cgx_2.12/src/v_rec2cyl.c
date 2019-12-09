
#include <extUtil.h>


/* --------------------------------------------------------------------------  */
/* Berechnet Zylinderkoordinaten aus Kartesischen Koordinaten                  */
/* Input:                                                                      */
/* pr == xyz Koordinaten des Punktes                                           */
/* axis == Komponente von pr die Drehachse ist                                 */
/* Output:                                                                     */
/* csys == Kartesisches Achsen-System auf dem die Zylinderkoordinaten beruhen  */
/*         ( csys[0] ist immer Drehachse, der winkel wird zwischen csys[1] und */
/*         csys[2] aufgespannt)                                                */
/* pc == rtx Koordinaten des Punktes                                           */
/*                                                                             */
/* Output:                                                                     */
/* if return=1                                                                 */
/* pc[0] = sqrt(csys[2]**2+csys[1]**2)                                         */
/* pc[1] = atan(csys[2]/csys[1])                                               */
/* pc[2] = csys[0]]                                                            */
/* if return=-1                                                                */
/*   axis not known                                                            */
/* if return=-2                                                                */
/*   point is very close to the axis, no angle can be calculated               */
/*                                                                             */
/* --------------------------------------------------------------------------  */

#define VERYCLOSE  1.e-10

int v_rec2cyl( double *pr, int axis, int *csys, double *pc )
{

        if( axis==0)
        {
          csys[0]=0; /* Achse   == x */
          csys[1]=1; /* 2D x    == y */
          csys[2]=2; /* 2D y    == z */
        }
        else if( axis==1)
        {
          csys[0]=1; /* Achse   == y */ 
          csys[1]=2; /* 2D x    == x */
          csys[2]=0; /* 2D y    == z */ 
        }
        else if( axis==2)
        {
          csys[0]=2; /* Achse   == z */  
          csys[1]=0; /* 2D x    == y */
          csys[2]=1; /* 2D y    == x */
        }
        else 
        {
          return(-1);
        }
  
        /* ignore the point if it is on the axis */
        if((pr[csys[1]]*pr[csys[1]]<VERYCLOSE )&&(pr[csys[2]]*pr[csys[2]]<VERYCLOSE)) return(-2);
  
        /* transformation into the cylindrical system r,fi,x */
        pc[0] = sqrt(pr[csys[1]]*pr[csys[1]]+ pr[csys[2]]*pr[csys[2]]);
        pc[2] = pr[csys[0]];

        if((pr[csys[2]]>0.)&&(pr[csys[1]]==0.)) pc[1]=PI/2.; 
        else if((pr[csys[2]]<0.)&&(pr[csys[1]]==0.)) pc[1]=PI+PI/2.;
        else if((pr[csys[2]]>=0.)&&(pr[csys[1]]>=0.))  /* 1. quadrant */
        {
          pc[1] = atan(pr[csys[2]]/pr[csys[1]]);
        }
        else if((pr[csys[2]]>=0.)&&(pr[csys[1]]<0.))  /* 2. quadrant */
        {
          pc[1] = PI+atan(pr[csys[2]]/pr[csys[1]]);
        }
        else if((pr[csys[2]]<0.)&&(pr[csys[1]]<0.))  /* 3. quadrant */
        {
          pc[1] = PI+atan(pr[csys[2]]/pr[csys[1]]);
        }
        else if((pr[csys[2]]<0.)&&(pr[csys[1]]>=0.))  /* 4. quadrant */
        {
          pc[1] = 2.*PI+atan(pr[csys[2]]/pr[csys[1]]);
        }
	else { printf("ERROR in v_rec2cyl pr: %f %f\n", pr[csys[1]], pr[csys[2]]); return(-3); }
  return(1);
}
