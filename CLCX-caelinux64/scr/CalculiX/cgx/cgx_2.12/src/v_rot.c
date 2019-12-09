#include <extUtil.h>

/******************************************************************/
/* rotates a given point pin around v  by fi radiants             */
/* p0 ist the origin of the system                                */
/* pout is the rotated point                                      */
/*                                                                */
/*                                                                */
/*                                                                */
/*                                                                */
/******************************************************************/
int v_rot(double fi, double *p0, double *v, double *pin, double *pout)
{
  double ex[3],ey[3],ez[3], el[3], eh[3], eq[3];
  double x,y,z,l,h,q,h_offs,q_offs,dh,dq;

    v_norm (v, el);

    /* konstuiere damit den 2. einheitsvektor eh  */
    eh[1] = el[0];
    eh[2] = el[1];
    eh[0] = el[2];
    v_prod( el, eh, ex );
    v_norm (ex, eh);


    /* konstuiere den 3. einheitsvektor eq  */
    v_prod( el, eh, ex );
    v_norm (ex, eq);

    /* berechnung der lhq-koordinaten der xyz einheitsvektoren durch zykl.vertausch.  */
  ex[0]=el[0];
  ex[1]=eh[0];
  ex[2]=eq[0];

  ey[0]=el[1];
  ey[1]=eh[1];
  ey[2]=eq[1];

  ez[0]=el[2];
  ez[1]=eh[2];
  ez[2]=eq[2];

  /* Berechnung der lhq-koordinaten der Drehachse (offset fuer die Drehung) */
  x=p0[0];
  y=p0[1];
  z=p0[2];
  
  /* l=ex[0]*x+ey[0]*y+ez[0]*z; */
  h_offs=ex[1]*x+ey[1]*y+ez[1]*z;
  q_offs=ex[2]*x+ey[2]*y+ez[2]*z;

  /* ab hier moeglicher loop ueber alle punkte*/
    x=pin[0];
    y=pin[1];
    z=pin[2];
    l=ex[0]*x+ey[0]*y+ez[0]*z;
   
    h=( ex[1]*x+ey[1]*y+ez[1]*z ) - h_offs ;
    q=( ex[2]*x+ey[2]*y+ez[2]*z ) - q_offs ;

    /* drehe um l  */
    dh=h*cos(fi)-q*sin(fi);
    dq=h*sin(fi)+q*cos(fi);
    
    dh+= h_offs;
    dq+= q_offs;
    

    pout[0]=el[0]*l+eh[0]*dh+eq[0]*dq;
    pout[1]=el[1]*l+eh[1]*dh+eq[1]*dq;
    pout[2]=el[2]*l+eh[2]*dh+eq[2]*dq;
   return(1);
}
