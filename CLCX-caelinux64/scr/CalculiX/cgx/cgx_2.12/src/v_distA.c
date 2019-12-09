
#include <extUtil.h>

/* **************************************************************************  */
/* liefert orient==1 wenn der node vor dem Dreieck liegt                       */  
/*         orient==-1 wenn der node hinter dem Dreieck liegt                   */  
/*         orient==2 wenn vray parallel zur ebene ist                          */  
/*         orient==0 wenn der node ausserhalb  liegt, redundant zu:            */  
/* liefert -1e10 wenn der node ausserhalb einer durch 3 Punkte beschriebenen   */
/* Flaeche liegt. Liegt er innerhalb liefert es  den kuerzesten Abstand zur    */
/* Ebene in Richtung vray.                                                     */
/* -> das Dreieck wird etwas vergroessert (triScale)                           */
/* **************************************************************************  */
double v_distA( double *pN0, double *pN1, double *pN2, double *N, double *vray, double  triScale, int *orient)
{
   register int i;
   int    inull, iplus, iminus;
   double N0[3], N1[3], N2[3];
   double  vN0N1[3], vN1N2[3], vN2N0[3], vN0N[3], vN1N[3],  vN2N[3],
          vprod0[3], vprod1[3], vprod2[3], vsprod[3], vnorm[3], Nproj[3];
   double  en[3], ev[3], eu[3], vNN0[3], g, sprod;
   double  ph[3],vphN[3],vphN_[3];


   /* der Punkt liegt im Dreieck wenn: */
   /* - berechne die Vektorprodukte zwischen den Kantenvektoren und dem Vektor von den    */
   /*   Kantenpunkten zu dem node (v_prod= vP1P2 x vP1Pn)                                 */
   /* - berechne die Scalarprtodukte v_sprod zwischen den v_prod und dem Normalenvektor   */
   /*   des Dreiecks. Wenn alle v_sprod positiv sind, dann liegt der node innerhalb und   */
   /*   davor, wenn alle negativ sind dann dahinter. Wenn uneinheitlich, dann ausserhalb. */

   if((triScale==0.)||(triScale==1.))
   {
     for(i=0; i<3; i++)  N0[i]=pN0[i];
     for(i=0; i<3; i++)  N1[i]=pN1[i];
     for(i=0; i<3; i++)  N2[i]=pN2[i];
   }
   else
   {
     /* scale the triangle */

     /* half-length of edge */
     for(i=0; i<3; i++)  ph[i]=(pN0[i]+pN1[i])*.5;
     /* vector to opposite point */
     v_result( ph, pN2, vphN);
     /* scale */
     v_scal(&triScale, vphN, vphN_);
     v_add( ph, vphN_, N2);

     /* half-length of edge */
     for(i=0; i<3; i++)  ph[i]=(pN1[i]+pN2[i])*.5;
     /* vector to opposite point */
     v_result( ph, pN0, vphN);
     /* scale */
     v_scal(&triScale, vphN, vphN_);
     v_add( ph, vphN_, N0);
     //for(i=0; i<3; i++)  printf("ph:%f vphN:%f vphN_:%f pN0:%f N0:%f\n", ph[i], vphN[i], vphN_[i], pN0[i], N0[i]);

     /* half-length of edge */
     for(i=0; i<3; i++)  ph[i]=(pN2[i]+pN0[i])*.5;
     /* vector to opposite point */
     v_result( ph, pN1, vphN);
     /* scale */
     v_scal(&triScale, vphN, vphN_);
     v_add( ph, vphN_, N1);
   }

   v_result( N0, N1, vN0N1);
   v_result( N1, N2, vN1N2);
   v_result( N2, N0, vN2N0);

   /* berechne den Normalenvektor auf der flaeche */
   v_prod( vN0N1, vN1N2, vnorm );

   /* ist vray parallel zur Ebene? */
   sprod= v_sprod( vnorm, vray);
   if( (int)(sprod*1e10) ==0 )
   {
     *orient=2;
     return(-1.e10);
   }

   /* berechne den Gradeneinheitsvektor en */
   v_norm( vray, en );

   /* berechne die einheitsvektoren der ebenengleichung des tri3 */
   v_norm( vN0N1, eu );
   v_norm( vN1N2, ev );

   /* bestimme den Abstand zwischen dem Aufpunkt des elements und des punktes  */
   v_result( N, N0, vNN0 );

   /* berechne die Konstante g (Abstand)  pn_neu=pn+en*g  */
   g = AsplitL( vNN0, eu, ev, en );

   /* berechne die projezierten punktkoordinaten */
   Nproj[0]=N[0]+en[0]*g;
   Nproj[1]=N[1]+en[1]*g;
   Nproj[2]=N[2]+en[2]*g;

   /* printf(" dist:%lf p0:%lf %lf %lf p1:%lf %lf %lf p2:%lf %lf %lf p:%lf %lf %lf pneu:%lf %lf %lf\n",g,
               N0[0],N0[1],N0[2], 
               N1[0],N1[1],N1[2], 
               N2[0],N2[1],N2[2], 
               N[0],N[1],N[2], 
               Nproj[0],Nproj[1],Nproj[2]); */
   

   /* berechne die vektoren die von den elem-nodes zum projezierten punkt laufen */
   v_result( N0, Nproj, vN0N);
   v_result( N1, Nproj, vN1N);
   v_result( N2, Nproj, vN2N);

   v_prod( vN0N1, vN0N, vprod0 );
   v_prod( vN1N2, vN1N, vprod1 );
   v_prod( vN2N0, vN2N, vprod2 );

   vsprod[0]= v_sprod( vnorm, vprod0);
   vsprod[1]= v_sprod( vnorm, vprod1);
   vsprod[2]= v_sprod( vnorm, vprod2);
   
   sprod= v_sprod( vnorm, vNN0);
   if( (int)(sprod*1e10) ==0 ) *orient=0;
   else if(sprod<0) *orient=1;
   else *orient=-1;

   /*   kontrolle ob ein vsprod=0 ist. Dann liegt naemlich                */
   /*   der zu kontrollierende node genau ueber einer der umrandungen     */
   /*   der kontroll-flaeche (vprod ist dann senkrecht auf der normalen). */
   /*   Dann werden vsprod mit den gleichen vorzeichen zusammen-          */
   /*   gezaelt, dabei gelten die nuller als joker                        */

   inull=0;
   for (i=0; i<3; i++) if (vsprod[i] == 0.) inull++;
   
   iplus=inull;
   for (i=0; i<3; i++) if (vsprod[i] > 0.) iplus++;
   
   iminus=inull;
   for (i=0; i<3; i++) if (vsprod[i] < 0.) iminus++;

   //printf (" vsprod: %lf %lf %lf inull:%d iplus:%d iminus:%d g:%lf ori:%e\n",vsprod[0],vsprod[1],vsprod[2], inull, iplus, iminus, g, sprod );
   
   /*   wenn alle vectorprodukte das gleiche vorzeichen haben             */
   /*   dann ist der node innerhalb der flaeche                           */
   if ((iplus == 3)||(iminus == 3)) return(g);
   else return(-1.e10);
}

