/*     CalculiX - A 3-dimensional finite element program                 */
/*              Copyright (C) 1998-2017 Guido Dhondt                          */

/*     This program is free software; you can redistribute it and/or     */
/*     modify it under the terms of the GNU General Public License as    */
/*     published by the Free Software Foundation(version 2);    */
/*                    */

/*     This program is distributed in the hope that it will be useful,   */
/*     but WITHOUT ANY WARRANTY; without even the implied warranty of    */ 
/*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the      */
/*     GNU General Public License for more details.                      */

/*     You should have received a copy of the GNU General Public License */
/*     along with this program; if not, write to the Free Software       */
/*     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.         */

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "CalculiX.h"
#ifdef SPOOLES
   #include "spooles.h"
#endif
#ifdef SGI
   #include "sgi.h"
#endif
#ifdef TAUCS
   #include "tau.h"
#endif

void checkconvnet(ITG *icutb, ITG *iin,
		  double *cam1t, double *cam1f, double *cam1p,
		  double *cam2t, double *cam2f, double *cam2p,
		  double *camt, double *camf, double *camp,
		  ITG *icntrl, double *dtheta, double *ctrl,
                  double *cam1a,double *cam2a,double *cama,
                  double *vamt, double *vamf, double *vamp, double *vama,
                  double *qa, double *qamt, double *qamf,
                  double *ramt, double *ramf, double *ramp, ITG *iplausi){
  
  ITG i0,ir,ip,ic,il,ig,ia,idivergence;
  
  double c2t,c2f,c2p,c2a,c1t,c1f,c1p,qamp=1.,
         df,dc,db,dd,ran,can,rap,ea,cae,ral;

  i0=ctrl[0];ir=ctrl[1];ip=ctrl[2];ic=ctrl[3];il=ctrl[4];ig=ctrl[5];ia=ctrl[7];
  df=ctrl[10];dc=ctrl[11];db=ctrl[12];dd=ctrl[16];ran=ctrl[18];can=ctrl[19];
  rap=ctrl[22];ea=ctrl[23];cae=ctrl[24];ral=ctrl[25];c1t=ctrl[32];c1f=ctrl[33];
  c1p=ctrl[34];c2t=ctrl[35];c2f=ctrl[36];c2p=ctrl[37];c2a=ctrl[38];
  
  /* temperature */
  
  if(*iin<=ip){c2t=0.0001*ran;}
  else{c2t=0.0001*rap;}
  
  if(*iin<=ip){c1t=0.0001*ran;}
  else{c1t=0.0001*rap;}
  
  /* mass flow */
  
  if(*iin<=ip){c2f=0.0001*ran;}
  else{c2f=0.0001*rap;}
  
  if(*iin<=ip){c1f=0.0001*ran;}
  else{c1f=0.0001*rap;}
  
  /* pressure */
  
  if(*iin<=ip){c2p=0.0001*ran;}
  else{c2p=0.0001*rap;}
  
  if(*iin<=ip){c1p=0.0001*ran;}
  else{c1p=0.0001*rap;}
  
  /* geometry */
  
  if(*iin<=ip){c2a=0.0001*ran;}
  else{c2a=0.0001*rap;}
  
  if(*cam1t<*cam2t) {*cam2t=*cam1t;}
  if(*cam1f<*cam2f) {*cam2f=*cam1f;}
  if(*cam1p<*cam2p) {*cam2p=*cam1p;}
  if(*cam1a<*cam2a) {*cam2a=*cam1a;}
  
  /* check for convergence or divergence; 
     the convergence check consists of 
     - a comparison of the correction in
       the latest network iteration with the change since the 
       start of the network calculations 
     - a comparison of the residual in the latest network
       iteration with mean typical values of the equation terms */

  *ramt=0.;*ramf=0.;*ramp=0.;
  if((*camt<=c2t**vamt)&&(*ramt<c1t**qamt)&&
     (*camf<=c2f**vamf)&&(*ramf<c1f**qamf)&&
     (*camp<=c2p**vamp)&&(*ramp<c1p*qamp)&&
     (*cama<=c2a**vama)&&
//     (*cama<=c2a**vama)&&(*iplausi==1)&&
     (*iin>3)){
      
      /* increment convergence reached */
      
      printf("      flow network: convergence in gas iteration %" ITGFORMAT " \n\n",*iin);
      *icntrl=1;
      *icutb=0;
  }
  
  else {

      idivergence=0;

      /* divergence based on temperatures */
      
      if((*iin>=20*i0)||(fabs(*camt)>1.e20)){
	  if((*cam1t>=*cam2t)&&(*camt>=*cam2t)&&(*camt>c2t**vamt)){
	      idivergence=1;
	  }
      }

      /* divergence based on the mass flux */
      
      if((*iin>=20*i0)||(fabs(*camf)>1.e20)){
	  if((*cam1f>=*cam2f)&&(*camf>=*cam2f)&&(*camf>c2f**vamf)){
	      idivergence=1;
	  }
      }

      /* divergence based on pressures */
      
      if((*iin>=20*i0)||(fabs(*camp)>1.e20)){
	  if((*cam1p>=*cam2p)&&(*camp>=*cam2p)&&(*camp>c2p**vamp)){
	      idivergence=1;
	  }
      }

      /* divergence based on geometry */
      
      if((*iin>=20*i0)||(fabs(*cama)>1.e20)){
	  if((*cam1a>=*cam2a)&&(*cama>=*cam2a)&&(*cama>c2a**vama)){
	      idivergence=1;
	  }
      }

      /* divergence based on the number of iterations */

      if(*iin>20*ic) idivergence=1;

      /* divergence based on singular matrix or negative pressures */

      if(*iin==0) idivergence=1;
      
      if(idivergence==1){
	  *dtheta=*dtheta*df;
	  printf("\n network divergence; the under-relaxation parameter is decreased to %e\n",*dtheta);
	  printf(" the network iteration for the increment is reattempted\n\n");
	  *iin=0;
	  (*icutb)++;
	  if(*icutb>ia){
	      qa[2]=0.25;
	      *icntrl=1;
//	    printf("\n *ERROR: too many cutbacks\n");
//	    FORTRAN(stop,());
	  }
      }else{
	 	  printf("      no convergence\n\n"); 
      }
  }
  return;
}
