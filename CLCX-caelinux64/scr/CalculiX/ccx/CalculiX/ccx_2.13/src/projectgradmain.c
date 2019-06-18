/*     CalculiX - A 3-dimensional finite element program                 */
/*              Copyright (C) 1998-2017 Guido Dhondt                     */

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

#ifdef ARPACK

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
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
#ifdef MATRIXSTORAGE
   #include "matrixstorage.h"
#endif
#ifdef PARDISO
   #include "pardiso.h"
#endif


void projectgradmain(ITG *nobject,char *objectset,double *dgdxglob,
         double *g0,ITG *ndesi,ITG *nodedesi,ITG *nk,ITG *isolver,
	 ITG *nactive,ITG *nnlconst,ITG *ipoacti){
	               
  /* generating the projected gradient vector for constraint 
     optimization */

  ITG nzss,*mast1=NULL,*irows=NULL,*icols=NULL,*jqs=NULL,*ipointer=NULL,
      symmetryflag=0,inputformat=0,i,iconst,nthread,nthread_v;
             
  double *au=NULL,*ad=NULL,*adb=NULL,*aub=NULL,sigma=0,*rhs=NULL,
      *vector=NULL;
  
  char *env;   
  
  /* determining the structure of the N-Matrix */
  
  nzss=20000000;
  NNEW(mast1,ITG,nzss);
  NNEW(irows,ITG,1);
  NNEW(icols,ITG,*nactive);
  NNEW(jqs,ITG,*nactive+1);
  NNEW(ipointer,ITG,*nactive);
  
  mastructnmatrix(icols,jqs,&mast1,&irows,ipointer,&nzss,nactive,nnlconst);
      
  SFREE(mast1);SFREE(ipointer);
  RENEW(irows,ITG,nzss);
   
  /* determining the entries of the N-Matrix */
  
  NNEW(ad,double,*nactive);
  NNEW(au,double,nzss);    
  
  FORTRAN(nmatrix,(ad,au,jqs,irows,ndesi,nodedesi,dgdxglob,nactive,
  	  nobject,nnlconst,ipoacti,nk));

  /* Calculate inverse of the N-matrix */

  NNEW(adb,double,*nactive);
  NNEW(aub,double,nzss); 

  for(i=0;i<*nactive;i++){
     adb[i]=1.0;
  }
       
  /* LU decomposition of the left hand matrix */

	if(*isolver==0){
#ifdef SPOOLES
	    spooles_factor(ad,au,adb,aub,&sigma,icols,irows,nactive,&nzss,
			   &symmetryflag,&inputformat,&nzss);
#else
	    printf("*ERROR in projectgrad: the SPOOLES library is not linked\n\n");
	    FORTRAN(stop,());
#endif
	}
	else if(*isolver==4){
#ifdef SGI
	    token=1;
	    sgi_factor(ad,au,adb,aub,&sigma,icols,irows,nactive,&nzss,token);
#else
	    printf("*ERROR in projectgrad: the SGI library is not linked\n\n");
	    FORTRAN(stop,());
#endif
	}
	else if(*isolver==5){
#ifdef TAUCS
	    tau_factor(ad,&au,adb,aub,&sigma,icols,&irows,nactive,&nzss);
#else
	    printf("*ERROR in projectgrad: the TAUCS library is not linked\n\n");
	    FORTRAN(stop,());
#endif
	}
	else if(*isolver==7){
#ifdef PARDISO
	    pardiso_factor(ad,au,adb,aub,&sigma,icols,irows,nactive,&nzss,
			   &symmetryflag,&inputformat,jqs,&nzss);
#else
	    printf("*ERROR in projectgrad: the PARDISO library is not linked\n\n");
	    FORTRAN(stop,());
#endif
	}
  
  /* solve the system nactive-times */

  NNEW(rhs,double,*nactive);
  NNEW(vector,double,*ndesi);
  
  for(iconst=1;iconst<=*nactive;iconst++){
      
      for(i=0;i<*nactive;i++){
  	  rhs[i]=0.00;
      }
  
      rhs[iconst-1]=1.0;
 
  /* solve the system */
		  
		  if(*isolver==0){
#ifdef SPOOLES
		    spooles_solve(rhs,nactive);
#endif
		  }
		  else if(*isolver==4){
#ifdef SGI
		    sgi_solve(rhs,token);
#endif
		  }
		  else if(*isolver==5){
#ifdef TAUCS
		    tau_solve(rhs,nactive);
#endif
		  }
		  else if(*isolver==7){
#ifdef PARDISO
		    pardiso_solve(rhs,nactive,&symmetryflag);
#endif
		  }

      for(i=0;i<*ndesi;i++){
    	 vector[i]=0.00;
      }
    	     
  /* carry out matrix multiplications */
  
      FORTRAN(projectgrad,(vector,ndesi,nodedesi,dgdxglob,nactive,
  	  nobject,nnlconst,ipoacti,nk,rhs,&iconst,objectset));
                
  }

  /* clean the system */

	if(*isolver==0){
#ifdef SPOOLES
	    spooles_cleanup();
#endif
	}
	else if(*isolver==4){
#ifdef SGI
	    sgi_cleanup(token);
#endif
	}
	else if(*isolver==5){
#ifdef TAUCS
	    tau_cleanup();
#endif
	}
	else if(*isolver==7){
#ifdef PARDISO
	    pardiso_cleanup(nactive,&symmetryflag);
#endif
	}

  SFREE(irows);SFREE(icols);SFREE(adb);SFREE(aub);
  SFREE(jqs);SFREE(ad);SFREE(au);SFREE(vector);SFREE(rhs);
  
  return;
  
} 

#endif
