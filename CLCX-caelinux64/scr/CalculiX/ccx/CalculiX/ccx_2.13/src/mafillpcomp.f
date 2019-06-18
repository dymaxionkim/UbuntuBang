!
!     CalculiX - A 3-dimensional finite element program
!              Copyright (C) 1998-2017 Guido Dhondt
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation(version 2);
!     
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of 
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with this program; if not, write to the Free Software
!     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!
      subroutine mafillpcomp(nef,lakonf,ipnei,neifa,neiel,vfa,area,
     &  advfa,xlet,cosa,volume,au,ad,jq,irow,ap,ielfa,ifabou,xle,
     &  b,xxn,neq,nzs,hfa,gradpel,bp,xxi,neij,
     &  xlen,cosb,ielmatf,mi,a1,a2,a3,velo,veloo,dtimef,shcon,
     &  ntmat_,vel,nactdohinv,xrlfa,flux,nefa,nefb,iau6,xxicn,
     &  gamma)
!
!     filling the lhs and rhs to calculate p
!
      implicit none
!
      character*8 lakonf(*)
!
      integer i,nef,indexf,ipnei(*),j,neifa(*),iel3,k,
     &  neiel(*),iel,ifa,irow(*),ielfa(4,*),compressible,
     &  ifabou(*),neq,jq(*),iel2,indexb,knownflux,indexf2,
     &  j2,neij(*),nzs,imat,nefa,nefb,iau6(6,*),
     &  mi(*),ielmatf(mi(3),*),ntmat_,nactdohinv(*),knownpressure
!
      real*8 coef,vfa(0:7,*),volume(*),area(*),advfa(*),xlet(*),
     &  cosa(*),ad(neq),au(nzs),xle(*),xxn(3,*),ap(*),b(neq),cosb(*),
     &  hfa(3,*),gradpel(3,*),bp(*),xxi(3,*),xlen(*),r,a1,a2,a3,
     &  xflux,constant,velo(nef,0:7),veloo(nef,0:7),dtimef,
     &  shcon(0:3,ntmat_,*),vel(nef,0:7),dd,convec,fluxisobar,
     &  coef1,coef3,xrlfa(3,*),coef2,gamma(*),coefp,coefn,xmach,
     &  flux(*),bp_ifa,aa(8,8),xxicn(3,*)
!
      intent(in) nef,lakonf,ipnei,neifa,neiel,vfa,area,
     &  advfa,xlet,cosa,volume,jq,irow,ielfa,ifabou,xle,
     &  xxn,nzs,hfa,gradpel,xxi,neij,
     &  xlen,cosb,ielmatf,mi,a1,a2,a3,velo,veloo,dtimef,shcon,
     &  ntmat_,vel,nactdohinv,xrlfa,flux,gamma
!
      intent(inout) ad,au,b,ap,bp
!
      do i=nefa,nefb
         imat=ielmatf(1,i)
         r=shcon(3,1,imat)
         indexf=ipnei(i)
         do j=1,ipnei(i+1)-ipnei(i)
            knownflux=0
            knownpressure=0
            convec=0
!     
!     diffusion
!     
            indexf=indexf+1
            ifa=neifa(indexf)
            iel=neiel(indexf)
            if(iel.ne.0) then
               coef=vfa(5,ifa)*(volume(i)+volume(iel))*area(ifa)/
     &              (advfa(ifa)*2.d0*xlet(indexf)*cosb(indexf))
               ad(i)=ad(i)+coef
               au(indexf)=au(indexf)-coef
               b(i)=b(i)+coef*(vel(iel,4)-vel(i,4))
               convec=coef*(vel(iel,4)-vel(i,4))
!     
!     correction for non-orthogonal meshes
!     
               j2=neij(indexf)
               indexf2=ipnei(iel)+j2
               bp_ifa=((gradpel(1,iel)*xxicn(1,indexf2)+
     &                  gradpel(2,iel)*xxicn(2,indexf2)+
     &                  gradpel(3,iel)*xxicn(3,indexf2))
     &                *xle(indexf2)
     &                -(gradpel(1,i)*xxicn(1,indexf)+
     &                  gradpel(2,i)*xxicn(2,indexf)+
     &                  gradpel(3,i)*xxicn(3,indexf))
     &                *xle(indexf))
               b(i)=b(i)+coef*bp_ifa
               convec=convec+coef*bp_ifa
!
!              following line is correct if the temperature
!              changes from one pressure correction iteration to
!              the next 
!
               convec=-convec/(vfa(5,ifa)*r*vfa(0,ifa))
            else
               iel2=ielfa(2,ifa)
               if(iel2.lt.0) then
                  if((ifabou(-iel2+1).ne.0).and.
     &               (ifabou(-iel2+2).ne.0).and.
     &               (ifabou(-iel2+3).ne.0)) then
!     
!     all velocity components given
!     
                     knownflux=1
                  elseif(ifabou(-iel2+5).lt.0) then
!
!                    sliding conditions
!
                     knownflux=2
                  elseif(ifabou(-iel2+4).ne.0) then
                     knownpressure=1
!     
!     pressure given (only if not all velocity
!     components are given)
!     
                     coef=vfa(5,ifa)*volume(i)*area(ifa)/
     &                    (advfa(ifa)*xle(indexf)*cosa(indexf))
                     ad(i)=ad(i)+coef
                     b(i)=b(i)+coef*vfa(4,ifa)
     &                                -coef*vel(i,4)
                     convec=coef*vfa(4,ifa)-coef*vel(i,4)
!     
!     correction for non-orthogonal meshes
!     
                     bp_ifa=(-(gradpel(1,i)*xxicn(1,indexf)+
     &                         gradpel(2,i)*xxicn(2,indexf)+
     &                         gradpel(3,i)*xxicn(3,indexf))
     &                     *xle(indexf))
                     b(i)=b(i)+coef*bp_ifa
                     convec=convec+coef*bp_ifa
!
!              following line is correct if the temperature
!              changes from one pressure correction iteration to
!              the next 
!
                     convec=-convec/(vfa(5,ifa)*r*vfa(0,ifa))
                  endif
               endif
            endif
!     
!     save coefficients for correctvfa.f
!     
            if((iel.eq.0).or.(i.lt.iel)) then
               ap(ifa)=coef
               bp(ifa)=bp_ifa
            endif
!
!           convection
!
!           flux
!
            if(knownflux.eq.1) then
               xflux=flux(indexf)
            elseif(knownflux.eq.2) then
               xflux=0.d0
            endif
!
!           flux based on constant pressure
!
            if(knownflux.eq.0) then
               fluxisobar=area(ifa)*
     &             (hfa(1,ifa)*xxn(1,indexf)+
     &              hfa(2,ifa)*xxn(2,indexf)+
     &              hfa(3,ifa)*xxn(3,indexf))
            endif
!     
!           rhs
!
            if(knownflux.eq.0) then
               b(i)=b(i)-vfa(5,ifa)*fluxisobar
            elseif(knownflux.eq.1) then
               b(i)=b(i)-xflux
            endif
!
            if(knownflux.eq.0) then
!     
!              following line leads to oscillations in the solution
!              (only for subsonic and transonic solutions)
!
               coef=fluxisobar/(r*vfa(0,ifa))+convec
            elseif(knownflux.eq.1) then
               coef=xflux/(r*vfa(0,ifa)*vfa(5,ifa))
            else
               coef=0.d0
            endif
!
            if(coef.ge.0.d0) then
!     
!     outflowing flux
!     
               ad(i)=ad(i)+coef
c
c               retarded central difference     
c               b(i)=b(i)-gamma(ifa)*(vfa(4,ifa)-vel(i,4))*coef
c
            else
               if(iel.gt.0) then
!     
!                    incoming flux from neighboring element
!
                  au(indexf)=au(indexf)+coef
!
c
c               retarded central difference     
c                  b(i)=b(i)-gamma(ifa)*(vfa(4,ifa)-vel(iel,4))*coef
c
               elseif(knownpressure.eq.0) then
                  ad(i)=ad(i)+coef
               endif
            endif
!
         enddo
!
!        transient term
!
c         a1=1.d0/dtimef
c         a2=-1.d0/dtimef
c         a3=0.d0/dtimef
c         constant=volume(i)/(r*vel(i,0))
c         b(i)=b(i)-
c     &        (a1*vel(i,5)+a2*velo(i,5)+a3*veloo(i,5))*volume(i)
         b(i)=b(i)-
     &        (vel(i,5)-velo(i,5))*volume(i)/dtimef
c         constant=a1*constant
         constant=volume(i)/(r*vel(i,0)*dtimef)
         ad(i)=ad(i)+constant
!
      enddo
!
      return
      end
