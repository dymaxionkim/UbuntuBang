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
      subroutine mafilltcomp(nef,ipnei,neifa,neiel,vfa,xxn,area,
     &  au,ad,jq,irow,nzs,b,vel,umel,xlet,xle,gradtfa,xxi,
     &  body,volume,ielfa,lakonf,ifabou,nbody,neq,
     &  dtimef,velo,veloo,cvfa,hcfa,cvel,gradvel,xload,gamma,xrlfa,
     &  xxj,nactdohinv,a1,a2,a3,flux,nefa,nefb,iau6,xxni,xxnj)
!
!     filling the matrix for the conservation of energy
!
      implicit none
!
      logical knownflux
!
      character*8 lakonf(*)
!
      integer i,nef,indexf,ipnei(*),j,ifa,iel,neifa(*),
     &  neiel(*),jdof2,jq(*),irow(*),nzs,ielfa(4,*),iau6(6,*),
     &  ipointer,ifabou(*),nbody,neq,indexb,nactdohinv(*),
     &  nefa,nefb
!
      real*8 xflux,vfa(0:7,*),xxn(3,*),area(*),au(*),ad(*),b(neq),
     &  vel(nef,0:7),umel(*),xlet(*),xle(*),coef,gradtfa(3,*),
     &  xxi(3,*),body(0:3,*),volume(*),dtimef,velo(nef,0:7),
     &  veloo(nef,0:7),rhovel,constant,cvel(*),gradvel(3,3,*),
     &  cvfa(*),hcfa(*),div,xload(2,*),gamma(*),xrlfa(3,*),
     &  xxj(3,*),a1,a2,a3,flux(*),xxni(3,*),xxnj(3,*)
!
      intent(in) nef,ipnei,neifa,neiel,vfa,xxn,area,
     &  jq,irow,nzs,vel,umel,xlet,xle,gradtfa,xxi,
     &  body,volume,ielfa,lakonf,ifabou,nbody,neq,
     &  dtimef,velo,veloo,cvfa,hcfa,cvel,gradvel,xload,gamma,xrlfa,
     &  xxj,nactdohinv,a1,a2,a3,flux,nefa,nefb
!
      intent(inout) au,ad,b
!
      do i=nefa,nefb
         do indexf=ipnei(i)+1,ipnei(i+1)
!
!     convection
!
            ifa=neifa(indexf)
            iel=neiel(indexf)
            xflux=flux(indexf)*cvfa(ifa)
!     
            if(xflux.ge.0.d0) then
!     
!     outflowing flux
!     
               ad(i)=ad(i)+xflux
!     centdiff
c               b(i)=b(i)-gamma(ifa)*(vfa(0,ifa)-vel(i,0))*xflux
!end centdiff
            else
               if(iel.gt.0) then
!
!                    incoming flux from neighboring element
!
                  au(indexf)=au(indexf)+xflux
!centdiff
c                  b(i)=b(i)-gamma(ifa)*(vfa(0,ifa)-vel(iel,0))*xflux
!end centdiff
               else
!
!                    incoming flux through boundary
!
                  if(ielfa(2,ifa).lt.0) then
                     indexb=-ielfa(2,ifa)
                     if((ifabou(indexb).ne.0).or.
     &                    (dabs(xflux).lt.1.d-10)) then
                        b(i)=b(i)-vfa(0,ifa)*xflux
                     else
c                        write(*,*) '*ERROR in mafillt: the tempera-'
c                        write(*,*) '       ture of an incoming flux'
c                        write(*,*) '       through face ',j,'of'
c                        write(*,*)'       element ',nactdohinv(i),
c     &                          ' is not given'
                     endif
                  else
c                     write(*,*) '*ERROR in mafillt: the tempera-'
c                     write(*,*) '       ture of an incoming flux'
c                     write(*,*) '       through face ',j,'of'
c                     write(*,*)'       element ',nactdohinv(i),
c     &                   ' is not given'
                  endif
               endif
            endif
!
!           diffusion
!
            if(iel.ne.0) then
!     
!              neighboring element
!     
               coef=hcfa(ifa)*area(ifa)/xlet(indexf)
               ad(i)=ad(i)+coef
               au(indexf)=au(indexf)-coef
!     
!              correction for non-orthogonal grid
!     
               b(i)=b(i)+hcfa(ifa)*area(ifa)*
     &              (gradtfa(1,ifa)*xxnj(1,indexf)+
     &               gradtfa(2,ifa)*xxnj(2,indexf)+
     &               gradtfa(3,ifa)*xxnj(3,indexf))
            else
!     
!              boundary; either temperature given or adiabatic
!              or outlet
!     
               knownflux=.false.
               ipointer=abs(ielfa(2,ifa))
               if(ipointer.gt.0) then
                  if(ifabou(ipointer+6).gt.0) then
!     
!     heat flux is known
!     
                     b(i)=b(i)-xload(1,ifabou(ipointer+6))
                     knownflux=.true.
!     
                  elseif((ifabou(ipointer).ne.0).or.
     &                   (ifabou(ipointer+1).ne.0).or.
     &                   (ifabou(ipointer+2).ne.0).or.
     &                   (ifabou(ipointer+3).ne.0)) then
!     
!                    temperature given or no outlet:
!                    temperature is assumed fixed
!     
                     coef=hcfa(ifa)*area(ifa)/xle(indexf)
                     ad(i)=ad(i)+coef
                     b(i)=b(i)+coef*vfa(0,ifa)
                  else
!     
!                     outlet: no diffusion
!     
                  endif
               endif
!     
!              correction for non-orthogonal grid
!     
               if(.not.knownflux) then
                  b(i)=b(i)+hcfa(ifa)*area(ifa)*
     &                 (gradtfa(1,ifa)*xxni(1,indexf)+
     &                  gradtfa(2,ifa)*xxni(2,indexf)+
     &                  gradtfa(3,ifa)*xxni(3,indexf))
               endif
            endif
         enddo
!     
!        -p*div(v) term
!     
         div=gradvel(1,1,i)+gradvel(2,2,i)+gradvel(3,3,i)
         b(i)=b(i)-vel(i,4)*div*volume(i)
!
!           viscous dissipation
!
         b(i)=b(i)+umel(i)*volume(i)*
     &        (2.d0*(gradvel(1,1,i)**2+gradvel(2,2,i)**2+
     &        gradvel(3,3,i)**2)+
     &        (gradvel(1,2,i)+gradvel(2,1,i))**2+
     &        (gradvel(1,3,i)+gradvel(3,1,i))**2+
     &        (gradvel(2,3,i)+gradvel(3,2,i))**2)
         b(i)=b(i)-2.d0*umel(i)*volume(i)/3.d0*div**2
!     
!           body heat source and body sources
!     
         rhovel=vel(i,5)*volume(i)
!
         if(nbody.gt.0) then
            b(i)=b(i)+rhovel*body(0,i)
         endif
!
!           transient term
!
c         a1=1.d0/dtimef
c         a2=-1.d0/dtimef
c         a3=0.d0/dtimef
c         constant=rhovel*cvel(i)
c         b(i)=b(i)-(a2*velo(i,0)+a3*veloo(i,0))*constant
c
         constant=rhovel*cvel(i)/dtimef
         b(i)=b(i)+velo(i,0)*constant
         ad(i)=ad(i)+constant
!     
      enddo
!     
      return
      end
