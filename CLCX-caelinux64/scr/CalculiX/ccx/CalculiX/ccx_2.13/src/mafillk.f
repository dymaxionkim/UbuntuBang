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
      subroutine mafillk(nef,ipnei,neifa,neiel,vfa,xxn,area,
     &  au,ad,jq,irow,nzs,b,vel,umfa,xlet,xle,gradkfa,xxi,
     &  body,volume,ielfa,lakonf,ifabou,nbody,neq,
     &  dtimef,velo,veloo,cvfa,hcfa,cvel,gradvel,xload,gamma,xrlfa,
     &  xxj,nactdohinv,a1,a2,a3,flux,nefa,nefb,iau6,xxni,xxnj,
     &  iturbulent)
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
     &  neiel(*),jq(*),irow(*),nzs,ielfa(4,*),nefa,nefb,
     &  ipointer,ifabou(*),nbody,neq,indexb,nactdohinv(*),
     &  iau6(6,*),iturbulent
!
      real*8 xflux,vfa(0:7,*),xxn(3,*),area(*),au(*),ad(*),b(neq),
     &  vel(nef,0:7),umfa(*),xlet(*),xle(*),coef,gradkfa(3,*),
     &  xxi(3,*),body(0:3,*),volume(*),dtimef,velo(nef,0:7),
     &  veloo(nef,0:7),rhovol,cvel(*),gradvel(3,3,*),sk,
     &  cvfa(*),hcfa(*),div,xload(2,*),gamma(*),xrlfa(3,*),
     &  xxj(3,*),a1,a2,a3,flux(*),xxnj(3,*),xxni(3,*),difcoef
!
      intent(in) nef,ipnei,neifa,neiel,vfa,xxn,area,
     &  jq,irow,nzs,vel,umfa,xlet,xle,gradkfa,xxi,
     &  body,volume,ielfa,lakonf,ifabou,nbody,neq,
     &  dtimef,velo,veloo,cvfa,hcfa,cvel,gradvel,xload,gamma,xrlfa,
     &  xxj,nactdohinv,a1,a2,a3,flux,nefa,nefb,iturbulent
!
      intent(inout) au,ad,b
!
      if(iturbulent.eq.1) then
!
!        k-epsilon
!
         sk=1.d0
      else
!
!        k-omega
!
         sk=0.5d0
      endif
!
      do i=nefa,nefb
         do indexf=ipnei(i)+1,ipnei(i+1)
!
!     convection
!
            ifa=neifa(indexf)
            iel=neiel(indexf)
            xflux=flux(indexf)
!
            if(xflux.ge.0.d0) then
!     
!     outflowing flux
!     
               ad(i)=ad(i)+xflux
!
               b(i)=b(i)-gamma(ifa)*(vfa(6,ifa)-vel(i,6))*xflux
!
            else
               if(iel.gt.0) then
!
!                    incoming flux from neighboring element
!
                  au(indexf)=au(indexf)+xflux
!
                  b(i)=b(i)-gamma(ifa)*(vfa(6,ifa)-vel(iel,6))*xflux
!
               else
!
!                    incoming flux through boundary
!
                  if(ielfa(2,ifa).lt.0) then
                     indexb=-ielfa(2,ifa)
                     if(((ifabou(indexb+1).ne.0).and.
     &                   (ifabou(indexb+2).ne.0).and.
     &                   (ifabou(indexb+3).ne.0)).or.
     &                    (dabs(xflux).lt.1.d-10)) then
                        b(i)=b(i)-vfa(6,ifa)*xflux
                     else
                        write(*,*) '*ERROR in mafillk: the turbulent'
                        write(*,*) '       kinetic energy'
                        write(*,*) '       of an incoming flux'
                        write(*,*) '       through face ',
     &                        indexf-ipnei(i),'of'
                        write(*,*)'       element ',nactdohinv(i),
     &                          ' is not given'
                     endif
                  else
                     write(*,*) '*ERROR in mafillk: the turbulent'
                     write(*,*) '       kinetic energy'
                     write(*,*) '       of an incoming flux'
                     write(*,*) '       through face ',
     &                         indexf-ipnei(i),'of'
                     write(*,*)'       element ',nactdohinv(i),
     &                   ' is not given'
                  endif
               endif
            endif
!
!           diffusion
!
            difcoef=umfa(ifa)+sk*vfa(5,ifa)*vfa(6,ifa)/vfa(7,ifa)
!
            if(iel.ne.0) then
!     
!              neighboring element
!     
               coef=difcoef*area(ifa)/xlet(indexf)
               ad(i)=ad(i)+coef
               au(indexf)=au(indexf)-coef
!     
!              correction for non-orthogonal grid
!     
               b(i)=b(i)+difcoef*area(ifa)*
     &              (gradkfa(1,ifa)*xxnj(1,indexf)+
     &               gradkfa(2,ifa)*xxnj(2,indexf)+
     &               gradkfa(3,ifa)*xxnj(3,indexf))
            else
!     
!              boundary; either temperature given or adiabatic
!              or outlet
!     
               knownflux=.false.
               ipointer=abs(ielfa(2,ifa))
               if(ipointer.gt.0) then
                  if((ifabou(ipointer+5).ne.0).or.
     &               (ifabou(ipointer+1).ne.0).or.
     &               (ifabou(ipointer+2).ne.0).or.
     &               (ifabou(ipointer+3).ne.0)) then
!     
!                    no outlet:
!                    (i.e. no wall || no sliding || at least one velocity given)
!                    turbulent variable is assumed fixed
!     
                     coef=difcoef*area(ifa)/xle(indexf)
                     ad(i)=ad(i)+coef
                     b(i)=b(i)+coef*vfa(6,ifa)
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
                  b(i)=b(i)+difcoef*area(ifa)*
     &                 (gradkfa(1,ifa)*xxni(1,indexf)+
     &                  gradkfa(2,ifa)*xxni(2,indexf)+
     &                  gradkfa(3,ifa)*xxni(3,indexf))
               endif
            endif
         enddo
!
!           viscous dissipation
!     
         rhovol=vel(i,5)*volume(i)
!
!        sink terms are treated implicitly (lhs)
!
         ad(i)=ad(i)+rhovol*0.09d0*vel(i,7)
!
!        source terms are treated explicitly (rhs)
!
         b(i)=b(i)+rhovol*vel(i,6)*((
     &        (2.d0*(gradvel(1,1,i)**2+gradvel(2,2,i)**2+
     &        gradvel(3,3,i)**2)+
     &        (gradvel(1,2,i)+gradvel(2,1,i))**2+
     &        (gradvel(1,3,i)+gradvel(3,1,i))**2+
     &        (gradvel(2,3,i)+gradvel(3,2,i))**2))/vel(i,7)
c     &        -0.09d0*vel(i,7)
     &        )
!
!           transient term
!
         b(i)=b(i)-(a2*velo(i,6)+a3*veloo(i,6))*rhovol
         rhovol=a1*rhovol
         ad(i)=ad(i)+rhovol
!     
      enddo
!     
      return
      end
