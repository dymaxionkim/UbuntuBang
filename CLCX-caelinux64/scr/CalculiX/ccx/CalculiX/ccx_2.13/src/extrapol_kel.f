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
      subroutine extrapol_kel(nface,ielfa,xrlfa,vel,vfa,
     &  ifabou,xbounact,nef,gradkel,gradkfa,neifa,rf,area,volume,
     &  xle,xxi,icyclic,xxn,ipnei,ifatie,xlet,xxj,
     &  coefmpc,nmpc,labmpc,ipompc,nodempc,ifaext,nfaext,nactdoh,
     &  umfa,physcon)
!
!     extrapolation of temperature values to the faces
!
      implicit none
!
      character*20 labmpc(*)
!
      integer nface,ielfa(4,*),ifabou(*),i,iel1,iel2,nef,
     &  neifa(*),icyclic,ifa,indexf,k,l,m,ipnei(*),ifatie(*),ipointer,
     &  is,ie,nmpc,ipompc(*),nodempc(3,*),ifaext(*),nfaext,nactdoh(*)
!
      real*8 xrlfa(3,*),vel(nef,0:7),vfa(0:7,*),xbounact(*),xl1,xl2,
     &   vfap(0:7,nface),gradkel(3,*),gradkfa(3,*),rf(3),area(*),
     &   volume(*),xle(*),xxi(3,*),c(3,3),gradnor,xxn(3,*),umfa(*),
     &   xxj(3,*),dd,xlet(*),coefmpc(*),constant,
     &   physcon(*)
!
      intent(in) nface,ielfa,xrlfa,umfa,physcon,
     &  ifabou,xbounact,nef,neifa,rf,area,volume,
     &  xle,xxi,icyclic,xxn,ipnei,ifatie,xlet,xxj,
     &  coefmpc,nmpc,labmpc,ipompc,nodempc,ifaext,nfaext,nactdoh
!
      intent(inout) vfa,gradkel,gradkfa,vel
!
!     5.5 x 10**(-3.5) = 1.7393e-3
!
      constant=1.7393d-3*physcon(5)/(physcon(7)*physcon(8))
!     
!c$omp parallel default(none)
!c$omp& shared(nface,ielfa,xrlfa,vfap,vel,ifabou,xbounact,constant,
!c$omp&        umfa)
!c$omp& private(i,iel1,xl1,iel2,ibou)
!c$omp do
      do i=1,nface
         iel1=ielfa(1,i)
         xl1=xrlfa(1,i)
         iel2=ielfa(2,i)
         if(iel2.gt.0) then
!
!           face between two elements: interpolation
!
            vfap(6,i)=xl1*vel(iel1,6)+xrlfa(2,i)*vel(iel2,6)
         elseif(ielfa(3,i).gt.0) then
!
!           boundary face; no zero gradient
!
!           iel2=0 is not possible: if iel2=0, there are no
!           boundary conditions on the face, hence it is an
!           exit, which means zero gradient and 
!           ielfa(3,i) <= 0
!     
            ipointer=-iel2
!     
            if(ifabou(ipointer+5).gt.0) then
!     
!              wall: kinetic turbulent energy known
!     
               vfap(6,i)=0.d0
            elseif(((ifabou(ipointer+1).gt.0).and.
     &              (ifabou(ipointer+2).gt.0).and.
     &              (ifabou(ipointer+3).gt.0)).or.
     &             (ifabou(ipointer+5).lt.0)) then
!     
!              inlet or sliding conditions: kinetic turbulent energy known
!     
c               write(*,*) constant
c               write(*,*) i
c               write(*,*) umfa(i)
               vfap(6,i)=constant*umfa(i)
            else
!
!              extrapolation
!
               vfap(6,i)=xl1*vel(iel1,6)+xrlfa(3,i)*vel(ielfa(3,i),6)
            endif
         else
!
!           boundary face; zero gradient
!
            vfap(6,i)=vel(iel1,6)
         endif
      enddo
!c$omp end do
!c$omp end parallel
!
!     Multiple point constraints
!
      if(nmpc.gt.0) then
         is=6
         ie=6
         call applympc(nface,ielfa,is,ie,ifabou,ipompc,vfap,coefmpc,
     &        nodempc,ipnei,neifa,labmpc,xbounact,nactdoh,
     &        ifaext,nfaext)
      endif
!
!     calculate the gradient of the temperature at the center of
!     the elements
!
!c$omp parallel default(none)
!c$omp& shared(nef,ipnei,neifa,gradkel,vfap,area,xxn,volume)
!c$omp& private(i,indexf,ifa)
!c$omp do
      do i=1,nef
!
!        initialization
!     
         do l=1,3
            gradkel(l,i)=0.d0
         enddo
!
         do indexf=ipnei(i)+1,ipnei(i+1)
            ifa=neifa(indexf)
            do l=1,3
               gradkel(l,i)=gradkel(l,i)+
     &              vfap(6,ifa)*area(ifa)*xxn(l,indexf)
            enddo
         enddo
!     
!        dividing by the volume of the element
!     
         do l=1,3
            gradkel(l,i)=gradkel(l,i)/volume(i)
         enddo
      enddo
!c$omp end do
!c$omp end parallel
! 
!     interpolate/extrapolate the temperature gradient from the
!     center of the elements to the center of the faces
!           
!c$omp parallel default(none)
!c$omp& shared(nface,ielfa,xrlfa,gradkfa,gradkel,icyclic,c,ifatie,
!c$omp&        ipnei,xxn,ifabou)
!c$omp& private(i,iel1,xl1,iel2,l,xl2,indexf,gradnor)
!c$omp do
      do i=1,nface
         iel1=ielfa(1,i)
         xl1=xrlfa(1,i)
         iel2=ielfa(2,i)
         if(iel2.gt.0) then
!
!           face between two elements
!
            xl2=xrlfa(2,i)
            if((icyclic.eq.0).or.(ifatie(i).eq.0)) then
               do l=1,3
                  gradkfa(l,i)=xl1*gradkel(l,iel1)+
     &                 xl2*gradkel(l,iel2)
               enddo
            elseif(ifatie(i).gt.0) then
               do l=1,3
                  gradkfa(l,i)=xl1*gradkel(l,iel1)+xl2*
     &                  (gradkel(1,iel2)*c(l,1)+
     &                   gradkel(2,iel2)*c(l,2)+
     &                   gradkel(3,iel2)*c(l,3))
               enddo
            else
               do l=1,3
                  gradkfa(l,i)=xl1*gradkel(l,iel1)+xl2*
     &                  (gradkel(1,iel2)*c(1,l)+
     &                   gradkel(2,iel2)*c(2,l)+
     &                   gradkel(3,iel2)*c(3,l))
               enddo
            endif
         elseif(ielfa(3,i).gt.0) then
!
!           the above condition implies iel2!=0
!           if iel2 were zero, no b.c. would apply
!           which means exit and hence zero gradient:
!           ielfa(3,i) would be zero or negative
!     
!           boundary face; no zero gradient
!
            do l=1,3
               gradkfa(l,i)=xl1*gradkel(l,iel1)+
     &              xrlfa(3,i)*gradkel(l,abs(ielfa(3,i)))
            enddo
         else
!     
!           boundary face; zero gradient in i-direction
!   
            indexf=ipnei(iel1)+ielfa(4,i)
            gradnor=gradkel(1,iel1)*xxi(1,indexf)+
     &              gradkel(2,iel1)*xxi(2,indexf)+
     &              gradkel(3,iel1)*xxi(3,indexf)
            do l=1,3
                  gradkfa(l,i)=gradkel(l,iel1)
     &                        -gradnor*xxi(l,indexf)
            enddo
         endif
      enddo
!c$omp end do
!c$omp end parallel
!
!     correction loops
!
      do m=1,2
!
!        Moukalled et al. p 279
!
!c$omp parallel default(none)
!c$omp& shared(nface,ielfa,xrlfa,vfa,vfap,gradkfa,rf,ifabou,xxi,xle,
!c$omp&        vel,ipnei)
!c$omp& private(i,iel1,iel2,xl1,indexf)
!c$omp do
         do i=1,nface
            iel1=ielfa(1,i)
            iel2=ielfa(2,i)
            xl1=xrlfa(1,i)
            if(iel2.gt.0) then
!
!              interpolation
!
               vfa(6,i)=vfap(6,i)+gradkfa(1,i)*rf(1)
     &                         +gradkfa(2,i)*rf(2)
     &                         +gradkfa(3,i)*rf(3)
            elseif(ielfa(3,i).gt.0) then
!
!              no implicit zero gradient
!
               ipointer=-iel2
!
               if(ifabou(ipointer+5).gt.0) then
!     
!                 wall: kinetic turbulent energy known
!     
                  vfa(6,i)=vfap(6,i)
               elseif(((ifabou(ipointer+1).gt.0).and.
     &                 (ifabou(ipointer+2).gt.0).and.
     &                 (ifabou(ipointer+3).gt.0)).or.
     &                (ifabou(ipointer+5).lt.0)) then
!     
!                 inlet or sliding conditions: kinetic turbulent energy known
!     
                  vfa(6,i)=vfap(6,i)
               else
!
!                 turbulent kinetic energy is not given
!
                  vfa(6,i)=vfap(6,i)+gradkfa(1,i)*rf(1)+
     &                               gradkfa(2,i)*rf(2)+
     &                               gradkfa(3,i)*rf(3)
               endif
            else
!
!              zero gradient in i-direction
!
               indexf=ipnei(iel1)+ielfa(4,i)
               vfa(6,i)=vel(iel1,6)
     &                 +(gradkfa(1,i)*xxi(1,indexf)+
     &                   gradkfa(2,i)*xxi(2,indexf)+
     &                   gradkfa(3,i)*xxi(3,indexf))*xle(indexf)
            endif
         enddo
!c$omp end do
!c$omp end parallel
!
!     Multiple point constraints
!
      if(nmpc.gt.0) then
         is=6
         ie=6
         call applympc(nface,ielfa,is,ie,ifabou,ipompc,vfa,coefmpc,
     &        nodempc,ipnei,neifa,labmpc,xbounact,nactdoh,
     &        ifaext,nfaext)
      endif
!
!        calculate the gradient of the temperature at the center of
!        the elements
!
!c$omp parallel default(none)
!c$omp& shared(nef,ipnei,neifa,gradkel,vfa,area,xxn,volume)
!c$omp& private(i,indexf,ifa)
!c$omp do
         do i=1,nef
!
!           initialization
!     
            do l=1,3
               gradkel(l,i)=0.d0
            enddo
!
            do indexf=ipnei(i)+1,ipnei(i+1)
               ifa=neifa(indexf)
               do l=1,3
                  gradkel(l,i)=gradkel(l,i)+
     &                 vfa(6,ifa)*area(ifa)*xxn(l,indexf)
               enddo
            enddo
!     
!           dividing by the volume of the element
!     
            do l=1,3
               gradkel(l,i)=gradkel(l,i)/volume(i)
            enddo
         enddo
!c$omp end do
!c$omp end parallel
! 
!        interpolate/extrapolate the temperature gradient from the
!        center of the elements to the center of the faces
!           
!c$omp parallel default(none)
!c$omp& shared(nface,ielfa,xrlfa,gradkfa,gradkel,icyclic,c,ifatie,
!c$omp&        ipnei,xxn,ifabou)
!c$omp& private(i,iel1,xl1,iel2,l,xl2,indexf,gradnor)
!c$omp do
         do i=1,nface
            iel1=ielfa(1,i)
            xl1=xrlfa(1,i)
            iel2=ielfa(2,i)
            if(iel2.gt.0) then
!
!              face in between two elements
!
               xl2=xrlfa(2,i)
               if((icyclic.eq.0).or.(ifatie(i).eq.0)) then
                  do l=1,3
                     gradkfa(l,i)=xl1*gradkel(l,iel1)+
     &                    xl2*gradkel(l,iel2)
                  enddo
               elseif(ifatie(i).gt.0) then
                  do l=1,3
                     gradkfa(l,i)=xl1*gradkel(l,iel1)+xl2*
     &                    (gradkel(1,iel2)*c(l,1)+
     &                    gradkel(2,iel2)*c(l,2)+
     &                    gradkel(3,iel2)*c(l,3))
                  enddo
               else
                  do l=1,3
                     gradkfa(l,i)=xl1*gradkel(l,iel1)+xl2*
     &                    (gradkel(1,iel2)*c(1,l)+
     &                    gradkel(2,iel2)*c(2,l)+
     &                    gradkel(3,iel2)*c(3,l))
                  enddo
               endif
            elseif(ielfa(3,i).gt.0) then
!     
!           boundary face; no zero gradient
!
               do l=1,3
                  gradkfa(l,i)=xl1*gradkel(l,iel1)+
     &                 xrlfa(3,i)*gradkel(l,abs(ielfa(3,i)))
               enddo
            else
!     
!           boundary face; zero gradient in i-direction
!   
               indexf=ipnei(iel1)+ielfa(4,i)
               gradnor=gradkel(1,iel1)*xxi(1,indexf)+
     &              gradkel(2,iel1)*xxi(2,indexf)+
     &              gradkel(3,iel1)*xxi(3,indexf)
               do l=1,3
                  gradkfa(l,i)=gradkel(l,iel1)
     &                 -gradnor*xxi(l,indexf)
               enddo
            endif
         enddo
!c$omp end do
!c$omp end parallel
!
      enddo
!
!     correct the facial temperature gradients:
!     Moukalled et al. p 289
!
!c$omp parallel default(none)
!c$omp& shared(nface,ielfa,ipnei,vel,xlet,gradkfa,xxj)
!c$omp& private(i,iel2,iel1,indexf,dd,k)
!c$omp do
      do i=1,nface
         iel2=ielfa(2,i)
         if(iel2.gt.0) then
            iel1=ielfa(1,i)
            indexf=ipnei(iel1)+ielfa(4,i)
            dd=(vel(iel2,6)-vel(iel1,6))/xlet(indexf)
     &        -gradkfa(1,i)*xxj(1,indexf)
     &        -gradkfa(2,i)*xxj(2,indexf)
     &        -gradkfa(3,i)*xxj(3,indexf)
            do k=1,3
               gradkfa(k,i)=gradkfa(k,i)+dd*xxj(k,indexf)
            enddo
         endif
      enddo
!c$omp end do
!c$omp end parallel
!            
      return
      end
