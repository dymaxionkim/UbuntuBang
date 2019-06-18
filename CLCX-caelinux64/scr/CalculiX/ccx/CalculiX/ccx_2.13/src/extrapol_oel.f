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
      subroutine extrapol_oel(nface,ielfa,xrlfa,vel,vfa,
     &  ifabou,xbounact,nef,gradoel,gradofa,neifa,rf,area,volume,
     &  xle,xxi,icyclic,xxn,ipnei,ifatie,xlet,xxj,
     &  coefmpc,nmpc,labmpc,ipompc,nodempc,ifaext,nfaext,nactdoh,
     &  umfa,physcon,dy)
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
     &   vfap(0:7,nface),gradoel(3,*),gradofa(3,*),rf(3),area(*),
     &   volume(*),xle(*),xxi(3,*),c(3,3),gradnor,xxn(3,*),umfa(*),
     &   xxj(3,*),dd,xlet(*),coefmpc(*),cinf,physcon(*),dy(*)
!
      intent(in) nface,ielfa,xrlfa,vel,umfa,physcon,
     &  ifabou,xbounact,nef,neifa,rf,area,volume,
     &  xle,xxi,icyclic,xxn,ipnei,ifatie,xlet,xxj,
     &  coefmpc,nmpc,labmpc,ipompc,nodempc,ifaext,nfaext,nactdoh
!
      intent(inout) vfa,gradoel,gradofa
!
!     5.5 x 10**(-3.5) = 1.7393e-3
!
      cinf=5.5d0*physcon(5)/physcon(8)
!     
!c$omp parallel default(none)
!c$omp& shared(nface,ielfa,xrlfa,vfap,vel,ifabou,xbounact,cinf,
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
            vfap(7,i)=xl1*vel(iel1,7)+xrlfa(2,i)*vel(iel2,7)
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
!              wall: turbulent dissipation rate known
!     
               vfap(7,i)=dy(ifabou(ipointer+5))*umfa(i)/vfa(5,i)
            elseif(((ifabou(ipointer+1).gt.0).and.
     &              (ifabou(ipointer+2).gt.0).and.
     &              (ifabou(ipointer+3).gt.0)).or.
     &             (ifabou(ipointer+5).lt.0)) then
!     
!              inlet or sliding conditions: turbulent dissipation rate known
!     
               vfap(7,i)=cinf*umfa(i)
            else
!
!              extrapolation
!
               vfap(7,i)=xl1*vel(iel1,7)+xrlfa(3,i)*vel(ielfa(3,i),7)
            endif
         else
!
!           boundary face; zero gradient
!
            vfap(7,i)=vel(iel1,7)
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
!c$omp& shared(nef,ipnei,neifa,gradoel,vfap,area,xxn,volume)
!c$omp& private(i,indexf,ifa)
!c$omp do
      do i=1,nef
!
!        initialization
!     
         do l=1,3
            gradoel(l,i)=0.d0
         enddo
!
         do indexf=ipnei(i)+1,ipnei(i+1)
            ifa=neifa(indexf)
            do l=1,3
               gradoel(l,i)=gradoel(l,i)+
     &              vfap(7,ifa)*area(ifa)*xxn(l,indexf)
            enddo
         enddo
!     
!        dividing by the volume of the element
!     
         do l=1,3
            gradoel(l,i)=gradoel(l,i)/volume(i)
         enddo
      enddo
!c$omp end do
!c$omp end parallel
! 
!     interpolate/extrapolate the temperature gradient from the
!     center of the elements to the center of the faces
!           
!c$omp parallel default(none)
!c$omp& shared(nface,ielfa,xrlfa,gradofa,gradoel,icyclic,c,ifatie,
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
                  gradofa(l,i)=xl1*gradoel(l,iel1)+
     &                 xl2*gradoel(l,iel2)
               enddo
            elseif(ifatie(i).gt.0) then
               do l=1,3
                  gradofa(l,i)=xl1*gradoel(l,iel1)+xl2*
     &                  (gradoel(1,iel2)*c(l,1)+
     &                   gradoel(2,iel2)*c(l,2)+
     &                   gradoel(3,iel2)*c(l,3))
               enddo
            else
               do l=1,3
                  gradofa(l,i)=xl1*gradoel(l,iel1)+xl2*
     &                  (gradoel(1,iel2)*c(1,l)+
     &                   gradoel(2,iel2)*c(2,l)+
     &                   gradoel(3,iel2)*c(3,l))
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
               gradofa(l,i)=xl1*gradoel(l,iel1)+
     &              xrlfa(3,i)*gradoel(l,abs(ielfa(3,i)))
            enddo
         else
!     
!           boundary face; zero gradient in i-direction
!   
            indexf=ipnei(iel1)+ielfa(4,i)
            gradnor=gradoel(1,iel1)*xxi(1,indexf)+
     &              gradoel(2,iel1)*xxi(2,indexf)+
     &              gradoel(3,iel1)*xxi(3,indexf)
            do l=1,3
                  gradofa(l,i)=gradoel(l,iel1)
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
!c$omp& shared(nface,ielfa,xrlfa,vfa,vfap,gradofa,rf,ifabou,xxi,xle,
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
               vfa(7,i)=vfap(7,i)+gradofa(1,i)*rf(1)
     &                         +gradofa(2,i)*rf(2)
     &                         +gradofa(3,i)*rf(3)
            elseif(ielfa(3,i).gt.0) then
!
!              no implicit zero gradient
!
               ipointer=-iel2
!
               if(ifabou(ipointer+5).gt.0) then
!     
!                 wall: turbulent dissipation rate known
!     
                  vfa(7,i)=vfap(7,i)
               elseif(((ifabou(ipointer+1).gt.0).and.
     &                 (ifabou(ipointer+2).gt.0).and.
     &                 (ifabou(ipointer+3).gt.0)).or.
     &                (ifabou(ipointer+5).lt.0)) then
!     
!                 inlet or sliding conditions: turbulent dissipation rate known
!     
                  vfa(7,i)=vfap(7,i)
               else
!
!                 turbulent kinetic energy is not given
!
                  vfa(7,i)=vfap(7,i)+gradofa(1,i)*rf(1)+
     &                               gradofa(2,i)*rf(2)+
     &                               gradofa(3,i)*rf(3)
               endif
            else
!
!              zero gradient in i-direction
!
               indexf=ipnei(iel1)+ielfa(4,i)
               vfa(7,i)=vel(iel1,7)
     &                 +(gradofa(1,i)*xxi(1,indexf)+
     &                   gradofa(2,i)*xxi(2,indexf)+
     &                   gradofa(3,i)*xxi(3,indexf))*xle(indexf)
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
!c$omp& shared(nef,ipnei,neifa,gradoel,vfa,area,xxn,volume)
!c$omp& private(i,indexf,ifa)
!c$omp do
         do i=1,nef
!
!           initialization
!     
            do l=1,3
               gradoel(l,i)=0.d0
            enddo
!
            do indexf=ipnei(i)+1,ipnei(i+1)
               ifa=neifa(indexf)
               do l=1,3
                  gradoel(l,i)=gradoel(l,i)+
     &                 vfa(7,ifa)*area(ifa)*xxn(l,indexf)
               enddo
            enddo
!     
!           dividing by the volume of the element
!     
            do l=1,3
               gradoel(l,i)=gradoel(l,i)/volume(i)
            enddo
         enddo
!c$omp end do
!c$omp end parallel
! 
!        interpolate/extrapolate the temperature gradient from the
!        center of the elements to the center of the faces
!           
!c$omp parallel default(none)
!c$omp& shared(nface,ielfa,xrlfa,gradofa,gradoel,icyclic,c,ifatie,
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
                     gradofa(l,i)=xl1*gradoel(l,iel1)+
     &                    xl2*gradoel(l,iel2)
                  enddo
               elseif(ifatie(i).gt.0) then
                  do l=1,3
                     gradofa(l,i)=xl1*gradoel(l,iel1)+xl2*
     &                    (gradoel(1,iel2)*c(l,1)+
     &                    gradoel(2,iel2)*c(l,2)+
     &                    gradoel(3,iel2)*c(l,3))
                  enddo
               else
                  do l=1,3
                     gradofa(l,i)=xl1*gradoel(l,iel1)+xl2*
     &                    (gradoel(1,iel2)*c(1,l)+
     &                    gradoel(2,iel2)*c(2,l)+
     &                    gradoel(3,iel2)*c(3,l))
                  enddo
               endif
            elseif(ielfa(3,i).gt.0) then
!     
!           boundary face; no zero gradient
!
               do l=1,3
                  gradofa(l,i)=xl1*gradoel(l,iel1)+
     &                 xrlfa(3,i)*gradoel(l,abs(ielfa(3,i)))
               enddo
            else
!     
!           boundary face; zero gradient in i-direction
!   
               indexf=ipnei(iel1)+ielfa(4,i)
               gradnor=gradoel(1,iel1)*xxi(1,indexf)+
     &              gradoel(2,iel1)*xxi(2,indexf)+
     &              gradoel(3,iel1)*xxi(3,indexf)
               do l=1,3
                  gradofa(l,i)=gradoel(l,iel1)
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
!c$omp& shared(nface,ielfa,ipnei,vel,xlet,gradofa,xxj)
!c$omp& private(i,iel2,iel1,indexf,dd,k)
!c$omp do
      do i=1,nface
         iel2=ielfa(2,i)
         if(iel2.gt.0) then
            iel1=ielfa(1,i)
            indexf=ipnei(iel1)+ielfa(4,i)
            dd=(vel(iel2,7)-vel(iel1,7))/xlet(indexf)
     &        -gradofa(1,i)*xxj(1,indexf)
     &        -gradofa(2,i)*xxj(2,indexf)
     &        -gradofa(3,i)*xxj(3,indexf)
            do k=1,3
               gradofa(k,i)=gradofa(k,i)+dd*xxj(k,indexf)
            enddo
         endif
      enddo
!c$omp end do
!c$omp end parallel
!            
      return
      end
