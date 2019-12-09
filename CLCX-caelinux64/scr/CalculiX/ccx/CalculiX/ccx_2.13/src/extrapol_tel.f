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
      subroutine extrapol_tel(nface,ielfa,xrlfa,vel,vfa,
     &  ifabou,xbounact,nef,gradtel,gradtfa,neifa,rf,area,volume,
     &  xle,xxi,icyclic,xxn,ipnei,ifatie,xload,xlet,xxj,
     &  coefmpc,nmpc,labmpc,ipompc,nodempc,ifaext,nfaext,nactdoh)
!
!     extrapolation of temperature values to the faces
!
!     for temperature calculations the external faces have
!     as boundary condition either a specified temperature or
!     a specified flux. If the user did not apply any of these,
!     a zero specified flux is implicitly assumed
!
      implicit none
!
      character*20 labmpc(*)
!
      integer nface,ielfa(4,*),ifabou(*),i,iel1,iel2,nef,ibou,
     &  neifa(*),icyclic,ifa,indexf,k,l,m,ipnei(*),ifatie(*),
     &  is,ie,nmpc,ipompc(*),nodempc(3,*),ifaext(*),nfaext,nactdoh(*)
!
      real*8 xrlfa(3,*),vel(nef,0:7),vfa(0:7,*),xbounact(*),xl1,xl2,
     &   vfap(0:7,nface),gradtel(3,*),gradtfa(3,*),rf(3),area(*),
     &   volume(*),xle(*),xxi(3,*),c(3,3),gradnor,xxn(3,*),
     &   xload(2,*),xxj(3,*),dd,xlet(*),coefmpc(*),q
!
      intent(in) nface,ielfa,xrlfa,vel,
     &  ifabou,xbounact,nef,neifa,rf,area,volume,
     &  xle,xxi,icyclic,xxn,ipnei,ifatie,xload,xlet,xxj,
     &  coefmpc,nmpc,labmpc,ipompc,nodempc,ifaext,nfaext,nactdoh
!
      intent(inout) vfa,gradtel,gradtfa
!     
!c$omp parallel default(none)
!c$omp& shared(nface,ielfa,xrlfa,vfap,vel,ifabou,xbounact)
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
            vfap(0,i)=xl1*vel(iel1,0)+xrlfa(2,i)*vel(iel2,0)
         elseif(ielfa(3,i).gt.0) then
!
!           boundary face; no zero gradient
!           
            if(ifabou(-iel2).gt.0) then
!
!              temperature boundary condition
!
               vfap(0,i)=xbounact(ifabou(-iel2))
            else
!
!              flux boundary condition
!
               vfap(0,i)=xl1*vel(iel1,0)+xrlfa(3,i)*vel(ielfa(3,i),0)
            endif
         else
!
!           boundary face; zero gradient
!
            vfap(0,i)=vel(iel1,0)
         endif
      enddo
!c$omp end do
!c$omp end parallel
!
!     Multiple point constraints
!
      if(nmpc.gt.0) then
         is=0
         ie=0
         call applympc(nface,ielfa,is,ie,ifabou,ipompc,vfap,coefmpc,
     &        nodempc,ipnei,neifa,labmpc,xbounact,nactdoh,
     &        ifaext,nfaext)
      endif
!
!     calculate the gradient of the temperature at the center of
!     the elements
!
!c$omp parallel default(none)
!c$omp& shared(nef,ipnei,neifa,gradtel,vfap,area,xxn,volume)
!c$omp& private(i,indexf,ifa)
!c$omp do
      do i=1,nef
!
!        initialization
!     
         do l=1,3
            gradtel(l,i)=0.d0
         enddo
!
         do indexf=ipnei(i)+1,ipnei(i+1)
            ifa=neifa(indexf)
            do l=1,3
               gradtel(l,i)=gradtel(l,i)+
     &              vfap(0,ifa)*area(ifa)*xxn(l,indexf)
            enddo
         enddo
!     
!        dividing by the volume of the element
!     
         do l=1,3
            gradtel(l,i)=gradtel(l,i)/volume(i)
         enddo
      enddo
!c$omp end do
!c$omp end parallel
! 
!     interpolate/extrapolate the temperature gradient from the
!     center of the elements to the center of the faces
!           
!c$omp parallel default(none)
!c$omp& shared(nface,ielfa,xrlfa,gradtfa,gradtel,icyclic,c,ifatie,
!c$omp&        ipnei,xxn,ifabou,xload)
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
                  gradtfa(l,i)=xl1*gradtel(l,iel1)+
     &                 xl2*gradtel(l,iel2)
               enddo
            elseif(ifatie(i).gt.0) then
               do l=1,3
                  gradtfa(l,i)=xl1*gradtel(l,iel1)+xl2*
     &                  (gradtel(1,iel2)*c(l,1)+
     &                   gradtel(2,iel2)*c(l,2)+
     &                   gradtel(3,iel2)*c(l,3))
               enddo
            else
               do l=1,3
                  gradtfa(l,i)=xl1*gradtel(l,iel1)+xl2*
     &                  (gradtel(1,iel2)*c(1,l)+
     &                   gradtel(2,iel2)*c(2,l)+
     &                   gradtel(3,iel2)*c(3,l))
               enddo
            endif
         elseif(ielfa(3,i).gt.0) then
!
!           the above condition implies iel2!=0
!           if iel2 were zero, no b.c. would apply and
!           ielfa(3,i) would be zero or negative
!     
!           boundary face; no zero gradient
!
            if(ifabou(-iel2).gt.0) then
!
!              temperature given: extrapolate gradient
!
               do l=1,3
                  gradtfa(l,i)=xl1*gradtel(l,iel1)+
     &                 xrlfa(3,i)*gradtel(l,abs(ielfa(3,i)))
               enddo
            else
!
!              facial temperature gradient given, may be nonzero
!
               indexf=ipnei(iel1)+ielfa(4,i)
!
               if(ifabou(-iel2+6).eq.0) then
                  q=0.d0
               else
                  q=xload(1,ifabou(-iel2+6))
               endif
               gradnor=gradtel(1,iel1)*xxn(1,indexf)
     &                +gradtel(2,iel1)*xxn(2,indexf)
     &                +gradtel(3,iel1)*xxn(3,indexf)-q
c     &                -xload(1,ifabou(-iel2+6))
               do l=1,3
                  gradtfa(l,i)=gradtel(l,iel1)
     &                        -gradnor*xxn(l,indexf)
               enddo
            endif
         else
!     
!           boundary face; zero gradient
!   
            indexf=ipnei(iel1)+ielfa(4,i)
            gradnor=gradtel(1,iel1)*xxi(1,indexf)+
     &              gradtel(2,iel1)*xxi(2,indexf)+
     &              gradtel(3,iel1)*xxi(3,indexf)
            do l=1,3
                  gradtfa(l,i)=gradtel(l,iel1)
     &                        -gradnor*xxi(l,indexf)
            enddo
         endif
      enddo
!c$omp end do
!c$omp end parallel
!
!     correction loops
!
c      do i=1,nface
c         vfa(0,i)=vfap(0,i)
c      enddo
      do m=1,2
c      do m=1,1
!
!        Moukalled et al. p 279
!
!c$omp parallel default(none)
!c$omp& shared(nface,ielfa,xrlfa,vfa,vfap,gradtfa,rf,ifabou,xxi,xle,
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
               vfa(0,i)=vfap(0,i)+gradtfa(1,i)*rf(1)
     &                         +gradtfa(2,i)*rf(2)
     &                         +gradtfa(3,i)*rf(3)
            elseif(ielfa(3,i).gt.0) then
!
!              no implicit zero gradient
!
               if(ifabou(-iel2).gt.0) then
!
!                 temperature given
!
                  vfa(0,i)=vfap(0,i)
               else
!
!                 flux given: gradient=flux
!
                  indexf=ipnei(iel1)+ielfa(4,i)
                  vfa(0,i)=vel(iel1,0)
     &                    +(gradtfa(1,i)*xxi(1,indexf)+
     &                      gradtfa(2,i)*xxi(2,indexf)+
     &                      gradtfa(3,i)*xxi(3,indexf))*xle(indexf)
               endif
            else
!
!                 zero gradient
!
c               write(*,*) 'extrapol_tel ',iel1,iel2
c               write(*,*) 'extrapol_tel ',vel(iel1,0)
c               write(*,*) 'extrapol_tel ',gradtfa(1,i),xxi(1,indexf)
c               write(*,*) 'extrapol_tel ',gradtfa(2,i),xxi(2,indexf)
c               write(*,*) 'extrapol_tel ',gradtfa(3,i),xxi(3,indexf)
               indexf=ipnei(iel1)+ielfa(4,i)
               vfa(0,i)=vel(iel1,0)
     &                 +(gradtfa(1,i)*xxi(1,indexf)+
     &                   gradtfa(2,i)*xxi(2,indexf)+
     &                   gradtfa(3,i)*xxi(3,indexf))*xle(indexf)
            endif
         enddo
!c$omp end do
!c$omp end parallel
!
!     Multiple point constraints
!
      if(nmpc.gt.0) then
         is=0
         ie=0
         call applympc(nface,ielfa,is,ie,ifabou,ipompc,vfa,coefmpc,
     &        nodempc,ipnei,neifa,labmpc,xbounact,nactdoh,
     &        ifaext,nfaext)
      endif
!
!        calculate the gradient of the temperature at the center of
!        the elements
!
!c$omp parallel default(none)
!c$omp& shared(nef,ipnei,neifa,gradtel,vfa,area,xxn,volume)
!c$omp& private(i,indexf,ifa)
!c$omp do
         do i=1,nef
!
!           initialization
!     
            do l=1,3
               gradtel(l,i)=0.d0
            enddo
!
            do indexf=ipnei(i)+1,ipnei(i+1)
               ifa=neifa(indexf)
               do l=1,3
                  gradtel(l,i)=gradtel(l,i)+
     &                 vfa(0,ifa)*area(ifa)*xxn(l,indexf)
               enddo
            enddo
!     
!           dividing by the volume of the element
!     
            do l=1,3
               gradtel(l,i)=gradtel(l,i)/volume(i)
            enddo
         enddo
!c$omp end do
!c$omp end parallel
! 
!        interpolate/extrapolate the temperature gradient from the
!        center of the elements to the center of the faces
!           
!c$omp parallel default(none)
!c$omp& shared(nface,ielfa,xrlfa,gradtfa,gradtel,icyclic,c,ifatie,
!c$omp&        ipnei,xxn,ifabou,xload)
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
                     gradtfa(l,i)=xl1*gradtel(l,iel1)+
     &                    xl2*gradtel(l,iel2)
                  enddo
               elseif(ifatie(i).gt.0) then
                  do l=1,3
                     gradtfa(l,i)=xl1*gradtel(l,iel1)+xl2*
     &                    (gradtel(1,iel2)*c(l,1)+
     &                    gradtel(2,iel2)*c(l,2)+
     &                    gradtel(3,iel2)*c(l,3))
                  enddo
               else
                  do l=1,3
                     gradtfa(l,i)=xl1*gradtel(l,iel1)+xl2*
     &                    (gradtel(1,iel2)*c(1,l)+
     &                    gradtel(2,iel2)*c(2,l)+
     &                    gradtel(3,iel2)*c(3,l))
                  enddo
               endif
            elseif(ielfa(3,i).gt.0) then
!     
!              no zero gradient
!
               if(ifabou(-iel2).gt.0) then
!
!                 temperature given: extrapolate gradient
!
                  do l=1,3
                     gradtfa(l,i)=xl1*gradtel(l,iel1)+
     &                    xrlfa(3,i)*gradtel(l,abs(ielfa(3,i)))
                  enddo
               else
!
!                 facial temperature gradient given, may be nonzero
!
                  indexf=ipnei(iel1)+ielfa(4,i)
!
                  if(ifabou(-iel2+6).eq.0) then
                     q=0.d0
                  else
                     q=xload(1,ifabou(-iel2+6))
                  endif
!
                  gradnor=gradtel(1,iel1)*xxn(1,indexf)
     &                 +gradtel(2,iel1)*xxn(2,indexf)
     &                 +gradtel(3,iel1)*xxn(3,indexf)-q
c     &                 -xload(1,ifabou(-iel2+6))
                  do l=1,3
                     gradtfa(l,i)=gradtel(l,iel1)
     &                    -gradnor*xxn(l,indexf)
                  enddo
               endif
            else
!     
!              zero temperature gradient in i-direction
!   
               indexf=ipnei(iel1)+ielfa(4,i)
               gradnor=gradtel(1,iel1)*xxi(1,indexf)+
     &              gradtel(2,iel1)*xxi(2,indexf)+
     &              gradtel(3,iel1)*xxi(3,indexf)
               do l=1,3
                  gradtfa(l,i)=gradtel(l,iel1)
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
!c$omp& shared(nface,ielfa,ipnei,vel,xlet,gradtfa,xxj)
!c$omp& private(i,iel2,iel1,indexf,dd,k)
!c$omp do
      do i=1,nface
         iel2=ielfa(2,i)
         if(iel2.gt.0) then
            iel1=ielfa(1,i)
            indexf=ipnei(iel1)+ielfa(4,i)
            dd=(vel(iel2,0)-vel(iel1,0))/xlet(indexf)
     &        -gradtfa(1,i)*xxj(1,indexf)
     &        -gradtfa(2,i)*xxj(2,indexf)
     &        -gradtfa(3,i)*xxj(3,indexf)
            do k=1,3
               gradtfa(k,i)=gradtfa(k,i)+dd*xxj(k,indexf)
            enddo
         endif
      enddo
!c$omp end do
!c$omp end parallel
!            
      return
      end
