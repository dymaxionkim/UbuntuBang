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
      subroutine extrapol_vel(nface,ielfa,xrlfa,vel,vfa,
     &  ifabou,xbounact,ipnei,nef,icyclic,c,ifatie,xxn,gradvel,
     &  gradvfa,neifa,rf,area,volume,xle,xxi,xxj,xlet,
     &  coefmpc,nmpc,labmpc,ipompc,nodempc,ifaext,nfaext,nactdoh)
!
!     inter/extrapolation of v at the center of the elements
!     to the center of the faces
!
      implicit none
!
      character*20 labmpc(*)
!
      integer nface,ielfa(4,*),ifabou(*),iel1,iel2,iel3,i,j,ipointer,
     &  indexf,ipnei(*),nef,icyclic,ifatie(*),k,l,m,ifa,neifa(*),
     &  is,ie,nmpc,ipompc(*),nodempc(3,*),ifaext(*),nfaext,nactdoh(*)
!
      real*8 xrlfa(3,*),vel(nef,0:7),vfa(0:7,*),xbounact(*),xl1,xl2,
     &  c(3,3),xxn(3,*),dd,vfap(0:7,nface),gradvel(3,3,*),
     &  gradvfa(3,3,*),
     &  rf(3),area(*),volume(*),xle(*),xxi(3,*),gradnor,xxj(3,*),
     &  xlet(*),coefmpc(*)
!
      intent(in) nface,ielfa,xrlfa,vel,
     &  ifabou,xbounact,ipnei,nef,icyclic,c,ifatie,xxn,
     &  neifa,rf,area,volume,xle,xxi,xxj,xlet,
     &  coefmpc,nmpc,labmpc,ipompc,nodempc,ifaext,nfaext,nactdoh
!
      intent(inout) vfa,gradvel,gradvfa
!
!     initialization of the facial velocities
!
!c$omp parallel default(none)
!c$omp& shared(nface,ielfa,xrlfa,vfap,vel,ipnei,ifabou,xbounact,
!c$omp&        icyclic,c,ifatie,xxn)
!c$omp& private(i,iel1,xl1,iel2,xl2,j,iel3,ipointer,indexf,dd)
!c$omp do
      do i=1,nface
         iel1=ielfa(1,i)
         xl1=xrlfa(1,i)
         iel2=ielfa(2,i)
         if(iel2.gt.0) then
!
!           face between two elements: interpolation
!
            xl2=xrlfa(2,i)
            if((icyclic.eq.0).or.(ifatie(i).eq.0)) then
               do j=1,3
                  vfap(j,i)=xl1*vel(iel1,j)+xl2*vel(iel2,j)
               enddo
            elseif(ifatie(i).gt.0) then
               do j=1,3
                  vfap(j,i)=xl1*vel(iel1,j)+xl2*
     &              (c(j,1)*vel(iel2,1)
     &              +c(j,2)*vel(iel2,2)
     &              +c(j,3)*vel(iel2,3))
               enddo
            else
               do j=1,3
                  vfap(j,i)=xl1*vel(iel1,j)+xl2*
     &              (c(1,j)*vel(iel2,1)
     &              +c(2,j)*vel(iel2,2)
     &              +c(3,j)*vel(iel2,3))
               enddo
            endif
         elseif(ielfa(3,i).gt.0) then
!
!           boundary face; no zero gradient
!
            iel3=ielfa(3,i)
            ipointer=-iel2
!
!           global x-direction
!     
            if(ifabou(ipointer+1).gt.0) then
!     
!              v_1 given
!     
               vfap(1,i)=xbounact(ifabou(ipointer+1))
            else
!     
!              extrapolation
!     
               vfap(1,i)=xl1*vel(iel1,1)+xrlfa(3,i)*vel(iel3,1)
            endif
!     
!           global y-direction
!     
            if(ifabou(ipointer+2).gt.0) then
!     
!              v_2 given
!     
               vfap(2,i)=xbounact(ifabou(ipointer+2))
            else
!     
!              extrapolation
!     
               vfap(2,i)=xl1*vel(iel1,2)+xrlfa(3,i)*vel(iel3,2)
            endif
!     
!           global z-direction
!     
            if(ifabou(ipointer+3).gt.0) then
!     
!              v_3 given
!     
               vfap(3,i)=xbounact(ifabou(ipointer+3))
            else
!     
!              extrapolation
!     
               vfap(3,i)=xl1*vel(iel1,3)+xrlfa(3,i)*vel(iel3,3)
            endif
!     
!           correction for sliding boundary conditions        
!     
            if(ifabou(ipointer+5).lt.0) then
               indexf=ipnei(iel1)+ielfa(4,i)
               dd=vfap(1,i)*xxn(1,indexf)+
     &              vfap(2,i)*xxn(2,indexf)+
     &              vfap(3,i)*xxn(3,indexf)
               do j=1,3
                  vfap(j,i)=vfap(j,i)-dd*xxn(j,indexf)
               enddo
            endif
!     
         else
!     
!           boundary face; zero gradient
!     
            do j=1,3
               vfap(j,i)=vel(iel1,j)
            enddo
         endif
      enddo
!c$omp end do
!c$omp end parallel
!
!     Multiple point constraints
!
      if(nmpc.gt.0) then
         is=1
         ie=3
         call applympc(nface,ielfa,is,ie,ifabou,ipompc,vfap,coefmpc,
     &        nodempc,ipnei,neifa,labmpc,xbounact,nactdoh,
     &        ifaext,nfaext)
      endif
!
!     calculate the gradient of the velocities at the center of
!     the elements
!
!c$omp parallel default(none)
!c$omp& shared(nef,ipnei,neifa,gradvel,vfap,area,xxn,volume)
!c$omp& private(i,indexf,ifa,k,l)
!c$omp do
      do i=1,nef
!
!           initialization
!
         do k=1,3
            do l=1,3
               gradvel(k,l,i)=0.d0
            enddo
         enddo
!
         do indexf=ipnei(i)+1,ipnei(i+1)
            ifa=neifa(indexf)
            do k=1,3
               do l=1,3
                  gradvel(k,l,i)=gradvel(k,l,i)+
     &                 vfap(k,ifa)*area(ifa)*xxn(l,indexf)
               enddo
            enddo
         enddo
!     
!     dividing by the volume of the element
!     
         do k=1,3
            do l=1,3
               gradvel(k,l,i)=gradvel(k,l,i)/volume(i)
            enddo
         enddo
      enddo
!c$omp end do
!c$omp end parallel
! 
!     interpolate/extrapolate the velocity gradient from the
!     center of the elements to the center of the faces
!           
!c$omp parallel default(none)
!c$omp& shared(nface,ielfa,xrlfa,gradvfa,gradvel,icyclic,c,ifatie,
!c$omp&        indexf,ipnei,xxn)
!c$omp& private(i,iel1,xl1,iel2,k,l,xl2,gradnor)
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
               do k=1,3
                  do l=1,3
                     gradvfa(k,l,i)=xl1*gradvel(k,l,iel1)+
     &                    xl2*gradvel(k,l,iel2)
                  enddo
               enddo
            elseif(ifatie(i).gt.0) then
               do k=1,3
                  do l=1,3
                     gradvfa(k,l,i)=xl1*gradvel(k,l,iel1)+xl2*
     &                   (c(k,1)*gradvel(1,1,iel2)*c(l,1)+
     &                    c(k,1)*gradvel(1,2,iel2)*c(l,2)+
     &                    c(k,1)*gradvel(1,3,iel2)*c(l,3)+
     &                    c(k,2)*gradvel(2,1,iel2)*c(l,1)+
     &                    c(k,2)*gradvel(2,2,iel2)*c(l,2)+
     &                    c(k,2)*gradvel(2,3,iel2)*c(l,3)+
     &                    c(k,3)*gradvel(3,1,iel2)*c(l,1)+
     &                    c(k,3)*gradvel(3,2,iel2)*c(l,2)+
     &                    c(k,3)*gradvel(3,3,iel2)*c(l,3))
                  enddo
               enddo
            else
               do k=1,3
                  do l=1,3
                     gradvfa(k,l,i)=xl1*gradvel(k,l,iel1)+xl2*
     &                   (c(1,k)*gradvel(1,1,iel2)*c(1,l)+
     &                    c(1,k)*gradvel(1,2,iel2)*c(2,l)+
     &                    c(1,k)*gradvel(1,3,iel2)*c(3,l)+
     &                    c(2,k)*gradvel(2,1,iel2)*c(1,l)+
     &                    c(2,k)*gradvel(2,2,iel2)*c(2,l)+
     &                    c(2,k)*gradvel(2,3,iel2)*c(3,l)+
     &                    c(3,k)*gradvel(3,1,iel2)*c(1,l)+
     &                    c(3,k)*gradvel(3,2,iel2)*c(2,l)+
     &                    c(3,k)*gradvel(3,3,iel2)*c(3,l))
                  enddo
               enddo
            endif
         elseif(ielfa(3,i).gt.0) then
!
!           boundary face; no zero gradient
!
            do k=1,3
               do l=1,3
                  gradvfa(k,l,i)=xl1*gradvel(k,l,iel1)+
     &                           xrlfa(3,i)*gradvel(k,l,ielfa(3,i))
               enddo
            enddo
         else
!
!           boundary face; zero gradient in i-direction
!
            indexf=ipnei(iel1)+ielfa(4,i)
            do k=1,3
               gradnor=gradvel(k,1,iel1)*xxi(1,indexf)
     &                +gradvel(k,2,iel1)*xxi(2,indexf)
     &                +gradvel(k,3,iel1)*xxi(3,indexf)
               do l=1,3
                  gradvfa(k,l,i)=gradvel(k,l,iel1)
     &                          -gradnor*xxi(l,indexf)
               enddo
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
!c$omp& shared(nface,ielfa,vfa,vfap,gradvfa,rf,ifabou,xxn,ipnei,xxi,
!c$omp&        xle,vel)
!c$omp& private(i,iel1,iel2,ipointer,dd,j,indexf)
!c$omp do
         do i=1,nface
            iel1=ielfa(1,i)
            iel2=ielfa(2,i)
            if(iel2.gt.0) then
!
!              face between two elements
!
               do j=1,3
                  vfa(j,i)=vfap(j,i)+gradvfa(j,1,i)*rf(1)+
     &                               gradvfa(j,2,i)*rf(2)+
     &                               gradvfa(j,3,i)*rf(3)
               enddo
            elseif(ielfa(3,i).gt.0) then
!
!              boundary face; no zero gradient
!
               ipointer=-iel2
!
!              x-direction
!
               if(ifabou(ipointer+1).gt.0) then
                  vfa(1,i)=vfap(1,i)
               else
                  vfa(1,i)=vfap(1,i)+gradvfa(1,1,i)*rf(1)+
     &                               gradvfa(1,2,i)*rf(2)+
     &                               gradvfa(1,3,i)*rf(3)
               endif
!
!              y-direction
!
               if(ifabou(ipointer+2).gt.0) then
                  vfa(2,i)=vfap(2,i)
               else
                  vfa(2,i)=vfap(2,i)+gradvfa(2,1,i)*rf(1)+
     &                               gradvfa(2,2,i)*rf(2)+
     &                               gradvfa(2,3,i)*rf(3)
               endif
!
!              z-direction
!
               if(ifabou(ipointer+3).gt.0) then
                  vfa(3,i)=vfap(3,i)
               else
                  vfa(3,i)=vfap(3,i)+gradvfa(3,1,i)*rf(1)+
     &                               gradvfa(3,2,i)*rf(2)+
     &                               gradvfa(3,3,i)*rf(3)
               endif
!
!              correction for sliding boundary conditions        
!
               if(ifabou(ipointer+5).lt.0) then
                  indexf=ipnei(iel1)+ielfa(4,i)
                  dd=vfa(1,i)*xxn(1,indexf)+
     &               vfa(2,i)*xxn(2,indexf)+
     &               vfa(3,i)*xxn(3,indexf)
                  do j=1,3
                     vfa(j,i)=vfa(j,i)-dd*xxn(j,indexf)
                  enddo
               endif
            else
!
!              boundary face; zero gradient
!
               indexf=ipnei(iel1)+ielfa(4,i)
               do j=1,3
                  vfa(j,i)=vel(iel1,j)+
     &                    (gradvfa(j,1,i)*xxi(1,indexf)+
     &                     gradvfa(j,2,i)*xxi(2,indexf)+
     &                     gradvfa(j,3,i)*xxi(3,indexf))*xle(indexf)
               enddo
            endif
         enddo
!c$omp end do
!c$omp end parallel
!
!     Multiple point constraints
!
      if(nmpc.gt.0) then
         is=1
         ie=3
         call applympc(nface,ielfa,is,ie,ifabou,ipompc,vfa,coefmpc,
     &        nodempc,ipnei,neifa,labmpc,xbounact,nactdoh,
     &        ifaext,nfaext)
      endif
!
!     calculate the gradient of the velocities at the center of
!     the elements
!
!c$omp parallel default(none)
!c$omp& shared(nef,ipnei,neifa,gradvel,vfa,area,xxn,volume)
!c$omp& private(i,indexf,ifa,k,l)
!c$omp do
         do i=1,nef
!
!           initialization
!
            do k=1,3
               do l=1,3
                  gradvel(k,l,i)=0.d0
               enddo
            enddo
!
            do indexf=ipnei(i)+1,ipnei(i+1)
               ifa=neifa(indexf)
               do k=1,3
                  do l=1,3
                     gradvel(k,l,i)=gradvel(k,l,i)+
     &                    vfa(k,ifa)*area(ifa)*xxn(l,indexf)
                  enddo
               enddo
            enddo
!     
!           dividing by the volume of the element
!     
            do k=1,3
               do l=1,3
                  gradvel(k,l,i)=gradvel(k,l,i)/volume(i)
               enddo
            enddo
         enddo
!c$omp end do
!c$omp end parallel
! 
!        interpolate/extrapolate the velocity gradient from the
!        center of the elements to the center of the faces
!           
!c$omp parallel default(none)
!c$omp& shared(nface,ielfa,xrlfa,gradvfa,gradvel,icyclic,c,ifatie,
!c$omp&        indexf,ipnei,xxn)
!c$omp& private(i,iel1,xl1,iel2,k,l,xl2)
!c$omp do
         do i=1,nface
            iel1=ielfa(1,i)
            xl1=xrlfa(1,i)
            iel2=ielfa(2,i)
            if(iel2.gt.0) then
!
!           face in between two elements
!
               xl2=xrlfa(2,i)
               if((icyclic.eq.0).or.(ifatie(i).eq.0)) then
                  do k=1,3
                     do l=1,3
                        gradvfa(k,l,i)=xl1*gradvel(k,l,iel1)+
     &                       xl2*gradvel(k,l,iel2)
                     enddo
                  enddo
               elseif(ifatie(i).gt.0) then
                  do k=1,3
                     do l=1,3
                        gradvfa(k,l,i)=xl1*gradvel(k,l,iel1)+xl2*
     &                       (c(k,1)*gradvel(1,1,iel2)*c(l,1)+
     &                       c(k,1)*gradvel(1,2,iel2)*c(l,2)+
     &                       c(k,1)*gradvel(1,3,iel2)*c(l,3)+
     &                       c(k,2)*gradvel(2,1,iel2)*c(l,1)+
     &                       c(k,2)*gradvel(2,2,iel2)*c(l,2)+
     &                       c(k,2)*gradvel(2,3,iel2)*c(l,3)+
     &                       c(k,3)*gradvel(3,1,iel2)*c(l,1)+
     &                       c(k,3)*gradvel(3,2,iel2)*c(l,2)+
     &                       c(k,3)*gradvel(3,3,iel2)*c(l,3))
                     enddo
                  enddo
               else
                  do k=1,3
                     do l=1,3
                        gradvfa(k,l,i)=xl1*gradvel(k,l,iel1)+xl2*
     &                       (c(1,k)*gradvel(1,1,iel2)*c(1,l)+
     &                       c(1,k)*gradvel(1,2,iel2)*c(2,l)+
     &                       c(1,k)*gradvel(1,3,iel2)*c(3,l)+
     &                       c(2,k)*gradvel(2,1,iel2)*c(1,l)+
     &                       c(2,k)*gradvel(2,2,iel2)*c(2,l)+
     &                       c(2,k)*gradvel(2,3,iel2)*c(3,l)+
     &                       c(3,k)*gradvel(3,1,iel2)*c(1,l)+
     &                       c(3,k)*gradvel(3,2,iel2)*c(2,l)+
     &                       c(3,k)*gradvel(3,3,iel2)*c(3,l))
                     enddo
                  enddo
               endif
            elseif(ielfa(3,i).gt.0) then
!     
!              boundary face; no zero gradient
!     
               do k=1,3
                  do l=1,3
                     gradvfa(k,l,i)=xl1*gradvel(k,l,iel1)+
     &                    xrlfa(3,i)*gradvel(k,l,ielfa(3,i))
                  enddo
               enddo
            else
!     
!              boundary face; zero gradient in i-direction
!
               indexf=ipnei(iel1)+ielfa(4,i)
               do k=1,3
                  gradnor=gradvel(k,1,iel1)*xxi(1,indexf)
     &                   +gradvel(k,2,iel1)*xxi(2,indexf)
     &                   +gradvel(k,3,iel1)*xxi(3,indexf)
                  do l=1,3
                     gradvfa(k,l,i)=gradvel(k,l,iel1)
     &                    -gradnor*xxi(l,indexf)
                  enddo
               enddo
            endif
         enddo
!c$omp end do
!c$omp end parallel
      enddo
!
!     correct the facial velocity gradients:
!     Moukalled et al. p 289
!
!c$omp parallel default(none)
!c$omp& shared(nface,ielfa,ipnei,vel,xlet,gradvfa,xxj)
!c$omp& private(i,iel2,iel1,indexf,dd,k,l)
!c$omp do
      do i=1,nface
         iel2=ielfa(2,i)
         if(iel2.gt.0) then
            iel1=ielfa(1,i)
            indexf=ipnei(iel1)+ielfa(4,i)
            do l=1,3
               dd=(vel(iel2,l)-vel(iel1,l))/xlet(indexf)
     &              -gradvfa(l,1,i)*xxj(1,indexf)
     &              -gradvfa(l,2,i)*xxj(2,indexf)
     &              -gradvfa(l,3,i)*xxj(3,indexf)
               do k=1,3
                  gradvfa(l,k,i)=gradvfa(l,k,i)+dd*xxj(k,indexf)
               enddo
            enddo
         endif
      enddo
!c$omp end do
!c$omp end parallel
!
      return
      end
