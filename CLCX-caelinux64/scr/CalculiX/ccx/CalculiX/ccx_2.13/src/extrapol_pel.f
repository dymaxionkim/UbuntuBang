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
      subroutine extrapol_pel(nface,ielfa,xrlfa,vel,vfa,
     &  ifabou,xbounact,nef,gradpel,gradpfa,neifa,rf,area,volume,
     &  xle,xxi,icyclic,xxn,ipnei,ifatie,
     &  coefmpc,nmpc,labmpc,ipompc,nodempc,ifaext,nfaext,nactdoh)
!
!     extrapolation of pressure element values to the faces
!
      implicit none
!
      character*20 labmpc(*)
!
      integer nface,ielfa(4,*),ifabou(*),i,iel1,iel2,nef,ibou,
     &  neifa(*),icyclic,ifa,indexf,l,m,ipnei(*),ifatie(*),
     &  is,ie,nmpc,ipompc(*),nodempc(3,*),ifaext(*),nfaext,nactdoh(*)
!
      real*8 xrlfa(3,*),vel(nef,0:7),vfa(0:7,*),xbounact(*),xl1,xl2,
     &   vfap(0:7,nface),gradpel(3,*),gradpfa(3,*),rf(3),area(*),
     &   volume(*),xle(*),xxi(3,*),c(3,3),gradnor,xxn(3,*),coefmpc(*)
!
      intent(in) nface,ielfa,xrlfa,vel,
     &  ifabou,xbounact,nef,neifa,rf,area,volume,
     &  xle,xxi,icyclic,xxn,ipnei,ifatie,
     &  coefmpc,nmpc,labmpc,ipompc,nodempc,ifaext,nfaext,nactdoh
!
      intent(inout) vfa,gradpel,gradpfa
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
            vfap(4,i)=xl1*vel(iel1,4)+xrlfa(2,i)*vel(iel2,4)
         elseif(ielfa(3,i).ne.0) then
!
!           boundary face; more than one layer
!            
            ibou=0
            if(iel2.lt.0) then
               if(ifabou(-iel2+4).gt.0) then
                  ibou=ifabou(-iel2+4)
               endif
            endif
!
            if(ibou.gt.0) then
!
!              pressure boundary condition
!
               vfap(4,i)=xbounact(ibou)
            else
!
!              extrapolation
!
               vfap(4,i)=xl1*vel(iel1,4)
     &                 +xrlfa(3,i)*vel(abs(ielfa(3,i)),4)
           endif
         else
!
!           boundary face; one layer
!
            vfap(4,i)=vel(iel1,4)
         endif
      enddo
!c$omp end do
!c$omp end parallel
!
!     Multiple point constraints
!
      if(nmpc.gt.0) then
         is=4
         ie=4
         call applympc(nface,ielfa,is,ie,ifabou,ipompc,vfap,coefmpc,
     &        nodempc,ipnei,neifa,labmpc,xbounact,nactdoh,
     &        ifaext,nfaext)
      endif
!
!     calculate the gradient of the pressure at the center of
!     the elements
!
!c$omp parallel default(none)
!c$omp& shared(nef,ipnei,neifa,gradpel,vfap,area,xxn,volume)
!c$omp& private(i,indexf,ifa)
!c$omp do
      do i=1,nef
!
!        initialization
!     
         do l=1,3
            gradpel(l,i)=0.d0
         enddo
!
         do indexf=ipnei(i)+1,ipnei(i+1)
            ifa=neifa(indexf)
            do l=1,3
               gradpel(l,i)=gradpel(l,i)+
     &              vfap(4,ifa)*area(ifa)*xxn(l,indexf)
            enddo
         enddo
!     
!        dividing by the volume of the element
!     
         do l=1,3
            gradpel(l,i)=gradpel(l,i)/volume(i)
         enddo
      enddo
!c$omp end do
!c$omp end parallel
! 
!     interpolate/extrapolate the pressure gradient from the
!     center of the elements to the center of the faces
!           
!c$omp parallel default(none)
!c$omp& shared(nface,ielfa,xrlfa,gradpfa,gradpel,icyclic,c,ifatie,
!c$omp&        ipnei,xxn)
!c$omp& private(i,iel1,xl1,iel2,l,xl2,indexf,gradnor)
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
               do l=1,3
                  gradpfa(l,i)=xl1*gradpel(l,iel1)+
     &                 xl2*gradpel(l,iel2)
               enddo
            elseif(ifatie(i).gt.0) then
               do l=1,3
                  gradpfa(l,i)=xl1*gradpel(l,iel1)+xl2*
     &                  (gradpel(1,iel2)*c(l,1)+
     &                   gradpel(2,iel2)*c(l,2)+
     &                   gradpel(3,iel2)*c(l,3))
               enddo
            else
               do l=1,3
                  gradpfa(l,i)=xl1*gradpel(l,iel1)+xl2*
     &                  (gradpel(1,iel2)*c(1,l)+
     &                   gradpel(2,iel2)*c(2,l)+
     &                   gradpel(3,iel2)*c(3,l))
               enddo
            endif
         elseif(ielfa(3,i).ne.0) then
!     
!           boundary face; more than one layer; extrapolation
!     
            do l=1,3
               gradpfa(l,i)=xl1*gradpel(l,iel1)+
     &              xrlfa(3,i)*gradpel(l,abs(ielfa(3,i)))
            enddo
         else
!     
!           boundary face; one layer
!   
            indexf=ipnei(iel1)+ielfa(4,i)
            gradnor=gradpel(1,iel1)*xxi(1,indexf)+
     &              gradpel(2,iel1)*xxi(2,indexf)+
     &              gradpel(3,iel1)*xxi(3,indexf)
            do l=1,3
                  gradpfa(l,i)=gradpel(l,iel1)
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
!c$omp& shared(nface,ielfa,xrlfa,vfa,vfap,gradpfa,rf,ifabou,ipnei,
!c$omp&        xxi,xle,vel)
!c$omp& private(i,iel1,iel2,xl1,ibou,indexf)
!c$omp do
         do i=1,nface
            iel1=ielfa(1,i)
            iel2=ielfa(2,i)
            xl1=xrlfa(1,i)
            if(iel2.gt.0) then
!
!              face between two elements
!
               vfa(4,i)=vfap(4,i)+gradpfa(1,i)*rf(1)
     &                         +gradpfa(2,i)*rf(2)
     &                         +gradpfa(3,i)*rf(3)
            elseif(ielfa(3,i).ne.0) then
!
!              boundary face; more than one layer
!
               ibou=0
               if(iel2.lt.0) then
                  if(ifabou(-iel2+4).gt.0) then
                     ibou=ifabou(-iel2+4)
                  endif
               endif
!
               if(ibou.gt.0) then
!
!                 pressure given
!
                  vfa(4,i)=vfap(4,i)
               else
!
!                 extrapolation
!
                  vfa(4,i)=vfap(4,i)+gradpfa(1,i)*rf(1)
     &                            +gradpfa(2,i)*rf(2)
     &                            +gradpfa(3,i)*rf(3)
               endif
            else
!
!              boundary face; one layer
!
               indexf=ipnei(iel1)+ielfa(4,i)
               vfa(4,i)=vel(iel1,4)
     &                 +(gradpfa(1,i)*xxi(1,indexf)+
     &                   gradpfa(2,i)*xxi(2,indexf)+
     &                   gradpfa(3,i)*xxi(3,indexf))*xle(indexf)
            endif
         enddo
!c$omp end do
!c$omp end parallel
!
!     Multiple point constraints
!
      if(nmpc.gt.0) then
         is=4
         ie=4
         call applympc(nface,ielfa,is,ie,ifabou,ipompc,vfa,coefmpc,
     &        nodempc,ipnei,neifa,labmpc,xbounact,nactdoh,
     &        ifaext,nfaext)
      endif
!
!        calculate the gradient of the pressure at the center of
!        the elements
!
!c$omp parallel default(none)
!c$omp& shared(nef,ipnei,neifa,gradpel,vfa,area,xxn,volume)
!c$omp& private(i,indexf,ifa)
!c$omp do
         do i=1,nef
!
!           initialization
!     
            do l=1,3
               gradpel(l,i)=0.d0
            enddo
!
            do indexf=ipnei(i)+1,ipnei(i+1)
               ifa=neifa(indexf)
               do l=1,3
                  gradpel(l,i)=gradpel(l,i)+
     &                 vfa(4,ifa)*area(ifa)*xxn(l,indexf)
               enddo
            enddo
!     
!           dividing by the volume of the element
!     
            do l=1,3
               gradpel(l,i)=gradpel(l,i)/volume(i)
            enddo
         enddo
!c$omp end do
!c$omp end parallel
! 
!        interpolate/extrapolate the pressure gradient from the
!        center of the elements to the center of the faces
!           
!c$omp parallel default(none)
!c$omp& shared(nface,ielfa,xrlfa,gradpfa,gradpel,icyclic,c,ifatie,
!c$omp&        ipnei,xxn)
!c$omp& private(i,iel1,xl1,iel2,l,xl2,indexf,gradnor)
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
                  do l=1,3
                     gradpfa(l,i)=xl1*gradpel(l,iel1)+
     &                    xl2*gradpel(l,iel2)
                  enddo
               elseif(ifatie(i).gt.0) then
                  do l=1,3
                     gradpfa(l,i)=xl1*gradpel(l,iel1)+xl2*
     &                    (gradpel(1,iel2)*c(l,1)+
     &                     gradpel(2,iel2)*c(l,2)+
     &                     gradpel(3,iel2)*c(l,3))
                  enddo
               else
                  do l=1,3
                     gradpfa(l,i)=xl1*gradpel(l,iel1)+xl2*
     &                    (gradpel(1,iel2)*c(1,l)+
     &                     gradpel(2,iel2)*c(2,l)+
     &                     gradpel(3,iel2)*c(3,l))
                  enddo
               endif
            elseif(ielfa(3,i).ne.0) then
!     
!           boundary face; linear extrapolation
!     
               do l=1,3
                  gradpfa(l,i)=xl1*gradpel(l,iel1)+
     &                 xrlfa(3,i)*gradpel(l,abs(ielfa(3,i)))
               enddo
            else
!     
!     boundary face; constant extrapolation (one element layer)
!   
               indexf=ipnei(iel1)+ielfa(4,i)
               gradnor=gradpel(1,iel1)*xxi(1,indexf)+
     &                 gradpel(2,iel1)*xxi(2,indexf)+
     &                 gradpel(3,iel1)*xxi(3,indexf)
               do l=1,3
                  gradpfa(l,i)=gradpel(l,iel1)
     &                 -gradnor*xxi(l,indexf)
               enddo
            endif
         enddo
!c$omp end do
!c$omp end parallel
!
      enddo
!            
      return
      end
