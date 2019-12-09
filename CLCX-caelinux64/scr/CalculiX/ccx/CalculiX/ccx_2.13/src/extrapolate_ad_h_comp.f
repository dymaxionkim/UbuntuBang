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
      subroutine extrapolate_ad_h_comp(nface,ielfa,xrlfa,adv,advfa,
     &                  hel,hfa,icyclic,c,ifatie)
!
!     inter/extrapolation of adv at the center of the elements
!     to the center of the faces
!
!     inter/extrapolation of h at the center of the elements 
!     to the center of the faces; division through advfa to obtain
!     the face velocity
!
      implicit none
!
      integer nface,ielfa(4,*),ipo1,iel2,ipo3,i,j,icyclic,ifatie(*)
!
      real*8 xrlfa(3,*),xl1,xl2,advfa(*),adv(*),hel(3,*),hfa(3,*),c(3,3)
!     
c$omp parallel default(none)
c$omp& shared(nface,ielfa,xrlfa,advfa,adv,hfa,hel,icyclic,c,ifatie)
c$omp& private(i,ipo1,xl1,iel2,j,ipo3,xl2)
c$omp do
      do i=1,nface
         ipo1=ielfa(1,i)
         xl1=xrlfa(1,i)
         iel2=ielfa(2,i)
         if(iel2.gt.0) then
!
!           internal face
!
            xl2=xrlfa(2,i)
            advfa(i)=1.d0/(xl1/adv(ipo1)+xl2/adv(iel2))
            if((icyclic.eq.0).or.(ifatie(i).eq.0)) then
               do j=1,3
                  hfa(j,i)=(xl1*hel(j,ipo1)/adv(ipo1)
     &                 +xl2*hel(j,iel2)/adv(iel2))
               enddo
            elseif(ifatie(i).gt.0) then
               do j=1,3
                  hfa(j,i)=(xl1*hel(j,ipo1)/adv(ipo1)
     &                 +xl2*(c(j,1)*hel(1,iel2)+
     &                       c(j,2)*hel(2,iel2)+
     &                       c(j,3)*hel(3,iel2))/adv(iel2))
               enddo
            else
               do j=1,3
                  hfa(j,i)=(xl1*hel(j,ipo1)/adv(ipo1)
     &                 +xl2*(c(1,j)*hel(1,iel2)+
     &                       c(2,j)*hel(2,iel2)+
     &                       c(3,j)*hel(3,iel2))/adv(iel2))
               enddo
            endif
         elseif(ielfa(3,i).ne.0) then
!
!           external face; linear extrapolation
!
            ipo3=abs(ielfa(3,i))
            advfa(i)=1.d0/(xl1/adv(ipo1)+xrlfa(3,i)/adv(ipo3))
            do j=1,3
               hfa(j,i)=(xl1*hel(j,ipo1)/adv(ipo1)+
     &                   xrlfa(3,i)*hel(j,ipo3)/adv(ipo3))
            enddo
         else
!
!           external face: constant extrapolation (only one adjacent
!           element layer)
!
            advfa(i)=adv(ipo1)
            do j=1,3
               hfa(j,i)=hel(j,ipo1)/adv(ipo1)
            enddo
         endif
      enddo
c$omp end do
c$omp end parallel
!            
      return
      end
