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
      subroutine correctvel(hel,adv,vfa,ipnei,area,bv,xxn,neifa,
     &  lakonf,nef,neq)
!
!     correction of the velocity at the element centers due to the
!     pressure change (balance of mass)
!
!     the solution is stored in field bv.
!
      implicit none
!
      character*8 lakonf(*)
!
      integer i,k,nef,jdof1,indexf,ipnei(*),neifa(*),ifa,
     &  neq
!
      real*8 bv(neq,3),hel(3,*),adv(*),xxn(3,*),area(*),vfa(0:7,*)
!
c$omp parallel default(none)
c$omp& shared(nef,bv,ipnei,lakonf,neifa,vfa,area,xxn,hel,adv)
c$omp& private(i,jdof1,k,indexf,ifa)
c$omp do
      do i=1,nef
!
         jdof1=i
         do k=1,3
            bv(jdof1,k)=0.d0
         enddo
c         indexf=ipnei(i)
!
c         do j=1,ipnei(i+1)-ipnei(i)
c            indexf=indexf+1
         do indexf=ipnei(i)+1,ipnei(i+1)
            ifa=neifa(indexf)
            do k=1,3
               bv(jdof1,k)=bv(jdof1,k)
     &              +vfa(4,ifa)*area(ifa)*xxn(k,indexf)
            enddo
         enddo
!
         do k=1,3
            bv(jdof1,k)=(hel(k,jdof1)-bv(jdof1,k))/adv(jdof1)
         enddo
      enddo
c$omp end do
c$omp end parallel
!  
      return
      end
