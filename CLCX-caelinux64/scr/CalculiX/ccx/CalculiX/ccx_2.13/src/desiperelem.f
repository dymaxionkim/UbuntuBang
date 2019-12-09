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
      subroutine desiperelem(ndesi,istartdesi,ialdesi,ipoeldi,ieldi,
     &      ne)
!
      implicit none
!
      integer ndesi,istartdesi(*),ialdesi(*),ipoeldi(*),ieldi(2,*),
     &  ieldifree,i,j,nelem,ne
!
!     storing the design variables per element
!
      ieldifree=1
      do i=1,ndesi
         do j=istartdesi(i),istartdesi(i+1)-1
            nelem=ialdesi(j)
            ieldi(1,ieldifree)=i
            ieldi(2,ieldifree)=ipoeldi(nelem)
            ipoeldi(nelem)=ieldifree
            ieldifree=ieldifree+1
         enddo
      enddo
!
!     adding the zero design variable to all elements with
!     a nonzero ipoeldi value
!
      do i=1,ne
         if(ipoeldi(i).eq.0) cycle
         ieldi(1,ieldifree)=0
         ieldi(2,ieldifree)=ipoeldi(i)
         ipoeldi(i)=ieldifree
         ieldifree=ieldifree+1
      enddo
!
      return
      end
