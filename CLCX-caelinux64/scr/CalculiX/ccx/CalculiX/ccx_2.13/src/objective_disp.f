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
      subroutine objective_disp(nodeset,istartset,iendset,ialset,
     &  nk,idesvarc,iobject,mi,g0,nobject,vold)
!
!     calculates the sum of the square of the displacements of a node
!     set
!
      implicit none
!
      integer nk,istartset(*),iendset(*),ialset(*),nodeset,idir,
     &  idesvarc,iobject,mi(*),j,k,nobject,idesvar
!
      real*8 g0(nobject),vold(0:mi(2),*)
!
      idesvar=idesvarc+1
!
      g0(iobject)=0.d0
!
!     check for the existence of a set, else take the complete mesh
!
      if(nodeset.eq.0) then
         do j=1,nk
            do idir=1,3
               g0(iobject)=g0(iobject)+vold(idir,j)**2
            enddo
         enddo
      else
         do j=istartset(nodeset),iendset(nodeset)
            if(ialset(j).gt.0) then
               do idir=1,3
                  g0(iobject)=g0(iobject)+vold(idir,ialset(j))**2
               enddo
            else
               k=ialset(j-2)
               do
                  k=k-ialset(j)
                  if(k.ge.ialset(j-1)) exit
                  do idir=1,3
                     g0(iobject)=g0(iobject)+vold(idir,k)**2
                  enddo
               enddo
            endif
         enddo
      endif
!     
      return
      end
      
