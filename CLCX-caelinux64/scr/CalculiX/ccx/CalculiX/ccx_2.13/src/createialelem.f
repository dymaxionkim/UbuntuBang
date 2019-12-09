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
      subroutine createialelem(ne,istartelem,ialelem,ipoeldi,ieldi)
!
      implicit none
!
      integer ne,istartelem(*),ialelem(*),ipoeldi(*),ieldi(2,*),
     &  ifree,i,index
!
!     determining the design variables belonging to a given 
!     element i. They are stored in ialelem(istartelem(i))..
!     ...up to..... ialdesi(istartelem(i+1)-1)
!
      ifree=1
      do i=1,ne
         istartelem(i)=ifree
         index=ipoeldi(i)
         do
            if(index.eq.0) exit
            ialelem(ifree)=ieldi(1,index)
            ifree=ifree+1
            index=ieldi(2,index)
         enddo
      enddo
      istartelem(ne+1)=ifree
!
      return
      end
