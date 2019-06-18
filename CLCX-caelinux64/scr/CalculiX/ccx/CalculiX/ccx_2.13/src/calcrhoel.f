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
      subroutine calcrhoel(nef,vel,rhcon,nrhcon,ielmat,ntmat_,
     &  ithermal,mi)
!
!     calculation of rho in the element centers (incompressible
!     fluids)
!
      implicit none
!
      integer nef,i,nrhcon(*),imat,ithermal,ntmat_,mi(*),
     &  ielmat(mi(3),*)
!
      real*8 t1l,vel(nef,0:7),rho,rhcon(0:1,ntmat_,*)
!     
      do i=1,nef
         t1l=vel(i,0)
         imat=ielmat(1,i)
         call materialdata_rho(rhcon,nrhcon,imat,rho,t1l,ntmat_,
     &            ithermal)
         vel(i,5)=rho
c         write(*,*) 'calcrhoel rho',i,rho
      enddo
c      write(*,*)
!            
      return
      end
