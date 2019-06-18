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
!     y=A*x for real sparse matrices (symmetric and non-symmetric)
!
!     storage of the matrix in a:
!        - first the lower triangular terms
!        - then, if the matrix is non-symmetric, the upper triangular terms
!        - finally the diagonal terms
!
      subroutine matvec(n,x,y,nelt,ia,ja,a,isym)
      use omp_lib
!
      implicit none
!
      integer ia(*),ja(*),i,j,n,nelt,isym,nflnei
      real*8 y(*),x(*),a(*)
!
!     number of off-diagonal terms
!
      nflnei=nelt-n
!
c$omp parallel default(none)
c$omp& shared(n,x,a,y,ja,ia,nflnei)
c$omp& private(i,j)
c$omp do
      do i=1,n
         y(i)=a(nflnei+i)*x(i)
         do j=ja(i)+1,ja(i+1)
c            if(ia(j).ne.0) then
               y(i)=y(i)+a(j)*x(ia(j))
c            endif
         enddo
      enddo
c$omp end do
c$omp end parallel
!
      return
      end
