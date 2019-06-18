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
      subroutine filter(dgdxglob,nobject,nk,nodedesi,ndesi,
     &           objectset,xo,yo,zo,x,y,z,nx,ny,nz,neighbor,r,
     &           ndesia,ndesib)               
!
!     Filtering of sensitivities      
!
      implicit none
!
      character*81 objectset(4,*)

      integer nobject,nk,nodedesi(*),nnodesinside,i,
     &        ndesi,j,m,neighbor(ndesi+6),nx(ndesi),
     &        ny(ndesi),nz(ndesi),istat,ndesia,ndesib
!
      real*8 dgdxglob(2,nk,nobject),xo(ndesi),yo(ndesi),zo(ndesi),
     &       x(ndesi),y(ndesi),z(ndesi),filterrad,r(ndesi+6),
     &       filterval(ndesi),nominator,denominator
!
!     Calculate filtered sensitivities
!   
!     Assign filter radius (taken from first defined object function)
!
      read(objectset(2,1)(21:40),'(f20.0)',iostat=istat) filterrad     
!        
      do j=ndesia,ndesib
!     
         call near3d_se(xo,yo,zo,x,y,z,nx,ny,nz,xo(j),yo(j),zo(j),
     &        ndesi,neighbor,r,nnodesinside,filterrad)
!  
!        Calculate function value of the filterfunction (linear) 
!
         if(objectset(2,1)(1:6).eq.'LINEAR') then
          do i=1,nnodesinside
               filterval(i)=filterrad-dsqrt(r(i))
            enddo
!
!        Calculate function value of the filterfunction (gauss) 
!
         elseif(objectset(2,1)(1:5).eq.'GAUSS') then
            do i=1,nnodesinside
               filterval(i)=dexp(-(2*r(i)/filterrad**2))
            enddo 
!
!        Calculate function value of the filterfunction (cubic) 
!
         elseif(objectset(2,1)(1:5).eq.'CUBIC') then
            do i=1,nnodesinside
               filterval(i)=filterrad-(1/filterrad**2)*dsqrt(r(i))**3
            enddo
         endif
!  
!        Calculate filtered sensitivity
 
         do m=1,nobject
            if(objectset(1,m)(1:9).eq.'THICKNESS') cycle             
            nominator=0.d0
            denominator=0.d0
            do i=1,nnodesinside
               nominator=nominator+filterval(i)*
     &             dgdxglob(1,nodedesi(neighbor(i)),m)
               denominator=denominator+filterval(i)
            enddo 
            dgdxglob(2,nodedesi(j),m)=nominator/denominator
         enddo
      enddo
!
      return        
      end




