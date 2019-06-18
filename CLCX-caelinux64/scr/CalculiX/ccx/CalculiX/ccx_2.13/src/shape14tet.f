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
      subroutine shape14tet(xi,et,ze,xl,xsj,shp,iflag,konl)
!
!     shape functions and derivatives for a 10-node quadratic
!     isoparametric tetrahedral element. 0<=xi,et,ze<=1,xi+et+ze<=1.
!
!     iflag=1: calculate only the value of the shape functions
!     iflag=2: calculate the value of the shape functions and
!              the Jacobian determinant
!     iflag=3: calculate the value of the shape functions, the
!              value of their derivatives w.r.t. the global
!              coordinates and the Jacobian determinant
!
      implicit none
!
      integer i,j,k,iflag,ifacet(7,4),konl(14)
!
      real*8 shp(4,14),xs(3,3),xsi(3,3),xl(3,14),sh(3),xi,et,ze,xsj,a
!
      intent(in) xi,et,ze,xl,iflag,konl
!
      intent(out) shp,xsj
!
!     nodes per face for tet elements
!
      ifacet=reshape((/1,3,2,7,6,5,11,
     &             1,2,4,5,9,8,12,
     &             2,3,4,6,10,9,13,
     &             1,4,3,8,10,7,14/),(/7,4/))
!
!     shape functions and their glocal derivatives
!
!     shape functions
!
      a=1.d0-xi-et-ze
      shp(4, 1)=(2.d0*a-1.d0)*a
      shp(4, 2)=xi*(2.d0*xi-1.d0)
      shp(4, 3)=et*(2.d0*et-1.d0)
      shp(4, 4)=ze*(2.d0*ze-1.d0)
      shp(4, 5)=4.d0*xi*a
      shp(4, 6)=4.d0*xi*et
      shp(4, 7)=4.d0*et*a
      shp(4, 8)=4.d0*ze*a
      shp(4, 9)=4.d0*xi*ze
      shp(4,10)=4.d0*et*ze
!
!     correction for the extra nodes in the middle of the faces
! 
      do i=1,4
         if(konl(10+i).eq.konl(10)) then
!
!           no extra node in this face
!
            shp(4,10+i)=0.d0
         else
            if(i.eq.1) then
               shp(4,11)=27.d0*a*xi*et
            elseif(i.eq.2) then
               shp(4,12)=27.d0*a*xi*ze
            elseif(i.eq.3) then
               shp(4,13)=27.d0*xi*et*ze
            elseif(i.eq.4) then
               shp(4,14)=27.d0*a*et*ze
            endif
            do j=1,3
               shp(4,ifacet(j,i))=shp(4,ifacet(j,i))+shp(4,20+i)/9.d0
            enddo
            do j=4,6
               shp(4,ifacet(j,i))=shp(4,ifacet(j,i))-
     &                            4.d0*shp(4,20+i)/9.d0
            enddo
         endif
      enddo
!
      if(iflag.eq.1) return
!
!     local derivatives of the shape functions: xi-derivative
!
      shp(1, 1)=1.d0-4.d0*a
      shp(1, 2)=4.d0*xi-1.d0
      shp(1, 3)=0.d0
      shp(1, 4)=0.d0
      shp(1, 5)=4.d0*(a-xi)
      shp(1, 6)=4.d0*et
      shp(1, 7)=-4.d0*et
      shp(1, 8)=-4.d0*ze
      shp(1, 9)=4.d0*ze
      shp(1,10)=0.d0
!
!     correction for the extra nodes in the middle of the faces
! 
      do i=1,4
         if(konl(10+i).eq.konl(10)) then
!
!           no extra node in this face
!
            shp(1,10+i)=0.d0
         else
            if(i.eq.1) then
               shp(1,11)=27.d0*(a-xi)*et
            elseif(i.eq.2) then
               shp(1,12)=27.d0*(a-xi)*ze
            elseif(i.eq.3) then
               shp(1,13)=27.d0*et*ze
            elseif(i.eq.4) then
               shp(1,14)=-27.d0*et*ze
            endif
            do j=1,3
               shp(1,ifacet(j,i))=shp(1,ifacet(j,i))+shp(1,20+i)/9.d0
            enddo
            do j=4,6
               shp(1,ifacet(j,i))=shp(1,ifacet(j,i))-
     &                            4.d0*shp(1,20+i)/9.d0
            enddo
         endif
      enddo
!
!     local derivatives of the shape functions: eta-derivative
!
      shp(2, 1)=1.d0-4.d0*a
      shp(2, 2)=0.d0
      shp(2, 3)=4.d0*et-1.d0
      shp(2, 4)=0.d0
      shp(2, 5)=-4.d0*xi
      shp(2, 6)=4.d0*xi
      shp(2, 7)=4.d0*(a-et)
      shp(2, 8)=-4.d0*ze
      shp(2, 9)=0.d0
      shp(2,10)=4.d0*ze
!
!     correction for the extra nodes in the middle of the faces
! 
      do i=1,4
         if(konl(10+i).eq.konl(10)) then
!
!           no extra node in this face
!
            shp(2,10+i)=0.d0
         else
            if(i.eq.1) then
               shp(2,11)=27.d0*(a-et)*xi
            elseif(i.eq.2) then
               shp(2,12)=-27.d0*xi*ze
            elseif(i.eq.3) then
               shp(2,13)=27.d0*xi*ze
            elseif(i.eq.4) then
               shp(2,14)=27.d0*(a-et)*ze
            endif
            do j=1,3
               shp(2,ifacet(j,i))=shp(2,ifacet(j,i))+shp(2,20+i)/9.d0
            enddo
            do j=4,6
               shp(2,ifacet(j,i))=shp(2,ifacet(j,i))-
     &                            4.d0*shp(2,20+i)/9.d0
            enddo
         endif
      enddo
!
!     local derivatives of the shape functions: zeta-derivative
!
      shp(3, 1)=1.d0-4.d0*a
      shp(3, 2)=0.d0
      shp(3, 3)=0.d0
      shp(3, 4)=4.d0*ze-1.d0
      shp(3, 5)=-4.d0*xi
      shp(3, 6)=0.d0
      shp(3, 7)=-4.d0*et
      shp(3, 8)=4.d0*(a-ze)
      shp(3, 9)=4.d0*xi
      shp(3,10)=4.d0*et
!
!     correction for the extra nodes in the middle of the faces
! 
      do i=1,4
         if(konl(10+i).eq.konl(10)) then
!
!           no extra node in this face
!
            shp(3,10+i)=0.d0
         else
            if(i.eq.1) then
               shp(3,11)=-27.d0*xi*et
            elseif(i.eq.2) then
               shp(3,12)=27.d0*(a-ze)*xi
            elseif(i.eq.3) then
               shp(3,13)=27.d0*xi*et
            elseif(i.eq.4) then
               shp(3,14)=27.d0*(a-ze)*et
            endif
            do j=1,3
               shp(3,ifacet(j,i))=shp(3,ifacet(j,i))+shp(3,20+i)/9.d0
            enddo
            do j=4,6
               shp(3,ifacet(j,i))=shp(3,ifacet(j,i))-
     &                            4.d0*shp(3,20+i)/9.d0
            enddo
         endif
      enddo
!
!     computation of the local derivative of the global coordinates
!     (xs)
!
      do i=1,3
        do j=1,3
          xs(i,j)=0.d0
          do k=1,10
            xs(i,j)=xs(i,j)+xl(i,k)*shp(j,k)
          enddo
        enddo
      enddo
!
!     computation of the jacobian determinant
!
      xsj=xs(1,1)*(xs(2,2)*xs(3,3)-xs(2,3)*xs(3,2))
     &   -xs(1,2)*(xs(2,1)*xs(3,3)-xs(2,3)*xs(3,1))
     &   +xs(1,3)*(xs(2,1)*xs(3,2)-xs(2,2)*xs(3,1))
!
      if(iflag.eq.2) return
!
!     computation of the global derivative of the local coordinates
!     (xsi) (inversion of xs)
!
      xsi(1,1)=(xs(2,2)*xs(3,3)-xs(3,2)*xs(2,3))/xsj
      xsi(1,2)=(xs(1,3)*xs(3,2)-xs(1,2)*xs(3,3))/xsj
      xsi(1,3)=(xs(1,2)*xs(2,3)-xs(2,2)*xs(1,3))/xsj
      xsi(2,1)=(xs(2,3)*xs(3,1)-xs(2,1)*xs(3,3))/xsj
      xsi(2,2)=(xs(1,1)*xs(3,3)-xs(3,1)*xs(1,3))/xsj
      xsi(2,3)=(xs(1,3)*xs(2,1)-xs(1,1)*xs(2,3))/xsj
      xsi(3,1)=(xs(2,1)*xs(3,2)-xs(3,1)*xs(2,2))/xsj
      xsi(3,2)=(xs(1,2)*xs(3,1)-xs(1,1)*xs(3,2))/xsj
      xsi(3,3)=(xs(1,1)*xs(2,2)-xs(2,1)*xs(1,2))/xsj
!
!     computation of the global derivatives of the shape functions
!
      do k=1,10
        do j=1,3
          sh(j)=shp(1,k)*xsi(1,j)+shp(2,k)*xsi(2,j)+shp(3,k)*xsi(3,j)
        enddo
        do j=1,3
          shp(j,k)=sh(j)
        enddo
      enddo
!
      return
      end
