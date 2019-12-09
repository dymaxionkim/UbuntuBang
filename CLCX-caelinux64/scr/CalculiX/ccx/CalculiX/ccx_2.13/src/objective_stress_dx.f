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
      subroutine objective_stress_dx(nodeset,istartset,iendset,ialset,
     &  nk,idesvarc,iobject,dgdx,ndesi,nobject,stn,dstn,objectset,g0)
!
!     calculates derivative of the sum of the square of the 
!     von Mises stress of a node set w.r.t. the coordinates of the mesh
!
      implicit none
!
      character*81 objectset(4,*)
!
      integer nk,istartset(*),iendset(*),ialset(*),nodeset,
     &  idesvarc,iobject,j,k,ndesi,nobject,idesvar
!
      real*8 dgdx(ndesi,nobject),stn(6,*),dstn(6,*),g0(nobject),
     &  rho,xstress,svm,dsvmdx,p
!
      idesvar=idesvarc+1
!
!     reading rho and the mean stress for the Kreisselmeier-Steinhauser
!     function
!
      read(objectset(2,iobject)(41:60),'(f20.0)') rho
      read(objectset(2,iobject)(61:80),'(f20.0)') xstress
!
!     check for the existence of a set, else take the complete mesh
!
      if(nodeset.eq.0) then
         do j=1,nk
            p=-(stn(1,j)+stn(2,j)+stn(3,j))/3.d0
            svm=dsqrt(1.5d0*
     &        ((stn(1,j)+p)**2+(stn(2,j)+p)**2+(stn(3,j)+p)**2+
     &         2.d0*(stn(4,j)**2+stn(5,j)**2+stn(6,j)**2)))
            dsvmdx=
     &           (2.d0*stn(1,j)-stn(2,j)-stn(3,j))*dstn(1,j)+
     &           (2.d0*stn(2,j)-stn(1,j)-stn(3,j))*dstn(2,j)+
     &           (2.d0*stn(3,j)-stn(1,j)-stn(2,j))*dstn(3,j)+
     &           6.d0*(stn(4,j)*dstn(4,j)+stn(5,j)*dstn(5,j)+
     &                 stn(6,j))
            dgdx(idesvar,iobject)=dgdx(idesvar,iobject)+
     &           dexp(rho*svm/xstress)*dsvmdx
         enddo
         dgdx(idesvar,iobject)=dgdx(idesvar,iobject)*
     &         dexp(-rho*g0(iobject))/xstress
      else
         do j=istartset(nodeset),iendset(nodeset)
            if(ialset(j).gt.0) then
               k=ialset(j)
               p=-(stn(1,k)+stn(2,k)+stn(3,k))/3.d0
               svm=dsqrt(1.5d0*
     &              ((stn(1,k)+p)**2+(stn(2,k)+p)**2+(stn(3,k)+p)**2+
     &              2.d0*(stn(4,k)**2+stn(5,k)**2+stn(6,k)**2)))
               dsvmdx=
     &              (2.d0*stn(1,k)-stn(2,k)-stn(3,k))*dstn(1,k)+
     &              (2.d0*stn(2,k)-stn(1,k)-stn(3,k))*dstn(2,k)+
     &              (2.d0*stn(3,k)-stn(1,k)-stn(2,k))*dstn(3,k)+
     &              6.d0*(stn(4,k)*dstn(4,k)+stn(5,k)*dstn(5,k)+
     &              stn(6,k))
               dgdx(idesvar,iobject)=dgdx(idesvar,iobject)+
     &              dexp(rho*svm/xstress)*dsvmdx
            else
               k=ialset(j-2)
               do
                  k=k-ialset(j)
                  if(k.ge.ialset(j-1)) exit
                  p=-(stn(1,k)+stn(2,k)+stn(3,k))/3.d0
                  svm=dsqrt(1.5d0*
     &                 ((stn(1,k)+p)**2+(stn(2,k)+p)**2+(stn(3,k)+p)**2+
     &                 2.d0*(stn(4,k)**2+stn(5,k)**2+stn(6,k)**2)))
                  dsvmdx=
     &                 (2.d0*stn(1,k)-stn(2,k)-stn(3,k))*dstn(1,k)+
     &                 (2.d0*stn(2,k)-stn(1,k)-stn(3,k))*dstn(2,k)+
     &                 (2.d0*stn(3,k)-stn(1,k)-stn(2,k))*dstn(3,k)+
     &                 6.d0*(stn(4,k)*dstn(4,k)+stn(5,k)*dstn(5,k)+
     &                 stn(6,k))
                  dgdx(idesvar,iobject)=dgdx(idesvar,iobject)+
     &                 dexp(rho*svm/xstress)*dsvmdx
               enddo
            endif
         enddo
         dgdx(idesvar,iobject)=dgdx(idesvar,iobject)*
     &         dexp(-rho*g0(iobject))/xstress
      endif
!     
      return
      end
      
