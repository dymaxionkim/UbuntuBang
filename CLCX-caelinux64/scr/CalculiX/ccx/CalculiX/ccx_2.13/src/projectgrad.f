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
      subroutine projectgrad(vector,ndesi,nodedesi,dgdxglob,nactive,
     &   nobject,nnlconst,ipoacti,nk,rhs,iconst,objectset)         
!
!     calculates the projected gradient
!
      implicit none
!
      character*81 objectset(4,*)
!
      integer ndesi,nodedesi(*),irow,icol,nactive,nobject,nnlconst,
     &   ipoacti(*),nk,ipos,node,iconst 
!
      real*8 dgdxglob(2,nk,nobject),vector(ndesi),rhs(*),scalar,dd
!
!     initialization of enlarged field dgdxglob
!
      if(iconst.eq.1) then
         do irow=1,nk
	    dgdxglob(2,irow,nobject)=0.d0
	 enddo
      endif
!
!     calculation of the vector
!
      do irow=1,ndesi
         do icol=1,nactive
            if(icol.le.nnlconst) then
               ipos=ipoacti(icol)
               node=nodedesi(irow)
               vector(irow)=vector(irow)
     &                      +rhs(icol)*dgdxglob(2,node,ipos)
            else
               if(ipoacti(icol).eq.irow) then
                  vector(irow)=vector(irow)+rhs(icol)
               endif
            endif
         enddo
      enddo
!
!     calculation of the scalar value
!
      scalar=0.d0
      if(iconst.le.nnlconst) then
         do irow=1,ndesi
            ipos=ipoacti(iconst)      
            node=nodedesi(irow)
            scalar=scalar+dgdxglob(2,node,1)*dgdxglob(2,node,ipos)
         enddo
      else
         node=nodedesi(ipoacti(iconst))
         scalar=dgdxglob(2,node,1)
      endif            
!
!     multiplication of scalar and vector
!
      do irow=1,ndesi
         node=nodedesi(irow)
         dgdxglob(2,node,nobject)=dgdxglob(2,node,nobject)
     &                            +vector(irow)*scalar
      enddo
!
!     calculation of final projected gradient in last iteration
!
      if(iconst.eq.nactive) then
         dd=0.d0
         do irow=1,ndesi
            node=nodedesi(irow)
            dgdxglob(2,node,nobject)=dgdxglob(2,node,1)
     &                               -dgdxglob(2,node,nobject)
	    dd=max(dd,abs(dgdxglob(2,node,nobject)))
         enddo
         do irow=1,ndesi
            node=nodedesi(irow)
            dgdxglob(2,node,nobject)=dgdxglob(2,node,nobject)/dd
         enddo
         objectset(1,nobject)(1:11)='PROJECTGRAD'
      endif
!
      return        
      end




