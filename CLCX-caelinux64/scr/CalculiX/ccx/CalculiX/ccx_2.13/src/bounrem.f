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
      subroutine bounrem(node,is,iboun,nodeboun,ndirboun,xboun,
     &  nboun,iamboun,nam,ikboun,ilboun,typeboun)
!
!     removes boundary conditions in directions is up to and including
!     ie in node "node" in the data base; no transformation is allowed
!     in the node
!
      implicit none
!
      character*1 typeboun(*)
!
      integer nodeboun(*),ndirboun(*),node,is,ie,nboun,i,j,
     &  iamboun(*),nam,ikboun(*),ilboun(*),idof,id,iboun
!
      real*8 xboun(*)
!
c      do i=is,ie
      do i=is,is
         idof=8*(node-1)+i
         call nident(ikboun,idof,nboun,id)
         if(id.gt.0) then
            if(ikboun(id).eq.idof) then
c               iboun=ilboun(id)
               do j=iboun,nboun-1
                  nodeboun(j)=nodeboun(j+1)
                  ndirboun(j)=ndirboun(j+1)
                  xboun(j)=xboun(j+1)
                  typeboun(j)=typeboun(j+1)
                  if(nam.gt.0) iamboun(j)=iamboun(j+1)
               enddo
               do j=id,nboun-1
                  ikboun(j)=ikboun(j+1)
                  ilboun(j)=ilboun(j+1)
               enddo
               do j=1,nboun-1
                  if(ilboun(j).ge.iboun) then
                     ilboun(j)=ilboun(j)-1
                  endif
               enddo
               nboun=nboun-1
            else
               write(*,*) '*ERROR in bounrem: the boundary condition'
               write(*,*) '       cannot be removed since it has'
               write(*,*) '       not been defined'
               call exit(201)
            endif
         else
            write(*,*) '*ERROR in bounrem: the boundary condition'
            write(*,*) '       cannot be removed since it has'
            write(*,*) '       not been defined'
            call exit(201)
         endif
      enddo
!
      return
      end

