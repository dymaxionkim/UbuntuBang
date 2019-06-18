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
      subroutine changefrictions(inpc,textpart,matname,nmat,nmat_,
     &  irstrt,istep,istat,n,iline,ipol,inl,ipoinp,inp,ipoinpc,
     &  imat)
!
!     reading the input deck: *CHANGE FRICTION
!
      implicit none
!
      character*1 inpc(*)
      character*80 matname(*),interactionname
      character*132 textpart(16)
!
      integer nmat,nmat_,istep,istat,n,key,i,irstrt,iline,ipol,inl,
     &  ipoinp(2,*),inp(3,*),ipoinpc(0:*),imat
!
      if(istep.eq.0) then
         write(*,*) '*ERROR reading *CHANGE FRICTION: *CHANGE FRICTION'
         write(*,*) '       cannot be used before the first step'
         call exit(201)
      endif
!
      do i=2,n
         if(textpart(i)(1:12).eq.'INTERACTION=') then
            interactionname=textpart(i)(13:92)
         else
            write(*,*) 
     &    '*WARNING reading *CHANGE FRICTION: parameter not recognized:'
            write(*,*) '         ',
     &                 textpart(i)(1:index(textpart(i),' ')-1)
            call inputwarning(inpc,ipoinpc,iline,
     &"*CHANGE FRICTION%")
         endif
      enddo
!
!     check whether the interaction exists
!
      imat=0
      do i=1,nmat
         if(matname(i).eq.interactionname) then
            imat=i
            exit
         endif
      enddo
!
      if(imat.eq.0) then
         write(*,*) '*ERROR reading *CHANGE FRICTION:',interactionname
         write(*,*) '       is a nonexistent interaction'
         call exit(201)
      endif
!
      call getnewline(inpc,textpart,istat,n,key,iline,ipol,inl,
     &     ipoinp,inp,ipoinpc)
!
      return
      end

