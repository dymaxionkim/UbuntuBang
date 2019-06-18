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
      subroutine dampings(inpc,textpart,xmodal,istep,
     &  istat,n,iline,ipol,inl,ipoinp,inp,ipoinpc,irstrt)
!
!     reading the input deck: *DAMPING
!
      implicit none
!
      character*1 inpc(*)
      character*132 textpart(16)
!
      integer istep,istat,n,key,iline,ipol,inl,ipoinp(2,*),
     &  inp(3,*),ipoinpc(0:*),i,irstrt
!
      real*8 xmodal(*)
!
      if((istep.gt.0).and.(irstrt.ge.0)) then
         write(*,*) '*ERROR reading *DAMPING: *DAMPING should be placed'
         write(*,*) '       before all step definitions'
         call exit(201)
      endif
!
      do i=2,n
         if(textpart(i)(1:6).eq.'ALPHA=') then
            read(textpart(i)(7:26),'(f20.0)',iostat=istat) xmodal(1)
            if(istat.gt.0) call inputerror(inpc,ipoinpc,iline,
     &           "*DAMPING%")
         elseif(textpart(i)(1:5).eq.'BETA=') then
            read(textpart(i)(6:25),'(f20.0)',iostat=istat) xmodal(2)
            if(istat.gt.0) call inputerror(inpc,ipoinpc,iline,
     &           "*DAMPING%")
         else
            write(*,*) 
     &        '*WARNING reading *DAMPING: parameter not recognized:'
            write(*,*) '         ',
     &                 textpart(i)(1:index(textpart(i),' ')-1)
            call inputwarning(inpc,ipoinpc,iline,
     &"*DAMPING%")
         endif
      enddo
!
      call getnewline(inpc,textpart,istat,n,key,iline,ipol,inl,
     &     ipoinp,inp,ipoinpc)
!
      return
      end

