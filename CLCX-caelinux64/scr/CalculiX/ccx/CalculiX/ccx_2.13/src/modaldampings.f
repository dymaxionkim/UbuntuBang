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
      subroutine modaldampings(inpc,textpart,nmethod,xmodal,istep,
     &  istat,n,iline,ipol,inl,ipoinp,inp,ipoinpc)
!
!     reading the input deck: *MODAL DAMPING
!
      implicit none
!
      logical rayleigh
!
      character*1 inpc(*)
      character*132 textpart(16)
!
      integer nmethod,istep,istat,n,key,iline,ipol,inl,ipoinp(2,*),
     &  inp(3,*),ipoinpc(0:*),i,lowfrequ,highfrequ,k
!
      real*8 xmodal(*),zeta
!
      if(istep.lt.1) then
         write(*,*) '*ERROR in modaldampings: *MODAL DAMPING can only'
         write(*,*) '  be used within a STEP'
         call exit(201)
      endif
!
      rayleigh=.false.
      do i=2,n
         if(textpart(i)(1:8).eq.'RAYLEIGH') then
            rayleigh=.true.
         else
            write(*,*) 
     &        '*WARNING in modaldampings: parameter not recognized:'
            write(*,*) '         ',
     &                 textpart(i)(1:index(textpart(i),' ')-1)
            call inputwarning(inpc,ipoinpc,iline,
     &"*MODAL DAMPING%")
         endif
      enddo
      if(rayleigh) then
         xmodal(11)=-0.5
         call getnewline(inpc,textpart,istat,n,key,iline,ipol,inl,
     &        ipoinp,inp,ipoinpc)
         if((istat.lt.0).or.(key.eq.1)) then
            write(*,*) '*ERROR in modaldampings: definition 
     &                  not complete'
            write(*,*) '       '
            call inputerror(inpc,ipoinpc,iline,
     &"*MODAL DAMPING%")
            call exit(201)
         endif
         read(textpart(3)(1:20),'(f20.0)',iostat=istat) xmodal(1)
         if(istat.gt.0) call inputerror(inpc,ipoinpc,iline,
     &"*MODAL DAMPING%")
         read(textpart(4)(1:20),'(f20.0)',iostat=istat) xmodal(2)
         if(istat.gt.0) call inputerror(inpc,ipoinpc,iline,
     &"*MODAL DAMPING%")
!
         call getnewline(inpc,textpart,istat,n,key,iline,ipol,inl,
     &        ipoinp,inp,ipoinpc)
!
      else
         do
            call getnewline(inpc,textpart,istat,n,key,iline,ipol,inl,
     &           ipoinp,inp,ipoinpc)
            if((istat.lt.0).or.(key.eq.1)) exit
!
            read(textpart(1)(1:10),'(i10)',iostat=istat) lowfrequ
            if(istat.gt.0) call inputerror(inpc,ipoinpc,iline,
     &"*MODAL DAMPING%")
            read(textpart(2)(1:10),'(i10)',iostat=istat) highfrequ
            if(istat.gt.0) call inputerror(inpc,ipoinpc,iline,
     &"*MODAL DAMPING%")
            read(textpart(3)(1:20),'(f20.0)',iostat=istat) zeta
            if(istat.gt.0) call inputerror(inpc,ipoinpc,iline,
     &"*MODAL DAMPING%")
!
            if(highfrequ<lowfrequ) highfrequ=lowfrequ  
            do k=lowfrequ,highfrequ
               xmodal(11+k)=zeta
            enddo
         enddo  
      endif
      return
      end

