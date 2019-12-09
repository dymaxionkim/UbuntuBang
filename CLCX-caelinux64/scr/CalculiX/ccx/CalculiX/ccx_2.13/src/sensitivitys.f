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
      subroutine sensitivitys(inpc,textpart,nmethod,
     &  istep,istat,n,iline,ipol,inl,ipoinp,
     &  inp,tieset,ipoinpc,ntie,tinc,tper,tmin,tmax,tincf)
!
!     reading the input deck: *SENSITIVITY
!
      implicit none
!
      character*1 inpc(*)
      character*81 tieset(3,*)
      character*132 textpart(16)
!
      integer nmethod,istep,istat,n,key,i,
     &  iline,ipol,inl,ipoinp(2,*),inp(3,*),
     &  ipoinpc(0:*),ntie
!
      real*8 tinc,tper,tmin,tmax,tincf
!
      if(istep.lt.1) then
         write(*,*) '*ERROR reading *SENSITIVITY: *SENSITIVITY can
     &only be used within a STEP'     
         call exit(201)
      endif
!
      if(istep.lt.2) then
         write(*,*) '*ERROR reading *SENSITIVITY: *SENSITIVITY'
         write(*,*) '       requires a previous *STATIC, *GREEN or'
         write(*,*) '       *FREQUENCY step'
         call exit(201)
      endif
!
      tinc=0.d0
      tper=0.d0
      tmin=0.d0
      tmax=0.d0
      tincf=0.d0
!
      do i=2,n
            write(*,*) 
     &        '*WARNING reading *SENSITIVITY: parameter not 
     &recognized:'
            write(*,*) '         ',
     &                 textpart(i)(1:index(textpart(i),' ')-1)
            call inputwarning(inpc,ipoinpc,iline,
     &"*SENSITIVITY%")
      enddo
!
      nmethod=12
!
!     check whether design variables were defined
!
      do i=1,ntie
         if(tieset(1,i)(81:81).eq.'D') exit
      enddo
      if(i.gt.ntie) then
         write(*,*) '*ERROR reading *SENSITIVITY'
         write(*,*) '       no design variables were defined'
         call exit(201)
      endif
!
      call getnewline(inpc,textpart,istat,n,key,iline,ipol,inl,
     &        ipoinp,inp,ipoinpc)
!
!
      return
      end

