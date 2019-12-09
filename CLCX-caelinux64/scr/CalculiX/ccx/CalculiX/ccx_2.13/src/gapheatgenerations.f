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
      subroutine gapheatgenerations(inpc,textpart,elcon,nelcon,
     &  imat,ntmat_,ncmat_,irstrt,istep,istat,n,iline,ipol,inl,ipoinp,
     &  inp,ipoinpc,nstate_)
!
!     reading the input deck: *GAP HEAT GENERATION
!
      implicit none
!
      character*1 inpc(*)
      character*132 textpart(16)
!
      integer nelcon(2,*),imat,ntmat_,istep,istat,ipoinpc(0:*),
     &  n,key,i,ncmat_,irstrt,iline,ipol,inl,ipoinp(2,*),inp(3,*),
     &  nstate_
!
      real*8 elcon(0:ncmat_,ntmat_,*)
!
      if((istep.gt.0).and.(irstrt.ge.0)) then
         write(*,*) '*ERROR reading *GAP HEAT GENERATION:'
         write(*,*) '       *GAP HEAT GENERATION should be placed'
         write(*,*) '       before all step definitions'
         call exit(201)
      endif
!
      if(imat.eq.0) then
         write(*,*) '*ERROR reading *GAP HEAT GENERATION:'
         write(*,*) '       *GAP HEAT GENERATION should be preceded'
         write(*,*) '       by a *SURFACE INTERACTION card'
         call exit(201)
      endif
!
      nstate_=max(nstate_,9)
!
      if(nelcon(1,imat).ne.-51) nelcon(1,imat)=max(nelcon(1,imat),11)
      nelcon(2,imat)=1
!
!     no temperature dependence allowed; last line is decisive
!
      do
         call getnewline(inpc,textpart,istat,n,key,iline,ipol,inl,
     &        ipoinp,inp,ipoinpc)
         if((istat.lt.0).or.(key.eq.1)) return
         do i=1,3
            read(textpart(i)(1:20),'(f20.0)',iostat=istat)
     &           elcon(8+i,1,imat)
            if(istat.gt.0) call inputerror(inpc,ipoinpc,iline,
     &"*GAP HEAT GENERATION%")
         enddo
         if(elcon(9,1,imat).le.0.d0) then
            write(*,*) 
     &       '*ERROR reading *GAP HEAT GENERATION: fraction of'
            write(*,*) '       dissipated energy converted into heat'
            write(*,*) '       must be strictly positive'
            call inputerror(inpc,ipoinpc,iline,
     &"*GAP HEAT GENERATION%")
            call exit(201)
         endif
         if((elcon(10,1,imat).lt.0.d0).or.
     &      (elcon(10,1,imat).gt.1.d0)) then
            write(*,*) 
     &         '*ERROR reading *GAP HEAT GENERATION: weighting factor'
            write(*,*) '       for the distribution of heat between'
            write(*,*) '       the slave and master surface must '
            write(*,*) '       be contained in [0,1]'
            call inputerror(inpc,ipoinpc,iline,
     &"*GAP HEAT GENERATION%")
            call exit(201)
         endif
         elcon(0,1,imat)=0.d0
      enddo
!     
      return
      end

