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
      subroutine surfacebehaviors(inpc,textpart,elcon,nelcon,
     &  imat,ntmat_,ncmat_,irstrt,istep,istat,n,iline,ipol,inl,ipoinp,
     &  inp,ipoinpc,npmat_,plicon,nplicon,nstate_,
     &  ichangesurfacebehavior)
!
!     reading the input deck: *SURFACE BEHAVIOR
!
!     nint(elcon(3,1,imat))=1: exponential
!     nint(elcon(3,1,imat))=2: linear
!     nint(elcon(3,1,imat))=3: tabular
!     nint(elcon(3,1,imat))=4: tied
! 
      implicit none
!
      character*1 inpc(*),pressureoverclosure
      character*132 textpart(16)
!
      integer nelcon(2,*),imat,ntmat_,istep,istat,ipoinpc(0:*),
     &  n,key,i,ncmat_,irstrt,iline,ipol,inl,ipoinp(2,*),inp(3,*),
     &  ntmat,npmat,npmat_,nplicon(0:ntmat_,*),nstate_,
     &  ichangesurfacebehavior
!
      real*8 elcon(0:ncmat_,ntmat_,*),plicon(0:2*npmat_,ntmat_,*),
     &  temperature
!
      if((istep.gt.0).and.(irstrt.ge.0).and.
     &   (ichangesurfacebehavior.eq.0)) then
         write(*,*) '*ERROR reading *SURFACE BEHAVIOR:'
         write(*,*) '       *SURFACE BEHAVIOR should be placed'
         write(*,*) '       before all step definitions'
         call exit(201)
      endif
!
      if(imat.eq.0) then
         write(*,*) '*ERROR reading *SURFACE BEHAVIOR:'
         write(*,*) '       *SURFACE BEHAVIOR should be preceded'
         write(*,*) '       by a *SURFACE INTERACTION card'
         call exit(201)
      endif
      pressureoverclosure=' '
!
      do i=2,n
         if(textpart(i)(1:27).eq.'PRESSURE-OVERCLOSURE=LINEAR') then
            pressureoverclosure='L'
         elseif(textpart(i)(1:32).eq.'PRESSURE-OVERCLOSURE=EXPONENTIAL') 
     &      then
            pressureoverclosure='E'
         elseif(textpart(i)(1:38).eq.'PRESSURE-OVERCLOSURE=TABULAR') 
     &      then
            pressureoverclosure='T'
         elseif(textpart(i)(1:35).eq.'PRESSURE-OVERCLOSURE=TIED') 
     &      then
            pressureoverclosure='D'
         else
            write(*,*) 
     &   '*WARNING reading *SURFACE BEHAVIOR: parameter not recognized:'
            write(*,*) '         ',
     &                 textpart(i)(1:index(textpart(i),' ')-1)
            call inputwarning(inpc,ipoinpc,iline,
     &"*SURFACE BEHAVIOR%")
         endif
      enddo
      if(pressureoverclosure.eq.' ') then
         write(*,*) '*ERROR reading *SURFACE BEHAVIOR:'
         write(*,*) '       no PRESSURE-OVERCLOSURE defined on the'
         write(*,*) '       *SURFACE BEHAVIOR card'
         call exit(201)
      endif
!
      if(nelcon(1,imat).ne.-51) nelcon(1,imat)=max(nelcon(1,imat),2)
      nelcon(2,imat)=1
!
      if(pressureoverclosure.eq.'E') then
!     
         call getnewline(inpc,textpart,istat,n,key,iline,ipol,inl,
     &        ipoinp,inp,ipoinpc)
         if((istat.lt.0).or.(key.eq.1)) then
            write(*,*) '*ERROR reading *SURFACE BEHAVIOR: data'
            write(*,*) '       line is lacking for exponential'
            write(*,*) '       behavior'
            call inputerror(inpc,ipoinpc,iline,
     &"*SURFACE BEHAVIOR%")
         endif
!     
         elcon(3,1,imat)=1.5d0
!     
!     exponential overclosure
!     
         do i=1,2
            read(textpart(i)(1:20),'(f20.0)',iostat=istat)
     &           elcon(i,1,imat)
            if(istat.gt.0) call inputerror(inpc,ipoinpc,iline,
     &"*SURFACE BEHAVIOR%")
         enddo
!     
!     checking the values
!     
         if(elcon(1,1,imat).le.0.d0) then
            write(*,*) '*ERROR reading *SURFACE BEHAVIOR: c_0 must'
            write(*,*) '       exceed zero'
            call exit(201)
         endif
         if(elcon(2,1,imat).lt.0.d0) then
            write(*,*) '*ERROR reading *SURFACE BEHAVIOR: p_0 must'
            write(*,*) '       not be smaller than zero'
            call exit(201)
         endif
!     
!     transforming the parameters c_0 into
!     beta such that the pressure p satisfies:
!     p=p_0*dexp(-beta*distance)
!     where n is the normal to the master surface, and
!     distance is the distance between slave node and
!     master surface (negative for penetration)
!     
         elcon(1,1,imat)=dlog(100.d0)/elcon(1,1,imat)
!     
         call getnewline(inpc,textpart,istat,n,key,iline,ipol,inl,
     &        ipoinp,inp,ipoinpc)
!     
      elseif((pressureoverclosure.eq.'L').or.
     &       (pressureoverclosure.eq.'D')) then
!     
         call getnewline(inpc,textpart,istat,n,key,iline,ipol,inl,
     &        ipoinp,inp,ipoinpc)
         if((istat.lt.0).or.(key.eq.1)) then
            write(*,*) '*ERROR reading *SURFACE BEHAVIOR: data'
            write(*,*) '       line is lacking for linear'
            write(*,*) '       behavior'
            call inputerror(inpc,ipoinpc,iline,
     &"*SURFACE BEHAVIOR%")
         endif
!     
         if(pressureoverclosure.eq.'L') then
            elcon(3,1,imat)=2.5d0
         else
            nstate_=max(nstate_,9)
            elcon(3,1,imat)=4.5d0
         endif
!     
!     linear overclosure
!     
!     linear spring stiffness
!     
         read(textpart(1)(1:20),'(f20.0)',iostat=istat)
     &        elcon(2,1,imat)
         if(istat.gt.0) call inputerror(inpc,ipoinpc,iline,
     &"*SURFACE BEHAVIOR%")
!     
         if(elcon(2,1,imat).le.0.d0) then
            write(*,*) '*ERROR reading *SURFACE BEHAVIOR: K must'
            write(*,*) '       be strictly positive'
            call exit(201)
         endif
!     
!     tension at large clearances
!     
         read(textpart(2)(1:20),'(f20.0)',iostat=istat)
     &        elcon(1,1,imat)
         if(istat.gt.0) call inputerror(inpc,ipoinpc,iline,
     &"*SURFACE BEHAVIOR%")
!
c         elcon(1,1,imat)=-elcon(1,1,imat)
!     
!     value of c0coef. If the clearance is inferior to 
!     c0coef*sqrt(slave_area) a contact spring element
!     is generated
!     
         read(textpart(3)(1:20),'(f20.0)',iostat=istat)
     &        elcon(4,1,imat)
         if(istat.gt.0) call inputerror(inpc,ipoinpc,iline,
     &"*SURFACE BEHAVIOR%")
!     
!     default value
!     
         if(elcon(4,1,imat).le.0.d0) then
            elcon(4,1,imat)=1.d-3
         endif
!     
         call getnewline(inpc,textpart,istat,n,key,iline,ipol,inl,
     &        ipoinp,inp,ipoinpc)
!     
      elseif(pressureoverclosure.eq.'T') then
         elcon(3,1,imat)=3.5d0
         elcon(4,1,imat)=1.d-3
         nelcon(1,imat)=-51
!     
!     tabular
!     
         ntmat=0
         npmat=0
!     
         do
!     
            call getnewline(inpc,textpart,istat,n,key,iline,ipol,inl,
     &           ipoinp,inp,ipoinpc)
            if((istat.lt.0).or.(key.eq.1)) exit
            read(textpart(3)(1:20),'(f20.0)',iostat=istat)temperature
            if(istat.gt.0) call inputerror(inpc,ipoinpc,iline,
     &"*SURFACE BEHAVIOR%")
!     
!     first temperature
!     
            if(ntmat.eq.0) then
               npmat=0
               ntmat=ntmat+1
               if(ntmat.gt.ntmat_) then
                  write(*,*) '*ERROR reading *SURFACE BEHAVIOR:'
                  write(*,*) '       increase ntmat_'
                  call exit(201)
               endif
               nplicon(0,imat)=ntmat
               plicon(0,ntmat,imat)=temperature
!     
!     new temperature
!     
            elseif(plicon(0,ntmat,imat).ne.temperature) then
               npmat=0
               ntmat=ntmat+1
               if(ntmat.gt.ntmat_) then
                  write(*,*) '*ERROR reading *SURFACE BEHAVIOR:' 
                  write(*,*) '       increase ntmat_'
                  call exit(201)
               endif
               nplicon(0,imat)=ntmat
               plicon(0,ntmat,imat)=temperature
            endif
            do i=1,2
               read(textpart(i)(1:20),'(f20.0)',iostat=istat) 
     &              plicon(2*npmat+i,ntmat,imat)
               if(istat.gt.0) call inputerror(inpc,ipoinpc,iline,
     &"*SURFACE BEHAVIOR%")
            enddo
            npmat=npmat+1
            if(npmat.gt.npmat_) then
               write(*,*) 
     &              '*ERROR reading *SURFACE BEHAVIOR: increase npmat_'
               call exit(201)
            endif
            nplicon(ntmat,imat)=npmat
         enddo
!     
         if(ntmat.eq.0) then
            write(*,*) 
     &           '*ERROR reading *SURFACE BEHAVIOR: *SURFACE BEHAVIOR'
            write(*,*) '       card without data'
            call exit(201)
         endif
!
!        check whether the difference between the overclosure data
!        points is at least equal to the smallest difference in 
!        double precision numbers (no vertical slope in the pressure
!        versus overclosure curve allowed)
!
         do i=1,npmat-1
            if(plicon(2*i+2,1,imat)-plicon(2*i,1,imat).lt.1.d-10) then
               plicon(2*i+2,1,imat)=plicon(2*i,1,imat)+1.d-10
            endif
         enddo
!     
      endif
!     
      elcon(0,1,imat)=0.d0
!     
      return
      end

