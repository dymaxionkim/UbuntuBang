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
      subroutine usermaterials(inpc,textpart,elcon,nelcon,
     &  nmat,ntmat_,ncmat_,iperturb,iumat,irstrt,istep,istat,n,
     &  iline,ipol,inl,ipoinp,inp,cocon,ncocon,ipoinpc)
!
!     reading the input deck: *USER MATERIAL
!
      implicit none
!
      character*1 inpc(*)
      character*132 textpart(16)
!
      integer nelcon(2,*),nmat,ntmat,ntmat_,istep,istat,ncocon(2,*),
     &  n,key,i,ncmat_,nconstants,imax,isum,j,iperturb(*),iumat,
     &  irstrt,iline,ipol,inl,ipoinp(2,*),inp(3,*),imech,ipoinpc(0:*)
!
      real*8 elcon(0:ncmat_,ntmat_,*),cocon(0:6,ntmat_,*)
!
      iperturb(1)=3
      iperturb(2)=0
      ntmat=0
      iumat=1
!
      if((istep.gt.0).and.(irstrt.ge.0)) then
         write(*,*)'*ERROR in usermaterials: *USER MATERIAL should be'
         write(*,*) '  placed before all step definitions'
         call exit(201)
      endif
!
      if(nmat.eq.0) then
         write(*,*) '*ERROR in usermaterials: *USER MATERIAL should be'
         write(*,*) '  preceded by a *MATERIAL card'
         call exit(201)
      endif
!
      imech=1
!
      do i=2,n
         if(textpart(i)(1:10).eq.'CONSTANTS=') then
            read(textpart(i)(11:20),'(i10)',iostat=istat) nconstants
            if(istat.gt.0) call inputerror(inpc,ipoinpc,iline,
     &"*USER MATERIAL%")
         elseif(textpart(i)(1:12).eq.'TYPE=THERMAL') then
            imech=0
         else
            write(*,*) 
     &        '*WARNING in usermaterials: parameter not recognized:'
            write(*,*) '         ',
     &                 textpart(i)(1:index(textpart(i),' ')-1)
            call inputwarning(inpc,ipoinpc,iline,
     &"*USER MATERIAL%")
         endif
      enddo
!
      if(imech.eq.1) then
!
!        mechanical user material
!
c         if(nconstants.gt.21) then
c            write(*,*) '*ERROR in usermaterials: number of'
c            write(*,*) '       mechanical constants cannot exceed 21'
c            write(*,*) '       change the source code or'
c            write(*,*) '       contact the author'
c            call exit(201)
c         endif
         nelcon(1,nmat)=-100-nconstants
!
         do
            isum=0
            do j=1,(nconstants)/8+1
               if(j.eq.1) then
                  call getnewline(inpc,textpart,istat,n,key,iline,ipol,
     &                 inl,ipoinp,inp,ipoinpc)
                  if((istat.lt.0).or.(key.eq.1)) return
                  ntmat=ntmat+1
                  nelcon(2,nmat)=ntmat
                  if(ntmat.gt.ntmat_) then
                     write(*,*) 
     &                    '*ERROR in usermaterials: increase ntmat_'
                     call exit(201)
                  endif
               else
                  call getnewline(inpc,textpart,istat,n,key,iline,ipol,
     &                 inl,ipoinp,inp,ipoinpc)
                  if((istat.lt.0).or.(key.eq.1)) then
                     write(*,*) 
     &                 '*ERROR in usermaterials: anisotropic definition'
                     write(*,*) '  is not complete. '
                     call inputerror(inpc,ipoinpc,iline,
     &"*USER MATERIAL%")
                     call exit(201)
                  endif
               endif
               imax=8
               if(8*j.gt.nconstants+1) then
                  imax=nconstants-8*j+9
               endif
               do i=1,imax
                  if(isum+i.le.nconstants) then
                     read(textpart(i)(1:20),'(f20.0)',iostat=istat) 
     &                    elcon(isum+i,ntmat,nmat)
                  else
                     read(textpart(i)(1:20),'(f20.0)',iostat=istat) 
     &                    elcon(0,ntmat,nmat)
                  endif
                  if(istat.gt.0) call inputerror(inpc,ipoinpc,iline,
     &"*USER MATERIAL%")
               enddo
               isum=isum+imax
!     
            enddo
         enddo
!
      else
!
!        thermal user material
!
         if(nconstants.gt.6) then
            write(*,*) '*ERROR in usermaterials: number of'
            write(*,*) '       thermal constants cannot exceed 6'
            write(*,*) '       change the source code or'
            write(*,*) '       contact the author'
            call exit(201)
         endif
         ncocon(1,nmat)=-100-nconstants
!
         do
            call getnewline(inpc,textpart,istat,n,key,iline,ipol,
     &           inl,ipoinp,inp,ipoinpc)
            if((istat.lt.0).or.(key.eq.1)) return
            ntmat=ntmat+1
            ncocon(2,nmat)=ntmat
            if(ntmat.gt.ntmat_) then
               write(*,*) 
     &              '*ERROR in usermaterials: increase ntmat_'
               call exit(201)
            endif
!     
            do i=1,nconstants+1
               if(i.le.nconstants) then
                  read(textpart(i)(1:20),'(f20.0)',iostat=istat) 
     &                 cocon(i,ntmat,nmat)
               else
                  read(textpart(i)(1:20),'(f20.0)',iostat=istat) 
     &                 cocon(0,ntmat,nmat)
               endif
               if(istat.gt.0) call inputerror(inpc,ipoinpc,iline,
     &"*USER MATERIAL%")
            enddo
         enddo
!     
      endif
!     
      return
      end

