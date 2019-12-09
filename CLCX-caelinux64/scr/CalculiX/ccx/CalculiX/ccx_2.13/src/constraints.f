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
      subroutine constraints(inpc,textpart,istep,istat,n,iline,ipol,inl,
     &        ipoinp,inp,ipoinpc,nener,nobject,objectset)        
!
!     reading the input deck: *CONSTRAINT
!
!     criteria: DISPLACEMENT
!               EIGENFREQUENCY
!               MASS
!               SHAPE ENERGY
!               STRESS
!               THICKNESS
!            
      implicit none
!
      character*1 inpc(*)
      character*2 consttype
      character*132 textpart(16)
      character*81 objectset(4,*)
!
      integer istep,istat,n,key,i,iline,ipol,inl,ipoinp(2,*),
     &  inp(3,*),ipoinpc(0:*),nener,nobject,k,ipos
!
      real*8 rho,stress,rel,abs
!
      if(istep.lt.1) then
         write(*,*) '*ERROR reading *CONSTRAINT: *CONSTRAINT
     &can only be used within a SENSITIVITY STEP'     
         call exit(201)
      endif
!
!     at least 1 objective must be defined
!     
      if(nobject.eq.0) then
         write(*,*) '*ERROR reading *CONSTRAINT'
         write(*,*) '      at least 1 objective function'
         write(*,*) '      must be defined '
         call inputerror(inpc,ipoinpc,iline,
     &"*CONSTRAINT%")
      endif
!
!     if more than 1 objective is defined no constraint is allowed
!     
      if(nobject.gt.1) then
         write(*,*) '*ERROR reading *CONSTRAINT'
         write(*,*) '      more than 1 objective function'
         write(*,*) '      defined while constraints are '
         write(*,*) '      present. Reduce the number of '
         write(*,*) '      objectives to 1 or remove all '
         write(*,*) '      the constraints               '
         call inputerror(inpc,ipoinpc,iline,
     &"*CONSTRAINT%")
      endif
!
!     reading the constraints
!     
      call getnewline(inpc,textpart,istat,n,key,iline,ipol,inl,
     &     ipoinp,inp,ipoinpc)
!     
      do
!
!        DISPLACEMENT
!
         if(textpart(1)(1:12).eq.'DISPLACEMENT') then
            nobject=nobject+1
            objectset(1,nobject)(1:12)='DISPLACEMENT'
            do k=13,81
               objectset(1,nobject)(k:k)=' '
            enddo
!
!           set definition
!
            if(n.ge.2) then
               read(textpart(2)(1:80),'(a80)',iostat=istat) 
     &              objectset(3,nobject)(1:80) 
               objectset(3,nobject)(81:81)=' '
               ipos=index(objectset(3,nobject),' ')
               if(ipos.ne.1) objectset(3,nobject)(ipos:ipos)='N'
            endif
!
!           LE or GE for constraint
!
            if(n.ge.3) then
               read(textpart(3)(1:2),'(a2)') consttype 
               if((consttype.ne.'LE').and.
     &            (consttype.ne.'GE')) then
                  write(*,*) '*ERROR reading *CONSTRAINT'
                  write(*,*) '       type of constraint must be'
                  write(*,*) '       LE or GE'
                  call inputerror(inpc,ipoinpc,iline,
     &"*CONSTRAINT%")
               endif
               objectset(1,nobject)(19:20)=textpart(3)(1:2)
            endif
!
!           relative constraint value
!
            if(n.ge.4) then
               read(textpart(4)(1:20),'(f20.0)',iostat=istat) rel
               if(istat.gt.0) call inputerror(inpc,ipoinpc,iline,
     &"*CONSTRAINT%")
               objectset(1,nobject)(41:60)=textpart(4)(1:20)
            endif
!
!           absolute constraint value
!
            if(n.ge.5) then
               read(textpart(5)(1:20),'(f20.0)',iostat=istat) abs
               if(istat.gt.0) call inputerror(inpc,ipoinpc,iline,
     &"*CONSTRAINT%")
               if(istat.le.0) then
                  objectset(1,nobject)(61:80)=textpart(5)(1:20)
               endif
            endif
!
!        EIGENFREQUENCY
!
         elseif(textpart(1)(1:14).eq.'EIGENFREQUENCY') then
            nobject=nobject+1
            objectset(1,nobject)(1:14)='EIGENFREQUENCY'
            do k=15,81
               objectset(1,nobject)(k:k)=' '
            enddo
!
!           LE or GE for constraint
!
            if(n.ge.3) then
               read(textpart(3)(1:2),'(a2)') consttype 
               if((consttype.ne.'LE').and.
     &            (consttype.ne.'GE')) then
                  write(*,*) '*ERROR reading *CONSTRAINT'
                  write(*,*) '       type of constraint must be'
                  write(*,*) '       LE or GE'
                  call inputerror(inpc,ipoinpc,iline,
     &"*CONSTRAINT%")
               endif
               objectset(1,nobject)(19:20)=textpart(3)(1:2)
            endif
!
!        MASS
!
         elseif(textpart(1)(1:4).eq.'MASS') then
            nobject=nobject+1
            objectset(1,nobject)(1:4)='MASS'
            do k=5,81
               objectset(1,nobject)(k:k)=' '
            enddo
!
!           set definition
!
            if(n.ge.2) then
               read(textpart(2)(1:80),'(a80)',iostat=istat) 
     &              objectset(3,nobject)(1:80) 
               objectset(3,nobject)(81:81)=' '
               ipos=index(objectset(3,nobject),' ')
               if(ipos.ne.1) objectset(3,nobject)(ipos:ipos)='E'
            endif
!
!           LE or GE for constraint
!
            if(n.ge.3) then
               read(textpart(3)(1:2),'(a2)') consttype 
               if((consttype.ne.'LE').and.
     &            (consttype.ne.'GE')) then
                  write(*,*) '*ERROR reading *CONSTRAINT'
                  write(*,*) '       type of constraint must be'
                  write(*,*) '       LE or GE'
                  call inputerror(inpc,ipoinpc,iline,
     &"*CONSTRAINT%")
               endif
               objectset(1,nobject)(19:20)=textpart(3)(1:2)
            endif
!
!           relative constraint value
!
            if(n.ge.4) then
               read(textpart(4)(1:20),'(f20.0)',iostat=istat) rel
               if(istat.gt.0) call inputerror(inpc,ipoinpc,iline,
     &"*CONSTRAINT%")
               objectset(1,nobject)(41:60)=textpart(4)(1:20)
            endif
!
!           absolute constraint value
!
            if(n.ge.5) then
               read(textpart(5)(1:20),'(f20.0)',iostat=istat) abs
               if(istat.gt.0) call inputerror(inpc,ipoinpc,iline,
     &"*CONSTRAINT%")
               if(istat.le.0) then
                  objectset(1,nobject)(61:80)=textpart(5)(1:20)
               endif
            endif
!
!        SHAPEENERGY
!
         elseif(textpart(1)(1:11).eq.'SHAPEENERGY') then
            nobject=nobject+1
            objectset(1,nobject)(1:11)='SHAPEENERGY'
            do k=12,81
               objectset(1,nobject)(k:k)=' '
            enddo
!
!           set definition
!
            if(n.ge.2) then
               read(textpart(2)(1:80),'(a80)',iostat=istat) 
     &              objectset(3,nobject)(1:80) 
               objectset(3,nobject)(81:81)=' '
               ipos=index(objectset(3,nobject),' ')
               if(ipos.ne.1) objectset(3,nobject)(ipos:ipos)='E'
            endif
            nener=1
!
!           LE or GE for constraint
!
            if(n.ge.3) then
               read(textpart(3)(1:2),'(a2)') consttype 
               if((consttype.ne.'LE').and.
     &            (consttype.ne.'GE')) then
                  write(*,*) '*ERROR reading *CONSTRAINT'
                  write(*,*) '       type of constraint must be'
                  write(*,*) '       LE or GE'
                  call inputerror(inpc,ipoinpc,iline,
     &"*CONSTRAINT%")
               endif
               objectset(1,nobject)(19:20)=textpart(3)(1:2)
            endif
!
!           relative constraint value
!
            if(n.ge.4) then
               read(textpart(4)(1:20),'(f20.0)',iostat=istat) rel
               if(istat.gt.0) call inputerror(inpc,ipoinpc,iline,
     &"*CONSTRAINT%")
         objectset(1,nobject)(41:60)=textpart(4)(1:20)
            endif
!
!           absolute constraint value
!
            if(n.ge.5) then
               read(textpart(5)(1:20),'(f20.0)',iostat=istat) abs
               if(istat.gt.0) call inputerror(inpc,ipoinpc,iline,
     &"*CONSTRAINT%")
               if(istat.le.0) then
                  objectset(1,nobject)(61:80)=textpart(5)(1:20)
               endif
            endif
!
!        STRESS
!
         elseif(textpart(1)(1:6).eq.'STRESS') then
            nobject=nobject+1
            objectset(1,nobject)(1:6)='STRESS'
            do k=7,81
               objectset(1,nobject)(k:k)=' '
            enddo
!
!           set definition
!      
            if(n.ge.2) then
               read(textpart(2)(1:80),'(a80)',iostat=istat) 
     &              objectset(3,nobject)(1:80) 
               objectset(3,nobject)(81:81)=' '
               ipos=index(objectset(3,nobject),' ')
               if(ipos.ne.1) objectset(3,nobject)(ipos:ipos)='N'
            endif
!
!           LE or GE for constraint
!
            if(n.ge.3) then
               read(textpart(3)(1:2),'(a2)') consttype 
               if((consttype.ne.'LE').and.
     &            (consttype.ne.'GE')) then
                  write(*,*) '*ERROR reading *CONSTRAINT'
                  write(*,*) '       type of constraint must be'
                  write(*,*) '       LE or GE'
                  call inputerror(inpc,ipoinpc,iline,
     &"*CONSTRAINT%")
               endif
               objectset(1,nobject)(19:20)=textpart(3)(1:2)
            endif
!
!           relative constraint value
!
            if(n.ge.4) then
               read(textpart(4)(1:20),'(f20.0)',iostat=istat) rel
               if(istat.gt.0) call inputerror(inpc,ipoinpc,iline,
     &"*CONSTRAINT%")
               objectset(1,nobject)(41:60)=textpart(4)(1:20)
            endif
!
!           absolute constraint value
!
            if(n.ge.5) then
               read(textpart(5)(1:20),'(f20.0)',iostat=istat) abs
               if(istat.gt.0) call inputerror(inpc,ipoinpc,iline,
     &"*CONSTRAINT%")
               if(istat.le.0) then
                  objectset(1,nobject)(61:80)=textpart(5)(1:20)
               endif
            endif
!
!           rho for the Kreisselmeier-Steinhauser function
!
            if(n.ge.6) then
               read(textpart(6)(1:20),'(f20.0)',iostat=istat) rho
               if(istat.gt.0) call inputerror(inpc,ipoinpc,iline,
     &"*CONSTRAINT%")
               if(rho.lt.1.d0) then
                  write(*,*) '*ERROR reading *OBJECTIVE'
                  write(*,*) '       first Kreisselmeier-Steinhauser'
                  write(*,*) '       parameter rho cannot be less'
                  write(*,*) '       than 1'
                  call inputerror(inpc,ipoinpc,iline,
     &"*CONSTRAINT%")
               endif
               objectset(2,nobject)(41:60)=textpart(6)(1:20)
            endif
!
!           the target stress for the Kreisselmeier-Steinhauser function
!
            if(n.ge.7) then
               read(textpart(7)(1:20),'(f20.0)',iostat=istat) stress
               if(istat.gt.0) call inputerror(inpc,ipoinpc,iline,
     &"*CONSTRAINT%")
               if(stress.le.0.d0) then
                  write(*,*) '*ERROR reading *OBJECTIVE'
                  write(*,*) '       the target stress in the'
                  write(*,*) '       Kreisselmeier-Steinhauser function'
                  write(*,*) '       must be strictly positive'
                  call inputerror(inpc,ipoinpc,iline,
     &"*CONSTRAINT%")
               endif
               objectset(2,nobject)(61:80)=textpart(7)(1:20)
            endif
!
!        THICKNESS
!
         elseif(textpart(1)(1:9).eq.'THICKNESS') then
            nobject=nobject+1
            objectset(1,nobject)(1:9)='THICKNESS'
            do k=10,81
               objectset(1,nobject)(k:k)=' '
            enddo
!
!           set definition for the constraint
!      
            if(n.ge.2) then
               read(textpart(2)(1:80),'(a80)',iostat=istat) 
     &              objectset(3,nobject)(1:80) 
               objectset(3,nobject)(81:81)=' '
               ipos=index(objectset(3,nobject),' ')
               if(ipos.ne.1) objectset(3,nobject)(ipos:ipos)='N'
            endif
!
!           LE or GE for constraint
!
            if(n.ge.3) then
               read(textpart(3)(1:2),'(a2)') consttype 
               if((consttype.ne.'LE').and.
     &            (consttype.ne.'GE')) then
                  write(*,*) '*ERROR reading *CONSTRAINT'
                  write(*,*) '       type of constraint must be'
                  write(*,*) '       LE or GE'
                  call inputerror(inpc,ipoinpc,iline,
     &"*CONSTRAINT%")
               endif
               objectset(1,nobject)(19:20)=textpart(3)(1:2)
            endif
!
!           set definition for opposite reference nodes
!      
            if(n.ge.4) then
               read(textpart(4)(1:80),'(a80)',iostat=istat) 
     &              objectset(4,nobject)(1:80) 
               objectset(4,nobject)(81:81)=' '
               ipos=index(objectset(4,nobject),' ')
               if(ipos.ne.1) objectset(4,nobject)(ipos:ipos)='N'
            endif
!
!           wall thickness definition
!
            if(n.ge.5) then
               read(textpart(5)(1:20),'(f20.0)',iostat=istat) abs
               if(istat.gt.0) call inputerror(inpc,ipoinpc,iline,
     &"*CONSTRAINT%")
               if(abs.le.0.d0) then
                  write(*,*) '*ERROR reading *OBJECTIVE'
                  write(*,*) '       the acceptable wall thickness'
                  write(*,*) '       value must be strictly positive'     
                  call inputerror(inpc,ipoinpc,iline,
     &"*CONSTRAINT%")
               endif
               objectset(1,nobject)(61:80)=textpart(5)(1:20)
            endif
         else
            write(*,*) '*ERROR reading *CONSTRAINT'
            write(*,*) '       constraint function not known'
            call inputerror(inpc,ipoinpc,iline,
     &"*CONSTRAINT%")
         endif
!     
         call getnewline(inpc,textpart,istat,n,key,iline,ipol,inl,
     &        ipoinp,inp,ipoinpc)
         if((istat.lt.0).or.(key.eq.1)) exit
!     
      enddo
!     
      return
      end
      
      
