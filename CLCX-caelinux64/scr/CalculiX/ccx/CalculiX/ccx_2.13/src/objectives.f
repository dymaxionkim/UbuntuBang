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
      subroutine objectives(inpc,textpart,istep,istat,n,iline,ipol,inl,
     &        ipoinp,inp,ipoinpc,nener,nobject,objectset)        
!
!     reading the input deck: *OBJECTIVE
!
!     criteria: DISPLACEMENT
!               EIGENFREQUENCY
!               GREEN
!               MASS
!               SHAPE ENERGY
!               STRESS
!            
      implicit none
!
      character*1 inpc(*)
      character*20 filter,radius,transition
      character*132 textpart(16)
      character*81 objectset(4,*)
!
      integer istep,istat,n,key,i,iline,ipol,inl,ipoinp(2,*),
     &  inp(3,*),ipoinpc(0:*),nener,nobject,k,ipos
!
      real*8 rho,stress
!
      filter='                    '
      radius='                    '
      transition='                    '
!
      if(istep.lt.1) then
         write(*,*) '*ERROR reading *OBJECTIVE: *OBJECTIVE can
     &only be used within a SENSITIVITY STEP'     
         call exit(201)
      endif
!
      do i=2,n
!  
!        reading filter function
!     
         if(textpart(i)(1:7).eq.'FILTER=') then
            read(textpart(i)(8:27),'(a20)') filter
!     
!        reading filter radius
!
         elseif(textpart(i)(1:7).eq.'RADIUS=') then
           radius=textpart(i)(8:27)
!     
!        reading transition distance
!
         elseif(textpart(i)(1:11).eq.'TRANSITION=') then
           transition=textpart(i)(12:31)          
         else
            write(*,*) 
     &        '*WARNING reading *OBJECTIVE: parameter not recognized:'
            write(*,*) '         ',
     &                 textpart(i)(1:index(textpart(i),' ')-1)
            call inputwarning(inpc,ipoinpc,iline,
     &"*OBJECTIVE%")
         endif
      enddo 
!
!     reading the objectives
!     
      call getnewline(inpc,textpart,istat,n,key,iline,ipol,inl,
     &     ipoinp,inp,ipoinpc)
!     
      do
         if(textpart(1)(1:12).eq.'DISPLACEMENT') then
            nobject=nobject+1
            objectset(1,nobject)(1:12)='DISPLACEMENT'
            do k=13,20
               objectset(1,nobject)(k:k)=' '
            enddo
            if(n.ge.2) then
               read(textpart(2)(1:80),'(a80)',iostat=istat) 
     &              objectset(3,nobject)(1:80) 
               objectset(3,nobject)(81:81)=' '
               ipos=index(objectset(3,nobject),' ')
               if(ipos.ne.1) objectset(3,nobject)(ipos:ipos)='N'
            endif
         elseif(textpart(1)(1:14).eq.'EIGENFREQUENCY') then
            nobject=nobject+1
            objectset(1,nobject)(1:14)='EIGENFREQUENCY'
            do k=15,81
               objectset(1,nobject)(k:k)=' '
            enddo
         elseif(textpart(1)(1:5).eq.'GREEN') then
            nobject=nobject+1
            objectset(1,nobject)(1:5)='GREEN'
            do k=6,81
               objectset(1,nobject)(k:k)=' '
            enddo
         elseif(textpart(1)(1:4).eq.'MASS') then
            nobject=nobject+1
            objectset(1,nobject)(1:4)='MASS'
            do k=5,81
               objectset(1,nobject)(k:k)=' '
            enddo
            if(n.ge.2) then
               read(textpart(2)(1:80),'(a80)',iostat=istat) 
     &              objectset(3,nobject)(1:80) 
               objectset(3,nobject)(81:81)=' '
               ipos=index(objectset(3,nobject),' ')
               if(ipos.ne.1) objectset(3,nobject)(ipos:ipos)='E'
            endif
         elseif(textpart(1)(1:11).eq.'SHAPEENERGY') then
            nobject=nobject+1
            objectset(1,nobject)(1:11)='SHAPEENERGY'
            do k=12,81
               objectset(1,nobject)(k:k)=' '
            enddo
            if(n.ge.2) then
               read(textpart(2)(1:80),'(a80)',iostat=istat) 
     &              objectset(3,nobject)(1:80) 
               objectset(3,nobject)(81:81)=' '
               ipos=index(objectset(3,nobject),' ')
               if(ipos.ne.1) objectset(3,nobject)(ipos:ipos)='E'
            endif
            nener=1
         elseif(textpart(1)(1:6).eq.'STRESS') then
            nobject=nobject+1
            objectset(1,nobject)(1:6)='STRESS'
            do k=7,81
               objectset(1,nobject)(k:k)=' '
            enddo
            if(n.ge.2) then
               read(textpart(2)(1:80),'(a80)',iostat=istat) 
     &              objectset(3,nobject)(1:80) 
               objectset(3,nobject)(81:81)=' '
               ipos=index(objectset(3,nobject),' ')
               if(ipos.ne.1) objectset(3,nobject)(ipos:ipos)='N'
            endif
!
!           rho for the Kreisselmeier-Steinhauser function
!
            if(n.ge.3) then
               read(textpart(3)(1:20),'(f20.0)',iostat=istat) rho
               if(istat.gt.0) call inputerror(inpc,ipoinpc,iline,
     &"*OBJECTIVE%")
               if(rho.lt.1.d0) then
                  write(*,*) '*ERROR reading *OBJECTIVE'
                  write(*,*) '       first Kreisselmeier-Steinhauser'
                  write(*,*) '       parameter rho cannot be less'
                  write(*,*) '       than 1'
                  call inputerror(inpc,ipoinpc,iline,
     &"*OBJECTIVE%")
               endif
               objectset(2,nobject)(41:60)=textpart(3)(1:20)
            endif
!
!           the target stress for the Kreisselmeier-Steinhauser function
!
            if(n.ge.4) then
               read(textpart(4)(1:20),'(f20.0)',iostat=istat) stress
               if(istat.gt.0) call inputerror(inpc,ipoinpc,iline,
     &"*OBJECTIVE%")
               if(stress.le.0.d0) then
                  write(*,*) '*ERROR reading *OBJECTIVE'
                  write(*,*) '       the target stress in the'
                  write(*,*) '       Kreisselmeier-Steinhauser function'
                  write(*,*) '       must be strictly positive'
                  call inputerror(inpc,ipoinpc,iline,
     &"*OBJECTIVE%")
               endif
               objectset(2,nobject)(61:80)=textpart(4)(1:20)
            endif
         else
            write(*,*) '*ERROR reading *OBJECTIVE'
            write(*,*) '       objective function not known'
            call inputerror(inpc,ipoinpc,iline,
     &"*OBJECTIVE%")
         endif
!
!        storing the kind of filter, the filter radius and 
!        the transition distance
!
         objectset(2,nobject)(1:20)=filter
         objectset(2,nobject)(21:40)=radius
         objectset(1,nobject)(21:40)=transition
!     
         call getnewline(inpc,textpart,istat,n,key,iline,ipol,inl,
     &        ipoinp,inp,ipoinpc)
         if((istat.lt.0).or.(key.eq.1)) exit
!     
      enddo
!     
      return
      end
      
      
