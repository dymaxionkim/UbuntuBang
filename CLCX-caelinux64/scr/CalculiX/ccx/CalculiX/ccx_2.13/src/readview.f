!
!     CalculiX - A 3-dimensional finite element program
!     Copyright (C) 1998-2017 Guido Dhondt
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
!
!     center of gravity of the projection of the vertices for
!     visibility purposes
!     exact integration for one triangle: routine cubtri
!     if the surfaces are far enough away, one-point integration
!     is used
! 
      subroutine readview(ntr,adview,auview,fenv,nzsrad,ithermal,
     &  jobnamef)
!     
!     reading the viewfactors from file
!
      implicit none
!     
      logical exi
!
      character*132 jobnamef(*),fnvw
!     
      integer ntr,nzsrad,ithermal,i,k
!
      real*8 adview(*),auview(*),fenv(*)
!     
      if(ithermal.eq.3) then
         write(*,*) '*WARNING in radmatrix: viewfactors are being'
         write(*,*) '         read from file for a thermomechani-'
         write(*,*) '         cal calculation: they will not be '
         write(*,*) '         recalculated in every iteration.'
      endif
!     
      write(*,*) 'Reading the viewfactors from file'
      write(*,*)
!     
      if(jobnamef(2)(1:1).eq.' ') then
         do i=1,132
            if(jobnamef(1)(i:i).eq.' ') exit
         enddo
         i=i-1
         fnvw=jobnamef(1)(1:i)//'.vwf'
      else
         fnvw=jobnamef(2)
      endif
      inquire(file=fnvw,exist=exi)
      if(exi) then
         open(10,file=fnvw,status='old',form='unformatted',
     &        access='sequential',err=10)
      else
         write(*,*) '*ERROR in radmatrix: viewfactor file ',fnvw
         write(*,*) 'does not exist'
         call exit(201)
      endif
!     
      read(10) (adview(k),k=1,ntr)
      read(10) (auview(k),k=1,2*nzsrad)
      read(10)(fenv(k),k=1,ntr)
!     
      close(10)
!     
      return
!
 10   write(*,*) '*ERROR in radmatrix: could not open file ',fnvw
      call exit(201)
      end
      
