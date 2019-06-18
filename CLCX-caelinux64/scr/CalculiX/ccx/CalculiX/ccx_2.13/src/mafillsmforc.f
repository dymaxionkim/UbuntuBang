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
      subroutine mafillsmforc(nforc,ndirforc,nodeforc,xforc,nactdof,
     &  fext,nmpc,ipompc,nodempc,ikmpc,ilmpc,coefmpc,mi,rhsi,fnext,
     &  nmethod)
!
!     including point forces into the external force vector
!
      implicit none
!
      logical rhsi
!
      integer nforc,ndirforc(*),nodeforc(2,*),mi(*),nactdof(0:mi(2),*),
     &  nmpc,ipompc(*),nodempc(3,*),ikmpc(*),ilmpc(*),i,jdof,id,ist,
     &  index,nmethod
!
      real*8 xforc(*),fext(*),coefmpc(*),fnext(0:mi(2),*)
!
      if(rhsi) then
!
!        point forces
!      
         do i=1,nforc
            if(ndirforc(i).gt.mi(2)) cycle
c            if(ndirforc(i).gt.3) cycle
!
!           updating the external force vector for dynamic
!           calculations
!
            if(nmethod.eq.4) fnext(ndirforc(i),nodeforc(1,i))=
     &                       fnext(ndirforc(i),nodeforc(1,i))+xforc(i)
!
            jdof=nactdof(ndirforc(i),nodeforc(1,i))
            if(jdof.gt.0) then
               fext(jdof)=fext(jdof)+xforc(i)
            else
!
!              node is a dependent node of a MPC: distribute
!              the forces among the independent nodes
!              (proportional to their coefficients)
!
c               jdof=8*(nodeforc(1,i)-1)+ndirforc(i)
c               call nident(ikmpc,jdof,nmpc,id)
c               if(id.gt.0) then
c                  if(ikmpc(id).eq.jdof) then
               if(jdof.ne.2*(jdof/2)) then
c                     id=ilmpc(id)
                     id=(-jdof+1)/2
                     ist=ipompc(id)
                     index=nodempc(3,ist)
                     if(index.eq.0) cycle
                     do
                        jdof=nactdof(nodempc(2,index),nodempc(1,index))
                        if(jdof.gt.0) then
                           fext(jdof)=fext(jdof)-
     &                          coefmpc(index)*xforc(i)/coefmpc(ist)
                        endif
                        index=nodempc(3,index)
                        if(index.eq.0) exit
                     enddo
c                  endif
               endif
            endif
         enddo
!
      endif
!
      return
      end
