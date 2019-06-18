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
!     construction of the B matrix      
!     
      subroutine networkmpc_rhs(i,ipompc,nodempc,coefmpc,
     &  labmpc,v,bc,j,mi)
!
!     user defined network mpc: calculation of the right hand
!     side
!
!     INPUT:
!
!     i                  MPC number
!     ipompc(1..nmpc))   ipompc(i) points to the first term of
!                        MPC i in field nodempc
!     nodempc(1,*)       node number of a MPC term
!     nodempc(2,*)       coordinate direction of a MPC term
!     nodempc(3,*)       if not 0: points towards the next term
!                                  of the MPC in field nodempc
!                        if 0: MPC definition is finished
!     coefmpc(*)         coefficient of a MPC term
!     labmpc(*)          label of the MPC. For user-defined
!                        network MPC's it starts with NETWORK;
!                        the remaining 13 characters can be used
!                        to distinguish between different kinds of 
!                        network user MPC's
!     vold(0..mi(2),1..nk)   solution field in all nodes
!                        0: total temperature
!                        1: mass flow
!                        2: total pressure
!     j                  network equation corresponding to the
!                        present MPC (i.e. MPC i)
!     mi(*)              field with global information; mi(2) is the
!                        highest variable number 
!
!     OUTPUT:
!
!     bc(*)              right hand side of the system of network
!                        equations; this routine should return bc(j)
!
      implicit none
!     
      character*20 labmpc(*)
!     
      integer mi(*),i,ipompc(*),nodempc(3,*),j,index,node,idir
!     
      real*8 coefmpc(*),v(0:mi(2),*),bc(*)
!
      intent(in) i,ipompc,nodempc,coefmpc,
     &  labmpc,v,j,mi
!
      intent(inout) bc
!     
      if(labmpc(i)(8:16).eq.'QUADRATIC') then
!
!        example equation of the form
!        f:=a*v(idir1,node1)+b*v(idir2,node2)**2=0
!
!        a,idir1,node1,b,idir2,node2 are given in the input deck
!        using the *NETWORK MPC keyword
!        to be calculated: bc(j):=-f
!
         index=ipompc(i)
         node=nodempc(1,index)
         idir=nodempc(2,index)
         bc(j)=coefmpc(index)*v(idir,node)
!
         index=nodempc(3,index)
         node=nodempc(1,index)
         idir=nodempc(2,index)
         bc(j)=bc(j)+coefmpc(index)*v(idir,node)**2
!
         bc(j)=-bc(j)
      else
         write(*,*) '*ERROR in networkmpc_rhs:'
         write(*,*) '       unknown MPC: ',labmpc(i)
      endif
!         
      return
      end
