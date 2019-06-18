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
      subroutine attach_3d(pneigh,pnode,nterms,ratio,dist,xil,etl,zel)
!
!     ataches node with coordinates in "pnode" to the face containing 
!     "nterms" nodes with coordinates in field "pneigh" (nterms < 21).
!     cave: the coordinates are stored in pneigh(1..3,*)
!
      implicit none
!
      integer nterms,i,j,k,imin,jmin,kmin,kflag,n,iy
!
      real*8 ratio(20),pneigh(3,20),pnode(3),dummy,
     &  a(-1:1,-1:1,-1:1),xi(-1:1,-1:1,-1:1),et(-1:1,-1:1,-1:1),p(3),
     &  aold(-1:1,-1:1,-1:1),ze(-1:1,-1:1,-1:1),zeold(-1:1,-1:1,-1:1),
     &  xiold(-1:1,-1:1,-1:1),etold(-1:1,-1:1,-1:1),distmin,xiopt,etopt,
     &  d1,d2,d3,d4,dist,xil,etl,zel,zeopt,dx(3),al
!
      intent(in) pneigh,nterms
!
      intent(inout) xil,etl,zel,dist,pnode,ratio
!
      kflag=1
      n=3
!
c      d1=0.25d0
c      d2=3.125d-2
c      d3=3.9063d-3
c      d4=1.d-3
      d1=1.d-2
      d2=1.d-4
      d3=1.d-6
      d4=1.d-7
!
!     initialisation
!
      do i=-1,1
        do j=-1,1
           do k=-1,1
              xi(i,j,k)=i*d1
              et(i,j,k)=j*d1
              ze(i,j,k)=k*d1
              call distattach_3d(xi(i,j,k),et(i,j,k),ze(i,j,k),pneigh,
     &                        pnode,a(i,j,k),p,ratio,nterms)
c              write(*,'(3(1x,i5),4(1x,e11.4))') 
c     &          i,j,k,xi(i,j,k),et(i,j,k),ze(i,j,k),a(i,j,k)
           enddo
        enddo
      enddo
!
!     minimizing the distance from the face to the node
!
      do
        distmin=a(0,0,0)
        imin=0
        jmin=0
        kmin=0
c        write(*,*) '1 ',xi(0,0,0),et(0,0,0),
c     &         ze(0,0,0),distmin
        do i=-1,1
          do j=-1,1
             do k=-1,1
                if(a(i,j,k).lt.distmin) then
                   distmin=a(i,j,k)
                   imin=i
                   jmin=j
                   kmin=k
                endif
             enddo
          enddo
        enddo
c        write(*,*) '1 ',xi(imin,jmin,kmin),et(imin,jmin,kmin),
c     &         ze(imin,jmin,kmin),distmin
cc        write(*,*) '1 ',xi(0,0,0),et(0,0,0),ze(0,0,0)
!
!       exit if minimum found
!
        if((imin.eq.0).and.(jmin.eq.0).and.(kmin.eq.0)) exit
!
        do i=-1,1
          do j=-1,1
             do k=-1,1
                aold(i,j,k)=a(i,j,k)
                xiold(i,j,k)=xi(i,j,k)
                etold(i,j,k)=et(i,j,k)
                zeold(i,j,k)=ze(i,j,k)
             enddo
          enddo
        enddo
!
        do i=-1,1
          do j=-1,1
             do k=-1,1
                if((i+imin.ge.-1).and.(i+imin.le.1).and.
     &             (j+jmin.ge.-1).and.(j+jmin.le.1).and.
     &             (k+kmin.ge.-1).and.(k+kmin.le.1)) then
                   a(i,j,k)=aold(i+imin,j+jmin,k+kmin)
                   xi(i,j,k)=xiold(i+imin,j+jmin,k+kmin)
                   et(i,j,k)=etold(i+imin,j+jmin,k+kmin)
                   ze(i,j,k)=zeold(i+imin,j+jmin,k+kmin)
                else
                   xi(i,j,k)=xi(i,j,k)+imin*d1
                   et(i,j,k)=et(i,j,k)+jmin*d1
                   ze(i,j,k)=ze(i,j,k)+kmin*d1
!
                   xi(i,j,k)=min(xi(i,j,k),1.d0)
                   xi(i,j,k)=max(xi(i,j,k),-1.d0)
                   et(i,j,k)=min(et(i,j,k),1.d0)
                   et(i,j,k)=max(et(i,j,k),-1.d0)
                   ze(i,j,k)=min(ze(i,j,k),1.d0)
                   ze(i,j,k)=max(ze(i,j,k),-1.d0)
!
                   call distattach_3d(xi(i,j,k),et(i,j,k),ze(i,j,k),
     &                  pneigh,
     &                  pnode,a(i,j,k),p,ratio,nterms)
c              write(*,'(3(1x,i5),4(1x,e11.4))') 
c     &          i,j,k,xi(i,j,k),et(i,j,k),ze(i,j,k),a(i,j,k)
!              write(*,*) a(i,j,k)
                endif
             enddo
          enddo
        enddo
      enddo
!
!     2nd run
!     initialisation
!
      xiopt=xi(0,0,0)
      etopt=et(0,0,0)
      zeopt=ze(0,0,0)
      do i=-1,1
        do j=-1,1
           do k=-1,1
              xi(i,j,k)=xiopt+i*d2
              et(i,j,k)=etopt+j*d2
              ze(i,j,k)=zeopt+k*d2
              xi(i,j,k)=min(xi(i,j,k),1.d0)
              xi(i,j,k)=max(xi(i,j,k),-1.d0)
              et(i,j,k)=min(et(i,j,k),1.d0)
              et(i,j,k)=max(et(i,j,k),-1.d0)
              ze(i,j,k)=min(ze(i,j,k),1.d0)
              ze(i,j,k)=max(ze(i,j,k),-1.d0)
              call distattach_3d(xi(i,j,k),et(i,j,k),ze(i,j,k),pneigh,
     &             pnode,a(i,j,k),p,ratio,nterms)
           enddo
        enddo
      enddo
!
!     minimizing the distance from the face to the node
!
      do
        distmin=a(0,0,0)
        imin=0
        jmin=0
        kmin=0
        do i=-1,1
          do j=-1,1
             do k=-1,1
                if(a(i,j,k).lt.distmin) then
                   distmin=a(i,j,k)
                   imin=i
                   jmin=j
                   kmin=k
                endif
             enddo
          enddo
        enddo
cc        write(*,*) '2 ',xi(0,0,0),et(0,0,0),ze(0,0,0)
!
!       exit if minimum found
!
        if((imin.eq.0).and.(jmin.eq.0).and.(kmin.eq.0)) exit
!
        do i=-1,1
          do j=-1,1
             do k=-1,1
                aold(i,j,k)=a(i,j,k)
                xiold(i,j,k)=xi(i,j,k)
                etold(i,j,k)=et(i,j,k)
                zeold(i,j,k)=ze(i,j,k)
             enddo
          enddo
        enddo
!
        do i=-1,1
          do j=-1,1
             do k=-1,1
                if((i+imin.ge.-1).and.(i+imin.le.1).and.
     &             (j+jmin.ge.-1).and.(j+jmin.le.1).and.
     &             (k+kmin.ge.-1).and.(k+kmin.le.1)) then
                   a(i,j,k)=aold(i+imin,j+jmin,k+kmin)
                   xi(i,j,k)=xiold(i+imin,j+jmin,k+kmin)
                   et(i,j,k)=etold(i+imin,j+jmin,k+kmin)
                   ze(i,j,k)=zeold(i+imin,j+jmin,k+kmin)
                else
                   xi(i,j,k)=xi(i,j,k)+imin*d2
                   et(i,j,k)=et(i,j,k)+jmin*d2
                   ze(i,j,k)=ze(i,j,k)+kmin*d2
!
                   xi(i,j,k)=min(xi(i,j,k),1.d0)
                   xi(i,j,k)=max(xi(i,j,k),-1.d0)
                   et(i,j,k)=min(et(i,j,k),1.d0)
                   et(i,j,k)=max(et(i,j,k),-1.d0)
                   ze(i,j,k)=min(ze(i,j,k),1.d0)
                   ze(i,j,k)=max(ze(i,j,k),-1.d0)
!
                   call distattach_3d(xi(i,j,k),et(i,j,k),ze(i,j,k),
     &                  pneigh,
     &                  pnode,a(i,j,k),p,ratio,nterms)
!              write(*,*) a(i,j,k)
                endif
             enddo
          enddo
        enddo
      enddo
!
!     3rd run
!     initialisation
!
      xiopt=xi(0,0,0)
      etopt=et(0,0,0)
      zeopt=ze(0,0,0)
      do i=-1,1
        do j=-1,1
           do k=-1,1
              xi(i,j,k)=xiopt+i*d3
              et(i,j,k)=etopt+j*d3
              ze(i,j,k)=zeopt+k*d3
              xi(i,j,k)=min(xi(i,j,k),1.d0)
              xi(i,j,k)=max(xi(i,j,k),-1.d0)
              et(i,j,k)=min(et(i,j,k),1.d0)
              et(i,j,k)=max(et(i,j,k),-1.d0)
              ze(i,j,k)=min(ze(i,j,k),1.d0)
              ze(i,j,k)=max(ze(i,j,k),-1.d0)
              call distattach_3d(xi(i,j,k),et(i,j,k),ze(i,j,k),pneigh,
     &             pnode,a(i,j,k),p,ratio,nterms)
           enddo
        enddo
      enddo
!
!     minimizing the distance from the face to the node
!
      do
        distmin=a(0,0,0)
        imin=0
        jmin=0
        kmin=0
        do i=-1,1
          do j=-1,1
             do k=-1,1
                if(a(i,j,k).lt.distmin) then
                   distmin=a(i,j,k)
                   imin=i
                   jmin=j
                   kmin=k
                endif
             enddo
          enddo
        enddo
cc        write(*,*) '3 ',xi(0,0,0),et(0,0,0),ze(0,0,0)
!
!       exit if minimum found
!
        if((imin.eq.0).and.(jmin.eq.0).and.(kmin.eq.0)) exit
!
        do i=-1,1
          do j=-1,1
             do k=-1,1
                aold(i,j,k)=a(i,j,k)
                xiold(i,j,k)=xi(i,j,k)
                etold(i,j,k)=et(i,j,k)
                zeold(i,j,k)=ze(i,j,k)
             enddo
          enddo
        enddo
!
        do i=-1,1
          do j=-1,1
             do k=-1,1
                if((i+imin.ge.-1).and.(i+imin.le.1).and.
     &               (j+jmin.ge.-1).and.(j+jmin.le.1).and.
     &               (k+kmin.ge.-1).and.(k+kmin.le.1)) then
                   a(i,j,k)=aold(i+imin,j+jmin,k+kmin)
                   xi(i,j,k)=xiold(i+imin,j+jmin,k+kmin)
                   et(i,j,k)=etold(i+imin,j+jmin,k+kmin)
                   ze(i,j,k)=zeold(i+imin,j+jmin,k+kmin)
                else
                   xi(i,j,k)=xi(i,j,k)+imin*d3
                   et(i,j,k)=et(i,j,k)+jmin*d3
                   ze(i,j,k)=ze(i,j,k)+kmin*d3
!
                   xi(i,j,k)=min(xi(i,j,k),1.d0)
                   xi(i,j,k)=max(xi(i,j,k),-1.d0)
                   et(i,j,k)=min(et(i,j,k),1.d0)
                   et(i,j,k)=max(et(i,j,k),-1.d0)
                   ze(i,j,k)=min(ze(i,j,k),1.d0)
                   ze(i,j,k)=max(ze(i,j,k),-1.d0)
!
                   call distattach_3d(xi(i,j,k),et(i,j,k),ze(i,j,k),
     &                  pneigh,
     &                  pnode,a(i,j,k),p,ratio,nterms)
!              write(*,*) a(i,j,k)
                endif
             enddo
          enddo
        enddo
      enddo
!
!     4th run
!     initialisation
!
      xiopt=xi(0,0,0)
      etopt=et(0,0,0)
      zeopt=ze(0,0,0)
      do i=-1,1
        do j=-1,1
           do k=-1,1
              xi(i,j,k)=xiopt+i*d4
              et(i,j,k)=etopt+j*d4
              ze(i,j,k)=zeopt+k*d4
              xi(i,j,k)=min(xi(i,j,k),1.d0)
              xi(i,j,k)=max(xi(i,j,k),-1.d0)
              et(i,j,k)=min(et(i,j,k),1.d0)
              et(i,j,k)=max(et(i,j,k),-1.d0)
              ze(i,j,k)=min(ze(i,j,k),1.d0)
              ze(i,j,k)=max(ze(i,j,k),-1.d0)
              call distattach_3d(xi(i,j,k),et(i,j,k),ze(i,j,k),pneigh,
     &           pnode,a(i,j,k),p,ratio,nterms)
           enddo
        enddo
      enddo
!
!     minimizing the distance from the face to the node
!
      do
        distmin=a(0,0,0)
        imin=0
        jmin=0
        kmin=0
        do i=-1,1
          do j=-1,1
             do k=-1,1
                if(a(i,j,k).lt.distmin) then
                   distmin=a(i,j,k)
                   imin=i
                   jmin=j
                   kmin=k
                endif
             enddo
          enddo
        enddo
cc        write(*,*) '4 ',xi(0,0,0),et(0,0,0),ze(0,0,0)
!
!       exit if minimum found
!
        if((imin.eq.0).and.(jmin.eq.0).and.(kmin.eq.0)) exit
!
        do i=-1,1
          do j=-1,1
             do k=-1,1
                aold(i,j,k)=a(i,j,k)
                xiold(i,j,k)=xi(i,j,k)
                etold(i,j,k)=et(i,j,k)
                zeold(i,j,k)=ze(i,j,k)
             enddo
          enddo
        enddo
!
        do i=-1,1
          do j=-1,1
             do k=-1,1
                if((i+imin.ge.-1).and.(i+imin.le.1).and.
     &               (j+jmin.ge.-1).and.(j+jmin.le.1).and.
     &               (k+kmin.ge.-1).and.(k+kmin.le.1)) then
                   a(i,j,k)=aold(i+imin,j+jmin,k+kmin)
                   xi(i,j,k)=xiold(i+imin,j+jmin,k+kmin)
                   et(i,j,k)=etold(i+imin,j+jmin,k+kmin)
                   ze(i,j,k)=zeold(i+imin,j+jmin,k+kmin)
                else
                   xi(i,j,k)=xi(i,j,k)+imin*d4
                   et(i,j,k)=et(i,j,k)+jmin*d4
                   ze(i,j,k)=ze(i,j,k)+kmin*d4
!
                   xi(i,j,k)=min(xi(i,j,k),1.d0)
                   xi(i,j,k)=max(xi(i,j,k),-1.d0)
                   et(i,j,k)=min(et(i,j,k),1.d0)
                   et(i,j,k)=max(et(i,j,k),-1.d0)
                   ze(i,j,k)=min(ze(i,j,k),1.d0)
                   ze(i,j,k)=max(ze(i,j,k),-1.d0)
!
                   call distattach_3d(xi(i,j,k),et(i,j,k),ze(i,j,k),
     &                  pneigh,
     &                  pnode,a(i,j,k),p,ratio,nterms)
!     write(*,*) a(i,j,k)
                endif
             enddo
          enddo
        enddo
      enddo
!
      call distattach_3d(xi(0,0,0),et(0,0,0),ze(0,0,0),pneigh,pnode,
     &     a(0,0,0),p,ratio,nterms)
!
      do i=1,3
        pnode(i)=p(i)
      enddo
!
      dist=a(0,0,0)
!
      if((nterms.eq.20).or.(nterms.eq.8)) then
         xil=xi(0,0,0)
         etl=et(0,0,0)
         zel=ze(0,0,0)
      elseif((nterms.eq.4).or.(nterms.eq.10)) then
         xil=(xi(0,0,0)+1.d0)/2.d0
         etl=(et(0,0,0)+1.d0)/2.d0
         zel=(ze(0,0,0)+1.d0)/2.d0
         dx(1)=xil
         dx(2)=etl
         dx(3)=zel
         call dsort(dx,iy,n,kflag)
         if(dx(3).gt.1.d-30) then
            al=dx(3)/(xil+etl+zel)
            xil=al*xil
            etl=al*etl
            zel=al*zel
         endif
c         if(xil+etl+zel.gt.1.d0) then
c            dummy=2.d0*(1.d0-xil-etl-zel)/3.d0
c            xil=dummy+xil
c            etl=dummy+etl
c            zel=dummy+zel
c         endif
      elseif((nterms.eq.6).or.(nterms.eq.15)) then
         xil=(xi(0,0,0)+1.d0)/2.d0
         etl=(et(0,0,0)+1.d0)/2.d0
         if(xil+etl.gt.1.d0) then
            dummy=xil
            xil=1.d0-etl
            etl=1.d0-dummy
         endif
         zel=ze(0,0,0)
      endif
!     
      return
      end
      
