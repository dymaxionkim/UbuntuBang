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
      subroutine correctvfa(nface,ielfa,area,vfa,ap,bp,xxn,
     &  ifabou,ipnei,nef,neifa,hfa,vel,xboun,lakonf,flux)
!
!     correction of v due to the balance of mass
!     the correction is in normal direction to the face
!
      implicit none
!
      character*8 lakonf(*)
!
      integer i,nface,ielfa(4,*),iel1,iel2,ifabou(*),j,indexf,k,
     &  ipnei(*),ifa,nef,neifa(*),indexb
!
      real*8 ap(*),bp(*),area(*),vfa(0:7,*),xxn(3,*),flux(*),
     &  hfa(3,*),vel(nef,0:7),dh,xboun(*),totflux
!
c$omp parallel default(none)
c$omp& shared(nface,ielfa,ipnei,area,ap,vfa,hfa,xxn,vel,bp,ifabou,xboun)
c$omp& private(i,iel1,j,indexf,iel2,dh,k,indexb)
c$omp do
      do i=1,nface
!
!        first neighboring element
!
         iel1=ielfa(1,i)
         j=ielfa(4,i)
         indexf=ipnei(iel1)+j
!
!        second neighboring element
!
         iel2=ielfa(2,i)
!
!        factor between mass flow and velocity
!
         ap(i)=ap(i)/(area(i)*vfa(5,i))
!
!        internal face
!
         if(iel2.gt.0) then
            dh=hfa(1,i)*xxn(1,indexf)+
     &         hfa(2,i)*xxn(2,indexf)+
     &         hfa(3,i)*xxn(3,indexf)
            do k=1,3
!
!              bp applies if the neighbor element has a higher
!              number than the observer element, else a negative
!              sign has to be appended
!
               if(iel1.lt.iel2) then
                  vfa(k,i)=(dh-ap(i)*(vel(iel2,4)-vel(iel1,4)+bp(i)))
     &                 *xxn(k,indexf)
               else
                  vfa(k,i)=(dh-ap(i)*(vel(iel2,4)-vel(iel1,4)-bp(i)))
     &                 *xxn(k,indexf)
               endif
            enddo
!
         elseif(iel2.lt.0) then
!
!           if flux given: no correction (important for compressible
!           flows in which flux and pressure can be given for the
!           same face)
!
            if(((ifabou(-iel2+1).eq.0).or.
     &          (ifabou(-iel2+2).eq.0).or.
     &          (ifabou(-iel2+3).eq.0)).and.
     &          (ifabou(-iel2+5).eq.0)) then
               indexb=ifabou(-iel2+4)
               if(indexb.ne.0) then
!
!        external face with pressure boundary condition
!
                  dh=hfa(1,i)*xxn(1,indexf)+
     &                 hfa(2,i)*xxn(2,indexf)+
     &                 hfa(3,i)*xxn(3,indexf)
                  do k=1,3
                     vfa(k,i)=(dh-ap(i)*
     &                    (xboun(indexb)-vel(iel1,4)+bp(i)))
     &                    *xxn(k,indexf)
                  enddo
               endif
            endif
         endif
      enddo
c$omp end do
c$omp end parallel
!
!     check conservation of mass
!
c$omp parallel default(none)
c$omp& shared(nef,ipnei,lakonf,neifa,ielfa,ifabou,flux,area,vfa,xxn)
c$omp& private(i,totflux,indexf,j,ifa)
c$omp do
      do i=1,nef
c         totflux=0.d0
c         indexf=ipnei(i)
c         do j=1,ipnei(i+1)-ipnei(i)
c            indexf=indexf+1
         do indexf=ipnei(i)+1,ipnei(i+1)
            ifa=neifa(indexf)
!
            if(ielfa(2,ifa).lt.0) then
               if(ifabou(-ielfa(2,ifa)+5).lt.0) then
                  flux(indexf)=0.d0
                  cycle
               endif
            endif
!
            flux(indexf)=area(ifa)*vfa(5,ifa)*
     &               (vfa(1,ifa)*xxn(1,indexf)+
     &                vfa(2,ifa)*xxn(2,indexf)+
     &                vfa(3,ifa)*xxn(3,indexf))
c               write(*,*) 'correctvfa ',i,ifa
c               write(*,*) vfa(5,ifa)
c               write(*,*) vfa(1,ifa)
c               write(*,*) vfa(2,ifa)
c               write(*,*) vfa(3,ifa)
c               write(*,*) flux(ifa)
c            totflux=totflux+flux(indexf)
         enddo
c         write(*,*) 'correctvfa mass check ',i,totflux
      enddo
c$omp end do
c$omp end parallel
c      write(*,*)
!  
      return
      end
