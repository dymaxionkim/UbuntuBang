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
      subroutine mafillvcomp(nef,ipnei,neifa,neiel,vfa,xxn,area,
     &  auv,adv,jq,irow,nzs,bv,vel,cosa,umfa,xlet,xle,gradvfa,xxi,
     &  body,volume,ielfa,lakonf,ifabou,nbody,
     &  dtimef,velo,veloo,sel,xrlfa,gamma,xxj,nactdohinv,a1,
     &  a2,a3,flux,nefa,nefb,icyclic,c,ifatie,iau6,xxni,xxnj)
!
      implicit none
!
      character*8 lakonf(*)
!
      integer i,nef,indexf,ipnei(*),j,ifa,iel,neifa(*),nefa,nefb,
     &  neiel(*),jq(*),irow(*),nzs,iwall,compressible,ielfa(4,*),
     &  ipointer,ifabou(*),nbody,k,indexb,nactdohinv(*),
     &  icyclic,ifatie(*),iau6(6,*)
!
      real*8 xflux,vfa(0:7,*),xxn(3,*),area(*),auv(*),adv(*),bv(nef,3),
     &  vel(nef,0:7),cosa(*),umfa(*),xlet(*),xle(*),coef,gradvfa(3,3,*),
     &  xxi(3,*),body(0:3,*),volume(*),coef2,dtimef,velo(nef,0:7),
     &  veloo(nef,0:7),rhovel,constant,sel(3,*),xrlfa(3,*),gamma(*),
     &  xxj(3,*),a1,a2,a3,flux(*),c(3,3),div,xxni(3,*),xxnj(3,*)
!
      intent(in) nef,ipnei,neifa,neiel,vfa,xxn,area,
     &  jq,irow,nzs,vel,cosa,umfa,xlet,xle,gradvfa,xxi,
     &  body,volume,ielfa,lakonf,ifabou,nbody,
     &  dtimef,velo,veloo,xrlfa,gamma,xxj,nactdohinv,a1,
     &  a2,a3,flux
!
      intent(inout) adv,auv,bv,sel
!
      do i=nefa,nefb
         do indexf=ipnei(i)+1,ipnei(i+1)
!
!           convection
!
            ifa=neifa(indexf)
            iel=neiel(indexf)
            xflux=flux(indexf)
!
            if(xflux.ge.0.d0) then
!
!              outflowing flux
!
               adv(i)=adv(i)+xflux
ccccc retarded central differences
c                  do k=1,3
c                     bv(i,k)=bv(i,k)-gamma(ifa)*
c     &                          (vfa(k,ifa)-vel(i,k))*xflux
c                  enddo
ccccc
            else
               if(iel.gt.0) then
!     
!                 incoming flux from neighboring element
!
                  if((icyclic.eq.0).or.(ifatie(ifa).eq.0)) then
                     auv(indexf)=auv(indexf)+xflux
ccccc retarded central differences
c                   do k=1,3
c                      bv(i,k)=bv(i,k)-gamma(ifa)*
c     &                        (vfa(k,ifa)-vel(iel,k))*xflux
c                   enddo
ccccc
                  elseif(ifatie(ifa).gt.0) then
!
!                    for cyclic symmetry the term is retarded, since
!                    otherwise the x-, y- and z- components are linked
!                    (i.e. the x-, y- and z- momentum equations cannot
!                    be solved separately any more)
!
                     do k=1,3
                        bv(i,k)=bv(i,k)-
     &                       (c(k,1)*vel(iel,1)+c(k,2)*vel(iel,2)+
     &                        c(k,3)*vel(iel,3))*xflux
                     enddo
                  else
                     do k=1,3
                        bv(i,k)=bv(i,k)-
     &                       (c(1,k)*vel(iel,1)+c(2,k)*vel(iel,2)+
     &                        c(3,k)*vel(iel,3))*xflux
                     enddo
                  endif
               else
!
!                 incoming flux through boundary
!
                  if(ielfa(2,ifa).lt.0) then
                     indexb=-ielfa(2,ifa)
                     if(((ifabou(indexb+1).ne.0).and.
     &                    (ifabou(indexb+2).ne.0).and.
     &                    (ifabou(indexb+3).ne.0)).or.
     &                    (dabs(xflux).lt.1.d-10)) then
                        do k=1,3
                           bv(i,k)=bv(i,k)-vfa(k,ifa)*xflux
                        enddo
                     else
                        write(*,*) '*ERROR in mafillv: not all'
                        write(*,*) '       components of an incoming'
                        write(*,*) '       flux through face ',j
                        write(*,*)'       of element ',nactdohinv(i),
     &                        ' are given'
                     endif
                  else
                     write(*,*) '*ERROR in mafillv: not all'
                     write(*,*) '       components of an incoming'
                     write(*,*) '       flux through face ',j
                     write(*,*)'       of element ',nactdohinv(i),
     &                     ' are given'
                  endif
               endif
            endif
!
!           diffusion
!
            if(iel.ne.0) then
!
!              neighboring element
!
               coef=umfa(ifa)*area(ifa)/xlet(indexf)
               adv(i)=adv(i)+coef
               if((icyclic.eq.0).or.(ifatie(ifa).eq.0)) then
                  auv(indexf)=auv(indexf)-coef
               elseif(ifatie(ifa).gt.0) then
!
!                 for cyclic symmetry the term is retarded, since
!                 otherwise the x-, y- and z- components are linked
!                 (i.e. the x-, y- and z- momentum equations cannot
!                  be solved separately any more)
!
                  do k=1,3
                     bv(i,k)=bv(i,k)+
     &                (c(k,1)*vel(iel,1)+c(k,2)*vel(iel,2)+
     &                 c(k,3)*vel(iel,3))*coef
                  enddo
               else
                  do k=1,3
                     bv(i,k)=bv(i,k)+
     &                (c(1,k)*vel(iel,1)+c(2,k)*vel(iel,2)+
     &                 c(3,k)*vel(iel,3))*coef
                  enddo
               endif
!
!              correction for non-orthogonal grid
!
               do k=1,3
                  bv(i,k)=bv(i,k)+umfa(ifa)*area(ifa)*
     &              (gradvfa(k,1,ifa)*xxnj(1,indexf)+
     &               gradvfa(k,2,ifa)*xxnj(2,indexf)+
     &               gradvfa(k,3,ifa)*xxnj(3,indexf))
               enddo
            else
!
!                 boundary; check whether wall (specified by user),
!                           outlet (no velocity boundary conditions or
!                           none of those
!
               iwall=0
               ipointer=abs(ielfa(2,ifa))
               if(ipointer.gt.0) then
                  iwall=ifabou(ipointer+5)
               endif
c               if(iwall.lt.1) then
               if(iwall.eq.0) then
!
!                 external face, but no wall
!
c                  if((ifabou(ipointer+1).ne.0).or.
c     &                 (ifabou(ipointer+2).ne.0).or.
c     &                 (ifabou(ipointer+3).ne.0)) then
c
                  if(ielfa(3,ifa).gt.0) then
!
!                    no outlet: face velocity fixed
!
                     coef=umfa(ifa)*area(ifa)/xle(indexf)
                     adv(i)=adv(i)+coef
                     do k=1,3
                        bv(i,k)=bv(i,k)+coef*vfa(k,ifa)
                     enddo
                  else
!
!                    outlet: no diffusion
!
                  endif
!
!                 correction for non-orthogonal grid
!
                  do k=1,3
                     bv(i,k)=bv(i,k)+umfa(ifa)*area(ifa)*
     &                 (gradvfa(k,1,ifa)*xxni(1,indexf)+
     &                  gradvfa(k,2,ifa)*xxni(2,indexf)+
     &                  gradvfa(k,3,ifa)*xxni(3,indexf))
                  enddo
c               else
               elseif(iwall.gt.0) then
!     
!                 wall
!     
                  coef=umfa(ifa)*area(ifa)/(xle(indexf)*cosa(indexf))
                  adv(i)=adv(i)+coef
!
!                 correction for non-orthogonal grid and nonzero
!                 wall velocity
!
                  coef2=((vel(i,1)-vfa(1,ifa))*xxn(1,indexf)+
     &                   (vel(i,2)-vfa(2,ifa))*xxn(2,indexf)+
     &                   (vel(i,3)-vfa(3,ifa))*xxn(3,indexf))*coef
                  do k=1,3
                     bv(i,k)=bv(i,k)+coef*vfa(k,ifa)+
     &                 coef2*xxn(k,indexf)
                  enddo
               else
!     
!                 sliding conditions (no shear stress)
!     
                  coef=umfa(ifa)*area(ifa)/(xle(indexf)*cosa(indexf))
!
!                 correction for non-orthogonal grid
!
                  coef2=((vel(i,1)-vfa(1,ifa))*xxn(1,indexf)+
     &                   (vel(i,2)-vfa(2,ifa))*xxn(2,indexf)+
     &                   (vel(i,3)-vfa(3,ifa))*xxn(3,indexf))*coef
                  do k=1,3
                     bv(i,k)=bv(i,k)-coef2*xxn(k,indexf)
                  enddo
               endif
            endif
!     
!           compressible
!     
            div=2.d0*(gradvfa(1,1,ifa)+gradvfa(2,2,ifa)+
     &                 gradvfa(3,3,ifa))/3.d0
            do k=1,3
               bv(i,k)=bv(i,k)+umfa(ifa)*area(ifa)*
     &           (gradvfa(1,k,ifa)*xxn(1,indexf)+
     &            gradvfa(2,k,ifa)*xxn(2,indexf)+
     &            gradvfa(3,k,ifa)*xxn(3,indexf)-
     &            div*xxn(k,indexf))
            enddo
!     
         enddo
!     
!        body force
!     
         rhovel=vel(i,5)*volume(i)
!
         if(nbody.gt.0) then
            do k=1,3
               bv(i,k)=bv(i,k)+rhovel*body(k,i)
            enddo
         endif
!
!           transient term
!
c         a1=1.d0/dtimef
c         a2=-1.d0/dtimef
c         a3=0.d0/dtimef
c         constant=rhovel
         constant=rhovel/dtimef
c         bv(i,1)=bv(i,1)-(a2*velo(i,1)+a3*veloo(i,1))*constant
c         bv(i,2)=bv(i,2)-(a2*velo(i,2)+a3*veloo(i,2))*constant
c         bv(i,3)=bv(i,3)-(a2*velo(i,3)+a3*veloo(i,3))*constant
         do k=1,3
            bv(i,k)=bv(i,k)+velo(i,k)*constant
         enddo
c         constant=a1*constant
c         call add_sm_fl(auv,adv,jq,irow,i,i,constant,nzs)
         adv(i)=adv(i)+constant
!
!        copying b into sel (rhs without pressure)
!
         do j=1,3
            sel(j,i)=bv(i,j)
         enddo
!
!           pressure contribution to b
!
         do indexf=ipnei(i)+1,ipnei(i+1)
            ifa=neifa(indexf)
            do k=1,3
               bv(i,k)=bv(i,k)
     &           -vfa(4,ifa)*xxn(k,indexf)*area(ifa)
            enddo
         enddo
!            
      enddo
!     
      return
      end
