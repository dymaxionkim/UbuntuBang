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
      subroutine mafillv(nef,ipnei,neifa,neiel,vfa,xxn,area,
     &  auv,adv,jq,irow,nzs,bv,vel,cosa,umfa,xlet,xle,gradvfa,xxi,
     &  body,volume,ielfa,lakonf,ifabou,nbody,
     &  dtimef,velo,veloo,sel,xrlfa,gamma,xxj,nactdohinv,a1,
     &  a2,a3,flux,nefa,nefb,icyclic,c,ifatie,iau6,xxni,xxnj,
     &  iturbulent,gradvel)
!
      implicit none
!
      character*8 lakonf(*)
!
      integer i,nef,indexf,ipnei(*),j,ifa,iel,neifa(*),icyclic,
     &  neiel(*),jq(*),irow(*),nzs,iwall,ielfa(4,*),nefa,nefb,
     &  ipointer,ifabou(*),nbody,k,indexb,nactdohinv(*),
     &  ifatie(*),iau6(6,*),iturbulent
!
      real*8 xflux,vfa(0:7,*),xxn(3,*),area(*),auv(*),adv(*),bv(nef,3),
     &  vel(nef,0:7),cosa(*),umfa(*),xlet(*),xle(*),coef,gradvfa(3,3,*),
     &  xxi(3,*),body(0:3,*),volume(*),coef2,dtimef,velo(nef,0:7),
     &  veloo(nef,0:7),rhovel,constant,sel(3,*),xrlfa(3,*),gamma(*),
     &  xxj(3,*),a1,a2,a3,flux(*),c(3,3),xxni(3,*),xxnj(3,*),difcoef,
     &  xl1,xl2,aa,bb,gradvel(3,3,*)
!
      intent(in) nef,ipnei,neifa,neiel,vfa,xxn,area,
     &  jq,irow,nzs,vel,cosa,umfa,xlet,xle,gradvfa,xxi,
     &  body,volume,ielfa,lakonf,ifabou,nbody,
     &  dtimef,velo,veloo,xrlfa,gamma,xxj,nactdohinv,a1,
     &  a2,a3,flux,gradvel
!
      intent(inout) adv,auv,bv,sel
!
      do i=nefa,nefb
         do indexf=ipnei(i)+1,ipnei(i+1)
!
!              convection
!
            ifa=neifa(indexf)
            iel=neiel(indexf)
            xflux=flux(indexf)
!
!              vfa sometimes undefined?
!
            if(xflux.ge.0.d0) then
!
!                 outflowing xflux
!
               if(iel.eq.0) then
                  adv(i)=adv(i)+xflux
               else
                  adv(i)=adv(i)+xflux
                  do k=1,3
c  cd
c                     bv(i,k)=bv(i,k)-(vfa(k,ifa)-vel(i,k))*xflux
c  retarded gamma
                    bv(i,k)=bv(i,k)-gamma(ifa)*(vfa(k,ifa)
     &                       -vel(i,k))*xflux
c  improved smart
c                     aa=gamma(ifa)
c                     bb=0.d0
c                     if(aa.lt.0.5d0) then
c                        bb=2.d0/3.d0
c                     elseif(aa.lt.0.8d0) then
c                        bb=3.d0/8.d0
c                     endif
c                     bv(i,k)=bv(i,k)-(aa*vel(i,k)
c     &                               +(1.d0-aa)*vel(iel,k)
c     &                               -2.d0*(1.d0-aa-bb)*
c     &                    (gradvel(k,1,i)*xxj(1,indexf)+
c     &                     gradvel(k,2,i)*xxj(2,indexf)+
c     &                     gradvel(k,3,i)*xxj(3,indexf))*xlet(indexf)
c     &                     -vel(i,k))
                  enddo
               endif
            else
               if(iel.gt.0) then
!
!                    incoming flux from neighboring element
!
                  if((icyclic.eq.0).or.(ifatie(ifa).eq.0)) then
                     auv(indexf)=auv(indexf)+xflux
                     do k=1,3
c   cd
c                        bv(i,k)=bv(i,k)
c     &                          -(vfa(k,ifa)-vel(iel,k))*xflux
c   retarded gamma
                        bv(i,k)=bv(i,k)-gamma(ifa)*
     &                          (vfa(k,ifa)-vel(iel,k))*xflux
c   improved smart
c                        aa=gamma(ifa)
c                        bb=0.d0
c                        if(aa.lt.0.5d0) then
c                           bb=2.d0/3.d0
c                        elseif(aa.lt.0.8d0) then
c                           bb=3.d0/8.d0
c                        endif
c                        bv(i,k)=bv(i,k)-(aa*vel(iel,k)
c     &                               +(1.d0-aa)*vel(i,k)
c     &                               +2.d0*(1.d0-aa-bb)*
c     &                    (gradvel(k,1,iel)*xxj(1,indexf)+
c     &                     gradvel(k,2,iel)*xxj(2,indexf)+
c     &                     gradvel(k,3,iel)*xxj(3,indexf))*xlet(indexf)
c     &                     -vel(iel,k))
                     enddo
                  else
                     do k=1,3
                        bv(i,k)=bv(i,k)-vfa(k,ifa)*xflux
                     enddo
                  endif
               else
!
!                    incoming flux through boundary
!
                  if(ielfa(2,ifa).lt.0) then
                     indexb=-ielfa(2,ifa)
                     if(((ifabou(indexb+1).ne.0).and.
     &                   (ifabou(indexb+2).ne.0).and.
     &                   (ifabou(indexb+3).ne.0)).or.
     &                    (dabs(xflux).lt.1.d-10)) then
                        do k=1,3
                           bv(i,k)=bv(i,k)-vfa(k,ifa)*xflux
                        enddo
                     else
c                        write(*,*) '*ERROR in mafillv: not all'
c                        write(*,*) '       components of an incoming'
c                        write(*,*) '       flux through face ',
c     &                       indexf-ipnei(i)
c                        write(*,*)'       of element ',nactdohinv(i),
c     &                        ' are given',
c     &vfa(1,ifa),vfa(2,ifa),vfa(3,ifa)
                     endif
                  else
c                     write(*,*) '*ERROR in mafillv: not all'
c                     write(*,*) '       components of an incoming'
c                     write(*,*) '       flux through face ',
c     &                   indexf-ipnei(i)
c                     write(*,*)'       of element ',nactdohinv(i),
c     &                     ' are given',
c     &vfa(1,ifa),vfa(2,ifa),vfa(3,ifa)
                  endif
               endif
            endif
!
!              diffusion
!
            if(iturbulent.eq.0) then
               difcoef=umfa(ifa)
            else
               difcoef=umfa(ifa)+vfa(5,ifa)*vfa(6,ifa)/vfa(7,ifa)
            endif
!
            if(iel.ne.0) then
!
!                 neighboring element
!
               coef=difcoef*area(ifa)/xlet(indexf)
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
!                 correction for non-orthogonal grid
!
               do k=1,3
                  bv(i,k)=bv(i,k)+difcoef*area(ifa)*
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
!                    external face, but no wall
!
c                  if((ifabou(ipointer+1).ne.0).or.
c     &                 (ifabou(ipointer+2).ne.0).or.
c     &                 (ifabou(ipointer+3).ne.0)) then
c
                  if(ielfa(3,ifa).gt.0) then
!
!                       no outlet: face velocity fixed
!
                     coef=difcoef*area(ifa)/xle(indexf)
                     adv(i)=adv(i)+coef
                     do k=1,3
                        bv(i,k)=bv(i,k)+coef*vfa(k,ifa)
                     enddo
                  else
!
!                       outlet: no diffusion
!
                  endif
!
!                    correction for non-orthogonal grid
!
                  do k=1,3
                     bv(i,k)=bv(i,k)+difcoef*area(ifa)*
     &                 (gradvfa(k,1,ifa)*xxni(1,indexf)+
     &                  gradvfa(k,2,ifa)*xxni(2,indexf)+
     &                  gradvfa(k,3,ifa)*xxni(3,indexf))
                  enddo
c               else
               elseif(iwall.gt.0) then
!     
!                    wall
!     
                  coef=difcoef*area(ifa)/(xle(indexf)*cosa(indexf))
                  adv(i)=adv(i)+coef
!
!                    correction for non-orthogonal grid and nonzero
!                    wall velocity
!
                  coef2=((vel(i,1)-vfa(1,ifa))*xxn(1,indexf)+
     &                 (vel(i,2)-vfa(2,ifa))*xxn(2,indexf)+
     &                 (vel(i,3)-vfa(3,ifa))*xxn(3,indexf))*coef
                  do k=1,3
                     bv(i,k)=bv(i,k)+coef*vfa(k,ifa)+
     &                 coef2*xxn(k,indexf)
                  enddo
               else
!     
!                    sliding conditions (no shear stress)
!     
                  coef=difcoef*area(ifa)/(xle(indexf)*cosa(indexf))
!
!                    correction for non-orthogonal grid
!
                  coef2=((vel(i,1)-vfa(1,ifa))*xxn(1,indexf)+
     &                 (vel(i,2)-vfa(2,ifa))*xxn(2,indexf)+
     &                 (vel(i,3)-vfa(3,ifa))*xxn(3,indexf))*coef
                  do k=1,3
                     bv(i,k)=bv(i,k)-coef2*xxn(k,indexf)
                  enddo
               endif
            endif
!     
!     pressure
!     
         enddo
!     
!           body force
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
         constant=rhovel
         do k=1,3
            bv(i,k)=bv(i,k)-(a2*velo(i,k)+a3*veloo(i,k))*constant
         enddo
         constant=a1*constant
         adv(i)=adv(i)+constant
!
!           copying b into sel (rhs without pressure)
!
         do j=1,3
            sel(j,i)=bv(i,j)
         enddo
!
!           pressure contribution to b
!
         if(iturbulent.eq.0) then
            do indexf=ipnei(i)+1,ipnei(i+1)
               ifa=neifa(indexf)
               do k=1,3
                  bv(i,k)=bv(i,k)
     &                 -vfa(4,ifa)*xxn(k,indexf)*area(ifa)
               enddo
            enddo
         else
            do indexf=ipnei(i)+1,ipnei(i+1)
               ifa=neifa(indexf)
               do k=1,3
                  bv(i,k)=bv(i,k)
     &               -(vfa(4,ifa)+2.d0*vfa(5,ifa)*vfa(6,ifa)/3.d0)
     &               *xxn(k,indexf)*area(ifa)
               enddo
            enddo
         endif
!            
      enddo
!     
      return
      end
