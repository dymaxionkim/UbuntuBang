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
      subroutine mafillp(nef,lakonf,ipnei,neifa,neiel,vfa,area,
     &  advfa,xlet,cosa,volume,au,ad,jq,irow,ap,ielfa,ifabou,xle,
     &  b,xxn,neq,nzs,hfa,gradpel,bp,xxi,neij,
     &  xlen,cosb,nefa,nefb,iau6,xxicn)
!
!     filling the lhs and rhs to calculate p
!
      implicit none
!
      character*8 lakonf(*)
!
      integer i,nef,indexf,ipnei(*),j,neifa(*),nefa,nefb,
     &  neiel(*),iel,ifa,irow(*),ielfa(4,*),compressible,
     &  ifabou(*),neq,jq(*),iel2,indexb,knownflux,indexf2,
     &  j2,neij(*),nzs,k,iau6(6,*)
!
      real*8 coef,vfa(0:7,*),volume(*),area(*),advfa(*),xlet(*),
     &  cosa(*),ad(*),au(*),xle(*),xxn(3,*),ap(*),b(*),cosb(*),
     &  hfa(3,*),gradpel(3,*),bp(*),xxi(3,*),xlen(*),bp_ifa,
     &  xxicn(3,*)
!
      do i=nefa,nefb
         indexf=ipnei(i)
         do j=1,ipnei(i+1)-indexf
!
            knownflux=0
!     
!     diffusion
! 
            indexf=indexf+1
            ifa=neifa(indexf)
            iel=neiel(indexf)
            if(iel.ne.0) then
               coef=vfa(5,ifa)*(volume(i)+volume(iel))*area(ifa)/
     &              (advfa(ifa)*2.d0*xlet(indexf)*cosb(indexf))
               ad(i)=ad(i)-coef
               if(i.gt.iel) au(iau6(j,i))=au(iau6(j,i))+coef
!     
!     correction for non-orthogonal meshes
!     
               j2=neij(indexf)
               indexf2=ipnei(iel)+j2
               bp_ifa=((gradpel(1,iel)*xxicn(1,indexf2)+
     &                  gradpel(2,iel)*xxicn(2,indexf2)+
     &                  gradpel(3,iel)*xxicn(3,indexf2))
     &              *xle(indexf2)
     &              -(gradpel(1,i)*xxicn(1,indexf)+
     &                gradpel(2,i)*xxicn(2,indexf)+
     &                gradpel(3,i)*xxicn(3,indexf))
     &              *xle(indexf))
               b(i)=b(i)-coef*bp_ifa
            else
               iel2=ielfa(2,ifa)
               if(iel2.lt.0) then
                  if((ifabou(-iel2+1).ne.0).and.
     &                 (ifabou(-iel2+2).ne.0).and.
     &                 (ifabou(-iel2+3).ne.0)) then
!     
!                    all velocity components given: inlet or wall
!     
                     knownflux=1
                  elseif(ifabou(-iel2+5).lt.0) then
!
!                    sliding conditions
!
                     knownflux=2
                  elseif(ifabou(-iel2+4).ne.0) then
!     
!                    pressure given (only if not all velocity
!                    components are given)
!     
                     coef=vfa(5,ifa)*volume(i)*area(ifa)/
     &                    (advfa(ifa)*xle(indexf)*cosa(indexf))
                     ad(i)=ad(i)-coef
                     b(i)=b(i)-coef*vfa(4,ifa)
!     
!                    correction for non-orthogonal meshes
!     
                     bp_ifa=(-(gradpel(1,i)*xxicn(1,indexf)+
     &                         gradpel(2,i)*xxicn(2,indexf)+
     &                         gradpel(3,i)*xxicn(3,indexf))
     &                            *xle(indexf))
                     b(i)=b(i)-coef*bp_ifa
                  endif
               endif
            endif
!     
!     save coefficients for correctvfa.f
!     
            if((iel.eq.0).or.(i.lt.iel)) then
               ap(ifa)=coef
               bp(ifa)=bp_ifa
            endif
!     
!     save the coefficient for correctvfa.f
!     
            if(knownflux.eq.1) then
               b(i)=b(i)+vfa(5,ifa)*area(ifa)*
     &              (vfa(1,ifa)*xxn(1,indexf)+
     &              vfa(2,ifa)*xxn(2,indexf)+
     &              vfa(3,ifa)*xxn(3,indexf))
            elseif(knownflux.ne.2) then
               b(i)=b(i)+vfa(5,ifa)*area(ifa)*
     &              (hfa(1,ifa)*xxn(1,indexf)+
     &              hfa(2,ifa)*xxn(2,indexf)+
     &              hfa(3,ifa)*xxn(3,indexf))
            endif
         enddo
      enddo
!     
      return
      end
