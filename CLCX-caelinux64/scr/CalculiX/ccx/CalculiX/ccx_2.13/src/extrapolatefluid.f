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
      subroutine extrapolatefluid(nk,iponofa,inofa,inum,vfa,v,ielfa,
     &  ithermal,imach,ikappa,xmach,xkappa,shcon,nshcon,ntmat_,ielmat,
     &  physcon,mi,iturb,xturb)
!
!     extrapolates the field values at the center of the faces to
!     the nodes
!
      implicit none
!
      integer nk,iponofa(*),inofa(2,*),inum(*),ielfa(4,*),i,l,indexf,
     &  iface,ithermal,imach,ikappa,imat,nshcon(*),ntmat_,mi(*),
     &  ielmat(mi(3),*),iturb
!
      real*8 vfa(0:7,*),v(0:4,*),cp,r,xk,xmach(*),xkappa(*),t1l,
     &  shcon(0:3,ntmat_,*),physcon(*),xturb(2,*)
!
      do i=1,nk
         if(ithermal.eq.0) then
            do l=1,4
               v(l,i)=0.d0
            enddo
            inum(i)=0
            indexf=iponofa(i)
            do
               if(indexf.eq.0) exit
               iface=inofa(1,indexf)
               do l=1,4
                  v(l,i)=v(l,i)+vfa(l,iface)
               enddo
               inum(i)=inum(i)+1
               indexf=inofa(2,indexf)
            enddo
            if(inum(i).gt.0) then
               do l=1,4
                  v(l,i)=v(l,i)/inum(i)
               enddo
            endif
         else
            do l=0,4
               v(l,i)=0.d0
            enddo
            inum(i)=0
            indexf=iponofa(i)
            do
               if(indexf.eq.0) exit
               iface=inofa(1,indexf)
               do l=0,4
                  v(l,i)=v(l,i)+vfa(l,iface)
               enddo
               if(imach.eq.1) then
                  t1l=vfa(0,iface)
                  imat=ielmat(1,ielfa(1,iface))
                  r=shcon(3,1,imat)
                  call materialdata_cp_sec(imat,ntmat_,t1l,
     &                 shcon,nshcon,cp,physcon)
                  xk=cp/(cp-r)
                  xmach(i)=xmach(i)+dsqrt((vfa(1,iface)**2+
     &               vfa(2,iface)**2+vfa(3,iface)**2)/(xk*r*t1l))
               endif
               if(ikappa.eq.1) then
                  xkappa(i)=xkappa(i)+xk
               endif
!
               inum(i)=inum(i)+1
               indexf=inofa(2,indexf)
            enddo
            if(inum(i).gt.0) then
               do l=0,4
                  v(l,i)=v(l,i)/inum(i)
               enddo
               if(imach.eq.1) xmach(i)=xmach(i)/inum(i)
               if(ikappa.eq.1) xkappa(i)=xkappa(i)/inum(i)
            endif
         endif
!
!        turbulence output
!
         if(iturb.ne.0) then
            do l=1,2
               xturb(l,i)=0.d0
            enddo
            indexf=iponofa(i)
            do
               if(indexf.eq.0) exit
               iface=inofa(1,indexf)
               do l=1,2
                  xturb(l,i)=xturb(l,i)+vfa(l+5,iface)
               enddo
               indexf=inofa(2,indexf)
            enddo
            if(inum(i).gt.0) then
               do l=1,2
                  xturb(l,i)=xturb(l,i)/inum(i)
               enddo
            endif
         endif
      enddo
!  
      return
      end
