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
      subroutine basis(x,y,z,xo,yo,zo,nx,ny,nz,
     &         planfa,ifatet,nktet,netet,field,nfield,
     &         cotet,kontyp,ipkon,kon,iparent,
     &         xp,yp,zp,value,ratio,iselect,nselect,
     &         istartset,iendset,ialset,imastset,ielemnr,
     &         nterms,konl)
!
      implicit none
!
!     100 nearest nodes: node(100),idummy1(100),idummy2(100),iparentel(100)
!
      integer ifatet(4,*),id,node(100),near,nx(*),ny(*),nz(*),
     &  ifs,iface,i,konl(20),nfield,nktet,ielement,ielmax,netet,k,
     &  j,iselect(nselect),nselect,kontyp(*),ipkon(*),kon(*),iparent(*),
     &  nterms,indexe,nelem,konl_opt(20),idummy1(100),idummy2(100),
     &  iparentel(100),nparentel,kflag,ii,inside,nterms_opt,
     &  istartset(*),iendset(*),ialset(*),imastset,ielemnr(*),
     &  ielementnr,nlength,nearset
!
      real*8 cotet(3,*),planfa(4,*),dface,dfacemax,tolerance,
     &  dist,field(nfield,nktet),ratio(20),pneigh(3,20),pnode(3),xi,et,
     &  ze,dist_opt,ratio_opt(20),xp,yp,zp,x(*),y(*),z(*),xo(*),yo(*),
     &  zo(*),value(nfield)
!
      intent(in) x,y,z,xo,yo,zo,nx,ny,nz,
     &         planfa,ifatet,nktet,netet,field,nfield,
     &         cotet,kontyp,ipkon,kon,iparent,
     &         xp,yp,zp,iselect,nselect,
     &         istartset,iendset,ialset,imastset,ielemnr
!
      intent(inout) konl,value,nterms,ratio
!
      tolerance=1.d-6
!
!     look for the global element encompassing point (xp,yp,zp) 
!     
      do ii=1,3
!     
!        three-level algorithm
!        - start with the search for the nearest neighboring element
!        - if this element is not the right one search for the 10
!          nearest neighbors
!        - if no fitting element was found, search for the 100 nearest
!          neighbors
!
         if(ii.eq.1) then
            near=1
         elseif(ii.eq.2) then
            near=10
         else
            near=100
         endif
         call near3d(xo,yo,zo,x,y,z,nx,ny,nz,xp,yp,zp,netet,
     &        node,near)
!     
         inside=0
         nearset=0
         ielmax=0
!     
         do i=1,near
            ielement=node(i)
c            write(*,*) 'basis tetrahedron ',ielement,iparent(ielement)
!
!           check whether the element belongs to the right set,
!           if a set is specified
!
!           ielement is a tetrahedral element number
!           iparent(ielement) is a running number from 1 to the
!                total number of elements
!           ielemnr(iparent(ielement)) is the real element number
!                (there can be gaps in the numbering)
!
            if(imastset.ne.0) then
               ielementnr=ielemnr(iparent(ielement))
               nlength=iendset(imastset)-istartset(imastset)+1
               call nident(ialset(istartset(imastset)),ielementnr,
     &                    nlength,id)
               if(id.le.0) cycle
               if(ialset(istartset(imastset)+id-1).ne.ielementnr) cycle
               nearset=nearset+1
               node(nearset)=node(i)
            else
               nearset=near
            endif

            dface=0.d0
            do j=1,4
!
!              the face number in ifatet is negative, if the equation
!              of the face plane is such, that its value in the 
!              remaining node of the tetrahedron is negative
!
               ifs=ifatet(j,ielement)
               iface=abs(ifs)
               dist=planfa(1,iface)*xp+planfa(2,iface)*yp
     &              +planfa(3,iface)*zp+planfa(4,iface)
               if(dist*ifs.lt.-1.d-10*iface) then
                  dface=dface+dist*ifs/iface
               endif
            enddo
c            write(*,*) 'basis dface ',dface
            if(dface.gt.-1.d-10) then
               inside=1
               exit
            endif
c            if(i.eq.1) then
            if(ielmax.eq.0) then
               dfacemax=dface
               ielmax=ielement
            else
               if(dface.gt.dfacemax) then
                  ielmax=ielement
                  dfacemax=dface
               endif
            endif
         enddo
!
!        if a global element set was defined, check whether an
!        appropriate element was found
!
         if(imastset.ne.0) then
            if(nearset.eq.0) then
               if(ii.lt.3) then
                  cycle
               else
                  write(*,*) '*ERROR: no suitable global element found'
                  write(*,*) '        for location (',xp,yp,zp,')'
                  call exit(201)
               endif
            endif
         endif
!     
!     if no element was found, the element with the smallest
!     dfacemax (in absolute value; summed distance) is taken 
!     
         if(inside.eq.0) then
            ielement=ielmax
         endif
!     
         nelem=iparent(ielement)
c         write(*,*) 'basis element ',nelem
         if(kontyp(nelem).eq.1) then
            nterms=8
         elseif(kontyp(nelem).eq.2) then
            nterms=6
         elseif(kontyp(nelem).eq.3) then
            nterms=4
         elseif(kontyp(nelem).eq.4) then
            nterms=20
         elseif(kontyp(nelem).eq.5) then
            nterms=15
         elseif(kontyp(nelem).eq.6) then
            nterms=10
         else
            nterms=0
         endif
         indexe=ipkon(nelem)
!     
!     modify order of connectivity ccx <-> cgx
!     
         if(kontyp(nelem).eq.4) then
            do i=1,12
               konl(i)=kon(indexe+i)
            enddo
            do i=13,16
               konl(i+4)=kon(indexe+i)
            enddo
            do i=17,20
               konl(i-4)=kon(indexe+i)
            enddo
         elseif(kontyp(nelem).eq.5) then
            do i=1,9
               konl(i)=kon(indexe+i)
            enddo
            do i=10,12
               konl(i+3)=kon(indexe+i)
            enddo
            do i=13,15
               konl(i-3)=kon(indexe+i)
            enddo
         else
            do i=1,nterms
               konl(i)=kon(indexe+i)
            enddo
         endif
!     
!     nodes of master element
!     
         do i=1,nterms
            do j=1,3
               pneigh(j,i)=cotet(j,konl(i))
            enddo
         enddo
!     
!     slave node
!     
         pnode(1)=xp
         pnode(2)=yp
         pnode(3)=zp
!     
!     attaching slave node to master element
!     
         if(nterms.ne.0) then
            call attach_3d(pneigh,pnode,nterms,ratio,dist,xi,et,ze)
         endif
!     
!     checking the parent elements of the "best" tetrahedra
!     in case the distance between slave node and location of
!     interpolation exceeds "tolerance"
!     
         if(dist.gt.tolerance) then
            do i=1,nterms
               konl_opt(i)=konl(i)
               ratio_opt(i)=ratio(i)
            enddo
            nterms_opt=nterms
            dist_opt=dist
!     
!     sorting the parent elements
!     
            do i=1,nearset
               idummy1(i)=iparent(node(i))
            enddo
            kflag=1
            call isortii(idummy1,idummy2,nearset,kflag)
            nparentel=0
            do i=1,nearset
               if(idummy1(i).eq.nelem) cycle
               if(i.eq.1) then
                  iparentel(1)=idummy1(1)
                  nparentel=1
               else
                  if(idummy1(i).ne.idummy1(i-1)) then
                     nparentel=nparentel+1
                     iparentel(nparentel)=idummy1(i)
                  endif
               endif
            enddo
!     
            do k=1,nparentel
!     
!     slave node
!     
               pnode(1)=xp
               pnode(2)=yp
               pnode(3)=zp
               nelem=iparentel(k)
               if(kontyp(nelem).eq.1) then
                  nterms=8
               elseif(kontyp(nelem).eq.2) then
                  nterms=6
               elseif(kontyp(nelem).eq.3) then
                  nterms=4
               elseif(kontyp(nelem).eq.4) then
                  nterms=20
               elseif(kontyp(nelem).eq.5) then
                  nterms=15
               elseif(kontyp(nelem).eq.6) then
                  nterms=10
               else
                  nterms=0
               endif
               indexe=ipkon(nelem)
!     
!     modify order of connectivity ccx <-> cgx
!     
               if(kontyp(nelem).eq.4) then
                  do i=1,12
                     konl(i)=kon(indexe+i)
                  enddo
                  do i=13,16
                     konl(i+4)=kon(indexe+i)
                  enddo
                  do i=17,20
                     konl(i-4)=kon(indexe+i)
                  enddo
               elseif(kontyp(nelem).eq.5) then
                  do i=1,9
                     konl(i)=kon(indexe+i)
                  enddo
                  do i=10,12
                     konl(i+3)=kon(indexe+i)
                  enddo
                  do i=13,15
                     konl(i-3)=kon(indexe+i)
                  enddo
               else
                  do i=1,nterms
                     konl(i)=kon(indexe+i)
                  enddo
               endif
!     
!     nodes of master element
!     
               do i=1,nterms
                  do j=1,3
                     pneigh(j,i)=cotet(j,konl(i))
                  enddo
               enddo
!     
!     attaching slave node to master element
!     
               if(nterms.ne.0) then
                  call attach_3d(pneigh,pnode,nterms,ratio,dist,
     &                 xi,et,ze)
               endif
!     
!     check whether the present element yields better results
!     
               if(dist.lt.dist_opt) then
                  do i=1,nterms
                     konl_opt(i)=konl(i)
                     ratio_opt(i)=ratio(i)
                  enddo
                  nterms_opt=nterms
                  dist_opt=dist
               endif
               if(dist.lt.tolerance) exit
            enddo
!     
!     storing the optimal configuration
!     
            nterms=nterms_opt
            do i=1,nterms
               konl(i)=konl_opt(i)
               ratio(i)=ratio_opt(i)
            enddo
         endif
!     
         if((ii.eq.3).or.(dist.lt.tolerance)) exit
!     
      enddo
!     
!     interpolating the fields
!     
      do k=1,nselect
         i=iselect(k)
         value(k)=0.d0
         do j=1,nterms
            value(k)=value(k)+ratio(j)*field(i,konl(j))
c            write(*,*) 'basis j',j,konl(j),field(i,konl(j))
         enddo
c         write(*,*) 'basis select',k,i,value(k)
      enddo
!     
      return
      end
      
