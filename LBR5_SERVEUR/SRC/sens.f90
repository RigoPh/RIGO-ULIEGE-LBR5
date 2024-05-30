subroutine sens(phil,teta,q,m,mt,ipan)



use param_section


implicit double precision(a-h,o-z)
dimension id1(13),id2(13)
data id1 /2, 1,29,25, 27,24,26,3, 35, 38, 31,30,33/       ! (ordre dans disc, ...)
data id2/1,-1,-1,+1, -1,+1,-1,1, +1, -1, -1,+1,-1/        ! (+1 symétrique
!    c.à.d.   u, v,wø,ny,nyx,my,ry,w,wøø,wøøø,uø,vø,vøø   ! (-1 antimétrique
!
!--------------------------------------------------------------------------------
!     subroutine sens (optimisation)
!     ===============================
!     la subr. calcule les dérivées des 13 fonctions (u,v,w,ny, ... ,wøøø)
!     en 10 pts selon l'axe oy, pour les 8 cas de base (disa, disb, etc.) 
!     et pour la charge extérieure (dish) 
!
!     modifié: le 19-6-95 (traverses: subr. ali2)             créé : 3-3-94
!     		le 20-8-95 
!     		le 1-12-95 (plusieurs cas de charge, nsol)
!     		le 27-3-96 (sens2 , termes en y2 et y3) 
!
!         last chanhe : 15-2-2001
!--------------------------------------------------------------------------------     
!  ypt   = coord. selon oy des pts de calcul.
!  iptmax= nbre de pts de calcul max (4 pts )
!  sens(13,10,9) = sens(j,i,k) = vecteurs résultats
!           avec j relatif aux variables (u,v,w,ny, ...);  j=1,13
!           avec i relatif aux points de calcul ;          i=1,iptmax
!           avec k relatif aux variables de conception xi; k=1,nvar(nel,iboat).
!  dhyp(i,j,k) = dérivée des vecteurs solutions de forces xo et zo (traverses)
!            si k  la variable de conception (1 à 9)
!               j  les 9 cas de charge (dish, disc en y=0, en y=yo, disb, ...)
!               i  les dérivées de xo pour les mt traverses + idem avec zo (1 à ms)
!  ordre des var. dans dvarx et sensx :
!    u=1 v=2 wø=3 ny=4 nyx=5 my=6 ry=7 w=8 wøø=9 wøøø=10 uø=11 vø=12 et vøø=13
!    puis les (xo et zo) des traverses de (14 et 15)  à (32 et 33)

nel=ipan

! pi/180
pi1=pi/180.d00                                     
! 2 pi 
pi2=2.*pi                                       
! yo pi/180 
phil1=phil*pi1                                  
teta1=teta*pi1
	
do i=1,iptmax                                                    
   !soit y1 la coordonnée du point de calcul par rapport à y=o 
   
   !     y pi/180
   y1=ypts(i,nel)*pi1                               
   !   q y pi/180
   y1q=y1 * q                                  
   !    (2pi-y)
   y11=pi2-y1                                  
   !   q(2pi-y) 
   y11q=y11*q                                  


   !soit y2 la coordonnée du point de calcul  par rapport à y=yo (phil) 
   !     (yo-y) 
   y2=phil1-y1                                 
   !    q(yo-y) 
   y2q=y2*q                                    
   !     (2 pi-(yo-y)) 
   y22=pi2-y2                                  
   !    q(2 pi-(yo-y)) 
   y22q=y22*q                                  



   tetat=teta1+y1
   if(phil.ge.1.e-05) then
      sint=dsin(tetat)
      cost=dcos(tetat)
   else
      if(dabs(teta).eq.180.) then
         cost=-dcos(y1)
         sint=-dsin(y1)
      endif
      if(dabs(teta).eq.90.)  then
         ii=1
         if(teta.eq.-90.) ii=-1
         cost=-dsin(y1)*ii
         sint=+dcos(y1)*ii
      endif
      cost=dcos(tetat)
      sint=dsin(tetat)
   endif
   
   do l=1,m                                                    
      ! y 
      al1=argq(2*l-1)*y1                          
      be1=argq(2*l)  *y1                          
      ! yo-y
      al2=argq(2*l-1)*y2                          
      be2=argq(2*l)  *y2                          
      ! 2 pi - y
      al3=argq(2*l-1)*y11                         
      be3=argq(2*l)  *y11                         
      ! 2 pi-(yo-y)
      al4=argq(2*l-1)*y22                         
      be4=argq(2*l)  *y22                         
      al1=expo(al1)
      al2=expo(al2)
      al3=expo(al3)
      al4=expo(al4)
      cos1=al1*dcos(be1)
      sin1=al1*dsin(be1)
      cos2=al2*dcos(be2)
      sin2=al2*dsin(be2)
      cos3=al3*dcos(be3)
      sin3=al3*dsin(be3)
      cos4=al4*dcos(be4)
      sin4=al4*dsin(be4) 	
      
      if(mt.ge.1) then

         call ali2(alix,aliz,disc,disb,panneau(nel).abtr(:),mt,y1,q,pi1,pi2,l,id1,id2,argq,dalix,daliz,dvarb,dvarc,darg,   &
		           nvar(nel,iboat),nxit(:,nel,iboat),phil)	

      endif
      
      
	    ! disc (y=0)
      call def1(sin1,cos1,sin3,cos3,disc,defc, i,l,m,id1,1,mt,alix,aliz,hyp)                           
      ! disc (y=yo)
      call def1(sin2,cos2,sin4,cos4,disc,defcc,i,l,m,id1,2,mt,alix,aliz,hyp)                           
      ! disd (y=0)
      call def1(sin1,cos1,sin3,cos3,disd,defd, i,l,m,id1,3,mt,alix,aliz,hyp)                        
      ! disd (y=yo) 
      call def1(sin2,cos2,sin4,cos4,disd,defdd,i,l,m,id1,4,mt,alix,aliz,hyp)                     
      ! disa (y=0)
      call def1(sin1,cos1,sin3,cos3,disa,defa, i,l,m,id1,5,mt,alix,aliz,hyp)                 
      ! disa (y=yo)
      call def1(sin2,cos2,sin4,cos4,disa,defaa,i,l,m,id1,6,mt,alix,aliz,hyp)                     
      ! disb (y=0)
      call def1(sin1,cos1,sin3,cos3,disb,defb, i,l,m,id1,7,mt,alix,aliz,hyp)
      ! disb (y=yo)     
      call def1(sin2,cos2,sin4,cos4,disb,defbb,i,l,m,id1,8,mt,alix,aliz,hyp)          
      
      if(nvar(nel,iboat).ne.0) then
         ! disc (y=0)   
         call sens1sr(y1q,y11q,sin1,cos1,sin3,cos3,disc,dvarc,sensc, 1,i,l,m,id1,nxit(:,nel,iboat),nvar(nel,iboat),mt,alix,aliz,dalix,daliz,dhyp,hyp,darg)
         ! disc (y=yo) 
         call sens1sr(y2q,y22q,sin2,cos2,sin4,cos4,disc,dvarc,senscc,2,i,l,m,id1,nxit(:,nel,iboat),nvar(nel,iboat),mt,alix,aliz,dalix,daliz,dhyp,hyp,darg)
         ! disd (y=0)  
         call sens1sr(y1q,y11q,sin1,cos1,sin3,cos3,disd,dvard,sensd, 3,i,l,m,id1,nxit(:,nel,iboat),nvar(nel,iboat),mt,alix,aliz,dalix,daliz,dhyp,hyp,darg)
         ! disd (y=yo) 
         call sens1sr(y2q,y22q,sin2,cos2,sin4,cos4,disd,dvard,sensdd,4,i,l,m,id1,nxit(:,nel,iboat),nvar(nel,iboat),mt,alix,aliz,dalix,daliz,dhyp,hyp,darg)
         ! disa (y=0)
         call sens1sr(y1q,y11q,sin1,cos1,sin3,cos3,disa,dvara,sensa, 5,i,l,m,id1,nxit(:,nel,iboat),nvar(nel,iboat),mt,alix,aliz,dalix,daliz,dhyp,hyp,darg)
         ! disa (y=yo)
         call sens1sr(y2q,y22q,sin2,cos2,sin4,cos4,disa,dvara,sensaa,6,i,l,m,id1,nxit(:,nel,iboat),nvar(nel,iboat),mt,alix,aliz,dalix,daliz,dhyp,hyp,darg)
         ! disb (y=0)    
         call sens1sr(y1q,y11q,sin1,cos1,sin3,cos3,disb,dvarb,sensb, 7,i,l,m,id1,nxit(:,nel,iboat),nvar(nel,iboat),mt,alix,aliz,dalix,daliz,dhyp,hyp,darg)
         ! disb (y=yo)
         call sens1sr(y2q,y22q,sin2,cos2,sin4,cos4,disb,dvarb,sensbb,8,i,l,m,id1,nxit(:,nel,iboat),nvar(nel,iboat),mt,alix,aliz,dalix,daliz,dhyp,hyp,darg)
         
	     is1=1

         do is=1,nsol
	        if(is.gt.1) is1=8+is

            call sens2sr(y1,y1q,y11q,y2q,y22q,sin1,cos1,sin2,cos2,sin3,cos3,   &
            sin4,cos4,cost,sint,dish(1,is),dvarh(1,1,1,is),sensh(1,1,1,is),    &
            i,l,m,id1,nxit(:,nel,iboat),nvar(nel,iboat),mt,alix,aliz,dalix,daliz,dhyp,is1,hyp,darg)
         enddo

         ! pour annuler sensh et sensa à sensdd 
     else                                            
        sensh(:,:,:,:)=0.0d+00            
     endif                                       
                                                     
   enddo
   
   
   if((impr.eq.1).and.(mt.ge.1))then
      write(iu_11(iboat),*) 'ipt',i
      write(iu_11(iboat),*) '======='
      write(iu_11(iboat),*) 'alix et aliz'
      do ik=1,13
         write(iu_11(iboat),567) 'alix(j,',ik,')',(alix(j,ik),j=1,mt)
         write(iu_11(iboat),567) 'aliz(j,',ik,')',(aliz(j,ik),j=1,mt)
      enddo
      do ivar=1,nvar(nel,iboat)
         k=nxit(ivar,nel,iboat)
         write(iu_11(iboat),*) 'dalix et daliz (pour xi=1)'
         do ik=1,13
            write(iu_11(iboat),568) 'dalix(j,',ik,k,')=',(dalix(j,ik,k),j=1,mt)
            write(iu_11(iboat),568) 'daliz(j,',ik,k,')=',(daliz(j,ik,k),j=1,mt)
         enddo
      enddo
   endif
567  format(a8,i2,a3,4(5e14.7/))
568  format(a8,i3,i3,a3,4(5e14.7/))

enddo

! impressions
! ------------

if(impr.eq.1) then
   write(iu_11(iboat),16)
   do i=1,iptmax
      write(iu_11(iboat),*)
      do j=1,13
         write(iu_11(iboat),7)i,j,defa(j,i),defb(j,i),defc(j,i),defd(j,i),defaa(j,i),defbb(j,i),defcc(j,i),defdd(j,i)
      enddo
   enddo
   
   if((nvar(nel,iboat).ge.1)) then
      write(iu_11(iboat),8) nsol
      do i=1,iptmax
         y1=ypts(i,nel)*pi1*q
         write(iu_11(iboat),*)'   pt nø= ',i,' ypt(i) =',y1,' m'
         do ivar=1,nvar(nel,iboat)
            k=nxit(ivar,nel,iboat)
            do j=1,13
               write(iu_11(iboat),6) j,k,                       &
                     sensa(j,i,k) ,sensb (j,i,k),sensc (j,i,k), &
                     sensd(j,i,k) ,sensaa(j,i,k),sensbb(j,i,k), &
                     senscc(j,i,k),sensdd(j,i,k),               &
                     (sensh (j,i,k,is),is=1,nsol)
            enddo
         enddo
      enddo
   endif
endif
	
	
!  formats
!  ------------
 6 format(1x,i2,4x,i2,1x,4(1x,e13.6)/10x,4(1x,e13.6)/10x,5(1x,e13.6))
 7 format(1x,i2,4x,i2,1x,4(1x,e13.6)/10x,4(1x,e13.6))
 8 format(/,'la valeur de la dérivée des fct. (u,v,wø,ny,nyx,my,',               &
            'ry,w,wøø,wøøø,uø,vø,vøø) aux points sont :'/                        &
            'fct  var.xi    sensa         sensb         sensc         sensd' /   &
            '               sensaa        sensbb        senscc        sensdd'/   &
            '               sensh (cas de charge nø1 à ',i2/)

16 format(/'la valeur des fct. (u,v,wø,ny,nyx,my,ry,w,wøø,wøøø,uø,',             &
           'vø,vøø) aux points sont :'/                                          &
           'pt.    fct      defa          defb         defc          defd '/     &
           '                defaa         defbb        defcc         defdd') 

return
end

!--------------------------------------------------------------------------------
!     subroutine ali2   (effet des traverses - calcul des sensibilités)
!--------------------------------------------------------------------------------

subroutine ali2(alix,aliz,disc,disb,abtr,mt,y1,q,pi1,pi2,l,id,id2,argq,dalix,daliz,dvarb,dvarc,darg,nbrxi,nxi,phil)


!     la subroutine calcule les 13 grandeurs utiles (ali)
!     (u,v,w,wø,my,ny,ry,nxy + wøø,wøøø,uø,vø et vøø)
!     sous l'action des effets concentres unitaires
!     de type xody et zody agissant au niveau des traverses. 
!
!     ces 13 grandeurs (u,v,w,..) sont calculées en 1 seul point y1 choisis
!     pour le calcul des sensibilités (voir boucle "do 10" ds subr sens).
!     soit alix(ms,13) val. des 13 grandeurs pour xo=1000 cos(lamb*x),
!          aliz(ms,13) val. des 13 grandeurs pour zo=1000 sin(lamb*x),
!                    avec xo et zo agissant au droit de la traverse i=1,mt.
!     soit dalix(ms,13,ivar) dérivées de alix pour ivar=1,9
!          daliz(ms,13,ivar) dérivées de aliz pour ivar=1,9
!                                                                       
!     variables:
!	         l =1 à m (nbre de racine à l'equ. différentielle) 
!	         y1=y *pi/180


implicit double precision(a-h,o-z)
dimension disc(720),disb(720),alix(10,13),aliz(10,13),abtr(10),  &
          id(13),id2(13),argq(8),dalix(10,13,9),daliz(10,13,9),  &
          dvarb(33,9,16),dvarc(33,9,16),darg(8,9),nxi(9)


l2=2*l
l1=l2-1

do i=1,mt
   xmt=abtr(i)*pi1
   dy1=dabs(xmt-y1)                                     ! iy-abtri
   if(((dy1/phil).lt.(0.000001)).or.(xmt.lt.y1)) then   ! si yòabtr
   sign=1.                                              !
   else                                                 !
   sign=-1.
   endif
   dy1q=q*dy1
   
   dy11=pi2-dy1                                 ! 2pi-(i y-abtr i)
   dy11q=q*dy11                                 !
   al1=argq(2*l-1)*dy1                          !
   be1=argq(2*l)  *dy1                          !
   al3=argq(2*l-1)*dy11                                  	
   be3=argq(2*l)  *dy11                              			
   al1=expo(al1)
   al3=expo(al3)
   cos1=al1*dcos(be1)
   sin1=al1*dsin(be1)
   cos3=al3*dcos(be3)
   sin3=al3*dsin(be3)
   
   jj=4*(l-1)

   do j=1,13
        
      kh=jj+(id(j)-1)*16
      sign2=sign                            !
      if(id2(j).eq.1) sign2=1.              ! si fct symétrique (id2=1)
      if(l.eq.1) alix(i,j)=0.               !
      if(l.eq.1) aliz(i,j)=0.               !
      
      alix(i,j)=alix(i,j)+ sign2* (disb(kh+1)*cos1+disb(kh+2)*sin1+  &
                                   disb(kh+3)*cos3+disb(kh+4)*sin3 )
      aliz(i,j)=aliz(i,j)+ sign2* (disc(kh+1)*cos1+disc(kh+2)*sin1+  &
                                   disc(kh+3)*cos3+disc(kh+4)*sin3 )
      
      do ivar=1,nbrxi
         k=nxi(ivar)
         
         calcx =                                                           &
         - dy1q * ( cos1* (darg(l1,k)*disb(kh+1)-darg(l2,k)*disb(kh+2) )   &
                   +sin1* (darg(l1,k)*disb(kh+2)+darg(l2,k)*disb(kh+1) ) ) &
         - dy11q* ( cos3* (darg(l1,k)*disb(kh+3)-darg(l2,k)*disb(kh+4) )   &
                   +sin3* (darg(l1,k)*disb(kh+4)+darg(l2,k)*disb(kh+3) ) ) &
         + cos1 * dvarb(j,k,jj+1)  + sin1 * dvarb(j,k,jj+2)                &
         + cos3 * dvarb(j,k,jj+3)  + sin3 * dvarb(j,k,jj+4)               
         calcz =                                                           &
         - dy1q * ( cos1* (darg(l1,k)*disc(kh+1)-darg(l2,k)*disc(kh+2) )   &
                   +sin1* (darg(l1,k)*disc(kh+2)+darg(l2,k)*disc(kh+1) ) ) &
         - dy11q* ( cos3* (darg(l1,k)*disc(kh+3)-darg(l2,k)*disc(kh+4) )   &
                   +sin3* (darg(l1,k)*disc(kh+4)+darg(l2,k)*disc(kh+3) ) ) &
         + cos1 * dvarc(j,k,jj+1)  + sin1 * dvarc(j,k,jj+2)                &
         + cos3 * dvarc(j,k,jj+3)  + sin3 * dvarc(j,k,jj+4)
         
         if(l.eq.1) dalix(i,j,k)=0.
         if(l.eq.1) daliz(i,j,k)=0.
         dalix(i,j,k)=dalix(i,j,k) + sign2 * calcx
         daliz(i,j,k)=daliz(i,j,k) + sign2 * calcz
         
      enddo
   enddo
enddo

return
end


!--------------------------------------------------------------------------------
!     subroutine pour les 4 cas de charge de base (disa, disb, etc.)
!       - calcul des déplacements crées par disa, disb, ..
!--------------------------------------------------------------------------------


subroutine def1(sin1,cos1,sin3,cos3,disx,defx,ipt,l,m,id,icas,mt,alix,aliz,hyp)

use param_section,ID_=>ID,ALIX_=>ALIX,ALIZ_=>ALIZ,HYP_=>HYP

implicit double precision(a-h,o-z)
!integer*4 nsol
dimension disx(720),defx(13,4),id(13),alix(10,13),aliz(10,13)
dimension hyp(2*mt_max*(8+nsolmax))

jj=(l-1)*4
do j=1,13
   ij=(id(j)-1)*16 + jj
   if(l.eq.1) defx(j,ipt)=0.
   defx(j,ipt)=defx(j,ipt)                              &
            + cos1 * disx(ij+1)  + sin1 * disx(ij+2)    &
            + cos3 * disx(ij+3)  + sin3 * disx(ij+4)
   
   if(l.eq.m) then
      select case(j)
         	     !fct symétrique
         	     case(1,4,6,8,9,12)
         	        if((icas.eq.4).or.(icas.eq.6))   defx(j,ipt)=-defx(j,ipt)
         		 !fct antimétrique
         	     case(2,3,5,7,10,11,13)
                     if((icas.eq.2).or.(icas.eq.8)) defx(j,ipt)=-defx(j,ipt)
         	     !case default
                  !  if((icas.eq.2).or.(icas.eq.8)) defx(j,ipt)=-defx(j,ipt)
         
      end select
   
   
   
      if(mt.ge.1) then
   
   
         do k=1,mt
            jmx=k+icas*20
            jmz=jmx+mt
            defx(j,ipt)=defx(j,ipt)+( alix(k,j) * hyp(jmx)  + aliz(k,j) * hyp(jmz) )/10000.
         enddo
   
      endif
   endif
   
enddo


return
end


!--------------------------------------------------------------------------------
!     subroutine pour les 4 cas de charge de base (disa, disb, etc.) 
!       - calcul des dérivées des déplacements crées par disa, disb, ..
!--------------------------------------------------------------------------------


subroutine sens1sr(y1q,y11q,sin1,cos1,sin3,cos3,disx,dvar,    &
                   sensx,icas,ipt,l,m,id,nxi,nbrxi,mt,        &
                   alix,aliz,dalix,daliz,dhyp,hyp,darg)

use param_section, ONLY : mt_max,nsolmax

implicit double precision(a-h,o-z)

dimension disx(720),dvar(33,9,16),sensx(13,4,9),id(13),       &
          nxi(9),alix(10,13),aliz(10,13),                     &
          dalix(10,13,9),daliz(10,13,9),                      &   
          dhyp(2*mt_max,(8+nsolmax),9),hyp(2*mt_max*(8+nsolmax)),darg(8,9)

l2=2*l
l1=l2-1
jj=(l-1)*4
icas1=icas+1


do j=1,13
   ij=(id(j)-1)*16 + jj


   do ivar=1,nbrxi
      
      
      k=nxi(ivar)
      if(l.eq.1) sensx(j,ipt,k)=0.
      
      
      sensx(j,ipt,k)=sensx(j,ipt,k)                                                    &
                     - y1q * ( cos1* (darg(l1,k)*disx(ij+1)-darg(l2,k)*disx(ij+2) )    &
                     +sin1* (darg(l1,k)*disx(ij+2)+darg(l2,k)*disx(ij+1) ) )           &
                     - y11q* ( cos3* (darg(l1,k)*disx(ij+3)-darg(l2,k)*disx(ij+4) )    &
                     +sin3* (darg(l1,k)*disx(ij+4)+darg(l2,k)*disx(ij+3) ) )           &
                     + cos1 * dvar(j,k,jj+1)  + sin1 * dvar(j,k,jj+2)                  &
                     + cos3 * dvar(j,k,jj+3)  + sin3 * dvar(j,k,jj+4)
      
      
      if(l.eq.m) then


            select case(j)
      	     !fct symétrique
      	     case(1,4,6,8,9,12)
      	        if((icas.eq.4).or.(icas.eq.6))   sensx(j,ipt,k)=-sensx(j,ipt,k)
      		 !fct antimétrique
      	     case(2,3,5,7,10,11,13)
                  if((icas.eq.2).or.(icas.eq.8)) sensx(j,ipt,k)=-sensx(j,ipt,k)
      	     !case default
               !   if((icas.eq.2).or.(icas.eq.8)) sensx(j,ipt,k)=-sensx(j,ipt,k)
      
            end select
      
      
            if(mt.ge.1) then

              do i=1,mt
                 jmx=i+icas*20
                 jmz=jmx+mt
              
                 sensx(j,ipt,k)=sensx(j,ipt,k) + (1./10000.) *                          &
                               (alix(i,j)*dhyp(i,icas1,k)+aliz(i,j)*dhyp(i+mt,icas1,k)  &
                               +hyp(jmx) *dalix(i,j,k)   +hyp(jmz) *daliz(i,j,k))
              
              enddo
            endif
      endif
      
      
   enddo
enddo



return
end


!--------------------------------------------------------------------------------
!     subroutine pour la charge extérieure (dish)
!--------------------------------------------------------------------------------
subroutine sens2sr(y1,y1q,y11q,y2q,y22q,sin1,cos1,sin2,cos2,sin3,         &
           cos3,sin4,cos4,cost,sint,dish,dvarh,sensh,ipt,l,m,id,nxi,nbrxi,&
           mt,alix,aliz,dalix,daliz,dhyp,is1,hyp,darg)

use param_section, ONLY : mt_max,nsolmax

implicit double precision(a-h,o-z)

dimension dish(1710),dvarh(33,9,38),sensh(13,4,9),id(13),                  &
          nxi(9),alix(10,13),aliz(10,13),dalix(10,13,9),daliz(10,13,9),    &
          dhyp(2*mt_max,(8+nsolmax),9),hyp(2*mt_max*(8+nsolmax)),darg(8,9)

l2=2*l
l1=l2-1
jj=(l-1)*8 + 6

do j=1,13
   ij=(id(j)-1)*38 + jj

   do ivar=1,nbrxi
      k=nxi(ivar)

      if(l.eq.1) sensh(j,ipt,k)=dvarh(j,k,1)+dvarh(j,k,2)*y1              &
     	                       +dvarh(j,k,3)*y1*y1+dvarh(j,k,4)*(y1**3)   &
                               +dvarh(j,k,5)*cost+dvarh(j,k,6)*sint

      sensh(j,ipt,k)=sensh(j,ipt,k)                                       &
      - y1q * ( cos1* (darg(l1,k)*dish(ij+1)-darg(l2,k)*dish(ij+2) )      &
               +sin1* (darg(l2,k)*dish(ij+1)+darg(l1,k)*dish(ij+2) ) )    &
      - y2q * ( cos2* (darg(l1,k)*dish(ij+3)-darg(l2,k)*dish(ij+4) )      &
               +sin2* (darg(l2,k)*dish(ij+3)+darg(l1,k)*dish(ij+4) ) )    &
      - y11q* ( cos3* (darg(l1,k)*dish(ij+5)-darg(l2,k)*dish(ij+6) )      &
               +sin3* (darg(l2,k)*dish(ij+5)+darg(l1,k)*dish(ij+6) ) )    &
      - y22q* ( cos4* (darg(l1,k)*dish(ij+7)-darg(l2,k)*dish(ij+8) )      &
               +sin4* (darg(l2,k)*dish(ij+7)+darg(l1,k)*dish(ij+8) ) )    &
      + cos1 * dvarh(j,k,jj+1)  + sin1 * dvarh(j,k,jj+2)                  &
      + cos2 * dvarh(j,k,jj+3)  + sin2 * dvarh(j,k,jj+4)                  &
      + cos3 * dvarh(j,k,jj+5)  + sin3 * dvarh(j,k,jj+6)                  &
      + cos4 * dvarh(j,k,jj+7)  + sin4 * dvarh(j,k,jj+8)

      if((l.eq.m).and.(mt.ge.1)) then

         do i=1,mt
            i1=i+(is1-1)*20
            sensh(j,ipt,k)=sensh(j,ipt,k) +  (1./10000.) *                &
                ( alix(i,j)*dhyp(i,is1,k) + aliz(i,j) *dhyp(i+mt,is1,k)   &
                + hyp(i1)  *dalix(i,j,k)  + hyp(i1+mt)*daliz(i,j,k)  )
            
         enddo
      endif



   enddo
enddo



return
end
