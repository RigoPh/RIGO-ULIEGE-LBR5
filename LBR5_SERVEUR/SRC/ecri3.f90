subroutine ecri3(dis,nel,ns,eff2,mt,uecr)

use param_section, dis_=>dis,uecr_=>uecr,B_=>B,NS_=>NS

implicit double precision(a-h,o-z)
dimension dis(5),eff2(2040),uecr(51)
character*40 b(8),bb(8)

!-----------------------------------------------------------------------
!
!     subroutine ecri3 (eff2)
!     impression des contraintes dans le bordage au niveau des faces du borde
!     z=+delta/2 et z=-delta/2.
!
!  créé    :  7-9-93   ph. rigo
!  modifié :  8-5-95   mod. d'optimisation: subr. extrem2
!            15-12-2000 version anglaise
!
!-----------------------------------------------------------------------

pi=2.d00*asin(1.d00)

if(langue==1) then
   data b/'contr. transv.  sy  bordé (z=+d/2)',    &
          'contr. longit.  sx  bordé (z=+d/2)',    &
          'contr. cisaill. txy bordé (z=+d/2)',    &
          'contr. de von mises, bordé (z=+d/2)',   &
          'contr. transv.  sy  bordé (z=-d/2)',    &
          'contr. longit.  sx  bordé (z=-d/2)',    &
          'contr. cisaill. txy bordé (z=-d/2)',    &
          'contr. de von mises, bordé (z=-d/2)'/
	else

   data bb/'transv. stress sy plate (z=+d/2)',     &
          'longit. stress sx plate (z=+d/2)',      &
          'shear stress, txy plate (z=+d/2)',      &
          'von mises stress, plate (z=+d/2)',      &
          'transv. stress sy plate (z=-d/2)',      &
          'longit. stress sx plate (z=-d/2)',      &
          'shear stress, txy plate (z=-d/2)',      &
          'von mises stress, plate (z=-d/2)'/
	endif


if(langue==1) then
   write(iu_11(iboat),10)nel
10 format(/5x,'résultats complémentaires - panneau ',i3,/5x, 45(1h*)/)

else
   write(iu_11(iboat),11)nel
11 format(/5x,'additional results - panel ',i3,/5x, 45(1h*)/)
   do i=1,8
	    b(i)=bb(i)
	 enddo
endif

kh=2*mt+31
l=0

do kr=1,8
   l=l+1
   write(iu_11(iboat),50) b(l)
50 format(/t15,a41/t15,40(1h+))
   write(iu_11(iboat),156) dis


   if(ns.eq.1) then

      do i=1,kh
         jm=(l-1)*255+i
         ij=jm+204
         write(iu_11(iboat),158) uecr(i),(eff2(k),k=jm,ij,51)
      enddo

   else

      goto(2,2,3,2,2,2,3,2),l
3     jm=(l-1)*255+1
      ij=l*255

      do 4 kk=jm,ij
4     eff2(kk)=-eff2(kk)

2     do i=1,31
         jm=(l-1)*255+32-i
         ij=jm+204
         write(iu_11(iboat),158) uecr(i),(eff2(k),k=jm,ij,51)
      enddo

      do i=32,kh
         jm=(l-1)*255+(kh+32-i)
         ij=jm+204
         utrav=uecr(31)-uecr(kh+32-i)
         write(iu_11(iboat),158) utrav,(eff2(k),k=jm,ij,51)
      enddo
         
   endif
call extre2(l,kh,eff2,b)

enddo

write(iu_27(iboat),*) '88'					

156 format(/10x,5(4x,'x=',f8.3,' m')/)
158 format(' y=',f6.3,'m',5(1x,e13.6))
return
end

!***********************************************************************
!***********************************************************************

subroutine extre2(l,kh,eff2,a)

use param_section, ONLY : iu_11,iu_27,iboat,langue

implicit double precision(a-h,o-z)
dimension eff2(2040)
character *40 a(8)

!***********************************************************************
!
!     subroutine extre2
!     ++++++++++++++++++
!     recherche les valeurs extrêmes (max et min)
!
!     modif : nov 2000                                 créé :  5-5-95
!
!***********************************************************************

vmin=0.
vmax=0.
ixmin=0
ixmax=0
iymin=0
iymax=0
i1=(l-1)*255

do kx=1,5
   i2=i1+(kx-1)*51
   do ky=1,kh
      k1=i2+ky
      if(vmin.gt.eff2(k1)) then
           vmin=eff2(k1)
          ixmin=kx
          iymin=ky
      endif
      if(vmax.lt.eff2(k1)) then
          vmax=eff2(k1)
          ixmax=kx
          iymax=ky
      endif
   enddo
enddo

if(langue==1) write(iu_11(iboat),102) vmin,ixmin,iymin,vmax,ixmax,iymax
if(langue==2) write(iu_11(iboat),100) vmin,ixmin,iymin,vmax,ixmax,iymax

! sauvetage pour le module d'analyse des extrêmes (subr. analys)
if(l.eq.4) goto 101
if(l.eq.8) goto 101
goto 999
101 continue
write(iu_27(iboat),50) l,a(l)							
write(iu_27(iboat),*) vmin,ixmin,iymin,vmax,ixmax,iymax	

999 return

50  format(i2,t15,40a)
100 format(/' min. value =',e11.4,'   in (x,y)=(',i2,' , ',i2,')'/  &
            ' max. value =',e11.4,'   in (x,y)=(',i2,' , ',i2,')')  
102 format(/' valeur min =',e11.4,'   en (x,y)=(',i2,' , ',i2,')'/  &
            ' valeur max =',e11.4,'   en (x,y)=(',i2,' , ',i2,')')  
return
end
