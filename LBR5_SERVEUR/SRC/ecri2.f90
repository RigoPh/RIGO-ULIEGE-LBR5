subroutine ecri2(dis,nel,eff,ns,q,mt,uecr)

use param_section,DIS_=>DIS,UECR_=>UECR,A_=>A,B_=>B,E_=>E,NS_=>NS

implicit double precision(a-h,o-z)
dimension dis(5),eff(9690),uecr(51)
character*50 a(3),b(3)
data a/'v (m) - deplacement transversal selon oy (z=0)',   &	
       'u (m) - deplacement longitudinal selon ox (z=0)',  &
       'w (m) - fleche, deplacement selon oz (z=0)'/			
data b/'v (m) - transv. displ. along oy (z=0)',		       &		
       'u (m) - long. displ. along ox (z=0)',              &
       'w (m) - lateral deflection along oz(z=0)'/		

!***********************************************************************
!     20-1-94
!
!     subroutine ecrit(forme réduite)
!     subroutine de mise en forme des resultats pour ecriture dans un fichier
!
!     dernière modif: nov. 2000
!***********************************************************************

pi=2.d00*asin(1.d00)
if(langue.ne.1) then
   do i=1,3
      a(i)=b(i)
	 enddo
endif

kh=2*mt+31
do 1 i=1,kh
  1   uecr(i)=uecr(i)*q*pi/180.
write(iu_11(iboat),150)nel
l=0
do l=1,3
   write(iu_11(iboat),50) a(l)
50 format(/t15,a50/t15,40(1h+))
   write(iu_11(iboat),156) dis
   if(ns.eq.1) then
      do 163 i=1,kh
         jm=(l-1)*255+i
         ij=jm+204
163   write(iu_11(iboat),158) uecr(i),(eff(k),k=jm,ij,51)
   else
      do 159 i=1,31
         jm=(l-1)*255+31-i
         ij=jm+204
159      write(iu_11(iboat),158) uecr(31-i),(eff(k),k=jm,ij,51)
      do 161 i=32,kh
         jm=(l-1)*255+(kh+32-i)
         ij=jm+204
161      write(iu_11(iboat),158) uecr(kh+32-i),(eff(k),k=jm,ij,51)
   endif
	 call extre5(l,kh,eff,a)			
201 write(iu_27(iboat),*) '99'			

enddo

150 format(t3,'panneau-panel no ',i3,/t3,18(1h+))
156 format(/10x,5(4x,'x=',f8.3,' m')/)
158 format(' y=',f6.3,'m',5(1x,e13.6))

return
end
!-----------------------------------------------------------------------
subroutine extre5(l,kh,eff,a) 

use param_section, ONLY : iu_11,iu_27,iboat,langue

implicit double precision(a-h,o-z)
dimension eff(9690)
character *50 a(3)

!
!-----------------------------------------------------------------------
!
!     subroutine extre5
!     ++++++++++++++++++
!     recherche les valeurs extrêmes (max et min)
!
!     modif : nov 2000                                    créé : 13-3-95
!
!-----------------------------------------------------------------------

vmin=0.
vmax=0.
ixmin=0
ixmax=0
iymin=0
iymax=0
i1=(l-1)*255
!sinon, on ne détecte pas le min s'il est positif
vmin=eff(i1+1) 
!sinon, on ne détecte pas le max s'il est négatif 
vmax=eff(i1+1) 
      
do kx=1,5
   i2=i1+(kx-1)*51
   do ky=1,kh
      k1=i2+ky
      if(vmin.ge.eff(k1)) then   
              vmin=eff(k1)
             ixmin=kx
             iymin=ky
      endif
      if(vmax.le.eff(k1)) then           
             vmax=eff(k1)
             ixmax=kx
             iymax=ky
      endif
   enddo
enddo


if(langue==1) write(iu_11(iboat),102) vmin,ixmin,iymin,vmax,ixmax,iymax
if(langue==2) write(iu_11(iboat),100) vmin,ixmin,iymin,vmax,ixmax,iymax


! sauvetage pour le module d'analyse des extrêmes (subr. analys)
write(iu_27(iboat),50) l,a(l)
write(iu_27(iboat),*) vmin,ixmin,iymin,vmax,ixmax,iymax	


return

50  format(i2,t15,50a)
100 format(/' min. value =',e11.4,'   in (x,y)=(',i2,' , ',i2,')'/ &
            ' max. value =',e11.4,'   in (x,y)=(',i2,' , ',i2,')')
102 format(/' valeur min =',e11.4,'   en (x,y)=(',i2,' , ',i2,')'/  &
            ' valeur max =',e11.4,'   en (x,y)=(',i2,' , ',i2,')')
110 format(/' min. value =',e11.4,'   in (x,y)=(',i2,' , ',i2,') with   &
              local flexion of girders'/                                &          
            ' max. value =',e11.4,'   in (x,y)=(',i2,' , ',i2,') with   &
              local flexion of girders')                                        
112 format(/' valeur min =',e11.4,'   en (x,y)=(',i2,' , ',i2,') avec   &
              flexion locale des raidisseurs'/                          &        
            ' valeur max =',e11.4,'   en (x,y)=(',i2,' , ',i2,') avec   &
              flexion locale des raidisseurs')                                  

return
end
