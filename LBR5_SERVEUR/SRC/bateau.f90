subroutine bateau(xneu,yneu)

use param_section

implicit double precision (a-h,o-z)
dimension sol (360) !avant 20 : c'est trop TODO : allouer d'une meilleure façon
dimension valf(11)
dimension zb  (360)
dimension b1  (400)

double precision, allocatable, save :: a1(:,:)

allocate (a1(8,neto))

! ====================================================================================================
!
!    calcul de la repartition des forces de bord sur les extremites.
!    ces forces créent aux deux extrémités des moments (bm1,bm3) et (bm2,bm4)             
!    qui correspondent à ceux induit par la proue et la poupe.
!
!    donnees : bm1,bm3,bm2,bm4                                                                        
!       mx: positif si le pont sup. est en traction et le fond en compression = hogging
!       my: positif si le vecteur moment est dirigé vers le bas                                  
!
!       on ne peut pas considérer un moment autour d'un axe de symétrie!
!
!    resultat: a1(8,neto)
!              abcd(8) vect solution pour le sauvetage des résultats
!                      via  write(iu_21(iboat)) abcd
!
!    vecteur de travail sol(20)
!
! ====================================================================================================

call annuld(zb,360)
sol(:)=0.d00 !call annuld(sol,20)
call annuld(a1,8*neto)
call annuld(b1,400)

ftmomyx=0.
ftmomxx=0.
forctx=0.
ftmomxy=0.
ftmomyy=0.
forcty=0.                                                                  

!
! ===main loop 10 forces de bord créées par un moment autour de x et autour de y===      

do 10 i=1,neto
if(itype(i).eq.5) goto 10


 phi=-panneau(i).phil
 q=panneau(i).q


phi2=phi/3.
phi3=2.*phi2
sph1=vsin(phi2/2.,0.d00)
sph2=vsin(phi3/2.,0.d00)


 cx1=vcos(panneau(i).tetas,phi2/2.)
 cx2=vcos(panneau(i).tetas,phi3/2.)
 cy1=vsin(panneau(i).tetas,phi2/2.)
 cy2=vsin(panneau(i).tetas,phi3/2.)                


zb(1)=panneau(i).delt *( z(i,3)-yneu   )
zb(2)=panneau(i).delt *( z(i,3)-q*2.*cx1*sph1-yneu   )       
zb(3)=panneau(i).delt *( z(i,3)-q*2.*cx2*sph2-yneu   )      
zb(4)=panneau(i).delt *( z(i,4)-yneu   )

qphi=phi*q*pi/180.d00
qphi2=qphi*qphi
qphi3=qphi2*qphi

! a l'origine y=0

b1(1)=0.
b1(21)=0.
b1(41)=0.
b1(61)=1.

! au point y=1/3 de phi

b1(2)=qphi3/27.
b1(22)=qphi2/9.
b1(42)=qphi/3.
b1(62)=1.

! au point y=2/3 de phi

b1(3)=8.*qphi3/27.
b1(23)=4.*qphi2/9.
b1(43)=2.*qphi/3.
b1(63)=1.

! au point y = phi

b1(4)=qphi3
b1(24)=qphi2
b1(44)=qphi
b1(64)=1.


 call sysmat(b1,zb,4,1,sol,20)

do j=1,4
   a1(j,i)=sol(j)
end do

zb(1)=panneau(i).delt *( z(i,1)-xneu   )                          
zb(2)=panneau(i).delt *( z(i,1)+q*2.*cy1*sph1-xneu   )            
zb(3)=panneau(i).delt *( z(i,1)+q*2.*cy2*sph2-xneu   )            
zb(4)=panneau(i).delt *( z(i,2)-xneu   )                          

!     a1 l'origine y=0

b1(1)=0.
b1(21)=0.
b1(41)=0.
b1(61)=1.

!     au point y=1/3 de phi

b1(2)=qphi3/27.
b1(22)=qphi2/9.
b1(42)=qphi/3.
b1(62)=1.

!     au point y=2/3 de phi

b1(3)=8.*qphi3/27.
b1(23)=4.*qphi2/9.
b1(43)=2.*qphi/3.
b1(63)=1.

!     au point y = phi

b1(4)=qphi3
b1(24)=qphi2
b1(44)=qphi
b1(64)=1.                            



 call sysmat(b1,zb,4,1,sol,20)

! sol contient les coefficients a,b,c,d 
!(forces de bord pour un moment autour de y)

do j=1,4
   a1(j+4,i)=sol(j)                                                   
end do

!     fmomyx moment autour de l'axe vert. y du aux forces de bord creees par un moment autour de x
!     fmomxx moment autour de l'axe horiz.x du aux forces de bord creees par un moment autour de x
!     forcx la somme selon ox des forces de bord creees par un moment autour de x(forctx la somme de forcx)

!     fmomyy moment autour de l'axe vert. y du aux forces de bord creees par un moment autour de y
!     fmomxy moment autour de l'axe horiz.x du aux forces de bord creees par un moment autour de y
!     forcy la somme selon ox des forces de bord creees par un moment autour de y(forcty la somme de forcy)

!     ftmomxx,ftmomyx les sommes des moments fmomxx et fmomyx
!     ftmomxy,ftmomyx les sommes des moments fmomxy et fmomyx                   

!     les axes consideres etant ceux au centre de gravite.


 phil=dabs(panneau(i).phil)

qph=q*phil*pi/180.d00
cc1=vcos(panneau(i).tetas,0.d00)
cc2=vcos(panneau(i).tetas,phil)
cc3=vcos(panneau(i).tetas,phil/2.)
 s1=vsin(panneau(i).tetas,0.d00)
 s2=vsin(panneau(i).tetas,phil)
 s3=vsin(panneau(i).tetas,phil/2.)



!     forces
!     ======

forcx=( a1(1,i)*qph**3/4.+a1(2,i)*qph*qph/3. &
       +a1(3,i)*qph/2.+a1(4,i))*qph
forcy=( a1(5,i)*qph**3/4.+a1(6,i)*qph*qph/3. &
       +a1(7,i)*qph/2.+a1(8,i))*qph

if(isymx.eq.0.and.isymy.eq.0) then
  forctx=forctx+forcx
  forcty=forcty+forcy
else
  if(isymx.eq.0.and.isymy.eq.1) forctx=forctx+2*forcx
  if(isymx.eq.1.and.isymy.eq.0) forcty=forcty+2*forcy
endif                                                                                          


!     moments
!     =======                                                                                                                                  bba01030
 if (phil.ge.1.e-05) then
   termx1=(a1(1,i)*qph**2+a1(2,i)*qph+a1(3,i)-q*q*6.*a1(1,i))*qph
   termx2=(3.*a1(1,i)*qph+2.*a1(2,i))*qph
   termx3=q* (-6.*a1(1,i)*q*q+a1(3,i))
   termx4=-2.*a1(2,i)*q*q+a1(4,i)
   termy1=(a1(5,i)*qph**2+a1(6,i)*qph+a1(7,i)-q*q*6.*a1(5,i))*qph
   termy2=(3.*a1(5,i)*qph+2.*a1(6,i))*qph
   termy3=q* (-6.*a1(5,i)*q*q+a1(7,i))
   termy4=-2.*a1(6,i)*q*q+a1(8,i)

   fmomyx=-forcx * (z(i,1)-xneu+q*cc1)
   fmomyx=fmomyx+q*q*  (s2*termx1+q*cc2*termx2-2.*sph1*s3*termx3+ &
                        2.*sph1 *cc3*termx4)

   fmomxx= - forcx * (z(i,3)-yneu+q*s1)
   fmomxx=fmomxx+q*q* (-cc2*termx1+q*s2*termx2+2.*sph1*s3*termx4+ &
                         2.*sph1*cc3*termx3)

   fmomyy=-forcy * (z(i,1)-xneu+q*cc1)
   fmomyy=fmomyy+q*q*  (s2*termy1+q*cc2*termy2-2.*sph1*s3*termy3+ &
                        2.*sph1 *cc3*termy4)

   fmomxy= - forcy * (z(i,3)-yneu+q*s1)
   fmomxy=fmomxy+q*q* (-cc2*termy1+q*s2*termy2+2.*sph1*s3*termy4+ &
                         2.*sph1*cc3*termy3)

 else
   tempx=qph*qph* (a1(1,i)*  qph**3 /5. + a1(2,i)  *qph**2 /4. &
                  +a1(3,i)*  qph /3.    + 0.5 * a1(4,i) )
   tempy=qph*qph* (a1(5,i)*  qph**3 /5. + a1(6,i)  *qph**2 /4. &
                  +a1(7,i)*  qph /3.    + 0.5 * a1(8,i) )

    fmomyx=-(z(i,1)-xneu) * forcx  -  tempx * s1
    fmomxx=-(z(i,3)-yneu) * forcx  +  tempx * cc1
    fmomyy=-(z(i,1)-xneu) * forcy  -  tempy * s1
    fmomxy=-(z(i,3)-yneu) * forcy  +  tempy * cc1
 endif

 if(isymx.eq.1.or.isymy.eq.1) then
   if(isymx.eq.1.and.isymy.eq.1) then
     ftmomxx=ftmomxx+4*fmomxx
     ftmomyy=ftmomyy+4*fmomyy
   else
     ftmomxx=ftmomxx+2*fmomxx
     ftmomyy=ftmomyy+2*fmomyy
   endif
 else
   ftmomxx=ftmomxx+fmomxx
   ftmomyx=ftmomyx+fmomyx
   ftmomyy=ftmomyy+fmomyy
   ftmomxy=ftmomxy+fmomxy
 endif

valf(1)=a1(4,i)

call pval(a1(1,i),a1(2,i),a1(3,i),a1(4,i),phil,q,0.1d+00,valf(2))
call pval(a1(1,i),a1(2,i),a1(3,i),a1(4,i),phil,q,0.2d+00,valf(3))
call pval(a1(1,i),a1(2,i),a1(3,i),a1(4,i),phil,q,0.3d+00,valf(4))
call pval(a1(1,i),a1(2,i),a1(3,i),a1(4,i),phil,q,0.4d+00,valf(5))
call pval(a1(1,i),a1(2,i),a1(3,i),a1(4,i),phil,q,0.5d+00,valf(6))
call pval(a1(1,i),a1(2,i),a1(3,i),a1(4,i),phil,q,0.6d+00,valf(7))
call pval(a1(1,i),a1(2,i),a1(3,i),a1(4,i),phil,q,0.7d+00,valf(8))
call pval(a1(1,i),a1(2,i),a1(3,i),a1(4,i),phil,q,0.8d+00,valf(9))
call pval(a1(1,i),a1(2,i),a1(3,i),a1(4,i),phil,q,0.9d+00,valf(10))
call pval(a1(1,i),a1(2,i),a1(3,i),a1(4,i),phil,q,1.0d+00,valf(11))                    

!     les impressions
!     ===============

valf(1)=a1(8,i)
call pval(a1(5,i),a1(6,i),a1(7,i),a1(8,i),phil,q,0.1d+00,valf(2))
call pval(a1(5,i),a1(6,i),a1(7,i),a1(8,i),phil,q,0.2d+00,valf(3))
call pval(a1(5,i),a1(6,i),a1(7,i),a1(8,i),phil,q,0.3d+00,valf(4))
call pval(a1(5,i),a1(6,i),a1(7,i),a1(8,i),phil,q,0.4d+00,valf(5))
call pval(a1(5,i),a1(6,i),a1(7,i),a1(8,i),phil,q,0.5d+00,valf(6))
call pval(a1(5,i),a1(6,i),a1(7,i),a1(8,i),phil,q,0.6d+00,valf(7))
call pval(a1(5,i),a1(6,i),a1(7,i),a1(8,i),phil,q,0.7d+00,valf(8))
call pval(a1(5,i),a1(6,i),a1(7,i),a1(8,i),phil,q,0.8d+00,valf(9))
call pval(a1(5,i),a1(6,i),a1(7,i),a1(8,i),phil,q,0.9d+00,valf(10))
call pval(a1(5,i),a1(6,i),a1(7,i),a1(8,i),phil,q,1.0d+00,valf(11))

 10   continue
!
! =====end of loop 10 ==================================================
!
if(impr2.ge.-1) then			
      write(iu_11(iboat),102)ftmomyx
      write(iu_11(iboat),103)ftmomxx
      write(iu_11(iboat),104)forctx
      write(iu_11(iboat),105)ftmomyy
      write(iu_11(iboat),106)ftmomxy
      write(iu_11(iboat),107)forcty			
endif

!   calcul des coéfficients pour chaque cas de charges et sauvetage des résultats
!   (pour subr. mom dans bo1) sur un fichier temporaire (file iu_21(iboat))
!    -------------------------------------------------

   do 15 is=1,nsol                                                                           

     b1(1)=ftmomxx
     b1(21)=ftmomxy
     b1(2)=ftmomyx
     b1(22)=ftmomyy
     zb(1)=bm1(is)
     zb(2)=bm3(is)
     call sysmat(b1,zb,2,1,sol,20)
     coefx1=sol(1)
     coefy1=sol(2)                                 

     b1(1)=ftmomxx
     b1(21)=ftmomxy
     b1(2)=ftmomyx
     b1(22)=ftmomyy
     zb(1)=bm2(is)
     zb(2)=bm4(is)
     call sysmat(b1,zb,2,1,sol,20)
     coefx2=sol(1)
     coefy2=sol(2)                                      

     do 15 i=1,neto

       if(itype(i).eq.5) goto 15  
       if(impr2.ge.-1) then		                                                                     
         write(iu_11(iboat),*)
         write(iu_11(iboat),'(a,i3)') 'panneau nø ',i
         write(iu_11(iboat),'(a,a)')'cas de charge :  abcd(1à4) for mx1 & my1'	&  
                                       ,'et abcd(5à8) for mx2 & my2'	
         
	   endif

	   do j=1,4
	      abcd(j)=a1(j,i)*coefx1+a1(j+4,i)*coefy1
		  abcd(j+4)=a1(j,i)*coefx2+a1(j+4,i)*coefy2
	   enddo                                                   

!       if(impr2.ge.-1) then
	   write(iu_11(iboat),'(t4,i2,a4,4(1x,e11.4),a4,4(1x,e11.4))') &
          is,' : ',(abcd(k),k=1,4),'   ',(abcd(k),k=5,8)

!       endif						
	   write(iu_21(iboat)) abcd
	   ! sauvetage sur file 97 des abcd (pour lecture dans bo1)

  15  continue


deallocate(a1)


!     les formats
!    -------------------------------------------------
   99 format(t2,'panneau',i2/t2,10(1h-))
  100 format(/t2,'valeurs en 11 pts , y=0,...,y=q*phil',           &
     ' des forces de bord creees par un moment autour de x'/       &
      t2,11(e10.3,1x)/                                             &
      t2,'moment d''axe vert.y du a ces forces  =',e11.4,' n.m'/   &
      t2,'moment d''axe horiz.x du a ces forces  =',e11.4,' n.m'/  &
      t2,'force resultante selon ox ',e11.4,' n. '/)

  101 format(t2,'valeurs en 11 pts , y=0, ,  ...,y=q*phil',        &
     ' des forces de bord creees par un moment autour de y'/       &
     t2,11(e10.3,1x)/                                              &
     t2,'moment d''axe vert.y du a ces forces =',e11.4,' n.m'/     &
     t2,'moment d''axe horiz.x du a ces forces  =',e11.4,' n.m'/   &
     t2,'force resultante selon ox ',e11.4,' n. '/) 

  102 format(t2,'moment general autour de l''axe vert.y pour tous les ',  &
     'panneaux du aux forces de bord creees par un moment autour de x'/   &
     t2,116(1h=)/t2,' moment myx total = ',e11.4,' n.m'/)

  103 format(t2,'moment general autour de l''axe horiz.x pour tous les    &
     ','panneaux du aux forces de bord creees par un moment autour de x'/ &
     t2,116(1h=)/t2,' moment mxx total = ',e11.4,' n.m'/)

  104 format(t2,'somme des forces selon ox pour tous les panneaux ',      &
     'du aux forces de bord creees par un moment autour de x'/            &
     t2,102(1h=)/t2,'force totale generale =',e11.4,' n.'/)

  105 format(t2,'moment general autour de l''axe vert.y pour tous les ',  &
     'panneaux du aux forces de bord creees par un moment autour de y'/   &
     t2,116(1h=)/t2,' moment myx total = ',e11.4,' n.m'/)

  106 format(t2,'moment general autour de l''axe horiz.x pour tous les ', &
     'panneaux du aux forces de bord creees par un moment autour de y'/   &
     t2,116(1h=)/t2,' moment mxx total = ',e11.4,' n.m'/)

  107 format(t2,'somme des forces selon ox pour tous les panneaux ',      &
     'du aux forces de bord creees par un moment autour de y'/            &
     t2,102(1h=)/t2,'force totale generale =',e11.4,' n.'/)                                  

      return
      end
!
! ====================================================================================================
!
      subroutine pval(a,b,c,d,ph,q,rap,val)
      implicit double precision(a-h,o-z)
      pi=2.d00*acos(0.d00)
      qph=q*ph*rap*pi/180.d00
      val=a*qph*qph*qph+b*qph*qph+c*qph+d
      return
      end
