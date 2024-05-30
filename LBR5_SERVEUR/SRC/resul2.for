      subroutine resul2(wya,tya,dya,hya,ksa,wxr,txr,dxr,hxr,ksr,
     *                  aire,aiy,lamb,e1,nz,mt,kst,
     *                  tork,isect1,epais,
     *                  jjk,nel,
     *                  eff,temp,temp2,temp3,conc,z2)

      use param_section, TEMP_=>TEMP

      implicit double precision(a-h,o-z)
      double precision lamb,lamb2
!      character *18 nom3(11)
	dimension eff(9690),temp(1530),temp2(1530),temp3(300),
     *          conc(750),z2(2295)

c*******************************************************************************
c	ordre ds sens1(jj,4,9,neto) avec jj=1 à 9
c     data nom/'w','wø','wøø','wøøø','u','uø','v','vø ','vøø'/
c		      1   2    3     4      5   6    7   8     9 
c	ordre ds sens2(j,iptmax=4,9,neto) avec j=1 à 15
!	data nom3/'same jas sup i' ,'same jas inf i' ,'same an i' ,
!     *			'ssemel sup i' ,'ssemel inf i' ,'sy sup cercle/carre',
!     *			'sy inf cercle/carre' ,'sy an cercle/carre' ,
!     *			'ttrav jas x=0' ,'ttrav jab x=0'   ,'strav sem x=l/2'/
c	ordre ds sens3(jj,4,9,neto) avec jj=1 à 12
       ! 1 et 2  dsy=d(sy/dx) sup et inf dans l'âme ou dans cercle/carré
       ! 3 et 4  dtyx=d(tyx/dx) a.n. et jas pour double t et sommet et a.n. pour cercle/carré
       ! 5 et 6  dsy=d(sy/dx) sup et inf de semelle de double t 
       ! 7	   dtyx=d(tyx/dx) semelle de double t
       !8 et9  dtxy(1) et dtxy(2)  jas trav en x=0 
       !10 et11  dtxy(1) et dtxy(2)  jab trav en x=0 
       !12       d(sx moyen) pour la restriction de paik. 
c*******************************************************************************
c     subroutine result2 
c     ===================
c     subroutine de mise en forme des resultats pour 5 points de l'axe  
c     des x a partir du vecteur z2,resultat du calcul sur les 31+20 de  
c     l'axe des y(phi).                                                 
c     les resultats sont contenus dans les vecteurs eff,eff2 et conc 
c                                                                       
c    dernière modification : j.pirard
c	- contraintes dans les épontilles	!22-01-04
			 
c    modif :22-01-04 				      créer : thèse de doctorat	
c                                                                       
c*******************************************************************************

      lamb2=lamb*lamb
	delta=0.0000
	sh=0.0000
c cadres/aiguilles
      ksaa=(-1)**ksa
      tm=2*hya*dya						!aire âme
      tmr=hya*dya							!aire demi-âme
	tms=wya*tya							!aire semelle
	sm=hya+ tya							!hauteur hors-tout de demi section
	sm2=(hya/2)				        	!distance demi-ame
	smc2=(hya+tya/2.)		    		!distance à an			 
	smd2=hya		   		        	!distance jas à an
	sma=hya*dya*dya*dya
	tpb2=tms*smc2						!moment statique semelle
	tpr2=tpb2*tmr*sm2					!moment statique section
	rex=hya/2							!rayon ext du cercle
	rin=(hya-2*epais)/2					!rayon int du cercle
	rtan=(rex*rex*rex-rin*rin*rin)		!diff rayon pour ta.n.
	cm=hya-epais						!coté milieu du carre
	cin=hya-2*epais						!coté int du carre
	s=(cm*cm)-(cin*cin)					!aire int à ligne médiane du carre
	s2=pi*(((cm*cm)/4)-(cin*cin/4))	    !aire int à ligne médiane du cercle
	asc=((3*hya*hya)+(4*epais*epais)-(6*hya*epais))/8	!moment statique carre max/epais
	asc2= hya*(hya-epais)/4						!moment statique carre 45/epais
c raidisseurs
      ksrr=(-1)**ksr
      tmrx=wxr*txr
      tmrr=tmrx+dxr*hxr
      smr2=(hxr+txr)*ksrr
      smar2=smr2-txr*ksrr
      smbr=(delta/2.)*ksrr
      smcr2=(hxr+txr/2.)*ksrr
      smdr2=(hxr/2.)*ksrr
      tpbr2=tmrx*smcr2
      tprr2=tpbr2+dxr*hxr*smdr2
		
c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                                       pre00340
c                                                                       
c     calcul des deformations u,v,w et contraintes (eff, eff2, conc)                                                                    
c                                                                       
c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                                       pre00340
c
c    ordre des variables classées dans eff (1 à 38)
c    --------------------------------------------------
c       1       v               l=2     u                l=3    w
c       4       dw/dy             5     nx                 6    ny
c       7       nxy               8     nyx                9    mx
c      10       my               11     mxy               12    myx
c      13       qx               14     qy                15    ry
c      16       rx

c      17	sig y bordage	 18	sig x bordage	  19	tau xy bordage
c      20	sig comp bordage

c      21	sig y jas (aig)	 22   sig y jab (aig)	  23	tau jas (aig) 
c      24	tau jab   (aig)	 25   sig comp jas (aig)  26	sig comp jab (aig)
c      27	sig y sem (aig)	 28   tau sem   (aig)     29    sig comp sem (aig)

c      30 sig x jas (raid) 	  31   sig x jab (raid)	    32	sig x sem (raid)  
c      33 tau   jas (raid) 	  34   tau jab (raid)       35	tau sem (raid)   
c      36 sig comp jas (raid)   37   sig comp jab (raid)  38  sig comp sem (raid)

c    ordre des variables classées dans eff2 (1 à 8) 
c    --------------------------------------------------
c      1	sig y bord.(z=+d/2)  2	sig x bord.(z=+d/2)  3	tau xy bord.(z=+d/2) 
c      4	sig comp bord.(z=+d/2) 
c      5	sig y bord.(z=-d/2)  6	sig x bord.(z=-d/2)  7	tau xy bord.(z=-d/2)
c      8	sig comp bord.(z=-d/2)

c    ordre des variables classées dans conc (1 à 8) 
c    --------------------------------------------------
c       1 nx    2 nxy    3 mx    4 mxy    5 qx    6 rx
c       7	sig x sem       8	sig x jas       9	sig x jab 
c      10	tyx   sem      11	txz   jas      12	txz   jab
c      13	sig comp sem   14	sig comp jas   15	sig comp jab

c    vecteur z2:contient 51 valeurs par variables (31 + 20 au droit des traverses)
c    --------------------------------------------------
c     z2(  0+1 à   51) : v     z2(153+1  à  204) : xo(1)        
c     z2( 51+1 à  102) : u     z2(204+1  à  255) : zo(1)      z2(1479+1 à  1530) : v°  
c     z2(102+1 à  153) : w     z2(255+1  à  306) : xo(2)      z2(1530+1 à  1581) : u°  
c                              z2(306+1  à  357) : zo(2)      z2(1581+1 à  1632) : w°  
c                              ... etc.                       z2(1632+1 à  1683) : v°°  
c                              z2(1071+1 à 1122) : xo(10)     z2(1683+1 à  1734) : u°°  
c                              z2(1122+1 à 1173) : zo(10)     z2(1734+1 à  1785) : w°°  
c                                                             z2(1785+1 à  1836) : v°°°  
c                              z2(1173+1 à 1224) : my         z2(1836+1 à  1887) : u°°°  
c                              z2(1224+1 à 1275) : ny         z2(1887+1 à  1938) : w°°°  
c                              z2(1275+1 à 1326) : ry         z2(1938+1 à  1989) : nx  
c                              z2(1326+1 à 1377) : nyx        z2(1989+1 à  2040) : nxy  
c                              z2(1377+1 à 1428) : qy         z2(2040+1 à  2091) : mx  
c                              z2(1428+1 à 1479) : w°         z2(2091+1 à  2142) : mxy  
c                                                             z2(2142+1 à  2193) : myx  
c                                                             z2(2193+1 à  2244) : qx  
c                                                             z2(2244+1 à  2295) : rx  

      kh=31+2*mt
      do 114 k=1,5
      sp=dsin(lamb*dis(k))
      sq=dcos(lamb*dis(k))
c
c     calcul en 5 pts selon x et en 31+20(max) pts selon y de:
c     --------------------------------------------------------
      do 112 i=1,kh
      ij=i+(k-1)*51
c     calcul de  v,u,w,dw/dy,nx,ny,nxy,nyx,mx,my,mxy,myx,qx,qy,ry,rx,
      eff(ij)=eff(ij)+z2(i)*sp                  ! v
      eff(ij+255)=eff(ij+255)+z2(i+51)*sq       ! u
      eff(ij+510)=eff(ij+510)+z2(i+102)*sp      ! w
      eff(ij+765)=eff(ij+765)+z2(i+1581)*sp     ! wø
      if(impr2.le.-1) goto 555                   !
      eff(ij+1020)=eff(ij+1020)+0				  ! nx
      eff(ij+1275)=eff(ij+1275)+z2(i+1224)*sp   ! ny
      eff(ij+1530)=eff(ij+1530)+z2(i+1989)*sq   ! nxy
      eff(ij+1785)=eff(ij+1785)+z2(i+1326)*sq   ! nyx
      eff(ij+2040)=eff(ij+2040)+z2(i+2040)*sp   ! mx
      eff(ij+2295)=eff(ij+2295)+z2(i+1173)*sp   ! my
      eff(ij+2550)=eff(ij+2550)+z2(i+2091)*sq   ! mxy
      eff(ij+2805)=eff(ij+2805)+z2(i+2142)*sq   ! myx
      eff(ij+3060)=eff(ij+3060)+z2(i+2193)*sq   ! qx
      eff(ij+3315)=eff(ij+3315)+z2(i+1377)*sp   ! qy
      eff(ij+3570)=eff(ij+3570)+z2(i+1275)*sp   ! ry
      eff(ij+3825)=eff(ij+3825)+z2(i+2244)*sq   ! rx
  555 continue

c	section de l'épontille en double t
	if(isect1.eq.3)then

      if(dya.eq.0.)goto 889
        temp(ij+765)=temp(ij+765)+(z2(i+1224)/aire)*sp						! sigmay compression
        eff(ij+4080)=eff(ij+4080)+((z2(i+1224)/aire)
     *					+(z2(i+1173)*hya/aiy))*sp		! sigmay jass
        eff(ij+4335)=eff(ij+4335)+((z2(i+1224)/aire)
     *					-(z2(i+1173)*hya/aiy))*sp		! sigmay jasi
        temp(ij)    =temp(ij) + (z2(i+1377)*sp*(tpb2/tya))/(aiy)			! tyz (my) jas =t1 (à j/s)
        temp(ij+255)=temp(ij+255)+((z2(i+1377)*((tpb2/dya)+(hya*sm2))*sp
     *					)/(aiy))												! tyz (my) an =t1 (max)
        temp(ij+510)=temp(ij+510)+ (z2(i+2091)*sq*dya)/(tork)				    ! tyz(mxy) jas = t2 (rectangle)
	  if (nz==jjk) then
        eff(ij+4590)=dabs(temp(ij))    +dabs(temp(ij+510))				! tyz jas = t1+t2
        eff(ij+4845)=dabs(temp(ij+255))									! tyz an = t1
        eff(ij+5100)=dsqrt(eff(ij+4080)**2+3.d0*(eff(ij+4590)**2))		! sigma  comp jas sup
        eff(ij+5355)=dsqrt(eff(ij+4335)**2+3.d0*(eff(ij+4590)**2))		! sigma  comp jas inf
        eff(ij+5610)=dsqrt(temp(ij+765)**2+3.d0*(eff(ij+4845)**2))		! sigma  comp an
        endif
 889    continue
c     calcul de : sigmay sup semelle,sigmay inf semelle, txy sem (torsion),
c				 sigma comp semelle cadre
       if(tya.eq.0.)goto 113
       eff(ij+5865)=eff(ij+5865) +((z2(i+1224)/aire)
     *		+((z2(i+1173)*smc2)/aiy))*sp	 ! sigmay sup semelle
       eff(ij+6120)=eff(ij+6120) +((z2(i+1224)/aire)
     *		-((z2(i+1173)*smc2)/aiy))*sp	 ! sigmay inf semelle
       temp(ij+1275)=temp(ij+1275)+ (z2(i+1377)*sp*(tpb2/(2*tya))
     *					/(aiy))										 ! txy(my) sem = t1 (max semelle)
        temp(ij+1020)=temp(ij+1020)+ ((z2(i+2091)*sq*tya)/tork)		 ! txy(mxy) sem=t3 (torsion)
        if (nz==jjk) then
        eff(ij+6375)=dabs(temp(ij+1275))+dabs(temp(ij+1020)) 			! txy sem =t1+t2+t3
        eff(ij+6630)=dsqrt(eff(ij+5865)**2+3.d0*(eff(ij+6375)**2))		! sigma  comp sem aig.sup
        eff(ij+6885)=dsqrt(eff(ij+6120)**2+3.d0*(eff(ij+6375)**2))		! sigma  comp sem aig.inf
	  endif
  113  continue

c          raidisseurs (doublet)
c         **************
c   calcul de : sigmax jas, sigmax jab, sigmax semelle,

      if((indrai.eq.0).and.(iopti.eq.0)) goto 112
      if(dxr.ne.0.) then
       eff(ij+7395)=eff(ij+7395)+									! sigmax jas
     *              e1*lamb* (-z2(i+51)+smar2*lamb*z2(i+102))*sp
       eff(ij+7650)=eff(ij+7650)+									! sigmax jab
     *              e1*lamb* (-z2(i+51)+smbr*lamb*z2(i+102))*sp
	endif
      if(txr.ne.0.)then
       eff(ij+7905)=eff(ij+7905)+									! sigmax semelle
     *              e1*lamb* (-z2(i+51)+smr2* lamb*z2(i+102))*sp       
      endif 
c   calcul de: txz jas, txz jab, tyx sem, 
      if(dxr.ne.0.) then
       temp2(ij)=temp2(ij)+ sh*dxr*lamb*z2(i+1581) *sq			! txz(mxy)jas = txz(mxy) jab
       temp2(ij+255)=temp2(ij+255)+								! txz(rx) jas
     *    (-lamb2*e1*(z2(i+51)*tmrx-lamb*tpbr2*z2(i+102))/dxr) *sq	
       temp2(ij+510)=temp2(ij+510)+								! txz(rx) jab
     *    (-lamb2*e1*(z2(i+51)*tmrr-lamb*tprr2*z2(i+102))/dxr) *sq    
	endif
      if(txr.ne.0.) then                                           
       temp2(ij+765) =temp2(ij+765) +  sh*txr*lamb*z2(i+1581)*sq		! tyx(rx) sem
       temp2(ij+1020)=temp2(ij+1020)+       
     * (-lamb2*e1* (z2(i+51)*tmrx/2.-lamb*tpbr2*z2(i+102)/2.) /txr)*sq	! tyx(mxy) sem   
       temp2(ij+1275)=temp2(ij+1275)+
     *            ( (-lamb*z2(i)+z2(i+1530)) *const(11)*1.5/tmrx) * sq	! tyx(??) sem  
      endif     
c   calcul de : sigma comp jas, sigma comp jab , sigma comp semelle     
      if (nz.ne.jjk) goto 112                                         
      eff(ij+8160)=dabs(temp2(ij+255))+dabs(temp2(ij))			! txz jas
      eff(ij+8415)=dabs(temp2(ij+510))+dabs(temp2(ij))			! txz jab
      eff(ij+8925)=dsqrt(eff(ij+7395)**2+3.d0*(eff(ij+8160)**2))	! sigma comp jas
      eff(ij+9180)=dsqrt(eff(ij+7650)**2+3.d0*(eff(ij+8415)**2))	! sigma comp jab
      if(txr.ne.0.) then
       eff(ij+8670)=dabs(temp2(ij+765))+dabs(temp2(ij+1020))		! tyx sem
     *                                 +dabs(temp2(ij+1275))
       eff(ij+9435)=dsqrt(eff(ij+7905)**2+3.d0*(eff(ij+8670)**2))	! sigma comp semelle
	endif
c	section de l'épontille en cercle
	else
	if(isect1.eq.1)then
       if(epais.eq.0.)goto 890
        eff(ij+4080)=eff(ij+4080)+((z2(i+1224)/aire)
     *				+(z2(i+1173)*hya/(aiy*2)))*sp						! sigmay sup
        eff(ij+4335)=eff(ij+4335)+((z2(i+1224)/aire)
     *				-(z2(i+1173)*hya/(2*aiy)))*sp						! sigmay inf
        temp(ij)    =0												! tyz (my) au sommet
        temp(ij+255)=temp(ij+255)
     *				+ z2(i+1377)*sp*rtan/(epais*aiy)					! tyz (my) an =t1 (max)
        temp(ij+510)=temp(ij+510)+ (z2(i+2091)*sq/(2*s2*epais))		! tyz(mxy) torsion max
	  temp(ij+765)=temp(ij+765)+(z2(i+1224)/aire)*sp					! sigmay compression
        temp(ij+1020)=temp(ij+1020)
     *				+ z2(i+1377)*0.707*sp*rtan/(epais*aiy)			! tyz (my) 45° =t1 (max)
       if (nz==jjk) then
        eff(ij+4590)=dabs(temp(ij))    +dabs(temp(ij+510))			! tyz sommet = t1+t2
        eff(ij+4845)=dabs(temp(ij+255))+dabs(temp(ij+510))			!tyz an = torsion+t2
        eff(ij+5100)=dsqrt(eff(ij+4080)**2+3.d0*(eff(ij+4590)**2))	! sigma comp sup
        eff(ij+5355)=dsqrt(eff(ij+4335)**2+3.d0*(eff(ij+4590)**2))		! sigma comp inf
	  eff(ij+5610)=dsqrt(temp(ij+765)**2+3.d0*(eff(ij+4845)**2))		! sigma comp an
	 endif
 890    continue

c	section de l'épontille en carré
		elseif(isect1.eq.2)then
       if(epais.eq.0.)goto 891
        eff(ij+4080)=eff(ij+4080)+((z2(i+1224)/aire)
     *				+(z2(i+1173)*hya/(aiy*2)))*sp						! sigmay sup
        eff(ij+4335)=eff(ij+4335)+((z2(i+1224)/aire)
     *				-(z2(i+1173)*hya/(2*aiy)))*sp						! sigmay inf
        temp(ij)    =0												! tyz (my) au sommet
        temp(ij+255)=temp(ij+255)
     *				+ z2(i+1377)*sp*asc/(aiy)					! tyz (my) an =t1 (max)
        temp(ij+510)=temp(ij+510)+ (z2(i+2091)*sq/(2*s*epais))		! tyz(mxy) torsion max
	  temp(ij+765)=temp(ij+765)+(z2(i+1224)/aire)*sp					! sigmay compression
        temp(ij+1020)=temp(ij+1020)
     *				+ z2(i+1377)*0.707*sp*asc2/(aiy)			! tyz (my) 45° =t1 (max)
       if (nz==jjk) then
        eff(ij+4590)=dabs(temp(ij))    +dabs(temp(ij+510))			! tyz sommet = t1+t2
        eff(ij+4845)=dabs(temp(ij+255))+dabs(temp(ij+510))			!tyz an = torsion+t2
        eff(ij+5100)=dsqrt(eff(ij+4080)**2+3.d0*(eff(ij+4590)**2))	! sigma comp sup
        eff(ij+5355)=dsqrt(eff(ij+4335)**2+3.d0*(eff(ij+4590)**2))		! sigma comp inf
	  eff(ij+5610)=dsqrt(temp(ij+765)**2+3.d0*(eff(ij+4845)**2))		! sigma comp an
       endif
 891   continue
	endif
c          raidisseurs (cercle et carre)
c         **************
c   calcul de : sigmax jas, sigmax jab, sigmax semelle,
      if(dxr.ne.0.) then
       eff(ij+7395)=eff(ij+7395)+									! sigmax jas
     *              e1*lamb* (-z2(i+51)+smar2*lamb*z2(i+102))*sp
       eff(ij+7650)=eff(ij+7650)+									! sigmax an
     *              e1*lamb* (-z2(i+51)+0*lamb*z2(i+102))*sp
	endif
      if(txr.ne.0.)then
       eff(ij+7905)=eff(ij+7905)+									! sigmax semelle
     *              e1*lamb* (-z2(i+51)+smr2* lamb*z2(i+102))*sp  
      endif 
c   calcul de: txz jas, txz jab, tyx sem, 
      if(dxr.ne.0.) then
       temp2(ij)=temp2(ij)+ (e1/2)*dxr*lamb*z2(i+1581) *sq			! txz(mxy)jas = txz(mxy) jab
       temp2(ij+255)=temp2(ij+255)+								! txz(rx) jas
     *    (-lamb2*e1*(z2(i+51)*tmrx-lamb*tpbr2*z2(i+102))/dxr) *sq	
       temp2(ij+510)=temp2(ij+510)+								! txz(rx) jab
     *    (-lamb2*e1*(z2(i+51)*tmrr-lamb*tprr2*z2(i+102))/dxr) *sq 
	endif
      if(txr.ne.0.) then   
       temp2(ij+765) =temp2(ij+765) +  (e1/2)*txr*lamb*z2(i+1581) * sq		! tyx(rx) sem
       temp2(ij+1020)=temp2(ij+1020)+       
     * (-lamb2*e1* (z2(i+51)*tmrx/2.-lamb*tpbr2*z2(i+102)/2.) /txr)*sq	! tyx(mxy) sem   
       temp2(ij+1275)=temp2(ij+1275)+
     *            ( (-lamb*z2(i)+z2(i+1530)) *const(11)*1.5/tmrx) * sq	! tyx(??) sem  
      endif     
c   calcul de : sigma comp jas, sigma comp jab , sigma comp semelle     
      if (nz.ne.jjk) goto 112                                         
      eff(ij+8160)=dabs(temp2(ij+255))+dabs(temp2(ij))			! txz jas
      eff(ij+8415)=dabs(temp2(ij+510))+dabs(temp2(ij))			! txz jab
      eff(ij+8925)=dsqrt(eff(ij+5865)**2+3.d0*(eff(ij+6630)**2))	! sigma comp jas
      eff(ij+9180)=dsqrt(eff(ij+6120)**2+3.d0*(eff(ij+6885)**2))	! sigma comp jab
      if(txr.ne.0.) then
       eff(ij+8670)=dabs(temp2(ij+765))+dabs(temp2(ij+1020))		! tyx sem
     *                                 +dabs(temp2(ij+1275))
       eff(ij+9435)=dsqrt(eff(ij+6375)**2+3.d0*(eff(ij+7140)**2))	! sigma comp semelle
	endif
	endif					!+++ fin cercle-carre
  112 continue   ! boucle sur les 31+2*mt points (selon y)                                                
      if(mt.eq.0) goto 114
c
c     calcul des effets concentres aux niveaux des traverses pour 5 pts
c     selon x et a l'emplacement des traverses selon y.
c     sont classes par ordre croissant dans le vecteur conc.
c
c     ordre des variables classées dans conc (1 à 8)
c     --------------------------------------------------
c       1 nx    2 nxy    3 mx    4 mxy    5 qx    6 rx
c       7	sig x sem       8	sig x jas       9	sig x jab 
c      10	tyx   sem      11	txz   jas      12	txz   jab
c      13	sig comp sem   14	sig comp jas   15	sig comp jab

	kstt=(-1)**kst
      do 116 i=1,mt
      ji=6*(i-1)
      ij=i+(k-1)*10

      wyt=panneau(nel).wxtr(i)               ! larg    sem
      tyt=panneau(nel).txtr(i)	           ! epaiss. sem
      hyt=panneau(nel).hxtr(i)               ! haut.   âme
      dyt=panneau(nel).dxtr(i)	           ! epaiss. âme
	
      tmt =wyt*tyt
      tmrt=tmt+dyt*hyt
      smt =(delta/2.+hyt+tyt)*kstt
 	smt2=(hyt+tyt)*kstt									
      smat=smt-tyt*kstt
      smat2=smt2-tyt*kstt									
      smbt=(delta/2.)*kstt
      tpbt=tmt*(smat+0.5*tyt*kstt)
	tpbt2=tmt*(smat2+0.5*tyt*kstt)						
      tprt=tpbt+dyt*hyt*(smat-0.5*hyt*kstt)
      tprt2=tpbt2+dyt*hyt*(smat2-0.5*hyt*kstt)			
	
c     rem: conc(1 a 50) contient "nx conc." sans la composante selon x  
c            de nyx (= sx*dv/dy + d2u/d2y / lamb ) c.a.d. la            
c            reciproque de "nxy conc.                                   
      conc(ij)=conc(ij)+lamb*( -const(15+ji)*z2(2*i+81)               ! nx
     *                         +lamb*const(16+ji)*z2(2*i+132) ) *sp     
      conc(ij+50)=conc(ij+50)+ const(17+ji)*sq *( z2(2*i+30)*lamb     ! nxy
     *                       + z2(2*i+1560))                            
      conc(ij+100)=conc(ij+100)+lamb*(-lamb*const(18+ji)*z2(2*i+132)+ ! mx
     *   const(16+ji)*z2(2*i+81))*sp
      conc(ij+150)=conc(ij+150) +( lamb*const(19+ji)*z2(2*i+1611)+    ! mxy
     *           const(20+ji) * (z2(2*i+30)*lamb+z2(2*i+1560)) )*sq     
      conc(ij+200)=conc(ij+200)+(lamb2*(-lamb*const(18+ji)*z2(2*i+132)! qx
     *       +const(16+ji)*z2(2*i+81))+                                 
     *    const(8) *(z2(2*i+1561)-z2(2*i+1560)))*sq                     
      conc(ij+250)=conc(ij+250)+ sq *( const(19+ji)*lamb*z2(2*i+1764) ! rx (partiel)
     *  + const(14)*(z2(2*i+1561)-z2(2*i+1560))                         
     *  +  const(20+ji)*(lamb*z2(2*i+1509)+z2(2*i+1713))  )             
      if((tyt.gt.0.).and.(wyt.gt.0.)) then                                             
       conc(ij+300)=conc(ij+300)+  e1*lamb*								! sig x sem
     *                (-z2(2*i+81)+lamb*smt2*z2(2*i+132))*sp
      endif
      conc(ij+350)=conc(ij+350)+e1*lamb*(-z2(2*i+81)						! sig x jas
     *                            +smat2*lamb*z2(2*i+132))*sp
      conc(ij+400)=conc(ij+400)+e1*lamb*(-z2(2*i+81)						! sig x jab
     *                            +smbt*lamb*z2(2*i+132))*sp
      if((tyt.gt.0.).and.(wyt.gt.0.)) then
       temp3(ij)    =temp3(ij) + sq*										! tyx(nxy)  sem
     *    (const(17+ji)*(-lamb*z2(2*i+30)+z2(2*i+1560))*1.5/tmt)*fam(5)           
       temp3(ij+50) =temp3(ij+50)  + sq* (e1/2)*tyt*lamb*z2(2*i+1611)			! tyx(mxy)  sem
       temp3(ij+100)=temp3(ij+100) + sq* 
     *  (-lamb2* e1*(z2(2*i+81)*tmt/2.-lamb*tpbt2*z2(2*i+132)/2.)) /tyt		! tyx(rx)  sem
      endif
      if(dyt.ne.0.) then
       temp3(ij+150)=temp3(ij+150)+ (-lamb**2*e1*(z2(2*i+81)*tmt-			! txz(rx) jas
     *            lamb*tpbt2*z2(2*i+132))/dyt)    *sq                    
       temp3(ij+200)=temp3(ij+200)+ (-lamb**2*e1*(z2(2*i+81)*tmrt-			! txz(rx) jab
     *            lamb*tprt2*z2(2*i+132))/dyt) *sq                    
       temp3(ij+250)=temp3(ij+250)+ (e1/2)*dyt*lamb*z2(2*i+1611) *sq			! txz(mxy)jas=txz(mxy)jab
      endif

      if(nz.eq.jjk) then                                           
       conc(ij+250)=conc(ij+250)+conc(ij+200)							! rx
       if((tyt.gt.0.).and.(wyt.gt.0.)) then
        conc(ij+450)=dabs(temp3(ij))+dabs(temp3(ij+50))				! tyx  sem
     *                              +dabs(temp3(ij+100))
        conc(ij+600)=dsqrt(conc(ij+300)**2+3.*conc(ij+450)**2)		! sig comp sem
	 endif
       conc(ij+500)=dabs(temp3(ij+150))+dabs(temp3(ij+250) )			! txz jas
       conc(ij+550)=dabs(temp3(ij+200))+dabs(temp3(ij+250) )			! txz jab
       conc(ij+650)=dsqrt(conc(ij+350)**2+3.*conc(ij+500)**2)			! sig comp jas
       conc(ij+700)=dsqrt(conc(ij+400)**2+3.*conc(ij+550)**2)			! sig comp jab
      endif

  116 continue  ! boucle sur les traverses
  114 continue  ! boucle sur les 5 coupes (selon x)

      return 

c ******************************************************************************
c 	les formats
c 	-----------
  282 format(a18,1x,5(1x,e13.6))
  288 format(' variable de conception nø',i3,' du panneau nø',i3/
     *         50(1h-))
  294 format('     fct.             pt.1          pt.2',
     *     '          pt.3          pt.4          pt.5')
      end
