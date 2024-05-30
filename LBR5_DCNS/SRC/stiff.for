      subroutine stiff(iprint,nel,e1,eta1,sigy1,width,q,phil,
     *                 ploc1,xi,xf,chamax,dchamax,kse,nsol,is,
     *                 delta,epsa,hxr,dxr,wxr,txr,epsr,entr,ksr,
     *                 plat,fl,
     *                 sig11,sig21,sig31,sig41,tau,flr,wpl1,wpl2,				    
     *                 dsig1,dsig2,dsig3,dsig4,dtau,dflr,dwpl1,dwpl2,
     *                 iq50)
      use sharedvar
      implicit real*8(a-h,o-z)
	real*8 ix,ix2
      dimension xi(nsol),xf(nsol),chamax(5,nsol),dchamax(5,nsol,9),
     *          dplat(9),dplat2(9)
      dimension dsig1(9),dsig2(9),dsig3(9),dsig4(9),dtau(9),dflr(9),
     *          dwpl1(9),dwpl2(9)
    
c*******************************************************************************
c     subroutine stiff (flexion locale des raidisseurs) 
c     =================
c     cette subrourine est appellée dans bo2.
c                                                                    
c  inputs:e1,eta1,sigy1,width,q,phil, 
c         delta, epsa et hxr,dxr,wxr,txr,epsr  + entr, ksr
c         avec entr : entredistance réelle entre raidisseurs = delta(raid)
c                     (cfr valeur du fichier de donnees)
c              epsr : largeur effective associée à 1 raid (= epsa/nbre de raid)
c                     soit la vrai varaible de coneption
c                                                                       
c  outputs:
c
c  créer: le 15-11-2000  par  ph. rigo
c
c  modifications: 
c     - 15 avril 2003 : modif en vue de la combinaison des contraintes (f. bair)
c                               
c  dernière modif : 20-5-2003	     	
c                                                                       
c*************************************************************************
c*************************************************************************
c*************************************************************************
c
c               ^ y et sigma y
c               i
c               *****************************
c               *                           *  b=epsr (var. de conception)
c               *                           *  b=entr (pour stiff.)
c               *                           *
c               ***************************** --------> x et sigma x
c               <----------- a = epsa ------>
c
c      epsa  	    = a   (m)
c      entr,epsr	= b   (m)
c      sigy1		= limite elastique (n/m2), (ex, re=240 n/mm2)
c      plat = max(xi,xf,cha,...)
c ------------------------------------------------------------------------
c          
c      ds   = dsdxi = d(s)/d(xi)    xi=1,9
c      ds(k)= d(s)/d(xi=k) 
c   
c    dzg,dv1,dv2,dz1,dz2,dz3,dza,dix,ds  ! variable de travail
c    dsig1(xi),dsig2(xi,dtau(xi),dflr(xi),dwpl(xi)
c
c	xi = 1  delta
c	xi = 5  epsa
c	xi = 6  hxr
c	xi = 7  dxr
c	xi = 8  wxr
c	xi = 9  epsr (vrai variable et non pas entr)
c =============================================================
 
c     recherche de la pression max et de son signe (sens)
c     ---------------------------------------------------
c     plat positif si oriente dans le sens de l'axe z du repere local du panneau
c          plat>0 si kse = 1  et xi>0
c                 si kse = 2  et xi<0
c          plat<0 si kse = 1  et xi<0
c                 si kse = 2  et xi>0
c     ksr = 1  raid. du cote des z<0  (cas de reference)
c          si xplat >0 : semelle comprimee et borde tendu
c          si xplat <0 : semelle tendue    et borde comprime
c     ksr = 2  raid. du cote des z>0
c          si xplat >0 : semelle tendue    et borde comprime  
c          si xplat <0 : semelle comprimee et borde tendu

	call annuld(dplat,9)							!!!aout04
      plat =dmax1(chamax(1,is),chamax(2,is),chamax(3,is),   ! plat en n/m2   
     *            chamax(4,is),chamax(5,is)) *9810.0        ! chamax en m d'eau
	indmax=1										!!!aout04
	do i=2,5										!!!aout04
	  if (chamax(i,is).gt.chamax(i-1,is)) indmax=i	!!!aout04
	enddo											!!!aout04
      plat2=dmin1(chamax(1,is),chamax(2,is),chamax(3,is),   !   
     *            chamax(4,is),chamax(5,is)) *9810.0        ! 
	indmin=1										!!!aout04
	do i=2,5										!!!aout04
	  if (chamax(i,is).lt.chamax(i-1,is)) indmin=i	!!!aout04
	enddo											!!!aout04
	
	if(dabs(plat).le.dabs(plat2)) then				            !!!aout04
	  plat=plat2									!!!aout04
	  ind=indmin									!!!aout04
	else											!!!aout04
	  ind=indmax									!!!aout04
	endif											!!!aout04

	if(dabs(plat).le.dabs(ploc1*9810)) then		            	!!!aout04
	  plat=ploc1*9810								      !!!aout04
	else											!!!aout04
	  do k=1,9										!!!aout04
		dplat(k)=dchamax(ind,is,k)*9810.0			            !!!aout04
	  enddo										!!!aout04
	endif											!!!aout04

	plat2= dabs(plat) ! pour le calcul des tau et de la fleche
	signe=1.										!!!aout04
	if (plat.lt.0) signe=-1.						      !!!aout04
	do k=1,9										!!!aout04
	  dplat2(k)=signe*dplat(k)						      !!!aout04
	enddo											!!!aout04 

c     détermination du signe des contraintes (positif en traction)                        !avril2003
      signe=1.										!!!aout04
	if (plat.lt.0) then
	   if (kse.eq.ksr) then						      	!!!aout04
	     plat = - plat								!!!aout04
		 signe=-1.									!!!aout04
	   endif										!!!aout04
	else
	   if (kse.ne.ksr) then							      !!!aout04
		 plat = - plat								!!!aout04
		 signe=-1.									!!!aout04
	   endif										!!!aout04
	endif

	do k=1,9										!!!aout04
		dplat(k)=signe*dplat(k)						      !!!aout04
	enddo

c------impressions des données-----------------------------------------------
	if(impr2.ge.-1) then		!sept06
	write (66,*)'subr stiff (flexion locale raid.)'
	write (66,*)'*********************************'
	write (66,*)'  delta(m)  h(web)   d(web)    b(flange)  t(flange)'
	write (66,'(5f9.4)')  delta,hxr,dxr,wxr,txr
	write (66,*)'  epsa(m)  epsr    entr(m)     plat(n/m2)    e(n/m2)'
	write (66,'(3f9.4,2e14.7)') epsa,epsr,entr,plat,e1
	endif						!sept06

c  ===========================================================
c !! le repère local du raid. est situé à la junction jab
c    -----------------------------------------------------
	delta2=delta*delta
	epsa2 =epsa *epsa
	hxr2  =hxr  *hxr

	area= (entr*delta +dxr*hxr +wxr*txr)			    ! surface totale non reduite
	stat= -entr*delta2+dxr*hxr2+wxr*txr*(2.0*hxr+txr)	! 2*moment statique
	zg= stat/(2*area)  ! position axe neutre
	z1=  zg+delta/2.0
	z2= -zg+  hxr/2.0
	z3= -zg+hxr+txr/2.0
	za=(-zg+hxr)/2.0

	ix= entr*delta*(z1*z1) + dxr*hxr*(z2*z2) + wxr*txr*(z3*z3)
     *	 + (entr*(delta**3)+dxr*(hxr**3)+wxr*(txr**3))/12
	s= (hxr-zg)**2*dxr/2 + wxr*txr*z3 ! moment statique partie sup. par rapport axe neutre 
                                        ! (pour calcul tau)
      v1 = zg + delta/2                                        !avril2003
	v2=-(hxr-zg+txr)	! excentrement partie sup semelle
	v3 = zg             ! jonction âme-bordage               !avril2003
	v4 = zg - hxr       ! jonction âme-semelle               !avril2003

c	calcul contraintes et fleche (fl) du raidisseur
c     ------------------------------------------------
	sig11 = plat*entr*epsa2*v1 /(10.0*ix)        ! m=pl**2/10 soit intermediare entre 1/8 et 1/12
	sig21 = plat*entr*epsa2*v2 /(10.0*ix)
	sig31 = plat*entr*epsa2*v3 /(10.0*ix)      !avril2003
	sig41 = plat*entr*epsa2*v4 /(10.0*ix)      !avril2003
	tau   = plat2*entr*epsa *s /(2.0*dxr*ix)
	fl   = 5.*plat2*entr*(epsa**4)/(384.0*e1*ix)  ! 5/384 c-à-d extrémites libres
	flr  = fl/epsa                               ! fl/portée = flèche relative
   
c	calcul de la fleche (wpl) de la plaque non raidie, encastrée entre 2 raidisseurs
c     ---------------------------------------------------------------------------------
c     wpl1= 1/384 p entr**4/ei  (borne sup. car effets des 2 autres bords négligés)
c     cette déformation correspond à une charge uniforme. 
c     toutes les mailles se déforment de façon identique (idem bord encasreés)
      wpl1=plat2*(entr**4)/(384.0*e1*delta2*delta/12.0)

c	calcul de la fleche (wpl) de la plaque non raidie, appuyée entre 2 raidisseurs
c     ---------------------------------------------------------------------------------
c     wpl2= 5/384 p entr**4/ei (borne sup. car effets des 2 autres bords négligés)
c     cette déformation correspond à une charge localisée. 
c     seule cette maille se déforme (idem poutre sur appuis multiples avec 1 seule travée chargée)
      wpl2=5.0*wpl1   ! borne sup. car assimilé à 1 poutre sur 2 appuis

c     impressions
c     -----------
	if(impr2.ge.-1) then			!sept06
	  write(66,9) ' area (area)           ',area
	  write(66,9) ' moment statique (stat)',stat
 	  write(66,9) ' gravity center        ',zg
	  write(66,9) ' static moment(s)        ', s         !avril2003
	  write(66,9) ' inertia moment          ', ix        !avril2003
	  write(66,9) ' v1 (milieu bordé z=0)   ', v1        !avril2003
	  write(66,9) ' v2 (partie sup semelle) ', v2        !avril2003
	  write(66,9) ' v3 (jonction âme-bordé) ', v3        !avril2003
	  write(66,9) ' v4 (jonction âme-sem)   ', v4        !avril2003

	  write(66,*)  
	  write(66,*) ' resultat flexion locale raid. et de la maille '
	  write(66,*) ' --------------------------------------------- '
	  write(66,9) ' sigma 1 (milieu bordé)  ', sig11       !avril2003
	  write(66,9) ' sigma 2 (semelle)       ', sig21
	  write(66,9) ' sigma 3 (jab)           ', sig31       !avril2003
	  write(66,9) ' sigma 4 (jas)           ', sig41       !avril2003
	  write(66,9) ' shear stress (max)      ', tau
	  write(66,9) ' flèche raid.(5/384) (m) ', fl
	  write(66,9) ' flèche relative (f/epsa)', flr
	  write(66,*)  
	  write(66,*) ' flèche maille (valeur absolue)' 
	  write(66,9) ' - charge uniforme  (bords encastrés)',wpl1
	  write(66,9) ' - charge localisée (bords articulés)',wpl2
	  write(66,*)

	endif

c     calcul des sensibilites
c     ===========================
      if(iopti.eq.0) return
      if(iq50.eq.0)  return ! restriction pas selectionnées

	nbrxi = nvar(nel)
      
	if(iprint.eq.1) then
	 write(66,*) ' =*=*=*=*=*=*=*=*=*=*=*=*=*=* '
	 write(66,*) '  sensibilities = d(f)/d(xi)  '
	 write(66,*) ' =*=*=*=*=*=*=*=*=*=*=*=*=*=* '
	endif

	area2=area*area
	ix2  =ix*ix

      do 1 j=1,nbrxi
	k=nxit(j,nel)

	if(k.eq.1) then

c	xi= delta 
c     --------
	dzg= -(2.0*delta*entr*area+entr*stat)/(2.0*area2)
	dv1=  dzg + 1.0/2   !avril2003
	dv2=  dzg
	dv3=  dzg           !avril2003
	dv4=  dzg           !avril2003
	dz1=  dzg + 0.5
	dz2= -dzg
	dz3= -dzg
	dza= -dzg/2.0
	dix= entr*((z1*z1)+2.0*delta*z1*dz1)
     *		  + (entr*delta2)/4.+2*dxr*hxr*z2*dz2
     *		  + 2.*wxr*txr*z3*dz3
	ds= dxr*(-dzg*za+(hxr-zg)*dza) + wxr*txr*dz3

	if (plat.ne.0) then														!!!aout04
	dsig1(k) = (plat*entr*epsa2/(10.*ix2))*(ix*dv1-v1*dix) 
     *           + dplat(k)*sig1/plat											!!!aout04
	dsig2(k) = (plat*entr*epsa2/(10.*ix2))*(ix*dv2-v2*dix)
     *		   + dplat(k)*sig2/plat											!!!aout04
	dsig3(k) = (plat*entr*epsa2/(10.*ix2))*(ix*dv3-v3*dix)    !avril2003
     *		   + dplat(k)*sig3/plat											!!!aout04
	dsig4(k) = (plat*entr*epsa2/(10.*ix2))*(ix*dv4-v4*dix)    !avril2003
     *		   + dplat(k)*sig4/plat											!!!aout04
	dtau(k)  = (plat2*entr*epsa/(2.*dxr*ix2))*(ix*ds-s*dix)
     *           + dplat2(k)*tau/plat2										!!!aout04
c	dfl(k) =-5.0*(plat2*entr*(epsa**4)/(384.*e1*ix2))*dix
	dflr(k)=-5.0*(plat2*entr*(epsa**3)/(384.*e1*ix2))*dix
     *           + dplat2(k)*flr/plat2										!!!aout04
      dwpl1(k)=-3.*wpl1/delta + dplat2(k)*wpl1/plat2							!!!aout04
      dwpl2(k)= 5.*dwpl1(k)
	endif																	!!!aout04

	if(iprint.eq.1) then
	 write(66,'(/a)') ' xi = delta '
       write(66,10) dsig1(k),dsig2(k),dsig3(k),dsig4(k),dtau(k),dflr(k),   !avril2003
     *             dwpl1(k)                                                !avril2003
      endif

	if (nel.eq.2) then
	continue
	endif

	else if(k.eq.5) then

c	xi= epsa 
c     --------
	if (plat.ne.0) then												!!!aout04
	dsig1(k) = plat*entr*epsa*v1/( 5.0*ix) + dplat(k)*sig1/plat		!!!aout04
	dsig2(k) = plat*entr*epsa*v2/( 5.0*ix) + dplat(k)*sig2/plat		!!!aout04
	dsig3(k) = plat*entr*epsa*v3/( 5.0*ix) + dplat(k)*sig3/plat		!!!aout04         !avril2003
	dsig4(k) = plat*entr*epsa*v4/( 5.0*ix) + dplat(k)*sig4/plat		!!!aout04         !avril2003
	dtau(k)  = plat2*entr*s      /( 2.0*dxr*ix) + dplat2(k)*tau/plat2 !!!aout04
	dflr(k)   = 5.0*plat2*entr*(epsa**2) /(128.0*e1*ix)
     *			 + dplat2(k)*flr/plat2								!!!aout04
      dwpl1(k)= 0. + dplat2(k)*wpl1/plat2								!!!aout04
      dwpl2(k)= 5.*dwpl1(k)											!!!aout04
	endif															!!!aout04

	if(iprint.eq.1) then
	 write(66,'(/a)') ' xi = epsa '
       write(66,10) dsig1(k),dsig2(k),dsig3(k),dsig4(k),dtau(k),dflr(k),   !avril2003
     *             dwpl1(k)                                                !avril2003
      endif

	else if(k.eq.6) then

c	xi= hxr 
c     --------
	dzg= (2.0*(dxr*hxr+wxr*txr)*area-dxr*stat)/(2.0*area2)
	dv1=       dzg
	dv2=-1.0 + dzg
      dv3=       dzg    !avril2003
      dv4=-1.0 + dzg    !avril2003
	dz1=       dzg
	dz2= 0.5 - dzg
	dz3= 1.0 - dzg
	dza=(1.0 - dzg) /2.0
	dix= dxr*((z2**2)+2.0*hxr*z2*dz2)
     *		+ (dxr*hxr2)/4.0 + 2.0*entr*delta*z1*dz1
     *		+ 2.*wxr*txr*z3*dz3
	ds= dxr*za + hxr*dxr*dza-dxr*za*dzg - zg*dxr*dza+wxr*txr*dz3

	if (plat.ne.0) then											!!!aout04
	dsig1(k)= (plat*entr*epsa2/(10.*ix2))*(ix*dv1-v1*dix)
     *           + dplat(k)*sig1/plat									!!!aout04
	dsig2(k)= (plat*entr*epsa2/(10.*ix2))*(ix*dv2-v2*dix)
     *           + dplat(k)*sig2/plat									!!!aout04
	dsig3(k)= (plat*entr*epsa2/(10.*ix2))*(ix*dv3-v3*dix)     !avril2003
     *           + dplat(k)*sig3/plat									!!!aout04
	dsig4(k)= (plat*entr*epsa2/(10.*ix2))*(ix*dv4-v4*dix)     !avril2003
     *           + dplat(k)*sig4/plat									!!!aout04
	dtau(k) = (plat2*entr*epsa/(2.*dxr*ix2))*(ix*ds-s*dix)
     *           + dplat2(k)*tau/plat2								      !!!aout04
c	dfl(k) = -5.0*(plat2*entr*(epsa**4)/(384.*e1*ix2))*dix
	dflr(k)= -5.0*(plat2*entr*(epsa**3)/(384.*e1*ix2))*dix
     *           + dplat2(k)*flr/plat2								      !!!aout04

      dwpl1(k)= 0. + dplat2(k)*wpl1/plat2								      !!!aout04
      dwpl2(k)= 5.*dwpl1(k)											!!!aout04
	endif														!!!aout04

	if(iprint.eq.1) then
	 write(66,'(/a)') ' xi = hxr '
       write(66,10) dsig1(k),dsig2(k),dsig3(k),dsig4(k),dtau(k),dflr(k),   !avril2003
     *             dwpl1(k)                                                !avril2003
      endif

	else if(k.eq.7) then

c	xi= dxr 
c     --------
	dzg= (hxr2*area-hxr*stat)/(2*area2)
	dv1=  dzg
	dv2=  dzg
	dv3=  dzg    !avril2003
	dv4=  dzg    !avril2003
	dz1=  dzg
	dz2= -dzg
	dz3= -dzg
	dza= -dzg/2
	dix= hxr*((z2**2)+2.*dxr*z2*dz2)
     *		  + (hxr**3)/12. + 2.*entr*delta*z1*dz1
     *		  + 2.*wxr*txr*z3*dz3
	ds= (hxr-zg)*za+dxr*(-dzg*za + (hxr-zg)*dza) + wxr*txr*dz3

	if (plat.ne.0) then											!!!aout04
	dsig1(k)= (plat*entr*epsa2/(10.*ix2))*(ix*dv1-v1*dix)
     *		   + dplat(k)*sig1/plat									!!!aout04
	dsig2(k)= (plat*entr*epsa2/(10.*ix2))*(ix*dv2-v2*dix)
     *		   + dplat(k)*sig2/plat									!!!aout04
	dsig3(k)= (plat*entr*epsa2/(10.*ix2))*(ix*dv3-v3*dix)    !avril2003
     *		   + dplat(k)*sig3/plat									!!!aout04
	dsig4(k)= (plat*entr*epsa2/(10.*ix2))*(ix*dv4-v4*dix)	 !avril2003
     *		   + dplat(k)*sig4/plat									!!!aout04
	dtau(k) = (plat2*entr*epsa/(2.*(dxr**2)*ix2))*
     *                 (ix*dxr*ds-s*(dxr*dix+ix))
     *		   + dplat2(k)*tau/plat2								!!!aout04
	dflr(k)= -5.0*(plat2*entr*(epsa**3)/(384.*e1*ix2))*dix
     *		   + dplat2(k)*flr/plat2								!!!aout04
      
	dwpl1(k)= 0. + dplat2(k)*wpl1/plat2 						      	!!!aout04
      dwpl2(k)= 5.*dwpl1(k)											!!!aout04
	endif														!!!aout04

	if(iprint.eq.1) then
 	 write(66,'(/a)') ' xi = dxr '
       write(66,10) dsig1(k),dsig2(k),dsig3(k),dsig4(k),dtau(k),dflr(k),   !avril2003
     *             dwpl1(k)                                                !avril2003
      endif

	else if(k.eq.8) then

c	xi= wxr 
c     --------
	dzg= (txr*(2.0*hxr+txr)*area-txr*stat)/(2.0*area2)
	dv1=  dzg
	dv2=  dzg
	dv3=  dzg   !avril2003
	dv4=  dzg   !avril2003
	dz1=  dzg
	dz2= -dzg
	dz3= -dzg
	dza= -dzg/2
	dix= txr*((z3**2)+2.0*wxr*z3*dz3) + (txr**3)/12.0 
     *		+ 2.0*dxr*hxr*z2*dz2 + 2.0*entr*delta*z1*dz1
	ds =  dxr*(-dzg*za+(hxr-zg)*dza)+txr*z3+wxr*txr*dz3

	if (plat.ne.0) then											!!!aout04
	dsig1(k) = (plat*entr*epsa2/(10.*ix2))*(ix*dv1-v1*dix)
     *		   + dplat(k)*sig1/plat									!!!aout04
	dsig2(k) = (plat*entr*epsa2/(10.*ix2))*(ix*dv2-v2*dix)
     *		   + dplat(k)*sig2/plat									!!!aout04
	dsig3(k) = (plat*entr*epsa2/(10.*ix2))*(ix*dv3-v3*dix)   !avril2003
     *		   + dplat(k)*sig3/plat									!!!aout04
	dsig4(k) = (plat*entr*epsa2/(10.*ix2))*(ix*dv4-v4*dix)   !avril2003
     *		   + dplat(k)*sig4/plat									!!!aout04
	dtau(k) =  (plat2*entr*epsa/(2.0*dxr*ix2))*(ix*ds-s*dix)
     *		   + dplat2(k)*tau/plat2								!!!aout04
c	dfl(k)   = -5.0*(plat2*entr*(epsa**4)/(384.0*e1*ix2))*dix
	dflr(k)  = -5.0*(plat2*entr*(epsa**3)/(384.0*e1*ix2))*dix
     *		   + dplat2(k)*flr/plat2								!!!aout04

      dwpl1(k)= 0. + dplat2(k)*wpl1/plat2								      !!!aout04
      dwpl2(k)= 5.*dwpl1(k)											!!!aout04
	endif														!!!aout04

	if(iprint.eq.1) then
	 write(66,'(/a)') ' xi = wxr '
       write(66,10) dsig1(k),dsig2(k),dsig3(k),dsig4(k),dtau(k),dflr(k),   !avril2003
     *             dwpl1(k)                                                !avril2003
      endif

	else if(k.eq.9) then

c	xi= epsr 
c     --------
	dzg= -(delta2*area+delta*stat)/(2*area2)  != d(zg)/d(xi)
	dv1=  dzg
	dv2=  dzg
	dv3=  dzg    !avril2003
	dv4=  dzg    !avril2003
	dz1=  dzg
	dz2= -dzg
	dz3= -dzg
	dza= -dzg/2.0
	dix= delta*((z1**2)+2.0*entr*z1*dz1) + (delta**3)/12.
     *	     + 2.0*dxr*hxr*z2*dz2 + 2.0*wxr*txr*z3*dz3
	ds  = dxr*(-dzg*za + (hxr-zg)*dza) + wxr*txr*dz3

	if (plat.ne.0) then												!!!aout04
	dsig1(k)= (plat*epsa2/(10.0*ix))*(v1+entr*(ix*dv1-v1*dix)/ix)
     *		   + dplat(k)*sig1/plat									!!!aout04
	dsig2(k)= (plat*epsa2/(10.0*ix))*(v2+entr*(ix*dv2-v2*dix)/ix)
     *		   + dplat(k)*sig2/plat									!!!aout04
	dsig3(k)= (plat*epsa2/(10.0*ix))*(v3+entr*(ix*dv3-v3*dix)/ix)   !avril2003
     *		   + dplat(k)*sig3/plat									!!!aout04
	dsig4(k)= (plat*epsa2/(10.0*ix))*(v4+entr*(ix*dv4-v4*dix)/ix)   !avril2003
     *		   + dplat(k)*sig4/plat									!!!aout04
	dtau(k) = (plat2*epsa/(2*dxr*ix))*(s+entr*(ix*ds-s*dix)/ix)
     *		   + dplat2(k)*tau/plat2								!!!aout04
	dflr(k)=5.0*(plat2*(epsa**3)/(384.*e1*ix2))*(ix-entr*dix)
     *		   + dplat2(k)*flr/plat2								!!!aout04

      dwpl1(k)= 4.0* wpl1/entr + dplat2(k)*wpl1/plat2					!!!aout04
      dwpl2(k)= 5.0*dwpl1(k)
	endif															!!!aout04


      !prise en compte de  d(entr)/d(epsr) = (entr/epsr)**2 
	  temp=(entr/epsr)**2
	  dsig1(k) = dsig1(k) * temp
	  dsig2(k) = dsig2(k) * temp
	  dtau(k)  = dtau(k)  * temp
	  dflr(k)  = dflr(k)  * temp
	  dwpl1(k) = dwpl1(k) * temp
	  dwpl2(k) = dwpl2(k) * temp

	if(iprint.eq.1) then
	 write(66,'(/a)') ' xi = epsr '
       write(66,10) dsig1(k),dsig2(k),dsig3(k),dsig4(k),dtau(k),dflr(k),   !avril2003
     *             dwpl1(k)                                                !avril2003
      endif

      else

c	xi= 2,3,4 (hya, dya et wya)
c     ---------------------------
	dsig1(k) = 0.0
	dsig2(k) = 0.0
	dsig3(k) = 0.0   !avril2003
	dsig4(k) = 0.0   !avril2003
	dtau (k) = 0.0
	dflr (k) = 0.0
      dwpl1(k) = 0.0
      dwpl2(k) = 0.0

	if(iprint.eq.1) then
	 write(66,'(/a,i1,a)') ' xi = ',k,' (not used)'
       write(66,10) dsig1(k),dsig2(k),dsig3(k),dsig4(k),dtau(k),dflr(k),   !avril2003
     *             dwpl1(k)                                                !avril2003
      endif


      endif

  1   continue

      if (epsa.ge.width) then !avril2003
	sig11 = 0.d00
	sig21 = 0.d00
	sig31 = 0.d00
	sig41 = 0.d00
	tau  = 0.d00
	call annuld(dsig1,nbrxi)!avril2003
	call annuld(dsig2,nbrxi)!avril2003
	call annuld(dsig3,nbrxi)!avril2003
	call annuld(dsig4,nbrxi)!avril2003
	call annuld(dtau,nbrxi) !avril2003
	endif                   !avril2003   

c ------------------------------------

   9	format(a,' = ',e14.7,1x,a)
  10  format('   sigma 1 (milieu bordé) ',e14.7/     !avril2003
     *       '   sigma 2 (semelle)      ',e14.7/     !avril2003
     *       '   sigma 3 (jab)          ',e14.7/     !avril2003
     *       '   sigma 4 (jas)          ',e14.7/     !avril2003 
     *       '   tau(ame - axe neutre)  ',e14.7/
     *       '   deflection  rel. raid. ',e14.7/
     *       '   deflection  abs. maille',e14.7)

c ------------------------------------	                                                                   
      return                                                              
      end                                                                       
