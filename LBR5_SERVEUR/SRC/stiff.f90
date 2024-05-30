subroutine stiff(nel,e1,ploc1,chamax,dchamax,is,            &
                 plat,fl,                                                    &
                 sig11,sig21,sig31,sig41,tau,flr,        				     &
                 wpl1,wpl2,                                                  &
				 dsig1,dsig2,dsig3,dsig4,dtau,dflr,dwpl1,dwpl2,              &
                 iq50)


use param_section, IS_=>IS, KSE_=>KSE


implicit double precision(a-h,o-z)
double precision ix,ix2
dimension chamax(5,nsol),dchamax(5,nsol,9),dplat(9),dplat2(9)
dimension dsig1(9),dsig2(9),dsig3(9),dsig4(9),dtau(9),dflr(9),dwpl1(9),dwpl2(9)

!*******************************************************************************
!     subroutine stiff (flexion locale des raidisseurs) 
!     =================
!     cette subrourine est appellée dans bo2.
!                                                                    
!  inputs:e1,eta1,sigy1,width,q,phil, 
!         delta, epsa et hxr,dxr,wxr,txr,epsr  + entr, ksr
!         avec entr : entredistance réelle entre raidisseurs = delta(raid)
!                     (cfr valeur du fichier de donnees)
!              epsr : largeur effective associée à 1 raid (= epsa/nbre de raid)
!                     soit la vrai varaible de coneption
!                                                                       
!  outputs:
!
!  créer: le 15-11-2000  par  ph. rigo
!
!  modifications: 
!     - 15 avril 2003 : modif en vue de la combinaison des contraintes (f. bair)
!     - 10 juillet 2009 : prise en compte des cadres secondaires (f.bair)
!                               
!  dernière modif : 10-7-2009
!                                                                       
!*************************************************************************
!*************************************************************************
!*************************************************************************
!
!               ^ y et sigma y
!               i
!               *****************************
!               *                           *  b=epsr (var. de conception)
!               *                           *  b=entr (pour stiff.)
!               *                           *
!               ***************************** --------> x et sigma x
!               <----------- a = epsa ------>
!
!      epsa  	    = a   (m)
!      entr,epsr	= b   (m)
!      sigy1		= limite elastique (n/m2), (ex, re=240 n/mm2)
!      plat = max(xi,xf,cha,...)
! ------------------------------------------------------------------------
!          
!      ds   = dsdxi = d(s)/d(xi)    xi=1,9
!      ds(k)= d(s)/d(xi=k) 
!   
!    dzg,dv1,dv2,dz1,dz2,dz3,dza,dix,ds  ! variable de travail
!    dsig1(xi),dsig2(xi,dtau(xi),dflr(xi),dwpl(xi)
!
!	xi = 1  delta
!	xi = 5  epsa
!	xi = 6  hxr
!	xi = 7  dxr
!	xi = 8  wxr
!	xi = 9  epsr (vrai variable et non pas entr)
! =============================================================
!
!     recherche de la pression max et de son signe (sens)
!     ---------------------------------------------------
!     plat positif si oriente dans le sens de l'axe z du repere local du panneau
!          plat>0 si kse = 1  et xi>0
!                 si kse = 2  et xi<0
!          plat<0 si kse = 1  et xi<0
!                 si kse = 2  et xi>0
!     ksr = 1  raid. du cote des z<0  (cas de reference)
!          si xplat >0 : semelle comprimee et borde tendu
!          si xplat <0 : semelle tendue    et borde comprime
!     ksr = 2  raid. du cote des z>0
!          si xplat >0 : semelle tendue    et borde comprime  
!          si xplat <0 : semelle comprimee et borde tendu


delta = panneau(nel).delta
epsa = panneau(nel).epsa
hxr = panneau(nel).hxr
dxr = panneau(nel).dxr
wxr =panneau(nel).wxr
txr = panneau(nel).txr
epsr = panneau(nel).epsr
entr = panneau(nel).entr
ksr = panneau(nel).ksr
kse = panneau(nel).kse

epsa2 = panneau(nel).epsa2
if ((epsa2.gt.0) .and.(epsa2.lt.epsa)) then
	epsa=epsa2
	ia = 1 !=> dérivée par rapport à epsa = 0
else
    ia = 0
endif

call annuld(dplat,9)							
! plat en n/m2
! chamax en m d'eau
plat =dmax1(chamax(1,is),chamax(2,is),chamax(3,is),chamax(4,is),chamax(5,is)) *9810.0        

indmax=1									

do i=2,5									
  if (chamax(i,is).gt.chamax(i-1,is)) indmax=i	
enddo										

plat2=dmin1(chamax(1,is),chamax(2,is),chamax(3,is),chamax(4,is),chamax(5,is)) *9810.0       

indmin=1										

do i=2,5										
  if (chamax(i,is).lt.chamax(i-1,is)) indmin=i	
enddo
	
if(dabs(plat).le.dabs(plat2)) then				      
  plat=plat2					
  ind=indmin					
else									
  ind=indmax					
endif									

if(dabs(plat).le.dabs(ploc1*9810)) then		        
  plat=ploc1*9810				
else										
  do k=1,9							
	dplat(k)=dchamax(ind,is,k)*9810.0			          
  enddo						
endif							

plat2= dabs(plat) ! pour le calcul des tau et de la fleche
signe=1.										
if (plat.lt.0) signe=-1.		
do k=1,9										
  dplat2(k)=signe*dplat(k)	
enddo											

! détermination du signe des contraintes (positif en traction)                 
signe=1.		
						
if (plat.lt.0) then
   if (kse.eq.ksr) then			
     plat = - plat					
	 signe=-1.								
   endif										
else
   if (kse.ne.ksr) then			
	 plat = - plat						
	 signe=-1.								
   endif										
endif

do k=1,9										
	dplat(k)=signe*dplat(k)		
enddo


!------impressions des données-----------------------------------------------
if(impr2.ge.-1) then		
	write (iu_11(iboat),*)'subr stiff (flexion locale raid.)'
	write (iu_11(iboat),*)'*********************************'
	write (iu_11(iboat),*)'  delta(m)  h(web)   d(web)    b(flange)  t(flange)'
	write (iu_11(iboat),'(5f9.4)')  delta,hxr,dxr,wxr,txr
	if ((epsa2.gt.0) .and.(epsa2.lt.epsa)) then
		write (iu_11(iboat),*)'  epsa2!(m) epsr   entr(m)     plat(n/m2)    e(n/m2)'
	else
		write (iu_11(iboat),*)'  epsa(m)  epsr    entr(m)     plat(n/m2)    e(n/m2)'
	endif
	write (iu_11(iboat),'(3f9.4,2e14.7)') epsa,epsr,entr,plat,e1
endif						

!  ===========================================================
! !! le repère local du raid. est situé à la junction jab
!    -----------------------------------------------------
delta2=delta*delta
epsa2 =epsa *epsa
hxr2  =hxr  *hxr

! surface totale non reduite
area= (entr*delta +dxr*hxr +wxr*txr)			    
! 2*moment statique 
stat= -entr*delta2+dxr*hxr2+wxr*txr*(2.0*hxr+txr)	
zg= stat/(2*area)  ! position axe neutre
z1=  zg+delta/2.0
z2= -zg+  hxr/2.0
z3= -zg+hxr+txr/2.0
za=(-zg+hxr)/2.0

ix= entr*delta*(z1*z1) + dxr*hxr*(z2*z2) + wxr*txr*(z3*z3)+ (entr*(delta**3)+dxr*(hxr**3)+wxr*(txr**3))/12.
   
! moment statique partie sup. par rapport axe neutre
s= (hxr-zg)**2*dxr/2 + wxr*txr*z3  

! (pour calcul tau)                                      
v1 = zg + delta/2 

! excentrement partie sup semelle                                 
v2=-(hxr-zg+txr)	

! jonction âme-bordage 
v3 = zg             

! jonction âme-semelle            
v4 = zg - hxr                    

!	calcul contraintes et fleche (fl) du raidisseur
!     ------------------------------------------------

! Si pas de raidisseurs => on ne calcule pas

if (panneau(nel).hxr.le.0.001) then
	sig11 = 0.d00
	sig21 = 0.d00
	sig31 = 0.d00
	sig41 = 0.d00
	tau = 0.d00
	fl = 0.d00
	flr = 0.d00
	wpl1 = 0.d00
	wpl2 = 0.d00
	ip50 = 1 !pour ne pas calculer les dérivées
else
	! m=pl**2/10 soit intermediare entre 1/8 et 1/12 
	sig11 = plat*entr*epsa2*v1 /(10.0*ix)        
	sig21 = plat*entr*epsa2*v2 /(10.0*ix)
	sig31 = plat*entr*epsa2*v3 /(10.0*ix)     
	sig41 = plat*entr*epsa2*v4 /(10.0*ix)     
	tau   = plat2*entr*epsa *s /(2.0*dxr*ix)
	! 5/384 c-à-d extrémites libres  
	fl   = 5.*plat2*entr*(epsa**4)/(384.0*e1*ix) 
	! fl/portée = flèche relative  
	flr  = fl/epsa                               
 
!	calcul de la fleche (wpl) de la plaque non raidie, encastrée entre 2 raidisseurs
!     ---------------------------------------------------------------------------------
!     wpl1= 1/384 p entr**4/ei  (borne sup. car effets des 2 autres bords négligés)
!     cette déformation correspond à une charge uniforme. 
!     toutes les mailles se déforment de façon identique (idem bord encasreés)
      wpl1=plat2*(entr**4)/(384.0*e1*delta2*delta/12.0)

!	calcul de la fleche (wpl) de la plaque non raidie, appuyée entre 2 raidisseurs
!     ---------------------------------------------------------------------------------
!     wpl2= 5/384 p entr**4/ei (borne sup. car effets des 2 autres bords négligés)
!     cette déformation correspond à une charge localisée. 
!     seule cette maille se déforme (idem poutre sur appuis multiples avec 1 seule travée chargée)
      wpl2=5.0*wpl1   ! borne sup. car assimilé à 1 poutre sur 2 appuis

endif

!     impressions
!     -----------
	if(impr2.ge.-1) then		
	  write(iu_11(iboat),9) ' area (area)           ',area
	  write(iu_11(iboat),9) ' moment statique (stat)',stat
 	  write(iu_11(iboat),9) ' gravity center        ',zg
	  write(iu_11(iboat),9) ' static moment(s)        ', s        
	  write(iu_11(iboat),9) ' inertia moment          ', ix       
	  write(iu_11(iboat),9) ' v1 (milieu bordé z=0)   ', v1       
	  write(iu_11(iboat),9) ' v2 (partie sup semelle) ', v2       
	  write(iu_11(iboat),9) ' v3 (jonction âme-bordé) ', v3       
	  write(iu_11(iboat),9) ' v4 (jonction âme-sem)   ', v4       

	  write(iu_11(iboat),*)  
	  write(iu_11(iboat),*) ' resultat flexion locale raid. et de la maille '
	  write(iu_11(iboat),*) ' --------------------------------------------- '
	  write(iu_11(iboat),9) ' sigma 1 (milieu bordé)  ', sig11     
	  write(iu_11(iboat),9) ' sigma 2 (semelle)       ', sig21
	  write(iu_11(iboat),9) ' sigma 3 (jab)           ', sig31     
	  write(iu_11(iboat),9) ' sigma 4 (jas)           ', sig41     
	  write(iu_11(iboat),9) ' shear stress (max)      ', tau
	  write(iu_11(iboat),9) ' flèche raid.(5/384) (m) ', fl
	  write(iu_11(iboat),9) ' flèche relative (f/epsa)', flr
	  write(iu_11(iboat),*)  
	  write(iu_11(iboat),*) ' flèche maille (valeur absolue)' 
	  write(iu_11(iboat),9) ' - charge uniforme  (bords encastrés)',wpl1
	  write(iu_11(iboat),9) ' - charge localisée (bords articulés)',wpl2
	  write(iu_11(iboat),*)

	endif

!     calcul des sensibilites
!     ===========================
      if(iopti.eq.0) return
      if(iq50.eq.0)  return ! restriction pas selectionnées

	nbrxi = nvar(nel,iboat)
      
	if(iprint.eq.1) then
	 write(iu_11(iboat),*) ' =*=*=*=*=*=*=*=*=*=*=*=*=*=* '
	 write(iu_11(iboat),*) '  sensibilities = d(f)/d(xi)  '
	 write(iu_11(iboat),*) ' =*=*=*=*=*=*=*=*=*=*=*=*=*=* '
	endif

	area2=area*area
	ix2  =ix*ix

      do 1 j=1,nbrxi
	k=nxit(j,nel,iboat)

	if(k.eq.1) then

!	xi= delta 
!   ---------
	dzg= -(2.0*delta*entr*area+entr*stat)/(2.0*area2)
	dv1=  dzg + 1.0/2  
	dv2=  dzg
	dv3=  dzg          
	dv4=  dzg           
	dz1=  dzg + 0.5
	dz2= -dzg
	dz3= -dzg
	dza= -dzg/2.0
	dix= entr*((z1*z1)+2.0*delta*z1*dz1)         &
    + (entr*delta2)/4.+2*dxr*hxr*z2*dz2          &
    + 2.*wxr*txr*z3*dz3

	ds= dxr*(-dzg*za+(hxr-zg)*dza) + wxr*txr*dz3

	if(plat.ne.0) then													
	   dsig1(k) = (plat*entr*epsa2/(10.*ix2))*(ix*dv1-v1*dix) + dplat(k)*sig11/plat											
	   dsig2(k) = (plat*entr*epsa2/(10.*ix2))*(ix*dv2-v2*dix) + dplat(k)*sig21/plat											
	   dsig3(k) = (plat*entr*epsa2/(10.*ix2))*(ix*dv3-v3*dix) + dplat(k)*sig31/plat											
	   dsig4(k) = (plat*entr*epsa2/(10.*ix2))*(ix*dv4-v4*dix) + dplat(k)*sig41/plat									
	   dtau(k)  = (plat2*entr*epsa/(2.*dxr*ix2))*(ix*ds-s*dix)+ dplat2(k)*tau/plat2										
!	   dfl(k) =-5.0*(plat2*entr*(epsa**4)/(384.*e1*ix2))*dix
	   dflr(k)=-5.0*(plat2*entr*(epsa**3)/(384.*e1*ix2))*dix+ dplat2(k)*flr/plat2										
       dwpl1(k)=-3.*wpl1/delta + dplat2(k)*wpl1/plat2			
       dwpl2(k)= 5.*dwpl1(k)
	endif																

	if(iprint.eq.1) then
	 write(iu_11(iboat),'(/a)') ' xi = delta '
       write(iu_11(iboat),10) dsig1(k),dsig2(k),dsig3(k),dsig4(k),dtau(k),dflr(k),dwpl1(k)                                               
      endif

	if (nel.eq.2) then
	continue
	endif

	else if(k.eq.5) then

!	xi= epsa 
!   --------
	if (epsa.ge.width.or.ia.eq.1) then
		dsig1(k) = 0.d00
		dsig2(k) = 0.d00
		dsig3(k) = 0.d00
		dsig4(k) = 0.d00
		dtau(k) = 0.d00
		dflr(k) = 0.d00
		dwpl1(k) = 0.d00
		dwpl2(k) = 0.d00
	else
		if (plat.ne.0) then												
			dsig1(k) = plat*entr*epsa*v1/( 5.0*ix) + dplat(k)*sig11/plat		
			dsig2(k) = plat*entr*epsa*v2/( 5.0*ix) + dplat(k)*sig21/plat		
			dsig3(k) = plat*entr*epsa*v3/( 5.0*ix) + dplat(k)*sig31/plat		
			dsig4(k) = plat*entr*epsa*v4/( 5.0*ix) + dplat(k)*sig41/plat		
			dtau(k)  = plat2*entr*s/( 2.0*dxr*ix)  + dplat2(k)*tau/plat2 
			dflr(k)   = 5.0*plat2*entr*(epsa**2) /                             &
	                        (128.0*e1*ix)+ dplat2(k)*flr/plat2								
			dwpl1(k)= 0. + dplat2(k)*wpl1/plat2					
			dwpl2(k)= 5.*dwpl1(k)										
		endif
	endif
	if(iprint.eq.1) then
		write(iu_11(iboat),'(/a)') ' xi = epsa '
		write(iu_11(iboat),10) dsig1(k),dsig2(k),dsig3(k),dsig4(k),dtau(k),dflr(k), dwpl1(k)                                              
	endif

	else if(k.eq.6) then

!	xi= hxr 
!   --------
	dzg= (2.0*(dxr*hxr+wxr*txr)*area-dxr*stat)/(2.0*area2)
	dv1=       dzg
	dv2=-1.0 + dzg
  dv3=       dzg   
  dv4=-1.0 + dzg   
	dz1=       dzg
	dz2= 0.5 - dzg
	dz3= 1.0 - dzg
	dza=(1.0 - dzg) /2.0
	dix= dxr*((z2**2)+2.0*hxr*z2*dz2)+ (dxr*hxr2)/4.0 + 2.0*entr*delta*z1*dz1+ 2.*wxr*txr*z3*dz3
	ds= dxr*za + hxr*dxr*dza-dxr*za*dzg - zg*dxr*dza+wxr*txr*dz3

	if (plat.ne.0) then										
	dsig1(k)= (plat*entr*epsa2/(10.*ix2))*(ix*dv1-v1*dix) + dplat(k)*sig11/plat									
	dsig2(k)= (plat*entr*epsa2/(10.*ix2))*(ix*dv2-v2*dix) + dplat(k)*sig21/plat									
	dsig3(k)= (plat*entr*epsa2/(10.*ix2))*(ix*dv3-v3*dix) + dplat(k)*sig31/plat								
	dsig4(k)= (plat*entr*epsa2/(10.*ix2))*(ix*dv4-v4*dix) + dplat(k)*sig41/plat								
	dtau(k) = (plat2*entr*epsa/(2.*dxr*ix2))*(ix*ds-s*dix)+ dplat2(k)*tau/plat2								      
!	dfl(k) = -5.0*(plat2*entr*(epsa**4)/(384.*e1*ix2))*dix
	dflr(k)= -5.0*(plat2*entr*(epsa**3)/(384.*e1*ix2))*dix+ dplat2(k)*flr/plat2								      

      dwpl1(k)= 0. + dplat2(k)*wpl1/plat2								    
      dwpl2(k)= 5.*dwpl1(k)											
	endif													

	if(iprint.eq.1) then
	 write(iu_11(iboat),'(/a)') ' xi = hxr '
       write(iu_11(iboat),10) dsig1(k),dsig2(k),dsig3(k),dsig4(k),dtau(k),dflr(k),dwpl1(k)                                                
      endif

	else if(k.eq.7) then

!	xi= dxr 
!   --------
	dzg= (hxr2*area-hxr*stat)/(2*area2)
	dv1=  dzg
	dv2=  dzg
	dv3=  dzg    
	dv4=  dzg   
	dz1=  dzg
	dz2= -dzg
	dz3= -dzg
	dza= -dzg/2
	dix= hxr*((z2**2)+2.*dxr*z2*dz2)+ (hxr**3)/12. + 2.*entr*delta*z1*dz1+ 2.*wxr*txr*z3*dz3
	ds= (hxr-zg)*za+dxr*(-dzg*za + (hxr-zg)*dza) + wxr*txr*dz3

	if (plat.ne.0) then										
	dsig1(k)= (plat*entr*epsa2/(10.*ix2))*(ix*dv1-v1*dix) + dplat(k)*sig11/plat									
	dsig2(k)= (plat*entr*epsa2/(10.*ix2))*(ix*dv2-v2*dix) + dplat(k)*sig21/plat								
	dsig3(k)= (plat*entr*epsa2/(10.*ix2))*(ix*dv3-v3*dix) + dplat(k)*sig31/plat								
	dsig4(k)= (plat*entr*epsa2/(10.*ix2))*(ix*dv4-v4*dix)	+ dplat(k)*sig41/plat								
	dtau(k) = (plat2*entr*epsa/(2.*(dxr**2)*ix2))*                                 &
	                            (ix*dxr*ds-s*(dxr*dix+ix))+ dplat2(k)*tau/plat2								
	dflr(k)= -5.0*(plat2*entr*(epsa**3)/(384.*e1*ix2))*dix+ dplat2(k)*flr/plat2						
      
	dwpl1(k)= 0. + dplat2(k)*wpl1/plat2 				
      dwpl2(k)= 5.*dwpl1(k)										
	endif													

	if(iprint.eq.1) then
 	 write(iu_11(iboat),'(/a)') ' xi = dxr '
       write(iu_11(iboat),10) dsig1(k),dsig2(k),dsig3(k),dsig4(k),dtau(k),dflr(k),dwpl1(k)                                       
      endif

	else if(k.eq.8) then

!	xi= wxr 
!   --------
	dzg= (txr*(2.0*hxr+txr)*area-txr*stat)/(2.0*area2)
	dv1=  dzg
	dv2=  dzg
	dv3=  dzg
	dv4=  dzg
	dz1=  dzg
	dz2= -dzg
	dz3= -dzg
	dza= -dzg/2
	dix= txr*((z3**2)+2.0*wxr*z3*dz3) + (txr**3)/12.0 + 2.0*dxr*hxr*z2*dz2 + 2.0*entr*delta*z1*dz1
	ds =  dxr*(-dzg*za+(hxr-zg)*dza)+txr*z3+wxr*txr*dz3

	if (plat.ne.0) then											
	dsig1(k) = (plat*entr*epsa2/(10.*ix2))*(ix*dv1-v1*dix)+ dplat(k)*sig11/plat								
	dsig2(k) = (plat*entr*epsa2/(10.*ix2))*(ix*dv2-v2*dix)+ dplat(k)*sig21/plat								
	dsig3(k) = (plat*entr*epsa2/(10.*ix2))*(ix*dv3-v3*dix)+ dplat(k)*sig31/plat								
	dsig4(k) = (plat*entr*epsa2/(10.*ix2))*(ix*dv4-v4*dix)+ dplat(k)*sig41/plat									
	dtau(k)  = (plat2*entr*epsa/(2.0*dxr*ix2))*(ix*ds-s*dix)+ dplat2(k)*tau/plat2								
!	dfl(k)   = -5.0*(plat2*entr*(epsa**4)/(384.0*e1*ix2))*dix
	dflr(k)  = -5.0*(plat2*entr*(epsa**3)/(384.0*e1*ix2))*dix+ dplat2(k)*flr/plat2								

  dwpl1(k)= 0. + dplat2(k)*wpl1/plat2							
  dwpl2(k)= 5.*dwpl1(k)		
	endif												

	if(iprint.eq.1) then
	 write(iu_11(iboat),'(/a)') ' xi = wxr '
       write(iu_11(iboat),10) dsig1(k),dsig2(k),dsig3(k),dsig4(k),dtau(k),dflr(k),dwpl1(k)                                               
      endif

	else if(k.eq.9) then

!	xi= epsr 
!   --------
	dzg= -(delta2*area+delta*stat)/(2*area2)  != d(zg)/d(xi)
	dv1=  dzg
	dv2=  dzg
	dv3=  dzg    
	dv4=  dzg    
	dz1=  dzg
	dz2= -dzg
	dz3= -dzg
	dza= -dzg/2.0
	dix= delta*((z1**2)+2.0*entr*z1*dz1) + (delta**3)/12.+ 2.0*dxr*hxr*z2*dz2 + 2.0*wxr*txr*z3*dz3
	ds  = dxr*(-dzg*za + (hxr-zg)*dza) + wxr*txr*dz3

	if (plat.ne.0) then											
	dsig1(k)= (plat*epsa2/(10.0*ix))*(v1+entr*(ix*dv1-v1*dix)/ix)+ dplat(k)*sig11/plat									
	dsig2(k)= (plat*epsa2/(10.0*ix))*(v2+entr*(ix*dv2-v2*dix)/ix)+ dplat(k)*sig21/plat									
	dsig3(k)= (plat*epsa2/(10.0*ix))*(v3+entr*(ix*dv3-v3*dix)/ix)+ dplat(k)*sig31/plat								
	dsig4(k)= (plat*epsa2/(10.0*ix))*(v4+entr*(ix*dv4-v4*dix)/ix)+ dplat(k)*sig41/plat								
	dtau(k) = (plat2*epsa/(2*dxr*ix))*(s+entr*(ix*ds-s*dix)/ix)  + dplat2(k)*tau/plat2								
	dflr(k)=5.0*(plat2*(epsa**3)/(384.*e1*ix2))*(ix-entr*dix)    + dplat2(k)*flr/plat2							

      dwpl1(k)= 4.0* wpl1/entr + dplat2(k)*wpl1/plat2					
      dwpl2(k)= 5.0*dwpl1(k)
	endif														


      !prise en compte de  d(entr)/d(epsr) = (entr/epsr)**2 
	  temp=(entr/epsr)**2
	  dsig1(k) = dsig1(k) * temp
	  dsig2(k) = dsig2(k) * temp
	  dtau(k)  = dtau(k)  * temp
	  dflr(k)  = dflr(k)  * temp
	  dwpl1(k) = dwpl1(k) * temp
	  dwpl2(k) = dwpl2(k) * temp

	if(iprint.eq.1) then
	 write(iu_11(iboat),'(/a)') ' xi = epsr '
     write(iu_11(iboat),10) dsig1(k),dsig2(k),dsig3(k),dsig4(k),dtau(k),dflr(k),dwpl1(k)                                              
    endif

    else

!	xi= 2,3,4 (hya, dya et wya)
!   ---------------------------
	dsig1(k) = 0.0
	dsig2(k) = 0.0
	dsig3(k) = 0.0   
	dsig4(k) = 0.0   
	dtau (k) = 0.0
	dflr (k) = 0.0
      dwpl1(k) = 0.0
      dwpl2(k) = 0.0

	if(iprint.eq.1) then
	 write(iu_11(iboat),'(/a,i1,a)') ' xi = ',k,' (not used)'
     write(iu_11(iboat),10) dsig1(k),dsig2(k),dsig3(k),dsig4(k),dtau(k),dflr(k),dwpl1(k)                                    
    endif


      endif

  1   continue


! ------------------------------------

   9	format(a,' = ',e14.7,1x,a)
  10  format('   sigma 1 (milieu bordé) ',e14.7/ &     
             '   sigma 2 (semelle)      ',e14.7/ &    
             '   sigma 3 (jab)          ',e14.7/ &    
             '   sigma 4 (jas)          ',e14.7/ &    
             '   tau(ame - axe neutre)  ',e14.7/ &
             '   deflection  rel. raid. ',e14.7/ &
             '   deflection  abs. maille',e14.7)

! ------------------------------------	                                                                   

return                                                              
end                                                                       
