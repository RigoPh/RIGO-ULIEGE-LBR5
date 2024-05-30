	subroutine poidstotal(pdstotal,nnel,var)
c	*****************************************************************************
c						Calcul du poids total de la structure
c
c	Avant, le calcul du poids de la structure était effectué un peu partout. Cette
c	sous-routine permet de centraliser ces calculs en un seul endroit.

c	!!!
c	Les variables nnel et var permettent d'augmenter de 1% la variable de conception
c	var du panneau nnel. Utile uniquement pour caclu différence finie !!!
c	Mettre nnem=0 et var=0 pour une utilsation classique


c	Les variables
c	-------------
c	1	delta =  épaisseur du bordage
c	2	hya   =  hauteur de l'âme des aiguilles
c	3	dya   =  épaisseur de l'âme des aiguilles
c	4	wya   =  largeur des semelles des aiguilles
c	5	epsa  =  entredistance entre aiguilles
c	6	hxr   =  hauteur de l'âme des raidisseurs
c	7	dxr   =  épaisseur de l'âme des raidisseurs
c	8	wxr   =  largeur des semelles des raidisseurs
c	9	epsr  =  entredistance entre raidisseurs

c 	autres paramètres :
c	 	tya = épaisseur semelle aiguilles
c     	txr = épaisseur semelle raidisseurs 
c
c  	Les variables de conception pour epontille
c     ------------------------------------------					
c	1	hya		=	hauteur demi âme ou diam ou côté ext.	
c	2	dya		=	épaisseur d'âme							
c	3	wya		=	largeur de semelle						
c	4	epais	=	épaisseur de paroi mince				
c	5	epsa	=	entredistance entre épontillesc
c
c	*****************************************************************************
      

      use param_section
!      use param_cout
!      use param_opti_local
      
	
	
	implicit double precision(a-h,o-z)
	integer*4 var	

	pdstotal = 0
	
	do nel=1,neto

	delta = panneau(nel).delta
	hya = panneau(nel).hya
	dya = panneau(nel).dya
	wya = panneau(nel).wya
	tya = panneau(nel).tya
	epsa = panneau(nel).epsa
	hxr = panneau(nel).hxr
	dxr = panneau(nel).dxr
	wxr = panneau(nel).wxr
	txr = panneau(nel).txr
	epsr = panneau(nel).epsr
	phil = dabs(panneau(nel).phil) !!!
	q = panneau(nel).q
	epais = panneau(nel).epais

	if (nnel.eq.nel) then
		if (var.eq.1) then
			delta = delta*1.01
		elseif (var.eq.2) then
			hya = hya*1.01
		elseif (var.eq.3) then
			dya = dya*1.01
		elseif (var.eq.4) then
			wya = wya*1.01
		elseif (var.eq.5) then
			epsa = epsa*1.01
		elseif (var.eq.6) then
			hxr = hxr*1.01
		elseif (var.eq.7) then
			dxr = dxr*1.01
		elseif (var.eq.8) then
			wxr = wxr*1.01
		elseif (var.eq.9) then
			epsr = epsr*1.01
		endif
	endif

	call poidspanneau(poidspan,nel,epsa,epsr,delta,hya,dya,wya,
     *tya,hxr,dxr,wxr,txr,phil,q,epais)

	pdstotal = pdstotal + poidspan

	enddo

	return
	end


c	********************************************************************
	subroutine poidspanneau(poidspan,nel,epsa,epsr,delta,hya,dya,wya,
     *tya,hxr,dxr,wxr,txr,phil,q,epais)
c
c	CALCUL DU POIDS DU PANNEAU/EPONTILLE NEL
c
c	Input : nel
c
c	Output : poidspan (= poids du panneau nel)
c
c	********************************************************************

      

      use param_section
!      use param_cout
!      use param_opti_local
      
	
      implicit double precision(a-h,o-z)

	dcor  = panneau(nel).corro(1)
	dcor2 = panneau(nel).corro(2)
	dcor3 = panneau(nel).corro(3)

	if(isect(nel).eq.3) then				!âme double t entree=1/2 âme	!février 2004
	hyb=2*hya															!février 2004
	endif																!février 2004

	if(itype(nel).ne.5) then								!février 2004
	delta= delta+dcor
	else											!février 2004
	delta =0.000									!février 2004
	endif											!février 2004
c	dcor2   ! epaisseur de corrosion pour cadres
	dya  = dya +dcor2
	tya  = tya +dcor2
	epais = epais +dcor2							!février 2004
c	dcor3   ! epaisseur de corrosion pour lisses
	dxr  = dxr +dcor3
	txr  = txr +dcor3

c     poidspan = poids = bordage + ames + semelles  (du panneau nel)
c     obj = poids total (structure complete)

	dens = spec(indMateriau(nel))/9.81
      temp=  phil * q * width * pi/180.
      temp=  temp * spec(indMateriau(nel))  !si poids en n

	dw3_local = dw3(indMateriau(nel))
	dw2_local = dw2(indMateriau(nel))
	
	if(((refthick_input.eq.1).and.(itype(nel).ne.5))) then

		! On utilise drefy_local qui est :
		! - soit drefy
		! - soit dref_b, dref_c ou dref_l (plat boudin, Té composite, Té laminé
		! Et pas drefy=dref_b car ça modifierait les valeurs de drefy qui est une variable globale
		!  => on modifierait les données

	   if (panneau(nel).itype_transv.eq.14)then   !Plat Boudin              
	        dw3_local=dw_b(indMateriau(nel))

	   elseif(panneau(nel).itype_transv.eq.7) then   ! Té Composite
              dw3_local=dw_c(indMateriau(nel))

	   elseif(panneau(nel).itype_transv.eq.17)then   ! Té Laminé
              dw3_local=dw_l(indMateriau(nel))

	   else 
	      write(* ,*)'type de profilé inexistant'
	   endif

	   if (panneau(nel).itype_longit.eq.14)then   !Plat Boudin
	        dw2_local=dw_b(indMateriau(nel))

	   elseif(panneau(nel).itype_longit.eq.7) then   ! Té Composite
	        dw2_local=dw_c(indMateriau(nel))

	   elseif(panneau(nel).itype_longit.eq.17)then   ! Té Laminé
	        dw2_local=dw_l(indMateriau(nel))
	   else 
	      write(* ,*)'type de profilé inexistant'
	   endif

      endif


	poidspan = 0.

c	Calul du poids du panneau
	if(itype(nel).ne.5) then			  !plaque		!février 2004
      poidspan= delta 
     *  + (1.0+dw3_local)* ( dya * hya + tya* wya ) /epsa
     *  + (1.0+dw2_local)* ( dxr * hxr + txr* wxr ) /epsr
	else						    	! epontille						!février 2004
	  if(isect(nel).eq.3)then											!février 2004
      poidspan = delta +													!février 2004
c     *      (1.0+dw2)*(dxr*hxr+txr*wxr)/epsr +							!oct06
     *      (1.0+dw3_local)*(dya*hyb+2*tya*wya)/epsa 						!février 2004
	  elseif(isect(nel).eq.1)then										!février 2004
	poidspan = delta +													!février 2004
c     *      (1.0+dw2)*(dxr*hxr+txr*wxr)/epsr +							!oct06
     *      (1.0+dw3_local)*(pi*(hya*hya-(hya-2*epais)**2))				!février 2004
     *	  	/(4*epsa)													!février 2004
	  elseif(isect(nel).eq.2) then										!février 2004
      poidspan = delta +													!février 2004
c     *      (1.0+dw2)*(dxr*hxr+txr*wxr)/epsr +							!oct06
     *      (1.0+dw3_local)*(hya*hya-(hya-2*epais)**2)/epsa				!20.02.04
	  endif
	endif

	poidspan = poidspan * temp

	return
	end
