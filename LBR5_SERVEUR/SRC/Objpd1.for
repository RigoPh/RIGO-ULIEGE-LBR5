      subroutine objpd1(obj_,derivee,iderivee,iprint_)			!février 2004



      

      use param_section
!      use param_cout
!      use param_opti_local
	

      implicit double precision(a-h,o-z)
	dimension derivee(ntot(iboat))

c ******************************************************************************
c     calcul de la fonction objectif poids (obj) et
c     calcul des dérivées de la fonction objectif (derivee)
c
c     version de : JANVIER 2009                                créé 29-4-94
c
c ******************************************************************************

c	Calcul Fonction objectif
	
	call poidstotal(obj_,0,0)

	if (iprint_.ne.0) then		!oct06
	 write(iu_31(iboat),*)
       write(iu_31(iboat),*)
     *' poids total pour toute la structure (subr.objpd1)'
       write(iu_31(iboat),*)
     *' -------------------------------------------------'
       write(iu_31(iboat),*)
     *'  poids - weight = ',obj_,' n'
	endif



c	Calcul Dérivées de la fonction objectif

	if (iderivee.eq.1) then

	k=0
	do 1 nel=1,neto
	  nbrxi=nvar(nel,iboat)

	  if (nbrxi.eq.0) goto 1

        if(itype(nel).ne.5) then
	  
		do i=1,nbrxi
	      j=nxit(i,nel,iboat)		! no de ref de chaque var. de conception

		  derivee(k+i) = dpds(nel,j)

	    enddo

	  else
	    do i=1,nbrxi
	      derivee(k+i)=1.e-10
	    enddo
	  endif
	  k=k+nbrxi
    1	continue

	endif

  
      return
      end

c	*************************************************************************************
c	CALCUL de la dérivée du POIDS par rapport à la variable var du panneau nel
c	*************************************************************************************

c les variables
c -------------
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
c  les variables de conception pour epontille					!février 2004
c  ------------------------------------------					
c	1	hya		=	hauteur demi âme ou diam ou côté ext.	
c	2	dya		=	épaisseur d'âme							
c	3	wya		=	largeur de semelle						
c	4	epais	=	épaisseur de paroi mince				
c	5	epsa	=	entredistance entre épontilles			
c

c ******************************************************************************

	double precision function dpds(nel,var)

      

      use param_section
!      use param_cout
!      use param_opti_local
      


	implicit double precision (a-h,o-z)
	integer*4 var

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


c	Pas utilisé ???
	if(isect(nel).eq.3) then			!âme double t entree=1/2 âme	!février 2004
	hyb=2*hya															!février 2004
	endif																!février 2004

	if(itype(nel).ne.5) then						!février 2004
	delta= delta + panneau(nel).corro(1)
	else											!février 2004
	delta =0.000									!février 2004
	endif											!février 2004
c	dcor2   ! epaisseur de corrosion pour cadres
	dya  = dya + panneau(nel).corro(2)
	tya  = tya + panneau(nel).corro(2)
	epais = epais + panneau(nel).corro(2)							!février 2004
c	dcor3   ! epaisseur de corrosion pour lisses
	dxr  = dxr + panneau(nel).corro(3)
	txr  = txr + panneau(nel).corro(3)

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

	! Calcul des dérivées
	if(var.eq.1) then
		dpds= temp
	else if (var.eq.2) then
		dpds= temp  * (1.0+dw3_local)*dya/epsa
	else if (var.eq.3) then
		dpds= temp  * (1.0+dw3_local)*hya/epsa
	else if (var.eq.4) then
		dpds= temp  * (1.0+dw3_local)*tya/epsa
	else if (var.eq.5) then
		dpds= -temp * (1.0+dw3_local)*(dya*hya+tya*wya)/(epsa*epsa) 
	else if (var.eq.6) then
		dpds= temp  * (1.0+dw2_local)*dxr/epsr
	else if (var.eq.7) then
		dpds= temp  * (1.0+dw2_local)*hxr/epsr
	else if (var.eq.8) then
		dpds= temp  * (1.0+dw2_local)*txr/epsr
	else if (var.eq.9) then
		dpds= -temp * (1.0+dw2_local)*(dxr*hxr+txr*wxr)/(epsr*epsr) 
	endif

	return
	end function
