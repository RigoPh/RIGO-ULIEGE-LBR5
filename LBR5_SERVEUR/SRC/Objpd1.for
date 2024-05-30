      subroutine objpd1(obj_,derivee,iderivee,iprint_)			!f�vrier 2004



      

      use param_section
!      use param_cout
!      use param_opti_local
	

      implicit double precision(a-h,o-z)
	dimension derivee(ntot(iboat))

c ******************************************************************************
c     calcul de la fonction objectif poids (obj) et
c     calcul des d�riv�es de la fonction objectif (derivee)
c
c     version de : JANVIER 2009                                cr�� 29-4-94
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



c	Calcul D�riv�es de la fonction objectif

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
c	CALCUL de la d�riv�e du POIDS par rapport � la variable var du panneau nel
c	*************************************************************************************

c les variables
c -------------
c	1	delta =  �paisseur du bordage
c	2	hya   =  hauteur de l'�me des aiguilles
c	3	dya   =  �paisseur de l'�me des aiguilles
c	4	wya   =  largeur des semelles des aiguilles
c	5	epsa  =  entredistance entre aiguilles
c	6	hxr   =  hauteur de l'�me des raidisseurs
c	7	dxr   =  �paisseur de l'�me des raidisseurs
c	8	wxr   =  largeur des semelles des raidisseurs
c	9	epsr  =  entredistance entre raidisseurs

c 	autres param�tres :
c	 	tya = �paisseur semelle aiguilles
c     	txr = �paisseur semelle raidisseurs 
c
c  les variables de conception pour epontille					!f�vrier 2004
c  ------------------------------------------					
c	1	hya		=	hauteur demi �me ou diam ou c�t� ext.	
c	2	dya		=	�paisseur d'�me							
c	3	wya		=	largeur de semelle						
c	4	epais	=	�paisseur de paroi mince				
c	5	epsa	=	entredistance entre �pontilles			
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


c	Pas utilis� ???
	if(isect(nel).eq.3) then			!�me double t entree=1/2 �me	!f�vrier 2004
	hyb=2*hya															!f�vrier 2004
	endif																!f�vrier 2004

	if(itype(nel).ne.5) then						!f�vrier 2004
	delta= delta + panneau(nel).corro(1)
	else											!f�vrier 2004
	delta =0.000									!f�vrier 2004
	endif											!f�vrier 2004
c	dcor2   ! epaisseur de corrosion pour cadres
	dya  = dya + panneau(nel).corro(2)
	tya  = tya + panneau(nel).corro(2)
	epais = epais + panneau(nel).corro(2)							!f�vrier 2004
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
		! - soit dref_b, dref_c ou dref_l (plat boudin, T� composite, T� lamin�
		! Et pas drefy=dref_b car �a modifierait les valeurs de drefy qui est une variable globale
		!  => on modifierait les donn�es

	   if (panneau(nel).itype_transv.eq.14)then   !Plat Boudin              
	        dw3_local=dw_b(indMateriau(nel))

	   elseif(panneau(nel).itype_transv.eq.7) then   ! T� Composite
              dw3_local=dw_c(indMateriau(nel))

	   elseif(panneau(nel).itype_transv.eq.17)then   ! T� Lamin�
              dw3_local=dw_l(indMateriau(nel))

	   else 
	      write(* ,*)'type de profil� inexistant'
	   endif

	   if (panneau(nel).itype_longit.eq.14)then   !Plat Boudin
	        dw2_local=dw_b(indMateriau(nel))

	   elseif(panneau(nel).itype_longit.eq.7) then   ! T� Composite
	        dw2_local=dw_c(indMateriau(nel))

	   elseif(panneau(nel).itype_longit.eq.17)then   ! T� Lamin�
	        dw2_local=dw_l(indMateriau(nel))
	   else 
	      write(* ,*)'type de profil� inexistant'
	   endif

      endif

	! Calcul des d�riv�es
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
